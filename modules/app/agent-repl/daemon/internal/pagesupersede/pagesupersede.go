// Package pagesupersede raises an error when a workspace page that a newer
// page has replaced goes on subscribing.
//
// WHY IT EXISTS (2026-10-18). An unloaded page's script went on running in
// WebKit. On 2026-10-17 agent-repl-response-tree's replaced page redialled its
// feed every few seconds for ten minutes beside the page that replaced it, and
// the leaked documents behind the web process's 11-18 GB footprint were pages
// like it that outlived their unload. A page that serves a workspace after a
// newer page took that workspace over is such a page. Its own logs cannot say
// so: it is the page that should be gone. So the daemon, which sees every
// page's subscriptions, raises it.
//
// A PAGE IS KNOWN BY ITS ID, minted once per document. The pages serving a
// workspace are remembered in the order each first served it, so the newest is
// the one the workspace's view now belongs to. A page reconnecting under its
// own id while it is still the newest is ordinary; any other page serving
// again is superseded, and raised once.
package pagesupersede

import (
	"errors"
	"slices"
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

const op = "daemon.server.superseded_page_active"

// Remembered is how many pages per workspace are kept, the newest last. A page
// that outlived its unload is among the last few that served its workspace.
const Remembered = 16

// Deps are the tracker's collaborators, every one required.
type Deps struct {
	Log dlog.Logger
}

// Tracker remembers which page each workspace's view belongs to.
type Tracker struct {
	log dlog.Logger

	mu       sync.Mutex
	pages    map[ids.WorkspaceID][]string
	reported map[string]bool
}

// New builds a tracker.
func New(deps Deps) (*Tracker, error) {
	if deps.Log == nil {
		return nil, errors.New("pagesupersede: a logger is required")
	}
	return &Tracker{log: deps.Log, pages: map[ids.WorkspaceID][]string{}, reported: map[string]bool{}}, nil
}

// Served records that PAGE's subscription was accepted for WS. A page serving
// WS for the first time becomes its newest. A page that served WS before and is
// no longer its newest is superseded, and raised once.
func (t *Tracker) Served(page string, ws ids.WorkspaceID) {
	t.mu.Lock()
	defer t.mu.Unlock()
	pages := t.pages[ws]
	at := slices.Index(pages, page)
	if at < 0 {
		pages = append(pages, page)
		if len(pages) > Remembered {
			pages = pages[len(pages)-Remembered:]
		}
		t.pages[ws] = pages
		return
	}
	newest := pages[len(pages)-1]
	if newest == page || t.reported[page] {
		return
	}
	t.reported[page] = true
	t.log.Error(op, "a page that a newer page replaced is still subscribing: an unloaded page's script is running", dlog.Context{
		"page": page, "workspace": string(ws), "superseded_by": newest, "pages_since": len(pages) - 1 - at,
	})
}
