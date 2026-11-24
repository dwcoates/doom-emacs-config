// Package pageheld holds which workspaces' pages Emacs's web view holds now,
// and why each was loaded, as Emacs reports them (agentrepl.v1
// ReportPageState). It is THE ONE SOURCE every count of loaded pages reads
// (the agentrepl_pages_loaded gauge, the sidebar's version hover), so no two
// can disagree, and a workspace that is visible with its page unloaded is
// never counted loaded.
//
// THE STATE IS SCOPED TO THE WORKSPACE'S HOST STREAM: the server forgets a
// workspace's page when its last WatchHostWorkspace stream ends (Forget), so
// an editor that exits or a workspace handed to a successor leaves no page
// counted that no web view holds. Emacs restates what it holds on every host
// subscribe.
package pageheld

import (
	"fmt"
	"sync"

	"agent-repld/internal/ids"
)

// LoadReason is why a page was loaded: the moment Emacs's one load path took.
type LoadReason string

// The load reasons, as the metrics label them.
const (
	LoadShown     LoadReason = "shown"
	LoadAwake     LoadReason = "awake"
	LoadSlot      LoadReason = "slot"
	LoadRequested LoadReason = "load_requested"
	LoadUngated   LoadReason = "ungated"
)

// LoadReasons is every load reason, for a caller that serves each from 0.
var LoadReasons = []LoadReason{LoadShown, LoadAwake, LoadSlot, LoadRequested, LoadUngated}

// UnloadReason is why a page was dropped.
type UnloadReason string

// The unload reasons, as the metrics label them.
const (
	UnloadDormant  UnloadReason = "dormant"
	UnloadDetached UnloadReason = "detached"
	UnloadReleased UnloadReason = "released"
)

// UnloadReasons is every unload reason, for a caller that serves each from 0.
var UnloadReasons = []UnloadReason{UnloadDormant, UnloadDetached, UnloadReleased}

// Telemetry counts every page load and unload (runtimemetrics.Surface). Each
// method is one counter update and never blocks.
type Telemetry interface {
	CountPageLoad(reason LoadReason)
	CountPageUnload(reason UnloadReason)
}

// Table holds every held page's load reason, by workspace.
type Table struct {
	telemetry Telemetry
	mu        sync.Mutex
	held      map[ids.WorkspaceID]LoadReason
}

// New answers an empty table that counts its changes through TELEMETRY, which
// is required: pass the off metrics surface to count nothing.
func New(telemetry Telemetry) *Table {
	if telemetry == nil {
		panic("pageheld: a table counts its changes; pass the off metrics surface to count nothing")
	}
	return &Table{telemetry: telemetry, held: map[ids.WorkspaceID]LoadReason{}}
}

// Loaded records WS's page just loaded for REASON, and counts the load.
func (t *Table) Loaded(ws ids.WorkspaceID, reason LoadReason) {
	t.set(ws, reason)
	t.telemetry.CountPageLoad(reason)
}

// Unloaded records WS's page just dropped for REASON, and counts the unload.
func (t *Table) Unloaded(ws ids.WorkspaceID, reason UnloadReason) {
	t.Forget(ws)
	t.telemetry.CountPageUnload(reason)
}

// Held records Emacs's restatement that it holds WS's page, loaded for
// REASON. Nothing is counted.
func (t *Table) Held(ws ids.WorkspaceID, reason LoadReason) { t.set(ws, reason) }

// Forget records that no page of WS is held: Emacs's restatement that it
// holds none, or the workspace's last host stream ending. Nothing is counted.
func (t *Table) Forget(ws ids.WorkspaceID) {
	t.mu.Lock()
	defer t.mu.Unlock()
	delete(t.held, ws)
}

func (t *Table) set(ws ids.WorkspaceID, reason LoadReason) {
	if !validLoad(reason) {
		panic(fmt.Sprintf("pageheld: a page of %s was loaded for %q, which LoadReasons does not hold", ws, reason))
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	t.held[ws] = reason
}

// Snapshot answers every held page's load reason, by workspace: a copy.
func (t *Table) Snapshot() map[ids.WorkspaceID]LoadReason {
	t.mu.Lock()
	defer t.mu.Unlock()
	out := make(map[ids.WorkspaceID]LoadReason, len(t.held))
	for ws, r := range t.held {
		out[ws] = r
	}
	return out
}

func validLoad(r LoadReason) bool {
	for _, known := range LoadReasons {
		if r == known {
			return true
		}
	}
	return false
}
