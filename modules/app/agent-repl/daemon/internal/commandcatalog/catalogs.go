// Package commandcatalog holds every workspace's standing slash-command
// catalog: the last conversation.v1.SessionCommandCatalog its session stated.
//
// THE RUNTIME IS THE AUTHORITY. The catalog is the shim's read of the vendor's
// own command list, stated on the session stream; nothing here adds to it,
// filters it or derives one from disk. It is kept here, not in a view
// resolver, because no drawn component owns it: the Emacs composer pulls it
// through ListSlashCommands when the user starts a `/` word.
//
// IT OUTLIVES THE SESSION THAT STATED IT. A parked or restarting session
// keeps offering the commands its runtime last answered until the next
// statement replaces them, so a composer completes even while no shim runs.
// A daemon that starts holds nothing until a session's watch opens, which is
// where the shim restates the standing catalog.
package commandcatalog

import (
	"fmt"
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
)

// Catalogs is the per-workspace table. The zero value is not usable; build
// one with New.
type Catalogs struct {
	mu   sync.Mutex
	byWS map[ids.WorkspaceID]*conversationv1.SessionCommandCatalog
}

// New builds an empty table.
func New() *Catalogs {
	return &Catalogs{byWS: make(map[ids.WorkspaceID]*conversationv1.SessionCommandCatalog)}
}

// OnCommandCatalog installs the catalog a workspace's session just stated,
// replacing whatever stood: every statement is the whole set.
//
// A nil catalog is an invariant violation, never "no catalog": a decoded
// command_catalog arm always carries its message, so nil here is a caller
// defect and panics at its origin.
func (c *Catalogs) OnCommandCatalog(ws ids.WorkspaceID, catalog *conversationv1.SessionCommandCatalog) {
	if catalog == nil {
		panic(fmt.Sprintf("commandcatalog: a nil catalog was stated for workspace %s", ws))
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.byWS[ws] = catalog
}

// Catalog answers the workspace's standing catalog, and false when no session
// of the workspace has stated one since this daemon took it on.
func (c *Catalogs) Catalog(ws ids.WorkspaceID) (*conversationv1.SessionCommandCatalog, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	catalog, ok := c.byWS[ws]
	return catalog, ok
}
