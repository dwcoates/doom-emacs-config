// Package holds is the hold-tray resolver.
//
// The tray is fed by the PROMPT QUEUE and the MERGE ORCHESTRATOR, never by the
// session watcher. Its heading is composed here. See ARCHITECTURE.md
// "resolvers".
package holds

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// Resolver is the tray's whole surface.
type Resolver interface {
	sessionwatcher.HoldsSink

	// SetHeldPrompts installs a workspace's standing holds, whole. The prompt
	// queue is the only caller.
	SetHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt)
	// SetOffer installs the parked question the tray poses — currently the
	// merge dequeue offer — or clears it with nil. The merge orchestrator is
	// the only caller.
	SetOffer(ws ids.WorkspaceID, offer *frontendv1.HeldOffer)
	// Topic is the workspace's tray publication.
	Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.DaemonHoldTray]
}

// New builds the holds resolver.
func New(log dlog.Surfaces) (Resolver, error) {
	return nil, notimpl.Err
}
