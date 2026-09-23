// Package holds is the hold-tray resolver.
//
// The tray is fed by the PROMPT QUEUE and the MERGE ORCHESTRATOR, never by the
// session watcher. See ARCHITECTURE.md "resolvers".
//
// THE TRAY IS ALWAYS WHOLE, INCLUDING WHEN IT IS EMPTY. An empty items list is
// a meaningful value — "the daemon is holding nothing for you" — so the tray
// publishes from the first fact a workspace receives rather than waiting for
// something to hold.
package holds

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// Resolver is the tray's whole surface.
type Resolver interface {
	sessionwatcher.HoldsSink

	// SetWorkspaceDir binds the workspace's directory, which is what resolves
	// its durable log sink. The daemon calls it at registration, BEFORE any
	// hold can arrive; a hold for an unbound workspace is an invariant
	// violation the resolver records loudly rather than writing globally by
	// default.
	SetWorkspaceDir(ws ids.WorkspaceID, dir string) error
	// SetHeldPrompts installs a workspace's standing holds, whole. The prompt
	// queue is the only caller.
	SetHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt)
	// SetEditing names the held prompt being edited (EditHeldPrompt), or
	// clears it with the empty turn. Its entry carries HeldPrompt.editing.
	// The prompt queue is the only caller.
	SetEditing(ws ids.WorkspaceID, turn ids.TurnID)
	// SetOffer installs the parked question the tray poses — currently the
	// merge dequeue offer — or clears it with nil. The merge orchestrator is
	// the only caller.
	SetOffer(ws ids.WorkspaceID, offer *frontendv1.HeldOffer)
	// Topic is the workspace's tray publication.
	Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.DaemonHoldTray]
}

// New builds the holds resolver.
func New(log dlog.Surfaces) (Resolver, error) {
	return newResolver(log)
}
