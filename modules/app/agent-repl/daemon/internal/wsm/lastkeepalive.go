package wsm

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
)

// lastKeepaliveDDL is the layout-81 addition: last_keepalive_at, when the
// daemon last sent the workspace's keep-alive (cachelapse.Keeper), in
// nanoseconds; NULL when none was sent, which every existing row is.
//
// IT IS THE BEAT'S HALF OF THE CACHE CLOCK. Nothing of a keep-alive turn is
// stored, so the turns' closed_at never moves for one; timed from the last
// turn end alone, the beat after a keep-alive was already overdue, and the
// keeper restored and beat a parked session again within seconds, over and
// over (2026-10-26). IT IS ADDITIVE: the previous build's writes name no such
// column. The fresh-file schema appends this same text (schema.go).
const lastKeepaliveDDL = `
ALTER TABLE workspaces ADD COLUMN last_keepalive_at INTEGER;
`

// SetLastKeepalive records AT as when the workspace's keep-alive was last
// sent.
func (s *store) SetLastKeepalive(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.setWorkspaceColumns(ctx, "daemon.wsm.set_last_keepalive", id, dlog.Context{"at": at},
		workspaceColumn{name: "last_keepalive_at", value: nanos(at)})
}

// CacheClock is when the workspace's prompt cache was last written: the later
// of its last turn's end and its last keep-alive. Nil when neither happened.
func (w Workspace) CacheClock() *time.Time {
	switch {
	case w.LastKeepaliveAt == nil:
		return w.LastTurnEndedAt
	case w.LastTurnEndedAt == nil || w.LastKeepaliveAt.After(*w.LastTurnEndedAt):
		return w.LastKeepaliveAt
	default:
		return w.LastTurnEndedAt
	}
}
