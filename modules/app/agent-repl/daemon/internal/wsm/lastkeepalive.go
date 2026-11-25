package wsm

import (
	"context"
	"database/sql"
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
// sent, and counts it into the workspace's keep-alive run (keepaliverun.go)
// in the same transaction.
func (s *store) SetLastKeepalive(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.write(ctx, "daemon.wsm.set_last_keepalive", dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		if err := updateWorkspaceColumns(ctx, tx, id, workspaceColumn{name: "last_keepalive_at", value: nanos(at)}); err != nil {
			return err
		}
		return countKeepalive(ctx, tx, id)
	})
}

// BindNewConversation records that workspace ID's session runs, from AT, a
// conversation other than the one its cache clock timed: a bring-up that came
// up FRESH because the recorded conversation's transcript is gone, or one
// that restored another transcript of the directory (workspace.Fleet
// .classifySource). The keep-alive half is cleared and ConversationBoundAt
// stamped, so CacheClock is nil until a turn of the new conversation closes.
//
// IT IS WRITTEN BEFORE THE SESSION'S SHIM IS SERVED, so the cache keeper can
// never time a beat for the new conversation from the old one's clock
// (2026-11-25 12:33:52: a keep-alive owed since 2026-10-14 went into a
// conversation started thirteen seconds before, which had no cache to keep).
func (s *store) BindNewConversation(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.setWorkspaceColumns(ctx, "daemon.wsm.bind_new_conversation", id, dlog.Context{"at": at},
		workspaceColumn{name: "last_keepalive_at", value: nil},
		workspaceColumn{name: "conversation_bound_at", value: nanos(at)})
}

// CacheClock is when the prompt cache of the conversation the workspace runs
// was last written: the later of its last turn's end and its last keep-alive.
// Nil when neither happened.
//
// A TURN FROM BEFORE THE BINDING IS THE OTHER CONVERSATION'S. A queue-bypass
// switch records the bound-in conversation's own clock as the keep-alive
// half and stamps ConversationBoundAt, and a bring-up that starts another
// conversation clears that half and stamps it (BindNewConversation), so a
// turn that closed before the stamp times nothing.
func (w Workspace) CacheClock() *time.Time {
	turn := w.LastTurnEndedAt
	if turn != nil && w.ConversationBoundAt != nil && !turn.After(*w.ConversationBoundAt) {
		turn = nil
	}
	switch {
	case w.LastKeepaliveAt == nil:
		return turn
	case turn == nil || w.LastKeepaliveAt.After(*turn):
		return w.LastKeepaliveAt
	default:
		return turn
	}
}
