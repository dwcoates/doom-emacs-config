package wsm

import (
	"context"
	"database/sql"
	"time"

	"agent-repld/internal/dlog"
)

// selfCloseDDL is the layout-63 addition: close_when_turn_ends, set while a
// workspace that asked to close ITSELF waits for its asking turn to end
// (agentrepl.v1.CloseWorkspaceSuccess.closes_when_turn_ends), to when it
// asked. DURABLE because a landing in the daemon's own checkout restarts the
// daemon mid-turn: a close the restart forgot would leave the workspace open
// for good. Every close clears it (SetClosed), so a mark can never outlive
// its workspace's close, however that close came about.
const selfCloseDDL = `
ALTER TABLE workspaces ADD COLUMN close_when_turn_ends INTEGER;
`

// MarkCloseWhenTurnEnds marks the workspace to close when its running turn
// ends, as of AT.
func (s *store) MarkCloseWhenTurnEnds(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.setWorkspaceColumns(ctx, "daemon.wsm.mark_close_when_turn_ends", id, dlog.Context{"at": at},
		workspaceColumn{name: "close_when_turn_ends", value: nanos(at)})
}

// TakeCloseWhenTurnEnds clears the workspace's self-close mark and answers
// whether one stood. THE CHECK AND THE CLEAR ARE ONE STATEMENT, so of every
// party that tries to carry the close out -- the turn-end edge, and the ask
// itself rechecking a turn that ended as it was marked -- exactly one takes
// it.
func (s *store) TakeCloseWhenTurnEnds(ctx context.Context, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.take_close_when_turn_ends"
	var taken bool
	err := s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET close_when_turn_ends = NULL WHERE id = ? AND close_when_turn_ends IS NOT NULL`, id)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		taken = n > 0
		return err
	})
	return taken, err
}
