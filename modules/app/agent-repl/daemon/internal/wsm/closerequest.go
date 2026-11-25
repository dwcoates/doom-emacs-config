package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// closeRequestDDL is the layout-215 addition: the workspace's STANDING CLOSE
// REQUEST (agentrepl.v1.CloseWorkspaceRequest.stand), when it was registered
// and who asked (a CloseCause). DURABLE because the daemon carries the close
// out on an edge that may come after a restart or a handover: a request the
// restart forgot would leave the workspace open for good. Every close clears
// it (MarkClosed), so a request never outlives its workspace's close.
//
// IT IS ADDITIVE: every self-close mark (close_when_turn_ends) becomes an
// agent's request, and triggers keep the old column equal to the request
// while a predecessor that still writes the mark serves beside this build. A
// pair already equal fires nothing (each trigger's WHEN), so the two
// directions never chase each other. Layout 216 retires the mark.
const closeRequestDDL = `
ALTER TABLE workspaces ADD COLUMN close_requested_at INTEGER;
ALTER TABLE workspaces ADD COLUMN close_requested_by TEXT;
UPDATE workspaces SET close_requested_at = close_when_turn_ends, close_requested_by = 'agent' WHERE close_when_turn_ends IS NOT NULL;
CREATE TRIGGER workspaces_self_close_writes_close_request AFTER UPDATE OF close_when_turn_ends ON workspaces
WHEN NEW.close_when_turn_ends IS NOT NEW.close_requested_at
BEGIN
  UPDATE workspaces SET close_requested_at = NEW.close_when_turn_ends,
    close_requested_by = CASE WHEN NEW.close_when_turn_ends IS NULL THEN NULL ELSE coalesce(NEW.close_requested_by, 'agent') END
  WHERE id = NEW.id;
END;
CREATE TRIGGER workspaces_close_request_writes_self_close AFTER UPDATE OF close_requested_at ON workspaces
WHEN NEW.close_requested_at IS NOT NEW.close_when_turn_ends
BEGIN
  UPDATE workspaces SET close_when_turn_ends = NEW.close_requested_at WHERE id = NEW.id;
END;
`

// retiredSelfCloseDDL is the layout-216 contract: the self-close mark and its
// mirroring triggers (closeRequestDDL). No build past layout 215 reads or
// writes the mark, and the contract runs only once no predecessor shares the
// file.
const retiredSelfCloseDDL = `
DROP TRIGGER workspaces_self_close_writes_close_request;
DROP TRIGGER workspaces_close_request_writes_self_close;
ALTER TABLE workspaces DROP COLUMN close_when_turn_ends;
`

// CloseRequest is a workspace's standing close request: the daemon closes the
// workspace as By once nothing blocks the close, unless it is withdrawn first.
type CloseRequest struct {
	// At is when the request was registered.
	At time.Time
	// By is who asked, and the cause the close it carries out records: the
	// user or an agent.
	By CloseCause
}

// closeRequestOf reads a row's request columns, nil while none stands. A
// stored cause outside the declared set is a corrupt row.
func closeRequestOf(at sql.NullInt64, by sql.NullString) (*CloseRequest, error) {
	if !at.Valid {
		return nil, nil
	}
	cause := CloseCause(by.String)
	if !cause.IsGesture() {
		return nil, fmt.Errorf("wsm: a standing close request names the cause %q, which is not a user's or an agent's", by.String)
	}
	return &CloseRequest{At: fromNanos(at.Int64), By: cause}, nil
}

// RequestClose registers REQUEST as the workspace's standing close request,
// replacing one that stood. Only a user's or an agent's ask may stand: any
// other cause is a caller's defect and is refused before anything is written.
func (s *store) RequestClose(ctx context.Context, id WorkspaceID, request CloseRequest) error {
	if !request.By.IsGesture() {
		return fmt.Errorf("wsm: a close request by %q cannot stand; only a user's or an agent's can", request.By)
	}
	return s.setWorkspaceColumns(ctx, "daemon.wsm.request_close", id, dlog.Context{"at": request.At, "by": string(request.By)},
		workspaceColumn{name: "close_requested_at", value: nanos(request.At)},
		workspaceColumn{name: "close_requested_by", value: string(request.By)})
}

// WithdrawCloseRequest clears the workspace's standing close request and
// answers whether one stood. THE CHECK AND THE CLEAR ARE ONE STATEMENT, so two
// withdrawals racing never both report one.
func (s *store) WithdrawCloseRequest(ctx context.Context, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.withdraw_close_request"
	var withdrawn bool
	err := s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET close_requested_at = NULL, close_requested_by = NULL WHERE id = ? AND close_requested_at IS NOT NULL`, id)
		if err != nil {
			return err
		}
		withdrawn, err = touchedRow(res)
		return err
	})
	return withdrawn, err
}

// CloseRequests answers every open workspace whose close request stands, in
// no particular order.
func (s *store) CloseRequests(ctx context.Context) ([]WorkspaceID, error) {
	rows, err := s.q(ctx).QueryContext(ctx, `SELECT id FROM workspaces WHERE close_requested_at IS NOT NULL AND closed = 0`)
	if err != nil {
		return nil, fmt.Errorf("wsm: read the standing close requests: %w", err)
	}
	defer rows.Close()
	var out []WorkspaceID
	for rows.Next() {
		var id WorkspaceID
		if err := rows.Scan(&id); err != nil {
			return nil, fmt.Errorf("wsm: read a standing close request: %w", err)
		}
		out = append(out, id)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("wsm: read the standing close requests: %w", err)
	}
	return out, nil
}
