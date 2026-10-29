package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// coldGateDDL is the layout-88 addition: the cold gate standing on a
// workspace, recorded so the gate outlives its shim (owner ruling, 2026-10-27:
// a workspace at its cold gate runs no shim until the user answers it). The
// gate is the fleet's to raise again from this row with no shim behind it --
// after its shim is stood down, after a restart, after a handover -- and an
// answer spawns the shim it resumes through. IT IS ADDITIVE: a new table the
// previous build never names.
//
// cold is the shim's own conversation.v1.SessionCold, kept whole so the gate
// raised again states exactly what the shim judged; config_dir is the account
// root the parked conversation spends from.
const coldGateDDL = `
CREATE TABLE cold_gate (
  workspace_id      TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  vendor_session_id TEXT NOT NULL,
  cold              BLOB NOT NULL,
  config_dir        TEXT NOT NULL
);
`

// ColdGateRecord is one workspace's recorded cold gate.
type ColdGateRecord struct {
	// VendorSessionID is the conversation the gate stands on.
	VendorSessionID string
	// Cold is the shim's judgment of the conversation's cost.
	Cold *conversationv1.SessionCold
	// ConfigDir is the account root the parked conversation spends from.
	ConfigDir string
}

// ColdGate answers workspace ID's recorded cold gate, false when none is
// recorded.
func (s *store) ColdGate(ctx context.Context, id WorkspaceID) (ColdGateRecord, bool, error) {
	var rec ColdGateRecord
	found := false
	err := s.read(ctx, "daemon.wsm.cold_gate", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var cold []byte
		err := s.q(ctx).QueryRowContext(ctx, `SELECT vendor_session_id, cold, config_dir FROM cold_gate WHERE workspace_id = ?`, id).
			Scan(&rec.VendorSessionID, &cold, &rec.ConfigDir)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		rec.Cold = &conversationv1.SessionCold{}
		if err := proto.Unmarshal(cold, rec.Cold); err != nil {
			return fmt.Errorf("wsm: decode the cold gate of %s: %w", id, err)
		}
		found = true
		return nil
	})
	return rec, found, err
}

// SetColdGate records REC as workspace ID's cold gate, replacing any earlier
// one.
func (s *store) SetColdGate(ctx context.Context, id WorkspaceID, rec ColdGateRecord) error {
	fields := dlog.Context{"workspace": string(id), "vendor_session_id": rec.VendorSessionID}
	if rec.VendorSessionID == "" || rec.Cold == nil {
		return fmt.Errorf("wsm: cold gate of %s: a gate names its conversation and its cold facts", id)
	}
	cold, err := proto.Marshal(rec.Cold)
	if err != nil {
		return fmt.Errorf("wsm: encode the cold gate of %s: %w", id, err)
	}
	return s.write(ctx, "daemon.wsm.set_cold_gate", fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `INSERT INTO cold_gate (workspace_id, vendor_session_id, cold, config_dir) VALUES (?, ?, ?, ?)
			ON CONFLICT (workspace_id) DO UPDATE SET vendor_session_id = excluded.vendor_session_id, cold = excluded.cold, config_dir = excluded.config_dir`,
			id, rec.VendorSessionID, cold, rec.ConfigDir)
		return err
	})
}

// ClearColdGate retires workspace ID's recorded cold gate, answering whether
// one stood.
func (s *store) ClearColdGate(ctx context.Context, id WorkspaceID) (bool, error) {
	cleared := false
	err := s.write(ctx, "daemon.wsm.clear_cold_gate", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM cold_gate WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		cleared, err = touchedRow(res)
		return err
	})
	return cleared, err
}
