package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// settingsVisitDDL is the layout-177 addition: settings_visit_at, when a
// settings pick (a model, permission-mode or effort pick on a workspace with
// no shim) started the workspace's shim, in nanoseconds, NULL while no such
// visit stands, which every existing row is.
//
// IT IS ON THE ROW, NEVER IN A DAEMON'S MEMORY, because the visit owns its
// session's stand-down (cachelapse.Keeper) and a daemon handover adopts the
// shim the visit started: held in memory, the visit died with the daemon
// that noted it, and its session was left with no stand-down at all
// (2026-11-20: 24 idle shims a 14:54 SetModel sweep revived stayed up past
// three handovers). IT IS ADDITIVE: the previous build's writes name no such
// column. The fresh-file schema appends this same text (schema.go).
const settingsVisitDDL = `
ALTER TABLE workspaces ADD COLUMN settings_visit_at INTEGER;
`

// SetSettingsVisit records that a settings pick started workspace ID's shim
// at AT. A later pick replaces the earlier instant.
func (s *store) SetSettingsVisit(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.write(ctx, "daemon.wsm.set_settings_visit", dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET settings_visit_at = ? WHERE id = ?`, nanos(at), id)
		if err != nil {
			return fmt.Errorf("wsm: record the settings visit of workspace %s: %w", id, err)
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}

// EndSettingsVisit ends workspace ID's settings visit begun at AT, and
// answers whether it did. A visit a later pick began is left standing, in the
// same statement, so ending an earlier one never ends it.
func (s *store) EndSettingsVisit(ctx context.Context, id WorkspaceID, at time.Time) (bool, error) {
	ended := false
	err := s.write(ctx, "daemon.wsm.end_settings_visit", dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET settings_visit_at = NULL WHERE id = ? AND settings_visit_at = ?`, id, nanos(at))
		if err != nil {
			return fmt.Errorf("wsm: end the settings visit of workspace %s: %w", id, err)
		}
		n, err := res.RowsAffected()
		if err != nil {
			return fmt.Errorf("wsm: end the settings visit of workspace %s: %w", id, err)
		}
		ended = n == 1
		return nil
	})
	return ended, err
}
