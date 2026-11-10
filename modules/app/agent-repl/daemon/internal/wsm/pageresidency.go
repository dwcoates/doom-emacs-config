package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// pageResidencyDDL is the layout-120 addition: whether Emacs last held each
// workspace's page (agentrepl.v1.ReportPageResidency), kept so the daemon
// answers it on the registration Emacs makes after a bounce
// (agentrepl.v1.RegisterWorkspaceSuccess.last_known_page). Without it every
// page was loaded again at once after a restart, 77 of them in ten seconds on
// 2026-11-10, where only the handful held before were wanted. IT IS ADDITIVE:
// one new table the previous build never names.
const pageResidencyDDL = `
CREATE TABLE workspace_page_residency (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  loaded       INTEGER NOT NULL CHECK (loaded IN (0, 1)),
  reported_at  INTEGER NOT NULL
);
`

// PageLoaded answers whether Emacs last held workspace ID's page, false for
// known when Emacs never reported it.
func (s *store) PageLoaded(ctx context.Context, id WorkspaceID) (loaded, known bool, err error) {
	err = s.read(ctx, "daemon.wsm.page_loaded", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT loaded FROM workspace_page_residency WHERE workspace_id = ?`, id).Scan(&loaded)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		known = true
		return nil
	})
	return loaded, known, err
}

// SetPageLoaded records whether Emacs holds workspace ID's page, as it
// reported AT. Reporting the same fact again changes nothing but its instant.
func (s *store) SetPageLoaded(ctx context.Context, id WorkspaceID, loaded bool, at time.Time) error {
	fields := dlog.Context{"workspace": string(id), "loaded": loaded}
	return s.write(ctx, "daemon.wsm.set_page_loaded", fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_page_residency (workspace_id, loaded, reported_at) VALUES (?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET loaded = excluded.loaded, reported_at = excluded.reported_at`,
			id, loaded, at.UnixMilli())
		return err
	})
}
