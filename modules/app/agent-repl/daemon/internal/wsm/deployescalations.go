package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// deployEscalationsDDL is the layout-147 addition: each deploy escalation
// workspace the daemon made, with the queue whose deploy it settles. An
// escalation's finish lands directly, as a deploy remediation's does, never
// through the queue (owner ruling, 2026-11-15): the record is what its finish
// reads, written when the daemon makes the workspace, so no agent decides it.
const deployEscalationsDDL = `
CREATE TABLE deploy_escalations (
  workspace_id TEXT PRIMARY KEY,
  repo_key     TEXT NOT NULL,
  made_at      INTEGER NOT NULL
);
`

// RecordDeployEscalation records WS as the deploy escalation workspace of
// REPO's queue, made AT.
func (s *store) RecordDeployEscalation(ctx context.Context, ws WorkspaceID, repo RepoKey, at time.Time) error {
	const op = "daemon.wsm.record_deploy_escalation"
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo)}
	if ws == "" || repo == "" {
		err := errors.New("wsm: a deploy escalation names its workspace and its queue")
		s.log.Error(op, "refused a deploy escalation record that names no workspace or queue", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `INSERT INTO deploy_escalations (workspace_id, repo_key, made_at) VALUES (?, ?, ?)`,
			string(ws), string(repo), at.UnixMilli())
		return err
	})
}

// DeployEscalationsOf answers every deploy escalation workspace recorded for
// REPO's queue, oldest first.
func (s *store) DeployEscalationsOf(ctx context.Context, repo RepoKey) ([]WorkspaceID, error) {
	const op = "daemon.wsm.deploy_escalations_of"
	var out []WorkspaceID
	err := s.read(ctx, op, dlog.Context{"repo": string(repo)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, `SELECT workspace_id FROM deploy_escalations WHERE repo_key = ? ORDER BY made_at, workspace_id`, string(repo))
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var ws string
			if err := rows.Scan(&ws); err != nil {
				return err
			}
			out = append(out, WorkspaceID(ws))
		}
		return rows.Err()
	})
	return out, err
}

// DeployEscalationOf answers the queue WS was made the deploy escalation
// workspace of, and false when it is none.
func (s *store) DeployEscalationOf(ctx context.Context, ws WorkspaceID) (RepoKey, bool, error) {
	const op = "daemon.wsm.deploy_escalation_of"
	var repo string
	found := false
	err := s.read(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT repo_key FROM deploy_escalations WHERE workspace_id = ?`, string(ws)).Scan(&repo)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		found = true
		return nil
	})
	return RepoKey(repo), found, err
}
