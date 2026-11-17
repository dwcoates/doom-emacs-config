package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"slices"

	"agent-repld/internal/dlog"
)

// deployStandingDDL is the layout-164 addition: the STANDING deploy check
// workspaces (owner ruling, 2026-11-17). A repository whose queue checks its
// deploys keeps one intake and one fixer workspace (a remediation and an
// escalation in one)
// across every deploy, so each role's conversation carries what the earlier
// checks found and fixed; the daemon prompts the standing one rather than
// making a workspace per check.
//
//   - deploy_check_standing: each queue's workspace in each role, and whether
//     it was briefed yet (its first brief states the role in full, every
//     later one only the new check).
//   - merge_queue_repos.deploy_quiet_checks: the checks in a row that ended
//     with no dispatch and no escalation.
//   - merge_queue_repos.deploy_compaction_armed: whether a dispatch or an
//     escalation came since the standing workspaces were last compacted, so
//     the next quiet run compacts them again.
//
// IT IS ADDITIVE: a new table and two defaulted columns the previous build
// never names.
const deployStandingDDL = `
CREATE TABLE deploy_check_standing (
  repo_key     TEXT NOT NULL,
  role         TEXT NOT NULL CHECK (role IN ('intake', 'fixer')),
  workspace_id TEXT NOT NULL,
  briefed      INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (repo_key, role)
);
ALTER TABLE merge_queue_repos ADD COLUMN deploy_quiet_checks INTEGER NOT NULL DEFAULT 0;
ALTER TABLE merge_queue_repos ADD COLUMN deploy_compaction_armed INTEGER NOT NULL DEFAULT 0;
`

// DeployCheckRoleName is one standing deploy check role as the store spells it.
type DeployCheckRoleName string

// The two standing roles.
const (
	StandingIntake      DeployCheckRoleName = "intake"
	// StandingFixer both remediates a dispatch and settles an escalation with
	// the user (owner ruling, 2026-11-17): one workspace, two briefs.
	StandingFixer DeployCheckRoleName = "fixer"
)

// StandingDeployCheckRoles is every standing role, in the order a deploy
// meets them.
var StandingDeployCheckRoles = []DeployCheckRoleName{StandingIntake, StandingFixer}

// QuietChecksBeforeCompaction is how many checks in a row must end with no
// dispatch and no escalation before an armed queue's standing workspaces are
// compacted (owner ruling, 2026-11-17: "a period of three consecutive merges").
const QuietChecksBeforeCompaction = 3

// StandingDeployCheck is a queue's workspace in one role.
type StandingDeployCheck struct {
	Workspace WorkspaceID
	// Briefed is whether the workspace was prompted in its role yet.
	Briefed bool
}

// validStandingRole refuses a role outside the two.
func validStandingRole(role DeployCheckRoleName) error {
	if slices.Contains(StandingDeployCheckRoles, role) {
		return nil
	}
	return fmt.Errorf("wsm: %q is no standing deploy check role", role)
}

// StandingDeployCheckOf answers REPO's standing workspace in ROLE, and
// whether it has one.
func (s *store) StandingDeployCheckOf(ctx context.Context, repo RepoKey, role DeployCheckRoleName) (StandingDeployCheck, bool, error) {
	repo, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error("daemon.wsm.standing_deploy_check", "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return StandingDeployCheck{}, false, err
	}
	var (
		out   StandingDeployCheck
		found bool
	)
	fields := dlog.Context{"repo": string(repo), "role": string(role)}
	if err := validStandingRole(role); err != nil {
		s.log.Error("daemon.wsm.standing_deploy_check", "refused a read of a role that is not a standing one", withError(fields, err))
		return out, false, err
	}
	err = s.read(ctx, "daemon.wsm.standing_deploy_check", fields, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT workspace_id, briefed FROM deploy_check_standing WHERE repo_key = ? AND role = ?`,
			string(repo), string(role)).Scan(&out.Workspace, &out.Briefed)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		found = true
		return nil
	})
	return out, found, err
}

// RecordStandingDeployCheck makes WS REPO's standing workspace in ROLE,
// unbriefed, replacing any workspace the role had.
func (s *store) RecordStandingDeployCheck(ctx context.Context, repo RepoKey, role DeployCheckRoleName, ws WorkspaceID) error {
	repo, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error("daemon.wsm.record_standing_deploy_check", "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return err
	}
	const op = "daemon.wsm.record_standing_deploy_check"
	fields := dlog.Context{"repo": string(repo), "role": string(role), "workspace": string(ws)}
	if err := validStandingRole(role); err != nil {
		s.log.Error(op, "refused a standing workspace in a role that is not a standing one", withError(fields, err))
		return err
	}
	if repo == "" || ws == "" {
		err := errors.New("wsm: a standing deploy check names its queue and its workspace")
		s.log.Error(op, "refused a standing deploy check that names no queue or workspace", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO deploy_check_standing (repo_key, role, workspace_id, briefed) VALUES (?, ?, ?, 0)
			 ON CONFLICT(repo_key, role) DO UPDATE SET workspace_id = excluded.workspace_id, briefed = 0`,
			string(repo), string(role), string(ws))
		return err
	})
}

// MarkStandingBriefed records that REPO's standing workspace in ROLE was
// briefed in its role. A role with no standing workspace is refused: only a
// workspace the daemon recorded is briefed.
func (s *store) MarkStandingBriefed(ctx context.Context, repo RepoKey, role DeployCheckRoleName) error {
	repo, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error("daemon.wsm.mark_standing_briefed", "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return err
	}
	const op = "daemon.wsm.mark_standing_briefed"
	return s.write(ctx, op, dlog.Context{"repo": string(repo), "role": string(role)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE deploy_check_standing SET briefed = 1 WHERE repo_key = ? AND role = ?`, string(repo), string(role))
		if err != nil {
			return err
		}
		if n, err := res.RowsAffected(); err != nil {
			return err
		} else if n != 1 {
			return fmt.Errorf("wsm: queue %s has no standing %s workspace to mark briefed", repo, role)
		}
		return nil
	})
}

// NoteDeployCheckWork records that a check of REPO dispatched a fix or was
// escalated: the quiet run starts over, and the standing workspaces owe a
// compaction once a quiet run completes.
func (s *store) NoteDeployCheckWork(ctx context.Context, repo RepoKey) error {
	repo, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error("daemon.wsm.note_deploy_check_work", "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return err
	}
	const op = "daemon.wsm.note_deploy_check_work"
	return s.write(ctx, op, dlog.Context{"repo": string(repo)}, func(ctx context.Context, tx *sql.Tx) error {
		return updateQueueRow(ctx, tx, repo,
			`UPDATE merge_queue_repos SET deploy_quiet_checks = 0, deploy_compaction_armed = 1 WHERE repo_key = ?`)
	})
}

// NoteQuietDeployCheck records that a check of REPO ended with no dispatch
// and no escalation, answering whether the quiet run just completed on an
// armed queue: the caller then compacts its standing workspaces, and the
// queue is disarmed until a dispatch or an escalation arms it again.
func (s *store) NoteQuietDeployCheck(ctx context.Context, repo RepoKey) (bool, error) {
	repo, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error("daemon.wsm.note_quiet_deploy_check", "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	const op = "daemon.wsm.note_quiet_deploy_check"
	var compact bool
	err = s.write(ctx, op, dlog.Context{"repo": string(repo)}, func(ctx context.Context, tx *sql.Tx) error {
		var quiet int
		var armed bool
		err := tx.QueryRowContext(ctx,
			`UPDATE merge_queue_repos SET deploy_quiet_checks = deploy_quiet_checks + 1 WHERE repo_key = ?
			 RETURNING deploy_quiet_checks, deploy_compaction_armed`, string(repo)).Scan(&quiet, &armed)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: no merge queue %s to note a quiet deploy check on", repo)
		}
		if err != nil {
			return err
		}
		if !armed || quiet < QuietChecksBeforeCompaction {
			return nil
		}
		compact = true
		_, err = tx.ExecContext(ctx, `UPDATE merge_queue_repos SET deploy_quiet_checks = 0, deploy_compaction_armed = 0 WHERE repo_key = ?`, string(repo))
		return err
	})
	return compact, err
}

// updateQueueRow runs QUERY, whose one argument is REPO, against REPO's
// queue row, refusing a queue that has none.
func updateQueueRow(ctx context.Context, tx *sql.Tx, repo RepoKey, query string) error {
	res, err := tx.ExecContext(ctx, query, string(repo))
	if err != nil {
		return err
	}
	if n, err := res.RowsAffected(); err != nil {
		return err
	} else if n != 1 {
		return fmt.Errorf("wsm: no merge queue %s", repo)
	}
	return nil
}

// OweCompaction records that workspace WS's next session start compacts the
// conversation it resumes (OwedStartWork.Compact), answering whether it now
// owes it. A workspace with no conversation to resume owes nothing, and one
// that owes start work already keeps what it owes.
func (s *store) OweCompaction(ctx context.Context, ws WorkspaceID) (bool, error) {
	const op = "daemon.wsm.owe_compaction"
	var owed bool
	err := s.write(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context, tx *sql.Tx) error {
		var vendor string
		err := tx.QueryRowContext(ctx, `SELECT vendor_session_id FROM sessions WHERE workspace_id = ?`, string(ws)).Scan(&vendor)
		if errors.Is(err, sql.ErrNoRows) || (err == nil && vendor == "") {
			return nil
		}
		if err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `INSERT INTO owed_start_work (workspace_id, compact) VALUES (?, 1) ON CONFLICT(workspace_id) DO NOTHING`, string(ws))
		if err != nil {
			return err
		}
		owed, err = touchedRow(res)
		return err
	})
	return owed, err
}
