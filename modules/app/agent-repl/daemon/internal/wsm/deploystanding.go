package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

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
//     later one only the new check), indexed by workspace for every
//     workspace read's DeployMediator column.
//   - merge_queue_repos.deploy_quiet_checks: the checks in a row that ended
//     with no dispatch and no escalation.
//   - merge_queue_repos.deploy_compaction_armed: whether a dispatch or an
//     escalation came since the standing workspaces were last compacted, so
//     the next quiet run compacts them again.
//   - merge_queue_repos.mediation_deploy_ending: how the checked deploy
//     ended, so the intake is told whether it deployed or stopped short
//     (owner ruling, 2026-11-17: a failed build or rollout is checked too).
//
// IT IS ADDITIVE: a new table and two defaulted columns the previous build
// never names. Its table is RETIRED by layouts 168 and 169
// (deployMediatorsDDL): the daemon knows only the deploy mediator.
const deployStandingDDL = `
CREATE TABLE deploy_check_standing (
  repo_key     TEXT NOT NULL,
  role         TEXT NOT NULL CHECK (role IN ('intake', 'fixer')),
  workspace_id TEXT NOT NULL,
  briefed      INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (repo_key, role)
);
CREATE INDEX deploy_check_standing_by_workspace ON deploy_check_standing(workspace_id);
ALTER TABLE merge_queue_repos ADD COLUMN deploy_quiet_checks INTEGER NOT NULL DEFAULT 0;
ALTER TABLE merge_queue_repos ADD COLUMN deploy_compaction_armed INTEGER NOT NULL DEFAULT 0;
ALTER TABLE merge_queue_repos ADD COLUMN mediation_deploy_ending TEXT NOT NULL DEFAULT '';
`

// deployMediatorsDDL is the layout-168 addition, THE RESTART OF THE DEPLOY
// CHECKS (owner ruling, 2026-11-17): one deploy mediator per repository
// checks every deploy, fixes what it dispatches and settles an escalation
// with the user, and the daemon knows no other deploy check workspace. Every
// check starts over as though the feature never ran:
//
//   - deploy_mediators: each queue's mediator, and whether it was briefed
//     yet (its first brief states the role in full, every later one only the
//     new check), indexed by workspace for every workspace read's
//     DeployMediator column. It starts empty, so each queue's next check
//     makes a fresh mediator; the retired deploy_check_standing, which
//     named the intake and fixer workspaces, is dropped by layout 169.
//   - deploy_mediator_problems: the problems the mediator took on, with no
//     owner, since the mediator owns every one. It starts empty too; the
//     retired deploy_problems is dropped by layout 169.
//   - every check the queues owe is released, with the quiet run, the
//     compaction arming and every remediation finish under way, so no check
//     is carried into the new build. A bypass landing waits for every deploy
//     check at work to end before it lands, so none is at work across this
//     step.
//
// IT IS ADDITIVE: new tables and a defaulted column the previous build never
// names, and a release the previous build reads as any check's release.
const deployMediatorsDDL = `
CREATE TABLE deploy_mediators (
  repo_key     TEXT PRIMARY KEY CHECK (repo_key <> ''),
  workspace_id TEXT NOT NULL CHECK (workspace_id <> ''),
  briefed      INTEGER NOT NULL DEFAULT 0
);
CREATE INDEX deploy_mediators_by_workspace ON deploy_mediators(workspace_id);
CREATE TABLE deploy_mediator_problems (
  repo_key   TEXT NOT NULL CHECK (repo_key <> ''),
  operation  TEXT NOT NULL CHECK (operation <> ''),
  message    TEXT NOT NULL,
  opened_at  INTEGER NOT NULL CHECK (opened_at > 0),
  fixed_at   INTEGER,
  findings   TEXT NOT NULL DEFAULT '',
  fixes      INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (repo_key, operation, message)
);
ALTER TABLE merge_queue_repos ADD COLUMN mediation_escalation_told INTEGER NOT NULL DEFAULT 0;
UPDATE merge_queue_repos SET mediation_owed_at = NULL, mediation_prompted_at = NULL, mediation_version = '', mediation_turn = '',
  mediation_resumed_owed_at = NULL, mediation_window = '', mediation_window_start = NULL, checked_through = NULL,
  mediation_chain_started_at = NULL, mediation_chain_redeploys = 0, mediation_intake = '', mediation_remediation = '',
  mediation_dispatched = 0, mediation_dispatched_at = NULL, mediation_escalation = '', mediation_escalation_brief = '',
  mediation_deploy_ending = '', mediation_fixer_turn = '', mediation_fixer_brief = '', mediation_fixer_origin = 0,
  deploy_quiet_checks = 0, deploy_compaction_armed = 0;
DELETE FROM deploy_remediation_finishes;
`

// retiredDeployChecksDDL is the layout-169 contract: the shapes of the
// intake, fixer and escalation workspaces the deploy mediator replaced
// (deployMediatorsDDL), which no build from layout 168 on reads.
const retiredDeployChecksDDL = `
DROP TABLE deploy_check_standing;
DROP TABLE deploy_escalations;
DROP TABLE deploy_problems;
ALTER TABLE merge_queue_repos DROP COLUMN mediation_intake;
ALTER TABLE merge_queue_repos DROP COLUMN mediation_remediation;
ALTER TABLE merge_queue_repos DROP COLUMN mediation_escalation;
`

// retiredQuietCompactionDDL is the layout-178 contract: the quiet-run counter
// and the compaction arming (deployStandingDDL), which no build from layout
// 178 on reads. The deploy mediator is compacted when a remediation's deploy
// starts, and after a check that approved with nothing to fix only when its
// context passed merge.QuietCheckCompactionTokens, so no count of checks or
// arming is kept.
const retiredQuietCompactionDDL = `
ALTER TABLE merge_queue_repos DROP COLUMN deploy_quiet_checks;
ALTER TABLE merge_queue_repos DROP COLUMN deploy_compaction_armed;
`

// DeployMediator is a queue's standing mediator in one role: its deploy
// mediator, or its production mediator.
type DeployMediator struct {
	Workspace WorkspaceID
	// Briefed is whether the mediator was prompted in its role yet.
	Briefed bool
}

// mediatorTable is the table each role's standing mediators are recorded in.
// A role with no table is a defect, refused loudly.
func mediatorTable(role MediatorRole) string {
	switch role {
	case MediatorDeploy:
		return "deploy_mediators"
	case MediatorProduction:
		return "production_mediators"
	}
	panic(fmt.Sprintf("wsm: the mediator role %q has no table", role))
}

// DeployMediatorOf answers REPO's deploy mediator, and whether it has one.
func (s *store) DeployMediatorOf(ctx context.Context, repo RepoKey) (DeployMediator, bool, error) {
	return s.StandingMediatorOf(ctx, MediatorDeploy, repo)
}

// StandingMediatorOf answers REPO's mediator in ROLE, and whether it has one.
func (s *store) StandingMediatorOf(ctx context.Context, role MediatorRole, repo RepoKey) (DeployMediator, bool, error) {
	const op = "daemon.wsm.deploy_mediator_of"
	fields := dlog.Context{"repo": string(repo), "role": string(role)}
	repo, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return DeployMediator{}, false, err
	}
	var (
		out   DeployMediator
		found bool
	)
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT workspace_id, briefed FROM `+mediatorTable(role)+` WHERE repo_key = ?`, string(repo)).Scan(&out.Workspace, &out.Briefed)
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

// MediatorRepoOf answers the queue whose deploy mediator WS is, and whether
// it is one.
func (s *store) MediatorRepoOf(ctx context.Context, ws WorkspaceID) (RepoKey, bool, error) {
	return s.StandingMediatorRepoOf(ctx, MediatorDeploy, ws)
}

// StandingMediatorRepoOf answers the queue whose mediator in ROLE WS is, and
// whether it is one.
func (s *store) StandingMediatorRepoOf(ctx context.Context, role MediatorRole, ws WorkspaceID) (RepoKey, bool, error) {
	const op = "daemon.wsm.mediator_repo_of"
	var (
		repo  string
		found bool
	)
	err := s.read(ctx, op, dlog.Context{"workspace": string(ws), "role": string(role)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT repo_key FROM `+mediatorTable(role)+` WHERE workspace_id = ?`, string(ws)).Scan(&repo)
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

// RecordDeployMediator makes WS REPO's deploy mediator, unbriefed, replacing
// any mediator the queue had.
func (s *store) RecordDeployMediator(ctx context.Context, repo RepoKey, ws WorkspaceID) error {
	return s.RecordStandingMediator(ctx, MediatorDeploy, repo, ws)
}

// RecordStandingMediator makes WS REPO's mediator in ROLE, unbriefed,
// replacing any mediator the queue had in that role.
func (s *store) RecordStandingMediator(ctx context.Context, role MediatorRole, repo RepoKey, ws WorkspaceID) error {
	const op = "daemon.wsm.record_deploy_mediator"
	fields := dlog.Context{"repo": string(repo), "workspace": string(ws), "role": string(role)}
	repo, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	if repo == "" || ws == "" {
		err := errors.New("wsm: a standing mediator names its queue and its workspace")
		s.log.Error(op, "refused a standing mediator that names no queue or workspace", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO `+mediatorTable(role)+` (repo_key, workspace_id, briefed) VALUES (?, ?, 0)
			 ON CONFLICT(repo_key) DO UPDATE SET workspace_id = excluded.workspace_id, briefed = 0`,
			string(repo), string(ws))
		return err
	})
}

// MarkMediatorBriefed records that REPO's deploy mediator was briefed in its
// role. A queue with no mediator is refused: only a mediator the daemon
// recorded is briefed.
func (s *store) MarkMediatorBriefed(ctx context.Context, repo RepoKey) error {
	return s.MarkStandingMediatorBriefed(ctx, MediatorDeploy, repo)
}

// MarkStandingMediatorBriefed records that REPO's mediator in ROLE was
// briefed. A queue with no mediator in ROLE is refused.
func (s *store) MarkStandingMediatorBriefed(ctx context.Context, role MediatorRole, repo RepoKey) error {
	const op = "daemon.wsm.mark_mediator_briefed"
	fields := dlog.Context{"repo": string(repo), "role": string(role)}
	repo, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE `+mediatorTable(role)+` SET briefed = 1 WHERE repo_key = ?`, string(repo))
		if err != nil {
			return err
		}
		if n, err := res.RowsAffected(); err != nil {
			return err
		} else if n != 1 {
			return fmt.Errorf("wsm: queue %s has no %s mediator to mark briefed", repo, role)
		}
		return nil
	})
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

// DeployEnding is how a deploy ended, as its check records it.
type DeployEnding string

// The endings a deploy's edges tell (deployprogress.Edges).
const (
	// DeployFinished: every component built and rolled out.
	DeployFinished DeployEnding = "finished"
	// DeployUnchanged: it finished, every component already up to date.
	DeployUnchanged DeployEnding = "unchanged"
	// DeployStoppedShort: it stopped before finishing: a build, a restart or
	// a rollout failed or could not finish.
	DeployStoppedShort DeployEnding = "stopped_short"
	// DaemonCrashed: no deploy at all. The daemon before the one that owed
	// the check died without its orderly exit, and the check is of that
	// death (the boot's daemon.boot.predecessor_died record).
	DaemonCrashed DeployEnding = "crashed"
)

// RecordMediationEnding records ENDING on the check REPO owes for the deploy
// that began at OWED, answering whether that check is the one owed: a newer
// deploy's check takes no ending of an older one.
func (s *store) RecordMediationEnding(ctx context.Context, repo RepoKey, owed time.Time, ending DeployEnding) (bool, error) {
	const op = "daemon.wsm.record_mediation_ending"
	fields := dlog.Context{"repo": string(repo), "owed_at": nanos(owed), "ending": string(ending)}
	repo, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return false, err
	}
	switch ending {
	case DeployFinished, DeployUnchanged, DeployStoppedShort, DaemonCrashed:
	default:
		err := fmt.Errorf("wsm: %q is no deploy ending", ending)
		s.log.Error(op, "refused an ending that is none of the three", withError(fields, err))
		return false, err
	}
	var recorded bool
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE merge_queue_repos SET mediation_deploy_ending = ? WHERE repo_key = ? AND mediation_owed_at = ?`,
			string(ending), string(repo), nanos(owed))
		if err != nil {
			return err
		}
		recorded, err = touchedRow(res)
		return err
	})
	return recorded, err
}
