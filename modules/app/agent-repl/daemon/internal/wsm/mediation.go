package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// mediationDDL is the layout-132 addition: the DEPLOY MEDIATION a
// repository owes (repoconfig.v1.LifecycleConfig.deploy_mediator), on the row
// that holds its queue's pause. A deploy owes one at its start
// (OweMediation), so a handover between the deploy and its check leaves it
// owed; the queue advances nothing while one is owed; and the mediator's
// decision releases exactly the deploy it was told of (ReleaseMediation).
// IT IS ADDITIVE: the previous build neither reads nor writes the columns.
const mediationDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN mediation_owed_at INTEGER;
ALTER TABLE merge_queue_repos ADD COLUMN mediation_prompted_at INTEGER;
ALTER TABLE merge_queue_repos ADD COLUMN mediation_version TEXT NOT NULL DEFAULT '';
ALTER TABLE merge_queue_repos ADD COLUMN mediation_turn TEXT NOT NULL DEFAULT '';
`

// Mediation is the deploy mediation a repository owes.
type Mediation struct {
	// OwedAt is the deploy's start: the instant the window its mediator
	// checks opens, and the identity of this mediation.
	OwedAt time.Time
	// PromptedAt is when the mediator was told of it; nil while it has not
	// been.
	PromptedAt *time.Time
	// Turn is the mediator's turn it was told in, set with PromptedAt: the
	// turn whose end, with the mediation still owed, is a mediation the
	// mediator decided nothing about.
	Turn TurnID
	// Version is the build the deploy installed, as its version names it;
	// empty when the deploy named none.
	Version string
	// ChainStartedAt is when the CHAIN this mediation belongs to began: the
	// deploy whose mediation no earlier one was owed beside. A mediation a
	// newer deploy replaces hands its chain on, so the chain lasts until the
	// queue advances, however many deploys remediated in between.
	ChainStartedAt time.Time
	// ChainRedeploys is how many mediations of the chain were replaced while
	// a fix was dispatched for them: the remediations deployed in a row
	// before the queue advanced.
	ChainRedeploys int
	// Intake is the chain's intake workspace, empty until it is made. Every
	// mediation of the chain carries it, so each deploy of the chain is told
	// to the same intake.
	Intake WorkspaceID
	// Remediation is the chain's last remediation workspace, empty until the
	// chain's first dispatch. A later dispatch may restore it.
	Remediation WorkspaceID
	// Dispatched reports that the intake dispatched a fix for THIS
	// mediation's deploy: the remediation workspace is at work, and the
	// queue starts no new work until its fix's deploy replaces this
	// mediation.
	Dispatched bool
	// LastDispatchedAt is when the chain's last fix was dispatched, nil
	// before its first: each remediation is measured from it to the check
	// that ends it.
	LastDispatchedAt *time.Time
	// Escalation is the escalation workspace the mediation waits on, and
	// EscalationBrief what it was escalated with: set (the brief first,
	// before the workspace is made) when the check is escalated, which holds
	// the queue until the escalation's own fix deploys or the user closes it
	// (owner ruling, 2026-11-16). Empty while the check is not escalated.
	Escalation      WorkspaceID
	EscalationBrief string
	// Resumed reports that the intake's turn on this mediation ended once
	// deciding nothing, and the intake was resumed in Turn to decide it
	// (owner ruling, 2026-11-16): a second such end is the intake's fault.
	Resumed bool
}

// Escalated reports whether the check was escalated: it holds the queue for
// the user, and no intake decides it.
func (m Mediation) Escalated() bool { return m.EscalationBrief != "" }

// mediationEscalationDDL is the layout-160 addition: the escalation an owed
// mediation waits on (Mediation.Escalation, EscalationBrief). IT IS
// ADDITIVE: the build before it neither reads nor writes the columns, so a
// mediation it owes carries no escalation.
const mediationEscalationDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN mediation_escalation TEXT NOT NULL DEFAULT '';
ALTER TABLE merge_queue_repos ADD COLUMN mediation_escalation_brief TEXT NOT NULL DEFAULT '';
`

// mediationResumedDDL is the layout-159 addition: the owed instant of the
// mediation whose intake was resumed once (Mediation.Resumed). IT NAMES THE
// MEDIATION IT WAS SET FOR, never a bare flag, so a mark left by a write
// that does not clear it (the build before this one owing a newer deploy)
// can never be read as a later mediation's. IT IS ADDITIVE: the build before
// it neither reads nor writes the column.
const mediationResumedDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN mediation_resumed_owed_at INTEGER;
`

// mediationRolesDDL is the layout-138 addition: each owed mediation carries
// its chain's intake and last remediation workspaces, and whether a fix was
// dispatched for its own deploy (Mediation.Intake, Remediation, Dispatched).
// The roles pass along the chain with it, and its release clears them.
//
// IT IS ADDITIVE: the build before it neither reads nor writes the columns.
const mediationRolesDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN mediation_intake TEXT NOT NULL DEFAULT '';
ALTER TABLE merge_queue_repos ADD COLUMN mediation_remediation TEXT NOT NULL DEFAULT '';
ALTER TABLE merge_queue_repos ADD COLUMN mediation_dispatched INTEGER NOT NULL DEFAULT 0;
`

// mediationChainDDL is the layout-135 addition: each owed mediation carries
// its chain (Mediation.ChainStartedAt, ChainRedeploys), so remediations
// deployed in a row are counted across the daemon restarts each one makes. A
// mediation already owed begins its chain at its own deploy.
//
// THE TRIGGERS KEEP EVERY OWED MEDIATION CHAINED, whichever build wrote it: a
// build from before the chain (the outgoing daemon of a handover) owes a
// mediation without one, and the trigger begins its chain at its own deploy,
// so no owed mediation is ever left without a chain (scanMediation).
const mediationChainDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN mediation_chain_started_at INTEGER;
ALTER TABLE merge_queue_repos ADD COLUMN mediation_chain_redeploys INTEGER NOT NULL DEFAULT 0;
UPDATE merge_queue_repos SET mediation_chain_started_at = mediation_owed_at WHERE mediation_owed_at IS NOT NULL;
CREATE TRIGGER merge_queue_mediation_chained_on_insert AFTER INSERT ON merge_queue_repos
WHEN NEW.mediation_owed_at IS NOT NULL AND NEW.mediation_chain_started_at IS NULL
BEGIN
  UPDATE merge_queue_repos SET mediation_chain_started_at = NEW.mediation_owed_at WHERE repo_key = NEW.repo_key;
END;
CREATE TRIGGER merge_queue_mediation_chained_on_update AFTER UPDATE OF mediation_owed_at ON merge_queue_repos
WHEN NEW.mediation_owed_at IS NOT NULL AND NEW.mediation_chain_started_at IS NULL
BEGIN
  UPDATE merge_queue_repos SET mediation_chain_started_at = NEW.mediation_owed_at WHERE repo_key = NEW.repo_key;
END;
`

// ReplacedMediation is a mediation a newer deploy replaced
// (Store.OweMediation), and whether the fix dispatched for it carries on into
// the newer deploy's mediation: true when the newer deploy is not that fix's.
type ReplacedMediation struct {
	Mediation
	DispatchCarried bool
}

// Prompted reports whether the mediator was told of this mediation.
func (m Mediation) Prompted() bool { return m.PromptedAt != nil }

// OweMediation records that REPO owes a mediation of the deploy that started
// AT, installing VERSION. A mediation still owed is replaced by the newer
// deploy's, never stacked: the mediator checks the window from the newest
// deploy's start, and the older deploy's decision would release nothing
// (ReleaseMediation). Its prompt is owed afresh. It answers the mediation it
// replaced, read in the same write, nil when none was owed: the replacement
// is how that mediation ended, so it is counted exactly once.
//
// A DISPATCHED FIX OUTLIVES A DEPLOY THAT IS NOT ITS OWN: the newer deploy
// ends the dispatch only when it is the remediation's, its finish deploying
// (deploy_remediation_finishes). Any other deploy carries the dispatch into
// its own mediation, so the remediation keeps landing directly and the
// intake decides nothing until the fix's deploy (2026-11-15 20:57: a
// landing's deploy replaced a dispatched check, and its intake approved that
// deploy and ended the chain while the remediation's fix stood unlanded).
func (s *store) OweMediation(ctx context.Context, repo RepoKey, at time.Time, version string) (*ReplacedMediation, error) {
	const op = "daemon.wsm.owe_mediation"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return nil, err
	}
	if at.IsZero() {
		err := fmt.Errorf("wsm: a mediation of %s names no deploy instant", key)
		s.log.Error(op, "refused a mediation that names no deploy instant", withError(dlog.Context{"repo": string(key)}, err))
		return nil, err
	}
	fields := dlog.Context{"repo": string(key), "owed_at": nanos(at), "version": version}
	var replaced *ReplacedMediation
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		prior, err := scanMediation(string(key), tx.QueryRowContext(ctx, mediationQuery, key))
		if err != nil {
			return err
		}
		ownDeploy := func(ws WorkspaceID) (bool, error) {
			var own int
			err := tx.QueryRowContext(ctx,
				`SELECT count(*) FROM deploy_remediation_finishes WHERE workspace_id = ? AND step = 'deploying' AND step_at <= ?`,
				string(ws), nanos(at)).Scan(&own)
			return own > 0, err
		}
		carried := false
		if prior != nil && prior.Dispatched {
			own, err := ownDeploy(prior.Remediation)
			if err != nil {
				return err
			}
			carried = !own
		}
		// AN ESCALATION HOLDS THROUGH EVERY DEPLOY BUT ITS OWN FIX'S: the
		// newer deploy's check waits on the same escalation, and only the
		// escalation's own deploy leaves a check its fresh intake decides.
		var escalation WorkspaceID
		var escalationBrief string
		if prior != nil && prior.Escalated() {
			own := false
			if prior.Escalation != "" {
				if own, err = ownDeploy(prior.Escalation); err != nil {
					return err
				}
			}
			if !own {
				escalation, escalationBrief = prior.Escalation, prior.EscalationBrief
			}
		}
		if prior != nil {
			replaced = &ReplacedMediation{Mediation: *prior, DispatchCarried: carried}
		}
		// THE CHAIN PASSES TO THE NEWER DEPLOY, with its intake and its last
		// remediation: it begins only where no mediation was owed, and counts
		// a replaced mediation that had a fix dispatched as one remediation
		// deployed. The newer deploy's own dispatch is owed afresh.
		chainStart, redeploys := at, 0
		var intake, remediation WorkspaceID
		var dispatchedAt sql.NullInt64
		if prior != nil {
			chainStart, redeploys = prior.ChainStartedAt, prior.ChainRedeploys
			remediation = prior.Remediation
			// AN INTAKE DECIDES ONCE (owner ruling, 2026-11-16): one whose
			// check was dispatched or escalated has decided, so the newer
			// deploy's check is told to a fresh intake; an undecided one
			// carries on to the newer check.
			if !prior.Dispatched && !prior.Escalated() {
				intake = prior.Intake
			}
			if prior.Dispatched && !carried {
				redeploys++
			}
			if prior.LastDispatchedAt != nil {
				dispatchedAt = sql.NullInt64{Int64: nanos(*prior.LastDispatchedAt), Valid: true}
			}
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO merge_queue_repos (repo_key, paused, mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn, mediation_chain_started_at, mediation_chain_redeploys, mediation_intake, mediation_remediation, mediation_dispatched, mediation_dispatched_at, mediation_escalation, mediation_escalation_brief) VALUES (?, 0, ?, NULL, ?, '', ?, ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(repo_key) DO UPDATE SET mediation_owed_at = excluded.mediation_owed_at,
			   mediation_prompted_at = NULL, mediation_version = excluded.mediation_version, mediation_turn = '', mediation_window = '', mediation_window_start = NULL,
			   mediation_chain_started_at = excluded.mediation_chain_started_at, mediation_chain_redeploys = excluded.mediation_chain_redeploys,
			   mediation_intake = excluded.mediation_intake, mediation_remediation = excluded.mediation_remediation, mediation_dispatched = excluded.mediation_dispatched,
			   mediation_dispatched_at = excluded.mediation_dispatched_at, mediation_escalation = excluded.mediation_escalation,
			   mediation_escalation_brief = excluded.mediation_escalation_brief`,
			key, nanos(at), version, nanos(chainStart), redeploys, string(intake), string(remediation), carried, dispatchedAt, string(escalation), escalationBrief)
		return err
	})
	if err != nil {
		return nil, err
	}
	return replaced, nil
}

// mediationQuery reads one repository's mediation columns, keyed by repo.
const mediationQuery = `SELECT mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn, mediation_chain_started_at, mediation_chain_redeploys, mediation_intake, mediation_remediation, mediation_dispatched, mediation_dispatched_at, mediation_resumed_owed_at, mediation_escalation, mediation_escalation_brief FROM merge_queue_repos WHERE repo_key = ?`

// scanMediation decodes ROW of mediationQuery, REPO's: nil when the
// repository has no row or owes no mediation. It is the one mediation
// decoding path. An owed mediation with no chain is a DecodeError: every
// write that owes one records its chain.
func scanMediation(repo string, row *sql.Row) (*Mediation, error) {
	var owed, prompted, chainStart, dispatchedAt, resumedOwed sql.NullInt64
	var version, turn, intake, remediation, escalation, escalationBrief string
	var redeploys int
	var dispatched bool
	err := row.Scan(&owed, &prompted, &version, &turn, &chainStart, &redeploys, &intake, &remediation, &dispatched, &dispatchedAt, &resumedOwed, &escalation, &escalationBrief)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, nil
	}
	if err != nil || !owed.Valid {
		return nil, err
	}
	if !chainStart.Valid {
		return nil, &DecodeError{Table: "merge_queue_repos", Row: repo, Field: "mediation_chain_started_at", Err: errors.New("an owed mediation records no chain")}
	}
	m := &Mediation{OwedAt: fromNanos(owed.Int64), Version: version, Turn: TurnID(turn), ChainStartedAt: fromNanos(chainStart.Int64), ChainRedeploys: redeploys,
		Intake: WorkspaceID(intake), Remediation: WorkspaceID(remediation), Dispatched: dispatched, Resumed: resumedOwed.Valid && resumedOwed.Int64 == owed.Int64,
		Escalation: WorkspaceID(escalation), EscalationBrief: escalationBrief}
	if prompted.Valid {
		at := fromNanos(prompted.Int64)
		m.PromptedAt = &at
	}
	if dispatchedAt.Valid {
		at := fromNanos(dispatchedAt.Int64)
		m.LastDispatchedAt = &at
	}
	return m, nil
}

// MediationOf answers the mediation REPO owes, nil when it owes none.
func (s *store) MediationOf(ctx context.Context, repo RepoKey) (*Mediation, error) {
	const op = "daemon.wsm.mediation_of"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return nil, err
	}
	var out *Mediation
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		m, err := scanMediation(string(key), s.q(ctx).QueryRowContext(ctx, mediationQuery, key))
		out = m
		return err
	})
	return out, err
}

// MarkMediationPrompted records that REPO's mediator was told, at AT, in
// TURN, of the mediation owed at OWED, answering false when that mediation is
// no longer the one owed (a newer deploy replaced it, or it was released).
func (s *store) MarkMediationPrompted(ctx context.Context, repo RepoKey, owed, at time.Time, turn TurnID) (bool, error) {
	return s.mediationWrite(ctx, "daemon.wsm.mark_mediation_prompted", repo, owed,
		`UPDATE merge_queue_repos SET mediation_prompted_at = ?, mediation_turn = ? WHERE repo_key = ? AND mediation_owed_at = ?`, nanos(at), string(turn))
}

// MarkMediationResumed records that REPO's intake, whose turn on the
// mediation owed at OWED ended deciding nothing, is resumed in TURN to decide
// it. It answers false when that mediation is no longer the one owed, was
// dispatched meanwhile, or was resumed already: the intake is resumed once.
func (s *store) MarkMediationResumed(ctx context.Context, repo RepoKey, owed time.Time, turn TurnID) (bool, error) {
	if turn == "" {
		err := errors.New("wsm: a resumed intake names the turn it is resumed in")
		s.log.Error("daemon.wsm.mark_mediation_resumed", "refused to resume an intake in no turn", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, "daemon.wsm.mark_mediation_resumed", repo, owed,
		`UPDATE merge_queue_repos SET mediation_turn = ?, mediation_resumed_owed_at = mediation_owed_at
		 WHERE repo_key = ? AND mediation_owed_at = ? AND mediation_dispatched = 0
		   AND (mediation_resumed_owed_at IS NULL OR mediation_resumed_owed_at <> mediation_owed_at)`, string(turn))
}

// EscalateMediation records that the mediation REPO owes at OWED is
// escalated with BRIEF (the escalation's summary and findings, encoded by
// its caller), before its escalation workspace is made: the check then
// holds the queue for the user, and no intake decides it. A dispatch it
// carried is over. It answers false when that mediation is no longer the
// one owed or is escalated already; an empty brief is refused.
func (s *store) EscalateMediation(ctx context.Context, repo RepoKey, owed time.Time, brief string) (bool, error) {
	if brief == "" {
		err := errors.New("wsm: an escalated mediation names what it was escalated with")
		s.log.Error("daemon.wsm.escalate_mediation", "refused to escalate a mediation with no brief", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, "daemon.wsm.escalate_mediation", repo, owed,
		`UPDATE merge_queue_repos SET mediation_escalation_brief = ?, mediation_dispatched = 0 WHERE repo_key = ? AND mediation_owed_at = ? AND mediation_escalation_brief = ''`, brief)
}

// RecordMediationEscalation records WS as the escalation workspace the
// escalated mediation REPO owes at OWED waits on. It answers false when that
// mediation is no longer the one owed or is not escalated.
func (s *store) RecordMediationEscalation(ctx context.Context, repo RepoKey, owed time.Time, ws WorkspaceID) (bool, error) {
	if ws == "" {
		err := errors.New("wsm: a mediation's escalation names its workspace")
		s.log.Error("daemon.wsm.record_mediation_escalation", "refused to record an escalation with no workspace", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, "daemon.wsm.record_mediation_escalation", repo, owed,
		`UPDATE merge_queue_repos SET mediation_escalation = ? WHERE repo_key = ? AND mediation_owed_at = ? AND mediation_escalation_brief <> ''`, string(ws))
}

// ReleaseMediation clears the mediation REPO owes when it is still the one
// owed at OWED, answering whether it did: a decision on an older deploy
// releases nothing a newer deploy owes.
func (s *store) ReleaseMediation(ctx context.Context, repo RepoKey, owed time.Time) (bool, error) {
	return s.mediationWrite(ctx, "daemon.wsm.release_mediation", repo, owed,
		`UPDATE merge_queue_repos SET mediation_owed_at = NULL, mediation_prompted_at = NULL, mediation_turn = '', mediation_resumed_owed_at = NULL, mediation_window = '', mediation_window_start = NULL, mediation_chain_started_at = NULL, mediation_chain_redeploys = 0, mediation_intake = '', mediation_remediation = '', mediation_dispatched = 0, mediation_dispatched_at = NULL, mediation_escalation = '', mediation_escalation_brief = '' WHERE repo_key = ? AND mediation_owed_at = ?`)
}

// mediationWrite runs QUERY against the mediation of REPO owed at OWED, its
// leading ARGS before the key and the instant, answering whether a row
// changed.
func (s *store) mediationWrite(ctx context.Context, op string, repo RepoKey, owed time.Time, query string, args ...any) (bool, error) {
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	changed := false
	err = s.write(ctx, op, dlog.Context{"repo": string(key), "owed_at": nanos(owed)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, query, append(args, key, nanos(owed))...)
		if err != nil {
			return err
		}
		changed, err = touchedRow(res)
		return err
	})
	return changed, err
}

// RecordMediationIntake records WS as the intake of the chain whose
// mediation REPO owes at OWED, answering false when that mediation is no
// longer the one owed. An intake already recorded is never replaced: a chain
// has one intake.
func (s *store) RecordMediationIntake(ctx context.Context, repo RepoKey, owed time.Time, ws WorkspaceID) (bool, error) {
	if ws == "" {
		err := errors.New("wsm: a chain's intake is a workspace")
		s.log.Error("daemon.wsm.record_mediation_intake", "refused an intake that names no workspace", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, "daemon.wsm.record_mediation_intake", repo, owed,
		`UPDATE merge_queue_repos SET mediation_intake = ? WHERE mediation_intake = '' AND repo_key = ? AND mediation_owed_at = ?`, string(ws))
}

// DispatchMediation records that the intake dispatched a fix, at AT, for the
// mediation REPO owes at OWED to remediation workspace WS, which becomes the
// chain's last remediation, answering false when that mediation is no
// longer the one owed or was dispatched already.
func (s *store) DispatchMediation(ctx context.Context, repo RepoKey, owed, at time.Time, ws WorkspaceID) (bool, error) {
	if ws == "" {
		err := errors.New("wsm: a dispatch names its remediation workspace")
		s.log.Error("daemon.wsm.dispatch_mediation", "refused a dispatch that names no workspace", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, "daemon.wsm.dispatch_mediation", repo, owed,
		`UPDATE merge_queue_repos SET mediation_remediation = ?, mediation_dispatched = 1, mediation_dispatched_at = ? WHERE mediation_dispatched = 0 AND repo_key = ? AND mediation_owed_at = ?`, string(ws), nanos(at))
}

// MediationRole is the owed mediation a workspace holds a role in: its
// queue, the mediation, and which role.
type MediationRole struct {
	Repo      RepoKey
	Mediation Mediation
	// Intake reports the workspace is the chain's intake; otherwise it is
	// the chain's last remediation.
	Intake bool
}

// MediationRoleOf answers the owed mediation WS is the intake or the last
// remediation of, nil when it holds neither role. A workspace holds a role
// only on the chain that recorded it, so one row answers.
func (s *store) MediationRoleOf(ctx context.Context, ws WorkspaceID) (*MediationRole, error) {
	const op = "daemon.wsm.mediation_role_of"
	if ws == "" {
		err := errors.New("wsm: a mediation role is asked of a workspace")
		s.log.Error(op, "refused a mediation role read that names no workspace", withError(nil, err))
		return nil, err
	}
	var out *MediationRole
	err := s.read(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context) error {
		var repo string
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT repo_key FROM merge_queue_repos WHERE mediation_owed_at IS NOT NULL AND (mediation_intake = ? OR mediation_remediation = ?)`,
			string(ws), string(ws)).Scan(&repo)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		m, err := scanMediation(repo, s.q(ctx).QueryRowContext(ctx, mediationQuery, repo))
		if err != nil || m == nil {
			return err
		}
		out = &MediationRole{Repo: RepoKey(repo), Mediation: *m, Intake: m.Intake == ws}
		return nil
	})
	return out, err
}
