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
	// DeployEnding is how the checked deploy ended (RecordMediationEnding),
	// empty while it has not, or for a check owed before endings were kept.
	DeployEnding DeployEnding
	// OwedAt is the deploy's start, the identity of this mediation.
	OwedAt time.Time
	// LandingStartedAt is when the landing this deploy belongs to began: the
	// earliest start of the queue's merges still running as the deploy
	// began, since a landing logs from its merge on (owner ruling,
	// 2026-11-16), or the deploy's own start when none was running (a direct
	// landing, a crash). The deploy mediator's window opens here, and the
	// production checks' span between landings closes here (owner ruling,
	// 2026-11-23).
	LandingStartedAt time.Time
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
	// Dispatched reports that the repository's deploy mediator dispatched a
	// fix of THIS mediation's deploy to itself: it is at work, and the queue
	// starts no new work until its fix's deploy replaces this mediation.
	Dispatched bool
	// LastDispatchedAt is when the chain's last fix was dispatched, nil
	// before its first: each remediation is measured from it to the check
	// that ends it.
	LastDispatchedAt *time.Time
	// EscalationBrief is what the check was escalated with: set when it is
	// escalated, which holds the queue until the mediator's fix deploys or
	// the user releases it (owner ruling, 2026-11-16), and EscalationTold
	// whether the escalation reached the mediator and the user. Empty and
	// false while the check is not escalated.
	EscalationBrief string
	EscalationTold  bool
	// FixerTurn is the latest brief the daemon gave the dispatched fixer:
	// set with the dispatch, and again by each brief after it (a failed
	// gate's). Zero while no fix is dispatched, and on a dispatch an earlier
	// build recorded.
	FixerTurn FixerTurn
	// Resumed reports that the mediator's turn on this mediation ended once
	// deciding nothing, and it was resumed in Turn to decide it (owner
	// ruling, 2026-11-16): a second such end is the mediator's fault.
	Resumed bool
}

// Escalated reports whether the check was escalated: it holds the queue for
// the user, and the mediator decides it no more.
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
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
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
		// THE MEDIATOR'S OWN DEPLOY is the one its finish was deploying
		// before: only its finishes stand in the repository's queue.
		ownDeploy := func() (bool, error) {
			var own int
			err := tx.QueryRowContext(ctx,
				`SELECT count(*) FROM deploy_remediation_finishes WHERE repo_key = ? AND step = 'deploying' AND step_at <= ?`,
				string(key), nanos(at)).Scan(&own)
			return own > 0, err
		}
		carried := false
		if prior != nil && prior.Dispatched {
			own, err := ownDeploy()
			if err != nil {
				return err
			}
			carried = !own
		}
		// AN ESCALATION HOLDS THROUGH EVERY DEPLOY BUT ITS OWN FIX'S: the
		// newer deploy's check stays escalated, and only the mediator's own
		// deploy leaves a check it decides afresh.
		var escalationBrief string
		escalationTold := false
		if prior != nil && prior.Escalated() {
			own, err := ownDeploy()
			if err != nil {
				return err
			}
			if !own {
				escalationBrief, escalationTold = prior.EscalationBrief, prior.EscalationTold
			}
		}
		if prior != nil {
			replaced = &ReplacedMediation{Mediation: *prior, DispatchCarried: carried}
		}
		landingStarted, err := landingStartedIn(ctx, tx, key, at)
		if err != nil {
			return err
		}
		// THE CHAIN PASSES TO THE NEWER DEPLOY: it begins only where no
		// mediation was owed, and counts a replaced mediation that had a fix
		// dispatched as one remediation deployed. The newer deploy's own
		// dispatch is owed afresh.
		chainStart, redeploys := at, 0
		var dispatchedAt sql.NullInt64
		var fixer FixerTurn
		if prior != nil && carried {
			fixer = prior.FixerTurn
		}
		if prior != nil {
			chainStart, redeploys = prior.ChainStartedAt, prior.ChainRedeploys
			if prior.Dispatched && !carried {
				redeploys++
			}
			if prior.LastDispatchedAt != nil {
				dispatchedAt = sql.NullInt64{Int64: nanos(*prior.LastDispatchedAt), Valid: true}
			}
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO merge_queue_repos (repo_key, paused, mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn, mediation_chain_started_at, mediation_chain_redeploys, mediation_dispatched, mediation_dispatched_at, mediation_escalation_brief, mediation_escalation_told, mediation_landing_started_at) VALUES (?, 0, ?, NULL, ?, '', ?, ?, ?, ?, ?, ?, ?)
			 ON CONFLICT(repo_key) DO UPDATE SET mediation_owed_at = excluded.mediation_owed_at, mediation_landing_started_at = excluded.mediation_landing_started_at,
			   mediation_prompted_at = NULL, mediation_version = excluded.mediation_version, mediation_turn = '', mediation_window = '', mediation_window_start = NULL,
			   mediation_chain_started_at = excluded.mediation_chain_started_at, mediation_chain_redeploys = excluded.mediation_chain_redeploys,
			   mediation_dispatched = excluded.mediation_dispatched, mediation_dispatched_at = excluded.mediation_dispatched_at,
			   mediation_escalation_brief = excluded.mediation_escalation_brief, mediation_escalation_told = excluded.mediation_escalation_told, mediation_deploy_ending = '',
			   mediation_fixer_turn = ?, mediation_fixer_brief = ?, mediation_fixer_origin = ?`,
			key, nanos(at), version, nanos(chainStart), redeploys, carried, dispatchedAt, escalationBrief, escalationTold, nanos(landingStarted),
			string(fixer.ID), fixer.Brief, fixer.Origin)
		return err
	})
	if err != nil {
		return nil, err
	}
	return replaced, nil
}

// landingStartedIn answers when the landing of KEY's deploy that began AT
// began, inside TX: the earliest opening of the ledger of a merge of KEY's
// queue still running (admitted or remediating) and opened by AT, or AT
// itself when none is.
func landingStartedIn(ctx context.Context, tx *sql.Tx, key RepoKey, at time.Time) (time.Time, error) {
	var opened sql.NullInt64
	err := tx.QueryRowContext(ctx,
		`SELECT MIN(l.opened_at) FROM merge_queue AS q JOIN merge_ledger AS l ON l.lease_id = q.ledger_id
		 WHERE q.repo_key = ? AND q.state IN (?, ?) AND l.opened_at <= ?`,
		string(key), int(MergeAdmitted), int(MergeRemediating), nanos(at)).Scan(&opened)
	if err != nil {
		return time.Time{}, fmt.Errorf("wsm: read when the landing of %s's deploy began: %w", key, err)
	}
	if !opened.Valid {
		return at, nil
	}
	return fromNanos(opened.Int64), nil
}

// landingWindowsDDL is the layout-200 addition: when each owed check's
// landing began (Mediation.LandingStartedAt). IT IS ADDITIVE: a column the
// previous build never names. A check owed before it reads its deploy's
// start.
const landingWindowsDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN mediation_landing_started_at INTEGER;
`

// mediationQuery reads one repository's mediation columns, keyed by repo.
const mediationQuery = `SELECT mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn, mediation_chain_started_at, mediation_chain_redeploys, mediation_dispatched, mediation_dispatched_at, mediation_resumed_owed_at, mediation_escalation_brief, mediation_escalation_told, mediation_deploy_ending, mediation_fixer_turn, mediation_fixer_brief, mediation_fixer_origin, mediation_landing_started_at FROM merge_queue_repos WHERE repo_key = ?`

// scanMediation decodes ROW of mediationQuery, REPO's: nil when the
// repository has no row or owes no mediation. It is the one mediation
// decoding path. An owed mediation with no chain is a DecodeError: every
// write that owes one records its chain.
func scanMediation(repo string, row *sql.Row) (*Mediation, error) {
	var owed, prompted, chainStart, dispatchedAt, resumedOwed, landingStarted sql.NullInt64
	var version, turn, escalationBrief, ending, fixerTurn, fixerBrief string
	var redeploys int
	var fixerOrigin int32
	var dispatched, escalationTold bool
	err := row.Scan(&owed, &prompted, &version, &turn, &chainStart, &redeploys, &dispatched, &dispatchedAt, &resumedOwed, &escalationBrief, &escalationTold, &ending, &fixerTurn, &fixerBrief, &fixerOrigin, &landingStarted)
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
		Dispatched: dispatched, Resumed: resumedOwed.Valid && resumedOwed.Int64 == owed.Int64,
		EscalationBrief: escalationBrief, EscalationTold: escalationTold, DeployEnding: DeployEnding(ending),
		FixerTurn: FixerTurn{ID: TurnID(fixerTurn), Brief: fixerBrief, Origin: fixerOrigin}}
	m.LandingStartedAt = m.OwedAt
	if landingStarted.Valid {
		m.LandingStartedAt = fromNanos(landingStarted.Int64)
	}
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
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
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

// MarkEscalationTold records that the escalation of the mediation REPO owes
// at OWED reached the mediator and the user. It answers false when that
// mediation is no longer the one owed or is not escalated.
func (s *store) MarkEscalationTold(ctx context.Context, repo RepoKey, owed time.Time) (bool, error) {
	return s.mediationWrite(ctx, "daemon.wsm.mark_escalation_told", repo, owed,
		`UPDATE merge_queue_repos SET mediation_escalation_told = 1 WHERE repo_key = ? AND mediation_owed_at = ? AND mediation_escalation_brief <> ''`)
}

// ReleaseMediation clears the mediation REPO owes when it is still the one
// owed at OWED, answering whether it did: a decision on an older deploy
// releases nothing a newer deploy owes.
func (s *store) ReleaseMediation(ctx context.Context, repo RepoKey, owed time.Time) (bool, error) {
	return s.mediationWrite(ctx, "daemon.wsm.release_mediation", repo, owed,
		`UPDATE merge_queue_repos SET mediation_owed_at = NULL, mediation_prompted_at = NULL, mediation_turn = '', mediation_resumed_owed_at = NULL, mediation_window = '', mediation_window_start = NULL, mediation_chain_started_at = NULL, mediation_chain_redeploys = 0, mediation_dispatched = 0, mediation_dispatched_at = NULL, mediation_escalation_brief = '', mediation_escalation_told = 0, mediation_fixer_turn = '', mediation_fixer_brief = '', mediation_fixer_origin = 0 WHERE repo_key = ? AND mediation_owed_at = ?`)
}

// mediationWrite runs QUERY against the mediation of REPO owed at OWED, its
// leading ARGS before the key and the instant, answering whether a row
// changed.
func (s *store) mediationWrite(ctx context.Context, op string, repo RepoKey, owed time.Time, query string, args ...any) (bool, error) {
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
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

// DispatchMediation records that the mediator dispatched a fix of the
// deploy whose mediation REPO owes at OWED to itself, at AT, answering false
// when that mediation is no longer the one owed or was dispatched already.
// TURN, the mediator's turn the fix is made in, is recorded in the same write
// (FixerTurn), so no dispatch stands without the turn that waits on its fix.
func (s *store) DispatchMediation(ctx context.Context, repo RepoKey, owed, at time.Time, turn FixerTurn) (bool, error) {
	if err := turn.valid(); err != nil {
		s.log.Error("daemon.wsm.dispatch_mediation", "refused a dispatch whose fixer's brief is incomplete", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, "daemon.wsm.dispatch_mediation", repo, owed,
		`UPDATE merge_queue_repos SET mediation_dispatched = 1, mediation_dispatched_at = ?, mediation_fixer_turn = ?, mediation_fixer_brief = ?, mediation_fixer_origin = ? WHERE mediation_dispatched = 0 AND repo_key = ? AND mediation_owed_at = ?`,
		nanos(at), string(turn.ID), turn.Brief, turn.Origin)
}
