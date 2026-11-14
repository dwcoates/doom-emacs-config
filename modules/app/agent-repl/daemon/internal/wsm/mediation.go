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
}

// mediationRolesDDL is the layout-137 addition: each owed mediation carries
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

// Prompted reports whether the mediator was told of this mediation.
func (m Mediation) Prompted() bool { return m.PromptedAt != nil }

// OweMediation records that REPO owes a mediation of the deploy that started
// AT, installing VERSION. A mediation still owed is replaced by the newer
// deploy's, never stacked: the mediator checks the window from the newest
// deploy's start, and the older deploy's decision would release nothing
// (ReleaseMediation). Its prompt is owed afresh. It answers the mediation it
// replaced, read in the same write, nil when none was owed: the replacement
// is how that mediation ended, so it is counted exactly once.
func (s *store) OweMediation(ctx context.Context, repo RepoKey, at time.Time, version string) (*Mediation, error) {
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
	var replaced *Mediation
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		prior, err := scanMediation(string(key), tx.QueryRowContext(ctx, mediationQuery, key))
		if err != nil {
			return err
		}
		replaced = prior
		// THE CHAIN PASSES TO THE NEWER DEPLOY, with its intake and its last
		// remediation: it begins only where no mediation was owed, and counts
		// a replaced mediation that had a fix dispatched as one remediation
		// deployed. The newer deploy's own dispatch is owed afresh.
		chainStart, redeploys := at, 0
		var intake, remediation WorkspaceID
		if prior != nil {
			chainStart, redeploys = prior.ChainStartedAt, prior.ChainRedeploys
			intake, remediation = prior.Intake, prior.Remediation
			if prior.Dispatched {
				redeploys++
			}
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO merge_queue_repos (repo_key, paused, mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn, mediation_chain_started_at, mediation_chain_redeploys, mediation_intake, mediation_remediation, mediation_dispatched) VALUES (?, 0, ?, NULL, ?, '', ?, ?, ?, ?, 0)
			 ON CONFLICT(repo_key) DO UPDATE SET mediation_owed_at = excluded.mediation_owed_at,
			   mediation_prompted_at = NULL, mediation_version = excluded.mediation_version, mediation_turn = '',
			   mediation_chain_started_at = excluded.mediation_chain_started_at, mediation_chain_redeploys = excluded.mediation_chain_redeploys,
			   mediation_intake = excluded.mediation_intake, mediation_remediation = excluded.mediation_remediation, mediation_dispatched = 0`,
			key, nanos(at), version, nanos(chainStart), redeploys, string(intake), string(remediation))
		return err
	})
	if err != nil {
		return nil, err
	}
	return replaced, nil
}

// mediationQuery reads one repository's mediation columns, keyed by repo.
const mediationQuery = `SELECT mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn, mediation_chain_started_at, mediation_chain_redeploys, mediation_intake, mediation_remediation, mediation_dispatched FROM merge_queue_repos WHERE repo_key = ?`

// scanMediation decodes ROW of mediationQuery, REPO's: nil when the
// repository has no row or owes no mediation. It is the one mediation
// decoding path. An owed mediation with no chain is a DecodeError: every
// write that owes one records its chain.
func scanMediation(repo string, row *sql.Row) (*Mediation, error) {
	var owed, prompted, chainStart sql.NullInt64
	var version, turn, intake, remediation string
	var redeploys int
	var dispatched bool
	err := row.Scan(&owed, &prompted, &version, &turn, &chainStart, &redeploys, &intake, &remediation, &dispatched)
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
		Intake: WorkspaceID(intake), Remediation: WorkspaceID(remediation), Dispatched: dispatched}
	if prompted.Valid {
		at := fromNanos(prompted.Int64)
		m.PromptedAt = &at
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

// ReleaseMediation clears the mediation REPO owes when it is still the one
// owed at OWED, answering whether it did: a decision on an older deploy
// releases nothing a newer deploy owes.
func (s *store) ReleaseMediation(ctx context.Context, repo RepoKey, owed time.Time) (bool, error) {
	return s.mediationWrite(ctx, "daemon.wsm.release_mediation", repo, owed,
		`UPDATE merge_queue_repos SET mediation_owed_at = NULL, mediation_prompted_at = NULL, mediation_turn = '', mediation_chain_started_at = NULL, mediation_chain_redeploys = 0, mediation_intake = '', mediation_remediation = '', mediation_dispatched = 0 WHERE repo_key = ? AND mediation_owed_at = ?`)
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

// DispatchMediation records that the intake dispatched a fix for the
// mediation REPO owes at OWED to remediation workspace WS, which becomes the
// chain's last remediation, answering false when that mediation is no
// longer the one owed or was dispatched already.
func (s *store) DispatchMediation(ctx context.Context, repo RepoKey, owed time.Time, ws WorkspaceID) (bool, error) {
	if ws == "" {
		err := errors.New("wsm: a dispatch names its remediation workspace")
		s.log.Error("daemon.wsm.dispatch_mediation", "refused a dispatch that names no workspace", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, "daemon.wsm.dispatch_mediation", repo, owed,
		`UPDATE merge_queue_repos SET mediation_remediation = ?, mediation_dispatched = 1 WHERE mediation_dispatched = 0 AND repo_key = ? AND mediation_owed_at = ?`, string(ws))
}
