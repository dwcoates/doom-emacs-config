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
}

// Prompted reports whether the mediator was told of this mediation.
func (m Mediation) Prompted() bool { return m.PromptedAt != nil }

// OweMediation records that REPO owes a mediation of the deploy that started
// AT, installing VERSION. A mediation still owed is replaced by the newer
// deploy's, never stacked: the mediator checks the window from the newest
// deploy's start, and the older deploy's decision would release nothing
// (ReleaseMediation). Its prompt is owed afresh.
func (s *store) OweMediation(ctx context.Context, repo RepoKey, at time.Time, version string) error {
	const op = "daemon.wsm.owe_mediation"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return err
	}
	if at.IsZero() {
		err := fmt.Errorf("wsm: a mediation of %s names no deploy instant", key)
		s.log.Error(op, "refused a mediation that names no deploy instant", withError(dlog.Context{"repo": string(key)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "owed_at": nanos(at), "version": version}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO merge_queue_repos (repo_key, paused, mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn) VALUES (?, 0, ?, NULL, ?, '')
			 ON CONFLICT(repo_key) DO UPDATE SET mediation_owed_at = excluded.mediation_owed_at,
			   mediation_prompted_at = NULL, mediation_version = excluded.mediation_version, mediation_turn = ''`,
			key, nanos(at), version)
		return err
	})
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
		var owed, prompted sql.NullInt64
		var version, turn string
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn FROM merge_queue_repos WHERE repo_key = ?`, key).
			Scan(&owed, &prompted, &version, &turn)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil || !owed.Valid {
			return err
		}
		m := &Mediation{OwedAt: fromNanos(owed.Int64), Version: version, Turn: TurnID(turn)}
		if prompted.Valid {
			at := fromNanos(prompted.Int64)
			m.PromptedAt = &at
		}
		out = m
		return nil
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
		`UPDATE merge_queue_repos SET mediation_owed_at = NULL, mediation_prompted_at = NULL, mediation_turn = '' WHERE repo_key = ? AND mediation_owed_at = ?`)
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
		n, err := res.RowsAffected()
		changed = n > 0
		return err
	})
	return changed, err
}
