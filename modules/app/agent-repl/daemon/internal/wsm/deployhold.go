package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// DeployHold is one repository's hold on its deploy stage
// (agentrepl.v1.UpdateDeployHold): while Held, a deploy into the repository
// runs its pre_deploy stage, skips its deploy hook and records the skip.
type DeployHold struct {
	// Held reports whether the repository's deploy stage is held.
	Held bool
	// Since is when the hold began; zero when not held.
	Since time.Time
	// Skipped is how many deploys skipped their deploy hook since it began.
	Skipped int
}

// deployHoldDDL is the layout-67 addition: each repository's hold on its
// deploy stage, on the row that already holds its queue's pause. The
// daemon-wide rollout hold (merge_rollout_hold, layout 37) held the only deploy
// any repository had, the daemon's own, so a hold standing at the upgrade is
// carried onto EVERY registered repository: nothing that was held is deployed
// by the upgrade, and the operator releases each repository's hold as it
// wants. IT IS ADDITIVE: the previous build neither reads nor writes these
// columns, and merge_rollout_hold stays until a breaking step drops it.
const deployHoldDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN deploy_held_at INTEGER;
ALTER TABLE merge_queue_repos ADD COLUMN deploy_skipped INTEGER NOT NULL DEFAULT 0;
INSERT INTO merge_queue_repos (repo_key, paused, deploy_held_at)
  SELECT r.dir, 0, h.held_at FROM repositories r, merge_rollout_hold h WHERE true
  ON CONFLICT(repo_key) DO UPDATE SET deploy_held_at = excluded.deploy_held_at;
`

// SetDeployHold holds one repository's deploy stage from at (held true), or
// ends the hold and clears its skip count (held false). Holding an
// already-held repository keeps the ORIGINAL start, so Since always answers
// how long the deploys have been held; the orchestrator refuses that no-op
// before it gets here.
func (s *store) SetDeployHold(ctx context.Context, repo RepoKey, held bool, at time.Time) error {
	const op = "daemon.wsm.set_deploy_hold"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a repository key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return err
	}
	return s.write(ctx, op, dlog.Context{"repo": string(key), "held": held}, func(ctx context.Context, tx *sql.Tx) error {
		if held {
			_, err := tx.ExecContext(ctx,
				`INSERT INTO merge_queue_repos (repo_key, paused, deploy_held_at, deploy_skipped) VALUES (?, 0, ?, 0)
				 ON CONFLICT(repo_key) DO UPDATE SET deploy_held_at = COALESCE(merge_queue_repos.deploy_held_at, excluded.deploy_held_at)`,
				key, nanos(at))
			return err
		}
		_, err := tx.ExecContext(ctx,
			`UPDATE merge_queue_repos SET deploy_held_at = NULL, deploy_skipped = 0 WHERE repo_key = ?`, key)
		return err
	})
}

// RecordDeploySkip counts one deploy that skipped its deploy hook while the
// repository was held. A repository that is not held is refused: the skip is
// only ever recorded by a deploy that read the hold.
func (s *store) RecordDeploySkip(ctx context.Context, repo RepoKey) error {
	const op = "daemon.wsm.record_deploy_skip"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a repository key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return err
	}
	return s.write(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE merge_queue_repos SET deploy_skipped = deploy_skipped + 1 WHERE repo_key = ? AND deploy_held_at IS NOT NULL`, key)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		if n != 1 {
			return errors.New("wsm: a deploy skip was recorded on a repository that is not held")
		}
		return nil
	})
}

// DeployHoldOf reports one repository's deploy hold. A repository with no row,
// or with no hold on its row, is NOT HELD, answered without an error.
func (s *store) DeployHoldOf(ctx context.Context, repo RepoKey) (DeployHold, error) {
	const op = "daemon.wsm.deploy_hold"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a repository key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return DeployHold{}, err
	}
	var hold DeployHold
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		var heldAt sql.NullInt64
		err := s.db().QueryRowContext(ctx,
			`SELECT deploy_held_at, deploy_skipped FROM merge_queue_repos WHERE repo_key = ?`, key).Scan(&heldAt, &hold.Skipped)
		if errors.Is(err, sql.ErrNoRows) {
			hold = DeployHold{}
			return nil
		}
		if err != nil {
			return err
		}
		if heldAt.Valid {
			hold.Held, hold.Since = true, fromNanos(heldAt.Int64)
		} else {
			hold.Skipped = 0
		}
		return nil
	})
	return hold, err
}
