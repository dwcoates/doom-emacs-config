package wsm

import (
	"context"
	"database/sql"
	"errors"

	"agent-repld/internal/dlog"
)

// queueDrainOwedDDL is the layout-94 addition: whether a repository owes the
// scripts its deploy config runs once its queue fully depletes
// (repoconfig.v1.DeployConfig.queue_drained), on the row that already holds
// its queue's pause. AdmitMerge sets it in the write that admits a merge, so
// no admitted merge can leave the repository without it, whatever restart
// follows; ClearQueueDrainOwed clears it once the scripts have all succeeded.
// IT IS ADDITIVE: the previous build neither reads nor writes the column.
const queueDrainOwedDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN drain_owed INTEGER NOT NULL DEFAULT 0;
`

// oweQueueDrain marks the repository KEY as owing its queue_drained scripts,
// inside the transaction TX that admits one of its merges.
func oweQueueDrain(ctx context.Context, tx *sql.Tx, key RepoKey) error {
	_, err := tx.ExecContext(ctx,
		`INSERT INTO merge_queue_repos (repo_key, paused, drain_owed) VALUES (?, 0, 1)
		 ON CONFLICT(repo_key) DO UPDATE SET drain_owed = 1`, key)
	return err
}

// QueueDrainOwed reports whether a merge was admitted into the repository's
// queue since its queue_drained scripts last all succeeded. A repository with
// no row has never admitted a merge, and owes nothing.
func (s *store) QueueDrainOwed(ctx context.Context, repo RepoKey) (bool, error) {
	const op = "daemon.wsm.queue_drain_owed"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	var owed bool
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT drain_owed FROM merge_queue_repos WHERE repo_key = ?`, key).Scan(&owed)
		if errors.Is(err, sql.ErrNoRows) {
			owed = false
			return nil
		}
		return err
	})
	return owed, err
}

// ClearQueueDrainOwed records that the repository's queue_drained scripts
// have all succeeded, so it owes them again only once another merge is
// admitted. A repository with no row owes nothing, so clearing it is a no-op.
func (s *store) ClearQueueDrainOwed(ctx context.Context, repo RepoKey) error {
	const op = "daemon.wsm.clear_queue_drain_owed"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo)}, err))
		return err
	}
	return s.write(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `UPDATE merge_queue_repos SET drain_owed = 0 WHERE repo_key = ?`, key)
		return err
	})
}
