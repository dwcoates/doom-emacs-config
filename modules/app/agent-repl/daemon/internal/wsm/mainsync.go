package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of the MAIN SYNC's telling
// (internal/mainsync): the one failure of each repository's fast-forward its
// main workspace was last told, so the same failure found by the next sweep,
// or after a restart, is not told again. Its table is part of layout 96
// (pullRequestsDDL).

// MainSyncTold is the failure a repository's main workspace was last told.
type MainSyncTold struct {
	// UpstreamSHA is the upstream commit the failed fast-forward was to
	// reach.
	UpstreamSHA string
	// Cause names the failure's kind ("diverged", "local_changes",
	// "git_failed").
	Cause string
	At    time.Time
}

// MainSyncTold answers the failure REPO's main workspace was last told, and
// whether one was.
func (s *store) MainSyncTold(ctx context.Context, repo RepoID) (MainSyncTold, bool, error) {
	var (
		out   MainSyncTold
		found bool
	)
	err := s.read(ctx, "daemon.wsm.main_sync_told", dlog.Context{"repo_id": string(repo)}, func(ctx context.Context) error {
		var at int64
		err := s.q(ctx).QueryRowContext(ctx, `SELECT upstream_sha, cause, told_at FROM main_sync_notices WHERE repo_id = ?`, repo).Scan(&out.UpstreamSHA, &out.Cause, &at)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		out.At, found = fromNanos(at), true
		return nil
	})
	return out, found, err
}

// RecordMainSyncTold records TOLD as the failure REPO's main workspace was
// last told, replacing the one before.
func (s *store) RecordMainSyncTold(ctx context.Context, repo RepoID, told MainSyncTold) error {
	return s.write(ctx, "daemon.wsm.record_main_sync_told", dlog.Context{"repo_id": string(repo), "upstream_sha": told.UpstreamSHA, "cause": told.Cause},
		func(ctx context.Context, tx *sql.Tx) error {
			_, err := tx.ExecContext(ctx, `INSERT INTO main_sync_notices (repo_id, upstream_sha, cause, told_at) VALUES (?, ?, ?, ?)
				ON CONFLICT(repo_id) DO UPDATE SET upstream_sha = excluded.upstream_sha, cause = excluded.cause, told_at = excluded.told_at`,
				repo, told.UpstreamSHA, told.Cause, nanos(told.At))
			return err
		})
}
