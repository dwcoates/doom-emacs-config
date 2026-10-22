package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// This file is the durable half of a merge queue subscriber's UNTOLD NOTICE
// (internal/mergenotice): the notice it was last submitted for one queue,
// which may not have been told yet, and every change that notice says.
//
// IT IS DURABLE BECAUSE A DEPLOY RESTARTS THE DAEMON. The notifier supersedes
// a subscriber's untold notice with the next one, which says its changes
// again; held only in memory, the untold notice was forgotten at every
// restart, so the next change submitted a second notice beside it and a busy
// subscriber's stale notices piled up again, one per restart (2026-10-22:
// thirty-eight standing for one workspace).

// mergeQueueUntoldNoticesDDL is the layout-69 addition: one row per subscriber
// and queue, keyed by the queue's key (its common dir). turn_id is the turn the
// notice was submitted as, empty when its submission failed; sentences is the
// JSON array of every change it says, oldest first. The fresh-file schema
// appends this same text (schema.go), so a migrated file and a created one
// cannot differ.
const mergeQueueUntoldNoticesDDL = `
CREATE TABLE merge_queue_untold_notices (
  subscriber TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  repo_key   TEXT NOT NULL,
  turn_id    TEXT NOT NULL,
  sentences  TEXT NOT NULL,
  PRIMARY KEY (subscriber, repo_key)
);
`

// UntoldMergeNotice is a subscriber's latest notice of one queue that may not
// have been told yet.
type UntoldMergeNotice struct {
	// Subscriber is the workspace whose agent the notice is for.
	Subscriber WorkspaceID
	// Repo is the queue the notice is about, by its queue key.
	Repo RepoKey
	// Turn is the turn the notice was submitted as; empty when its submission
	// failed and nothing was submitted.
	Turn TurnID
	// Sentences states every change the notice says, oldest first.
	Sentences []string
}

// UntoldMergeNotice answers SUBSCRIBER's untold notice of REPO's queue, and
// whether one is recorded.
func (s *store) UntoldMergeNotice(ctx context.Context, subscriber WorkspaceID, repo RepoKey) (UntoldMergeNotice, bool, error) {
	fields := dlog.Context{"subscriber": string(subscriber), "repo": string(repo)}
	var (
		out   UntoldMergeNotice
		found bool
	)
	err := s.read(ctx, "daemon.wsm.untold_merge_notice", fields, func(ctx context.Context) error {
		var (
			turn      string
			sentences string
		)
		err := s.db().QueryRowContext(ctx,
			`SELECT turn_id, sentences FROM merge_queue_untold_notices WHERE subscriber = ? AND repo_key = ?`,
			subscriber, repo).Scan(&turn, &sentences)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		var said []string
		if err := json.Unmarshal([]byte(sentences), &said); err != nil {
			return fmt.Errorf("wsm: the untold merge notice of %s for %q holds unreadable sentences: %w", subscriber, repo, err)
		}
		out = UntoldMergeNotice{Subscriber: subscriber, Repo: repo, Turn: TurnID(turn), Sentences: said}
		found = true
		return nil
	})
	return out, found, err
}

// RecordUntoldMergeNotice records N as its subscriber's untold notice of its
// queue, replacing the one recorded before. A notice that says no change is
// refused: a notice is always of at least one change.
func (s *store) RecordUntoldMergeNotice(ctx context.Context, n UntoldMergeNotice) error {
	const op = "daemon.wsm.record_untold_merge_notice"
	fields := dlog.Context{"subscriber": string(n.Subscriber), "repo": string(n.Repo), "turn": string(n.Turn), "sentences": len(n.Sentences)}
	if len(n.Sentences) == 0 {
		err := fmt.Errorf("wsm: the untold merge notice of %s for %q says no change", n.Subscriber, n.Repo)
		s.log.Error(op, "refused an untold merge notice that says no change", withError(fields, err))
		return err
	}
	said, err := json.Marshal(n.Sentences)
	if err != nil {
		s.log.Error(op, "refused an untold merge notice whose sentences cannot be encoded", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO merge_queue_untold_notices (subscriber, repo_key, turn_id, sentences) VALUES (?, ?, ?, ?)
			 ON CONFLICT(subscriber, repo_key) DO UPDATE SET turn_id = excluded.turn_id, sentences = excluded.sentences`,
			n.Subscriber, n.Repo, n.Turn, string(said))
		return err
	})
}
