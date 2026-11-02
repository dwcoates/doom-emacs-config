package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of MERGE QUEUE SUBSCRIPTIONS
// (agentrepl.v1.UpdateMergeQueueSubscribe): which workspace's agent is told
// about which repository's merge queue. A subscription is
// DURABLE because a landing in the daemon's own checkout redeploys and
// restarts the daemon: one that a restart dropped would end at the very change
// it exists to report.
//
// A SUBSCRIPTION ENDS WITH ITS WORKSPACE'S CLOSE, STRUCTURALLY. SetClosed
// deletes the closing workspace's subscriptions in the transaction that marks
// it closed, and Forget's delete of the workspace row cascades to them, so no
// close path -- whichever verb or landing it runs through -- can leave one
// standing. A closed workspace cannot take one either (ErrSubscriberClosed).
//
// A SUBSCRIPTION ALSO ENDS WITH ITS QUEUE'S DEPLETION. The notifier ends every
// subscription in the depleted queue's scope as it delivers the depletion
// (internal/mergenotice), through EndMergeQueueSubscription, which ends the
// subscription only as the notifier read it: one its subscriber took again
// after the read is a new subscription, and stands.

// mergeQueueSubscriptionsDDL is the layout-55 addition: one row per
// subscription, keyed by its subscriber and its scope. The scope is the
// repository's queue key (its common dir, as merge_queue.repo_key holds it),
// or the empty string for the every-repository subscription; a NULL cannot take part in a
// primary key's uniqueness, so the every-repository scope is the empty key.
// The fresh-file schema appends this same text (schema.go), so a migrated
// file and a created one cannot differ.
const mergeQueueSubscriptionsDDL = `
CREATE TABLE merge_queue_subscriptions (
  subscriber    TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  repo_key      TEXT NOT NULL,
  detail        INTEGER NOT NULL,
  subscribed_at INTEGER NOT NULL,
  PRIMARY KEY (subscriber, repo_key)
);
`

// EveryRepository is the scope of a subscription to every repository's merge
// queue: MergeQueueSubscription.Repo when the subscription names none.
const EveryRepository RepoKey = ""

// legacyMergeQueueDetail is what every row's detail column holds. A
// subscription once chose how much of a queue's change it was told; every one
// is now told the same changes (internal/mergenotice), and nothing reads the
// column. IT IS STILL WRITTEN because the build before this one reads it and
// refuses a row without a detail it knows, and both builds serve the store
// across a handover. 4 is that build's "deploys", the level whose notices are
// closest to what every subscription is now told. Dropping the column is a
// breaking layout step, left until no running build reads it (org/todo.md).
const legacyMergeQueueDetail = 4

// MergeQueueSubscription is one workspace's subscription to one scope's merge
// queue changes.
type MergeQueueSubscription struct {
	// Subscriber is the workspace whose agent is told.
	Subscriber WorkspaceID
	// Repo is the queue subscribed to, by its queue key; EveryRepository
	// subscribes to every repository's queue.
	Repo RepoKey
	// SubscribedAt is when the subscription was taken, or last taken again.
	SubscribedAt time.Time
}

// ErrSubscriberClosed is a subscription refused because its subscriber is
// closed: a close is what ends a subscription, so one taken while closed
// would never end.
var ErrSubscriberClosed = errors.New("wsm: a closed workspace cannot subscribe")

// SubscribeMergeQueue records SUB, standing in for the subscription its
// subscriber already holds in the same scope, and answers whether it replaced
// one. The subscriber's open state is checked in the same
// transaction, so a close cannot slip between the check and the write.
func (s *store) SubscribeMergeQueue(ctx context.Context, sub MergeQueueSubscription) (bool, error) {
	const op = "daemon.wsm.subscribe_merge_queue"
	fields := dlog.Context{"subscriber": string(sub.Subscriber), "repo": string(sub.Repo)}
	repo, err := subscriptionScope(sub.Repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue subscription whose scope cannot be normalized", withError(fields, err))
		return false, err
	}
	var replaced bool
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var closed bool
		err := tx.QueryRowContext(ctx, `SELECT closed FROM workspaces WHERE id = ?`, sub.Subscriber).Scan(&closed)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return fmt.Errorf("wsm: merge queue subscription of %s: %w", sub.Subscriber, ErrNotFound)
		case err != nil:
			return err
		case closed:
			return ErrSubscriberClosed
		}
		var held int
		if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM merge_queue_subscriptions WHERE subscriber = ? AND repo_key = ?`,
			sub.Subscriber, repo).Scan(&held); err != nil {
			return err
		}
		replaced = held > 0
		_, err = tx.ExecContext(ctx,
			`INSERT INTO merge_queue_subscriptions (subscriber, repo_key, detail, subscribed_at) VALUES (?, ?, ?, ?)
			 ON CONFLICT(subscriber, repo_key) DO UPDATE SET detail = excluded.detail, subscribed_at = excluded.subscribed_at`,
			sub.Subscriber, repo, legacyMergeQueueDetail, nanos(sub.SubscribedAt))
		return err
	})
	return replaced, err
}

// UnsubscribeMergeQueue ends SUBSCRIBER's subscription in scope REPO, and
// answers whether one stood there. It ends nothing in any other scope.
func (s *store) UnsubscribeMergeQueue(ctx context.Context, subscriber WorkspaceID, repo RepoKey) (bool, error) {
	return s.endSubscription(ctx, "daemon.wsm.unsubscribe_merge_queue", "refused a merge queue unsubscribe whose scope cannot be normalized",
		dlog.Context{"subscriber": string(subscriber), "repo": string(repo)}, subscriber, repo, "")
}

// EndMergeQueueSubscription ends SUB as it was read: its subscriber's
// subscription in its scope, only while it still holds SUB's SubscribedAt. It answers whether it ended one. A subscription the
// subscriber took again since SUB was read is not SUB, and stands.
func (s *store) EndMergeQueueSubscription(ctx context.Context, sub MergeQueueSubscription) (bool, error) {
	return s.endSubscription(ctx, "daemon.wsm.end_merge_queue_subscription", "refused to end a merge queue subscription whose scope cannot be normalized",
		dlog.Context{"subscriber": string(sub.Subscriber), "repo": string(sub.Repo)},
		sub.Subscriber, sub.Repo, ` AND subscribed_at = ?`, nanos(sub.SubscribedAt))
}

// endSubscription deletes SUBSCRIBER's subscription in scope REPO, narrowed by
// the extra WHERE clause MATCH and its ARGS, and answers whether it deleted
// one. Every end of a single subscription goes through it, so each normalizes
// its scope and records its refusal the same way.
func (s *store) endSubscription(ctx context.Context, op, refused string, fields dlog.Context, subscriber WorkspaceID, repo RepoKey, match string, args ...any) (bool, error) {
	key, err := subscriptionScope(repo)
	if err != nil {
		s.log.Error(op, refused, withError(fields, err))
		return false, err
	}
	var ended bool
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM merge_queue_subscriptions WHERE subscriber = ? AND repo_key = ?`+match,
			append([]any{subscriber, key}, args...)...)
		if err != nil {
			return err
		}
		ended, err = touchedRow(res)
		return err
	})
	return ended, err
}

// MergeQueueSubscriptions loads every subscription, by subscriber then scope,
// all-or-nothing.
func (s *store) MergeQueueSubscriptions(ctx context.Context) ([]MergeQueueSubscription, error) {
	var out []MergeQueueSubscription
	err := s.read(ctx, "daemon.wsm.merge_queue_subscriptions", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT subscriber, repo_key, subscribed_at FROM merge_queue_subscriptions ORDER BY subscriber, repo_key`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []MergeQueueSubscription
		for rows.Next() {
			var (
				sub MergeQueueSubscription
				at  int64
			)
			if err := rows.Scan(&sub.Subscriber, &sub.Repo, &at); err != nil {
				return err
			}
			sub.SubscribedAt = fromNanos(at)
			loaded = append(loaded, sub)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
}

// endSubscriptionsInTx deletes every subscription WS holds, inside the
// caller's transaction, answering how many it ended. SetClosed calls it in
// the transaction that marks WS closed.
func endSubscriptionsInTx(ctx context.Context, tx *sql.Tx, ws WorkspaceID) (int64, error) {
	res, err := tx.ExecContext(ctx, `DELETE FROM merge_queue_subscriptions WHERE subscriber = ?`, ws)
	if err != nil {
		return 0, err
	}
	// A CLOSED WORKSPACE IS TOLD NOTHING MORE, so its untold notices go with
	// its subscriptions (mergenotices.go).
	if _, err := tx.ExecContext(ctx, `DELETE FROM merge_queue_untold_notices WHERE subscriber = ?`, ws); err != nil {
		return 0, err
	}
	return res.RowsAffected()
}

// subscriptionScope normalizes a subscription's scope the way the queue keys
// it, leaving the every-repository scope as it is.
func subscriptionScope(repo RepoKey) (RepoKey, error) {
	if repo == EveryRepository {
		return EveryRepository, nil
	}
	return normalizeRepoKey(repo)
}
