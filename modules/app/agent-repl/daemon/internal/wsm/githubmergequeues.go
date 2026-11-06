package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"regexp"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of GITHUB MERGE QUEUE SUBSCRIPTIONS
// (agentrepl.v1.SubscribeGithubMergeQueue, internal/prwatch): which
// workspace's agent is told about which repository branch's GitHub merge
// queue, what it was already told (TOLD, the poller's snapshot, opaque here),
// how the daemon's reads of the queue fare, and the notices it is owed. It is
// GitHub's queue, never agent-repl's own landing queue (mergequeue*.go).
//
// It keeps a pull-request subscription's guarantees, through the same
// helpers (githubnotices.go): TOLD is advanced by compare-and-set together
// with the notice it owes, in one transaction, and a subscription ends
// structurally with its subscriber's close (endWorkspaceSubscriptionsInTx).
// A queue never ends on its own, so a row has no end.

// githubMergeQueuesDDL is the layout-104 addition: the subscriptions and
// their owed notices. The fresh-file schema appends this same text
// (schema.go), so a migrated file and a created one cannot differ.
const githubMergeQueuesDDL = `
CREATE TABLE github_merge_queue_subscriptions (
  subscriber        TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  owner             TEXT NOT NULL,
  repo              TEXT NOT NULL,
  branch            TEXT NOT NULL,
  subscribed_at     INTEGER NOT NULL,
  told              TEXT NOT NULL,
  last_read         INTEGER,
  failing_since     INTEGER,
  failure_kind      TEXT,
  failure_detail    TEXT,
  failure_resets_at INTEGER,
  PRIMARY KEY (subscriber, owner, repo, branch)
);
CREATE TABLE github_merge_queue_untold_notices (
  subscriber   TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  merge_queue  TEXT NOT NULL,
  turn_id      TEXT NOT NULL,
  sentences    TEXT NOT NULL,
  pending      TEXT NOT NULL DEFAULT '[]',
  status       TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (subscriber, merge_queue)
);
`

// ownedLinksFollowTheMergeQueueDDL is the layout-105 step: every
// subscription taken for an owned link asks for every event
// (AllPullRequestEvents), which now includes PullRequestEventMergeQueue (bit
// 6, 64), so the ones taken before it gain it. IT IS BREAKING: the previous
// build refuses a row holding an event it does not declare
// (scanPullRequestSubscription), so it must be gone before the step runs.
const ownedLinksFollowTheMergeQueueDDL = `
UPDATE pull_request_subscriptions SET events = events | 64 WHERE origin = 'owned_link';
`

// githubMergeQueueTables are a merge-queue subscription's.
var githubMergeQueueTables = githubTables{
	noun:          "merge-queue",
	subscriptions: "github_merge_queue_subscriptions",
	key:           []string{"owner", "repo", "branch"},
	live:          "1",
	notices:       "github_merge_queue_untold_notices",
	subject:       "merge_queue",
}

// GithubMergeQueueKey names one repository branch's merge queue on
// github.com.
type GithubMergeQueueKey struct {
	Owner  string
	Repo   string
	Branch string
}

// String is the key as `owner/repo@branch`, the subject an untold notice is
// recorded under and the spelling every record carries. A GitHub owner or
// repository name holds no `@`, so the first one is the separator however
// the branch is named.
func (k GithubMergeQueueKey) String() string {
	return fmt.Sprintf("%s/%s@%s", k.Owner, k.Repo, k.Branch)
}

// Valid reports whether every part is set.
func (k GithubMergeQueueKey) Valid() bool { return k.Owner != "" && k.Repo != "" && k.Branch != "" }

// githubMergeQueueKeyText is `owner/repo@branch`.
var githubMergeQueueKeyText = regexp.MustCompile(`^([^/@]+)/([^/@]+)@(.+)$`)

// ParseGithubMergeQueueKey reads a key spelled `owner/repo@branch`.
func ParseGithubMergeQueueKey(text string) (GithubMergeQueueKey, error) {
	m := githubMergeQueueKeyText.FindStringSubmatch(text)
	if m == nil {
		return GithubMergeQueueKey{}, fmt.Errorf("wsm: %q is not a merge queue key", text)
	}
	return GithubMergeQueueKey{Owner: m[1], Repo: m[2], Branch: m[3]}, nil
}

// GithubMergeQueueSubscription is one workspace's subscription to one merge
// queue.
type GithubMergeQueueSubscription struct {
	Subscriber   WorkspaceID
	Queue        GithubMergeQueueKey
	SubscribedAt time.Time
	// Told is the poller's snapshot of what the subscriber was told, opaque
	// to the store. Never empty: a queue subscription is taken only from a
	// successful read.
	Told string
	// LastRead is when the latest successful read was made; zero before one.
	LastRead time.Time
	// Failing is set while reads fail.
	Failing *PullRequestFailure
}

// githubMergeQueueColumns is every subscription column, in
// scanGithubMergeQueueSubscription's order.
const githubMergeQueueColumns = `subscriber, owner, repo, branch, subscribed_at, told,
  last_read, failing_since, failure_kind, failure_detail, failure_resets_at`

// SubscribeGithubMergeQueue records SUB, or leaves the subscription its
// subscriber already holds to the same queue as it stands, and answers
// whether one was already held. The subscriber's row is checked in the same
// transaction: an unregistered subscriber is ErrNotFound, a closed one
// ErrSubscriberClosed.
func (s *store) SubscribeGithubMergeQueue(ctx context.Context, sub GithubMergeQueueSubscription) (bool, error) {
	const op = "daemon.wsm.subscribe_github_merge_queue"
	fields := dlog.Context{"subscriber": string(sub.Subscriber), "merge_queue": sub.Queue.String()}
	if !sub.Queue.Valid() || sub.Told == "" {
		err := fmt.Errorf("wsm: merge-queue subscription of %s to %s told %q", sub.Subscriber, sub.Queue, sub.Told)
		s.log.Error(op, "refused a merge-queue subscription its caller should never have made", withError(fields, err))
		return false, err
	}
	var held bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireOpenSubscriber(ctx, tx, sub.Subscriber); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx,
			`INSERT INTO github_merge_queue_subscriptions (subscriber, owner, repo, branch, subscribed_at, told) VALUES (?, ?, ?, ?, ?, ?)
			 ON CONFLICT(subscriber, owner, repo, branch) DO NOTHING`,
			sub.Subscriber, sub.Queue.Owner, sub.Queue.Repo, sub.Queue.Branch, nanos(sub.SubscribedAt), sub.Told)
		if err != nil {
			return err
		}
		inserted, err := touchedRow(res)
		held = !inserted
		return err
	})
	return held, err
}

// UnsubscribeGithubMergeQueue ends SUBSCRIBER's subscription to QUEUE, and
// answers whether one stood.
func (s *store) UnsubscribeGithubMergeQueue(ctx context.Context, subscriber WorkspaceID, queue GithubMergeQueueKey) (bool, error) {
	const op = "daemon.wsm.unsubscribe_github_merge_queue"
	var ended bool
	err := s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "merge_queue": queue.String()}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM github_merge_queue_subscriptions WHERE subscriber = ? AND owner = ? AND repo = ? AND branch = ?`,
			subscriber, queue.Owner, queue.Repo, queue.Branch)
		if err != nil {
			return err
		}
		if ended, err = touchedRow(res); err != nil {
			return err
		}
		// Nothing more is told about a queue no longer followed.
		_, err = tx.ExecContext(ctx, `DELETE FROM github_merge_queue_untold_notices WHERE subscriber = ? AND merge_queue = ?`, subscriber, queue.String())
		return err
	})
	return ended, err
}

// LiveGithubMergeQueueSubscriptions loads every subscription: what the
// poller reads.
func (s *store) LiveGithubMergeQueueSubscriptions(ctx context.Context) ([]GithubMergeQueueSubscription, error) {
	var out []GithubMergeQueueSubscription
	err := s.read(ctx, "daemon.wsm.live_github_merge_queue_subscriptions", nil, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT `+githubMergeQueueColumns+` FROM github_merge_queue_subscriptions ORDER BY owner, repo, branch, subscriber`,
			scanGithubMergeQueueSubscription)
		return err
	})
	return out, err
}

// WorkspaceGithubMergeQueueSubscriptions loads every subscription SUBSCRIBER
// holds.
func (s *store) WorkspaceGithubMergeQueueSubscriptions(ctx context.Context, subscriber WorkspaceID) ([]GithubMergeQueueSubscription, error) {
	var out []GithubMergeQueueSubscription
	err := s.read(ctx, "daemon.wsm.workspace_github_merge_queue_subscriptions", dlog.Context{"subscriber": string(subscriber)}, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT `+githubMergeQueueColumns+` FROM github_merge_queue_subscriptions WHERE subscriber = ? ORDER BY owner, repo, branch`,
			scanGithubMergeQueueSubscription, subscriber)
		return err
	})
	return out, err
}

// GithubMergeQueueAdvance is one compare-and-set of a subscription's told
// snapshot (AdvanceGithubMergeQueueTold).
type GithubMergeQueueAdvance struct {
	Subscriber WorkspaceID
	Queue      GithubMergeQueueKey
	// From is the snapshot the poller read; the advance applies only while
	// the row still holds it.
	From string
	// To is the snapshot after the activity.
	To string
	// Owed is the notice's new sentences, appended to the subscriber's owed
	// notice in the same transaction; empty when nothing is to be told.
	Owed []string
	// Status is the queue as it now stands, in the words the notice ends
	// with; it replaces the owed notice's status when Owed is set.
	Status string
	// ReadAt is when the read the advance comes from was made, which ends any
	// failing stretch.
	ReadAt time.Time
}

// AdvanceGithubMergeQueueTold applies A when the subscription still holds
// A.From, answering whether it did. False is the compare lost, or the
// subscription gone.
func (s *store) AdvanceGithubMergeQueueTold(ctx context.Context, a GithubMergeQueueAdvance) (bool, error) {
	const op = "daemon.wsm.advance_github_merge_queue_told"
	fields := dlog.Context{"subscriber": string(a.Subscriber), "merge_queue": a.Queue.String(), "owed": len(a.Owed)}
	if a.ReadAt.IsZero() {
		err := fmt.Errorf("wsm: an advance of %s's subscription to %s names no read", a.Subscriber, a.Queue)
		s.log.Error(op, "refused a merge-queue advance no read made", withError(fields, err))
		return false, err
	}
	var applied bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE github_merge_queue_subscriptions SET told = ?, `+healthyColumns+`
			 WHERE subscriber = ? AND owner = ? AND repo = ? AND branch = ? AND told = ?`,
			a.To, nanos(a.ReadAt), a.Subscriber, a.Queue.Owner, a.Queue.Repo, a.Queue.Branch, a.From)
		if err != nil {
			return err
		}
		if applied, err = touchedRow(res); err != nil || !applied || len(a.Owed) == 0 {
			return err
		}
		return appendOwedInTx(ctx, tx, githubMergeQueueTables, a.Subscriber, a.Queue.String(), a.Owed, a.Status)
	})
	return applied, err
}

// RecordGithubMergeQueueReadFailure records that a read of QUEUE failed for
// every subscription to it, starting a failing stretch at F.Since when none
// stands, and answers the subscribers whose stretch this read began.
func (s *store) RecordGithubMergeQueueReadFailure(ctx context.Context, queue GithubMergeQueueKey, f PullRequestFailure) ([]WorkspaceID, error) {
	const op = "daemon.wsm.record_github_merge_queue_read_failure"
	var began []WorkspaceID
	err := s.write(ctx, op, dlog.Context{"merge_queue": queue.String(), "kind": string(f.Kind)}, func(ctx context.Context, tx *sql.Tx) error {
		var err error
		began, err = recordGithubReadFailureInTx(ctx, tx, githubMergeQueueTables, []any{queue.Owner, queue.Repo, queue.Branch}, f)
		return err
	})
	return began, err
}

// OweGithubMergeQueueNotice appends SENTENCES to SUBSCRIBER's owed notice
// about QUEUE without touching what it was told: a notice of the
// subscription itself (its reads began failing).
func (s *store) OweGithubMergeQueueNotice(ctx context.Context, subscriber WorkspaceID, queue GithubMergeQueueKey, sentences []string) error {
	const op = "daemon.wsm.owe_github_merge_queue_notice"
	return s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "merge_queue": queue.String(), "owed": len(sentences)}, func(ctx context.Context, tx *sql.Tx) error {
		return appendOwedInTx(ctx, tx, githubMergeQueueTables, subscriber, queue.String(), sentences, "")
	})
}

// GithubMergeQueueOwed is a subscriber's notice about one queue: what it was
// last submitted as, and the sentences owed since (PullRequestOwed's
// meaning, for a queue).
type GithubMergeQueueOwed struct {
	Subscriber WorkspaceID
	Queue      GithubMergeQueueKey
	Turn       TurnID
	Sentences  []string
	Pending    []string
	Status     string
}

// OwedGithubMergeQueueNotices loads every notice with sentences still
// pending.
func (s *store) OwedGithubMergeQueueNotices(ctx context.Context) ([]GithubMergeQueueOwed, error) {
	rows, err := s.owedGithubNotices(ctx, "daemon.wsm.owed_github_merge_queue_notices", githubMergeQueueTables)
	if err != nil {
		return nil, err
	}
	out := make([]GithubMergeQueueOwed, 0, len(rows))
	for _, o := range rows {
		queue, err := ParseGithubMergeQueueKey(o.Subject)
		if err != nil {
			return nil, err
		}
		out = append(out, GithubMergeQueueOwed{Subscriber: o.Subscriber, Queue: queue, Turn: o.Turn, Sentences: o.Sentences, Pending: o.Pending, Status: o.Status})
	}
	return out, nil
}

// RecordGithubMergeQueueNoticeSubmitted records a submitted notice
// (RecordPullRequestNoticeSubmitted's meaning, for a queue).
func (s *store) RecordGithubMergeQueueNoticeSubmitted(ctx context.Context, subscriber WorkspaceID, queue GithubMergeQueueKey, turn TurnID, sentences []string, took int) error {
	return s.recordGithubNoticeSubmitted(ctx, "daemon.wsm.record_github_merge_queue_notice_submitted", githubMergeQueueTables, subscriber, queue.String(), turn, sentences, took)
}

// scanGithubMergeQueueSubscription reads one row of githubMergeQueueColumns.
func scanGithubMergeQueueSubscription(row interface{ Scan(...any) error }) (GithubMergeQueueSubscription, error) {
	var (
		sub                              GithubMergeQueueSubscription
		at                               int64
		kind, detail                     sql.NullString
		lastRead, failingSince, resetsAt sql.NullInt64
	)
	if err := row.Scan(&sub.Subscriber, &sub.Queue.Owner, &sub.Queue.Repo, &sub.Queue.Branch, &at, &sub.Told,
		&lastRead, &failingSince, &kind, &detail, &resetsAt); err != nil {
		return sub, err
	}
	sub.SubscribedAt = fromNanos(at)
	sub.LastRead, sub.Failing = scanGithubHealth(lastRead, failingSince, kind, detail, resetsAt)
	return sub, nil
}
