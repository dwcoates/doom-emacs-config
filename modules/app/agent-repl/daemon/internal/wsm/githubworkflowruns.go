package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of GITHUB WORKFLOW-RUN SUBSCRIPTIONS
// (agentrepl.v1.SubscribeGithubWorkflowRun, internal/prwatch): which
// workspace's agent waits on which GitHub Actions run, how the daemon's reads
// of the run fare, and the one notice each is owed when the run completes.
//
// It keeps the other GitHub kinds' notice guarantees, through the same
// helpers (githubnotices.go), and ends structurally with its subscriber's
// close (endGithubSubscriptionsInTx). A run's subscription is told nothing
// before the run completes, so a row holds no told snapshot: the completion
// REMOVES the row in the same transaction that owes its notice
// (OweGithubWorkflowRunCompletion), so a run is owed once per subscription
// and never read again.

// githubWorkflowRunsDDL is the layout-184 addition: the subscriptions and
// their owed notices. The fresh-file schema appends this same text
// (schema.go), so a migrated file and a created one cannot differ.
const githubWorkflowRunsDDL = `
CREATE TABLE github_workflow_run_subscriptions (
  subscriber        TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  owner             TEXT NOT NULL,
  repo              TEXT NOT NULL,
  run_id            INTEGER NOT NULL,
  subscribed_at     INTEGER NOT NULL,
  last_read         INTEGER,
  failing_since     INTEGER,
  failure_kind      TEXT,
  failure_detail    TEXT,
  failure_resets_at INTEGER,
  PRIMARY KEY (subscriber, owner, repo, run_id)
);
CREATE TABLE github_workflow_run_untold_notices (
  subscriber    TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  workflow_run  TEXT NOT NULL,
  turn_id       TEXT NOT NULL,
  sentences     TEXT NOT NULL,
  pending       TEXT NOT NULL DEFAULT '[]',
  status        TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (subscriber, workflow_run)
);
`

// githubWorkflowRunTables are a workflow-run subscription's.
var githubWorkflowRunTables = githubTables{
	noun:          "workflow-run",
	subscriptions: "github_workflow_run_subscriptions",
	key:           []string{"owner", "repo", "run_id"},
	live:          "1",
	notices:       "github_workflow_run_untold_notices",
	subject:       "workflow_run",
}

// GithubWorkflowRunKey names one GitHub Actions workflow run.
type GithubWorkflowRunKey struct {
	Owner string
	Repo  string
	// RunID is GitHub's id of the run; ids exceed 32 bits.
	RunID uint64
}

// String is the key as `owner/repo/runs/id`, the subject an untold notice is
// recorded under and the spelling every record carries. A GitHub owner or
// repository name holds no `/`, so it reads back unambiguously.
func (k GithubWorkflowRunKey) String() string {
	return fmt.Sprintf("%s/%s/runs/%d", k.Owner, k.Repo, k.RunID)
}

// Valid reports whether every part is set.
func (k GithubWorkflowRunKey) Valid() bool { return k.Owner != "" && k.Repo != "" && k.RunID != 0 }

// githubWorkflowRunKeyText is `owner/repo/runs/id`.
var githubWorkflowRunKeyText = regexp.MustCompile(`^([^/]+)/([^/]+)/runs/([1-9][0-9]*)$`)

// ParseGithubWorkflowRunKey reads a key spelled `owner/repo/runs/id`.
func ParseGithubWorkflowRunKey(text string) (GithubWorkflowRunKey, error) {
	m := githubWorkflowRunKeyText.FindStringSubmatch(text)
	if m == nil {
		return GithubWorkflowRunKey{}, fmt.Errorf("wsm: %q is not a workflow run key", text)
	}
	id, err := strconv.ParseUint(m[3], 10, 64)
	if err != nil {
		return GithubWorkflowRunKey{}, fmt.Errorf("wsm: %q is not a workflow run key: %w", text, err)
	}
	return GithubWorkflowRunKey{Owner: m[1], Repo: m[2], RunID: id}, nil
}

// GithubWorkflowRunSubscription is one workspace's subscription to one run.
type GithubWorkflowRunSubscription struct {
	Subscriber   WorkspaceID
	Run          GithubWorkflowRunKey
	SubscribedAt time.Time
	// LastRead is when the latest successful read was made; zero before one.
	LastRead time.Time
	// Failing is set while reads fail.
	Failing *PullRequestFailure
}

// githubWorkflowRunColumns is every subscription column, in
// scanGithubWorkflowRunSubscription's order.
const githubWorkflowRunColumns = `subscriber, owner, repo, run_id, subscribed_at,
  last_read, failing_since, failure_kind, failure_detail, failure_resets_at`

// SubscribeGithubWorkflowRun records SUB, or leaves the subscription its
// subscriber already holds to the same run as it stands, and answers
// whether one was already held. The subscriber's row is checked in the same
// transaction: an unregistered subscriber is ErrNotFound, a closed one
// ErrSubscriberClosed.
func (s *store) SubscribeGithubWorkflowRun(ctx context.Context, sub GithubWorkflowRunSubscription) (bool, error) {
	const op = "daemon.wsm.subscribe_github_workflow_run"
	fields := dlog.Context{"subscriber": string(sub.Subscriber), "workflow_run": sub.Run.String()}
	if !sub.Run.Valid() || sub.LastRead.IsZero() {
		err := fmt.Errorf("wsm: workflow-run subscription of %s to %s read at %v", sub.Subscriber, sub.Run, sub.LastRead)
		s.log.Error(op, "refused a workflow-run subscription its caller should never have made", withError(fields, err))
		return false, err
	}
	var held bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireOpenSubscriber(ctx, tx, sub.Subscriber); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx,
			`INSERT INTO github_workflow_run_subscriptions (subscriber, owner, repo, run_id, subscribed_at, last_read) VALUES (?, ?, ?, ?, ?, ?)
			 ON CONFLICT(subscriber, owner, repo, run_id) DO NOTHING`,
			sub.Subscriber, sub.Run.Owner, sub.Run.Repo, int64(sub.Run.RunID), nanos(sub.SubscribedAt), nanos(sub.LastRead))
		if err != nil {
			return err
		}
		inserted, err := touchedRow(res)
		held = !inserted
		return err
	})
	return held, err
}

// UnsubscribeGithubWorkflowRun ends SUBSCRIBER's subscription to RUN, and
// answers whether one stood.
func (s *store) UnsubscribeGithubWorkflowRun(ctx context.Context, subscriber WorkspaceID, run GithubWorkflowRunKey) (bool, error) {
	const op = "daemon.wsm.unsubscribe_github_workflow_run"
	var ended bool
	err := s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "workflow_run": run.String()}, func(ctx context.Context, tx *sql.Tx) error {
		var err error
		if ended, err = deleteGithubWorkflowRunSubscription(ctx, tx, subscriber, run); err != nil {
			return err
		}
		// Nothing more is told about a run no longer waited on.
		_, err = tx.ExecContext(ctx, `DELETE FROM github_workflow_run_untold_notices WHERE subscriber = ? AND workflow_run = ?`, subscriber, run.String())
		return err
	})
	return ended, err
}

// deleteGithubWorkflowRunSubscription removes SUBSCRIBER's row for RUN,
// answering whether one stood.
func deleteGithubWorkflowRunSubscription(ctx context.Context, tx *sql.Tx, subscriber WorkspaceID, run GithubWorkflowRunKey) (bool, error) {
	res, err := tx.ExecContext(ctx, `DELETE FROM github_workflow_run_subscriptions WHERE subscriber = ? AND owner = ? AND repo = ? AND run_id = ?`,
		subscriber, run.Owner, run.Repo, int64(run.RunID))
	if err != nil {
		return false, err
	}
	return touchedRow(res)
}

// LiveGithubWorkflowRunSubscriptions loads every subscription: what the
// poller reads.
func (s *store) LiveGithubWorkflowRunSubscriptions(ctx context.Context) ([]GithubWorkflowRunSubscription, error) {
	var out []GithubWorkflowRunSubscription
	err := s.read(ctx, "daemon.wsm.live_github_workflow_run_subscriptions", nil, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT `+githubWorkflowRunColumns+` FROM github_workflow_run_subscriptions ORDER BY owner, repo, run_id, subscriber`,
			scanGithubWorkflowRunSubscription)
		return err
	})
	return out, err
}

// WorkspaceGithubWorkflowRunSubscriptions loads every subscription
// SUBSCRIBER holds.
func (s *store) WorkspaceGithubWorkflowRunSubscriptions(ctx context.Context, subscriber WorkspaceID) ([]GithubWorkflowRunSubscription, error) {
	var out []GithubWorkflowRunSubscription
	err := s.read(ctx, "daemon.wsm.workspace_github_workflow_run_subscriptions", dlog.Context{"subscriber": string(subscriber)}, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT `+githubWorkflowRunColumns+` FROM github_workflow_run_subscriptions WHERE subscriber = ? ORDER BY owner, repo, run_id`,
			scanGithubWorkflowRunSubscription, subscriber)
		return err
	})
	return out, err
}

// RecordGithubWorkflowRunRead records that a read of RUN at READAT succeeded
// and found it unfinished, ending any failing stretch of every subscription
// to it.
func (s *store) RecordGithubWorkflowRunRead(ctx context.Context, run GithubWorkflowRunKey, readAt time.Time) error {
	const op = "daemon.wsm.record_github_workflow_run_read"
	fields := dlog.Context{"workflow_run": run.String()}
	if readAt.IsZero() {
		err := fmt.Errorf("wsm: a read of %s names no time", run)
		s.log.Error(op, "refused a workflow-run read no read made", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `UPDATE github_workflow_run_subscriptions SET `+healthyColumns+` WHERE owner = ? AND repo = ? AND run_id = ?`,
			nanos(readAt), run.Owner, run.Repo, int64(run.RunID))
		return err
	})
}

// GithubWorkflowRunCompletion is a run's completion owed to one subscriber
// (OweGithubWorkflowRunCompletion).
type GithubWorkflowRunCompletion struct {
	Subscriber WorkspaceID
	Run        GithubWorkflowRunKey
	// Sentences is the notice's words; never empty.
	Sentences []string
	// Status is the run as it finished, in the words the notice ends with.
	Status string
	// Held is whether the completion ends a subscription the subscriber
	// holds. True (the poller's) owes nothing when the subscription is gone,
	// so a run unsubscribed from, or completed already, is never owed twice;
	// false (a subscribe to a run already finished) owes it whatever is held,
	// ending any subscription that stands.
	Held bool
}

// OweGithubWorkflowRunCompletion removes C's subscription and owes its
// notice, in one transaction, answering whether the notice was owed. The
// subscriber must be open: an unregistered one is ErrNotFound, a closed one
// ErrSubscriberClosed.
func (s *store) OweGithubWorkflowRunCompletion(ctx context.Context, c GithubWorkflowRunCompletion) (bool, error) {
	const op = "daemon.wsm.owe_github_workflow_run_completion"
	fields := dlog.Context{"subscriber": string(c.Subscriber), "workflow_run": c.Run.String(), "held": c.Held}
	if !c.Run.Valid() || len(c.Sentences) == 0 {
		err := fmt.Errorf("wsm: the completion of %s owed to %s says %q", c.Run, c.Subscriber, c.Sentences)
		s.log.Error(op, "refused a workflow-run completion its caller should never have owed", withError(fields, err))
		return false, err
	}
	var owed bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireOpenSubscriber(ctx, tx, c.Subscriber); err != nil {
			return err
		}
		ended, err := deleteGithubWorkflowRunSubscription(ctx, tx, c.Subscriber, c.Run)
		if err != nil {
			return err
		}
		if c.Held && !ended {
			return nil
		}
		owed = true
		return appendOwedInTx(ctx, tx, githubWorkflowRunTables, c.Subscriber, c.Run.String(), c.Sentences, c.Status)
	})
	return owed, err
}

// RecordGithubWorkflowRunReadFailure records that a read of RUN failed for
// every subscription to it, starting a failing stretch at F.Since when none
// stands, and answers the subscribers whose stretch this read began.
func (s *store) RecordGithubWorkflowRunReadFailure(ctx context.Context, run GithubWorkflowRunKey, f PullRequestFailure) ([]WorkspaceID, error) {
	const op = "daemon.wsm.record_github_workflow_run_read_failure"
	var began []WorkspaceID
	err := s.write(ctx, op, dlog.Context{"workflow_run": run.String(), "kind": string(f.Kind)}, func(ctx context.Context, tx *sql.Tx) error {
		var err error
		began, err = recordGithubReadFailureInTx(ctx, tx, githubWorkflowRunTables, []any{run.Owner, run.Repo, int64(run.RunID)}, f)
		return err
	})
	return began, err
}

// OweGithubWorkflowRunNotice appends SENTENCES to SUBSCRIBER's owed notice
// about RUN while its subscription stands: a notice of the subscription
// itself (GitHub rejected the credentials), never of the run.
func (s *store) OweGithubWorkflowRunNotice(ctx context.Context, subscriber WorkspaceID, run GithubWorkflowRunKey, sentences []string) error {
	const op = "daemon.wsm.owe_github_workflow_run_notice"
	return s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "workflow_run": run.String(), "owed": len(sentences)}, func(ctx context.Context, tx *sql.Tx) error {
		return appendOwedInTx(ctx, tx, githubWorkflowRunTables, subscriber, run.String(), sentences, "")
	})
}

// GithubWorkflowRunOwed is a subscriber's notice about one run: what it was
// last submitted as, and the sentences owed since (PullRequestOwed's
// meaning, for a run).
type GithubWorkflowRunOwed struct {
	Subscriber WorkspaceID
	Run        GithubWorkflowRunKey
	Turn       TurnID
	Sentences  []string
	Pending    []string
	Status     string
}

// OwedGithubWorkflowRunNotices loads every notice with sentences still
// pending.
func (s *store) OwedGithubWorkflowRunNotices(ctx context.Context) ([]GithubWorkflowRunOwed, error) {
	rows, err := s.owedGithubNotices(ctx, "daemon.wsm.owed_github_workflow_run_notices", githubWorkflowRunTables)
	if err != nil {
		return nil, err
	}
	out := make([]GithubWorkflowRunOwed, 0, len(rows))
	for _, o := range rows {
		run, err := ParseGithubWorkflowRunKey(o.Subject)
		if err != nil {
			return nil, err
		}
		out = append(out, GithubWorkflowRunOwed{Subscriber: o.Subscriber, Run: run, Turn: o.Turn, Sentences: o.Sentences, Pending: o.Pending, Status: o.Status})
	}
	return out, nil
}

// RecordGithubWorkflowRunNoticeSubmitted records a submitted notice
// (RecordPullRequestNoticeSubmitted's meaning, for a run).
func (s *store) RecordGithubWorkflowRunNoticeSubmitted(ctx context.Context, subscriber WorkspaceID, run GithubWorkflowRunKey, turn TurnID, sentences []string, took int) error {
	return s.recordGithubNoticeSubmitted(ctx, "daemon.wsm.record_github_workflow_run_notice_submitted", githubWorkflowRunTables, subscriber, run.String(), turn, sentences, took)
}

// scanGithubWorkflowRunSubscription reads one row of
// githubWorkflowRunColumns.
func scanGithubWorkflowRunSubscription(row interface{ Scan(...any) error }) (GithubWorkflowRunSubscription, error) {
	var (
		sub                              GithubWorkflowRunSubscription
		at, runID                        int64
		kind, detail                     sql.NullString
		lastRead, failingSince, resetsAt sql.NullInt64
	)
	if err := row.Scan(&sub.Subscriber, &sub.Run.Owner, &sub.Run.Repo, &runID, &at,
		&lastRead, &failingSince, &kind, &detail, &resetsAt); err != nil {
		return sub, err
	}
	if runID <= 0 {
		return sub, errors.New("wsm: a workflow-run subscription row holds no run id")
	}
	sub.Run.RunID = uint64(runID)
	sub.SubscribedAt = fromNanos(at)
	sub.LastRead, sub.Failing = scanGithubHealth(lastRead, failingSince, kind, detail, resetsAt)
	return sub, nil
}
