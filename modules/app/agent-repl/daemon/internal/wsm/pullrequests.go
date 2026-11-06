package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of PULL-REQUEST SUBSCRIPTIONS
// (agentrepl.v1.SubscribePullRequest, internal/prwatch): which workspace's
// agent is told about which GitHub pull request, what it was already told
// (TOLD, the poller's snapshot, opaque here), how the daemon's reads of the
// pull request fare, and the notices it is owed.
//
// TOLD IS ADVANCED BY COMPARE-AND-SET, together with the notice it owes, in
// one transaction (AdvancePullRequestTold): a restart between a read and its
// write repeats the read, and a second poller during a handover loses the
// compare, so an item can be neither lost nor told twice.
//
// A SUBSCRIPTION ENDS STRUCTURALLY, as a workspace subscription does: the
// subscriber's close deletes its rows in the transaction that marks it closed
// (endWorkspaceSubscriptionsInTx), and forgetting it cascades. A pull request
// that merged or closed keeps its row, marked ended, so an owned link can say
// how its pull request ended; nothing polls an ended row.

// pullRequestsDDL is the layout-96 addition: the subscriptions, their owed
// notices, the pull request each link names (a link's ownership is layout
// 92's, linkOwnershipDDL), and the failure of each repository's main sync
// last told (mainsync.go). The
// fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const pullRequestsDDL = `
CREATE TABLE pull_request_subscriptions (
  subscriber        TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  owner             TEXT NOT NULL,
  repo              TEXT NOT NULL,
  number            INTEGER NOT NULL,
  events            INTEGER NOT NULL,
  origin            TEXT NOT NULL,
  subscribed_at     INTEGER NOT NULL,
  told              TEXT NOT NULL,
  ended             TEXT,
  ended_merge_sha   TEXT,
  last_read         INTEGER,
  failing_since     INTEGER,
  failure_kind      TEXT,
  failure_detail    TEXT,
  failure_resets_at INTEGER,
  PRIMARY KEY (subscriber, owner, repo, number)
);
CREATE TABLE pull_request_untold_notices (
  subscriber   TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  pull_request TEXT NOT NULL,
  turn_id      TEXT NOT NULL,
  sentences    TEXT NOT NULL,
  pending      TEXT NOT NULL DEFAULT '[]',
  status       TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (subscriber, pull_request)
);
CREATE TABLE main_sync_notices (
  repo_id      TEXT PRIMARY KEY REFERENCES repositories(id) ON DELETE CASCADE,
  upstream_sha TEXT NOT NULL,
  cause        TEXT NOT NULL,
  told_at      INTEGER NOT NULL
);
ALTER TABLE workspace_links ADD COLUMN pr_owner TEXT;
ALTER TABLE workspace_links ADD COLUMN pr_repo TEXT;
ALTER TABLE workspace_links ADD COLUMN pr_number INTEGER;
`

// PullRequestKey names one pull request on github.com.
type PullRequestKey struct {
	Owner  string
	Repo   string
	Number uint32
}

// String is the key as `owner/repo#number`, the subject an untold notice is
// recorded under and the spelling every record carries.
func (k PullRequestKey) String() string { return fmt.Sprintf("%s/%s#%d", k.Owner, k.Repo, k.Number) }

// Valid reports whether every part is set.
func (k PullRequestKey) Valid() bool { return k.Owner != "" && k.Repo != "" && k.Number > 0 }

// PullRequestEvent is one activity kind a subscription may ask for
// (agentrepl.v1.PullRequestNoticeEvent). ITS VALUES ARE DURABLE bit
// positions and must not be renumbered.
type PullRequestEvent int

// The events. The zero value is no event.
const (
	PullRequestEventComment PullRequestEvent = iota + 1
	PullRequestEventReview
	PullRequestEventReviewComment
	PullRequestEventState
	PullRequestEventChecks
	PullRequestEventMergeQueue
)

// pullRequestEvents is every declared event, in order.
var pullRequestEvents = []PullRequestEvent{PullRequestEventComment, PullRequestEventReview, PullRequestEventReviewComment, PullRequestEventState, PullRequestEventChecks, PullRequestEventMergeQueue}

// String names the event in records.
func (e PullRequestEvent) String() string {
	switch e {
	case PullRequestEventComment:
		return "comment"
	case PullRequestEventReview:
		return "review"
	case PullRequestEventReviewComment:
		return "review_comment"
	case PullRequestEventState:
		return "state"
	case PullRequestEventChecks:
		return "checks"
	case PullRequestEventMergeQueue:
		return "merge_queue"
	}
	return fmt.Sprintf("PullRequestEvent(%d)", int(e))
}

// PullRequestEvents is a set of events, stored as its bits.
type PullRequestEvents uint32

// NewPullRequestEvents is the set of EVENTS; a repeat is one member.
func NewPullRequestEvents(events ...PullRequestEvent) PullRequestEvents {
	var set PullRequestEvents
	for _, e := range events {
		set |= 1 << uint(e)
	}
	return set
}

// AllPullRequestEvents is every event: what an owned link's subscription asks
// for.
func AllPullRequestEvents() PullRequestEvents { return NewPullRequestEvents(pullRequestEvents...) }

// Has reports whether E is a member.
func (s PullRequestEvents) Has(e PullRequestEvent) bool { return s&(1<<uint(e)) != 0 }

// Valid reports whether the set is non-empty and holds only declared events.
func (s PullRequestEvents) Valid() bool {
	return s != 0 && s&^AllPullRequestEvents() == 0
}

// Members is every member, in declaration order.
func (s PullRequestEvents) Members() []PullRequestEvent {
	var out []PullRequestEvent
	for _, e := range pullRequestEvents {
		if s.Has(e) {
			out = append(out, e)
		}
	}
	return out
}

// String names the members in order, comma-separated.
func (s PullRequestEvents) String() string {
	var names []string
	for _, e := range s.Members() {
		names = append(names, e.String())
	}
	return strings.Join(names, ",")
}

// PullRequestOrigin is how a subscription came to be.
type PullRequestOrigin string

// The origins.
const (
	// PullRequestOriginOwnedLink is a subscription the daemon took for an
	// owned link; the link's removal or demotion ends it.
	PullRequestOriginOwnedLink PullRequestOrigin = "owned_link"
	// PullRequestOriginRequested is a subscription SubscribePullRequest took.
	PullRequestOriginRequested PullRequestOrigin = "requested"
)

// PullRequestEnd is how a followed pull request ended.
type PullRequestEnd struct {
	// Merged is true for a merge, false for a close without one.
	Merged bool
	// MergeSHA is the merge commit; empty unless Merged.
	MergeSHA string
}

// PullRequestFailureKind is why the latest read of a pull request failed
// (agentrepl.v1.PullRequestGitHubFailure's arms).
type PullRequestFailureKind string

// The kinds.
const (
	PullRequestFailureUnauthorized PullRequestFailureKind = "unauthorized"
	PullRequestFailureNotFound     PullRequestFailureKind = "not_found"
	PullRequestFailureRateLimited  PullRequestFailureKind = "rate_limited"
	PullRequestFailureUnreachable  PullRequestFailureKind = "unreachable"
)

// PullRequestFailure is a failing stretch of reads.
type PullRequestFailure struct {
	// Since is when the stretch's first read failed.
	Since time.Time
	// Kind is the latest failure's cause.
	Kind PullRequestFailureKind
	// Detail is the unreachable cause's text; empty for the other kinds.
	Detail string
	// ResetsAt is when a spent rate limit resets; zero for the other kinds.
	ResetsAt time.Time
}

// PullRequestSubscription is one workspace's subscription to one pull
// request.
type PullRequestSubscription struct {
	Subscriber   WorkspaceID
	PullRequest  PullRequestKey
	Events       PullRequestEvents
	Origin       PullRequestOrigin
	SubscribedAt time.Time
	// Told is the poller's snapshot of what the subscriber was told, opaque
	// to the store. Empty until the first successful read anchors it.
	Told string
	// Ended is set once the pull request merged or closed.
	Ended *PullRequestEnd
	// LastRead is when the latest successful read was made; zero before one.
	LastRead time.Time
	// Failing is set while reads fail.
	Failing *PullRequestFailure
}

// ErrSubscriptionEnded is a write to a subscription whose pull request has
// already ended.
var ErrSubscriptionEnded = errors.New("wsm: the pull request's subscription has ended")

// pullRequestColumns is every subscription column, in scanPullRequestSubscription's
// order.
const pullRequestColumns = `subscriber, owner, repo, number, events, origin, subscribed_at, told, ended, ended_merge_sha,
  last_read, failing_since, failure_kind, failure_detail, failure_resets_at`

// SubscribePullRequest records SUB, or replaces the events of the
// subscription its subscriber already holds to the same pull request, keeping
// what it was already told and its origin, and answers whether it replaced
// one. The subscriber's row is checked in the same transaction: an
// unregistered subscriber is ErrNotFound, a closed one ErrSubscriberClosed. A
// held subscription whose pull request ended is ErrSubscriptionEnded.
func (s *store) SubscribePullRequest(ctx context.Context, sub PullRequestSubscription) (bool, error) {
	const op = "daemon.wsm.subscribe_pull_request"
	fields := dlog.Context{"subscriber": string(sub.Subscriber), "pull_request": sub.PullRequest.String(), "events": sub.Events.String(), "origin": string(sub.Origin)}
	if !sub.Events.Valid() || !sub.PullRequest.Valid() || (sub.Origin != PullRequestOriginOwnedLink && sub.Origin != PullRequestOriginRequested) {
		err := fmt.Errorf("wsm: pull-request subscription of %s to %s: events %b, origin %q", sub.Subscriber, sub.PullRequest, uint32(sub.Events), sub.Origin)
		s.log.Error(op, "refused a pull-request subscription its caller should never have made", withError(fields, err))
		return false, err
	}
	var replaced bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireOpenSubscriber(ctx, tx, sub.Subscriber); err != nil {
			return err
		}
		var ended sql.NullString
		err := tx.QueryRowContext(ctx, `SELECT ended FROM pull_request_subscriptions WHERE subscriber = ? AND owner = ? AND repo = ? AND number = ?`,
			sub.Subscriber, sub.PullRequest.Owner, sub.PullRequest.Repo, sub.PullRequest.Number).Scan(&ended)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			_, err = tx.ExecContext(ctx,
				`INSERT INTO pull_request_subscriptions (subscriber, owner, repo, number, events, origin, subscribed_at, told) VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
				sub.Subscriber, sub.PullRequest.Owner, sub.PullRequest.Repo, sub.PullRequest.Number, int64(sub.Events), string(sub.Origin), nanos(sub.SubscribedAt), sub.Told)
			return err
		case err != nil:
			return err
		case ended.Valid:
			return ErrSubscriptionEnded
		}
		replaced = true
		_, err = tx.ExecContext(ctx, `UPDATE pull_request_subscriptions SET events = ? WHERE subscriber = ? AND owner = ? AND repo = ? AND number = ?`,
			int64(sub.Events), sub.Subscriber, sub.PullRequest.Owner, sub.PullRequest.Repo, sub.PullRequest.Number)
		return err
	})
	return replaced, err
}

// UnsubscribePullRequest ends SUBSCRIBER's subscription to PR, ended or not,
// and answers whether one stood.
func (s *store) UnsubscribePullRequest(ctx context.Context, subscriber WorkspaceID, pr PullRequestKey) (bool, error) {
	const op = "daemon.wsm.unsubscribe_pull_request"
	var ended bool
	err := s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "pull_request": pr.String()}, func(ctx context.Context, tx *sql.Tx) error {
		var err error
		ended, err = deletePullRequestSubscription(ctx, tx, subscriber, pr, "")
		return err
	})
	return ended, err
}

// LivePullRequestSubscriptions loads every subscription whose pull request
// has not ended: what the poller reads.
func (s *store) LivePullRequestSubscriptions(ctx context.Context) ([]PullRequestSubscription, error) {
	var out []PullRequestSubscription
	err := s.read(ctx, "daemon.wsm.live_pull_request_subscriptions", nil, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT `+pullRequestColumns+` FROM pull_request_subscriptions WHERE ended IS NULL ORDER BY owner, repo, number, subscriber`,
			scanPullRequestSubscription)
		return err
	})
	return out, err
}

// WorkspacePullRequestSubscriptions loads every subscription SUBSCRIBER holds,
// ended ones included.
func (s *store) WorkspacePullRequestSubscriptions(ctx context.Context, subscriber WorkspaceID) ([]PullRequestSubscription, error) {
	var out []PullRequestSubscription
	err := s.read(ctx, "daemon.wsm.workspace_pull_request_subscriptions", dlog.Context{"subscriber": string(subscriber)}, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT `+pullRequestColumns+` FROM pull_request_subscriptions WHERE subscriber = ? ORDER BY owner, repo, number`,
			scanPullRequestSubscription, subscriber)
		return err
	})
	return out, err
}

// PullRequestAdvance is one compare-and-set of a subscription's told
// snapshot (AdvancePullRequestTold).
type PullRequestAdvance struct {
	Subscriber  WorkspaceID
	PullRequest PullRequestKey
	// From is the snapshot the poller read; the advance applies only while
	// the row still holds it.
	From string
	// To is the snapshot after the activity.
	To string
	// Owed is the notice's new sentences, appended to the subscriber's owed
	// notice in the same transaction; empty when nothing is to be told.
	Owed []string
	// Status is the pull request as it now stands, in the words the notice
	// ends with; it replaces the owed notice's status when Owed is set.
	Status string
	// End marks the subscription ended; nil leaves it live.
	End *PullRequestEnd
	// ReadAt is when the read the advance comes from was made, which ends any
	// failing stretch. Zero is an advance no read made (a comment the
	// workspace posted itself), which leaves the read health as it stands.
	ReadAt time.Time
}

// AdvancePullRequestTold applies A when the subscription still holds A.From,
// answering whether it did. False is the compare lost: another write moved
// the snapshot, and the caller reads again. A subscription that is gone or
// ended is also false.
func (s *store) AdvancePullRequestTold(ctx context.Context, a PullRequestAdvance) (bool, error) {
	const op = "daemon.wsm.advance_pull_request_told"
	fields := dlog.Context{"subscriber": string(a.Subscriber), "pull_request": a.PullRequest.String(), "owed": len(a.Owed), "ends": a.End != nil}
	var applied bool
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var endedKind, mergeSHA any
		if a.End != nil {
			endedKind = "closed"
			if a.End.Merged {
				endedKind, mergeSHA = "merged", a.End.MergeSHA
			}
		}
		health := `, ` + healthyColumns
		args := []any{a.To, endedKind, mergeSHA, nanos(a.ReadAt)}
		if a.ReadAt.IsZero() {
			health, args = ``, args[:3]
		}
		res, err := tx.ExecContext(ctx,
			`UPDATE pull_request_subscriptions SET told = ?, ended = ?, ended_merge_sha = ?`+health+`
			 WHERE subscriber = ? AND owner = ? AND repo = ? AND number = ? AND told = ? AND ended IS NULL`,
			append(args, a.Subscriber, a.PullRequest.Owner, a.PullRequest.Repo, a.PullRequest.Number, a.From)...)
		if err != nil {
			return err
		}
		if applied, err = touchedRow(res); err != nil || !applied || len(a.Owed) == 0 {
			return err
		}
		return appendOwedInTx(ctx, tx, pullRequestTables, a.Subscriber, a.PullRequest.String(), a.Owed, a.Status)
	})
	return applied, err
}

// OwePullRequestNotice appends SENTENCES to SUBSCRIBER's owed notice about
// PR without touching what it was told: a notice of the subscription itself
// (its reads began failing), not of the pull request's activity.
func (s *store) OwePullRequestNotice(ctx context.Context, subscriber WorkspaceID, pr PullRequestKey, sentences []string) error {
	const op = "daemon.wsm.owe_pull_request_notice"
	return s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "pull_request": pr.String(), "owed": len(sentences)}, func(ctx context.Context, tx *sql.Tx) error {
		return appendOwedInTx(ctx, tx, pullRequestTables, subscriber, pr.String(), sentences, "")
	})
}

// RecordPullRequestReadFailure records that a read of PR failed for every
// live subscription to it, starting a failing stretch at F.Since when none
// stands, and answers the subscribers whose stretch this read began.
func (s *store) RecordPullRequestReadFailure(ctx context.Context, pr PullRequestKey, f PullRequestFailure) ([]WorkspaceID, error) {
	const op = "daemon.wsm.record_pull_request_read_failure"
	var began []WorkspaceID
	err := s.write(ctx, op, dlog.Context{"pull_request": pr.String(), "kind": string(f.Kind)}, func(ctx context.Context, tx *sql.Tx) error {
		var err error
		began, err = recordGithubReadFailureInTx(ctx, tx, pullRequestTables, []any{pr.Owner, pr.Repo, pr.Number}, f)
		return err
	})
	return began, err
}

// PullRequestOwed is a subscriber's notice about one pull request: what it
// was last submitted as, and the sentences owed since.
type PullRequestOwed struct {
	Subscriber  WorkspaceID
	PullRequest PullRequestKey
	// Turn and Sentences are the last submitted notice (UntoldWorkspaceNotice's
	// meaning); Turn is empty when its submission failed.
	Turn      TurnID
	Sentences []string
	// Pending is every sentence owed and not yet submitted, oldest first.
	Pending []string
	// Status is the pull request as it last stood, in words; empty before
	// any read told it.
	Status string
}

// OwedPullRequestNotices loads every notice with sentences still pending.
func (s *store) OwedPullRequestNotices(ctx context.Context) ([]PullRequestOwed, error) {
	rows, err := s.owedGithubNotices(ctx, "daemon.wsm.owed_pull_request_notices", pullRequestTables)
	if err != nil {
		return nil, err
	}
	out := make([]PullRequestOwed, 0, len(rows))
	for _, o := range rows {
		pr, err := ParsePullRequestKey(o.Subject)
		if err != nil {
			return nil, err
		}
		out = append(out, PullRequestOwed{Subscriber: o.Subscriber, PullRequest: pr, Turn: o.Turn, Sentences: o.Sentences, Pending: o.Pending, Status: o.Status})
	}
	return out, nil
}

// RecordPullRequestNoticeSubmitted records that SUBSCRIBER was submitted a
// notice about PR as TURN saying SENTENCES, which took the first TOOK pending
// sentences; any appended since stay pending. TURN is empty when the
// submission failed, so the next notice says SENTENCES again.
func (s *store) RecordPullRequestNoticeSubmitted(ctx context.Context, subscriber WorkspaceID, pr PullRequestKey, turn TurnID, sentences []string, took int) error {
	return s.recordGithubNoticeSubmitted(ctx, "daemon.wsm.record_pull_request_notice_submitted", pullRequestTables, subscriber, pr.String(), turn, sentences, took)
}

// ParsePullRequestKey reads a key spelled `owner/repo#number`.
func ParsePullRequestKey(text string) (PullRequestKey, error) {
	slug, number, ok := strings.Cut(text, "#")
	owner, repo, ok2 := strings.Cut(slug, "/")
	var n uint32
	if _, err := fmt.Sscanf(number, "%d", &n); !ok || !ok2 || err != nil || fmt.Sprint(n) != number {
		return PullRequestKey{}, fmt.Errorf("wsm: %q is not a pull request key", text)
	}
	key := PullRequestKey{Owner: owner, Repo: repo, Number: n}
	if !key.Valid() {
		return PullRequestKey{}, fmt.Errorf("wsm: %q is not a pull request key", text)
	}
	return key, nil
}

// deletePullRequestSubscription deletes SUBSCRIBER's subscription to PR, of
// ORIGIN when ORIGIN is set (any origin otherwise), answering whether one
// stood.
func deletePullRequestSubscription(ctx context.Context, tx *sql.Tx, subscriber WorkspaceID, pr PullRequestKey, origin PullRequestOrigin) (bool, error) {
	query := `DELETE FROM pull_request_subscriptions WHERE subscriber = ? AND owner = ? AND repo = ? AND number = ?`
	args := []any{subscriber, pr.Owner, pr.Repo, pr.Number}
	if origin != "" {
		query += ` AND origin = ?`
		args = append(args, string(origin))
	}
	res, err := tx.ExecContext(ctx, query, args...)
	if err != nil {
		return false, err
	}
	return touchedRow(res)
}

// requireOpenSubscriber answers ErrNotFound for an unregistered workspace and
// ErrSubscriberClosed for a closed one.
func requireOpenSubscriber(ctx context.Context, tx *sql.Tx, ws WorkspaceID) error {
	var closed bool
	if err := scanWorkspaceRow(ctx, tx, ws, "closed", &closed); err != nil {
		return err
	}
	if closed {
		return ErrSubscriberClosed
	}
	return nil
}

// scanPullRequestSubscription reads one row of pullRequestColumns.
func scanPullRequestSubscription(row interface{ Scan(...any) error }) (PullRequestSubscription, error) {
	var (
		sub                              PullRequestSubscription
		events, at                       int64
		origin                           string
		ended, mergeSHA, kind, detail    sql.NullString
		lastRead, failingSince, resetsAt sql.NullInt64
	)
	if err := row.Scan(&sub.Subscriber, &sub.PullRequest.Owner, &sub.PullRequest.Repo, &sub.PullRequest.Number, &events, &origin, &at, &sub.Told,
		&ended, &mergeSHA, &lastRead, &failingSince, &kind, &detail, &resetsAt); err != nil {
		return sub, err
	}
	sub.Events = PullRequestEvents(events)
	if !sub.Events.Valid() {
		return sub, fmt.Errorf("wsm: pull-request subscription of %s to %s holds undeclared events %b", sub.Subscriber, sub.PullRequest, events)
	}
	sub.Origin = PullRequestOrigin(origin)
	if sub.Origin != PullRequestOriginOwnedLink && sub.Origin != PullRequestOriginRequested {
		return sub, fmt.Errorf("wsm: pull-request subscription of %s to %s holds undeclared origin %q", sub.Subscriber, sub.PullRequest, origin)
	}
	sub.SubscribedAt = fromNanos(at)
	if ended.Valid {
		switch ended.String {
		case "merged":
			sub.Ended = &PullRequestEnd{Merged: true, MergeSHA: mergeSHA.String}
		case "closed":
			sub.Ended = &PullRequestEnd{}
		default:
			return sub, fmt.Errorf("wsm: pull-request subscription of %s to %s holds undeclared end %q", sub.Subscriber, sub.PullRequest, ended.String)
		}
	}
	sub.LastRead, sub.Failing = scanGithubHealth(lastRead, failingSince, kind, detail, resetsAt)
	return sub, nil
}
