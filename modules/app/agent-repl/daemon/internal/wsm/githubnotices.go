package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// This file is what every GITHUB SUBSCRIPTION shares, whatever it follows (a
// pull request, pullrequests.go; a merge queue, githubmergequeues.go): the
// subscriber's owed notice about one subject, and the health of the daemon's
// reads of it. Each kind keeps its own tables, named by a githubTables, so
// one kind's rows never mix with another's.

// githubTables names one subscription kind's tables and how its rows are
// keyed.
type githubTables struct {
	// noun names the kind in errors (`pull-request`).
	noun string
	// subscriptions is the subscription table.
	subscriptions string
	// key is the subscription table's columns naming the subject, in the
	// order keyArgs answers them.
	key []string
	// live is the condition a subscription row the poller still reads
	// meets.
	live string
	// notices is the owed-notice table, and subject its column naming the
	// subject by its text.
	notices, subject string
}

// pullRequestTables are a pull-request subscription's.
var pullRequestTables = githubTables{
	noun:          "pull-request",
	subscriptions: "pull_request_subscriptions",
	key:           []string{"owner", "repo", "number"},
	live:          "ended IS NULL",
	notices:       "pull_request_untold_notices",
	subject:       "pull_request",
}

// keyWhere is the condition matching one subject's subscription rows.
func (t githubTables) keyWhere() string {
	parts := make([]string, len(t.key))
	for i, column := range t.key {
		parts[i] = column + " = ?"
	}
	return strings.Join(parts, " AND ")
}

// githubOwed is a subscriber's owed notice about one subject, its subject
// spelled as text; each kind reads the text back into its key.
type githubOwed struct {
	Subscriber WorkspaceID
	Subject    string
	Turn       TurnID
	Sentences  []string
	Pending    []string
	Status     string
}

// owedGithubNotices loads every notice in T with sentences still pending.
func (s *store) owedGithubNotices(ctx context.Context, op string, t githubTables) ([]githubOwed, error) {
	var out []githubOwed
	err := s.read(ctx, op, nil, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT subscriber, `+t.subject+`, turn_id, sentences, pending, status FROM `+t.notices+`
			WHERE pending != '[]' ORDER BY subscriber, `+t.subject,
			func(row interface{ Scan(...any) error }) (githubOwed, error) {
				var (
					o                  githubOwed
					sentences, pending string
				)
				if err := row.Scan(&o.Subscriber, &o.Subject, &o.Turn, &sentences, &pending, &o.Status); err != nil {
					return o, err
				}
				if err := json.Unmarshal([]byte(sentences), &o.Sentences); err != nil {
					return o, fmt.Errorf("wsm: the untold %s notice of %s about %s holds unreadable sentences: %w", t.noun, o.Subscriber, o.Subject, err)
				}
				if err := json.Unmarshal([]byte(pending), &o.Pending); err != nil {
					return o, fmt.Errorf("wsm: the untold %s notice of %s about %s holds unreadable pending sentences: %w", t.noun, o.Subscriber, o.Subject, err)
				}
				return o, nil
			})
		return err
	})
	return out, err
}

// recordGithubNoticeSubmitted records that SUBSCRIBER was submitted a notice
// about SUBJECT as TURN saying SENTENCES, which took the first TOOK pending
// sentences; any appended since stay pending. TURN is empty when the
// submission failed, so the next notice says SENTENCES again.
func (s *store) recordGithubNoticeSubmitted(ctx context.Context, op string, t githubTables, subscriber WorkspaceID, subject string, turn TurnID, sentences []string, took int) error {
	fields := dlog.Context{"subscriber": string(subscriber), t.subject: subject, "turn": string(turn), "sentences": len(sentences), "took": took}
	if len(sentences) == 0 {
		err := fmt.Errorf("wsm: the untold %s notice of %s about %s says no change", t.noun, subscriber, subject)
		s.log.Error(op, "refused an untold "+t.noun+" notice that says no change", withError(fields, err))
		return err
	}
	said, err := json.Marshal(sentences)
	if err != nil {
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var raw string
		if err := tx.QueryRowContext(ctx, `SELECT pending FROM `+t.notices+` WHERE subscriber = ? AND `+t.subject+` = ?`,
			subscriber, subject).Scan(&raw); err != nil {
			return err
		}
		var pending []string
		if err := json.Unmarshal([]byte(raw), &pending); err != nil {
			return err
		}
		if took > len(pending) {
			return fmt.Errorf("wsm: the notice of %s about %s took %d pending sentences of %d", subscriber, subject, took, len(pending))
		}
		rest, err := json.Marshal(append([]string{}, pending[took:]...))
		if err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx, `UPDATE `+t.notices+` SET turn_id = ?, sentences = ?, pending = ? WHERE subscriber = ? AND `+t.subject+` = ?`,
			turn, string(said), string(rest), subscriber, subject)
		return err
	})
}

// appendOwedInTx appends SENTENCES to SUBSCRIBER's owed notice about
// SUBJECT in T, creating the notice row when none stands, and sets its
// status when STATUS is not empty.
func appendOwedInTx(ctx context.Context, tx *sql.Tx, t githubTables, subscriber WorkspaceID, subject string, sentences []string, status string) error {
	var raw string
	err := tx.QueryRowContext(ctx, `SELECT pending FROM `+t.notices+` WHERE subscriber = ? AND `+t.subject+` = ?`,
		subscriber, subject).Scan(&raw)
	var pending []string
	switch {
	case errors.Is(err, sql.ErrNoRows):
	case err != nil:
		return err
	default:
		if err := json.Unmarshal([]byte(raw), &pending); err != nil {
			return fmt.Errorf("wsm: the untold %s notice of %s about %s holds unreadable pending sentences: %w", t.noun, subscriber, subject, err)
		}
	}
	encoded, err := json.Marshal(append(pending, sentences...))
	if err != nil {
		return err
	}
	_, err = tx.ExecContext(ctx,
		`INSERT INTO `+t.notices+` (subscriber, `+t.subject+`, turn_id, sentences, pending, status) VALUES (?, ?, '', '[]', ?, ?)
		 ON CONFLICT(subscriber, `+t.subject+`) DO UPDATE SET pending = excluded.pending,
		   status = CASE WHEN excluded.status = '' THEN status ELSE excluded.status END`,
		subscriber, subject, string(encoded), status)
	return err
}

// recordGithubReadFailureInTx records that a read of the subject KEYARGS
// names failed for every live subscription to it in T, starting a failing
// stretch at F.Since when none stands, and answers the subscribers whose
// stretch this read began.
func recordGithubReadFailureInTx(ctx context.Context, tx *sql.Tx, t githubTables, keyArgs []any, f PullRequestFailure) ([]WorkspaceID, error) {
	began, err := listRows(ctx, tx, `SELECT subscriber FROM `+t.subscriptions+`
		WHERE `+t.keyWhere()+` AND `+t.live+` AND failing_since IS NULL ORDER BY subscriber`,
		func(row interface{ Scan(...any) error }) (WorkspaceID, error) {
			var id WorkspaceID
			return id, row.Scan(&id)
		}, keyArgs...)
	if err != nil {
		return nil, err
	}
	var resets any
	if !f.ResetsAt.IsZero() {
		resets = nanos(f.ResetsAt)
	}
	_, err = tx.ExecContext(ctx, `UPDATE `+t.subscriptions+` SET
		  failing_since = COALESCE(failing_since, ?), failure_kind = ?, failure_detail = ?, failure_resets_at = ?
		WHERE `+t.keyWhere()+` AND `+t.live,
		append([]any{nanos(f.Since), string(f.Kind), f.Detail, resets}, keyArgs...)...)
	return began, err
}

// healthyColumns is the assignment that ends a failing stretch, recording
// the successful read's time as its one argument.
const healthyColumns = `last_read = ?, failing_since = NULL, failure_kind = NULL, failure_detail = NULL, failure_resets_at = NULL`

// scanGithubHealth reads the five health columns every GitHub subscription
// row ends with: when the latest successful read was made (zero before one),
// and the failing stretch, nil while reads succeed.
func scanGithubHealth(lastRead, failingSince sql.NullInt64, kind, detail sql.NullString, resetsAt sql.NullInt64) (time.Time, *PullRequestFailure) {
	var (
		read    time.Time
		failing *PullRequestFailure
	)
	if lastRead.Valid {
		read = fromNanos(lastRead.Int64)
	}
	if failingSince.Valid {
		failing = &PullRequestFailure{Since: fromNanos(failingSince.Int64), Kind: PullRequestFailureKind(kind.String), Detail: detail.String}
		if resetsAt.Valid {
			failing.ResetsAt = fromNanos(resetsAt.Int64)
		}
	}
	return read, failing
}
