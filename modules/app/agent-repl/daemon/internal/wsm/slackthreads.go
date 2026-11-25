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
	"agent-repld/internal/slacklink"
)

// This file is the durable half of SLACK-THREAD SUBSCRIPTIONS
// (agentrepl.v1.SubscribeSlackThread, internal/slackwatch): which workspace's
// agent is told about which Slack thread and in which mode, the daemon's
// cursor into the thread (its channel's echo pipe, or a poll of the thread
// itself), the messages waiting for the next
// digest, the last digest submitted (so a newer one can supersede it while it
// is still held), and the nonce of every post the daemon made for the
// subscription.
//
// MESSAGES AND THE CURSOR ADVANCE TOGETHER (RecordSlackThreadMessages): a
// restart between a read and its write repeats the read, and the pending
// table's key drops a message stored twice, so a message is neither lost nor
// told twice.
//
// A WORKSPACE IS NEVER TOLD ITS OWN POST. A post's nonce row is written before
// the post is sent (ReserveSlackThreadPost), and a message carrying one of the
// subscription's nonces is dropped in the transaction that would store it.
//
// A SUBSCRIPTION ENDS STRUCTURALLY: the subscriber's close deletes its rows in
// the transaction that marks it closed (endWorkspaceSubscriptionsInTx).

// slackThreadsDDL is the layout-195 addition: the thread subscriptions and
// their pending messages and post nonces, and the reaction initiator's
// settings, bindings and fired ledger (slackreactions.go). The fresh-file
// schema appends this same text (schema.go), so a migrated file and a created
// one cannot differ. IT IS ADDITIVE: six new tables the previous build never
// names.
const slackThreadsDDL = `
CREATE TABLE slack_thread_subscriptions (
  subscriber      TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  channel         TEXT NOT NULL,
  thread_ts       TEXT NOT NULL,
  thread_url      TEXT NOT NULL,
  mode            TEXT NOT NULL,
  origin          TEXT NOT NULL,
  subscribed_at   INTEGER NOT NULL,
  cursor_seq      INTEGER NOT NULL,
  posts_made      INTEGER NOT NULL DEFAULT 0,
  failing_since   INTEGER,
  failure_detail  TEXT,
  untold_turn     TEXT NOT NULL DEFAULT '',
  untold_messages TEXT NOT NULL DEFAULT '[]',
  PRIMARY KEY (subscriber, channel, thread_ts)
);
CREATE TABLE slack_thread_pending (
  subscriber  TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  channel     TEXT NOT NULL,
  thread_ts   TEXT NOT NULL,
  ts          TEXT NOT NULL,
  author      TEXT NOT NULL,
  text        TEXT NOT NULL,
  urgent      INTEGER NOT NULL,
  received_at INTEGER NOT NULL,
  PRIMARY KEY (subscriber, channel, thread_ts, ts)
);
CREATE TABLE slack_thread_posts (
  subscriber TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  channel    TEXT NOT NULL,
  thread_ts  TEXT NOT NULL,
  nonce      TEXT NOT NULL,
  reserved_at INTEGER NOT NULL,
  posted_ts  TEXT,
  PRIMARY KEY (subscriber, channel, thread_ts, nonce)
);
CREATE TABLE slack_reaction_settings (
  id           INTEGER PRIMARY KEY CHECK (id = 1),
  watch_user   TEXT,
  repo_id      TEXT REFERENCES repositories(id) ON DELETE SET NULL,
  baselined_at INTEGER
);
CREATE TABLE slack_reaction_bindings (
  emoji    TEXT PRIMARY KEY,
  thread   TEXT NOT NULL,
  mode     TEXT NOT NULL DEFAULT '',
  one_shot INTEGER NOT NULL,
  priority INTEGER
);
CREATE TABLE slack_reaction_fired (
  dispatch_id TEXT PRIMARY KEY,
  channel     TEXT NOT NULL,
  ts          TEXT NOT NULL,
  emoji       TEXT NOT NULL,
  fired_at    INTEGER NOT NULL,
  baseline    INTEGER NOT NULL
);
`

// slackThreadPollDDL is the layout-208 addition: how a subscription's thread
// is followed (SlackThreadSource), and the cursor of one followed by polling
// it. Every row before was followed on its channel's echo pipe. IT IS
// ADDITIVE: two defaulted columns the previous build never names.
const slackThreadPollDDL = `
ALTER TABLE slack_thread_subscriptions ADD COLUMN source TEXT NOT NULL DEFAULT 'echo_pipe';
ALTER TABLE slack_thread_subscriptions ADD COLUMN cursor_ts TEXT NOT NULL DEFAULT '';
`

// SlackThreadKey names one Slack thread: its channel and root timestamp.
type SlackThreadKey struct {
	Channel  string
	ThreadTS string
}

// String is the key as `channel/thread_ts`, the spelling every record carries.
func (k SlackThreadKey) String() string { return k.Channel + "/" + k.ThreadTS }

// Valid reports whether both parts are set.
func (k SlackThreadKey) Valid() bool { return k.Channel != "" && k.ThreadTS != "" }

// SlackThreadMode is what a subscribed workspace may do in its thread
// (agentrepl.v1.SlackThreadMode). ITS VALUES ARE DURABLE spellings.
type SlackThreadMode string

// The modes.
const (
	SlackThreadModeListen   SlackThreadMode = "listen"
	SlackThreadModeRespond  SlackThreadMode = "respond"
	SlackThreadModeInteract SlackThreadMode = "interact"
)

// Valid reports whether the mode is one of the declared three.
func (m SlackThreadMode) Valid() bool {
	return m == SlackThreadModeListen || m == SlackThreadModeRespond || m == SlackThreadModeInteract
}

// SlackThreadOrigin is how a subscription came to be. ITS VALUES ARE DURABLE.
type SlackThreadOrigin string

// The origins.
const (
	SlackThreadOriginRequested SlackThreadOrigin = "requested"
	SlackThreadOriginCreation  SlackThreadOrigin = "creation"
)

// Valid reports whether the origin is a declared one.
func (o SlackThreadOrigin) Valid() bool {
	return o == SlackThreadOriginRequested || o == SlackThreadOriginCreation
}

// SlackThreadSource is how a subscription's thread is followed. ITS VALUES ARE
// DURABLE.
type SlackThreadSource string

// The sources.
const (
	// SlackThreadSourceEchoPipe streams the channel's echo pipe, which GNS
	// keeps for every channel its bot is a member of; its cursor is the
	// pipe's sequence number (Cursor).
	SlackThreadSourceEchoPipe SlackThreadSource = "echo_pipe"
	// SlackThreadSourceThreadPoll polls the thread through the login's own
	// Slack access, for a channel GNS keeps no echo pipe for (a direct
	// message); its cursor is the latest message's timestamp (CursorTS).
	SlackThreadSourceThreadPoll SlackThreadSource = "thread_poll"
)

// Valid reports whether the source is a declared one.
func (s SlackThreadSource) Valid() bool {
	return s == SlackThreadSourceEchoPipe || s == SlackThreadSourceThreadPoll
}

// SlackThreadMessage is one message of a thread as the daemon keeps it.
type SlackThreadMessage struct {
	TS     string `json:"ts"`
	Author string `json:"author"`
	Text   string `json:"text"`
	// Urgent is the classifier's verdict, taken as the message arrived.
	Urgent bool `json:"urgent"`
	// ReceivedAt is when the daemon read it.
	ReceivedAt time.Time `json:"received_at"`
}

// SlackThreadSubscription is one workspace's subscription to one thread.
type SlackThreadSubscription struct {
	Subscriber   WorkspaceID
	Thread       SlackThreadKey
	URL          string
	Mode         SlackThreadMode
	Origin       SlackThreadOrigin
	SubscribedAt time.Time
	// Source is how the thread is followed, which names its cursor.
	Source SlackThreadSource
	// Cursor is the highest echo-pipe sequence number read
	// (SlackThreadSourceEchoPipe).
	Cursor int64
	// CursorTS is the latest message's timestamp a poll read
	// (SlackThreadSourceThreadPoll).
	CursorTS string
	// PostsMade counts the posts since the count was last reset.
	PostsMade int
	// FailingSince is set while the stream's attempts fail, with the latest
	// failure's words in FailureDetail.
	FailingSince  *time.Time
	FailureDetail string
	// Pending counts the messages waiting for the next digest.
	Pending int
	// UntoldTurn and UntoldMessages are the last digest submitted; the turn is
	// empty when its submission failed and its messages wait to be said again.
	UntoldTurn     string
	UntoldMessages []SlackThreadMessage
}

// FindSlackThreadSubscription answers the subscription of SUBS that follows
// the thread THREAD names: the one rooted at the link's message, or the one
// whose own link names that message. The ONE rule every "is this thread
// subscribed" question answers by, so a verb that finds a subscription and a
// view that draws one never disagree about which thread a link names.
func FindSlackThreadSubscription(subs []SlackThreadSubscription, thread slacklink.Thread) (SlackThreadSubscription, bool) {
	for _, sub := range subs {
		if sub.Thread.Channel != thread.Channel {
			continue
		}
		if sub.Thread.ThreadTS == thread.TS {
			return sub, true
		}
		if named, err := slacklink.ParseThread(sub.URL); err == nil && named.TS == thread.TS {
			return sub, true
		}
	}
	return SlackThreadSubscription{}, false
}

// slackThreadColumns is every subscription column, in scanSlackThread's
// order, the pending count included.
const slackThreadColumns = `s.subscriber, s.channel, s.thread_ts, s.thread_url, s.mode, s.origin, s.subscribed_at, s.source, s.cursor_seq,
  s.cursor_ts, s.posts_made, s.failing_since, s.failure_detail, s.untold_turn, s.untold_messages,
  (SELECT COUNT(*) FROM slack_thread_pending p WHERE p.subscriber = s.subscriber AND p.channel = s.channel AND p.thread_ts = s.thread_ts)`

func scanSlackThread(row interface{ Scan(...any) error }) (SlackThreadSubscription, error) {
	var (
		sub      SlackThreadSubscription
		at       int64
		failing  sql.NullInt64
		detail   sql.NullString
		mode     string
		origin   string
		untoldJS string
		source   string
	)
	if err := row.Scan(&sub.Subscriber, &sub.Thread.Channel, &sub.Thread.ThreadTS, &sub.URL, &mode, &origin, &at, &source, &sub.Cursor,
		&sub.CursorTS, &sub.PostsMade, &failing, &detail, &sub.UntoldTurn, &untoldJS, &sub.Pending); err != nil {
		return SlackThreadSubscription{}, err
	}
	sub.Mode, sub.Origin, sub.Source, sub.SubscribedAt = SlackThreadMode(mode), SlackThreadOrigin(origin), SlackThreadSource(source), fromNanos(at)
	if !sub.Mode.Valid() || !sub.Origin.Valid() || !sub.Source.Valid() {
		return SlackThreadSubscription{}, fmt.Errorf("wsm: the subscription of %s to %s stores mode %q, origin %q and source %q", sub.Subscriber, sub.Thread, mode, origin, source)
	}
	if failing.Valid {
		since := fromNanos(failing.Int64)
		sub.FailingSince, sub.FailureDetail = &since, detail.String
	}
	if err := json.Unmarshal([]byte(untoldJS), &sub.UntoldMessages); err != nil {
		return SlackThreadSubscription{}, fmt.Errorf("wsm: the subscription of %s to %s stores unreadable untold messages: %w", sub.Subscriber, sub.Thread, err)
	}
	return sub, nil
}

// SubscribeSlackThread records SUB, or sets the mode of the subscription its
// subscriber already holds to the same thread, keeping its cursor. It
// answers the mode the held subscription had, and whether one was held. A
// fresh subscription names its source, and a polled one its cursor. The
// subscriber's row is checked in the same transaction: an unregistered
// subscriber is ErrNotFound, a closed one ErrSubscriberClosed.
func (s *store) SubscribeSlackThread(ctx context.Context, sub SlackThreadSubscription) (SlackThreadMode, bool, error) {
	const op = "daemon.wsm.subscribe_slack_thread"
	fields := dlog.Context{"subscriber": string(sub.Subscriber), "thread": sub.Thread.String(), "mode": string(sub.Mode)}
	if !sub.Thread.Valid() || !sub.Mode.Valid() || !sub.Origin.Valid() || !sub.Source.Valid() || sub.URL == "" ||
		(sub.Source == SlackThreadSourceThreadPoll) != (sub.CursorTS != "") {
		err := fmt.Errorf("wsm: a Slack-thread subscription of %s to %s names mode %q, origin %q, source %q, timestamp cursor %q and url %q",
			sub.Subscriber, sub.Thread, sub.Mode, sub.Origin, sub.Source, sub.CursorTS, sub.URL)
		s.log.Error(op, "refused a Slack-thread subscription its caller should never have made", withError(fields, err))
		return "", false, err
	}
	var (
		previous SlackThreadMode
		held     bool
	)
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireOpenSubscriber(ctx, tx, sub.Subscriber); err != nil {
			return err
		}
		var mode string
		err := tx.QueryRowContext(ctx, `SELECT mode FROM slack_thread_subscriptions WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
			sub.Subscriber, sub.Thread.Channel, sub.Thread.ThreadTS).Scan(&mode)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			_, err = tx.ExecContext(ctx,
				`INSERT INTO slack_thread_subscriptions (subscriber, channel, thread_ts, thread_url, mode, origin, subscribed_at, source, cursor_seq, cursor_ts) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
				sub.Subscriber, sub.Thread.Channel, sub.Thread.ThreadTS, sub.URL, string(sub.Mode), string(sub.Origin), nanos(sub.SubscribedAt), string(sub.Source), sub.Cursor, sub.CursorTS)
			return err
		case err != nil:
			return err
		}
		previous, held = SlackThreadMode(mode), true
		_, err = tx.ExecContext(ctx, `UPDATE slack_thread_subscriptions SET mode = ? WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
			string(sub.Mode), sub.Subscriber, sub.Thread.Channel, sub.Thread.ThreadTS)
		return err
	})
	return previous, held, err
}

// SetSlackThreadMode sets the mode of SUBSCRIBER's subscription to THREAD,
// resetting its post count when RESETPOSTS, and answers the mode it had. No
// such subscription is ErrNotFound.
func (s *store) SetSlackThreadMode(ctx context.Context, subscriber WorkspaceID, thread SlackThreadKey, mode SlackThreadMode, resetPosts bool) (SlackThreadMode, error) {
	const op = "daemon.wsm.set_slack_thread_mode"
	fields := dlog.Context{"subscriber": string(subscriber), "thread": thread.String(), "mode": string(mode), "reset_posts": resetPosts}
	if !mode.Valid() {
		err := fmt.Errorf("wsm: mode %q is no Slack-thread mode", mode)
		s.log.Error(op, "refused a mode its caller should never have named", withError(fields, err))
		return "", err
	}
	var previous SlackThreadMode
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var was string
		err := tx.QueryRowContext(ctx, `SELECT mode FROM slack_thread_subscriptions WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
			subscriber, thread.Channel, thread.ThreadTS).Scan(&was)
		if errors.Is(err, sql.ErrNoRows) {
			return ErrNotFound
		}
		if err != nil {
			return err
		}
		previous = SlackThreadMode(was)
		query := `UPDATE slack_thread_subscriptions SET mode = ? WHERE subscriber = ? AND channel = ? AND thread_ts = ?`
		if resetPosts {
			query = `UPDATE slack_thread_subscriptions SET mode = ?, posts_made = 0 WHERE subscriber = ? AND channel = ? AND thread_ts = ?`
		}
		_, err = tx.ExecContext(ctx, query, string(mode), subscriber, thread.Channel, thread.ThreadTS)
		return err
	})
	return previous, err
}

// UnsubscribeSlackThread ends SUBSCRIBER's subscription to THREAD with its
// pending messages and post nonces, and answers whether one stood.
func (s *store) UnsubscribeSlackThread(ctx context.Context, subscriber WorkspaceID, thread SlackThreadKey) (bool, error) {
	const op = "daemon.wsm.unsubscribe_slack_thread"
	var ended bool
	err := s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "thread": thread.String()}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM slack_thread_subscriptions WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
			subscriber, thread.Channel, thread.ThreadTS)
		if err != nil {
			return err
		}
		if ended, err = touchedRow(res); err != nil {
			return err
		}
		for _, table := range []string{"slack_thread_pending", "slack_thread_posts"} {
			if _, err := tx.ExecContext(ctx, `DELETE FROM `+table+` WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
				subscriber, thread.Channel, thread.ThreadTS); err != nil {
				return err
			}
		}
		return nil
	})
	return ended, err
}

// endSlackThreadSubscriptionsInTx deletes every Slack-thread row of WS, inside
// the close's transaction (endWorkspaceSubscriptionsInTx).
func endSlackThreadSubscriptionsInTx(ctx context.Context, tx *sql.Tx, ws WorkspaceID) error {
	for _, table := range []string{"slack_thread_subscriptions", "slack_thread_pending", "slack_thread_posts"} {
		if _, err := tx.ExecContext(ctx, `DELETE FROM `+table+` WHERE subscriber = ?`, ws); err != nil {
			return err
		}
	}
	return nil
}

// SlackThreadSubscriptions loads every subscription, by subscriber, channel
// and thread: what the watcher streams.
func (s *store) SlackThreadSubscriptions(ctx context.Context) ([]SlackThreadSubscription, error) {
	var out []SlackThreadSubscription
	err := s.read(ctx, "daemon.wsm.slack_thread_subscriptions", nil, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT `+slackThreadColumns+` FROM slack_thread_subscriptions s ORDER BY s.subscriber, s.channel, s.thread_ts`, scanSlackThread)
		return err
	})
	return out, err
}

// WorkspaceSlackThreadSubscriptions loads every subscription SUBSCRIBER
// holds, by channel and thread.
func (s *store) WorkspaceSlackThreadSubscriptions(ctx context.Context, subscriber WorkspaceID) ([]SlackThreadSubscription, error) {
	var out []SlackThreadSubscription
	err := s.read(ctx, "daemon.wsm.workspace_slack_thread_subscriptions", dlog.Context{"subscriber": string(subscriber)}, func(ctx context.Context) error {
		var err error
		out, err = slackThreadsOf(ctx, s.q(ctx), subscriber)
		return err
	})
	return out, err
}

// slackThreadsOf reads every subscription SUBSCRIBER holds through Q, by
// channel and thread.
func slackThreadsOf(ctx context.Context, q querier, subscriber WorkspaceID) ([]SlackThreadSubscription, error) {
	return listRows(ctx, q, `SELECT `+slackThreadColumns+` FROM slack_thread_subscriptions s WHERE s.subscriber = ? ORDER BY s.channel, s.thread_ts`,
		scanSlackThread, subscriber)
}

// ReserveSlackThreadPost records NONCE as a post SUBSCRIBER is about to make
// in THREAD, before the post is sent. No such subscription is ErrNotFound.
func (s *store) ReserveSlackThreadPost(ctx context.Context, subscriber WorkspaceID, thread SlackThreadKey, nonce string, at time.Time) error {
	const op = "daemon.wsm.reserve_slack_thread_post"
	fields := dlog.Context{"subscriber": string(subscriber), "thread": thread.String(), "nonce": nonce}
	if nonce == "" {
		err := errors.New("wsm: a Slack post reserved with no nonce")
		s.log.Error(op, "refused a post reservation its caller should never have made", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireSlackThread(ctx, tx, subscriber, thread); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `INSERT INTO slack_thread_posts (subscriber, channel, thread_ts, nonce, reserved_at) VALUES (?, ?, ?, ?, ?)`,
			subscriber, thread.Channel, thread.ThreadTS, nonce, nanos(at))
		return err
	})
}

// RecordSlackThreadPost records that the post reserved under NONCE was made as
// TS, counting it against the subscription's limit when COUNTED, and answers
// the posts made since the count was last reset. No such subscription is
// ErrNotFound.
func (s *store) RecordSlackThreadPost(ctx context.Context, subscriber WorkspaceID, thread SlackThreadKey, nonce, ts string, counted bool) (int, error) {
	const op = "daemon.wsm.record_slack_thread_post"
	var made int
	err := s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "thread": thread.String(), "nonce": nonce, "ts": ts, "counted": counted},
		func(ctx context.Context, tx *sql.Tx) error {
			res, err := tx.ExecContext(ctx, `UPDATE slack_thread_posts SET posted_ts = ? WHERE subscriber = ? AND channel = ? AND thread_ts = ? AND nonce = ?`,
				ts, subscriber, thread.Channel, thread.ThreadTS, nonce)
			if err != nil {
				return err
			}
			if touched, err := touchedRow(res); err != nil || !touched {
				if err == nil {
					err = ErrNotFound
				}
				return err
			}
			if counted {
				if _, err := tx.ExecContext(ctx, `UPDATE slack_thread_subscriptions SET posts_made = posts_made + 1 WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
					subscriber, thread.Channel, thread.ThreadTS); err != nil {
					return err
				}
			}
			return tx.QueryRowContext(ctx, `SELECT posts_made FROM slack_thread_subscriptions WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
				subscriber, thread.Channel, thread.ThreadTS).Scan(&made)
		})
	return made, err
}

// SlackThreadPostNonces answers every nonce SUBSCRIBER reserved in THREAD.
func (s *store) SlackThreadPostNonces(ctx context.Context, subscriber WorkspaceID, thread SlackThreadKey) ([]string, error) {
	var out []string
	err := s.read(ctx, "daemon.wsm.slack_thread_post_nonces", dlog.Context{"subscriber": string(subscriber), "thread": thread.String()}, func(ctx context.Context) error {
		var err error
		out, err = nonces(ctx, s.q(ctx), subscriber, thread)
		return err
	})
	return out, err
}

func nonces(ctx context.Context, q querier, subscriber WorkspaceID, thread SlackThreadKey) ([]string, error) {
	return listRows(ctx, q, `SELECT nonce FROM slack_thread_posts WHERE subscriber = ? AND channel = ? AND thread_ts = ? ORDER BY nonce`,
		func(row interface{ Scan(...any) error }) (string, error) {
			var n string
			return n, row.Scan(&n)
		}, subscriber, thread.Channel, thread.ThreadTS)
}

// SlackThreadRead is one read of a thread, to record.
type SlackThreadRead struct {
	Subscriber WorkspaceID
	Thread     SlackThreadKey
	// Messages are the messages read, in the order they were read.
	Messages []SlackThreadMessage
	// Cursor is the highest sequence number a pipe's read reached.
	Cursor int64
	// CursorTS is the latest timestamp a poll read, empty for a pipe's read
	// and for a poll that read nothing new. A poll reads only past the
	// stored one, so it never moves the cursor back.
	CursorTS string
	// At is when the read ended, which ends any failing stretch.
	At time.Time
}

// RecordSlackThreadMessages stores R's messages as pending, dropping each that
// carries one of the subscription's post nonces and each already pending, and
// advances the cursor to R.Cursor when that is past it (and the timestamp
// cursor to R.CursorTS when that is set), in one transaction. It
// answers the messages stored. No such subscription answers none: a thread
// unsubscribed while it was read keeps nothing of the read.
func (s *store) RecordSlackThreadMessages(ctx context.Context, r SlackThreadRead) ([]SlackThreadMessage, error) {
	const op = "daemon.wsm.record_slack_thread_messages"
	var stored []SlackThreadMessage
	err := s.write(ctx, op, dlog.Context{"subscriber": string(r.Subscriber), "thread": r.Thread.String(), "read": len(r.Messages), "cursor": r.Cursor, "cursor_ts": r.CursorTS},
		func(ctx context.Context, tx *sql.Tx) error {
			stored = nil
			res, err := tx.ExecContext(ctx, `UPDATE slack_thread_subscriptions SET cursor_seq = MAX(cursor_seq, ?), cursor_ts = CASE WHEN ? = '' THEN cursor_ts ELSE ? END,
				failing_since = NULL, failure_detail = NULL
				WHERE subscriber = ? AND channel = ? AND thread_ts = ?`, r.Cursor, r.CursorTS, r.CursorTS, r.Subscriber, r.Thread.Channel, r.Thread.ThreadTS)
			if err != nil {
				return err
			}
			if live, err := touchedRow(res); err != nil || !live {
				return err
			}
			own, err := nonces(ctx, tx, r.Subscriber, r.Thread)
			if err != nil {
				return err
			}
			for _, m := range r.Messages {
				if CarriesNonce(m.Text, own) {
					continue
				}
				res, err := tx.ExecContext(ctx, `INSERT INTO slack_thread_pending (subscriber, channel, thread_ts, ts, author, text, urgent, received_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
					ON CONFLICT DO NOTHING`, r.Subscriber, r.Thread.Channel, r.Thread.ThreadTS, m.TS, m.Author, m.Text, m.Urgent, nanos(m.ReceivedAt))
				if err != nil {
					return err
				}
				if inserted, err := touchedRow(res); err != nil {
					return err
				} else if inserted {
					stored = append(stored, m)
				}
			}
			return nil
		})
	return stored, err
}

// CarriesNonce reports whether TEXT carries any of NONCES as a post footer's
// nonce (SlackPostFooter). It is the one self-filter: the store applies it to
// every arriving message, and a thread read applies it to mark own posts.
func CarriesNonce(text string, nonces []string) bool {
	for _, n := range nonces {
		if strings.Contains(text, slackNonceMark(n)) {
			return true
		}
	}
	return false
}

// slackNonceMark is how a nonce is spelled inside a post's footer.
func slackNonceMark(nonce string) string { return "ref " + nonce }

// SlackPostFooter is the footer every post the daemon makes for a
// subscription ends with: an italic line naming agent-repl, the workspace and
// the post's nonce, which CarriesNonce recognizes.
func SlackPostFooter(workspace, nonce string) string {
	return fmt.Sprintf("_via agent-repl · `%s` · %s_", workspace, slackNonceMark(nonce))
}

// RecordSlackThreadStreamFailure records that a read of THREAD for
// SUBSCRIBER failed at AT, starting a failing stretch when none stands, and
// answers whether this failure began one.
func (s *store) RecordSlackThreadStreamFailure(ctx context.Context, subscriber WorkspaceID, thread SlackThreadKey, at time.Time, detail string) (bool, error) {
	const op = "daemon.wsm.record_slack_thread_stream_failure"
	var began bool
	err := s.write(ctx, op, dlog.Context{"subscriber": string(subscriber), "thread": thread.String()}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE slack_thread_subscriptions SET failing_since = ?, failure_detail = ?
			WHERE subscriber = ? AND channel = ? AND thread_ts = ? AND failing_since IS NULL`, nanos(at), detail, subscriber, thread.Channel, thread.ThreadTS)
		if err != nil {
			return err
		}
		if began, err = touchedRow(res); err != nil || began {
			return err
		}
		_, err = tx.ExecContext(ctx, `UPDATE slack_thread_subscriptions SET failure_detail = ? WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
			detail, subscriber, thread.Channel, thread.ThreadTS)
		return err
	})
	return began, err
}

// SlackThreadPending answers the messages pending for SUBSCRIBER's next digest
// of THREAD, oldest read first.
func (s *store) SlackThreadPending(ctx context.Context, subscriber WorkspaceID, thread SlackThreadKey) ([]SlackThreadMessage, error) {
	var out []SlackThreadMessage
	err := s.read(ctx, "daemon.wsm.slack_thread_pending", dlog.Context{"subscriber": string(subscriber), "thread": thread.String()}, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT ts, author, text, urgent, received_at FROM slack_thread_pending
			WHERE subscriber = ? AND channel = ? AND thread_ts = ? ORDER BY received_at, ts`,
			func(row interface{ Scan(...any) error }) (SlackThreadMessage, error) {
				var (
					m  SlackThreadMessage
					at int64
				)
				err := row.Scan(&m.TS, &m.Author, &m.Text, &m.Urgent, &at)
				m.ReceivedAt = fromNanos(at)
				return m, err
			}, subscriber, thread.Channel, thread.ThreadTS)
		return err
	})
	return out, err
}

// SlackThreadDigest is one digest's submission, to record.
type SlackThreadDigest struct {
	Subscriber WorkspaceID
	Thread     SlackThreadKey
	// Turn is the turn the digest was submitted as; empty when its submission
	// failed.
	Turn string
	// Messages are every message the digest said (or would have said),
	// carried ones first.
	Messages []SlackThreadMessage
	// Took are the pending messages' timestamps the digest took off the
	// pending table.
	Took []string
}

// RecordSlackThreadDigest records D as the subscription's last digest and
// removes the pending messages it took, in one transaction. A subscription
// gone since is no error: there is nothing left to record against.
func (s *store) RecordSlackThreadDigest(ctx context.Context, d SlackThreadDigest) error {
	const op = "daemon.wsm.record_slack_thread_digest"
	encoded, err := json.Marshal(d.Messages)
	if err != nil {
		// Messages are the store's own strings and instants.
		panic(fmt.Sprintf("wsm: encode a Slack digest's messages: %v", err))
	}
	return s.write(ctx, op, dlog.Context{"subscriber": string(d.Subscriber), "thread": d.Thread.String(), "turn": d.Turn, "messages": len(d.Messages), "took": len(d.Took)},
		func(ctx context.Context, tx *sql.Tx) error {
			if _, err := tx.ExecContext(ctx, `UPDATE slack_thread_subscriptions SET untold_turn = ?, untold_messages = ? WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
				d.Turn, string(encoded), d.Subscriber, d.Thread.Channel, d.Thread.ThreadTS); err != nil {
				return err
			}
			for _, ts := range d.Took {
				if _, err := tx.ExecContext(ctx, `DELETE FROM slack_thread_pending WHERE subscriber = ? AND channel = ? AND thread_ts = ? AND ts = ?`,
					d.Subscriber, d.Thread.Channel, d.Thread.ThreadTS, ts); err != nil {
					return err
				}
			}
			return nil
		})
}

// requireSlackThread answers ErrNotFound unless SUBSCRIBER holds a
// subscription to THREAD.
func requireSlackThread(ctx context.Context, tx *sql.Tx, subscriber WorkspaceID, thread SlackThreadKey) error {
	var one int
	err := tx.QueryRowContext(ctx, `SELECT 1 FROM slack_thread_subscriptions WHERE subscriber = ? AND channel = ? AND thread_ts = ?`,
		subscriber, thread.Channel, thread.ThreadTS).Scan(&one)
	if errors.Is(err, sql.ErrNoRows) {
		return ErrNotFound
	}
	return err
}
