package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of the Slack REACTION INITIATOR
// (agentrepl.v1.UpdateSlackReactionSettings, internal/slackwatch): its
// settings (one row), its bindings, and the ledger of every (message, emoji)
// that fired or was recorded by the baseline. Its tables are slackThreadsDDL's.
//
// A TRIGGER FIRES AT MOST ONCE, EVER: MarkSlackReactionFired inserts its
// dispatch id or answers that it stood, in one statement, before the
// workspace it creates is asked for.

// SlackReactionThread is how a binding's workspace relates to the thread
// (agentrepl.v1.SlackReactionAction.thread). ITS VALUES ARE DURABLE.
type SlackReactionThread string

// The thread relations.
const (
	SlackReactionAnnounced  SlackReactionThread = "announced"
	SlackReactionSubscribed SlackReactionThread = "subscribed"
)

// SlackReactionBinding is one emoji bound to the workspace it creates.
type SlackReactionBinding struct {
	Emoji  string
	Thread SlackReactionThread
	// Mode is the subscription's mode, set exactly when Thread is subscribed.
	Mode    SlackThreadMode
	OneShot bool
	// Priority is the workspace's roster priority, nil for none.
	Priority *Priority
}

// valid reports whether the binding is one the contract can state.
func (b SlackReactionBinding) valid() bool {
	switch b.Thread {
	case SlackReactionAnnounced:
		return b.Emoji != "" && b.Mode == ""
	case SlackReactionSubscribed:
		return b.Emoji != "" && b.Mode.Valid()
	}
	return false
}

// SlackReactionSettings are the initiator's settings as they stand.
type SlackReactionSettings struct {
	// WatchUser is the Slack user id watched, empty until set.
	WatchUser string
	// RepoID is the repository fired workspaces are created in, empty until
	// set (or since the repository was forgotten).
	RepoID string
	// BaselinedAt is when the standing reactions were recorded, nil while the
	// baseline is owed.
	BaselinedAt *time.Time
	// SearchedThrough is when the latest search of the watch user's messages
	// that read every page it needed began, nil until one has: every trigger
	// typed before it was read then, but for the ones Slack's search had not
	// indexed yet.
	SearchedThrough *time.Time
	// Bindings are every binding, by emoji.
	Bindings []SlackReactionBinding
}

// Missing names, in plain words, what the settings still need before the
// initiator polls; empty when they are complete.
func (s SlackReactionSettings) Missing() []string {
	var out []string
	if s.WatchUser == "" {
		out = append(out, "a watch user")
	}
	if s.RepoID == "" {
		out = append(out, "a repository")
	}
	if len(s.Bindings) == 0 {
		out = append(out, "a binding")
	}
	return out
}

// Binding answers EMOJI's binding, false when it has none.
func (s SlackReactionSettings) Binding(emoji string) (SlackReactionBinding, bool) {
	for _, b := range s.Bindings {
		if b.Emoji == emoji {
			return b, true
		}
	}
	return SlackReactionBinding{}, false
}

// SlackReactionSettings reads the settings.
func (s *store) SlackReactionSettings(ctx context.Context) (SlackReactionSettings, error) {
	var out SlackReactionSettings
	err := s.read(ctx, "daemon.wsm.slack_reaction_settings", nil, func(ctx context.Context) error {
		var err error
		out, err = reactionSettings(ctx, s.q(ctx))
		return err
	})
	return out, err
}

// settingsQuerier is what reactionSettings reads through: the store's
// connection or a write's transaction.
type settingsQuerier interface {
	querier
	rowQuerier
}

func reactionSettings(ctx context.Context, q settingsQuerier) (SlackReactionSettings, error) {
	var (
		out          SlackReactionSettings
		user, rep    sql.NullString
		at, searched sql.NullInt64
	)
	err := q.QueryRowContext(ctx, `SELECT watch_user, repo_id, baselined_at, searched_through FROM slack_reaction_settings WHERE id = 1`).Scan(&user, &rep, &at, &searched)
	if err != nil && !errors.Is(err, sql.ErrNoRows) {
		return SlackReactionSettings{}, err
	}
	out.WatchUser, out.RepoID = user.String, rep.String
	if at.Valid {
		t := fromNanos(at.Int64)
		out.BaselinedAt = &t
	}
	if searched.Valid {
		t := fromNanos(searched.Int64)
		out.SearchedThrough = &t
	}
	out.Bindings, err = listRows(ctx, q, `SELECT emoji, thread, mode, one_shot, priority FROM slack_reaction_bindings ORDER BY emoji`,
		func(row interface{ Scan(...any) error }) (SlackReactionBinding, error) {
			var (
				b        SlackReactionBinding
				thread   string
				mode     string
				priority sql.NullInt64
			)
			if err := row.Scan(&b.Emoji, &thread, &mode, &b.OneShot, &priority); err != nil {
				return b, err
			}
			b.Thread, b.Mode = SlackReactionThread(thread), SlackThreadMode(mode)
			if priority.Valid {
				p := Priority(priority.Int64)
				b.Priority = &p
			}
			if !b.valid() {
				return b, fmt.Errorf("wsm: the binding of %q stores thread %q and mode %q", b.Emoji, thread, mode)
			}
			return b, nil
		})
	return out, err
}

// SetSlackReactionWatchUser watches USER, owing a new baseline and a new
// first search when the user changed: what another user reacted with or
// typed says nothing about this one.
func (s *store) SetSlackReactionWatchUser(ctx context.Context, user string) (SlackReactionSettings, error) {
	const op = "daemon.wsm.set_slack_reaction_watch_user"
	var out SlackReactionSettings
	err := s.write(ctx, op, dlog.Context{"watch_user": user}, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `INSERT INTO slack_reaction_settings (id, watch_user) VALUES (1, ?)
			ON CONFLICT(id) DO UPDATE SET watch_user = excluded.watch_user,
			  baselined_at = CASE WHEN slack_reaction_settings.watch_user IS excluded.watch_user THEN slack_reaction_settings.baselined_at ELSE NULL END,
			  searched_through = CASE WHEN slack_reaction_settings.watch_user IS excluded.watch_user THEN slack_reaction_settings.searched_through ELSE NULL END`, user); err != nil {
			return err
		}
		var err error
		out, err = reactionSettings(ctx, tx)
		return err
	})
	return out, err
}

// SetSlackReactionRepository creates fired workspaces in REPO. An unknown
// repository is ErrNotFound.
func (s *store) SetSlackReactionRepository(ctx context.Context, repo string) (SlackReactionSettings, error) {
	const op = "daemon.wsm.set_slack_reaction_repository"
	var out SlackReactionSettings
	err := s.write(ctx, op, dlog.Context{"repo_id": repo}, func(ctx context.Context, tx *sql.Tx) error {
		var one int
		if err := tx.QueryRowContext(ctx, `SELECT 1 FROM repositories WHERE id = ?`, repo).Scan(&one); errors.Is(err, sql.ErrNoRows) {
			return ErrNotFound
		} else if err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `INSERT INTO slack_reaction_settings (id, repo_id) VALUES (1, ?)
			ON CONFLICT(id) DO UPDATE SET repo_id = excluded.repo_id`, repo); err != nil {
			return err
		}
		var err error
		out, err = reactionSettings(ctx, tx)
		return err
	})
	return out, err
}

// BindSlackReaction stores B, replacing any binding its emoji had.
func (s *store) BindSlackReaction(ctx context.Context, b SlackReactionBinding) (SlackReactionSettings, error) {
	const op = "daemon.wsm.bind_slack_reaction"
	fields := dlog.Context{"emoji": b.Emoji, "thread": string(b.Thread), "mode": string(b.Mode), "one_shot": b.OneShot}
	if !b.valid() {
		err := fmt.Errorf("wsm: a binding of %q names thread %q and mode %q", b.Emoji, b.Thread, b.Mode)
		s.log.Error(op, "refused a binding its caller should never have made", withError(fields, err))
		return SlackReactionSettings{}, err
	}
	var priority any
	if b.Priority != nil {
		priority = int(*b.Priority)
	}
	var out SlackReactionSettings
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `INSERT INTO slack_reaction_bindings (emoji, thread, mode, one_shot, priority) VALUES (?, ?, ?, ?, ?)
			ON CONFLICT(emoji) DO UPDATE SET thread = excluded.thread, mode = excluded.mode, one_shot = excluded.one_shot, priority = excluded.priority`,
			b.Emoji, string(b.Thread), string(b.Mode), b.OneShot, priority); err != nil {
			return err
		}
		var err error
		out, err = reactionSettings(ctx, tx)
		return err
	})
	return out, err
}

// UnbindSlackReaction removes EMOJI's binding. An emoji with none is
// ErrNotFound.
func (s *store) UnbindSlackReaction(ctx context.Context, emoji string) (SlackReactionSettings, error) {
	const op = "daemon.wsm.unbind_slack_reaction"
	var out SlackReactionSettings
	err := s.write(ctx, op, dlog.Context{"emoji": emoji}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM slack_reaction_bindings WHERE emoji = ?`, emoji)
		if err != nil {
			return err
		}
		if removed, err := touchedRow(res); err != nil {
			return err
		} else if !removed {
			return ErrNotFound
		}
		out, err = reactionSettings(ctx, tx)
		return err
	})
	return out, err
}

// SlackReactionTrigger is one (message, emoji) the initiator found.
type SlackReactionTrigger struct {
	DispatchID string
	Channel    string
	TS         string
	Emoji      string
}

// MarkSlackReactionFired records T as fired at AT, answering true when it is
// newly recorded and false when it stood already, which is how a trigger
// fires at most once.
func (s *store) MarkSlackReactionFired(ctx context.Context, t SlackReactionTrigger, at time.Time) (bool, error) {
	const op = "daemon.wsm.mark_slack_reaction_fired"
	var fresh bool
	err := s.write(ctx, op, dlog.Context{"dispatch_id": t.DispatchID, "channel": t.Channel, "ts": t.TS, "emoji": t.Emoji}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `INSERT INTO slack_reaction_fired (dispatch_id, channel, ts, emoji, fired_at, baseline) VALUES (?, ?, ?, ?, ?, 0)
			ON CONFLICT(dispatch_id) DO NOTHING`, t.DispatchID, t.Channel, t.TS, t.Emoji, nanos(at))
		if err != nil {
			return err
		}
		fresh, err = touchedRow(res)
		return err
	})
	return fresh, err
}

// RecordSlackReactionBaseline records every trigger in STANDING as fired
// without firing, and the baseline as taken at AT, in one transaction.
func (s *store) RecordSlackReactionBaseline(ctx context.Context, standing []SlackReactionTrigger, at time.Time) error {
	const op = "daemon.wsm.record_slack_reaction_baseline"
	return s.write(ctx, op, dlog.Context{"standing": len(standing)}, func(ctx context.Context, tx *sql.Tx) error {
		for _, t := range standing {
			if _, err := tx.ExecContext(ctx, `INSERT INTO slack_reaction_fired (dispatch_id, channel, ts, emoji, fired_at, baseline) VALUES (?, ?, ?, ?, ?, 1)
				ON CONFLICT(dispatch_id) DO NOTHING`, t.DispatchID, t.Channel, t.TS, t.Emoji, nanos(at)); err != nil {
				return err
			}
		}
		_, err := tx.ExecContext(ctx, `INSERT INTO slack_reaction_settings (id, baselined_at) VALUES (1, ?)
			ON CONFLICT(id) DO UPDATE SET baselined_at = excluded.baselined_at`, nanos(at))
		return err
	})
}

// RecordSlackReactionSearch records AT as the start of the latest search of
// the watch user's messages that read every page it needed, so the next
// poll's search reads on from there (SlackReactionSettings.SearchedThrough).
func (s *store) RecordSlackReactionSearch(ctx context.Context, at time.Time) error {
	const op = "daemon.wsm.record_slack_reaction_search"
	return s.write(ctx, op, dlog.Context{"searched_through": at.UTC().Format(time.RFC3339Nano)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `INSERT INTO slack_reaction_settings (id, searched_through) VALUES (1, ?)
			ON CONFLICT(id) DO UPDATE SET searched_through = excluded.searched_through`, nanos(at))
		return err
	})
}
