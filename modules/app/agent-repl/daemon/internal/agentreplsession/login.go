package agentreplsession

import (
	"context"
	"errors"
	"sync"
	"time"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
)

// LoginSessions is what a completed login begins: the tracker.
type LoginSessions interface {
	LoginCompleted(ctx context.Context, at time.Time)
}

// LoginWatch tells a login made THROUGH agent-repl from a flow that was opened
// and abandoned. It is the login manager's observer (login.Observer).
//
// NOTHING PARSES THE LOGIN TUI (internal/login), so completion is read off the
// account root the flow logged into: a completed OAuth login rewrites the
// root's `oauthAccount` block — at least its profile fetch stamp — so the
// block read when the flow ended differing from the one read when it opened is
// a login this flow made. The session begins at the vendor's own profile
// fetch stamp when that falls inside the flow, which is the login's moment;
// otherwise at the instant the change was seen.
//
// A login made anywhere else (a terminal's `claude /login`) opens no flow
// here, so it never begins a session (owner ruling, 2026-10-06).
type LoginWatch struct {
	read     func(configDir string) (account.LoginRecord, error)
	sessions LoginSessions
	now      func() time.Time
	log      dlog.Logger

	mu     sync.Mutex
	opened map[string]openedFlow
}

// openedFlow is a flow's reading at its opening.
type openedFlow struct {
	record account.LoginRecord
	at     time.Time
}

// NewLoginWatch builds the watch, refusing a missing collaborator.
func NewLoginWatch(read func(string) (account.LoginRecord, error), sessions LoginSessions, now func() time.Time, log dlog.Logger) (*LoginWatch, error) {
	switch {
	case read == nil:
		return nil, errors.New("agentreplsession: a login record reader is required")
	case sessions == nil:
		return nil, errors.New("agentreplsession: a session to begin is required")
	case now == nil:
		return nil, errors.New("agentreplsession: a clock is required")
	case log == nil:
		return nil, errors.New("agentreplsession: a logger is required")
	}
	return &LoginWatch{read: read, sessions: sessions, now: now, log: log, opened: map[string]openedFlow{}}, nil
}

// LoginOpened reads the root's login record as the flow opens.
func (w *LoginWatch) LoginOpened(configDir string) {
	const op = "daemon.agentreplsession.login_opened"
	fields := dlog.Context{"config_dir": configDir}
	record, err := w.read(configDir)
	if err != nil {
		fields["cause"] = err.Error()
		w.log.Error(op, "the account root's login record could not be read; a login this flow makes will not begin a session", fields)
		return
	}
	w.mu.Lock()
	w.opened[configDir] = openedFlow{record: record, at: w.now()}
	w.mu.Unlock()
	fields["logged_in"] = record.Block != ""
	w.log.Debug(op, "read the account root's login record as its login flow opened", fields)
}

// LoginEnded reads the root's login record again and begins a session when
// the flow made a login.
func (w *LoginWatch) LoginEnded(configDir string) {
	const op = "daemon.agentreplsession.login_ended"
	fields := dlog.Context{"config_dir": configDir}
	w.mu.Lock()
	flow, ok := w.opened[configDir]
	delete(w.opened, configDir)
	w.mu.Unlock()
	if !ok {
		w.log.Debug(op, "the flow's opening was not read; its ending decides nothing", fields)
		return
	}
	after, err := w.read(configDir)
	if err != nil {
		fields["cause"] = err.Error()
		w.log.Error(op, "the account root's login record could not be read; whether this flow made a login is unknown", fields)
		return
	}
	switch {
	case after.Block == "":
		w.log.Info(op, "the login flow ended with the root naming no account; no login was made", fields)
		return
	case after.Block == flow.record.Block:
		w.log.Info(op, "the login flow ended with the root's account record unchanged; no login was made", fields)
		return
	}
	at := w.now()
	if after.ProfileFetchedAt.After(flow.at) && after.ProfileFetchedAt.Before(at) {
		at = after.ProfileFetchedAt
	}
	fields["at"] = at.UTC().Format(time.RFC3339Nano)
	w.log.Info(op, "a login made through agent-repl completed", fields)
	w.sessions.LoginCompleted(context.Background(), at)
}
