// Package agentreplsession owns agent-repl's SESSION (owner ruling,
// 2026-10-06): the span the topbar's connectivity dropdown reports.
//
// # THE SESSION IS SINCE THE LATER OF TWO EVENTS
//
// The session began at the later of the last login made THROUGH agent-repl
// (its own login flow; a login anywhere else does not count) and the start of
// this Emacs. Each event, as it happens, begins a new session unless the
// session standing began later than it did — so an event is only ever
// replaced by a later one, which is the "later of" rule applied as the events
// arrive.
//
// # IT OUTLIVES THE DAEMON, NOT THE EDITOR
//
// The session is durable in wsm (agent_repl_session), so a daemon restart under
// the same Emacs — a bounce, a deploy — loads it and carries on. A
// new Emacs process is told apart from a reconnect by internal/editorinstance,
// and only a new one begins a session.
//
// A session's start is persisted and pushed to every topbar at once.
package agentreplsession

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// Store is the durable session.
type Store interface {
	AgentReplSession(ctx context.Context) (wsm.AgentReplSession, bool, error)
	PutAgentReplSession(ctx context.Context, session wsm.AgentReplSession) error
}

// Publisher states the session on every topbar (the topbar resolver).
type Publisher interface {
	SetAgentReplSession(session *frontendv1.TopbarAgentReplSession)
}

// Tracker is agent-repl's session.
//
// THE STORE IS THE ONE SOURCE OF TRUTH; the tracker holds no session of its
// own. Every begin reads the stored session, applies its change and writes it
// back, so a successor daemon that took over mid-session continues from what
// the incumbent last wrote rather than from what it read when it was built.
type Tracker struct {
	store   Store
	publish Publisher
	log     dlog.Logger

	// mu serializes begins, so each one's read-decide-write is one step.
	mu sync.Mutex
}

// New loads the persisted session and states it, refusing a missing
// collaborator. A store that cannot be read is an error, recorded at ERROR:
// starting with no session would draw a dropdown that forgot the one standing.
func New(ctx context.Context, store Store, publish Publisher, log dlog.Logger) (*Tracker, error) {
	switch {
	case store == nil:
		return nil, errors.New("agentreplsession: a store is required")
	case publish == nil:
		return nil, errors.New("agentreplsession: a publisher is required")
	case log == nil:
		return nil, errors.New("agentreplsession: a logger is required")
	}
	t := &Tracker{store: store, publish: publish, log: log}
	loaded, found, err := store.AgentReplSession(ctx)
	if err != nil {
		log.Error("daemon.agentreplsession.load", "the persisted session could not be read", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("agentreplsession: load the session: %w", err)
	}
	if !found {
		log.Info("daemon.agentreplsession.load", "no session has begun yet; none is stated until a login or an editor start", nil)
		return t, nil
	}
	log.Info("daemon.agentreplsession.load", "carried the persisted session forward", fields(loaded))
	publish.SetAgentReplSession(view(loaded))
	return t, nil
}

// EditorStarted begins a session for a new Emacs process that started at at.
func (t *Tracker) EditorStarted(ctx context.Context, at time.Time) {
	t.begin(ctx, at, wsm.SessionBeganEditorStart)
}

// LoginCompleted begins a session for a login made through agent-repl at at.
func (t *Tracker) LoginCompleted(ctx context.Context, at time.Time) {
	t.begin(ctx, at, wsm.SessionBeganLogin)
}

// begin applies the "later of" rule: the event begins a new session unless
// the standing one began at or after it.
func (t *Tracker) begin(ctx context.Context, at time.Time, began wsm.SessionBegan) {
	const op = "daemon.agentreplsession.begin"
	t.mu.Lock()
	defer t.mu.Unlock()
	event := dlog.Context{"event": string(began), "event_at": at.UTC().Format(time.RFC3339Nano)}
	standing, found, err := t.store.AgentReplSession(ctx)
	if err != nil {
		event["cause"] = err.Error()
		t.log.Error(op, "the standing session could not be read; no session was begun", event)
		return
	}
	if found && !at.After(standing.StartedAt) {
		ctxFields := fields(standing)
		for k, v := range event {
			ctxFields[k] = v
		}
		t.log.Info(op, "the standing session began later than this event; it stays", ctxFields)
		return
	}
	next := wsm.AgentReplSession{StartedAt: at, Began: began}
	if err := t.store.PutAgentReplSession(ctx, next); err != nil {
		ctxFields := fields(next)
		ctxFields["cause"] = err.Error()
		t.log.Error(op, "the new session could not be persisted; it was not begun", ctxFields)
		return
	}
	t.publish.SetAgentReplSession(view(next))
	t.log.Info(op, "a new session began", fields(next))
}

// view is the session as the topbar states it.
func view(s wsm.AgentReplSession) *frontendv1.TopbarAgentReplSession {
	out := &frontendv1.TopbarAgentReplSession{StartedAtMs: s.StartedAt.UnixMilli()}
	switch s.Began {
	case wsm.SessionBeganLogin:
		out.Began = &frontendv1.TopbarAgentReplSession_Login{Login: &frontendv1.TopbarSessionBeganLogin{}}
	case wsm.SessionBeganEditorStart:
		out.Began = &frontendv1.TopbarAgentReplSession_EditorStart{EditorStart: &frontendv1.TopbarSessionBeganEditorStart{}}
	default:
		// wsm refuses any other cause on write and on read, so one here is a
		// broken invariant, not a state to draw.
		panic(fmt.Sprintf("agentreplsession: a session began by %q, which wsm never holds", s.Began))
	}
	return out
}

// fields is a session's structured context (wsm.AgentReplSession.LogContext).
func fields(s wsm.AgentReplSession) dlog.Context { return s.LogContext() }
