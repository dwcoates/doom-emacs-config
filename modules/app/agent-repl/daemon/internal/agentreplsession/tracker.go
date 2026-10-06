// Package agentreplsession owns agent-repl's SESSION (owner ruling,
// 2026-10-06): the span the topbar's connectivity dropdown reports, and the
// vendor traffic counted inside it.
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
// ONE SOURCE OF TRUTH: the dropdown's duration and its traffic both count from
// the session's start. A new session therefore starts its traffic at zero.
//
// # IT OUTLIVES THE DAEMON, NOT THE EDITOR
//
// The session is durable in wsm (agent_repl_session), so a daemon restart under
// the same Emacs — a bounce, a deploy — loads it and carries on counting. A
// new Emacs process is told apart from a reconnect by internal/editorinstance,
// and only a new one begins a session.
//
// # THE PUSH IS THROTTLED BY THE SAMPLER'S ROUND
//
// Traffic accumulates as the sampler counts it and is stated — persisted and
// pushed to every topbar — only when the sampler ends a round (FlushTraffic),
// and only when something was counted. A session's start is pushed at once.
package agentreplsession

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/vendortraffic"
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
type Tracker struct {
	store   Store
	publish Publisher
	log     dlog.Logger

	mu sync.Mutex
	// current is the standing session; nil before any began.
	current *wsm.AgentReplSession
	// pending is traffic counted since the last flush.
	pending vendortraffic.Counts
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
	t.current = &loaded
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
	if t.current != nil && !at.After(t.current.StartedAt) {
		standing := *t.current
		t.mu.Unlock()
		ctxFields := fields(standing)
		ctxFields["event"] = string(began)
		ctxFields["event_at"] = at.UTC().Format(time.RFC3339Nano)
		t.log.Info(op, "the standing session began later than this event; it stays", ctxFields)
		return
	}
	next := wsm.AgentReplSession{StartedAt: at, Began: began}
	t.current = &next
	// Traffic counted before this instant belongs to the session it replaces.
	t.pending = vendortraffic.Counts{}
	t.persistAndPublishLocked(ctx, op, next)
	t.mu.Unlock()
	t.log.Info(op, "a new session began", fields(next))
}

// AddTraffic accumulates traffic the sampler counted. With no session standing
// there is nothing to count it toward.
func (t *Tracker) AddTraffic(c vendortraffic.Counts) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.current == nil {
		return
	}
	t.pending = t.pending.Plus(c)
}

// FlushTraffic states the accumulated traffic: persisted and pushed, once,
// when anything was counted since the last flush.
func (t *Tracker) FlushTraffic() {
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.current == nil || t.pending.IsZero() {
		return
	}
	next := *t.current
	next.BytesReceived += t.pending.Received
	next.BytesSent += t.pending.Sent
	t.current = &next
	t.pending = vendortraffic.Counts{}
	t.persistAndPublishLocked(context.Background(), "daemon.agentreplsession.flush", next)
}

// persistAndPublishLocked writes the session and pushes it. A write that fails
// is recorded at ERROR — the session stands in this process and is pushed, but
// a daemon restart would not find it — and never withholds the push.
func (t *Tracker) persistAndPublishLocked(ctx context.Context, op string, session wsm.AgentReplSession) {
	if err := t.store.PutAgentReplSession(ctx, session); err != nil {
		ctxFields := fields(session)
		ctxFields["cause"] = err.Error()
		t.log.Error(op, "the session could not be persisted; a daemon restart would not carry it", ctxFields)
	}
	t.publish.SetAgentReplSession(view(session))
}

// view is the session as the topbar states it.
func view(s wsm.AgentReplSession) *frontendv1.TopbarAgentReplSession {
	out := &frontendv1.TopbarAgentReplSession{
		StartedAtMs:   s.StartedAt.UnixMilli(),
		BytesReceived: s.BytesReceived,
		BytesSent:     s.BytesSent,
	}
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

// fields is a session's structured context.
func fields(s wsm.AgentReplSession) dlog.Context {
	return dlog.Context{
		"started_at":     s.StartedAt.UTC().Format(time.RFC3339Nano),
		"began":          string(s.Began),
		"bytes_received": s.BytesReceived,
		"bytes_sent":     s.BytesSent,
	}
}
