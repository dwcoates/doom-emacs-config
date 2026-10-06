package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"math"
	"time"

	"claude-repld/internal/dlog"
)

// agentReplSessionDDL is the layout-21 addition: agent-repl's SESSION (owner
// ruling, 2026-10-06) — when it began, what began it, and the vendor traffic
// counted since — durable so a daemon restart under the same Emacs keeps it.
// Kept apart from the rest of the schema for the reason portedPromptsDDL is: a
// fresh file gets it as part of schemaDDL, a layout-20 file from the 20 -> 21
// migration, so one text keeps the two shapes from drifting.
//
// It is a SINGLETON (CHECK (id = 1)): there is one session at a time, and a
// new one replaces the row whole. `began` is the arm of
// frontend.v1.TopbarAgentReplSession.began, spelled.
const agentReplSessionDDL = `
CREATE TABLE agent_repl_session (
  id             INTEGER PRIMARY KEY CHECK (id = 1),
  started_at     INTEGER NOT NULL,
  began          TEXT NOT NULL CHECK (began IN ('login', 'editor_start')),
  bytes_received INTEGER NOT NULL CHECK (bytes_received >= 0),
  bytes_sent     INTEGER NOT NULL CHECK (bytes_sent >= 0)
);
`

// SessionBegan is what began agent-repl's session.
type SessionBegan string

// The causes a session can have.
const (
	// SessionBeganLogin is a login made through agent-repl's own login flow.
	SessionBeganLogin SessionBegan = "login"
	// SessionBeganEditorStart is this Emacs starting.
	SessionBeganEditorStart SessionBegan = "editor_start"
)

// AgentReplSession is agent-repl's session as it is persisted.
type AgentReplSession struct {
	// StartedAt is when the session began.
	StartedAt time.Time
	// Began is what began it.
	Began SessionBegan
	// BytesReceived and BytesSent are the vendor traffic counted since.
	BytesReceived uint64
	BytesSent     uint64
}

// validate refuses a session the table could not hold faithfully.
func (s AgentReplSession) validate() error {
	switch {
	case s.StartedAt.IsZero():
		return errors.New("wsm: a session has the instant it began")
	case s.Began != SessionBeganLogin && s.Began != SessionBeganEditorStart:
		return fmt.Errorf("wsm: %q is not a cause a session can have", s.Began)
	case s.BytesReceived > math.MaxInt64 || s.BytesSent > math.MaxInt64:
		return fmt.Errorf("wsm: traffic of %d received / %d sent exceeds what the table holds", s.BytesReceived, s.BytesSent)
	}
	return nil
}

// AgentReplSession loads the session, reporting false when none has begun.
func (s *store) AgentReplSession(ctx context.Context) (AgentReplSession, bool, error) {
	var (
		out   AgentReplSession
		found bool
	)
	err := s.read(ctx, "daemon.wsm.agent_repl_session", dlog.Context{}, func(ctx context.Context) error {
		var (
			startedAt      int64
			began          string
			received, sent int64
		)
		err := s.db().QueryRowContext(ctx,
			`SELECT started_at, began, bytes_received, bytes_sent FROM agent_repl_session WHERE id = 1`).
			Scan(&startedAt, &began, &received, &sent)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		out = AgentReplSession{
			StartedAt:     fromNanos(startedAt),
			Began:         SessionBegan(began),
			BytesReceived: uint64(received),
			BytesSent:     uint64(sent),
		}
		if err := out.validate(); err != nil {
			return &DecodeError{Table: "agent_repl_session", Row: "1", Err: err}
		}
		found = true
		return nil
	})
	return out, found, err
}

// PutAgentReplSession replaces the session whole.
func (s *store) PutAgentReplSession(ctx context.Context, session AgentReplSession) error {
	const op = "daemon.wsm.put_agent_repl_session"
	fields := dlog.Context{
		"started_at":     session.StartedAt.UTC().Format(time.RFC3339Nano),
		"began":          string(session.Began),
		"bytes_received": session.BytesReceived,
		"bytes_sent":     session.BytesSent,
	}
	if err := session.validate(); err != nil {
		s.log.Error(op, "refused a session the table cannot hold", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO agent_repl_session (id, started_at, began, bytes_received, bytes_sent) VALUES (1, ?, ?, ?, ?)
			 ON CONFLICT(id) DO UPDATE SET started_at = excluded.started_at, began = excluded.began,
			   bytes_received = excluded.bytes_received, bytes_sent = excluded.bytes_sent`,
			nanos(session.StartedAt), string(session.Began), int64(session.BytesReceived), int64(session.BytesSent))
		return err
	})
}
