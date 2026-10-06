package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// agentReplSessionDDL is agent-repl's SESSION (owner ruling, 2026-10-06) as
// a fresh file declares it: when it began and what began it, durable so a
// daemon restart under the same Emacs keeps it.
//
// It is a SINGLETON (CHECK (id = 1)): there is one session at a time, and a
// new one replaces the row whole. `began` is the arm of
// frontend.v1.TopbarAgentReplSession.began, spelled.
//
// A FILE OLDER THAN LAYOUT 26 REACHES THIS SHAPE BY TWO STEPS: layout 21
// created the table with the vendor traffic counted since the session began
// (agentReplSessionLayout21DDL), and layout 26 dropped those two columns
// (agentReplSessionDropTrafficDDL) when the traffic measurement was removed.
// TestAMigratedSessionTableMatchesAFreshOne keeps the two routes to one shape.
const agentReplSessionDDL = `
CREATE TABLE agent_repl_session (
  id             INTEGER PRIMARY KEY CHECK (id = 1),
  started_at     INTEGER NOT NULL,
  began          TEXT NOT NULL CHECK (began IN ('login', 'editor_start'))
);
`

// agentReplSessionLayout21DDL is the layout-21 step exactly as it shipped:
// the session table with the vendor traffic counted since it began. Layout 26
// drops the traffic again (agentReplSessionDropTrafficDDL); the text is kept
// because a file older than layout 21 still migrates through it.
const agentReplSessionLayout21DDL = `
CREATE TABLE agent_repl_session (
  id             INTEGER PRIMARY KEY CHECK (id = 1),
  started_at     INTEGER NOT NULL,
  began          TEXT NOT NULL CHECK (began IN ('login', 'editor_start')),
  bytes_received INTEGER NOT NULL CHECK (bytes_received >= 0),
  bytes_sent     INTEGER NOT NULL CHECK (bytes_sent >= 0)
);
`

// agentReplSessionDropTrafficDDL is the layout-26 step: the session's vendor
// traffic is REMOVED (owner ruling, 2026-10-06: the traffic measurement is
// dropped in its entirety). Its sampler held one kernel network-statistics
// control socket per vendor process, opened without close-on-exec, so every
// daemon generation leaked them into its successor and children, where
// nobody read them; they filled and are the inferred cause of the kernel's
// network buffer (mbuf) exhaustion that froze the owner's keyboard.
//
// The step is BREAKING: the build before it reads and writes both columns.
// Dropping them is lossless in the only sense that matters: nothing in this
// build reads a byte count, and no fresh file declares them.
const agentReplSessionDropTrafficDDL = `
ALTER TABLE agent_repl_session DROP COLUMN bytes_sent;
ALTER TABLE agent_repl_session DROP COLUMN bytes_received;
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
}

// LogContext is the session's structured context, the one shape every record
// about a session carries.
func (s AgentReplSession) LogContext() dlog.Context {
	return dlog.Context{
		"started_at": s.StartedAt.UTC().Format(time.RFC3339Nano),
		"began":      string(s.Began),
	}
}

// validate refuses a session the table could not hold faithfully.
func (s AgentReplSession) validate() error {
	switch {
	case s.StartedAt.IsZero():
		return errors.New("wsm: a session has the instant it began")
	case s.Began != SessionBeganLogin && s.Began != SessionBeganEditorStart:
		return fmt.Errorf("wsm: %q is not a cause a session can have", s.Began)
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
			startedAt int64
			began     string
		)
		err := s.db().QueryRowContext(ctx,
			`SELECT started_at, began FROM agent_repl_session WHERE id = 1`).
			Scan(&startedAt, &began)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		out = AgentReplSession{
			StartedAt: fromNanos(startedAt),
			Began:     SessionBegan(began),
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
	fields := session.LogContext()
	if err := session.validate(); err != nil {
		s.log.Error(op, "refused a session the table cannot hold", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO agent_repl_session (id, started_at, began) VALUES (1, ?, ?)
			 ON CONFLICT(id) DO UPDATE SET started_at = excluded.started_at, began = excluded.began`,
			nanos(session.StartedAt), string(session.Began))
		return err
	})
}
