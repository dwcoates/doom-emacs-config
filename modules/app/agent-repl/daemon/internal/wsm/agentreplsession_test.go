package wsm

import (
	"context"
	"errors"
	"math"
	"testing"
	"time"
)

// sessionAt is a fixed instant the session tests record against.
var sessionAt = time.Date(2026, 10, 6, 9, 30, 0, 0, time.UTC)

func TestAgentReplSessionIsAbsentBeforeAnyBegan(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, found, err := s.AgentReplSession(context.Background())

	// Assert
	if err != nil || found {
		t.Fatalf("AgentReplSession = (found %v, %v), want (false, nil)", found, err)
	}
}

func TestAPutSessionReadsBackWhole(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	want := AgentReplSession{StartedAt: sessionAt, Began: SessionBeganLogin, BytesReceived: 412 << 20, BytesSent: 38 << 20}

	// Act
	if err := s.PutAgentReplSession(context.Background(), want); err != nil {
		t.Fatalf("PutAgentReplSession: %v", err)
	}
	got, found, err := s.AgentReplSession(context.Background())

	// Assert
	if err != nil || !found || got != want {
		t.Fatalf("AgentReplSession = (%+v, %v, %v), want (%+v, true, nil)", got, found, err, want)
	}
}

func TestANewSessionReplacesTheOldOneWhole(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	old := AgentReplSession{StartedAt: sessionAt, Began: SessionBeganLogin, BytesReceived: 900, BytesSent: 90}
	if err := s.PutAgentReplSession(context.Background(), old); err != nil {
		t.Fatalf("PutAgentReplSession: %v", err)
	}
	want := AgentReplSession{StartedAt: sessionAt.Add(time.Hour), Began: SessionBeganEditorStart}

	// Act
	if err := s.PutAgentReplSession(context.Background(), want); err != nil {
		t.Fatalf("PutAgentReplSession: %v", err)
	}
	got, _, err := s.AgentReplSession(context.Background())

	// Assert
	if err != nil || got != want {
		t.Fatalf("AgentReplSession = (%+v, %v), want %+v", got, err, want)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM agent_repl_session`); n != 1 {
		t.Fatalf("session rows = %d, want the one singleton", n)
	}
}

func TestPutAgentReplSessionRefusesASessionTheTableCannotHold(t *testing.T) {
	tests := []struct {
		name    string
		session AgentReplSession
	}{
		{name: "no start instant", session: AgentReplSession{Began: SessionBeganLogin}},
		{name: "an unknown cause", session: AgentReplSession{StartedAt: sessionAt, Began: "reboot"}},
		{name: "traffic past the column's range", session: AgentReplSession{StartedAt: sessionAt, Began: SessionBeganLogin, BytesReceived: math.MaxInt64 + 1}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			err := s.PutAgentReplSession(context.Background(), tt.session)

			// Assert
			if err == nil {
				t.Fatal("PutAgentReplSession = nil, want a refusal")
			}
			if !loggedOperation(log, "daemon.wsm.put_agent_repl_session", "error") {
				t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
			}
			if _, found, _ := s.AgentReplSession(context.Background()); found {
				t.Fatal("a refused session was written")
			}
		})
	}
}

func TestPutAgentReplSessionRefusesAReadOnlyHandle(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	s.current.Store(&handleState{db: s.db(), readOnly: true})

	// Act
	err := s.PutAgentReplSession(context.Background(), AgentReplSession{StartedAt: sessionAt, Began: SessionBeganLogin})

	// Assert
	if !errors.Is(err, ErrReadOnly) {
		t.Fatalf("PutAgentReplSession = %v, want ErrReadOnly", err)
	}
}

func TestAStoredSessionWithAnUnknownCauseIsADecodeError(t *testing.T) {
	// Arrange: the CHECK is dropped by rebuilding the table, so the decoder's
	// own refusal is what is exercised.
	s, _ := testStore(t)
	corrupt(t, s, `DROP TABLE agent_repl_session`)
	corrupt(t, s, `CREATE TABLE agent_repl_session (id INTEGER PRIMARY KEY, started_at INTEGER, began TEXT, bytes_received INTEGER, bytes_sent INTEGER)`)
	corrupt(t, s, `INSERT INTO agent_repl_session VALUES (1, ?, 'reboot', 0, 0)`, nanos(sessionAt))

	// Act
	_, _, err := s.AgentReplSession(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Table != "agent_repl_session" {
		t.Fatalf("AgentReplSession = %v, want a DecodeError on agent_repl_session", err)
	}
}

func TestTheMigrationAddsTheAgentReplSessionTable(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 20)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-20 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'agent_repl_session'`); got != 1 {
		t.Fatalf("agent_repl_session tables after the migration = %d, want 1", got)
	}
}

func TestAgentReplSessionLogContextNamesEveryFact(t *testing.T) {
	// Arrange
	s := AgentReplSession{StartedAt: sessionAt, Began: SessionBeganLogin, BytesReceived: 5, BytesSent: 6}

	// Act
	got := s.LogContext()

	// Assert
	if got["started_at"] != "2026-10-06T09:30:00Z" || got["began"] != "login" || got["bytes_received"] != uint64(5) || got["bytes_sent"] != uint64(6) {
		t.Fatalf("LogContext = %v, want every fact named", got)
	}
}
