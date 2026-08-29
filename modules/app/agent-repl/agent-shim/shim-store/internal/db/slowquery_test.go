package db

import (
	"errors"
	"testing"
	"time"

	"agentrepl/shim-store/internal/logging"
)

func TestSlowQueryFromEnvUsesTheShippedDefaultWhenUnset(t *testing.T) {
	// Arrange
	t.Setenv(EnvSlowQueryMs, "")

	// Act
	got, err := SlowQueryFromEnv()

	// Assert
	if err != nil {
		t.Fatalf("SlowQueryFromEnv: %v", err)
	}
	if got != DefaultSlowQuery {
		t.Fatalf("threshold = %v, want %v", got, DefaultSlowQuery)
	}
}

func TestSlowQueryFromEnvReadsAnExplicitThreshold(t *testing.T) {
	// Arrange
	t.Setenv(EnvSlowQueryMs, "50")

	// Act
	got, err := SlowQueryFromEnv()

	// Assert
	if err != nil {
		t.Fatalf("SlowQueryFromEnv: %v", err)
	}
	if got != 50*time.Millisecond {
		t.Fatalf("threshold = %v, want 50ms", got)
	}
}

func TestSlowQueryFromEnvRefusesAMalformedValue(t *testing.T) {
	// Arrange
	t.Setenv(EnvSlowQueryMs, "soon")

	// Act
	_, err := SlowQueryFromEnv()

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestSlowQueryFromEnvRefusesANonPositiveValue(t *testing.T) {
	// Arrange: an operator who set it to zero meant something by it, and
	// running the shipped default underneath them is the failure the loud
	// refusal exists to prevent.
	t.Setenv(EnvSlowQueryMs, "0")

	// Act
	_, err := SlowQueryFromEnv()

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestObserveQueryReportsAStatementOverTheThreshold(t *testing.T) {
	// Arrange
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Nanosecond}

	// Act
	d.observeQuery(StatementOpenPage, "entry", logging.Fields{BookAgentID: "agent-1"}, time.Now().Add(-time.Second), 12)

	// Assert
	s.assertLogged(t, "warn", "exceeded the slow-query threshold")
	s.assertContext(t, "statement", StatementOpenPage)
	s.assertContext(t, "book_agent_id", "agent-1")
	s.assertContext(t, "rows", float64(12))
}

func TestObserveQuerySaysNothingAboutAFastStatement(t *testing.T) {
	// Arrange: successful query timing is exactly the high-volume narration
	// the verbose gate exists to keep out of a singleton global log.
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: time.Hour}

	// Act
	d.observeQuery(StatementWriteBatch, "entry", logging.Fields{}, time.Now(), 1)

	// Assert
	if len(s.records(t)) != 0 {
		t.Fatalf("a fast statement was reported: %s", s.file.String())
	}
}

func TestObserveQuerySaysNothingWhenReportingIsDisabled(t *testing.T) {
	// Arrange
	s, log := newSink(t)
	d := &DB{log: log, slowQuery: 0}

	// Act
	d.observeQuery(StatementWriteBatch, "entry", logging.Fields{}, time.Now().Add(-time.Hour), 1)

	// Assert
	if len(s.records(t)) != 0 {
		t.Fatalf("reporting was disabled but a record was written: %s", s.file.String())
	}
}
