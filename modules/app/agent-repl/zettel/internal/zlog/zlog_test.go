package zlog

import (
	"bytes"
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
)

func fixedClock() time.Time { return time.Date(2026, 10, 24, 12, 0, 0, 0, time.UTC) }

func decode(t *testing.T, line string) map[string]any {
	t.Helper()
	var out map[string]any
	if err := json.Unmarshal([]byte(line), &out); err != nil {
		t.Fatalf("record is not JSON: %v: %q", err, line)
	}
	return out
}

func TestRecordShape(t *testing.T) {
	// Arrange
	var sink, terminal bytes.Buffer
	log := New(&sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock).With(Context{"thread": "t1"})

	// Act
	log.Error("zettel.exchange.begin", "the write failed", Context{"cause": "disk full"})

	// Assert
	rec := decode(t, strings.TrimSpace(sink.String()))
	for key, want := range map[string]any{"runtime": "zettel", "level": "error", "verbosity": "normal", "operation": "zettel.exchange.begin", "message": "the write failed"} {
		if rec[key] != want {
			t.Errorf("%s = %v, want %v", key, rec[key], want)
		}
	}
	ctx := rec["context"].(map[string]any)
	if ctx["thread"] != "t1" || ctx["cause"] != "disk full" {
		t.Errorf("context = %v, want the base and the call's fields merged", ctx)
	}
	if terminal.Len() != 0 {
		t.Errorf("terminal got %q, want nothing while the sink accepts", terminal.String())
	}
}

func TestLevelWindowDropsBelowThreshold(t *testing.T) {
	// Arrange
	var sink, terminal bytes.Buffer
	log := New(&sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelInfo), fixedClock)

	// Act
	log.Debug("zettel.test", "dropped", nil)

	// Assert
	if sink.Len() != 0 {
		t.Errorf("sink got %q, want a debug record dropped at info", sink.String())
	}
}

type refusingWriter struct{}

func (refusingWriter) Write([]byte) (int, error) { return 0, errors.New("read-only file system") }

func TestRefusingSinkFallsToTerminal(t *testing.T) {
	// Arrange
	var terminal bytes.Buffer
	log := New(refusingWriter{}, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock)

	// Act
	log.Warn("zettel.test", "kept", nil)

	// Assert
	if !strings.Contains(terminal.String(), "read-only file system") || !strings.Contains(terminal.String(), `"message":"kept"`) {
		t.Errorf("terminal = %q, want the refusal and the record", terminal.String())
	}
}

func TestWithDoesNotLeakIntoParent(t *testing.T) {
	// Arrange
	var sink, terminal bytes.Buffer
	parent := New(&sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock)
	_ = parent.With(Context{"child": true})

	// Act
	parent.Info("zettel.test", "parent", nil)

	// Assert
	ctx := decode(t, strings.TrimSpace(sink.String()))["context"].(map[string]any)
	if _, leaked := ctx["child"]; leaked {
		t.Errorf("parent record carries the child's field: %v", ctx)
	}
}

func TestEmptyOperationPanics(t *testing.T) {
	// Arrange
	var sink, terminal bytes.Buffer
	log := New(&sink, &terminal, sharedlogging.FixedWindow(sharedlogging.LevelDebug), fixedClock)
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("an empty operation did not panic")
		}
	}()

	// Act
	log.Info("", "no operation", nil)
}
