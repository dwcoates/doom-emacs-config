package logging

import (
	"bytes"
	"encoding/json"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
)

type failingWriter struct{}

func (failingWriter) Write([]byte) (int, error) { return 0, errors.New("sink is gone") }

// pinned builds a Logger whose clock and pid are fixed, so a record's identity
// fields are assertable rather than merely present.
func pinned(sink *bytes.Buffer) (*Logger, time.Time) {
	at := time.Date(2026, 9, 3, 12, 34, 56, 789000000, time.UTC)
	log := New(sink)
	log.clock = func() time.Time { return at }
	log.pid = func() int { return 4242 }
	return log, at
}

func TestLogWritesTheCanonicalRecordShape(t *testing.T) {
	// Arrange
	var sink bytes.Buffer
	log, at := pinned(&sink)

	// Act
	log.Info("shim-lock.hold", "holding the lock until stdin closes", Context{"lock_path": "/run/a.lock"})

	// Assert
	var got map[string]any
	if err := json.Unmarshal(sink.Bytes(), &got); err != nil {
		t.Fatalf("record is not JSON: %v\n%s", err, sink.String())
	}
	want := map[string]any{
		"timestamp": sharedlogging.Timestamp(at),
		"runtime":   Runtime,
		"pid":       float64(4242),
		"level":     "info",
		"component": Component,
		"operation": "shim-lock.hold",
		"message":   "holding the lock until stdin closes",
		"context":   map[string]any{"lock_path": "/run/a.lock"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("record = %#v, want %#v", got, want)
	}
}

func TestErrorRecordsTheErrorLevel(t *testing.T) {
	// Arrange
	var sink bytes.Buffer
	log, _ := pinned(&sink)

	// Act
	log.Error("shim-lock.acquire", "the lock is already held by another process", nil)

	// Assert
	var got map[string]any
	if err := json.Unmarshal(sink.Bytes(), &got); err != nil {
		t.Fatalf("record is not JSON: %v", err)
	}
	if got["level"] != "error" {
		t.Fatalf("level = %v, want error", got["level"])
	}
}

func TestAbsentContextIsOmittedRatherThanEmitted(t *testing.T) {
	// Arrange
	var sink bytes.Buffer
	log, _ := pinned(&sink)

	// Act
	log.Info("shim-lock.release", "released", nil)

	// Assert: presence, never sentinels.
	if strings.Contains(sink.String(), `"context"`) {
		t.Fatalf("record carries an empty context: %s", sink.String())
	}
}

func TestEveryRecordEndsWithOneNewline(t *testing.T) {
	// Arrange: readers split this sink on lines, so a record that did not
	// terminate would glue itself to the next one.
	var sink bytes.Buffer
	log, _ := pinned(&sink)

	// Act
	log.Info("shim-lock.hold", "first", nil)
	log.Info("shim-lock.release", "second", nil)

	// Assert
	if lines := strings.Count(sink.String(), "\n"); lines != 2 {
		t.Fatalf("newline count = %d, want 2 for two records: %q", lines, sink.String())
	}
}

func TestARecordWithNoOperationIsRefused(t *testing.T) {
	// Arrange: an operation-less record cannot be correlated against the
	// daemon's or the shim's, so emitting it would be worse than refusing.
	var sink bytes.Buffer
	log, _ := pinned(&sink)
	defer func() {
		if recover() == nil {
			t.Fatal("an operation-less record was accepted")
		}
	}()

	// Act / Assert
	log.Info("", "nowhere", nil)
}

func TestSinkFailedReportsAWriteThatNeverLanded(t *testing.T) {
	// Arrange
	log := New(failingWriter{})

	// Act
	log.Info("shim-lock.hold", "holding", nil)

	// Assert: the failure is remembered, so main can exit nonzero over it.
	if !log.SinkFailed() {
		t.Fatal("a failed sink write was swallowed")
	}
}

func TestSinkFailedIsFalseWhileEveryRecordLands(t *testing.T) {
	// Arrange
	var sink bytes.Buffer
	log, _ := pinned(&sink)

	// Act
	log.Info("shim-lock.hold", "holding", nil)

	// Assert
	if log.SinkFailed() {
		t.Fatal("a successful write was reported as a sink failure")
	}
}
