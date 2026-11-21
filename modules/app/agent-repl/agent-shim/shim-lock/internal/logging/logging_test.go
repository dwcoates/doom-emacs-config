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
	"agentrepl/logging/logsite"
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
		"site":      logsite.Key(Runtime, "agentrepl/shim-lock/internal/logging.TestLogWritesTheCanonicalRecordShape", "holding the lock until stdin closes"),
		"template":  "holding the lock until stdin closes",
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

func TestAnUnmarshallableContextStillEmitsTheBranchItWasReporting(t *testing.T) {
	// Arrange: a context value encoding/json cannot represent. The branch being
	// reported must survive the encoder's failure rather than vanish with it.
	var sink bytes.Buffer
	log, _ := pinned(&sink)

	// Act
	log.Error("shim-lock.acquire", "flock failed", Context{"unencodable": make(chan int)})

	// Assert
	line := sink.String()
	if !strings.Contains(line, `"operation":"shim-lock.acquire"`) {
		t.Fatalf("record = %q, want the operation preserved", line)
	}
	if !strings.Contains(line, "record could not be marshalled") {
		t.Fatalf("record = %q, want the marshalling failure named", line)
	}
	if !strings.Contains(line, "flock failed") {
		t.Fatalf("record = %q, want the original message preserved", line)
	}
}

func TestEveryRecordRendersItsTemplateFromItsNamedValues(t *testing.T) {
	const function = "agentrepl/shim-lock/internal/logging.TestEveryRecordRendersItsTemplateFromItsNamedValues"
	tests := []struct {
		name        string
		log         func(l *Logger)
		template    string
		wantMessage string
		wantContext map[string]any
	}{
		{
			name:        "a placeholder takes the call's own value",
			log:         func(l *Logger) { l.Error("shim-lock.acquire", "flock failed: {cause}", Context{"cause": "EIO"}) },
			template:    "flock failed: {cause}",
			wantMessage: "flock failed: EIO",
			wantContext: map[string]any{"cause": "EIO"},
		},
		{
			name: "a placeholder takes a value bound with With",
			log: func(l *Logger) {
				l.With(Context{"lock_path": "/run/a.lock"}).Info("shim-lock.hold", "holding {lock_path}", nil)
			},
			template:    "holding {lock_path}",
			wantMessage: "holding /run/a.lock",
			wantContext: map[string]any{"lock_path": "/run/a.lock"},
		},
		{
			name:        "a doubled brace renders as one literal brace",
			log:         func(l *Logger) { l.Info("shim-lock.hold", "a {{literal}} brace", nil) },
			template:    "a {{literal}} brace",
			wantMessage: "a {literal} brace",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var sink bytes.Buffer
			log, _ := pinned(&sink)

			// Act
			tt.log(log)

			// Assert
			var got map[string]any
			if err := json.Unmarshal(sink.Bytes(), &got); err != nil {
				t.Fatalf("record is not JSON: %v", err)
			}
			if got["message"] != tt.wantMessage || got["template"] != tt.template {
				t.Errorf("message, template = %v, %v, want %q, %q", got["message"], got["template"], tt.wantMessage, tt.template)
			}
			if want := logsite.Key(Runtime, function, tt.template); got["site"] != want {
				t.Errorf("site = %v, want %q", got["site"], want)
			}
			if ctx, _ := got["context"].(map[string]any); tt.wantContext != nil && !reflect.DeepEqual(ctx, tt.wantContext) {
				t.Errorf("context = %v, want %v", ctx, tt.wantContext)
			}
		})
	}
}

func TestAnUnrenderableRecordPanics(t *testing.T) {
	tests := []struct {
		name string
		log  func(l *Logger)
	}{
		{name: "a placeholder no value fills", log: func(l *Logger) { l.Error("shim-lock.acquire", "lost {gone}", Context{"here": 1}) }},
		{name: "a template whose braces do not parse", log: func(l *Logger) { l.Info("shim-lock.hold", "unclosed {", nil) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var sink bytes.Buffer
			log, _ := pinned(&sink)
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatal("the record was written instead of panicking")
				}
				if sink.Len() != 0 {
					t.Errorf("sink got %q, want nothing", sink.String())
				}
			}()

			// Act
			tt.log(log)
		})
	}
}

func TestWithSharesTheSinkAndLeavesItsParentAlone(t *testing.T) {
	// Arrange
	log := New(failingWriter{})
	child := log.With(Context{"lock_path": "/run/a.lock"})

	// Act
	child.Info("shim-lock.hold", "holding", nil)

	// Assert
	if !log.SinkFailed() {
		t.Fatal("the child's failed write was not seen by its parent")
	}
	if len(log.base) != 0 {
		t.Fatalf("the parent's base = %v, want the child's field kept out of it", log.base)
	}
}
