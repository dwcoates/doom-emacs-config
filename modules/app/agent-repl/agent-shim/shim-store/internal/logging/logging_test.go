package logging

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"regexp"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
)

type failingWriter struct{ err error }

func (w failingWriter) Write([]byte) (int, error) { return 0, w.err }

type shortWriter struct{ writes int }

func (w *shortWriter) Write(p []byte) (int, error) {
	w.writes++
	if len(p) > 3 {
		return 3, nil
	}
	return len(p), nil
}

func TestLogFormatsBoundAndRecordContext(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false).With(Fields{
		Component: "db", DatabasePath: "/tmp/events.db", Table: "event",
	})
	at := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)
	log.clock = func() time.Time { return at }
	log.pid = func() int { return 84 }

	log.Log(Fields{Session: "vendor-session", Producer: "sidecar", Operation: "ingest"}, "accepted=%d", 2)

	var got record
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("persistent record is not JSON: %v\n%s", err, file.String())
	}
	if got.Timestamp != at.Local().Format(sharedlogging.TimestampLayout) || got.Runtime != "store" || got.PID != 84 {
		t.Fatalf("runtime identity = %#v", got)
	}
	if got.Level != "info" || got.Verbosity != "normal" || got.Operation != "ingest" || got.Message != "accepted=2" {
		t.Fatalf("record fields = %#v", got)
	}
	if got.ClaudeSessionID != "vendor-session" || got.Context["component"] != "db" || got.Context["db"] != "/tmp/events.db" || got.Context["table"] != "event" || got.Context["producer"] != "sidecar" {
		t.Fatalf("record attribution = %#v", got)
	}
	if stderr.String() != file.String() {
		t.Fatalf("normal routing differs: file=%q stderr=%q", file.String(), stderr.String())
	}
}

func TestLogMarshalsReplayAndTerminalAttributionExactly(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false).With(Fields{
		AgentReplSessionID: "agent-1",
		Session:            "claude-1",
		RequestID:          "request-1",
		ReplayFromSeq:      10,
		ReplayFirstSeq:     11,
		ReplayLastSeq:      12,
		Delivered:          2,
	})
	at := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)
	log.clock = func() time.Time { return at }
	log.pid = func() int { return 84 }

	log.Log(Fields{
		Component:      "replay",
		TerminalOwner:  "subscriber",
		TerminalReason: "completed",
		ErrorCause:     "connection reset",
		Operation:      "store.replay",
	}, "replay finished")

	var got map[string]any
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("record is not JSON: %v: %q", err, file.String())
	}
	want := map[string]any{
		"timestamp":             at.Local().Format(sharedlogging.TimestampLayout),
		"runtime":               "store",
		"pid":                   float64(84),
		"level":                 "info",
		"verbosity":             "normal",
		"operation":             "store.replay",
		"message":               "replay finished",
		"agent_repl_session_id": "agent-1",
		"claude_session_id":     "claude-1",
		"request_id":            "request-1",
		"context": map[string]any{
			"component":        "replay",
			"replay_from_seq":  float64(10),
			"replay_first_seq": float64(11),
			"replay_last_seq":  float64(12),
			"delivered":        float64(2),
			"terminal_owner":   "subscriber",
			"terminal_reason":  "completed",
			"error":            "connection reset",
		},
	}
	assertJSONExactly(t, got, want)
}

func TestLogOmitsEmptyReplayAndTerminalAttribution(t *testing.T) {
	var file, stderr bytes.Buffer
	New(&file, &stderr, false).Log(Fields{Operation: "store.replay"}, "replay finished")

	var got map[string]any
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("record is not JSON: %v: %q", err, file.String())
	}
	context := got["context"].(map[string]any)
	for _, key := range []string{"replay_from_seq", "replay_first_seq", "replay_last_seq", "delivered", "terminal_owner", "terminal_reason", "error"} {
		if _, exists := context[key]; exists {
			t.Fatalf("context unexpectedly contains %q: %#v", key, context)
		}
	}
}

func TestLogMarshalsZeroReplayCountersForTerminalRecordExactly(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)
	at := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)
	log.clock = func() time.Time { return at }
	log.pid = func() int { return 84 }

	log.Log(Fields{TerminalOwner: "subscriber", TerminalReason: "exhausted", Operation: "store.replay"}, "replay finished")

	var got map[string]any
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("record is not JSON: %v: %q", err, file.String())
	}
	want := map[string]any{
		"timestamp": at.Local().Format(sharedlogging.TimestampLayout),
		"runtime":   "store",
		"pid":       float64(84),
		"level":     "info",
		"verbosity": "normal",
		"operation": "store.replay",
		"message":   "replay finished",
		"context": map[string]any{
			"replay_from_seq":  float64(0),
			"replay_first_seq": float64(0),
			"replay_last_seq":  float64(0),
			"delivered":        float64(0),
			"terminal_owner":   "subscriber",
			"terminal_reason":  "exhausted",
		},
	}
	assertJSONExactly(t, got, want)
}

func assertJSONExactly(t *testing.T, got, want map[string]any) {
	t.Helper()
	gotJSON, gotErr := json.Marshal(got)
	wantJSON, wantErr := json.Marshal(want)
	if gotErr != nil || wantErr != nil {
		t.Fatalf("marshal record=%v want=%v", gotErr, wantErr)
	}
	if string(gotJSON) != string(wantJSON) {
		t.Fatalf("record = %s\nwant = %s", gotJSON, wantJSON)
	}
}

func TestLogVerboseRequiresEnabledModeForBothSinks(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)
	log.LogVerbose(Fields{Operation: "tail"}, "queued=%d", 4)
	if file.Len() != 0 || stderr.Len() != 0 {
		t.Fatalf("disabled verbose mutated sinks: file=%q stderr=%q", file.String(), stderr.String())
	}

	enabled := New(&file, &stderr, true)
	enabled.LogVerbose(Fields{Operation: "tail"}, "queued=%d", 4)
	var verboseRecord record
	if err := json.Unmarshal(file.Bytes(), &verboseRecord); err != nil {
		t.Fatalf("persistent verbose record is not JSON: %v", err)
	}
	if verboseRecord.Verbosity != "verbose" || verboseRecord.Operation != "tail" || verboseRecord.Message != "queued=4" {
		t.Fatalf("persistent verbose record = %#v", verboseRecord)
	}
	if file.String() != stderr.String() {
		t.Fatalf("enabled verbose routing differs: file=%q stderr=%q", file.String(), stderr.String())
	}
}

func TestLogRejectsMissingOperationAndInvalidLevel(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)
	got := capturePanic(t, func() {
		log.Log(Fields{}, "record")
	})
	if !strings.Contains(fmt.Sprint(got), "operation is required") {
		t.Fatalf("panic = %v, want missing operation", got)
	}
	got = capturePanic(t, func() {
		log.Log(Fields{Operation: "write", Level: "fatal"}, "record")
	})
	if !strings.Contains(fmt.Sprint(got), "invalid level") {
		t.Fatalf("panic = %v, want invalid level", got)
	}
	got = capturePanic(t, func() {
		log.LogVerbose(Fields{}, "record")
	})
	if !strings.Contains(fmt.Sprint(got), "operation is required") {
		t.Fatalf("disabled verbose panic = %v, want missing operation", got)
	}
	got = capturePanic(t, func() {
		log.LogVerbose(Fields{Operation: "write", Level: "fatal"}, "record")
	})
	if !strings.Contains(fmt.Sprint(got), "invalid level") {
		t.Fatalf("disabled verbose panic = %v, want invalid level", got)
	}
	if file.Len() != 0 || stderr.Len() != 0 {
		t.Fatalf("invalid records mutated sinks: file=%q stderr=%q", file.String(), stderr.String())
	}
}

func TestLogFailsLoudlyWhenPersistentSinkCannotWrite(t *testing.T) {
	err := errors.New("disk full")
	var stderr bytes.Buffer
	got := capturePanic(t, func() {
		New(failingWriter{err}, &stderr, false).Log(Fields{Operation: "write", Level: "error"}, "critical")
	})
	var emergency record
	if err := json.Unmarshal(stderr.Bytes(), &emergency); err != nil {
		t.Fatalf("emergency stderr is not JSON: %v: %q", err, stderr.String())
	}
	if emergency.Operation != "store.logging.sink-failure" || emergency.Level != "error" {
		t.Fatalf("emergency stderr record = %#v", emergency)
	}
	if !strings.Contains(fmt.Sprint(got), "disk full") {
		t.Fatalf("panic = %v, want persistent sink failure", got)
	}
}

func TestLogFailsLoudlyWhenStderrCannotWrite(t *testing.T) {
	var file bytes.Buffer
	err := errors.New("terminal closed")
	got := capturePanic(t, func() {
		New(&file, failingWriter{err}, false).Log(Fields{Operation: "write", Level: "error"}, "critical")
	})
	if !strings.Contains(file.String(), "critical") {
		t.Fatalf("persistent record missing before stderr failure: %q", file.String())
	}
	if !strings.Contains(fmt.Sprint(got), "terminal closed") {
		t.Fatalf("panic = %v, want stderr sink failure", got)
	}
}

func TestLogReportsBothSinkFailures(t *testing.T) {
	fileErr := errors.New("disk full")
	stderrErr := errors.New("terminal closed")
	got := capturePanic(t, func() {
		New(failingWriter{fileErr}, failingWriter{stderrErr}, false).Log(Fields{Operation: "write", Level: "error"}, "critical")
	})
	message := fmt.Sprint(got)
	if !strings.Contains(message, "disk full") || !strings.Contains(message, "terminal closed") {
		t.Fatalf("panic = %v, want both sink failures", got)
	}
}

func TestLogCompletesShortPersistentWritesAcrossBoundLoggers(t *testing.T) {
	file := &shortWriter{}
	var stderr bytes.Buffer
	log := New(file, &stderr, false)
	bound := log.With(Fields{Component: "db"})
	log.Log(Fields{Operation: "write", Level: "error"}, "critical")
	if file.writes <= 1 {
		t.Fatalf("persistent writes = %d, want multiple writes", file.writes)
	}
	firstWrites := file.writes
	bound.Log(Fields{Operation: "write", Level: "error"}, "later critical")
	if file.writes <= firstWrites {
		t.Fatalf("bound logger did not complete another record: writes=%d", file.writes)
	}
}

func TestLogCompletesShortTerminalWrite(t *testing.T) {
	var file bytes.Buffer
	stderr := &shortWriter{}
	New(&file, stderr, false).Log(Fields{Operation: "write", Level: "error"}, "critical")
	if stderr.writes <= 1 {
		t.Fatalf("terminal writes = %d, want multiple writes", stderr.writes)
	}
	var record record
	if err := json.Unmarshal(file.Bytes(), &record); err != nil {
		t.Fatalf("durable error record is not JSON: %v", err)
	}
	if record.Level != "error" || record.Message != "critical" {
		t.Fatalf("durable critical record = %#v", record)
	}
}

func TestLoggerRejectsMissingDependencies(t *testing.T) {
	assertPanics := func(name string, call func()) {
		t.Helper()
		defer func() {
			if recover() == nil {
				t.Errorf("%s did not panic", name)
			}
		}()
		call()
	}
	assertPanics("nil file", func() { New(nil, io.Discard, false) })
	assertPanics("nil stderr", func() { New(io.Discard, nil, false) })
	var logger *Logger
	assertPanics("nil With", func() { logger.With(Fields{}) })
	assertPanics("nil Log", func() { logger.Log(Fields{}, "record") })
	assertPanics("nil LogVerbose", func() { logger.LogVerbose(Fields{}, "record") })
}

func capturePanic(t *testing.T, call func()) any {
	t.Helper()
	var got any
	func() {
		defer func() { got = recover() }()
		call()
	}()
	if got == nil {
		t.Fatal("call did not panic")
	}
	return got
}

// canonicalTimestampPattern is the shared shape every agent-repl runtime emits:
// RFC 3339, 24-hour clock, fixed-width microseconds, explicit numeric offset.
var canonicalTimestampPattern = regexp.MustCompile(`^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}[+-]\d{2}:\d{2}$`)

func TestLogTimestampUsesCanonicalFixedWidthLayout(t *testing.T) {
	// Arrange: a whole second, whose subsecond digits RFC3339Nano would drop.
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)
	log.clock = func() time.Time { return time.Date(2026, 7, 28, 12, 34, 56, 0, time.UTC) }

	// Act
	log.Log(Fields{Operation: "ingest"}, "accepted")

	// Assert
	var got record
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatal(err)
	}
	if !canonicalTimestampPattern.MatchString(got.Timestamp) {
		t.Fatalf("timestamp = %q, want canonical layout", got.Timestamp)
	}
}

func TestLogTimestampUsesLocalZoneRatherThanUTC(t *testing.T) {
	// Arrange
	at := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)
	log.clock = func() time.Time { return at }

	// Act
	log.Log(Fields{Operation: "ingest"}, "accepted")

	// Assert
	var got record
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatal(err)
	}
	if got.Timestamp != at.Local().Format(sharedlogging.TimestampLayout) || strings.HasSuffix(got.Timestamp, "Z") {
		t.Fatalf("timestamp = %q, want %q", got.Timestamp, at.Local().Format(sharedlogging.TimestampLayout))
	}
}
