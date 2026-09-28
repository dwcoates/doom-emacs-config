package logging

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"reflect"
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

	log.Log(Fields{VendorSessionID: "vendor-session", Producer: "sidecar", Operation: "write-batch"}, "accepted=%d", 2)

	var got record
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("persistent record is not JSON: %v\n%s", err, file.String())
	}
	if got.Timestamp != at.Local().Format(sharedlogging.TimestampLayout) || got.Runtime != "store" || got.PID != 84 {
		t.Fatalf("runtime identity = %#v", got)
	}
	if got.Level != "info" || got.Verbosity != "normal" || got.Operation != "write-batch" || got.Message != "accepted=2" {
		t.Fatalf("record fields = %#v", got)
	}
	if got.Context["vendor_session_id"] != "vendor-session" || got.Context["component"] != "db" || got.Context["db"] != "/tmp/events.db" || got.Context["table"] != "event" || got.Context["producer"] != "sidecar" {
		t.Fatalf("record attribution = %#v", got)
	}
	if stderr.String() != file.String() {
		t.Fatalf("normal routing differs: file=%q stderr=%q", file.String(), stderr.String())
	}
}

func TestLogMarshalsCorrelationAndTerminalAttributionExactly(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false).With(Fields{
		AgentReplSessionID: "agent-1",
		RequestID:          "request-1",
		AgentID:            "agent-value",
		BookAgentID:        "book-value",
		WriteID:            "write-value",
		UpsertKey:          "upsert-value",
		Position:           "sip1-2a",
		WriteSeq:           12,
		Delivered:          2,
	})
	at := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)
	log.clock = func() time.Time { return at }
	log.pid = func() int { return 84 }

	log.Log(Fields{
		Component:      "fanout",
		TerminalOwner:  "subscriber",
		TerminalReason: "completed",
		ErrorCause:     "connection reset",
		Operation:      "store.watch",
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
		"operation":             "store.watch",
		"message":               "replay finished",
		"agent_repl_session_id": "agent-1",
		"request_id":            "request-1",
		"context": map[string]any{
			"component":       "fanout",
			"agent_id":        "agent-value",
			"book_agent_id":   "book-value",
			"write_id":        "write-value",
			"upsert_key":      "upsert-value",
			"position":        "sip1-2a",
			"write_seq":       float64(12),
			"delivered":       float64(2),
			"terminal_owner":  "subscriber",
			"terminal_reason": "completed",
			"error":           "connection reset",
		},
	}
	assertJSONExactly(t, got, want)
}

func TestLogOmitsEmptyCorrelationAndTerminalAttribution(t *testing.T) {
	var file, stderr bytes.Buffer
	New(&file, &stderr, false).Log(Fields{Operation: "store.watch"}, "replay finished")

	var got map[string]any
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("record is not JSON: %v: %q", err, file.String())
	}
	context := got["context"].(map[string]any)
	for _, key := range []string{"agent_id", "book_agent_id", "write_id", "upsert_key", "position", "write_seq", "offset", "delivered", "terminal_owner", "terminal_reason", "error"} {
		if _, exists := context[key]; exists {
			t.Fatalf("context unexpectedly contains %q: %#v", key, context)
		}
	}
}

func TestLogMarshalsAggregateAgentAttribution(t *testing.T) {
	// Arrange.
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)

	// Act.
	log.Log(Fields{
		Operation:    "store.rpc.write-batch",
		AgentIDs:     []string{"agent-a", "agent-b"},
		BookAgentIDs: []string{"book-a", "book-b"},
	}, "accepted")

	// Assert.
	var got map[string]any
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("record is not JSON: %v: %q", err, file.String())
	}
	context := got["context"].(map[string]any)
	if fmt.Sprint(context["agent_ids"]) != "[agent-a agent-b]" {
		t.Fatalf("agent_ids = %#v, want both agents", context["agent_ids"])
	}
	if fmt.Sprint(context["book_agent_ids"]) != "[book-a book-b]" {
		t.Fatalf("book_agent_ids = %#v, want both books", context["book_agent_ids"])
	}
}

func TestLogMarshalsZeroDeliveredForTerminalRecordExactly(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)
	at := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)
	log.clock = func() time.Time { return at }
	log.pid = func() int { return 84 }

	log.Log(Fields{TerminalOwner: "subscriber", TerminalReason: "exhausted", Operation: "store.watch"}, "replay finished")

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
		"operation": "store.watch",
		"message":   "replay finished",
		"context": map[string]any{
			"delivered":       float64(0),
			"terminal_owner":  "subscriber",
			"terminal_reason": "exhausted",
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
	if verboseRecord.Level != "debug" || verboseRecord.Verbosity != "verbose" || verboseRecord.Operation != "tail" || verboseRecord.Message != "queued=4" {
		t.Fatalf("persistent verbose record = %#v", verboseRecord)
	}
	if file.String() != stderr.String() {
		t.Fatalf("enabled verbose routing differs: file=%q stderr=%q", file.String(), stderr.String())
	}
}

func TestLogLevelFiltersPersistentAndInteractiveSinksTogether(t *testing.T) {
	tests := []struct {
		name      string
		threshold sharedlogging.Level
		record    Fields
		want      bool
	}{
		{name: "debug excludes verbose", threshold: sharedlogging.LevelInfo, record: Fields{Operation: "tail"}, want: false},
		{name: "warn excludes info", threshold: sharedlogging.LevelWarn, record: Fields{Operation: "serve"}, want: false},
		{name: "warn includes warn", threshold: sharedlogging.LevelWarn, record: Fields{Operation: "retry", Level: "warn"}, want: true},
		{name: "error includes error", threshold: sharedlogging.LevelError, record: Fields{Operation: "abort", Level: "error"}, want: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			var file, stderr bytes.Buffer
			log := NewAtLevel(&file, &stderr, tt.threshold)

			// Act.
			if tt.record.Level == "" && tt.record.Operation == "tail" {
				log.LogVerbose(tt.record, "record")
			} else {
				log.Log(tt.record, "record")
			}

			// Assert.
			if got := file.Len() > 0; got != tt.want {
				t.Fatalf("persistent record present = %t, want %t: %q", got, tt.want, file.String())
			}
			if got := stderr.Len() > 0; got != tt.want {
				t.Fatalf("interactive record present = %t, want %t: %q", got, tt.want, stderr.String())
			}
		})
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

func TestStatementFamilyEmitsTheQueryTimingGroup(t *testing.T) {
	tests := []struct {
		name   string
		fields Fields
		want   map[string]any
	}{
		{
			name: "a statement family carries duration, rows and threshold",
			fields: Fields{
				Operation: "store.db.slow-query", Level: "warn",
				Statement: "replay", Duration: 900 * time.Millisecond, Rows: 4212, Threshold: 250 * time.Millisecond,
			},
			want: map[string]any{
				"statement": "replay", "duration_ms": float64(900),
				"rows": float64(4212), "threshold_ms": float64(250),
				"lock_wait_ms": float64(0),
			},
		},
		{
			name: "a queued statement reports the wait apart from the total",
			fields: Fields{
				Operation: "store.db.slow-query", Level: "warn",
				Statement: "write_batch", Duration: 3822 * time.Millisecond,
				LockWait: 3800 * time.Millisecond, Rows: 6, Threshold: 280 * time.Millisecond,
			},
			want: map[string]any{
				"statement": "write_batch", "duration_ms": float64(3822),
				"lock_wait_ms": float64(3800),
			},
		},
		{
			name: "a zero row count is reported as zero, never omitted",
			fields: Fields{
				Operation: "store.db.slow-query", Level: "warn",
				Statement: "cursor", Duration: 300 * time.Millisecond, Rows: 0, Threshold: 250 * time.Millisecond,
			},
			want: map[string]any{"statement": "cursor", "rows": float64(0)},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			var file, stderr bytes.Buffer
			log := New(&file, &stderr, false)

			// Act.
			log.Log(tc.fields, "slow")

			// Assert.
			var got record
			if err := json.Unmarshal(file.Bytes(), &got); err != nil {
				t.Fatalf("record is not JSON: %v\n%s", err, file.String())
			}
			for key, value := range tc.want {
				if got.Context[key] != value {
					t.Fatalf("context[%q] = %#v, want %#v", key, got.Context[key], value)
				}
			}
		})
	}
}

func TestNoStatementFamilyOmitsTheQueryTimingGroup(t *testing.T) {
	// Arrange. Every ordinary store record would otherwise carry three zeroed
	// query-timing fields it has no query for.
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)

	// Act.
	log.Log(Fields{Operation: "open", Table: "event"}, "ready")

	// Assert.
	var got record
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("record is not JSON: %v\n%s", err, file.String())
	}
	for _, key := range []string{"statement", "duration_ms", "lock_wait_ms", "rows", "threshold_ms"} {
		if _, present := got.Context[key]; present {
			t.Fatalf("context carries %q on a non-query record: %+v", key, got.Context)
		}
	}
}

// TestDurableOnlyWithholdsAnOrdinaryRecordFromTheTerminal covers the bound on
// the launchd stderr file: the record stream is durable-sink-only, so the file
// nobody rolls stops receiving a second copy of an already-rotated log.
func TestDurableOnlyWithholdsAnOrdinaryRecordFromTheTerminal(t *testing.T) {
	// Arrange.
	var file, stderr bytes.Buffer
	log := NewDurableOnly(&file, &stderr, false)

	// Act.
	log.Log(Fields{Operation: "write-batch"}, "accepted=%d", 2)

	// Assert: durably recorded, and not mirrored.
	if !strings.Contains(file.String(), `"operation":"write-batch"`) {
		t.Fatalf("the durable sink holds %q, want the record", file.String())
	}
	if stderr.Len() != 0 {
		t.Fatalf("an ordinary record reached the terminal under NewDurableOnly: %q", stderr.String())
	}
}

// TestDurableOnlyStillNarratesASinkFailureToTheTerminal covers the carve-out: a
// failure OF the durable sink can only be reported through the terminal, so
// withholding the ordinary stream must not close that channel.
func TestDurableOnlyStillNarratesASinkFailureToTheTerminal(t *testing.T) {
	// Arrange.
	var stderr bytes.Buffer

	// Act.
	capturePanic(t, func() {
		NewDurableOnly(failingWriter{errors.New("disk full")}, &stderr, false).
			Log(Fields{Operation: "write", Level: "error"}, "critical")
	})

	// Assert.
	var emergency record
	if err := json.Unmarshal(stderr.Bytes(), &emergency); err != nil {
		t.Fatalf("emergency stderr is not JSON: %v: %q", err, stderr.String())
	}
	if emergency.Operation != "store.logging.sink-failure" {
		t.Fatalf("emergency stderr record = %#v, want the sink-failure narration", emergency)
	}
}

// TestTheWindowIsEmittedOnlyWhenItWasMeasured pins that the over-budget window
// is absent rather than zero on a record that never measured one — the
// statement TRACE carries a statement family too, and "0 of 0" there would read
// as a family with a clean window.
func TestTheWindowIsEmittedOnlyWhenItWasMeasured(t *testing.T) {
	tests := []struct {
		name       string
		fields     Fields
		wantWindow bool
	}{
		{
			name:       "a measured window is reported with its count",
			fields:     Fields{Operation: "store.db.slow-query", Statement: "write_batch", OverBudget: 9, BudgetWindow: 16},
			wantWindow: true,
		},
		{
			name:       "a statement that measured no window omits both keys",
			fields:     Fields{Operation: "store.db.statement", Statement: "write_batch"},
			wantWindow: false,
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			var file, stderr bytes.Buffer
			log := New(&file, &stderr, false)

			// Act
			log.Log(test.fields, "a record")

			// Assert
			var got record
			if err := json.Unmarshal(file.Bytes(), &got); err != nil {
				t.Fatal(err)
			}
			_, gotRecent := got.Context["over_budget_recent"]
			_, gotWindow := got.Context["over_budget_window"]
			if gotRecent != test.wantWindow || gotWindow != test.wantWindow {
				t.Fatalf("over-budget keys present = (%v, %v), want %v: %s", gotRecent, gotWindow, test.wantWindow, file.String())
			}
		})
	}
}

func TestLogMarshalsAPinnedWALsState(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)

	log.Log(Fields{Operation: "store.db.wal-pin", WAL: &WALState{
		Frames: 47506, Backfilled: 0, ReadMarks: []uint32{0, 47481, 0xffffffff},
		PinnedFor: 90 * time.Second, ReadPoolOpen: 2, ReadPoolInUse: 0, ReadPoolIdle: 2,
	}}, "pinned")

	var got record
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("persistent record is not JSON: %v\n%s", err, file.String())
	}
	want := map[string]any{
		"wal_frames": float64(47506), "wal_backfilled": float64(0),
		"wal_read_marks":    []any{float64(0), float64(47481), float64(0xffffffff)},
		"wal_pinned_for_ms": float64(90000), "read_pool_open": float64(2),
		"read_pool_in_use": float64(0), "read_pool_idle": float64(2),
	}
	for key, value := range want {
		if !reflect.DeepEqual(got.Context[key], value) {
			t.Fatalf("context[%s] = %#v, want %#v; record %s", key, got.Context[key], value, file.String())
		}
	}
}

func TestLogOmitsWALStateWhenNoneIsGiven(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false)

	log.Log(Fields{Operation: "store.db.wal-checkpoint"}, "checkpointed")

	var got record
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("persistent record is not JSON: %v\n%s", err, file.String())
	}
	if _, ok := got.Context["wal_frames"]; ok {
		t.Fatalf("a record without WAL state carries wal_frames: %s", file.String())
	}
}

func TestWithKeepsABoundWALStateWhenARecordAddsNone(t *testing.T) {
	var file, stderr bytes.Buffer
	log := New(&file, &stderr, false).With(Fields{WAL: &WALState{Frames: 7}})

	log.Log(Fields{Operation: "store.db.wal-pin"}, "pinned")

	var got record
	if err := json.Unmarshal(file.Bytes(), &got); err != nil {
		t.Fatalf("persistent record is not JSON: %v\n%s", err, file.String())
	}
	if got.Context["wal_frames"] != float64(7) {
		t.Fatalf("wal_frames = %#v, want the bound 7", got.Context["wal_frames"])
	}
}
