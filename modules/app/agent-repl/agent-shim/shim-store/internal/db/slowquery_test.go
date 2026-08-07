package db

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"path/filepath"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/logging"
)

// slowQueryRecord is the subset of the canonical store record a slow-query
// assertion reads.
type slowQueryRecord struct {
	Operation string `json:"operation"`
	Level     string `json:"level"`
	Verbosity string `json:"verbosity"`
	Session   string `json:"claude_session_id"`
	Context   struct {
		Statement   string  `json:"statement"`
		DurationMS  float64 `json:"duration_ms"`
		Rows        float64 `json:"rows"`
		ThresholdMS float64 `json:"threshold_ms"`
		Table       string  `json:"table"`
	} `json:"context"`
}

// openThreshold opens a fresh WAL database with an explicit slow-query
// threshold and returns it alongside the buffer its records land in.
//
// A one-nanosecond threshold is deterministically crossed by any real
// statement and an hour is deterministically not, so both sides of the
// comparison are exercised without a test ever waiting on a clock.
func openThreshold(t *testing.T, threshold time.Duration) (*DB, *bytes.Buffer) {
	t.Helper()
	sink := &bytes.Buffer{}
	path := filepath.Join(t.TempDir(), "events.db")
	d, err := OpenWithOptions(path, logging.New(sink, io.Discard, false), Options{SlowQuery: threshold})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() })
	sink.Reset()
	return d, sink
}

// slowQueryRecords decodes every slow-query record in the sink, ignoring the
// store's other lifecycle output.
func slowQueryRecords(t *testing.T, sink *bytes.Buffer) []slowQueryRecord {
	t.Helper()
	var records []slowQueryRecord
	for _, line := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		if line == "" {
			continue
		}
		var record slowQueryRecord
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("decode %q: %v", line, err)
		}
		if record.Operation == SlowQueryOperation {
			records = append(records, record)
		}
	}
	return records
}

func TestSlowQueryReportingFollowsTheThreshold(t *testing.T) {
	tests := []struct {
		name      string
		threshold time.Duration
		wantSlow  bool
	}{
		{
			name:      "a query under the threshold says nothing",
			threshold: time.Hour,
			wantSlow:  false,
		},
		{
			name:      "a query over the threshold is reported",
			threshold: time.Nanosecond,
			wantSlow:  true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			d, sink := openThreshold(t, tc.threshold)

			// Act.
			if _, err := d.MaxSeq("session-1"); err != nil {
				t.Fatalf("MaxSeq: %v", err)
			}

			// Assert.
			records := slowQueryRecords(t, sink)
			if got := len(records) > 0; got != tc.wantSlow {
				t.Fatalf("slow-query records = %d with a %s threshold, want reported=%v",
					len(records), tc.threshold, tc.wantSlow)
			}
		})
	}
}

func TestSlowQueryReportingIsDisabledByANonPositiveThreshold(t *testing.T) {
	// Arrange. Only an explicit Options caller can ask for this; the
	// environment path refuses a non-positive value.
	d, sink := openThreshold(t, 0)

	// Act.
	if _, err := d.MaxSeq("session-1"); err != nil {
		t.Fatalf("MaxSeq: %v", err)
	}

	// Assert.
	if records := slowQueryRecords(t, sink); len(records) != 0 {
		t.Fatalf("slow-query records = %d, want the reporting off", len(records))
	}
}

func TestSlowQueryRecordIsNormalVerbosityWarn(t *testing.T) {
	// Arrange. The whole point is that an operator sees it without having
	// enabled verbose mode in advance.
	d, sink := openThreshold(t, time.Nanosecond)

	// Act.
	if _, err := d.MaxSeq("session-1"); err != nil {
		t.Fatalf("MaxSeq: %v", err)
	}

	// Assert.
	record := slowQueryRecords(t, sink)[0]
	if record.Level != "warn" || record.Verbosity != "normal" {
		t.Fatalf("record level/verbosity = %s/%s, want warn/normal", record.Level, record.Verbosity)
	}
}

func TestSlowQueryRecordCarriesTheStatementFamilyAndCost(t *testing.T) {
	// Arrange.
	d, sink := openThreshold(t, time.Nanosecond)
	if _, err := d.Ingest("producer", []*corev1.Event{persistentCore("session-1"), persistentCore("session-1")}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	sink.Reset()

	// Act.
	if _, err := d.ReplayFrom(context.Background(), "session-1", 0, func(*corev1.Event) error { return nil }); err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}

	// Assert.
	record := slowQueryRecords(t, sink)[0]
	if record.Context.Statement != StatementReplay {
		t.Fatalf("statement = %q, want %q", record.Context.Statement, StatementReplay)
	}
	if record.Context.Rows != 2 {
		t.Fatalf("rows = %v, want the 2 events the replay yielded", record.Context.Rows)
	}
	if record.Context.ThresholdMS != float64(time.Nanosecond.Milliseconds()) {
		t.Fatalf("threshold_ms = %v, want the configured threshold", record.Context.ThresholdMS)
	}
	if record.Session != "session-1" {
		t.Fatalf("claude_session_id = %q, want the replayed session", record.Session)
	}
}

func TestSlowQueryRecordNeverCarriesRenderedSQL(t *testing.T) {
	// Arrange. The store's payloads are opaque to it; a record quoting a
	// parameterized statement would leak session content into the global log.
	d, sink := openThreshold(t, time.Nanosecond)

	// Act.
	if _, err := d.EventsByTask("session-1", "task-1"); err != nil {
		t.Fatalf("EventsByTask: %v", err)
	}

	// Assert.
	raw := sink.String()
	if strings.Contains(raw, "SELECT") || strings.Contains(raw, "task-1") {
		t.Fatalf("slow-query output = %q, want a statement FAMILY with no SQL and no bound values", raw)
	}
}

func TestSlowQueryReportsAZeroRowResultAsZero(t *testing.T) {
	// Arrange. A row count omitted as "unset" would make an empty answer
	// indistinguishable from a missing field.
	d, sink := openThreshold(t, time.Nanosecond)

	// Act. No cursor exists, so the read resolves nothing.
	if _, err := d.Cursor("file-absent"); err != nil {
		t.Fatalf("Cursor: %v", err)
	}

	// Assert.
	record := slowQueryRecords(t, sink)[0]
	if record.Context.Statement != StatementCursor || record.Context.Rows != 0 {
		t.Fatalf("record context = %+v, want the cursor family with rows=0", record.Context)
	}
}

func TestSlowQueryReportsTheIngestTransaction(t *testing.T) {
	// Arrange.
	d, sink := openThreshold(t, time.Nanosecond)

	// Act.
	if _, err := d.Ingest("producer", []*corev1.Event{persistentCore("session-1")}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Assert. The whole BEGIN IMMEDIATE transaction is the timed unit.
	var found bool
	for _, record := range slowQueryRecords(t, sink) {
		if record.Context.Statement == StatementIngest && record.Context.Rows == 1 {
			found = true
		}
	}
	if !found {
		t.Fatalf("slow-query records = %+v, want an ingest transaction with rows=1", slowQueryRecords(t, sink))
	}
}

func TestSlowQueryFromEnvReadsAnUnsetEnvironmentAsTheDefault(t *testing.T) {
	// Arrange.
	t.Setenv(EnvSlowQueryMs, "")

	// Act.
	got, err := SlowQueryFromEnv()

	// Assert.
	if err != nil || got != DefaultSlowQuery {
		t.Fatalf("SlowQueryFromEnv() = (%s, %v), want (%s, nil)", got, err, DefaultSlowQuery)
	}
}

func TestSlowQueryFromEnvReadsAnOverride(t *testing.T) {
	// Arrange.
	t.Setenv(EnvSlowQueryMs, "50")

	// Act.
	got, err := SlowQueryFromEnv()

	// Assert.
	if err != nil || got != 50*time.Millisecond {
		t.Fatalf("SlowQueryFromEnv() = (%s, %v), want (50ms, nil)", got, err)
	}
}

func TestSlowQueryFromEnvRefusesAMalformedValue(t *testing.T) {
	// Arrange.
	t.Setenv(EnvSlowQueryMs, "250ms")

	// Act.
	_, err := SlowQueryFromEnv()

	// Assert.
	if err == nil {
		t.Fatal("SlowQueryFromEnv() with a non-integer = nil error, want a loud refusal")
	}
}

func TestSlowQueryFromEnvRefusesZeroRatherThanDefaulting(t *testing.T) {
	// Arrange.
	t.Setenv(EnvSlowQueryMs, "0")

	// Act.
	_, err := SlowQueryFromEnv()

	// Assert. Defaulting would run a threshold the operator believes they
	// disabled.
	if err == nil {
		t.Fatal("SlowQueryFromEnv() with zero = nil error, want a loud refusal")
	}
}

func TestOpenRefusesAMalformedThresholdRatherThanOpening(t *testing.T) {
	// Arrange.
	t.Setenv(EnvSlowQueryMs, "nope")

	// Act.
	d, err := Open(filepath.Join(t.TempDir(), "events.db"), logging.New(io.Discard, io.Discard, false))

	// Assert.
	if err == nil {
		d.Close()
		t.Fatal("Open with a malformed threshold = nil error, want a loud refusal")
	}
}
