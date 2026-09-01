package main

import (
	"encoding/json"
	"strings"
	"testing"
)

// The unit harness reads its own log the way the integration suite reads the
// sidecar's durable one: as TYPED RECORDS, never as a blob of text.
//
// A subject that greps `h.logText()` for a sentence asserts the PROSE of a
// branch rather than the branch. The prose is not the contract — the operation,
// the level and the dedicated correlation keys are (AGENTS.md "Logging") — and a
// substring subject fails on a reworded message while passing on a record that
// dropped every key a reader joins on. Everything below addresses records by
// those fields instead.

// logRecord mirrors internal/logging's canonical JSON record.
type logRecord struct {
	Timestamp string         `json:"timestamp"`
	Runtime   string         `json:"runtime"`
	PID       int            `json:"pid"`
	Level     string         `json:"level"`
	Verbosity string         `json:"verbosity"`
	Operation string         `json:"operation"`
	Message   string         `json:"message"`
	RequestID string         `json:"request_id"`
	Context   map[string]any `json:"context"`
}

// parseLogLines parses captured log lines STRICTLY: the contract is JSONL, so a
// line that is not a JSON object is a defect rather than a line to skip.
func parseLogLines(t *testing.T, lines []string) []logRecord {
	t.Helper()
	var out []logRecord
	for i, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var rec logRecord
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("sidecar log line %d is not JSON: %v\n%s", i+1, err, line)
		}
		out = append(out, rec)
	}
	return out
}

// opsAt keeps the records one operation wrote at one level. An empty level
// keeps every level.
func opsAt(records []logRecord, operation, level string) []logRecord {
	var out []logRecord
	for _, r := range records {
		if r.Operation != operation {
			continue
		}
		if level != "" && r.Level != level {
			continue
		}
		out = append(out, r)
	}
	return out
}

// requireOnceIn states that a branch was reached EXACTLY once and answers its
// record. Exactly-once is part of the contract ("every error is logged EXACTLY
// ONCE by its owning layer"), so a helper that accepted "at least one" would let
// a double-logged failure pass.
func requireOnceIn(t *testing.T, records []logRecord, operation, level string) logRecord {
	t.Helper()
	got := opsAt(records, operation, level)
	if len(got) != 1 {
		t.Fatalf("operation %q at level %q was recorded %d times, want exactly once; the log held %v",
			operation, level, len(got), operationLevels(records))
	}
	return got[0]
}

// requireNoneIn states that a branch was never reached.
func requireNoneIn(t *testing.T, records []logRecord, operation, level string) {
	t.Helper()
	if got := opsAt(records, operation, level); len(got) != 0 {
		t.Fatalf("operation %q at level %q was recorded %d times, want none; the log held %v",
			operation, level, len(got), operationLevels(records))
	}
}

// operationLevels renders what the log actually holds, so a failure names
// branches rather than dumping prose.
func operationLevels(records []logRecord) []string {
	var out []string
	for _, r := range records {
		out = append(out, r.Operation+"/"+r.Level)
	}
	return out
}

// ctxString reads one correlation key as a string, failing when the record does
// not carry it. A missing key is the defect the subject exists to catch, so it
// is never defaulted away.
func ctxString(t *testing.T, r logRecord, key string) string {
	t.Helper()
	raw, ok := r.Context[key]
	if !ok {
		t.Fatalf("record %q carries no %q; its context was %v", r.Operation, key, r.Context)
	}
	value, ok := raw.(string)
	if !ok {
		t.Fatalf("record %q carries %q as %T, want a string", r.Operation, key, raw)
	}
	return value
}

// ---- harness conveniences -------------------------------------------------

func (h *harness) records(t *testing.T) []logRecord {
	t.Helper()
	return parseLogLines(t, *h.logs)
}

func (h *harness) ops(t *testing.T, operation string) []logRecord {
	t.Helper()
	return opsAt(h.records(t), operation, "")
}

func (h *harness) opsAt(t *testing.T, operation, level string) []logRecord {
	t.Helper()
	return opsAt(h.records(t), operation, level)
}

func (h *harness) requireOnce(t *testing.T, operation, level string) logRecord {
	t.Helper()
	return requireOnceIn(t, h.records(t), operation, level)
}

func (h *harness) requireNone(t *testing.T, operation, level string) {
	t.Helper()
	requireNoneIn(t, h.records(t), operation, level)
}
