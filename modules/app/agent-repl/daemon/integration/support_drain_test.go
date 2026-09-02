//go:build integration

package integration

import (
	"testing"

	"claude-repld/integration/harness"
)

// support_drain_test.go holds the drain/rollout suite's own small helpers,
// kept separate from drain_rollout_test.go's drain*-prefixed test-fixture
// helpers so the rate-limit assertions' log-filtering stays in one place.

// drainRunLogRecordsAt answers every run-log record under an operation at an
// exact level ("warn" or "debug"), in file order. It reads the run log
// directly rather than waiting, because the rate-limit assertion needs the
// FULL set of records a burst of refusals produced, not just the first one
// matching a predicate.
func drainRunLogRecordsAt(t *testing.T, d *harness.Daemon, operation, level string) []harness.LogRecord {
	t.Helper()
	var out []harness.LogRecord
	for _, r := range d.RunLog() {
		if r.Operation == operation && r.Level == level {
			out = append(out, r)
		}
	}
	return out
}

// drainRefusalCounts reads the suppressed/total pair off a rate-limited drain
// refusal record's context, failing loudly if either is missing or not a
// number (the JSON decoder answers every number as float64).
func drainRefusalCounts(t *testing.T, r harness.LogRecord) (suppressed, total int) {
	t.Helper()
	s, ok := r.Context["suppressed"].(float64)
	if !ok {
		t.Fatalf("record %q context = %v, want a numeric \"suppressed\"", r.Operation, r.Context)
	}
	tot, ok := r.Context["total"].(float64)
	if !ok {
		t.Fatalf("record %q context = %v, want a numeric \"total\"", r.Operation, r.Context)
	}
	return int(s), int(tot)
}
