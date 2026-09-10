package integration

import (
	"context"
	"testing"
	"time"
)

// Shared vocabulary for the log subjects.
//
// A RECORD IS ADDRESSED BY ITS OPERATION, ITS LEVEL AND ITS DEDICATED KEYS —
// never by a substring of its message. The message is prose that may be reworded
// at any time; the operation, the level and the correlation keys are the
// contract (AGENTS.md "Logging", logging-contract.md), and they are the only
// thing a reader joining sidecar records against the store's can filter on.

// lostTerminalOperations names every outcome of the reader's attempt to spell a
// LOST conclusion as a terminal: the one that MINTED it, and the three that
// could not. They are separate operations precisely so a reader can tell a run
// that was settled from one left open, without reading a sentence.
var lostTerminalOperations = map[string]bool{
	"lost-terminal":             true,
	"lost-terminal-unwatched":   true,
	"lost-terminal-residue":     true,
	"lost-terminal-unsupported": true,
}

// recordsFor keeps every record one operation wrote.
func recordsFor(records []logRecord, operation string) []logRecord {
	var out []logRecord
	for _, r := range records {
		if r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
}

// recordsAt keeps every record one operation wrote at one level.
func recordsAt(records []logRecord, operation, level string) []logRecord {
	var out []logRecord
	for _, r := range recordsFor(records, operation) {
		if r.Level == level {
			out = append(out, r)
		}
	}
	return out
}

// operationLevels renders what a log holds, so a failure names branches instead
// of dumping prose.
func operationLevels(records []logRecord) []string {
	var out []string
	for _, r := range records {
		out = append(out, r.Operation+"/"+r.Level)
	}
	return out
}

// awaitOperationCount waits until an operation has been recorded at least n
// times at one level, then answers those records. It waits on the RECORDS, never
// on a duration: the log is the observable event, and the deadline is the
// suite's budget rather than a guess about how long the sidecar needs.
func awaitOperationCount(ctx context.Context, t *testing.T, logPath, operation, level string, n int) []logRecord {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if got := recordsAt(readLog(t, logPath), operation, level); len(got) >= n {
			return got
		}
		select {
		case <-ctx.Done():
			t.Fatalf("operation %q at level %q was recorded fewer than %d times within the deadline; the log held %v",
				operation, level, n, operationLevels(readLog(t, logPath)))
		case <-tick.C:
		}
	}
}

// requireContextKeys states that a record carries every correlation key its site
// class owes. A missing key is the defect — the record cannot be joined — so it
// is reported per key rather than as one opaque mismatch.
func requireContextKeys(t *testing.T, r logRecord, keys ...string) {
	t.Helper()
	for _, key := range keys {
		value, ok := r.Context[key]
		if !ok {
			t.Errorf("record %q (%s) carries no %q; its context was %v", r.Operation, r.Level, key, r.Context)
			continue
		}
		if text, isText := value.(string); isText && text == "" {
			t.Errorf("record %q carries %q as an empty string; presence, never sentinels", r.Operation, key)
		}
	}
}
