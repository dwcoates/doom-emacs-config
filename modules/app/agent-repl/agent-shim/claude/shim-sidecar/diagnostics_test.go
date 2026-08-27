package main

import (
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

func testDiagnostic() logging.Diagnostic {
	return logging.Diagnostic{
		Timestamp: time.UnixMilli(1_700_000_000_123),
		PID:       42,
		Level:     "error",
		Verbosity: "normal",
		Operation: "sidecar.tail.poll",
		Message:   "read failed",
		Session:   "claude-session",
		RequestID: "request-1",
		Path:      "/tmp/transcript.jsonl",
		Context:   map[string]any{"component": "tail", "cursor": "8"},
	}
}

// A write that failed acknowledges nothing, so a retry must reuse the EXACT
// record — and therefore its write identity — rather than manufacturing a second
// diagnostic that would land as a duplicate.
func TestDiagnosticOutboxRetainsExactRecordUntilAcknowledged(t *testing.T) {
	// Arrange.
	var out diagnosticOutbox
	out.enqueue(testDiagnostic())

	// Act.
	first := out.snapshot()
	second := out.snapshot()

	// Assert.
	if len(first) != 1 {
		t.Fatalf("queued diagnostics = %d, want 1", len(first))
	}
	if first[0] != second[0] {
		t.Fatalf("retry did not retain the exact queued record: first=%p second=%p", first[0], second[0])
	}
	if first[0].GetWriteId() != second[0].GetWriteId() {
		t.Fatal("a retry minted a new write identity, so the store would write the diagnostic twice")
	}
	out.acknowledge(len(second))
	if got := len(out.snapshot()); got != 0 {
		t.Fatalf("acknowledged diagnostics remain queued: %d", got)
	}
}

// A diagnostic has NO TYPED HOME LEFT AT ALL: protocol.v1 BookkeepingEntry and
// its ProducerDiagnostic arm were deleted, and store.v1 StoreEntry has no
// bookkeeping arm, so the record travels whole as an unported entry.
//
// EVERYTHING THE LOGGER RECORDED IS STILL THERE, which is what this covers. The
// operation and the flattened detail — level, verbosity, runtime, pid, path,
// request id and the structured context — are preserved, not structured.
func TestDiagnosticDetailPreservesEveryFieldThatLostItsHome(t *testing.T) {
	// Arrange / Act.
	raw := diagnosticEvent(testDiagnostic(), 1).
		GetAgentUpdate().GetUnservedItem().GetUnknown().GetRaw()
	operation := raw.GetFields()["operation"].GetStringValue()
	detail := raw.GetFields()["detail"].GetStringValue()

	// Assert.
	if operation != "sidecar.tail.poll" {
		t.Fatalf("operation = %q, want the logger's own operation", operation)
	}
	for _, want := range []string{
		"read failed",
		"level=error",
		"verbosity=normal",
		"pid=42",
		"path=/tmp/transcript.jsonl",
		"request_id=request-1",
		"component=tail",
		"cursor=8",
	} {
		if !strings.Contains(detail, want) {
			t.Fatalf("detail %q lost %q", detail, want)
		}
	}
}

// The write identity is what makes a replayed diagnostic idempotent, and that
// only holds if one diagnostic renders to the same bytes every time.
func TestDiagnosticDetailRenderingIsOrderStable(t *testing.T) {
	// Arrange.
	d := testDiagnostic()
	d.Context = map[string]any{"z": 1, "a": 2, "m": 3}

	// Act.
	first := diagnosticDetail(d)
	second := diagnosticDetail(d)

	// Assert.
	if first != second {
		t.Fatalf("detail rendering is unstable:\n%q\n%q", first, second)
	}
}

// A context the schema cannot represent is a bug in the caller, not a record to
// quietly truncate. The check outlived the field it used to guard, because
// dropping it would silently accept diagnostics this system used to refuse.
func TestUnrepresentableDiagnosticContextIsStillRefused(t *testing.T) {
	// Arrange.
	d := testDiagnostic()
	d.Context = map[string]any{"bad": make(chan int)}

	// Act / Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("an unrepresentable diagnostic context was accepted silently")
		}
	}()
	diagnosticEvent(d, 1)
}

func TestDiagnosticOutboxConcurrentEnqueueHasUniqueStableIdentities(t *testing.T) {
	// Arrange.
	var out diagnosticOutbox
	const workers = 32
	var group sync.WaitGroup
	group.Add(workers)

	// Act.
	for range workers {
		go func() {
			defer group.Done()
			out.enqueue(logging.Diagnostic{
				Timestamp: time.UnixMilli(100), PID: 9, Level: "info", Verbosity: "normal",
				Operation: "sidecar.tail.poll", Message: "same millisecond", Session: "s1",
			})
		}()
	}
	group.Wait()

	// Assert.
	entries := out.snapshot()
	if len(entries) != workers {
		t.Fatalf("queued diagnostics = %d, want %d", len(entries), workers)
	}
	seen := map[string]bool{}
	for _, entry := range entries {
		id := entry.GetWriteId()
		if seen[id] {
			t.Fatalf("duplicate write identity %q; one diagnostic would silently replace another", id)
		}
		seen[id] = true
	}
	// Snapshot is a copy: callers cannot rewrite the queue by replacing slots.
	entries[0] = nil
	if out.snapshot()[0] == nil {
		t.Fatal("snapshot aliases the outbox")
	}
}

func TestDiagnosticOutboxFailedFlushNeverGrowsQueue(t *testing.T) {
	// Arrange.
	var out diagnosticOutbox
	out.enqueue(logging.Diagnostic{
		Timestamp: time.UnixMilli(1), PID: 1, Level: "error", Verbosity: "normal",
		Operation: "sidecar.tail.poll", Message: "failed", Session: "s1",
	})
	first := out.snapshot()[0]

	// Act / Assert.
	for attempt := 0; attempt < 4; attempt++ {
		entry, err := out.flush(func(*storev1.StoreEntry) error { return errors.New("store unavailable") })
		if err == nil || entry != first {
			t.Fatalf("attempt %d result entry=%p err=%v", attempt, entry, err)
		}
		queued := out.snapshot()
		if len(queued) != 1 || queued[0] != first {
			t.Fatalf("attempt %d queue changed after a failed flush: %#v", attempt, queued)
		}
	}
}

// A record the outbox retained across failures is the one that flushes when the
// link returns — the same object, so the replay reuses its write identity rather
// than minting a second copy of one diagnostic.
func TestRetainedDiagnosticFlushesOnceTheLinkReturns(t *testing.T) {
	// Arrange — a diagnostic that failed to write three times.
	var out diagnosticOutbox
	out.enqueue(testDiagnostic())
	for attempt := 0; attempt < 3; attempt++ {
		if _, err := out.flush(func(*storev1.StoreEntry) error { return errors.New("store unavailable") }); err == nil {
			t.Fatalf("flush attempt %d reported success against an unavailable store", attempt)
		}
	}
	retained := out.snapshot()[0]

	// Act.
	var delivered []*storev1.StoreEntry
	entry, err := out.flush(func(e *storev1.StoreEntry) error {
		delivered = append(delivered, e)
		return nil
	})

	// Assert.
	if err != nil || entry != nil {
		t.Fatalf("flush result entry=%p err=%v, want complete success", entry, err)
	}
	if len(delivered) != 1 || delivered[0] != retained {
		t.Fatalf("delivered diagnostics = %#v, want the exact retained record %p", delivered, retained)
	}
	if got := len(out.snapshot()); got != 0 {
		t.Fatalf("successful flush retained %d diagnostics", got)
	}
}
