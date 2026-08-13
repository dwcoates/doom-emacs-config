package main

import (
	"errors"
	"io"
	"strings"
	"sync"
	"testing"
	"time"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
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
	if first[0].GetInternal().GetWriteId() != second[0].GetInternal().GetWriteId() {
		t.Fatal("a retry minted a new write identity, so the store would write the diagnostic twice")
	}
	out.acknowledge(len(second))
	if got := len(out.snapshot()); got != 0 {
		t.Fatalf("acknowledged diagnostics remain queued: %d", got)
	}
}

// A diagnostic is a fact about the READER rather than the read, so it is
// bookkeeping — nothing in the feed corresponds to it, and a consumer must never
// render it as conversation material.
func TestDiagnosticIsCarriedAsBookkeeping(t *testing.T) {
	// Arrange / Act.
	entry := diagnosticEvent(testDiagnostic(), 1)

	// Assert.
	external := entry.GetExternal()
	if external.GetMessage() != nil {
		t.Fatal("a diagnostic was carried as a conversation message")
	}
	if external.GetBookkeeping().GetProducerDiagnostic() == nil {
		t.Fatal("a diagnostic was not carried as a producer diagnostic")
	}
	if external.GetSessionId() != "claude-session" || external.GetProducedAtMs() != 1_700_000_000_123 {
		t.Fatalf("diagnostic attribution = %#v", external)
	}
	if entry.GetInternal().GetPlane().GetFile() == nil {
		t.Fatal("a diagnostic was not attributed to the file plane")
	}
}

// ProducerDiagnostic has only an operation and a free-text detail, where the
// retired FilePlaneDiagnostic had the level, verbosity, runtime, pid, path,
// request id and a structured context as FIELDS. Everything without a field is
// flattened into the detail rather than dropped — preserved, not structured.
func TestDiagnosticDetailPreservesEveryFieldThatLostItsHome(t *testing.T) {
	// Arrange / Act.
	diagnostic := diagnosticEvent(testDiagnostic(), 1).
		GetExternal().GetBookkeeping().GetProducerDiagnostic()

	// Assert.
	if diagnostic.GetOperation() != "sidecar.tail.poll" {
		t.Fatalf("operation = %q, want the logger's own operation", diagnostic.GetOperation())
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
		if !strings.Contains(diagnostic.GetDetail(), want) {
			t.Fatalf("detail %q lost %q", diagnostic.GetDetail(), want)
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
		id := entry.GetInternal().GetWriteId()
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
		entry, err := out.flush(func(*agentshimv1.Entry) error { return errors.New("store unavailable") })
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
		if _, err := out.flush(func(*agentshimv1.Entry) error { return errors.New("store unavailable") }); err == nil {
			t.Fatalf("flush attempt %d reported success against an unavailable store", attempt)
		}
	}
	retained := out.snapshot()[0]

	// Act.
	var delivered []*agentshimv1.Entry
	entry, err := out.flush(func(e *agentshimv1.Entry) error {
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

// A recovery-validation failure USED TO reach the session whose open task was
// malformed, as a diagnostic that session's log would show. It cannot any more:
// `OpenTaskState.started` carried the session id, so a failure now has nothing to
// attribute itself to and lands in the GLOBAL sidecar log instead.
//
// This pins that routing change rather than the delivery that is gone, because
// the difference matters to whoever is debugging: the failure is still loud, and
// it is no longer visible from inside the workspace it concerns.
func TestRecoveryValidationFailureNoLongerReachesASession(t *testing.T) {
	// Arrange.
	var out diagnosticOutbox
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(out.enqueue)
	tracker := stale.New(stale.Options{}, log)
	// The only invalidity OpenTaskState can still express: it does not say when
	// the task was last active.
	invalid := []*agentshimv1.OpenTaskState{{LastActivityAtMs: 0}}

	// Act.
	for attempt := 0; attempt < 3; attempt++ {
		if err := tracker.Restore(invalid); err == nil {
			t.Fatalf("Restore attempt %d accepted an invalid snapshot", attempt)
		}
	}

	// Assert.
	if queued := out.snapshot(); len(queued) != 0 {
		t.Fatalf("recovery failure queued %d session diagnostics; it names no session to route to", len(queued))
	}
}
