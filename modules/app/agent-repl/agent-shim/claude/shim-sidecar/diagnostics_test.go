package main

import (
	"errors"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

func TestDiagnosticOutboxRetainsExactEventUntilAcknowledged(t *testing.T) {
	var out diagnosticOutbox
	d := logging.Diagnostic{
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
	out.enqueue(d)
	first := out.snapshot()
	if len(first) != 1 {
		t.Fatalf("queued diagnostics = %d, want 1", len(first))
	}
	// A failed StoreWrite does not acknowledge anything. Retrying reuses the
	// exact event and dedup key rather than manufacturing a second diagnostic.
	second := out.snapshot()
	if first[0] != second[0] || first[0].GetDedupKey() != second[0].GetDedupKey() {
		t.Fatalf("retry did not retain exact queued event: first=%p second=%p", first[0], second[0])
	}
	out.acknowledge(len(second))
	if got := len(out.snapshot()); got != 0 {
		t.Fatalf("acknowledged diagnostics remain queued: %d", got)
	}
}

func TestDiagnosticEventHasCanonicalFilePlaneContract(t *testing.T) {
	e := diagnosticEvent(logging.Diagnostic{
		Timestamp: time.UnixMilli(99), PID: 7, Level: "warn", Verbosity: "verbose",
		Operation: "sidecar.handler.convert", Message: "unknown field", Session: "s1",
		RequestID: "r1", Path: "/tmp/s1.jsonl", Context: map[string]any{"component": "handler"},
	}, 1)
	if e.GetPlane() != corev1.Plane_PLANE_FILE || e.GetClass() != corev1.EventClass_EVENT_CLASS_PERSISTENT {
		t.Fatalf("event routing = plane %v class %v", e.GetPlane(), e.GetClass())
	}
	if e.GetSessionId() != "s1" || e.GetRequestId() != "r1" || e.GetProducedAtMs() != 99 || e.GetDedupKey() == "" {
		t.Fatalf("event attribution = %#v", e)
	}
	p := e.GetFilePlaneDiagnostic()
	if p == nil || p.GetSourceRuntime() != corev1.DiagnosticSourceRuntime_DIAGNOSTIC_SOURCE_RUNTIME_SIDECAR || p.GetSourcePid() != 7 || p.GetSourcePath() != "/tmp/s1.jsonl" {
		t.Fatalf("file-plane diagnostic = %#v", p)
	}
	if p.GetLevel() != "warn" || p.GetVerbosity() != "verbose" || p.GetOperation() != "sidecar.handler.convert" || p.GetContext().GetFields()["component"].GetStringValue() != "handler" {
		t.Fatalf("diagnostic payload = %#v", p)
	}
}

func TestDiagnosticOutboxConcurrentEnqueueHasUniqueStableKeys(t *testing.T) {
	var out diagnosticOutbox
	const workers = 32
	var group sync.WaitGroup
	group.Add(workers)
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
	events := out.snapshot()
	if len(events) != workers {
		t.Fatalf("queued diagnostics = %d, want %d", len(events), workers)
	}
	keys := map[string]bool{}
	for _, event := range events {
		if keys[event.GetDedupKey()] {
			t.Fatalf("duplicate dedup key %q", event.GetDedupKey())
		}
		keys[event.GetDedupKey()] = true
	}
	// Snapshot is a copy: callers cannot rewrite the queue by replacing slots.
	events[0] = nil
	if out.snapshot()[0] == nil {
		t.Fatal("snapshot aliases the outbox")
	}
}

func TestDiagnosticOutboxFailedFlushNeverGrowsQueue(t *testing.T) {
	var out diagnosticOutbox
	out.enqueue(logging.Diagnostic{
		Timestamp: time.UnixMilli(1), PID: 1, Level: "error", Verbosity: "normal",
		Operation: "sidecar.tail.poll", Message: "failed", Session: "s1",
	})
	first := out.snapshot()[0]
	for attempt := 0; attempt < 4; attempt++ {
		event, err := out.flush(func(*corev1.Event) error { return errors.New("store unavailable") })
		if err == nil || event != first {
			t.Fatalf("attempt %d result event=%p err=%v", attempt, event, err)
		}
		queued := out.snapshot()
		if len(queued) != 1 || queued[0] != first {
			t.Fatalf("attempt %d queue changed after failed flush: %#v", attempt, queued)
		}
	}
}
