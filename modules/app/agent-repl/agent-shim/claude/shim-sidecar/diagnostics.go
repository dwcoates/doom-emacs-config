package main

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"google.golang.org/protobuf/types/known/structpb"
)

// diagnosticOutbox owns diagnostics until the store has acknowledged the batch
// carrying them. It deliberately knows nothing about transport: logger calls
// append here, and the sidecar event loop includes a snapshot in a StoreWrite.
// This makes recursive store I/O structurally impossible.
type diagnosticOutbox struct {
	mu     sync.Mutex
	next   uint64
	events []*corev1.Event
}

func (o *diagnosticOutbox) enqueue(d logging.Diagnostic) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.next++
	o.events = append(o.events, diagnosticEvent(d, o.next))
}

func (o *diagnosticOutbox) snapshot() []*corev1.Event {
	o.mu.Lock()
	defer o.mu.Unlock()
	return append([]*corev1.Event(nil), o.events...)
}

func (o *diagnosticOutbox) acknowledge(n int) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if n < 0 || n > len(o.events) {
		panic(fmt.Sprintf("sidecar: diagnostic acknowledge %d for %d queued records", n, len(o.events)))
	}
	o.events = o.events[n:]
}

// flush writes queued events in order. A failed write leaves that exact event
// at the queue head, so a later retry reuses its producer identity and dedup
// key rather than creating a replacement.
func (o *diagnosticOutbox) flush(write func(*corev1.Event) error) (*corev1.Event, error) {
	for {
		events := o.snapshot()
		if len(events) == 0 {
			return nil, nil
		}
		if err := write(events[0]); err != nil {
			return events[0], err
		}
		o.acknowledge(1)
	}
}

func diagnosticEvent(d logging.Diagnostic, ordinal uint64) *corev1.Event {
	context, err := structpb.NewStruct(d.Context)
	if err != nil {
		panic(fmt.Sprintf("sidecar: diagnostic context is not protobuf-compatible: %v", err))
	}
	if ordinal == 0 {
		panic("sidecar: diagnostic ordinal must be positive")
	}
	keySource := fmt.Sprintf("%s\x00%d\x00%d\x00%d", d.Session, d.PID, ordinal, d.Timestamp.UnixMilli())
	digest := sha256.Sum256([]byte(keySource))
	return &corev1.Event{
		SessionId:    d.Session,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId:    d.RequestID,
		ProducedAtMs: d.Timestamp.UnixMilli(),
		DedupKey:     "sidecar-diagnostic:" + hex.EncodeToString(digest[:]),
		Payload: &corev1.Event_FilePlaneDiagnostic{FilePlaneDiagnostic: &corev1.FilePlaneDiagnostic{
			SourceRuntime: corev1.DiagnosticSourceRuntime_DIAGNOSTIC_SOURCE_RUNTIME_SIDECAR,
			Level:         d.Level,
			Verbosity:     d.Verbosity,
			Operation:     d.Operation,
			Message:       d.Message,
			Context:       context,
			SourcePid:     int64(d.PID),
			SourcePath:    d.Path,
		}},
	}
}
