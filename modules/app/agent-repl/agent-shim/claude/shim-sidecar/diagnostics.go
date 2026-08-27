package main

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"sort"
	"strings"
	"sync"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
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
	events []*storev1.StoreEntry
}

func (o *diagnosticOutbox) enqueue(d logging.Diagnostic) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.next++
	o.events = append(o.events, diagnosticEvent(d, o.next))
}

func (o *diagnosticOutbox) snapshot() []*storev1.StoreEntry {
	o.mu.Lock()
	defer o.mu.Unlock()
	return append([]*storev1.StoreEntry(nil), o.events...)
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
func (o *diagnosticOutbox) flush(write func(*storev1.StoreEntry) error) (*storev1.StoreEntry, error) {
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

// diagnosticEvent turns one logged diagnostic into the record that carries it
// through the store to a session's own log.
//
// ITS HOME IS ProducerDiagnostic, WHICH IS NARROWER THAN WHAT IT REPLACES. The
// retired FilePlaneDiagnostic carried the level, the verbosity, the emitting
// runtime, the pid, the source path, a request id and a structured context
// object as FIELDS — every one of them separately readable. ProducerDiagnostic
// carries an operation and a free-text detail, so everything else is flattened
// into that detail rather than dropped. It is PRESERVED, not STRUCTURED, and a
// consumer that used to filter on level now has to parse prose. See the gap
// note; nothing here invents a field to keep the old shape.
func diagnosticEvent(d logging.Diagnostic, ordinal uint64) *storev1.StoreEntry {
	// An unencodable context is still rejected loudly, exactly as before: it is
	// a bug in the caller, not a record to quietly truncate. The check is kept
	// even though the Struct itself no longer has a field to sit in, because
	// dropping it would silently accept diagnostics this system used to refuse.
	if _, err := structpb.NewStruct(d.Context); err != nil {
		panic(fmt.Sprintf("sidecar: diagnostic context is not protobuf-compatible: %v", err))
	}
	if ordinal == 0 {
		panic("sidecar: diagnostic ordinal must be positive")
	}
	// The write identity is the digest the retired dedup key used, unchanged, so
	// a diagnostic replayed after a lost connection is still one record.
	keySource := fmt.Sprintf("%s\x00%d\x00%d\x00%d", d.Session, d.PID, ordinal, d.Timestamp.UnixMilli())
	digest := sha256.Sum256([]byte(keySource))
	return convert.ProducerDiagnostic(
		convert.Attribution{
			SessionID:    d.Session,
			Path:         d.Path,
			ProducedAtMs: d.Timestamp.UnixMilli(),
		},
		"sidecar-diagnostic:"+hex.EncodeToString(digest[:]),
		d.Operation,
		diagnosticDetail(d),
	)
}

// diagnosticDetail flattens everything ProducerDiagnostic has no field for into
// the one field it has, so nothing the logger recorded is lost on the way to the
// store.
//
// The rendering is ORDER-STABLE, which is not cosmetic: the write identity above
// makes a replayed diagnostic idempotent, and that only holds if the same
// diagnostic renders to the same bytes every time it is produced.
func diagnosticDetail(d logging.Diagnostic) string {
	var b strings.Builder
	b.WriteString(d.Message)
	fmt.Fprintf(&b, " [runtime=sidecar level=%s verbosity=%s pid=%d", d.Level, d.Verbosity, d.PID)
	if d.Path != "" {
		fmt.Fprintf(&b, " path=%s", d.Path)
	}
	if d.RequestID != "" {
		fmt.Fprintf(&b, " request_id=%s", d.RequestID)
	}
	keys := make([]string, 0, len(d.Context))
	for key := range d.Context {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	for _, key := range keys {
		fmt.Fprintf(&b, " %s=%v", key, d.Context[key])
	}
	b.WriteString("]")
	return b.String()
}
