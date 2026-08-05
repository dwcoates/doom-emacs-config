package sessioncontroller

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"

	"google.golang.org/protobuf/proto"
)

const (
	responseDiagnosticDedupeCapacity = 4096
	responseDiagnosticRepeatLimit    = 3
)

// diagnosticDeduper bounds repeated diagnostics while retaining evidence of a
// repeated delivery.  It is deliberately process-local: diagnostics are an
// observation aid, not an authority over stream processing.
type diagnosticDeduper struct {
	capacity    int
	repeatLimit int
	entries     map[string]*diagnosticDedupeEntry
	order       []string
}

type diagnosticDedupeEntry struct {
	repeats        int
	summaryEmitted bool
}

type diagnosticDedupeObservation struct {
	Fingerprint string
	RepeatCount int
	Emit        bool
	First       bool
	Summary     bool
}

func newDiagnosticDeduper(capacity, repeatLimit int) *diagnosticDeduper {
	if capacity <= 0 {
		panic(fmt.Sprintf("session-controller: diagnostic dedupe capacity must be positive, got %d", capacity))
	}
	if repeatLimit <= 0 {
		panic(fmt.Sprintf("session-controller: diagnostic dedupe repeat limit must be positive, got %d", repeatLimit))
	}
	return &diagnosticDeduper{capacity: capacity, repeatLimit: repeatLimit, entries: map[string]*diagnosticDedupeEntry{}}
}

// observe records APIMessageID plus the deterministic fingerprint of PAYLOAD.
// It emits the first observation and exactly one summary when the bounded
// repeat count is reached; subsequent identical observations stay silent.
func (d *diagnosticDeduper) observe(apiMessageID string, payload proto.Message) (diagnosticDedupeObservation, error) {
	if apiMessageID == "" {
		return diagnosticDedupeObservation{}, fmt.Errorf("session-controller: diagnostic dedupe requires api message id")
	}
	if payload == nil {
		return diagnosticDedupeObservation{}, fmt.Errorf("session-controller: diagnostic dedupe requires payload for api message id %q", apiMessageID)
	}
	encoded, err := proto.MarshalOptions{Deterministic: true}.Marshal(payload)
	if err != nil {
		return diagnosticDedupeObservation{}, fmt.Errorf("session-controller: fingerprint diagnostic payload for api message id %q: %w", apiMessageID, err)
	}
	digest := sha256.Sum256(encoded)
	fingerprint := hex.EncodeToString(digest[:])
	key := apiMessageID + "\x00" + fingerprint
	if entry := d.entries[key]; entry != nil {
		if entry.repeats < d.repeatLimit {
			entry.repeats++
		}
		summary := entry.repeats == d.repeatLimit && !entry.summaryEmitted
		if summary {
			entry.summaryEmitted = true
		}
		return diagnosticDedupeObservation{Fingerprint: fingerprint, RepeatCount: entry.repeats, Emit: summary, Summary: summary}, nil
	}
	if len(d.order) == d.capacity {
		delete(d.entries, d.order[0])
		d.order = d.order[1:]
	}
	d.entries[key] = &diagnosticDedupeEntry{}
	d.order = append(d.order, key)
	return diagnosticDedupeObservation{Fingerprint: fingerprint, Emit: true, First: true}, nil
}
