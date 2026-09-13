package handler

// handler_test.go — the attribution and write-record duties every handler shares.

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"strings"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// captured is one decoded log record, read back the way the integration loop
// reads the sidecar's log.
type captured struct {
	Operation string         `json:"operation"`
	Message   string         `json:"message"`
	Context   map[string]any `json:"context"`
}

// captureLog builds a logger whose records a test can read back. Only the FILE
// sink is captured: the logger writes every record to both sinks, and reading
// one buffer that received it twice would double every assertion's count.
func captureLog(t *testing.T, sink *bytes.Buffer) *logging.Bound {
	t.Helper()
	return logging.New(io.Discard, sink).With(logging.Context{Component: "test"})
}

func decodeCaptured(t *testing.T, sink *bytes.Buffer) []captured {
	t.Helper()
	var out []captured
	for _, line := range strings.Split(sink.String(), "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var rec captured
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("log line %q is not JSON: %v", line, err)
		}
		out = append(out, rec)
	}
	return out
}

func TestAResidueWriteRecordNamesTheRecordsOwnOffset(t *testing.T) {
	// Arrange. A write record owes the whole write vocabulary, and the offset is
	// the coordinate that says WHERE on disk the stored record came from — a
	// residue record without it cannot be found again in the file.
	var sink bytes.Buffer
	h := NewSessionTranscriptHandler(captureLog(t, &sink))
	// A `queue-operation` line is withheld bookkeeping: it is stored as an
	// unserved item, which is exactly what logResidue reports.
	lines := "{\"type\":\"queue-operation\",\"uuid\":\"q0\",\"timestamp\":\"2026-07-21T15:36:10.000Z\"}\n" +
		"{\"type\":\"queue-operation\",\"uuid\":\"q1\",\"timestamp\":\"2026-07-21T15:36:11.000Z\"}"
	frames := framesFrom(t, lines)

	// Act.
	h.Handle(frames, sessionContext("/p/s.jsonl", "s"))

	// Assert: each residue record names the offset of the frame it came from.
	var offsets []float64
	for _, rec := range decodeCaptured(t, &sink) {
		if rec.Operation != "residue" {
			continue
		}
		raw, ok := rec.Context["offset"]
		if !ok {
			t.Fatalf("residue record %q carries no offset; its context was %v", rec.Message, rec.Context)
		}
		offsets = append(offsets, raw.(float64))
	}
	if len(offsets) != 2 {
		t.Fatalf("residue records = %d, want one per withheld record", len(offsets))
	}
	if offsets[0] != float64(frames[0].Offset) || offsets[1] != float64(frames[1].Offset) {
		t.Fatalf("residue offsets = %v, want the frames' own %d and %d", offsets, frames[0].Offset, frames[1].Offset)
	}
}

func TestAnUnparsableLineIsReportedAtItsOwnOffset(t *testing.T) {
	// Arrange. An unparsable line is the record a human is most likely to go
	// looking for on disk, so its byte position is the least optional of all.
	var sink bytes.Buffer
	h := NewSessionTranscriptHandler(captureLog(t, &sink))
	frames := []tail.Frame{{Raw: []byte("{not json"), Offset: 4096, ParseErr: errors.New("invalid character 'n'")}}

	// Act.
	h.Handle(frames, sessionContext("/p/s.jsonl", "s"))

	// Assert.
	var found bool
	for _, rec := range decodeCaptured(t, &sink) {
		if rec.Operation != "residue" {
			continue
		}
		found = true
		if got, ok := rec.Context["offset"]; !ok || got.(float64) != 4096 {
			t.Fatalf("unparsed residue record names offset %v, want 4096", got)
		}
	}
	if !found {
		t.Fatal("an unparsable line produced no residue write record at all")
	}
}

// TestTheClassificationRecordCarriesTheResidueLabelItWillBeWithheldUnder is the
// join. Since the residue ruling (2026-09-13) the bytes are not stored, so the
// only account of an uncarried line is two records: this one, which has the
// POSITION, and the reader's `residue-drop`, which has the withholding and the
// tally. They join on `reason`, and without it the position record and the
// count record are two unrelated lines about one line of a file.
func TestTheClassificationRecordCarriesTheResidueLabelItWillBeWithheldUnder(t *testing.T) {
	// Arrange.
	var sink bytes.Buffer
	h := NewSessionTranscriptHandler(captureLog(t, &sink))
	frames := framesFrom(t, `{"type":"queue-operation","uuid":"q0","timestamp":"2026-07-21T15:36:10.000Z"}`)

	// Act.
	h.Handle(frames, sessionContext("/p/s.jsonl", "s"))

	// Assert.
	var reasons []string
	for _, rec := range decodeCaptured(t, &sink) {
		if rec.Operation != "residue" {
			continue
		}
		reason, ok := rec.Context["reason"].(string)
		if !ok {
			t.Fatalf("the classification record carries no reason; its context was %v", rec.Context)
		}
		reasons = append(reasons, reason)
	}
	if len(reasons) != 1 || reasons[0] != "vendor_specific/queue-operation" {
		t.Fatalf("classification reasons = %v, want the one residue label the reader withholds it under", reasons)
	}
}

// TestAnUnparsableLineIsStillInvestigableFromTheLogAlone: the raw bytes are no
// longer stored anywhere, so the parse failure record IS the investigation —
// it must name the error and the position in the vendor's own durable file
// where the offending bytes still sit.
func TestAnUnparsableLineIsStillInvestigableFromTheLogAlone(t *testing.T) {
	// Arrange.
	var sink bytes.Buffer
	h := NewSessionTranscriptHandler(captureLog(t, &sink))
	frames := framesFrom(t, `{"type":`)

	// Act.
	h.Handle(frames, sessionContext("/p/s.jsonl", "s"))

	// Assert.
	var found bool
	for _, rec := range decodeCaptured(t, &sink) {
		if rec.Operation != "parse" {
			continue
		}
		found = true
		if rec.Context["path"] != "/p/s.jsonl" {
			t.Fatalf("the parse failure names path %v, want the file the bytes are in", rec.Context["path"])
		}
		if _, ok := rec.Context["offset"]; !ok {
			t.Fatalf("the parse failure names no offset; its context was %v", rec.Context)
		}
		if !strings.Contains(rec.Message, "parse failure") {
			t.Fatalf("the parse failure message %q does not state the failure", rec.Message)
		}
	}
	if !found {
		t.Fatal("an unparsable line produced no parse failure record, so nothing says which bytes to go and look at")
	}
}
