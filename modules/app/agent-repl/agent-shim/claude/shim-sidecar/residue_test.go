package main

import (
	"io"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func residueHandlerFor(t *testing.T) *residueHandler {
	t.Helper()
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "residue-test"})
	return newResidueHandler("no conversion could be selected", log)
}

func TestResidueEntryIsUnservedUnparsed(t *testing.T) {
	// Arrange, Act.
	entry := residueEntry("/private/tmp/b1.output", 128, "why", []byte("bytes"))

	// Assert: it can never reach a page, and it is investigable.
	unparsed := entry.GetAgentUpdate().GetUnservedItem().GetUnparsed()
	if unparsed == nil {
		t.Fatalf("entry = %v, want the unserved unparsed arm", entry)
	}
	if unparsed.GetSource() != "/private/tmp/b1.output" || unparsed.GetOffset() != 128 || unparsed.GetRaw() != "bytes" {
		t.Fatalf("unparsed = %v, want the spool named as the source with its bytes whole", unparsed)
	}
}

func TestResidueEntryNamesTheFilePlane(t *testing.T) {
	// Arrange, Act.
	entry := residueEntry("/private/tmp/b1.output", 0, "why", nil)

	// Assert.
	if entry.GetPlane().GetFile() == nil {
		t.Fatalf("plane = %v, want the file plane", entry.GetPlane())
	}
}

func TestResidueWriteIDIsDeterministic(t *testing.T) {
	// Arrange, Act: the same bytes re-read after a restart.
	first := residueEntry("/private/tmp/b1.output", 128, "why", []byte("bytes"))
	second := residueEntry("/private/tmp/b1.output", 128, "why", []byte("bytes"))

	// Assert: replay absorption rests on this.
	if first.GetWriteId() != second.GetWriteId() {
		t.Fatalf("write ids %q and %q differ for one source coordinate", first.GetWriteId(), second.GetWriteId())
	}
}

func TestResidueWriteIDVariesByOffset(t *testing.T) {
	// Arrange, Act.
	first := residueEntry("/private/tmp/b1.output", 0, "why", []byte("bytes"))
	second := residueEntry("/private/tmp/b1.output", 128, "why", []byte("bytes"))

	// Assert.
	if first.GetWriteId() == second.GetWriteId() {
		t.Fatal("two positions in one file minted the same write id")
	}
}

func TestResidueUpsertKeyIsItsSourceCoordinate(t *testing.T) {
	// Arrange, Act.
	entry := residueEntry("/private/tmp/b1.output", 128, "why", nil)

	// Assert: a re-read supersedes the row whole rather than growing a second.
	if got := entry.GetUpsertKey(); got != "residue:/private/tmp/b1.output:128" {
		t.Fatalf("upsert key = %q, want the source coordinate", got)
	}
}

func TestResidueHandlerConvertsEveryFrame(t *testing.T) {
	// Arrange.
	h := residueHandlerFor(t)
	frames := []tail.Frame{
		{Raw: []byte("first"), Offset: 0},
		{Raw: []byte("second"), Offset: 5},
	}

	// Act.
	got := h.Handle(frames, &tail.Context{Path: "/private/tmp/q1.output"})

	// Assert: nothing on disk is dropped.
	if len(got) != 2 {
		t.Fatalf("entries = %d, want one per frame", len(got))
	}
}

func TestResidueHandlerCarriesTheReason(t *testing.T) {
	// Arrange.
	h := residueHandlerFor(t)

	// Act.
	got := h.Handle([]tail.Frame{{Raw: []byte("x")}}, &tail.Context{Path: "/private/tmp/q1.output"})

	// Assert: the stored record says what a human needs without reading a log.
	if reason := got[0].GetAgentUpdate().GetUnservedItem().GetUnparsed().GetParseError(); reason != "no conversion could be selected" {
		t.Fatalf("parse error = %q, want the handler's reason", reason)
	}
}

func TestResidueHandlerOnAnEmptyBatch(t *testing.T) {
	// Arrange.
	h := residueHandlerFor(t)

	// Act.
	got := h.Handle(nil, &tail.Context{Path: "/private/tmp/q1.output"})

	// Assert.
	if len(got) != 0 {
		t.Fatalf("entries = %d, want none", len(got))
	}
}
