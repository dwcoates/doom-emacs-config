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
	entry := residueEntry("16777232:501", "/private/tmp/b1.output", 128, "why", []byte("bytes"))

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
	entry := residueEntry("16777232:501", "/private/tmp/b1.output", 0, "why", nil)

	// Assert.
	if entry.GetPlane().GetFile() == nil {
		t.Fatalf("plane = %v, want the file plane", entry.GetPlane())
	}
}

func TestResidueWriteIDIsDeterministic(t *testing.T) {
	// Arrange, Act: the same bytes re-read after a restart.
	first := residueEntry("16777232:501", "/private/tmp/b1.output", 128, "why", []byte("bytes"))
	second := residueEntry("16777232:501", "/private/tmp/b1.output", 128, "why", []byte("bytes"))

	// Assert: replay absorption rests on this.
	if first.GetWriteId() != second.GetWriteId() {
		t.Fatalf("write ids %q and %q differ for one source coordinate", first.GetWriteId(), second.GetWriteId())
	}
}

func TestResidueWriteIDVariesByOffset(t *testing.T) {
	// Arrange, Act.
	first := residueEntry("16777232:501", "/private/tmp/b1.output", 0, "why", []byte("bytes"))
	second := residueEntry("16777232:501", "/private/tmp/b1.output", 128, "why", []byte("bytes"))

	// Assert.
	if first.GetWriteId() == second.GetWriteId() {
		t.Fatal("two positions in one file minted the same write id")
	}
}

func TestResidueWriteIDIsUnchangedByARenameOfTheSpool(t *testing.T) {
	// Arrange, Act. R-S1: the cursor is keyed by "dev:inode" and survives a
	// rename, so the write identity minted from the same byte range must too —
	// otherwise the replay after the rename is stored a second time instead of
	// being absorbed.
	first := residueEntry("16777232:501", "/private/tmp/b1.output", 128, "why", []byte("bytes"))
	second := residueEntry("16777232:501", "/private/tmp/b2.output", 128, "why", []byte("bytes"))

	// Assert.
	if first.GetWriteId() != second.GetWriteId() {
		t.Fatalf("a rename changed the residue write id (%q -> %q)", first.GetWriteId(), second.GetWriteId())
	}
}

func TestResidueWriteIDVariesByFileID(t *testing.T) {
	// Arrange, Act: the same offset in two DIFFERENT files.
	first := residueEntry("16777232:501", "/private/tmp/b1.output", 0, "why", []byte("bytes"))
	second := residueEntry("16777232:502", "/private/tmp/b1.output", 0, "why", []byte("bytes"))

	// Assert: two files must never share an identity space keyed only by offset.
	if first.GetWriteId() == second.GetWriteId() {
		t.Fatal("two distinct files minted the same residue write id for one offset")
	}
}

func TestResidueEntryWithoutAFileIDIsRaisedAsAReaderDefect(t *testing.T) {
	// Arrange, Act + Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("residue minted without a file id must be raised, never digested as an empty string")
		}
	}()
	_ = residueEntry("", "/private/tmp/b1.output", 0, "why", nil)
}

func TestResidueUpsertKeyIsItsSourceCoordinate(t *testing.T) {
	// Arrange, Act.
	entry := residueEntry("16777232:501", "/private/tmp/b1.output", 128, "why", nil)

	// Assert: a re-read supersedes the row whole rather than growing a second.
	// A spool's bytes carry no vendor uuid — there is no record, only a byte
	// range — so they key on where they live, in the `residue:file:` space that
	// is kept visibly apart from the uuid space.
	if got := entry.GetUpsertKey(); got != "residue:file:/private/tmp/b1.output:128" {
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
	got := h.Handle(frames, &tail.Context{Path: "/private/tmp/q1.output", FileID: "16777232:777"})

	// Assert: nothing on disk is dropped.
	if len(got) != 2 {
		t.Fatalf("entries = %d, want one per frame", len(got))
	}
}

func TestResidueHandlerCarriesTheReason(t *testing.T) {
	// Arrange.
	h := residueHandlerFor(t)

	// Act.
	got := h.Handle([]tail.Frame{{Raw: []byte("x")}}, &tail.Context{Path: "/private/tmp/q1.output", FileID: "16777232:777"})

	// Assert: the stored record says what a human needs without reading a log.
	if reason := got[0].GetAgentUpdate().GetUnservedItem().GetUnparsed().GetParseError(); reason != "no conversion could be selected" {
		t.Fatalf("parse error = %q, want the handler's reason", reason)
	}
}

func TestResidueHandlerOnAnEmptyBatch(t *testing.T) {
	// Arrange.
	h := residueHandlerFor(t)

	// Act.
	got := h.Handle(nil, &tail.Context{Path: "/private/tmp/q1.output", FileID: "16777232:777"})

	// Assert.
	if len(got) != 0 {
		t.Fatalf("entries = %d, want none", len(got))
	}
}

// ---- ruling R-S4: a w* spool is DECLARED residue, not a classification failure ----

func declaredResidueHandlerFor(t *testing.T) *declaredResidueHandler {
	t.Helper()
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "declared-residue-test"})
	return newDeclaredResidueHandler(workflowSpoolKind, log)
}

func TestAWorkflowSpoolLandsAsItsDeclaredVendorKind(t *testing.T) {
	// Arrange. Workflow is KICKED this wave, so the conversion deliberately does
	// not exist. The bytes are still ingested WHOLE, under a kind that says so —
	// the day workflow ingestion lands, every one of these rows is findable by
	// it, which an `unparsed` row saying "no conversion could be selected" would
	// never be.
	h := declaredResidueHandlerFor(t)
	ctx := &tail.Context{
		Path: "/private/tmp/w1.output", FileID: "16777232:41", TaskID: "w1",
		Kind: tail.KindWorkflowSpool,
	}

	// Act.
	got := h.Handle([]tail.Frame{{Raw: []byte("journal bytes"), Offset: 0}}, ctx)

	// Assert.
	if len(got) != 1 {
		t.Fatalf("entries = %d, want one per frame", len(got))
	}
	v := got[0].GetAgentUpdate().GetUnservedItem().GetVendorSpecific()
	if v == nil {
		t.Fatalf("entry = %v, want the vendor_specific arm, not unparsed", got[0])
	}
	if v.GetKind() != "spool/workflow" {
		t.Fatalf("kind = %q, want the declared %q", v.GetKind(), "spool/workflow")
	}
}

func TestAWorkflowSpoolIsKeyedByItsFileCoordinates(t *testing.T) {
	// Arrange. R-S4 pins the key: a spool's bytes are a byte RANGE, not a
	// record, so there is no vendor uuid the other plane could agree on and the
	// row is keyed by where it lives.
	h := declaredResidueHandlerFor(t)
	ctx := &tail.Context{Path: "/private/tmp/w1.output", FileID: "16777232:41", TaskID: "w1"}

	// Act.
	got := h.Handle([]tail.Frame{{Raw: []byte("more"), Offset: 128}}, ctx)

	// Assert.
	if want := "residue:file:/private/tmp/w1.output:128"; got[0].GetUpsertKey() != want {
		t.Fatalf("upsert key = %q, want %q", got[0].GetUpsertKey(), want)
	}
}

func TestAWorkflowSpoolCarriesItsBytesWhole(t *testing.T) {
	// Arrange. Nothing on disk is dropped, whatever this wave converts.
	h := declaredResidueHandlerFor(t)
	ctx := &tail.Context{Path: "/private/tmp/w1.output", FileID: "16777232:41", TaskID: "w1"}

	// Act.
	got := h.Handle([]tail.Frame{{Raw: []byte("the workflow said this"), Offset: 0}}, ctx)

	// Assert.
	raw := got[0].GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetRaw()
	if raw.GetFields()["output"].GetStringValue() != "the workflow said this" {
		t.Fatalf("raw = %v, want the spool's bytes whole", raw)
	}
}

func TestAWorkflowSpoolIsNeverAPageLine(t *testing.T) {
	// Arrange. Residue is by construction not servable: it must never reach a
	// book, or a kicked feature would show up in a conversation as itself.
	h := declaredResidueHandlerFor(t)
	ctx := &tail.Context{Path: "/private/tmp/w1.output", FileID: "16777232:41", TaskID: "w1"}

	// Act.
	got := h.Handle([]tail.Frame{{Raw: []byte("x"), Offset: 0}}, ctx)

	// Assert.
	if got[0].GetAgentUpdate().GetServeableFrame() != nil {
		t.Fatal("a workflow spool reached a page; workflow is kicked and its bytes are residue")
	}
}
