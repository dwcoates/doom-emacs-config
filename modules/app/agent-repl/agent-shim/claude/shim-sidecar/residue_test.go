package main

import (
	"io"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

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
