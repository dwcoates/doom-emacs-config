package handler

import (
	"io"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/tail"
)

func journalContext() *Context {
	return &Context{
		SessionID: "sess-1",
		Path:      "/t/workflows/wf_1/journal.jsonl",
		Kind:      tail.KindWorkflowJournal,
		TaskID:    "wf_1",
		RunID:     "wf_1",
	}
}

// A journal record type this reader does not know is stored whole rather than
// rendered as a blank step, because only one of the two is reversible.
func TestUnknownJournalStepIsStoredUnconverted(t *testing.T) {
	// Arrange.
	h := NewWorkflowJournalHandler(testLog(t))

	// Act.
	entries := h.Handle([]tail.Frame{frame(t, 0, map[string]any{"type": "brand-new"})}, journalContext())

	// Assert.
	if entries[0].GetAgentUpdate().GetUnservedItem() == nil {
		t.Fatal("an unmodeled journal step was rendered into the run's output")
	}
	if entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown() == nil {
		t.Fatal("an unmodeled journal step was not stored as an unknown record")
	}
}

// A parse failure in a journal is stored as evidence like any other.
func TestJournalParseFailureIsStored(t *testing.T) {
	// Arrange.
	h := NewWorkflowJournalHandler(testLog(t))
	frames := []tail.Frame{{Raw: []byte("{"), Offset: 3, ParseErr: io.ErrUnexpectedEOF}}

	// Act.
	entries := h.Handle(frames, journalContext())

	// Assert.
	if len(entries) != 1 || entries[0].GetAgentUpdate().GetUnservedItem().GetUnparsed() == nil {
		t.Fatalf("entries = %#v, want one unparsed record", entries)
	}
}
