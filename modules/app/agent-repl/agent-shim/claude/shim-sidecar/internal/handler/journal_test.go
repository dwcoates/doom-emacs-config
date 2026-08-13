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

// A workflow's journal is the OUTPUT of a run that already has a card, so its
// steps accumulate into that card rather than becoming feed rows of their own.
func TestJournalStepsAccumulateIntoTheRunsCard(t *testing.T) {
	// Arrange.
	h := NewWorkflowJournalHandler(testLog(t))
	frames := []tail.Frame{
		frame(t, 0, map[string]any{"type": "started", "key": "build"}),
		frame(t, 40, map[string]any{"type": "result", "key": "build", "result": "ok"}),
	}

	// Act.
	entries := h.Handle(frames, journalContext())

	// Assert.
	if len(entries) != 2 {
		t.Fatalf("entries = %d, want one per step", len(entries))
	}
	for _, entry := range entries {
		message := entry.GetExternal().GetMessage()
		if message.GetMessageId() != "dw:wf_1" {
			t.Fatalf("message_id = %q, want the run's card %q", message.GetMessageId(), "dw:wf_1")
		}
		if message.GetDetachedWorkProgressed() == nil {
			t.Fatal("a journal step did not become progress on the run")
		}
	}
}

// The run id lives in the file PATH rather than in any record, so the handler
// supplies it. When the path gave none, the task id names the same card, because
// the launch that opened it used whichever the harness reported.
func TestJournalFallsBackToTheTaskIDWhenThePathNamesNoRun(t *testing.T) {
	// Arrange.
	h := NewWorkflowJournalHandler(testLog(t))
	ctx := journalContext()
	ctx.RunID = ""
	ctx.TaskID = "local_workflow_3"

	// Act.
	entries := h.Handle([]tail.Frame{frame(t, 0, map[string]any{"type": "started", "key": "step"})}, ctx)

	// Assert.
	if got := entries[0].GetExternal().GetMessage().GetMessageId(); got != "dw:local_workflow_3" {
		t.Fatalf("message_id = %q, want the task's card", got)
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
	if entries[0].GetExternal() != nil {
		t.Fatal("an unmodeled journal step was rendered into the run's output")
	}
	if entries[0].GetInternal().GetUnknown() == nil {
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
	if len(entries) != 1 || entries[0].GetInternal().GetUnparsed() == nil {
		t.Fatalf("entries = %#v, want one unparsed record", entries)
	}
}
