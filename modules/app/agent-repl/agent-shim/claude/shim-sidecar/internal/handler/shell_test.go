package handler

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/tail"
)

func shellContext() *Context {
	return &Context{SessionID: "sess-1", Path: "/tmp/tasks/b-1.output", Kind: tail.KindShellSpool, TaskID: "b-1"}
}

// A spool with no task identity names no card, so its bytes have nowhere to go.
// The sidecar refuses to tail an unattributed spool at all, so reaching here
// means that guarantee broke and the handler must not invent a card.
func TestUnattributedSpoolProducesNoCard(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLog(t))
	ctx := shellContext()
	ctx.TaskID = ""

	// Act.
	entries := h.Handle([]tail.Frame{{Raw: []byte("bytes\n")}}, ctx)

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("entries = %d, want none; a spool with no owner was given an invented card", len(entries))
	}
}

// An empty batch is not a spool that finished; it is a spool that said nothing.
func TestEmptyBatchProducesNothing(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLog(t))

	// Act.
	entries := h.Handle(nil, shellContext())

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("entries = %d, want none", len(entries))
	}
}
