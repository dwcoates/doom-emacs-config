package handler

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/tail"
)

func shellContext() *Context {
	return &Context{SessionID: "sess-1", Path: "/tmp/tasks/b-1.output", Kind: tail.KindShellSpool, TaskID: "b-1"}
}

// A spool's bytes accumulate into the card its run owns, as a DELTA. Re-sending
// the whole spool on every update is how a long-running shell costs more to
// watch than it did to run.
func TestSpoolBytesAppendToTheRunsCard(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLog(t))
	frames := []tail.Frame{{Raw: []byte("line one\n"), Offset: 0}}

	// Act.
	entries := h.Handle(frames, shellContext())

	// Assert.
	message := entries[0].GetExternal().GetMessage()
	if message.GetMessageId() != "dw:b-1" {
		t.Fatalf("message_id = %q, want the run's card %q", message.GetMessageId(), "dw:b-1")
	}
	if got := message.GetDetachedWorkProgressed().GetOutput(); got != "line one\n" {
		t.Fatalf("output = %q, want the batch's bytes", got)
	}
}

// The exit marker is the one structured byte a spool has, and reading it is what
// keeps a task that plainly finished from sitting as running until a silence
// timeout eventually calls it LOST — the wrong verdict as well as a late one.
func TestExitMarkerEndsTheRunOnEvidence(t *testing.T) {
	tests := []struct {
		name        string
		raw         string
		offset      int64
		wantEnded   bool
		wantSuccess bool
	}{
		{name: "clean exit terminating the spool", raw: "done\nEXIT=0\n", wantEnded: true, wantSuccess: true},
		{name: "failing exit terminating the spool", raw: "boom\nEXIT=2\n", wantEnded: true, wantSuccess: false},
		{name: "marker alone in a spool with no other output", raw: "EXIT=0\n", wantEnded: true, wantSuccess: true},
		{name: "marker as ordinary mid-line output", raw: "BUILD_EXIT=0\n", wantEnded: false},
		{name: "marker not at the end of the batch", raw: "EXIT=0\nmore output\n", wantEnded: false},
		{name: "marker with no trailing newline", raw: "EXIT=0", wantEnded: false},
		{name: "marker with non-digit payload", raw: "EXIT=abc\n", wantEnded: false},
		{name: "marker with too many digits to be an exit code", raw: "EXIT=1234\n", wantEnded: false},
		{name: "line-start claim that begins mid-file", raw: "EXIT=0\n", offset: 500, wantEnded: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := NewShellOutputHandler(testLog(t))
			frames := []tail.Frame{{Raw: []byte(tc.raw), Offset: tc.offset}}

			// Act.
			entries := h.Handle(frames, shellContext())

			// Assert.
			var ended bool
			var succeeded bool
			for _, entry := range entries {
				if e := entry.GetExternal().GetMessage().GetDetachedWorkEnded(); e != nil {
					ended = true
					succeeded = e.GetSucceeded() != nil
				}
			}
			if ended != tc.wantEnded {
				t.Fatalf("ended = %t for %q, want %t", ended, tc.raw, tc.wantEnded)
			}
			if tc.wantEnded && succeeded != tc.wantSuccess {
				t.Fatalf("succeeded = %t for %q, want %t", succeeded, tc.raw, tc.wantSuccess)
			}
		})
	}
}

// Completion is never GUESSED. Absent the marker this handler infers nothing and
// the staleness policy owns the outcome.
func TestNoMarkerMeansNoTerminalVerdict(t *testing.T) {
	// Arrange.
	h := NewShellOutputHandler(testLog(t))
	frames := []tail.Frame{{Raw: []byte("still working\n"), Offset: 0}}

	// Act.
	entries := h.Handle(frames, shellContext())

	// Assert.
	for _, entry := range entries {
		if entry.GetExternal().GetMessage().GetDetachedWorkEnded() != nil {
			t.Fatal("the handler ended a run it had no evidence had finished")
		}
	}
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
