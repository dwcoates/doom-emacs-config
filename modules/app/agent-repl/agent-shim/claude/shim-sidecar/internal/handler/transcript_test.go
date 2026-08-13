package handler

import (
	"encoding/json"
	"io"
	"testing"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func testLog(t *testing.T) *logging.Bound {
	t.Helper()
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	return log
}

// frame builds one decoded JSONL frame at an offset.
func frame(t *testing.T, offset int64, record map[string]any) tail.Frame {
	t.Helper()
	raw, err := json.Marshal(record)
	if err != nil {
		t.Fatalf("marshaling test record: %v", err)
	}
	return tail.Frame{Obj: record, Raw: raw, Offset: offset}
}

func sessionContext() *Context {
	return &Context{SessionID: "sess-1", Path: "/t/sess-1.jsonl", Kind: tail.KindSessionTranscript}
}

// Every frame the handler is given must produce at least one stored record.
// Ingestion's job is that a JSON object on disk ends up in the database; whether
// anyone ever reads it is a consumption-side judgment made much later.
func TestEveryFrameProducesARecord(t *testing.T) {
	// Arrange.
	h := NewSessionTranscriptHandler(testLog(t))
	frames := []tail.Frame{
		frame(t, 0, map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hi"}}),
		frame(t, 10, map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}}}),
		frame(t, 20, map[string]any{"type": "ai-title", "aiTitle": "a name"}),
	}

	// Act.
	entries := h.Handle(frames, sessionContext())

	// Assert.
	if len(entries) < len(frames) {
		t.Fatalf("entries = %d for %d frames; at least one line never reached the store", len(entries), len(frames))
	}
}

// A frame that failed to PARSE is a failure rather than a gap, and it is stored
// as evidence rather than skipped.
func TestParseFailureIsStoredAsUnparsed(t *testing.T) {
	// Arrange.
	h := NewSessionTranscriptHandler(testLog(t))
	broken := tail.Frame{Raw: []byte(`{"type":"user"`), Offset: 42, ParseErr: io.ErrUnexpectedEOF}

	// Act.
	entries := h.Handle([]tail.Frame{broken}, sessionContext())

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	unparsed := entries[0].GetInternal().GetUnparsed()
	if unparsed == nil {
		t.Fatal("a parse failure produced no unparsed record")
	}
	if unparsed.GetOffset() != 42 {
		t.Fatalf("offset = %d, want 42", unparsed.GetOffset())
	}
}

// ---------------------------------------------------------------------------
// the compaction hold
// ---------------------------------------------------------------------------

// A compaction boundary at the END of a batch is UNSETTLED: its summary is the
// next line in the file and may not be written yet. It is deferred, with the
// cursor left parked before it, rather than converted on half the evidence.
func TestBatchTerminalCompactBoundaryIsDeferred(t *testing.T) {
	// Arrange.
	h := NewSessionTranscriptHandler(testLog(t))
	ctx := sessionContext()
	ctx.Redelivers = true
	frames := []tail.Frame{
		frame(t, 0, map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hi"}}),
		frame(t, 60, map[string]any{"type": "system", "uuid": "s1", "subtype": "compact_boundary"}),
	}

	// Act.
	entries := h.Handle(frames, ctx)

	// Assert.
	if ctx.HeldOffset != 60 || ctx.HeldDeliveries != 1 {
		t.Fatalf("held offset/deliveries = %d/%d, want 60/1", ctx.HeldOffset, ctx.HeldDeliveries)
	}
	for _, entry := range entries {
		if entry.GetExternal().GetMessage().GetContextCut() != nil {
			t.Fatal("the deferred boundary was converted anyway, so its summary is lost for good")
		}
	}
}

// A boundary in the MIDDLE of a batch is settled by the line after it, so there
// is nothing to wait for.
func TestMidBatchCompactBoundaryIsNotDeferred(t *testing.T) {
	// Arrange.
	h := NewSessionTranscriptHandler(testLog(t))
	ctx := sessionContext()
	ctx.Redelivers = true
	frames := []tail.Frame{
		frame(t, 0, map[string]any{"type": "system", "uuid": "s1", "subtype": "compact_boundary"}),
		frame(t, 60, map[string]any{"type": "user", "uuid": "u1", "isCompactSummary": true, "message": map[string]any{"content": "we did X"}}),
	}

	// Act.
	entries := h.Handle(frames, ctx)

	// Assert.
	if ctx.HeldDeliveries != 0 {
		t.Fatalf("held deliveries = %d, want 0", ctx.HeldDeliveries)
	}
	var summary string
	for _, entry := range entries {
		if cut := entry.GetExternal().GetMessage().GetContextCut(); cut.GetCompacted() != nil {
			summary = cut.GetCompacted().GetSummary().GetBlocks()[0].GetText().GetText()
		}
	}
	if summary != "we did X" {
		t.Fatalf("summary = %q, want the following line's text", summary)
	}
}

// A caller that will NOT hand the frame back gets the conversion now. Deferring
// there would drop the compaction for good, which is never allowed.
func TestBoundaryIsNotDeferredWhenTheReaderWillNotRedeliver(t *testing.T) {
	// Arrange.
	h := NewSessionTranscriptHandler(testLog(t))
	ctx := sessionContext()
	ctx.Redelivers = false
	frames := []tail.Frame{frame(t, 0, map[string]any{"type": "system", "uuid": "s1", "subtype": "compact_boundary"})}

	// Act.
	entries := h.Handle(frames, ctx)

	// Assert.
	if !hasContextCut(entries) {
		t.Fatal("a boundary was withheld from a caller that cannot redeliver it")
	}
}

// The wait is BOUNDED. A session that genuinely stops at a boundary must still
// render its truncation rather than holding forever.
func TestHeldBoundaryIsConvertedAfterTheSilenceBound(t *testing.T) {
	// Arrange.
	h := NewSessionTranscriptHandler(testLog(t))
	ctx := sessionContext()
	ctx.Redelivers = true
	frames := []tail.Frame{frame(t, 60, map[string]any{"type": "system", "uuid": "s1", "subtype": "compact_boundary"})}

	// Act: the first delivery defers it, the second gives up.
	first := h.Handle(frames, ctx)
	second := h.Handle(frames, ctx)

	// Assert.
	if hasContextCut(first) {
		t.Fatal("the boundary was converted on its first delivery instead of being held")
	}
	if !hasContextCut(second) {
		t.Fatal("the boundary was still held past the silence bound, so a stopped session never renders its cut")
	}
}

// A hold is only ever taken for a boundary. Every other record is settled by its
// own bytes and must be converted the moment it is read.
func TestOnlyACompactBoundaryIsEverDeferred(t *testing.T) {
	tests := []struct {
		name string
		last map[string]any
	}{
		{name: "a user prompt", last: map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hi"}}},
		{name: "an assistant response", last: map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}}}},
		{name: "another system subtype", last: map[string]any{"type": "system", "uuid": "s1", "subtype": "turn_duration"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := NewSessionTranscriptHandler(testLog(t))
			ctx := sessionContext()
			ctx.Redelivers = true

			// Act.
			h.Handle([]tail.Frame{frame(t, 0, tc.last)}, ctx)

			// Assert.
			if ctx.HeldDeliveries != 0 {
				t.Fatalf("held deliveries = %d for a settled record, want 0", ctx.HeldDeliveries)
			}
		})
	}
}

// An unparsable final frame is settled: it becomes an unparsed record now, and
// no later line can change that.
func TestUnparsableFinalFrameIsNotDeferred(t *testing.T) {
	// Arrange.
	h := NewSessionTranscriptHandler(testLog(t))
	ctx := sessionContext()
	ctx.Redelivers = true
	frames := []tail.Frame{{Raw: []byte("{"), Offset: 5, ParseErr: io.ErrUnexpectedEOF}}

	// Act.
	entries := h.Handle(frames, ctx)

	// Assert.
	if ctx.HeldDeliveries != 0 {
		t.Fatalf("held deliveries = %d, want 0", ctx.HeldDeliveries)
	}
	if entries[0].GetInternal().GetUnparsed() == nil {
		t.Fatal("the unparsable frame was not stored")
	}
}

func hasContextCut(entries []*agentshimv1.Entry) bool {
	for _, entry := range entries {
		if entry.GetExternal().GetMessage().GetContextCut() != nil {
			return true
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// attribution
// ---------------------------------------------------------------------------

// A session transcript's records are feed rows; an agent sidechain's sit inside
// the card the subagent runs as. The handler is what decides which, from the
// file it is reading rather than from anything in the record.
func TestAttributionPlacesRecordsByTheFileTheyCameFrom(t *testing.T) {
	tests := []struct {
		name          string
		kind          tail.Kind
		taskID        string
		wantContainer string
	}{
		{name: "session transcript", kind: tail.KindSessionTranscript, taskID: "", wantContainer: ""},
		{name: "agent sidechain", kind: tail.KindAgentTranscript, taskID: "agent-1", wantContainer: "dw:agent-1"},
		{name: "shell spool", kind: tail.KindShellSpool, taskID: "b-2", wantContainer: "dw:b-2"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ctx := &Context{SessionID: "sess-1", Path: "/t/f", Kind: tc.kind, TaskID: tc.taskID}

			// Act.
			at := attribute(ctx, 12)

			// Assert.
			if at.Container != tc.wantContainer {
				t.Fatalf("container = %q, want %q", at.Container, tc.wantContainer)
			}
			if at.SessionID != "sess-1" || at.Path != "/t/f" || at.Offset != 12 {
				t.Fatalf("attribution did not carry the reader's own position: %+v", at)
			}
		})
	}
}

// A session transcript never sits inside a card, even when the discoverer
// happened to give the context a task id.
func TestSessionTranscriptIsNeverPlacedInsideACard(t *testing.T) {
	// Arrange.
	ctx := &Context{SessionID: "sess-1", Path: "/t/f", Kind: tail.KindSessionTranscript, TaskID: "agent-1"}

	// Act.
	at := attribute(ctx, 0)

	// Assert.
	if at.Container != "" {
		t.Fatalf("container = %q, want the session transcript to stay a feed row", at.Container)
	}
}
