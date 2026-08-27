package handler

import (
	"io"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/tail"
)

func agentContext() *Context {
	return &Context{SessionID: "sess-1", Path: "/t/agent-7.jsonl", Kind: tail.KindAgentTranscript, TaskID: "agent-7"}
}

// A parse failure inside a sidechain is stored as evidence, exactly as in a
// session transcript: ingestion loses nothing whichever file it is reading.
func TestSidechainParseFailureIsStored(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLog(t))
	frames := []tail.Frame{{Raw: []byte("{"), Offset: 9, ParseErr: io.ErrUnexpectedEOF}}

	// Act.
	entries := h.Handle(frames, agentContext())

	// Assert.
	if len(entries) != 1 || entries[0].GetAgentUpdate().GetUnservedItem().GetUnparsed() == nil {
		t.Fatalf("entries = %#v, want one unparsed record", entries)
	}
}
