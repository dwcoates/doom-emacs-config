package handler

import (
	"io"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/tail"
)

func agentContext() *Context {
	return &Context{SessionID: "sess-1", Path: "/t/agent-7.jsonl", Kind: tail.KindAgentTranscript, TaskID: "agent-7"}
}

// A subagent's conversation is a real conversation, and it lands INSIDE the card
// the subagent runs as. That is the whole difference from a session transcript:
// a page of ten rows counts the subagent once, however long it talks.
func TestSidechainRecordsLandInsideTheSubagentsCard(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLog(t))
	frames := []tail.Frame{
		frame(t, 0, map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "go"}}),
		frame(t, 30, map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}}}),
	}

	// Act.
	entries := h.Handle(frames, agentContext())

	// Assert.
	if len(entries) != 2 {
		t.Fatalf("entries = %d, want one per frame", len(entries))
	}
	for _, entry := range entries {
		message := entry.GetExternal().GetMessage()
		if got := message.GetTopLevelMessageId(); got != "dw:agent-7" {
			t.Fatalf("top_level_message_id = %q, want the subagent's card %q", got, "dw:agent-7")
		}
		if message.GetParent().GetInside() == nil {
			t.Fatal("a sidechain record was emitted as a feed row of its own")
		}
	}
}

// The agent speaking inside detached work is the DETACHED agent, named by the
// work it runs as, so its emissions route to the card without a correlation.
func TestSidechainAgentNamesItsOwnWork(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLog(t))
	frames := []tail.Frame{frame(t, 0, map[string]any{
		"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}},
	})}

	// Act.
	entries := h.Handle(frames, agentContext())

	// Assert.
	author := entries[0].GetExternal().GetMessage().GetAuthor()
	if got := author.GetDetachedAgent().GetDetachedWorkMessageId(); got != "dw:agent-7" {
		t.Fatalf("detached_work_message_id = %q, want %q", got, "dw:agent-7")
	}
}

// A subagent can itself launch detached work. Those grandchildren open their own
// cards, because the converter reads a launch off the tool result rather than
// off the file it happens to be reading.
func TestSidechainGrandchildLaunchOpensItsOwnCard(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLog(t))
	frames := []tail.Frame{frame(t, 0, map[string]any{
		"type": "user", "uuid": "u1", "sourceToolUseID": "call-1",
		"toolUseResult": map[string]any{"isAsync": true, "agentId": "agent-99", "description": "nested"},
		"message":       map[string]any{"content": "ok"},
	})}

	// Act.
	entries := h.Handle(frames, agentContext())

	// Assert.
	var opened bool
	for _, entry := range entries {
		message := entry.GetExternal().GetMessage()
		if message.GetDetachedWorkStarted() == nil {
			continue
		}
		opened = true
		if message.GetMessageId() != "dw:agent-99" {
			t.Fatalf("grandchild card = %q, want %q", message.GetMessageId(), "dw:agent-99")
		}
		if message.GetParent().GetRoot() == nil {
			t.Fatal("a grandchild card is not a feed row, so it owns no page slot")
		}
	}
	if !opened {
		t.Fatal("a nested launch inside a sidechain opened no card")
	}
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
	if len(entries) != 1 || entries[0].GetInternal().GetUnparsed() == nil {
		t.Fatalf("entries = %#v, want one unparsed record", entries)
	}
}
