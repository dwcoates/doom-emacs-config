package convert

// frames_test.go — the refusal for a frame that names no agent, and whose book a
// frame lands in.

import "testing"

func TestFrameWithNoAgentIsRefusedRatherThanServedToNoBook(t *testing.T) {
	// Arrange. The store reads the book FROM THE FRAME and never invents one, so
	// a frame naming no agent is a record that cannot be served — an invariant
	// violation, reported loudly rather than emitted against a guess.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.AgentID = ""
	at.MainAgentID = ""

	// Act.
	entries := c.Line(decode(t, assistantWith("a1", "msg_1", ts1, `{"type":"text","text":"x"}`)), at, nil)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	unknown := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown == nil || unknown.GetDiscriminator() != "unattributed_frame" {
		t.Fatalf("want an unattributed_frame refusal, got %v", unknown)
	}
	if pageLine(entries[0]) != nil {
		t.Fatal("an unattributed record must not be a page line")
	}
}

func TestTopLevelIsTheMainAgentForASubagentsWork(t *testing.T) {
	// Arrange. A sync subagent's work belongs to the turn's main agent: top_level
	// names the nearest NON-SYNC ancestor.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.MainAgentID = "main-uuid"
	at.AgentID = "sub-abc"
	at.Backgrounded = false
	line := `{"type":"assistant","uuid":"a1","isSidechain":true,"agentId":"sub-abc","timestamp":"` + ts1 +
		`","message":{"id":"msg_1","role":"assistant","content":[{"type":"text","text":"x"}]}}`

	// Act.
	entries := c.Line(decode(t, line), at, nil)

	// Assert.
	if got := pageLine(entries[0]).GetPageAgentId().GetValue(); got != "sub-abc" {
		t.Fatalf("page_agent_id = %q: a subagent's constituents form its OWN book", got)
	}
	if got := entries[0].GetAgentUpdate().GetTopLevel().GetValue(); got != "main-uuid" {
		t.Fatalf("top_level = %q, want the session's main agent", got)
	}
}

func TestBackgroundedSubagentIsItsOwnTopLevel(t *testing.T) {
	// Arrange. A backgrounded spawn's stream OUTLIVES the turn that started it,
	// so it is its own nearest non-sync ancestor.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.MainAgentID = "main-uuid"
	at.AgentID = "sub-abc"
	at.Backgrounded = true
	line := `{"type":"assistant","uuid":"a1","isSidechain":true,"agentId":"sub-abc","timestamp":"` + ts1 +
		`","message":{"id":"msg_1","role":"assistant","content":[{"type":"text","text":"x"}]}}`

	// Act.
	entries := c.Line(decode(t, line), at, nil)

	// Assert.
	if got := entries[0].GetAgentUpdate().GetTopLevel().GetValue(); got != "sub-abc" {
		t.Fatalf("top_level = %q, want the backgrounded agent itself", got)
	}
}

func quote(s string) string {
	out := []byte{'"'}
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '"', '\\':
			out = append(out, '\\', s[i])
		case '\n':
			out = append(out, '\\', 'n')
		default:
			out = append(out, s[i])
		}
	}
	return string(append(out, '"'))
}
