package footer

import (
	"strings"
	"testing"
	"unicode/utf8"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
)

// thinkingDelta is one streamed fragment of reasoning text.
func thinkingDelta(unit, text string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
			Result: &conversationv1.AgentThinking_Update{Update: &conversationv1.AgentThinkingUpdate{
				Reasoning: &conversationv1.AgentThinkingUpdate_Text{Text: &conversationv1.AgentThinkingTextDelta{NewText: text}}}}}},
	}
}

// thinkingWithheld is one streamed frame of withheld reasoning.
func thinkingWithheld(unit string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
			Result: &conversationv1.AgentThinking_Update{Update: &conversationv1.AgentThinkingUpdate{
				Reasoning: &conversationv1.AgentThinkingUpdate_Withheld{Withheld: &conversationv1.AgentThinkingWithheld{}}}}}},
	}
}

// thinkingSettled is a reasoning unit's settle carrying its whole text.
func thinkingSettled(unit, text string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
			Result: &conversationv1.AgentThinking_Success{Success: &conversationv1.AgentThinkingSuccess{
				Reasoning: &conversationv1.AgentThinkingSuccess_Text{Text: &conversationv1.AgentThinkingText{Text: text}}}}}},
	}
}

// responseDelta is one streamed fragment of prose markdown.
func responseDelta(unit, markdown string) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Update{Update: &conversationv1.AgentResponseUpdate{NewMarkdown: markdown}}}},
	}
}

// raisedCount counts the transients the resolver raised, the throttle's
// observable.
func raisedCount(h *harness) int {
	return len(recordsOf(h.log.Records(), "daemon.footer.transient_raised"))
}

// inBareTurn opens a turn with no prompt text, so no submitting line is raised.
func inBareTurn(h *harness) {
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
}

func TestAReasoningFragmentWithNoCompleteLineRaisesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", "Let me look at the"))

	// Assert
	if got := transientOf(t, h); got != nil {
		t.Fatalf("transient = %+v, want none while the line is in progress", got)
	}
}

func TestACompletedReasoningLineRaisesTheThinkingTail(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", "Let me look at the"))

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", " resolver first.\nThen"))

	// Assert
	if got := transientOf(t, h).GetThinking().GetText().GetTail(); got != "Let me look at the resolver first." {
		t.Fatalf("thinking tail = %q, want the completed line", got)
	}
}

func TestAFragmentThatOnlyExtendsTheLineInProgressRaisesNothingNew(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", "First line.\nSecond"))
	before := raisedCount(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", " line keeps going"))

	// Assert
	if after := raisedCount(h); after != before {
		t.Fatalf("%d transients raised by a fragment that completed no line, want 0", after-before)
	}
}

func TestTheSettleRaisesTheFinalLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", "First line.\nThe last"))

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingSettled("th-1", "First line.\nThe last thought."))

	// Assert
	if got := transientOf(t, h).GetThinking().GetText().GetTail(); got != "The last thought." {
		t.Fatalf("thinking tail = %q, want the final line no newline completed", got)
	}
}

func TestASettleRepeatingTheRaisedLineRaisesNothingNew(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", "Only line.\n"))
	before := raisedCount(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingSettled("th-1", "Only line.\n"))

	// Assert
	if after := raisedCount(h); after != before {
		t.Fatalf("%d transients raised by a settle that changed no line, want 0", after-before)
	}
}

func TestWithheldReasoningRaisesTheWithheldLineOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, thinkingWithheld("th-1"))
	before := raisedCount(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingWithheld("th-1"))

	// Assert
	if transientOf(t, h).GetThinking().GetWithheld() == nil {
		t.Fatalf("transient = %+v, want the withheld thinking line", transientOf(t, h))
	}
	if after := raisedCount(h); after != before {
		t.Fatalf("%d transients raised by a second withheld frame, want 0", after-before)
	}
}

func TestAResponseLineIsDrawnAsPlainText(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, responseDelta("r-1", "## Plan\n- read **the** [resolver](a.go) `code`\n"))

	// Assert
	if got := transientOf(t, h).GetResponse().GetTail(); got != "read the resolver code" {
		t.Fatalf("response tail = %q, want the markup removed", got)
	}
}

func TestALongLineIsCappedFromItsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	line := strings.Repeat("a", 200) + "THE END"

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", line+"\n"))

	// Assert
	got := transientOf(t, h).GetThinking().GetText().GetTail()
	if utf8.RuneCountInString(got) != DefaultWarningRowWidth || !strings.HasSuffix(got, "THE END") || !strings.HasPrefix(got, "…") {
		t.Fatalf("tail = %q (%d runes), want the last %d runes behind an ellipsis", got, utf8.RuneCountInString(got), DefaultWarningRowWidth)
	}
}

func TestTheMainTurnsTerminalDropsEveryStreamingTail(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, thinkingDelta("th-1", "cut short"))

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, ptr(ids.TurnID("turn-1")), completed(), nil)

	// Assert
	if n := len(h.r.states[testWS].streams); n != 0 {
		t.Fatalf("%d tails kept past the turn's end, want none", n)
	}
}

func TestAStreamingUnitsTailIsBounded(t *testing.T) {
	// Arrange
	tail := &streamTail{}

	// Act
	tail.append(strings.Repeat("é", streamKeep))

	// Assert
	if len(tail.text) > streamKeep || !utf8.ValidString(tail.text) {
		t.Fatalf("tail holds %d bytes (valid utf-8: %v), want at most %d cut at a rune boundary",
			len(tail.text), utf8.ValidString(tail.text), streamKeep)
	}
}

func TestPlainMarkdownRendersOneLine(t *testing.T) {
	tests := []struct {
		name string
		line string
		want string
	}{
		{name: "a heading", line: "### The plan", want: "The plan"},
		{name: "a quote", line: "> quoted", want: "quoted"},
		{name: "a bullet", line: "* item", want: "item"},
		{name: "a checklist item", line: "- [x] done", want: "done"},
		{name: "a numbered item", line: "2. second", want: "second"},
		{name: "a code fence", line: "```go", want: ""},
		{name: "a rule", line: "---", want: ""},
		{name: "an image", line: "see ![the chart](c.png)", want: "see the chart"},
		{name: "a link", line: "read [the doc](d.md) now", want: "read the doc now"},
		{name: "emphasis and code", line: "**bold** __strong__ ~~gone~~ `x`", want: "bold strong gone x"},
		{name: "plain prose", line: "  just words  ", want: "just words"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got := plainMarkdown(tt.line)

			// Assert
			if got != tt.want {
				t.Fatalf("plainMarkdown(%q) = %q, want %q", tt.line, got, tt.want)
			}
		})
	}
}

func TestCompleteLastLineReadsOnlyCompletedLines(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "no newline yet", text: "in progress", want: ""},
		{name: "one completed line", text: "done\nin progress", want: "done"},
		{name: "blank lines skipped", text: "done\n\n\nin progress", want: "done"},
		{name: "the latest completed line", text: "one\ntwo\n", want: "two"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got := completeLastLine(tt.text, plainReasoning)

			// Assert
			if got != tt.want {
				t.Fatalf("completeLastLine(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestTailOfLeavesAShortLineAlone(t *testing.T) {
	// Arrange, Act
	got := tailOf("short", 10)

	// Assert
	if got != "short" {
		t.Fatalf("tailOf = %q, want the line unchanged", got)
	}
}
