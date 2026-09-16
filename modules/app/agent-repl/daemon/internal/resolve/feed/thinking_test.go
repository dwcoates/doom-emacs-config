package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THINKING DRAWS ITS OWN BUBBLE: intermediate reasoning, one bubble per block,
// reusing the response bubble on the `thinking` flag so the client draws it
// purple, non-bordered, and never green.

// thinkingResultFrame builds one reasoning frame carrying the given result arm.
func thinkingResultFrame(unit string, result any) *conversationv1.AgentActivity {
	block := &conversationv1.AgentThinking{}
	switch r := result.(type) {
	case *conversationv1.AgentThinkingStart:
		block.Result = &conversationv1.AgentThinking_Start{Start: r}
	case *conversationv1.AgentThinkingUpdate:
		block.Result = &conversationv1.AgentThinking_Update{Update: r}
	case *conversationv1.AgentThinkingSuccess:
		block.Result = &conversationv1.AgentThinking_Success{Success: r}
	case *conversationv1.AgentThinkingFailure:
		block.Result = &conversationv1.AgentThinking_Failure{Failure: r}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item:       &conversationv1.AgentActivity_Thinking{Thinking: block},
	}
}

// thinkingTextDelta is a reasoning update carrying a fragment of text.
func thinkingTextDelta(text string) *conversationv1.AgentThinkingUpdate {
	return &conversationv1.AgentThinkingUpdate{
		Reasoning: &conversationv1.AgentThinkingUpdate_Text{
			Text: &conversationv1.AgentThinkingTextDelta{NewText: text},
		},
	}
}

// thinkingRows answers every thinking bubble on the root feed.
func (h *harness) thinkingRows() []*frontendv1.FeedResponse {
	h.t.Helper()
	var out []*frontendv1.FeedResponse
	for _, row := range h.rows(rootFeed()) {
		if resp := row.GetActivity().GetResponse(); resp.GetThinking() {
			out = append(out, resp)
		}
	}
	return out
}

// A REASONING BLOCK DRAWS A THINKING BUBBLE.
func TestAThinkingBlockDrawsABubble(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: a reasoning block opens and streams a fragment.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", thinkingTextDelta("weighing options")), noAddress())

	// Assert: one thinking bubble, marked as thinking.
	got := h.thinkingRows()
	if len(got) != 1 {
		t.Fatalf("thinking rows = %d, want 1", len(got))
	}
	if !got[0].GetThinking() {
		t.Fatalf("thinking flag = false, want true")
	}
}

// TEXT DELTAS ACCUMULATE, exactly as the prose fold accumulates a response.
func TestThinkingTextDeltasAccumulate(t *testing.T) {
	// Arrange: a reasoning block has started.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", &conversationv1.AgentThinkingStart{}), noAddress())

	// Act: two fragments arrive.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", thinkingTextDelta("first ")), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", thinkingTextDelta("second")), noAddress())

	// Assert: the bubble holds both fragments folded into one.
	got := h.thinkingRows()
	if len(got) != 1 {
		t.Fatalf("thinking rows = %d, want 1", len(got))
	}
	if md := got[0].GetUpdate().GetProse().GetMarkdown(); md != "first second" {
		t.Fatalf("thinking markdown = %q, want %q", md, "first second")
	}
}

// SUCCESS SETTLES THE BUBBLE with the whole reasoning text.
func TestThinkingSuccessSettlesTheBubble(t *testing.T) {
	// Arrange: a reasoning block streamed a fragment.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", thinkingTextDelta("partial")), noAddress())

	// Act: the block settles, restating the whole.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", &conversationv1.AgentThinkingSuccess{
			Reasoning: &conversationv1.AgentThinkingSuccess_Text{
				Text: &conversationv1.AgentThinkingText{Text: "the whole reasoning"},
			},
		}), noAddress())

	// Assert: the bubble is settled with the whole text.
	got := h.thinkingRows()
	if len(got) != 1 {
		t.Fatalf("thinking rows = %d, want 1", len(got))
	}
	if md := got[0].GetSuccess().GetProse().GetMarkdown(); md != "the whole reasoning" {
		t.Fatalf("settled thinking markdown = %q, want %q", md, "the whole reasoning")
	}
}

// A THINKING BUBBLE IS NEVER THE TURN'S CONCLUDED ANSWER: naming the reasoning
// unit as the answer names no row, so no green final-answer border is drawn.
func TestAThinkingBubbleIsNeverTheConcludedAnswer(t *testing.T) {
	// Arrange: a reasoning block settled with text.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", &conversationv1.AgentThinkingSuccess{
			Reasoning: &conversationv1.AgentThinkingSuccess_Text{
				Text: &conversationv1.AgentThinkingText{Text: "reasoning"},
			},
		}), noAddress())

	// Act: the turn concludes naming the thinking unit as the answer.
	h.terminal("turn-1", completedWith("unit-t"), nil)

	// Assert: the terminal names no answering row — a thinking row is never
	// filed as an answer, so the green treatment can never reach it.
	if answer := h.terminalRow("turn-1").GetConcluded().GetAnswer(); answer != nil {
		t.Fatalf("concluded answer = %v, want nil for a thinking unit", answer)
	}
}

// WITHHELD REASONING DRAWS NO BUBBLE: the model emits the block and a signature
// but no text, so there is nothing to draw but a footer indicator.
func TestWithheldThinkingDrawsNoBubble(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: a withheld reasoning update arrives.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", &conversationv1.AgentThinkingUpdate{
			Reasoning: &conversationv1.AgentThinkingUpdate_Withheld{
				Withheld: &conversationv1.AgentThinkingWithheld{},
			},
		}), noAddress())

	// Assert: no thinking bubble is drawn.
	if got := h.thinkingRows(); len(got) != 0 {
		t.Fatalf("thinking rows = %d, want none for withheld reasoning", len(got))
	}
}

// A WITHHELD BLOCK SETTLES WITHHELD, drawing nothing at all rather than an
// empty card.
func TestWithheldThinkingSettlesToNoBubble(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: a withheld reasoning block settles.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", &conversationv1.AgentThinkingSuccess{
			Reasoning: &conversationv1.AgentThinkingSuccess_Withheld{
				Withheld: &conversationv1.AgentThinkingWithheld{},
			},
		}), noAddress())

	// Assert: no thinking bubble is drawn.
	if got := h.thinkingRows(); len(got) != 0 {
		t.Fatalf("thinking rows = %d, want none for a settled withheld block", len(got))
	}
}

// A FAILED REASONING BLOCK KEEPS ITS PARTIAL TEXT, marked broken — the mirror
// of the prose bubble's error arm.
func TestThinkingFailureKeepsPartialReasoning(t *testing.T) {
	// Arrange: a reasoning block streamed a fragment.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", thinkingTextDelta("half a thought")), noAddress())

	// Act: the block fails.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", &conversationv1.AgentThinkingFailure{}), noAddress())

	// Assert: the partial reasoning stays drawn on the broken arm.
	got := h.thinkingRows()
	if len(got) != 1 {
		t.Fatalf("thinking rows = %d, want 1", len(got))
	}
	if md := got[0].GetError().GetProse().GetMarkdown(); md != "half a thought" {
		t.Fatalf("broken thinking markdown = %q, want %q", md, "half a thought")
	}
}

// A THINKING BUBBLE CARRIES NO USAGE STAMP: reasoning's cost is the footer's,
// and the thinking unit is frequently the sibling that carries the PROSE
// bubble's usage — so drawing a corner here would double the figure.
func TestAThinkingBubbleCarriesNoUsageStamp(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: a reasoning block streams, carrying usage on its envelope.
	act := thinkingResultFrame("unit-t", thinkingTextDelta("reasoning"))
	act.Usage = misses(18_000, 240)
	h.resolver.OnActivity(testWorkspace, mainAgent(), act, noAddress())

	// Assert: the thinking bubble draws no cost corner.
	got := h.thinkingRows()
	if len(got) != 1 {
		t.Fatalf("thinking rows = %d, want 1", len(got))
	}
	if usage := got[0].GetUsage(); usage != nil {
		t.Fatalf("thinking usage = %+v, want unset", usage)
	}
}

// A THINKING BLOCK AND A PROSE BLOCK OF THE SAME TURN DRAW SEPARATE BUBBLES,
// keyed on their distinct unit ids.
func TestThinkingAndResponseDrawSeparateBubbles(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: a reasoning block, then a prose block.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("unit-t", thinkingTextDelta("reasoning")), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-p", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "the answer"},
		}, nil), noAddress())

	// Assert: two response-shaped rows — one flagged thinking, one not.
	all := h.responseRows()
	if len(all) != 2 {
		t.Fatalf("response-shaped rows = %d, want 2 (one thinking, one prose)", len(all))
	}
	var thinkingCount, proseCount int
	for _, row := range all {
		if row.GetActivity().GetResponse().GetThinking() {
			thinkingCount++
		} else {
			proseCount++
		}
	}
	if thinkingCount != 1 || proseCount != 1 {
		t.Fatalf("thinking=%d prose=%d, want exactly one of each", thinkingCount, proseCount)
	}
}

// A WITHHELD REASONING BLOCK LEAVES NO EMPTY BUBBLE. The stream opens a thinking
// block with a Start (which draws an empty card) before the block reveals itself
// withheld at settle; the withheld settle must retire that card so the block
// draws nothing at all, and the turn's real prose is the only response row and
// takes the green final answer.
func TestAWithheldThinkingLeadingBlockLeavesOnlyTheGreenedProse(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: block :0 opens as thinking (empty card) then settles WITHHELD; block
	// :1 is the real prose; the turn concludes naming the prose as its answer.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", &conversationv1.AgentThinkingStart{}), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", &conversationv1.AgentThinkingSuccess{
			Reasoning: &conversationv1.AgentThinkingSuccess_Withheld{Withheld: &conversationv1.AgentThinkingWithheld{}},
		}), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("msg:1", &conversationv1.AgentResponseStart{}, nil), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("msg:1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "the real answer"},
		}, nil), noAddress())
	h.terminal("turn-1", completedWith("msg:1"), nil)

	// Assert: exactly one response-shaped row — the settled prose — and the
	// terminal's green final answer names that same row, not any empty leftover.
	rows := h.responseRows()
	if len(rows) != 1 {
		t.Fatalf("response-shaped rows = %d, want 1 (the prose only)", len(rows))
	}
	prose := rows[0]
	if prose.GetActivity().GetResponse().GetThinking() {
		t.Fatalf("the surviving row is flagged thinking; want the prose row")
	}
	if md := prose.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown(); md != "the real answer" {
		t.Fatalf("surviving prose = %q, want %q", md, "the real answer")
	}
	if answer := h.terminalRow("turn-1").GetConcluded().GetAnswer().GetValue(); answer != prose.GetId().GetValue() {
		t.Fatalf("green final answer = %q, want the prose row %q", answer, prose.GetId().GetValue())
	}
}

// A WITHHELD REASONING BLOCK BEFORE A TOOL CALL LEAVES ONLY THE TOOL CARD. The
// message opens with a withheld thinking block and then a tool_use; the empty
// thinking card its Start opened must be retired, so the tool card is the only
// row and no empty response bubble precedes it.
func TestAWithheldThinkingBlockBeforeAToolCallDrawsOnlyTheToolCard(t *testing.T) {
	// Arrange: a turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: block :0 opens as thinking then settles withheld; block :1 is a bash
	// tool call.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", &conversationv1.AgentThinkingStart{}), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", &conversationv1.AgentThinkingSuccess{
			Reasoning: &conversationv1.AgentThinkingSuccess_Withheld{Withheld: &conversationv1.AgentThinkingWithheld{}},
		}), noAddress())
	h.send(activityOf("msg:1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "go test ./..."},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))

	// Assert: no response-shaped bubble at all, and exactly one tool card.
	if rows := h.responseRows(); len(rows) != 0 {
		t.Fatalf("response-shaped rows = %d, want 0 (only the tool card)", len(rows))
	}
	var toolCards int
	for _, row := range h.rows(rootFeed()) {
		if row.GetActivity().GetSimpleToolCall() != nil {
			toolCards++
		}
	}
	if toolCards != 1 {
		t.Fatalf("tool cards = %d, want 1", toolCards)
	}
}

// A WITHHELD UPDATE RETIRES THE OPENED CARD TOO. A block can reveal itself
// withheld on the update arm (a withheld beat before the message settles), which
// must retire the Start arm's empty card exactly as the withheld settle does.
func TestAWithheldThinkingUpdateRetiresTheOpenedCard(t *testing.T) {
	// Arrange: a thinking block opened its empty card.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", &conversationv1.AgentThinkingStart{}), noAddress())

	// Act: a withheld update arrives.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", &conversationv1.AgentThinkingUpdate{
			Reasoning: &conversationv1.AgentThinkingUpdate_Withheld{Withheld: &conversationv1.AgentThinkingWithheld{}},
		}), noAddress())

	// Assert: no thinking bubble stands.
	if got := h.thinkingRows(); len(got) != 0 {
		t.Fatalf("thinking rows = %d, want 0 after a withheld update", len(got))
	}
}

// A SHOWN REASONING BLOCK IS NEVER RETIRED. The withheld retirement must not
// touch a block that actually carries reasoning text: an opened-then-settled
// shown block keeps its thinking bubble.
func TestAShownThinkingBlockSurvivesSettlement(t *testing.T) {
	// Arrange: a thinking block opened and streamed a fragment.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", &conversationv1.AgentThinkingStart{}), noAddress())
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", thinkingTextDelta("weighing options")), noAddress())

	// Act: the block settles with its whole reasoning text.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		thinkingResultFrame("msg:0", &conversationv1.AgentThinkingSuccess{
			Reasoning: &conversationv1.AgentThinkingSuccess_Text{
				Text: &conversationv1.AgentThinkingText{Text: "weighing options"},
			},
		}), noAddress())

	// Assert: the thinking bubble stands, settled with its text.
	got := h.thinkingRows()
	if len(got) != 1 {
		t.Fatalf("thinking rows = %d, want 1 (the shown block survives)", len(got))
	}
	if md := got[0].GetSuccess().GetProse().GetMarkdown(); md != "weighing options" {
		t.Fatalf("settled thinking markdown = %q, want %q", md, "weighing options")
	}
}
