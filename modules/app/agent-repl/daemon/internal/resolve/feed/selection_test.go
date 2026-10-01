package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// completedWith is the success terminal that names its answering response unit —
// the frame that draws the green final-answer border on that row.
func completedWith(unit string) *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{
			Answer: &conversationv1.AgentActivityId{Value: unit},
		}},
	}
}

// concludeAnswer draws one settled response and concludes its turn on it, so
// the row becomes a selectable final response.
func (h *harness) concludeAnswer(turn, unit, markdown string) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame(unit, &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
		}, nil), nil, nil)
	id := ids.TurnID(turn)
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &id, completedWith(unit), nil, nil)
}

// TestFinalResponsesAreOrderedOldestFirst pins that concluded answers accrue in
// conclusion order, which is root-feed order, so the last element is the most
// recent.
func TestFinalResponsesAreOrderedOldestFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.concludeAnswer("turn-1", "unit-1", "first answer")
	h.concludeAnswer("turn-2", "unit-2", "second answer")

	// Assert.
	got := h.resolver.FinalResponses(testWorkspace)
	if len(got) != 2 {
		t.Fatalf("final responses = %d, want 2", len(got))
	}
	first := got[0].GetValue()
	last := got[1].GetValue()
	if md, ok := selectableTextOf(h, testWorkspace, got[0]); !ok || md != "first answer" {
		t.Fatalf("markdown[%s] = (%q, %v), want (\"first answer\", true)", first, md, ok)
	}
	if md, ok := selectableTextOf(h, testWorkspace, got[1]); !ok || md != "second answer" {
		t.Fatalf("markdown[%s] = (%q, %v), want (\"second answer\", true)", last, md, ok)
	}
}

// TestFinalResponsesAreDedupedAcrossReplayedTerminals pins that a terminal
// replayed across planes appends its answer ONCE.
func TestFinalResponsesAreDedupedAcrossReplayedTerminals(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.concludeAnswer("turn-1", "unit-1", "the answer")

	// Act: the same terminal arrives again (the file plane after the stream).
	id := ids.TurnID("turn-1")
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &id, completedWith("unit-1"), nil, nil)

	// Assert.
	if got := h.resolver.FinalResponses(testWorkspace); len(got) != 1 {
		t.Fatalf("final responses = %d, want 1 after a replayed terminal", len(got))
	}
}

// TestFinalResponsesEmptyForAWorkspaceWithNoConclusions pins that a workspace
// with no concluded answers answers an empty set, never an error.
func TestFinalResponsesEmptyForAWorkspaceWithNoConclusions(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	got := h.resolver.FinalResponses(testWorkspace)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("final responses = %d, want none", len(got))
	}
}

// TestSelectableMarkdownMissesAnUnselectableFeedid pins that a feedid naming no
// final-response row answers not-selectable, which is the submit path's cue to
// refuse rather than deliver an empty prefix.
func TestSelectableMarkdownMissesAnUnselectableFeedid(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.concludeAnswer("turn-1", "unit-1", "the answer")

	// Act.
	_, ok := selectableTextOf(h, testWorkspace, &frontendv1.FeedId{Value: "not-a-final"})

	// Assert.
	if ok {
		t.Fatalf("SelectableMarkdown reported selectable for a feedid that names no final response")
	}
}

// TestConcludingATurnStampsItsAnswerRowFinal pins the LIVE green: when a turn
// concludes on an answering response, that response ROW carries
// final_answer=true, so the client draws the green border from the row's data
// rather than from a live turn-ended event. The response is drawn before the
// terminal names it, so this proves the terminal re-stamps the already-drawn
// row.
func TestConcludingATurnStampsItsAnswerRowFinal(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.concludeAnswer("turn-1", "unit-1", "the answer")

	// Assert.
	rows := h.responseRows()
	if len(rows) != 1 {
		t.Fatalf("response rows = %d, want 1", len(rows))
	}
	if !rows[0].GetActivity().GetResponse().GetFinalAnswer() {
		t.Fatal("the concluded answer row was not stamped final_answer=true")
	}
}

// TestABackgroundedConclusionStampsNoAnswerRow pins that a turn concluding with
// NO answering response (backgrounded) leaves every response row unstamped —
// the flag names exactly the row the conclusion pointed at.
func TestABackgroundedConclusionStampsNoAnswerRow(t *testing.T) {
	// Arrange: an ordinary response, then a conclusion that names no answer.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "some prose"},
		}, nil), nil, nil)
	id := ids.TurnID("turn-1")

	// Act: conclude with backgrounded (no answer unit).
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &id, &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Backgrounded{Backgrounded: &conversationv1.AgentBackgrounded{}},
	}, nil, nil)

	// Assert.
	rows := h.responseRows()
	if len(rows) != 1 {
		t.Fatalf("response rows = %d, want 1", len(rows))
	}
	if rows[0].GetActivity().GetResponse().GetFinalAnswer() {
		t.Fatal("a response was stamped final_answer with no conclusion naming it")
	}
}

// selectableTextOf answers SelectableText's markdown and ok, the pair most
// assertions compare.
func selectableTextOf(h *harness, ws ids.WorkspaceID, id *frontendv1.FeedId) (string, bool) {
	h.t.Helper()
	text, ok := h.resolver.SelectableText(ws, id)
	return text.Markdown, ok
}

// TestSelectableTextSaysWhetherTheRowIsAPrompt pins the speaker a reply quotes.
func TestSelectableTextSaysWhetherTheRowIsAPrompt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "what is the capital?")
	h.concludeAnswer("turn-1", "unit-1", "Paris")

	// Act.
	prompt, _ := h.resolver.SelectableText(testWorkspace, &frontendv1.FeedId{Value: h.promptRowID("turn-1")})
	answer, _ := h.resolver.SelectableText(testWorkspace, &frontendv1.FeedId{Value: h.responseRowID("unit-1")})

	// Assert.
	if !prompt.Prompt || answer.Prompt {
		t.Fatalf("prompt.Prompt = %v, answer.Prompt = %v; want true, false", prompt.Prompt, answer.Prompt)
	}
}

// promptRowWith is a user-prompt row carrying TEXTS as text blocks.
func promptRowWith(texts ...string) *frontendv1.FeedRow {
	blocks := make([]*frontendv1.FeedUserPromptBlock, 0, len(texts))
	for _, text := range texts {
		blocks = append(blocks, &frontendv1.FeedUserPromptBlock{
			Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: text}},
		})
	}
	return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
		Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
			Body: &frontendv1.FeedUserPromptBody{Blocks: blocks},
		}},
	}}}
}

// agentPromptRowWith is an agent-prompt row carrying TEXTS as text blocks.
func agentPromptRowWith(texts ...string) *frontendv1.FeedRow {
	blocks := make([]*frontendv1.FeedAgentPromptBlock, 0, len(texts))
	for _, text := range texts {
		blocks = append(blocks, &frontendv1.FeedAgentPromptBlock{
			Block: &frontendv1.FeedAgentPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: text}},
		})
	}
	return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
		Body: &frontendv1.FeedAgentPromptBody{Blocks: blocks},
	}}}
}

// responseRowWith is a response row in RESPONSE's state.
func responseRowWith(response *frontendv1.FeedResponse) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
		Unit: &frontendv1.FeedTurnActivity_Response{Response: response},
	}}}
}

// settled is a response's success arm carrying MARKDOWN.
func settled(markdown string) *frontendv1.FeedResponse_Success {
	return &frontendv1.FeedResponse_Success{Success: &frontendv1.FeedResponseSuccess{
		Prose: &frontendv1.FeedResponseProse{Markdown: markdown},
	}}
}

// TestSelectableMarkdownIsTheOneRule pins which rows can be selected and what
// each says: prompts and landed, non-notice response bubbles, nothing else.
func TestSelectableMarkdownIsTheOneRule(t *testing.T) {
	tests := []struct {
		name       string
		row        *frontendv1.FeedRow
		wantOK     bool
		wantMarkup string
	}{
		{name: "a user prompt joins its text blocks", row: promptRowWith("first", "second"), wantOK: true, wantMarkup: "first\n\nsecond"},
		{name: "an agent prompt joins its text blocks", row: agentPromptRowWith("go look"), wantOK: true, wantMarkup: "go look"},
		{name: "an image-only prompt is selectable with no text", row: promptRowWith(), wantOK: true, wantMarkup: ""},
		{name: "a final response", row: responseRowWith(&frontendv1.FeedResponse{Result: settled("the answer"), FinalAnswer: true}), wantOK: true, wantMarkup: "the answer"},
		{name: "an interim response", row: responseRowWith(&frontendv1.FeedResponse{Result: settled("meanwhile")}), wantOK: true, wantMarkup: "meanwhile"},
		{name: "a thinking response", row: responseRowWith(&frontendv1.FeedResponse{Result: settled("pondering"), Thinking: true}), wantOK: true, wantMarkup: "pondering"},
		{name: "a response still arriving", row: responseRowWith(&frontendv1.FeedResponse{Result: &frontendv1.FeedResponse_Update{Update: &frontendv1.FeedResponseUpdate{
			Prose: &frontendv1.FeedResponseProse{Markdown: "partial"},
		}}}), wantOK: false},
		{name: "a broken response", row: responseRowWith(&frontendv1.FeedResponse{Result: &frontendv1.FeedResponse_Error{Error: &frontendv1.FeedResponseError{
			Prose: &frontendv1.FeedResponseProse{Markdown: "cut"},
		}}}), wantOK: false},
		{name: "a vendor notice", row: responseRowWith(&frontendv1.FeedResponse{Result: settled("interrupted"), Notice: &frontendv1.FeedResponseNotice{}}), wantOK: false},
		{name: "a tool call", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_SimpleToolCall{SimpleToolCall: &frontendv1.FeedSimpleToolCall{}},
		}}}, wantOK: false},
		{name: "a separation divider", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Separation{Separation: &frontendv1.FeedSessionSeparation{}}}, wantOK: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, ok := selectableMarkdown(tc.row)

			// Assert.
			if ok != tc.wantOK || got != tc.wantMarkup {
				t.Fatalf("selectableMarkdown = (%q, %v), want (%q, %v)", got, ok, tc.wantMarkup, tc.wantOK)
			}
		})
	}
}

// TestStampSelectableMarksOnlyRootRows pins that the stamp follows the one
// rule on the root feed and is cleared everywhere else.
func TestStampSelectableMarksOnlyRootRows(t *testing.T) {
	tests := []struct {
		name string
		feed feedid.Feed
		row  *frontendv1.FeedRow
		want bool
	}{
		{name: "a root prompt is stamped", feed: rootFeed(), row: promptRowWith("hi"), want: true},
		{name: "a sub-feed prompt is not", feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "sub"}}, row: promptRowWith("hi"), want: false},
		{name: "a root streaming response is not", feed: rootFeed(), row: responseRowWith(&frontendv1.FeedResponse{
			Result: &frontendv1.FeedResponse_Update{Update: &frontendv1.FeedResponseUpdate{}},
		}), want: false},
		{name: "a stale stamp on an unselectable row is cleared", feed: rootFeed(), row: func() *frontendv1.FeedRow {
			row := responseRowWith(&frontendv1.FeedResponse{Result: &frontendv1.FeedResponse_Error{Error: &frontendv1.FeedResponseError{}}})
			row.Selectable = &frontendv1.FeedRowSelectable{}
			return row
		}(), want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			stampSelectable(tc.feed, tc.row)

			// Assert.
			if got := tc.row.GetSelectable() != nil; got != tc.want {
				t.Fatalf("selectable = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestAPromptRowIsPublishedSelectable pins the stamp on the real publication
// path: a delivered prompt's root row carries FeedRow.selectable.
func TestAPromptRowIsPublishedSelectable(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.deliverPrompt("turn-1", "what is the capital?")

	// Assert.
	if h.promptRow("turn-1").GetSelectable() == nil {
		t.Fatalf("the prompt row was published without selectable")
	}
}

// TestAResponseBecomesSelectableWhenItSettles pins that a streaming bubble is
// not selectable and gains the stamp on the publication that settles it.
func TestAResponseBecomesSelectableWhenItSettles(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "partial"}, nil), nil, nil)
	streaming := h.rowByID(rootFeed(), h.responseRowID("unit-1"))
	if streaming.GetSelectable() != nil {
		t.Fatalf("a streaming response was published selectable")
	}

	// Act.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "partial and done"},
		}, nil), nil, nil)

	// Assert.
	if h.rowByID(rootFeed(), h.responseRowID("unit-1")).GetSelectable() == nil {
		t.Fatalf("the settled response was published without selectable")
	}
}

// TestSelectableMarkdownReadsAPromptRow pins the lookup the selection and the
// reply path share, for a kind that is not a final response.
func TestSelectableMarkdownReadsAPromptRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "what is the capital?")

	// Act.
	md, ok := selectableTextOf(h, testWorkspace, &frontendv1.FeedId{Value: h.promptRowID("turn-1")})

	// Assert.
	if !ok || md != "what is the capital?" {
		t.Fatalf("SelectableMarkdown = (%q, %v), want the prompt's text", md, ok)
	}
}

// TestSelectableMarkdownMissesAStreamingResponse pins that a row the resolver
// published unselectable reads as not selectable.
func TestSelectableMarkdownMissesAStreamingResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "partial"}, nil), nil, nil)

	// Act.
	_, ok := selectableTextOf(h, testWorkspace, &frontendv1.FeedId{Value: h.responseRowID("unit-1")})

	// Assert.
	if ok {
		t.Fatalf("a streaming response read as selectable")
	}
}
