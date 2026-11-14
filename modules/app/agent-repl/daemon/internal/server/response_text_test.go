package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/revision"
	"agent-repld/internal/workspace"
)

// withRevisable seeds row-1 as a final response a highlight may name.
func withRevisable(h *harness) {
	h.Feed.revisable = map[string]feed.RevisableResponse{"row-1": {Unit: "msg_1:0", Markdown: "1 🔎 the answer"}}
}

// highlight holds WORDS of ROW as the workspace's highlight.
func highlight(t *testing.T, h *harness, row, words string) *agentreplv1.SelectResponseTextResponse {
	t.Helper()
	resp, err := h.Client.SelectResponseText(context.Background(), connect.NewRequest(&agentreplv1.SelectResponseTextRequest{
		Workspace: ref(),
		Selection: &agentreplv1.SelectResponseTextSelection{State: &agentreplv1.SelectResponseTextSelection_Highlighted{
			Highlighted: &agentreplv1.SelectResponseTextHighlighted{Row: &frontendv1.FeedId{Value: row}, Words: words},
		}},
	}))
	if err != nil {
		t.Fatalf("SelectResponseText: %v", err)
	}
	return resp.Msg
}

// clearHighlight clears the workspace's highlight.
func clearHighlight(t *testing.T, h *harness) {
	t.Helper()
	if _, err := h.Client.SelectResponseText(context.Background(), connect.NewRequest(&agentreplv1.SelectResponseTextRequest{
		Workspace: ref(),
		Selection: &agentreplv1.SelectResponseTextSelection{State: &agentreplv1.SelectResponseTextSelection_Cleared{Cleared: &agentreplv1.SelectResponseTextCleared{}}},
	})); err != nil {
		t.Fatalf("SelectResponseText: %v", err)
	}
}

func TestNewRefusesAMissingReviser(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.Revisions = nil })

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the response reviser") {
		t.Fatalf("New = %v, want the refusal naming the response reviser", err)
	}
}

func TestAPromptSentWhileWordsAreHighlightedRevisesTheResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	highlight(t, h, "row-1", "entropy")

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetResponseRevising().GetRow().GetValue(); got != "row-1" {
		t.Fatalf("SubmitPrompt answered %v, want response_revising naming row-1", resp.Msg)
	}
	asks := h.Revisions.asks
	if len(asks) != 1 || asks[0].Row.GetValue() != "row-1" || asks[0].Words != "entropy" || promptText(asks[0].Said) != "hello" {
		t.Fatalf("revisions asked = %+v, want row-1's entropy asked about with hello", asks)
	}
}

func TestARevisingPromptMintsNoTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	highlight(t, h, "row-1", "entropy")

	// Act.
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest())); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if h.Prompts.submits != 0 {
		t.Fatalf("the prompt handler took %d submissions, want none", h.Prompts.submits)
	}
}

func TestTheRevisingPromptTakesTheHighlight(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	h.Prompts.outcome = prompthandler.Outcome{Turn: "turn-2"}
	highlight(t, h, "row-1", "entropy")
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest())); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil || resp.Msg.GetSuccess().GetTurn().GetTurn().GetValue() != "turn-2" {
		t.Fatalf("the next prompt answered %v (%v), want an ordinary turn", resp.Msg, err)
	}
}

func TestAClearedHighlightLeavesThePromptATurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	h.Prompts.outcome = prompthandler.Outcome{Turn: "turn-2"}
	highlight(t, h, "row-1", "entropy")
	clearHighlight(t, h)

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil || resp.Msg.GetSuccess().GetTurn().GetTurn().GetValue() != "turn-2" || len(h.Revisions.asks) != 0 {
		t.Fatalf("SubmitPrompt answered %v (%v) with revisions %v, want an ordinary turn", resp.Msg, err, h.Revisions.asks)
	}
}

func TestAnAgentsPromptLeavesTheHighlightStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	sentBy := registerSender(h)
	h.Prompts.outcome = prompthandler.Outcome{Turn: "turn-2"}
	highlight(t, h, "row-1", "entropy")

	// Act.
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: ref(), Said: workspace.SaidText("from an agent"), IdempotencyKey: "k-2",
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, SentBy: sentBy})); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil || resp.Msg.GetSuccess().GetResponseRevising() == nil {
		t.Fatalf("the user's prompt after the agent's answered %v (%v), want the revision", resp.Msg, err)
	}
	if h.Prompts.submits != 1 {
		t.Fatalf("the prompt handler took %d submissions, want the agent's one", h.Prompts.submits)
	}
}

func TestARevisingPromptWhoseResponseIsGoneIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	highlight(t, h, "row-1", "entropy")
	h.Revisions.err = revision.ErrNotRevisable

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert: the reply path's own refusal, which no landed arm carries, so it
	// surfaces as NotFound.
	if connectCode(t, err) != connect.CodeNotFound {
		t.Fatalf("code = %v, want NotFound for a highlighted response that is gone", connectCode(t, err))
	}
	if h.Prompts.submits != 0 {
		t.Fatal("a refused revision was delivered as a turn")
	}
}

func TestARevisionThatCannotBeQueuedFailsTheSubmission(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	highlight(t, h, "row-1", "entropy")
	h.Revisions.err = errors.New("the lane is gone")

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err == nil {
		t.Fatal("a revision that could not be queued answered success")
	}
}

func TestHighlightingARowThatIsNoFinalResponseIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)

	// Act.
	resp := highlight(t, h, "row-9", "entropy")

	// Assert.
	if resp.GetError().GetNotAFinalResponse() == nil {
		t.Fatalf("SelectResponseText answered %v, want not_a_final_response", resp)
	}
}

func TestARefusedHighlightHoldsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	h.Prompts.outcome = prompthandler.Outcome{Turn: "turn-2"}
	highlight(t, h, "row-9", "entropy")

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil || resp.Msg.GetSuccess().GetTurn() == nil {
		t.Fatalf("SubmitPrompt answered %v (%v), want an ordinary turn", resp.Msg, err)
	}
}

func TestSelectResponseTextRefusesAMalformedSelection(t *testing.T) {
	tests := []struct {
		name      string
		selection *agentreplv1.SelectResponseTextSelection
	}{
		{name: "no state", selection: &agentreplv1.SelectResponseTextSelection{}},
		{name: "no row", selection: &agentreplv1.SelectResponseTextSelection{State: &agentreplv1.SelectResponseTextSelection_Highlighted{
			Highlighted: &agentreplv1.SelectResponseTextHighlighted{Words: "entropy"},
		}}},
		{name: "blank words", selection: &agentreplv1.SelectResponseTextSelection{State: &agentreplv1.SelectResponseTextSelection_Highlighted{
			Highlighted: &agentreplv1.SelectResponseTextHighlighted{Row: &frontendv1.FeedId{Value: "row-1"}, Words: " \n "},
		}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.SelectResponseText(context.Background(), connect.NewRequest(&agentreplv1.SelectResponseTextRequest{
				Workspace: ref(), Selection: tt.selection,
			}))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("SelectResponseText = %v, want InvalidArgument", err)
			}
		})
	}
}

// TestSelectResponseTextAsksTheHostToFocusTheComposerOnlyForAHighlight pins
// which selections push response_text_highlighted: only a highlight the
// daemon takes. Each selection is followed by a notification click, which
// must then be the first event the host stream carries when no focus was
// asked, so a stray response_text_highlighted shows as the wrong first event.
func TestSelectResponseTextAsksTheHostToFocusTheComposerOnlyForAHighlight(t *testing.T) {
	tests := []struct {
		name      string
		selection *agentreplv1.SelectResponseTextSelection
		wantFocus bool
	}{
		{name: "a highlight", wantFocus: true, selection: &agentreplv1.SelectResponseTextSelection{State: &agentreplv1.SelectResponseTextSelection_Highlighted{
			Highlighted: &agentreplv1.SelectResponseTextHighlighted{Row: &frontendv1.FeedId{Value: "row-1"}, Words: "entropy"},
		}}},
		{name: "a refused highlight", selection: &agentreplv1.SelectResponseTextSelection{State: &agentreplv1.SelectResponseTextSelection_Highlighted{
			Highlighted: &agentreplv1.SelectResponseTextHighlighted{Row: &frontendv1.FeedId{Value: "row-9"}, Words: "entropy"},
		}}},
		{name: "a clear", selection: &agentreplv1.SelectResponseTextSelection{State: &agentreplv1.SelectResponseTextSelection_Cleared{
			Cleared: &agentreplv1.SelectResponseTextCleared{},
		}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			withRevisable(h)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := dialHostStream(t, h, ctx)

			// Act.
			if _, err := h.Client.SelectResponseText(context.Background(), connect.NewRequest(&agentreplv1.SelectResponseTextRequest{
				Workspace: ref(), Selection: tc.selection,
			})); err != nil {
				t.Fatalf("SelectResponseText: %v", err)
			}
			h.Server.NotificationClicked(testWorkspaceID)
			first := receiveHostEventPastSelection(t, stream)

			// Assert.
			if got := first.GetResponseTextHighlighted() != nil; got != tc.wantFocus {
				t.Fatalf("first event = %v, want response_text_highlighted %v", first, tc.wantFocus)
			}
		})
	}
}

// receiveHighlight reads the watch stream until a response-highlight frame arrives.
func receiveHighlight(t *testing.T, stream *connect.ServerStreamForClient[agentreplv1.WatchFeedResponse]) *frontendv1.FeedResponseHighlight {
	t.Helper()
	return receivePush(t, stream, "a response highlight", (*agentreplv1.WatchFeedResponse).GetResponseHighlight)
}

// wordsOf answers WORDS' row and words, "" for none.
func wordsOf(words *frontendv1.FeedResponseHighlightWords) (string, string) {
	return words.GetRow().GetValue(), words.GetWords()
}

func TestARootWatchOfADaemonHoldingNoHighlightIsToldNone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	got := receiveHighlight(t, openRootWatch(t, h, ctx))

	// Assert.
	if got.GetNone() == nil {
		t.Fatalf("frame = %v, want none: a page whose daemon restarted must learn it holds nothing", got)
	}
}

func TestTheRootFeedWatchStatesEachHighlightChange(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	receiveHighlight(t, stream)

	// Act.
	highlight(t, h, "row-1", "entropy")
	held := receiveHighlight(t, stream)
	clearHighlight(t, h)
	cleared := receiveHighlight(t, stream)

	// Assert.
	if row, words := wordsOf(held.GetHeld()); row != "row-1" || words != "entropy" {
		t.Fatalf("frame after the highlight = %v, want held row-1 entropy", held)
	}
	if cleared.GetNone() == nil {
		t.Fatalf("frame after the clear = %v, want none", cleared)
	}
}

func TestALateRootWatchIsToldTheHeldHighlight(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withRevisable(h)
	highlight(t, h, "row-1", "entropy")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	got := receiveHighlight(t, openRootWatch(t, h, ctx))

	// Assert.
	if row, words := wordsOf(got.GetHeld()); row != "row-1" || words != "entropy" {
		t.Fatalf("frame = %v, want held row-1 entropy", got)
	}
}

func TestThePromptThatTakesTheHighlightStatesItTaken(t *testing.T) {
	cases := []struct {
		name string
		err  error
	}{
		{name: "a queued revision", err: nil},
		{name: "a refused revision", err: revision.ErrNotRevisable},
		{name: "a revision that could not be queued", err: errors.New("the lane is gone")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			withRevisable(h)
			h.Revisions.err = tc.err
			highlight(t, h, "row-1", "entropy")
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := openRootWatch(t, h, ctx)
			receiveHighlight(t, stream)

			// Act.
			_, _ = h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))
			got := receiveHighlight(t, stream)

			// Assert.
			if row, words := wordsOf(got.GetTaken()); row != "row-1" || words != "entropy" {
				t.Fatalf("frame = %v, want taken row-1 entropy", got)
			}
		})
	}
}

func TestTakingWithNoHighlightHeldPublishesNothing(t *testing.T) {
	// Arrange.
	highlights := newResponseHighlights()
	topic := highlights.topic("ws-1")

	// Act.
	_, ok := highlights.take("ws-1")

	// Assert.
	got, _ := topic.Latest()
	if ok || got.GetNone() == nil {
		t.Fatalf("take answered %v and left %v, want false and none", ok, got)
	}
}
