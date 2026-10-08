package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// A RESPONSE IS INTERIM ONCE A LATER ROW OF ITS TURN LANDS AFTER IT (owner
// rule, 2026-10-08): a tool call, another response, or any row of the turn but
// its ending; never while it is the turn's latest row, so a final answer — even
// while it streams — is never drawn as an interim.

func TestAResponseIsInterimOnlyOnceALaterRowOfItsTurnLands(t *testing.T) {
	tests := []struct {
		name  string
		after func(h *harness)
		unit  string
		want  bool
	}{
		{
			name:  "a response that is the turn's latest row is not interim",
			after: func(h *harness) {},
			unit:  "prose-1",
			want:  false,
		},
		{
			name:  "a later tool call proves it interim",
			after: func(h *harness) { h.send(settledRead("read-1")) },
			unit:  "prose-1",
			want:  true,
		},
		{
			name:  "a later response proves it interim",
			after: func(h *harness) { h.send(responseSuccessActivity("prose-2", "more")) },
			unit:  "prose-1",
			want:  true,
		},
		{
			name:  "a later thinking row proves it interim",
			after: func(h *harness) { h.send(settledThinking("think-1", "reasoning")) },
			unit:  "prose-1",
			want:  true,
		},
		{
			name:  "the turn's ending alone does not",
			after: func(h *harness) { h.terminal("turn-1", completedWith("prose-1"), nil) },
			unit:  "prose-1",
			want:  false,
		},
		{
			name:  "a later turn's prompt does not",
			after: func(h *harness) { h.deliverPrompt("turn-2", "and another thing") },
			unit:  "prose-1",
			want:  false,
		},
		{
			name:  "the later response is itself the latest and not interim",
			after: func(h *harness) { h.send(responseSuccessActivity("prose-2", "more")) },
			unit:  "prose-2",
			want:  false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a turn drew one response.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "do the thing")
			h.send(responseSuccessActivity("prose-1", "a note"))

			// Act.
			tt.after(h)

			// Assert.
			if got := h.responseOn(rootFeed(), tt.unit).GetInterim(); got != tt.want {
				t.Fatalf("interim = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAStreamingAnswerIsNotInterim(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(settledRead("read-1"))

	// Act: the answer is still arriving.
	h.send(responseFrame("answer-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "the ans"}, nil))

	// Assert: drawn in full while it streams.
	if h.responseOn(rootFeed(), "answer-1").GetInterim() {
		t.Fatal("a response still arriving as the turn's latest row was drawn interim")
	}
}

func TestAThinkingRowIsNeverStampedInterim(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(settledThinking("think-1", "reasoning"))

	// Act.
	h.send(settledRead("read-1"))

	// Assert: thinking keeps its own collapse rule.
	if h.responseOn(rootFeed(), "think-1").GetInterim() {
		t.Fatal("a thinking row was stamped interim")
	}
}

func TestARedrawOfAnInterimResponseKeepsTheFlag(t *testing.T) {
	// Arrange: a streaming response is followed by a tool call mid-stream.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(responseFrame("prose-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "a no"}, nil))
	h.send(settledRead("read-1"))

	// Act: its own settle redraws it afterwards.
	h.send(responseSuccessActivity("prose-1", "a note"))

	// Assert.
	if !h.responseOn(rootFeed(), "prose-1").GetInterim() {
		t.Fatal("a redraw of an interim response dropped the flag")
	}
}

func TestProvingAResponseInterimRecordsTheEdgeAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(responseSuccessActivity("prose-1", "a note"))

	// Act.
	h.send(settledRead("read-1"))

	// Assert.
	if !h.hasRecord("info", "daemon.feed.response_interim") {
		t.Fatal("proving a response interim recorded no INFO daemon.feed.response_interim")
	}
}
