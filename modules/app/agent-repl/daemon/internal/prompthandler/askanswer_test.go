package prompthandler

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/wsm"
)

// A PLAIN USER SEND MADE WHILE AN ASK STANDS IS THAT ASK'S TYPED ANSWER: it is
// handed to the standing-ask answerer under the key's claim and mints no turn.
// Every other submission reaches the queue exactly as before.

func TestAnAnsweredSendMintsNoTurnAndReachesNoQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.asks.answered = true

	// Act.
	outcome, err := h.submit("use OAuth")

	// Assert.
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if outcome.Recognition != RecognizedAskAnswer || outcome.Turn != "" {
		t.Fatalf("outcome = %+v, want an ask answer with no turn", outcome)
	}
	if n := len(h.queue.forwarded()); n != 0 {
		t.Fatalf("%d submissions forwarded, want none", n)
	}
	asked := h.asks.asked()
	if len(asked) != 1 || asked[0].text != "use OAuth" || asked[0].ws != theWorkspace || asked[0].target != nil {
		t.Fatalf("asked = %+v, want the typed text for the root feed", asked)
	}
	recorded := false
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opSubmit && r.Message == "the send was delivered as a standing ask's typed answer; no turn was minted" {
			recorded = true
		}
	}
	if !recorded {
		t.Fatalf("records = %+v, want the ask answer recorded", h.log.Records())
	}
	if moved := h.moved(); len(moved) != 1 {
		t.Fatalf("moved on = %v, want the workspace moved on once", moved)
	}
}

func TestASendWithNoStandingAskIsAnOrdinaryPrompt(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	outcome, err := h.submit("hello")

	// Assert.
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if outcome.Recognition != RecognizedNone || len(h.queue.forwarded()) != 1 {
		t.Fatalf("outcome = %+v, forwarded = %d; want an ordinary forwarded prompt", outcome, len(h.queue.forwarded()))
	}
	if len(h.asks.asked()) != 1 {
		t.Fatalf("asked = %+v, want the answerer consulted once", h.asks.asked())
	}
}

func TestABubbleAddressedSendLooksForTheAskOnThatBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.asks.answered = true
	target := &feedid.Ref{WS: theWorkspace, Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-1"}}

	// Act.
	if _, err := h.h.Submit(context.Background(), theWorkspace, userSaid("yes"), "key-1",
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT, nil, wsm.DeliveryOrdinary, target); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert.
	if asked := h.asks.asked(); len(asked) != 1 || asked[0].target != target {
		t.Fatalf("asked = %+v, want the bubble target carried", asked)
	}
}

func TestOnlyAPlainUserSendCanBeATypedAnswer(t *testing.T) {
	image := &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{}}}
	tests := []struct {
		name     string
		ctx      context.Context
		said     *conversationv1.UserSaid
		origin   conversationv1.PromptOrigin
		delivery wsm.Delivery
	}{
		{"a decorated send", context.Background(), userSaid("hello"),
			conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT, wsm.DeliveryOrdinary},
		{"a program's submission", context.Background(), userSaid("hello"),
			conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED, wsm.DeliveryOrdinary},
		{"a deferred prompt", context.Background(), userSaid("hello"),
			conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, wsm.DeliveryDeferred},
		{"a re-drive", WithRedrive(context.Background()), userSaid("hello"),
			conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, wsm.DeliveryOrdinary},
		{"a send with an image", context.Background(), &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: append(userSaid("look").GetContent().GetBlocks(), image),
		}}, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, wsm.DeliveryOrdinary},
		{"a blank send", context.Background(), userSaid("  \n "),
			conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, wsm.DeliveryOrdinary},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.asks.answered = true

			// Act.
			outcome, err := h.h.Submit(tc.ctx, theWorkspace, tc.said, "key-1", tc.origin, nil, tc.delivery, nil)

			// Assert.
			if err != nil {
				t.Fatalf("Submit: %v", err)
			}
			if len(h.asks.asked()) != 0 {
				t.Fatalf("asked = %+v, want the answerer never consulted", h.asks.asked())
			}
			if outcome.Recognition != RecognizedNone {
				t.Fatalf("recognition = %v, want an ordinary prompt", outcome.Recognition)
			}
		})
	}
}

func TestACommandIsNeverATypedAnswer(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.asks.answered = true

	// Act.
	outcome, err := h.submit("/status")

	// Assert.
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if outcome.Recognition != RecognizedPanel || len(h.asks.asked()) != 0 {
		t.Fatalf("outcome = %+v, asked = %+v; want the panel and no ask consulted", outcome, h.asks.asked())
	}
}

func TestAFailedTypedAnswerSurfacesAndLeavesTheKeyUnaccepted(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.asks.answered = true
	h.asks.err = errors.New("the ask settled")

	// Act.
	_, err := h.submit("too late")

	// Assert.
	if err == nil || err.Error() != "the ask settled" {
		t.Fatalf("err = %v, want the answerer's error", err)
	}
	if len(h.queue.forwarded()) != 0 {
		t.Fatal("a failed typed answer reached the queue as a prompt")
	}
	// The key was never accepted, so the retry is driven rather than refused.
	h.asks.err = nil
	if _, err := h.submit("too late"); err != nil {
		t.Fatalf("the retry: %v, want it driven", err)
	}
}

func TestARetryOfAnAnsweredSendIsADuplicate(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.asks.answered = true
	if _, err := h.submit("use OAuth"); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act.
	_, err := h.submit("use OAuth")

	// Assert.
	if !errors.Is(err, ErrDuplicateSubmission) {
		t.Fatalf("err = %v, want ErrDuplicateSubmission", err)
	}
	if n := len(h.asks.asked()); n != 1 {
		t.Fatalf("the answerer was handed the text %d times, want once", n)
	}
}
