package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
)

// submitRequest is one well-formed submission.
func submitRequest() *agentreplv1.SubmitPromptRequest {
	return &agentreplv1.SubmitPromptRequest{
		Workspace:      ref(),
		Said:           said("hello"),
		IdempotencyKey: "k1",
		Origin:         conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}
}

// TestSubmitPromptAnswersTheMintedTurn pins the ordinary path.
func TestSubmitPromptAnswersTheMintedTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedNone,
		Turn:        "turn-7",
		Disposition: promptqueue.Disposition{Delivered: true},
	}

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetTurn().GetTurn().GetValue(); got != "turn-7" {
		t.Fatalf("turn = %q, want turn-7", got)
	}
}

// TestSubmitPromptAnswersAHeldPromptWithItsTurn pins that A HOLD IS AN ANSWER:
// the composer still learns the turn it must match its own row against.
func TestSubmitPromptAnswersAHeldPromptWithItsTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedNone,
		Turn:        "turn-8",
		Disposition: promptqueue.Disposition{},
	}

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetTurn().GetTurn().GetValue(); got != "turn-8" {
		t.Fatalf("turn = %q, want turn-8 even though the prompt was held", got)
	}
}

// TestSubmitPromptAnswersARecognizedPanel pins the panel path.
func TestSubmitPromptAnswersARecognizedPanel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedPanel,
		Panel: &agentreplv1.SubmitPromptCommandPanel{
			Panel: &agentreplv1.SubmitPromptCommandPanel_Status{
				Status: &frontendv1.StatusPanelView{},
			},
		},
	}

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if resp.Msg.GetSuccess().GetCommandPanel().GetStatus() == nil {
		t.Fatalf("result = %v, want the status panel", resp.Msg.GetResult())
	}
}

// TestSubmitPromptAnswersARecognizedRefusal pins the refusal-card path, which
// names the literal command as typed.
func TestSubmitPromptAnswersARecognizedRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition:    prompthandler.RecognizedRefused,
		RefusedCommand: "/agents",
	}

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetCommandRefused().GetCommand(); got != "/agents" {
		t.Fatalf("command = %q, want /agents", got)
	}
}

// TestSubmitPromptRefusesASessionActLoudly pins the project-lead ruling:
// `command_acted` lands in landing 6, and until then an ACTED session command
// answers the loud sentinel naming the arm — never a fabricated turn or panel.
func TestSubmitPromptRefusesASessionActLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedAct,
		Turn:        "turn-9",
		Act:         promptqueue.Act{Kind: "ActClear"},
	}

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err == nil {
		t.Fatal("an acted session command answered a success; it must answer the sentinel")
	}
	if !strings.Contains(err.Error(), "SubmitPromptError.command_acted") {
		t.Fatalf("error = %v, want the command_acted sentinel", err)
	}
}

// TestSubmitPromptRefusesAForeignFeed pins that a FeedId belonging to another
// workspace is refused BEFORE the handler is called.
func TestSubmitPromptRefusesAForeignFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := submitRequest()
	req.Feed = feedIDFor("ws-other")

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if resp.Msg.GetError().GetFeedNotInWorkspace() == nil {
		t.Fatalf("result = %v, want feed_not_in_workspace", resp.Msg.GetResult())
	}
}

// TestSubmitPromptRefusesAnUndecodableFeed pins that a value that does not
// decode never becomes a handler call.
func TestSubmitPromptRefusesAnUndecodableFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := submitRequest()
	req.Feed = &frontendv1.FeedId{Value: "not-a-feed-id"}

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if resp.Msg.GetError().GetFeedUndecodable() == nil {
		t.Fatalf("result = %v, want feed_undecodable", resp.Msg.GetResult())
	}
}

// TestSubmitPromptMapsTheDuplicateSubmissionRefusal pins the landed arm: a
// retried idempotency key mints no second turn and answers IN BAND.
func TestSubmitPromptMapsTheDuplicateSubmissionRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.err = prompthandler.ErrDuplicateSubmission

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if resp.Msg.GetError().GetDuplicateSubmission() == nil {
		t.Fatalf("result = %v, want duplicate_submission", resp.Msg.GetResult())
	}
}
