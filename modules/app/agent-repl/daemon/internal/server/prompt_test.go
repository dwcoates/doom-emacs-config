package server

import (
	"context"
	"fmt"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
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

// promptText flattens a said's text blocks the way the daemon delivers them.
func promptText(s *conversationv1.UserSaid) string {
	var parts []string
	for _, block := range s.GetContent().GetBlocks() {
		if text := block.GetText(); text != nil {
			parts = append(parts, text.GetText())
		}
	}
	return join(parts)
}

// join concatenates with newlines, mirroring the daemon's own flattening.
func join(parts []string) string {
	out := ""
	for i, p := range parts {
		if i > 0 {
			out += "\n"
		}
		out += p
	}
	return out
}

// TestSubmitPromptPrependsTheReferencedResponse pins the exact reply-preamble
// wording: the markdown of the response SELECTED when the prompt is accepted,
// between the two ⟢ markers, followed by the user's own words, delivered as
// one prompt to the shim. The reply target is no longer named on the request:
// the daemon reads its own held selection.
func TestSubmitPromptPrependsTheReferencedResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("resp-1")
	h.Feed.markdown = map[string]string{"resp-1": "The capital is Paris."}
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedNone,
		Turn:        "turn-9",
		Disposition: promptqueue.Disposition{Delivered: true},
	}
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
		t.Fatalf("select the response: %v", err)
	}
	req := submitRequest()
	req.Said = said("And its population?")

	// Act.
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(req)); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	want := "⟢ Replying to an earlier response of yours:\n\n" +
		"The capital is Paris." +
		"\n\n⟢ My message:\n\n" +
		"And its population?"
	if got := promptText(h.Prompts.lastSaid); got != want {
		t.Fatalf("delivered prompt =\n%q\nwant\n%q", got, want)
	}
}

// TestSubmitPromptRefusesAnUnresolvableReference pins that a selected response
// the daemon cannot resolve to markdown is REFUSED, never silently dropped:
// the user's message is not delivered shorn of the reply they asked for, and
// the selection they saw is not quietly cleared out from under them.
func TestSubmitPromptRefusesAnUnresolvableReference(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("resp-older", "resp-gone")
	// The resolver could read the row when it was selected, and no longer can.
	h.Feed.unreadable = map[string]bool{}
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
		t.Fatalf("select the response: %v", err)
	}
	h.Feed.unreadable["resp-gone"] = true

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert: no landed arm carries this refusal, so it surfaces loudly as a
	// Connect error rather than an answer — and the prompt never reached the
	// handler.
	if connectCode(t, err) != connect.CodeNotFound {
		t.Fatalf("code = %v, want NotFound for an unresolvable reference", connectCode(t, err))
	}
	if h.Prompts.lastSaid != nil {
		t.Fatalf("the prompt was delivered despite the unresolvable reference: %v", h.Prompts.lastSaid)
	}

	// The selection is KEPT, not cleared: OLDER from the held resp-gone lands
	// on resp-older, while OLDER from nothing would restart at the newest
	// (resp-gone again).
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(older)))
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "resp-older" {
		t.Fatalf("selected response = %q, want resp-older (the held resp-gone kept)", got)
	}
}

// TestSubmitPromptEndsTheSelectionAfterConsumingTheReference pins that a
// successful reply-consuming submit ends the daemon's held selection. The
// two-row set makes the end observable: with the most recent seeded, a later
// NEWER restarts at the most recent when ended, but would WRAP to the oldest
// if the selection had survived.
func TestSubmitPromptEndsTheSelectionAfterConsumingTheReference(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("resp-old", "resp-new")
	h.Feed.markdown = map[string]string{"resp-new": "prior answer"}
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedNone,
		Turn:        "turn-10",
		Disposition: promptqueue.Disposition{Delivered: true},
	}
	// Seed the selection at the most recent (NEWER from none).
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
		t.Fatalf("seed selection: %v", err)
	}

	// Act.
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest())); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert: a NEWER now restarts at the most recent (ended), rather than
	// wrapping to the oldest (which a surviving selection would do).
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer)))
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "resp-new" {
		t.Fatalf("selected = %q, want resp-new (an ended selection restarts at the most recent)", got)
	}
}

// TestSubmitPromptWithASelectedPromptQuotesItAsAPromptAndEndsIt pins that a
// SELECTED PROMPT is a reply target like every selected bubble (owner ruling,
// 2026-10-01): its text is prepended under the prompt preamble, since it was
// not the agent's response, and an accepted submit ends it, exactly as it ends
// a selected response.
func TestSubmitPromptWithASelectedPromptQuotesItAsAPromptAndEndsIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.prompts = feedIDs("p1", "p2")
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedNone,
		Turn:        "turn-11",
		Disposition: promptqueue.Disposition{Delivered: true},
	}
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(promptStep(newer))); err != nil {
		t.Fatalf("seed a prompt selection: %v", err)
	}
	req := submitRequest()
	req.Said = said("unchanged message")

	// Act.
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(req)); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert: quoted as a prompt.
	want := "⟢ Replying to an earlier prompt in this conversation:\n\ntext of p2\n\n⟢ My message:\n\nunchanged message"
	if got := promptText(h.Prompts.lastSaid); got != want {
		t.Fatalf("delivered prompt = %q, want %q", got, want)
	}
	// Assert: the prompt selection ended (NEWER restarts at the most recent
	// rather than wrapping past it).
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(promptStep(newer)))
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetPrompt().GetRow().GetValue(); got != "p2" {
		t.Fatalf("selected prompt = %q, want p2 (the ended selection restarts at the most recent)", got)
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

// TestSubmitPromptAnswersATurnMintingSessionAct pins that a context cut, which
// reaches the vendor AS A TURN, answers with the turn it minted.
func TestSubmitPromptAnswersATurnMintingSessionAct(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedAct,
		Turn:        "turn-9",
		Act:         promptqueue.Act{Kind: "ActClear"},
	}

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetTurn().GetTurn().GetValue(); got != "turn-9" {
		t.Fatalf("turn = %q, want turn-9", got)
	}
}

// TestSubmitPromptAnswersCommandActedForATurnlessAct pins the landed arm: an
// act that mints no turn (/model <arg>) answers `command_acted`.
func TestSubmitPromptAnswersCommandActedForATurnlessAct(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedAct,
		Act:         promptqueue.Act{Kind: "ActSetModel", Value: "opus"},
	}

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if resp.Msg.GetSuccess().GetCommandActed() == nil {
		t.Fatalf("outcome = %v, want command_acted", resp.Msg.GetSuccess().GetOutcome())
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

// TestBubbleRefusedFoldsNotDeliverable pins the shim's not_deliverable onto the
// LANDED bubble_refused arm under its own kind (landing 7).
func TestBubbleRefusedFoldsNotDeliverable(t *testing.T) {
	// Arrange.
	in := refusal{Arm: "not_deliverable", Reason: "the SDK has no route to agent \"a1\""}

	// Act.
	out := bubbleRefused(in)

	// Assert.
	if out.Arm != "bubble_refused" || out.Fields["kind"] != nestedArm("not_deliverable") {
		t.Fatalf("refusal = %+v, want bubble_refused with kind not_deliverable", out)
	}
}

// TestBubbleRefusedKeepsTheShimsSentenceAsTheReason pins that the shim's own
// words survive the fold, because they become the arm's `detail`.
func TestBubbleRefusedKeepsTheShimsSentenceAsTheReason(t *testing.T) {
	// Arrange.
	in := refusal{Arm: "not_deliverable", Reason: "the SDK has no route to agent \"a1\""}

	// Act.
	out := bubbleRefused(in)

	// Assert.
	if out.Reason != "the SDK has no route to agent \"a1\"" {
		t.Fatalf("refusal reason = %q, want the shim's own sentence", out.Reason)
	}
}

// TestBubbleRefusedFoldsAgentBusy pins the shim's agent_busy onto the same arm
// under the agent_busy kind.
func TestBubbleRefusedFoldsAgentBusy(t *testing.T) {
	// Arrange.
	in := refusal{Arm: "agent_busy", Reason: "the subagent's turn is running"}

	// Act.
	out := bubbleRefused(in)

	// Assert.
	if out.Arm != "bubble_refused" || out.Fields["kind"] != nestedArm("agent_busy") {
		t.Fatalf("refusal = %+v, want bubble_refused with kind agent_busy", out)
	}
}

// TestBubbleRefusedPassesOtherRefusalsThrough pins that no other refusal is
// reinterpreted as a bubble refusal.
func TestBubbleRefusedPassesOtherRefusalsThrough(t *testing.T) {
	// Arrange.
	in := refusal{Arm: "no_session", Reason: "no live session"}

	// Act.
	out := bubbleRefused(in)

	// Assert.
	if out.Arm != "no_session" || out.Reason != "no live session" {
		t.Fatalf("refusal = %+v, want the input untouched", out)
	}
}

// ---- every typed refusal of SubmitPrompt is recorded ----------------------
//
// Owner's report, 2026-09-14: three prompts were refused inside thirteen
// seconds and the daemon wrote no record of any of them — the refusal path
// logs at DEBUG and production runs at INFO, so the whole episode existed only
// in the editor's log. An answer nothing records is an invisible action.

func TestSubmitRefusalIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	s := &server{}
	log := &recordingLogger{}
	resp := &agentreplv1.SubmitPromptResponse{}

	// Act.
	cerr := s.refuse(log, "SubmitPrompt", resp,
		submitRefusal(refusal{Arm: "no_session", Reason: "no live session"}))

	// Assert.
	if cerr != nil {
		t.Fatalf("refuse = %v, want the refusal encoded onto the response", cerr)
	}
	if got := log.at("INFO"); len(got) != 1 || got[0].Context["arm"] != "no_session" {
		t.Fatalf("INFO records = %v, want exactly one naming the arm", log.records)
	}
}

func TestSubmitRefusalIsRecordedUnderThePromptHandlersOperation(t *testing.T) {
	// Arrange.
	s := &server{}
	log := &recordingLogger{}
	resp := &agentreplv1.SubmitPromptResponse{}

	// Act.
	_ = s.refuse(log, "SubmitPrompt", resp,
		submitRefusal(refusal{Arm: "merging", Reason: "a merge is in flight"}))

	// Assert.
	if got := log.at("INFO"); len(got) != 1 || got[0].Operation != opSubmit {
		t.Fatalf("INFO records = %v, want one filed under %q", log.records, opSubmit)
	}
}

func TestAnUnmarkedRefusalStaysAtDebugUnderItsRpc(t *testing.T) {
	// Arrange. Nothing else's records move: only SubmitPrompt was ruled.
	s := &server{}
	log := &recordingLogger{}
	resp := &agentreplv1.SubmitPromptResponse{}

	// Act.
	_ = s.refuse(log, "SubmitPrompt", resp, refusal{Arm: "no_session", Reason: "no live session"})

	// Assert.
	debug := log.at("DEBUG")
	if len(debug) != 1 || debug[0].Operation != "SubmitPrompt" {
		t.Fatalf("DEBUG records = %v, want one under the rpc's own name", log.records)
	}
}

func TestSubmitRefusalCarriesTheColdGateArm(t *testing.T) {
	// Arrange.
	s := &server{}
	log := &recordingLogger{}
	resp := &agentreplv1.SubmitPromptResponse{}
	detail := "the conversation is cold at 101600 context tokens"

	// Act.
	cerr := s.refuse(log, "SubmitPrompt", resp,
		submitRefusal(s.fill(refusal{Arm: "cold_gate", Reason: detail})))

	// Assert.
	if cerr != nil {
		t.Fatalf("refuse = %v, want the cold_gate arm encoded", cerr)
	}
	if got := resp.GetError().GetColdGate().GetDetail(); got != detail {
		t.Fatalf("cold_gate detail = %q, want the gate's own sentence", got)
	}
}

func TestAColdGateRefusalMapsOntoItsOwnArm(t *testing.T) {
	// Arrange.
	s := &server{}
	err := &promptqueue.ColdGateRefusal{Detail: "the conversation is cold at 101600 context tokens"}

	// Act.
	got, ok := s.asRefusal(err)

	// Assert.
	if !ok || got.Arm != "cold_gate" {
		t.Fatalf("asRefusal = (%+v, %v), want the cold_gate arm", got, ok)
	}
}

func TestAColdGateRefusalCarriesTheGatesSentenceAlone(t *testing.T) {
	// Arrange. The queue's own package name must not reach the user.
	s := &server{}
	detail := "the conversation is cold at 101600 context tokens"

	// Act.
	got, _ := s.asRefusal(&promptqueue.ColdGateRefusal{Detail: detail})

	// Assert.
	if got.Reason != detail {
		t.Fatalf("reason = %q, want the gate's sentence alone", got.Reason)
	}
}

func TestARefusedModelActMapsOntoItsOwnArm(t *testing.T) {
	cases := []struct {
		name    string
		shimArm string
		want    string
	}{
		{"a model the catalog lacks", workspace.ArmShimModelNotInCatalog, "model_not_in_catalog"},
		{"a model the vendor refused", "vendor_refused", "model_refused"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the queue wraps the shim's refusal as it returns it.
			s := &server{}
			err := fmt.Errorf("set model on %q: %w", "ws-1",
				&workspace.ShimRefusal{Verb: "SetSessionModel", Arm: tc.shimArm, Detail: `"opus" is not in this session's model catalog`})
			refused, _ := s.asRefusal(err)

			// Act.
			got := modelActRefused(err, refused)

			// Assert.
			if got.Arm != tc.want {
				t.Fatalf("arm = %q, want %q", got.Arm, tc.want)
			}
		})
	}
}

func TestAnotherVerbsVendorRefusalIsNotAModelRefusal(t *testing.T) {
	// Arrange: StartTurn's own vendor refusal is not about a model.
	s := &server{}
	err := &workspace.ShimRefusal{Verb: "StartTurn", Arm: "vendor_refused", Detail: "refused"}
	refused, _ := s.asRefusal(err)

	// Act.
	got := modelActRefused(err, refused)

	// Assert.
	if got.Arm != "vendor_refused" {
		t.Fatalf("arm = %q, want it left as vendor_refused", got.Arm)
	}
}

func TestSubmitRefusalCarriesTheModelNotInCatalogArm(t *testing.T) {
	// Arrange.
	s := &server{}
	log := &recordingLogger{}
	resp := &agentreplv1.SubmitPromptResponse{}
	detail := `"opus" is not in this session's model catalog`

	// Act.
	cerr := s.refuse(log, "SubmitPrompt", resp,
		submitRefusal(s.fill(refusal{Arm: "model_not_in_catalog", Reason: detail})))

	// Assert: an ANSWER the client can show, never a transport error it holds as an outage.
	if cerr != nil {
		t.Fatalf("refuse = %v, want the model_not_in_catalog arm encoded", cerr)
	}
	if got := resp.GetError().GetModelNotInCatalog().GetDetail(); got != detail {
		t.Fatalf("model_not_in_catalog detail = %q, want the shim's sentence", got)
	}
}

func TestSubmitRefusalCarriesTheModelRefusedArm(t *testing.T) {
	// Arrange.
	s := &server{}
	log := &recordingLogger{}
	resp := &agentreplv1.SubmitPromptResponse{}

	// Act.
	cerr := s.refuse(log, "SubmitPrompt", resp,
		submitRefusal(s.fill(refusal{Arm: "model_refused", Reason: "the vendor refused"})))

	// Assert.
	if cerr != nil || resp.GetError().GetModelRefused().GetDetail() != "the vendor refused" {
		t.Fatalf("refuse = %v, error = %v, want the model_refused arm with its sentence", cerr, resp.GetError())
	}
}

// TestSubmitPromptHandsTheHandlerItsDelivery pins the mapping of the optional
// delivery onto the queue's: absent is ordinary, DEFERRED is deferred.
func TestSubmitPromptHandsTheHandlerItsDelivery(t *testing.T) {
	deferred := agentreplv1.SubmitPromptDelivery_SUBMIT_PROMPT_DELIVERY_DEFERRED
	tests := []struct {
		name     string
		delivery *agentreplv1.SubmitPromptDelivery
		want     wsm.Delivery
	}{
		{name: "an absent delivery is ordinary", delivery: nil, want: wsm.DeliveryOrdinary},
		{name: "DEFERRED is deferred", delivery: &deferred, want: wsm.DeliveryDeferred},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Prompts.outcome = prompthandler.Outcome{Recognition: prompthandler.RecognizedNone, Turn: "turn-7"}
			h.Prompts.lastDelivery = wsm.Delivery(-1)
			req := submitRequest()
			req.Delivery = tc.delivery

			// Act.
			if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(req)); err != nil {
				t.Fatalf("SubmitPrompt: %v", err)
			}

			// Assert.
			if h.Prompts.lastDelivery != tc.want {
				t.Fatalf("delivery handed to the handler = %v, want %v", h.Prompts.lastDelivery, tc.want)
			}
		})
	}
}
