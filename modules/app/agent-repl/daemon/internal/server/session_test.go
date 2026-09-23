package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/promptqueue"
	"claude-repld/internal/workspace"
)

// releaseRequest is the release action on one held prompt.
func releaseRequest() *agentreplv1.UpdateHeldPromptRequest {
	return &agentreplv1.UpdateHeldPromptRequest{
		Workspace: ref(),
		Turn:      &conversationv1.TurnId{Value: "turn-1"},
		Action: &agentreplv1.UpdateHeldPromptRequest_Release{
			Release: &agentreplv1.UpdateHeldPromptRelease{},
		},
	}
}

// acceptRequest is the accept action on one held prompt.
func acceptRequest() *agentreplv1.UpdateHeldPromptRequest {
	return &agentreplv1.UpdateHeldPromptRequest{
		Workspace: ref(),
		Turn:      &conversationv1.TurnId{Value: "turn-1"},
		Action: &agentreplv1.UpdateHeldPromptRequest_Accept{
			Accept: &agentreplv1.UpdateHeldPromptAccept{},
		},
	}
}

// TestUpdateHeldPromptReleaseReachesTheQueue pins that release delivers through
// the queue's own verb rather than through a second delivery path.
func TestUpdateHeldPromptReleaseReachesTheQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.UpdateHeldPrompt(context.Background(), connect.NewRequest(releaseRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateHeldPrompt: %v", err)
	}
	if resp.Msg.GetSuccess() == nil || len(h.Queue.released) != 1 {
		t.Fatalf("released = %v, result = %v", h.Queue.released, resp.Msg.GetResult())
	}
}

// TestUpdateHeldPromptMapsNoSuchHold pins the queue's unknown-hold refusal.
func TestUpdateHeldPromptMapsNoSuchHold(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Queue.releaseErr = promptqueue.ErrNoSuchHold

	// Act.
	resp, err := h.Client.UpdateHeldPrompt(context.Background(), connect.NewRequest(releaseRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateHeldPrompt: %v", err)
	}
	if resp.Msg.GetError().GetNoSuchHold() == nil {
		t.Fatalf("result = %v, want no_such_hold", resp.Msg.GetResult())
	}
}

// TestUpdateHeldPromptMapsAcceptNotApplicable pins that `accept` is legal ONLY
// on a hold_for_turn_end verdict.
func TestUpdateHeldPromptMapsAcceptNotApplicable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Queue.acceptErr = promptqueue.ErrAcceptNotApplicable

	// Act.
	resp, err := h.Client.UpdateHeldPrompt(context.Background(), connect.NewRequest(acceptRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateHeldPrompt: %v", err)
	}
	if resp.Msg.GetError().GetAcceptNotApplicable() == nil {
		t.Fatalf("result = %v, want accept_not_applicable", resp.Msg.GetResult())
	}
}

// TestAnswerHeldOfferAnswersTheDequeue pins that the tray's one offer reaches
// the merge orchestrator.
func TestAnswerHeldOfferAnswersTheDequeue(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.AnswerHeldOffer(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerHeldOfferRequest{
			Workspace: ref(),
			Answer: &agentreplv1.AnswerHeldOfferRequest_MergeDequeue{
				MergeDequeue: &agentreplv1.AnswerHeldOfferMergeDequeue{
					Decision: &agentreplv1.AnswerHeldOfferMergeDequeue_Keep{
						Keep: &agentreplv1.AnswerHeldOfferKeep{},
					},
				},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerHeldOffer: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestSetPermissionModeMapsTheModeNotServedArm pins that the daemon accepts
// only the modes its own picker served.
func TestSetPermissionModeMapsTheModeNotServedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.setPermissionModeErr = &workspace.Refusal{
		Arm: workspace.ArmModeNotServed, Reason: "the picker never served it",
	}

	// Act.
	resp, err := h.Client.SetPermissionMode(context.Background(),
		connect.NewRequest(&agentreplv1.SetPermissionModeRequest{
			Workspace: ref(), Mode: "bypass",
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SetPermissionMode: %v", err)
	}
	if resp.Msg.GetError().GetModeNotServed() == nil {
		t.Fatalf("result = %v, want mode_not_served", resp.Msg.GetResult())
	}
}

// setModelRequest is the topbar selector's pick of one served model, echoed
// back as the typed catalog token.
func setModelRequest(model string) *agentreplv1.SetModelRequest {
	return &agentreplv1.SetModelRequest{
		Workspace: ref(),
		Model:     &conversationv1.AgentModel{Name: model},
	}
}

// TestSetModelRelaysTheEchoedTokenToTheVerb pins that a model the daemon
// served travels to the verb UNCHANGED and answers success: the echoed catalog
// token is the whole request, and the handler neither rewrites nor re-derives
// it.
func TestSetModelRelaysTheEchoedTokenToTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SetModel(context.Background(), connect.NewRequest(setModelRequest("sonnet")))

	// Assert.
	if err != nil {
		t.Fatalf("SetModel: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
	if h.Verbs.setModel != "sonnet" {
		t.Fatalf("verb saw model %q, want the echoed token \"sonnet\"", h.Verbs.setModel)
	}
}

// TestSetModelAnswersTheUnservedTokenByItsOwnArm pins that a model outside
// what the topbar served is refused as `not_in_catalog` — a NAMED arm on the
// response, not a transport failure.
func TestSetModelAnswersTheUnservedTokenByItsOwnArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.setModelErr = &workspace.Refusal{
		Arm:    workspace.ArmNotInCatalog,
		Reason: "the model \"gpt\" is not in the served catalog [opus sonnet]",
	}

	// Act.
	resp, err := h.Client.SetModel(context.Background(), connect.NewRequest(setModelRequest("gpt")))

	// Assert.
	if err != nil {
		t.Fatalf("SetModel answered a transport error for an unserved model: %v", err)
	}
	if resp.Msg.GetError().GetNotInCatalog() == nil {
		t.Fatalf("result = %v, want not_in_catalog", resp.Msg.GetResult())
	}
}

// TestSetModelRelaysAShimRefusalByName pins that a refusal the SHIM made
// arrives on the response under the arm the contract spells it with, per
// refusal arm the shim can raise. A shim refusal answered as a transport error
// is the defect this covers: the client then reads an unreachable daemon
// instead of the reason its model change was refused.
func TestSetModelRelaysAShimRefusalByName(t *testing.T) {
	tests := []struct {
		name string
		arm  string
		want func(*agentreplv1.SetModelError) bool
	}{
		{
			name: "the shim's catalog refusal, renamed onto the rpc's arm",
			arm:  workspace.ArmShimModelNotInCatalog,
			want: func(e *agentreplv1.SetModelError) bool { return e.GetNotInCatalog() != nil },
		},
		{
			name: "the vendor's own refusal",
			arm:  "vendor_refused",
			want: func(e *agentreplv1.SetModelError) bool { return e.GetVendorRefused() != nil },
		},
		{
			name: "the shim raised the cold gate on the switch",
			arm:  workspace.ArmShimCold,
			want: func(e *agentreplv1.SetModelError) bool { return e.GetCold() != nil },
		},
		{
			name: "no session to set a model on",
			arm:  workspace.ArmShimNoSession,
			want: func(e *agentreplv1.SetModelError) bool { return e.GetNoSession() != nil },
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.setModelErr = &workspace.ShimRefusal{
				Verb: "SetSessionModel", Arm: test.arm, Detail: "the shim said so",
			}

			// Act.
			resp, err := h.Client.SetModel(context.Background(),
				connect.NewRequest(setModelRequest("sonnet")))

			// Assert.
			if err != nil {
				t.Fatalf("SetModel answered a transport error for shim arm %q: %v", test.arm, err)
			}
			if !test.want(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the arm for shim refusal %q", resp.Msg.GetResult(), test.arm)
			}
		})
	}
}

// TestSelectAccountRelaysTheEchoedRootToTheVerb pins that the option's own
// config_dir travels to the verb UNCHANGED: it is an echo token, and the
// handler neither rewrites nor re-derives a path.
func TestSelectAccountRelaysTheEchoedRootToTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.selectAccountLoggedIn = true

	// Act.
	resp, err := h.Client.SelectAccount(context.Background(),
		connect.NewRequest(&agentreplv1.SelectAccountRequest{
			Workspace: ref(), ConfigDir: "/Users/dev/.claude-work",
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}
	if h.Verbs.selectAccountDir != "/Users/dev/.claude-work" {
		t.Fatalf("verb root = %q, want the echoed option", h.Verbs.selectAccountDir)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestSelectAccountSaysWhenTheChosenRootIsLoggedOut pins the cue the client
// opens the login flow on: the switch happened, and the root holds no login.
func TestSelectAccountSaysWhenTheChosenRootIsLoggedOut(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.selectAccountLoggedIn = false

	// Act.
	resp, err := h.Client.SelectAccount(context.Background(),
		connect.NewRequest(&agentreplv1.SelectAccountRequest{
			Workspace: ref(), ConfigDir: "/Users/dev/.claude-work",
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want a success for a root the daemon knows", resp.Msg.GetResult())
	}
	if resp.Msg.GetSuccess().GetLoggedIn() {
		t.Fatalf("logged_in = true for a root with no login")
	}
}

// TestSelectAccountMapsTheUnknownAccountArm pins that only a root the daemon
// actually knows may be chosen.
func TestSelectAccountMapsTheUnknownAccountArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.selectAccountErr = &workspace.Refusal{
		Arm: workspace.ArmUnknownAccount, Reason: "no such root",
	}

	// Act.
	resp, err := h.Client.SelectAccount(context.Background(),
		connect.NewRequest(&agentreplv1.SelectAccountRequest{
			Workspace: ref(), ConfigDir: "/Users/dev/.claude-elsewhere",
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}
	if resp.Msg.GetError().GetUnknownAccount() == nil {
		t.Fatalf("result = %v, want unknown_account", resp.Msg.GetResult())
	}
}

// TestSelectAccountRefusesABlankRoot pins that a request naming no root is a
// validation failure rather than an arm: no option the daemon served is blank.
func TestSelectAccountRefusesABlankRoot(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SelectAccount(context.Background(),
		connect.NewRequest(&agentreplv1.SelectAccountRequest{Workspace: ref()}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", connect.CodeOf(err))
	}
}

// editRequest is one step of a held-prompt edit on turn-1.
func editRequest(step string) *agentreplv1.EditHeldPromptRequest {
	req := &agentreplv1.EditHeldPromptRequest{
		Workspace: ref(),
		Turn:      &conversationv1.TurnId{Value: "turn-1"},
	}
	switch step {
	case "begin":
		req.Action = &agentreplv1.EditHeldPromptRequest_Begin{Begin: &agentreplv1.EditHeldPromptBegin{}}
	case "commit":
		req.Action = &agentreplv1.EditHeldPromptRequest_Commit{Commit: &agentreplv1.EditHeldPromptCommit{
			Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
				Blocks: []*conversationv1.UserContentBlock{{
					Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "the edited words"}},
				}},
			}},
		}}
	case "cancel":
		req.Action = &agentreplv1.EditHeldPromptRequest_Cancel{Cancel: &agentreplv1.EditHeldPromptCancel{}}
	}
	return req
}

func TestEditHeldPromptBeginWithNoHostStreamProbesNoEditor(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.EditHeldPrompt(context.Background(), connect.NewRequest(editRequest("begin")))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("EditHeldPrompt = (%v, %v), want success", resp, err)
	}
	if len(h.Queue.editorLive) != 1 || h.Queue.editorLive[0] {
		t.Fatalf("editor probe = %v, want one probe answering no editor", h.Queue.editorLive)
	}
}

func TestEditHeldPromptBeginProbesTheStandingHostStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, dialErr := h.Client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{
		Workspace: ref(),
	}))
	if dialErr != nil {
		t.Fatalf("open the stream: %v", dialErr)
	}
	h.Server.Relay().ReloadWebapp(testWorkspaceID)
	receiveHostEvent(t, stream)

	// Act.
	if _, err := h.Client.EditHeldPrompt(context.Background(), connect.NewRequest(editRequest("begin"))); err != nil {
		t.Fatalf("EditHeldPrompt: %v", err)
	}

	// Assert.
	if len(h.Queue.editorLive) != 1 || !h.Queue.editorLive[0] {
		t.Fatalf("editor probe = %v, want the held host stream seen", h.Queue.editorLive)
	}
}

func TestEditHeldPromptCommitCarriesTheNewContent(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.EditHeldPrompt(context.Background(), connect.NewRequest(editRequest("commit")))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("EditHeldPrompt = (%v, %v), want success", resp, err)
	}
	if len(h.Queue.committed) != 1 ||
		h.Queue.committed[0].GetContent().GetBlocks()[0].GetText().GetText() != "the edited words" {
		t.Fatalf("committed = %v, want the edited words", h.Queue.committed)
	}
}

func TestEditHeldPromptCancelReachesTheQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.EditHeldPrompt(context.Background(), connect.NewRequest(editRequest("cancel")))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("EditHeldPrompt = (%v, %v), want success", resp, err)
	}
	if len(h.Queue.cancelled) != 1 || h.Queue.cancelled[0] != "turn-1" {
		t.Fatalf("cancelled = %v, want turn-1", h.Queue.cancelled)
	}
}

func TestEditHeldPromptMapsEveryRefusal(t *testing.T) {
	tests := []struct {
		name    string
		step    string
		err     error
		arrange func(q *fakeQueue, err error)
		armOf   func(*agentreplv1.EditHeldPromptError) bool
	}{
		{"no such hold", "begin", promptqueue.ErrNoSuchHold, func(q *fakeQueue, err error) { q.beginErr = err },
			func(e *agentreplv1.EditHeldPromptError) bool { return e.GetNoSuchHold() != nil }},
		{"not held", "begin", promptqueue.ErrNotHeld, func(q *fakeQueue, err error) { q.beginErr = err },
			func(e *agentreplv1.EditHeldPromptError) bool { return e.GetNotHeld() != nil }},
		{"already delivered", "begin", promptqueue.ErrAlreadyDelivered, func(q *fakeQueue, err error) { q.beginErr = err },
			func(e *agentreplv1.EditHeldPromptError) bool { return e.GetAlreadyDelivered() != nil }},
		{"being edited", "begin", &promptqueue.BeingEditedError{Turn: "turn-0"}, func(q *fakeQueue, err error) { q.beginErr = err },
			func(e *agentreplv1.EditHeldPromptError) bool {
				return e.GetBeingEdited().GetEditingTurn().GetValue() == "turn-0"
			}},
		{"no editor", "begin", promptqueue.ErrNoEditor, func(q *fakeQueue, err error) { q.beginErr = err },
			func(e *agentreplv1.EditHeldPromptError) bool { return e.GetNoEditor() != nil }},
		{"not editing on commit", "commit", promptqueue.ErrNotEditing, func(q *fakeQueue, err error) { q.commitErr = err },
			func(e *agentreplv1.EditHeldPromptError) bool { return e.GetNotEditing() != nil }},
		{"not editing on cancel", "cancel", promptqueue.ErrNotEditing, func(q *fakeQueue, err error) { q.cancelErr = err },
			func(e *agentreplv1.EditHeldPromptError) bool { return e.GetNotEditing() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tt.arrange(h.Queue, tt.err)

			// Act.
			resp, err := h.Client.EditHeldPrompt(context.Background(), connect.NewRequest(editRequest(tt.step)))

			// Assert.
			if err != nil {
				t.Fatalf("EditHeldPrompt: %v", err)
			}
			if !tt.armOf(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.name)
			}
		})
	}
}

func TestEditHeldPromptSurfacesAnOrdinaryFailureAsAnError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Queue.commitErr = errors.New("disk full")

	// Act.
	_, err := h.Client.EditHeldPrompt(context.Background(), connect.NewRequest(editRequest("commit")))

	// Assert.
	if err == nil {
		t.Fatal("EditHeldPrompt succeeded, want the failure surfaced")
	}
}
