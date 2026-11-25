package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/workspace"
)

// TestInterruptAnswersNothingRunningAsASuccess pins that "nothing was running"
// is an ANSWER, not a failure.
func TestInterruptAnswersNothingRunningAsASuccess(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{NothingRunning: true}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if resp.Msg.GetSuccess().GetNothingRunning() == nil {
		t.Fatalf("result = %v, want nothing_running", resp.Msg.GetResult())
	}
}

// TestInterruptOfATurnStopsTheWorkspacesRevisionsFirst pins that a turn stop
// with response revisions running stops those and nothing else: the verbs
// never see the stop, so a turn in flight runs on.
func TestInterruptOfATurnStopsTheWorkspacesRevisionsFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Revisions.running = 2
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{Turn: true}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target: &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{
				RollBackUnanswered: &agentreplv1.InterruptTurnRollBackUnanswered{},
			}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetInterruptedRevisions(); got == nil || got.GetCount() != 2 {
		t.Fatalf("result = %v, want interrupted_revisions count 2", resp.Msg.GetResult())
	}
	if h.Verbs.interruptTarget.Turn {
		t.Fatalf("the verbs were asked to stop the turn %+v; the revisions are stopped instead", h.Verbs.interruptTarget)
	}
}

// TestInterruptOfATurnWithNoRevisionStopsTheTurn pins that a turn stop finding
// no revision goes on to stop the turn.
func TestInterruptOfATurnWithNoRevisionStopsTheTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{Turn: true}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if len(h.Revisions.stops) != 1 || h.Revisions.stops[0] != testWorkspaceID {
		t.Fatalf("revision stops = %v, want one for the workspace", h.Revisions.stops)
	}
	if !h.Verbs.interruptTarget.Turn || resp.Msg.GetSuccess().GetInterruptedTurn() == nil {
		t.Fatalf("target = %+v, result = %v, want the turn stopped", h.Verbs.interruptTarget, resp.Msg.GetResult())
	}
}

// TestInterruptOfAgentsLeavesRevisionsRunning pins that only the turn stop
// reaches revisions: the fan-wide agent stop leaves them running.
func TestInterruptOfAgentsLeavesRevisionsRunning(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Revisions.running = 1
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{DetachedCount: 1}

	// Act.
	_, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_AllAgents{AllAgents: &agentreplv1.InterruptAllAgents{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if len(h.Revisions.stops) != 0 {
		t.Fatalf("revision stops = %v, want none", h.Revisions.stops)
	}
}

// TestInterruptSucceededAnswersTheSuccessItIsGiven pins the one success
// answer: the response carries exactly the success handed to it.
func TestInterruptSucceededAnswersTheSuccessItIsGiven(t *testing.T) {
	// Arrange.
	success := &agentreplv1.InterruptSuccess{Outcome: &agentreplv1.InterruptSuccess_NothingRunning{NothingRunning: &agentreplv1.InterruptNothingRunning{}}}

	// Act.
	answered, err := interruptSucceeded(&agentreplv1.InterruptResponse{}, success)

	// Assert.
	if err != nil || answered.Msg.GetSuccess() != success {
		t.Fatalf("interruptSucceeded() = %v, %v, want the given success", answered.Msg, err)
	}
}

// TestInterruptAnswersTheMergeQueueQuestionWithNoRollback pins that a stop which
// raised the merge-queue question answers that arm, and never a rollback offer.
func TestInterruptAnswersTheMergeQueueQuestionWithNoRollback(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{MergeQueueOffered: true}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if resp.Msg.GetSuccess().GetMergeQueueOffered() == nil {
		t.Fatalf("result = %v, want merge_queue_offered", resp.Msg.GetResult())
	}
}

// TestInterruptAnswersTheDetachedCount pins that a fan-wide interrupt reports
// how many detached items it stopped.
func TestInterruptAnswersTheDetachedCount(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{DetachedCount: 3}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_AllAgents{AllAgents: &agentreplv1.InterruptAllAgents{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetInterruptedDetached().GetCount(); got != 3 {
		t.Fatalf("count = %d, want 3", got)
	}
}

// TestInterruptAnswersTheTurnStillRunningNamingTheTurn pins that a fan-wide
// stop which reached no detached work while a turn runs answers
// turn_still_running with the turn, never nothing_running.
func TestInterruptAnswersTheTurnStillRunningNamingTheTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	turn := ids.TurnID("t-busy")
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{TurnStillRunning: &turn}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_AllAgents{AllAgents: &agentreplv1.InterruptAllAgents{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetTurnStillRunning().GetTurn().GetValue(); got != "t-busy" {
		t.Fatalf("result = %v, want turn_still_running naming t-busy", resp.Msg.GetResult())
	}
}

// TestInterruptRaisesTheConfirmChallenge pins the unconfirmed turn interrupt
// with live detached agents: the user is asked once, with the count.
func TestInterruptRaisesTheConfirmChallenge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptErr = &workspace.ConfirmRequired{LiveAgentCount: 2}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if got := resp.Msg.GetError().GetConfirmRequired().GetLiveAgentCount(); got != 2 {
		t.Fatalf("live_agent_count = %d, want 2", got)
	}
}

// TestInterruptDecodesTheDetachedTarget pins that the clicked bubble's FeedId
// reaches the verbs as the decoded ref.
func TestInterruptDecodesTheDetachedTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{DetachedCount: 1}

	// Act.
	if _, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target: &agentreplv1.InterruptRequest_Detached{
				Detached: feedIDFor(testWorkspaceID),
			},
		})); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert.
	if h.Verbs.interruptTarget.Detached == nil {
		t.Fatal("the detached target did not reach the verbs")
	}
}

// mergeTabIDFor encodes a conflicts tab row of lease-7's bubble in WS's feed.
func mergeTabIDFor(ws ids.WorkspaceID) *frontendv1.FeedId {
	lease := ids.LeaseID("lease-7")
	return feedid.Encode(feedid.Ref{WS: ws, Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "conflicts", Sub: "1"}})
}

func TestInterruptHandsAMergeTabToTheMergeOrchestrator(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.tabRunning = true

	// Act.
	resp, err := h.Client.Interrupt(context.Background(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ref(), Target: &agentreplv1.InterruptRequest_MergeTab{MergeTab: mergeTabIDFor(testWorkspaceID)},
	}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess().GetInterruptedTurn() == nil {
		t.Fatalf("Interrupt = %v, %v; want interrupted_turn", resp, err)
	}
	if len(h.Merge.tabInterrupts) != 1 || h.Merge.tabInterrupts[0].Row.ID != "conflicts" {
		t.Fatalf("tab interrupts = %+v, want the decoded tab", h.Merge.tabInterrupts)
	}
	if h.Verbs.interruptTarget != (workspace.InterruptTarget{}) {
		t.Fatalf("the verbs were asked to interrupt %+v, want the merge alone", h.Verbs.interruptTarget)
	}
}

func TestInterruptOfAMergeTabWithNothingRunningAnswersNothingRunning(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.Interrupt(context.Background(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ref(), Target: &agentreplv1.InterruptRequest_MergeTab{MergeTab: mergeTabIDFor(testWorkspaceID)},
	}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess().GetNothingRunning() == nil {
		t.Fatalf("Interrupt = %v, %v; want nothing_running", resp, err)
	}
}

func TestInterruptOfAnotherWorkspacesMergeTabIsNotMergeTab(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.Interrupt(context.Background(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ref(), Target: &agentreplv1.InterruptRequest_MergeTab{MergeTab: mergeTabIDFor("ws-other")},
	}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetNotMergeTab() == nil {
		t.Fatalf("Interrupt = %v, %v; want not_merge_tab", resp, err)
	}
	if len(h.Merge.tabInterrupts) != 0 {
		t.Fatal("a tab of another workspace reached the orchestrator")
	}
}

func TestInterruptAnswersTheOrchestratorsNotMergeTabRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.tabErr = &merge.RefusalError{Arm: merge.ArmNotMergeTab, Workspace: testWorkspaceID, Reason: "no live tab"}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ref(), Target: &agentreplv1.InterruptRequest_MergeTab{MergeTab: mergeTabIDFor(testWorkspaceID)},
	}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetNotMergeTab() == nil {
		t.Fatalf("Interrupt = %v, %v; want not_merge_tab", resp, err)
	}
}

// TestAnswerPermissionMapsTheNoStandingOfferArm pins that granting a standing
// the ask never offered is refused by its own arm.
func TestAnswerPermissionMapsTheNoStandingOfferArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.answerPermissionErr = &workspace.Refusal{
		Arm: workspace.ArmNoStandingOffer, Reason: "the ask offered none",
	}

	// Act.
	resp, err := h.Client.AnswerPermission(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
			Workspace:  ref(),
			Permission: permissionIDFor(testWorkspaceID, "ask-1"),
			Answer: &agentreplv1.AnswerPermissionRequest_AllowStanding{
				AllowStanding: &agentreplv1.AnswerPermissionAllowStanding{},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerPermission: %v", err)
	}
	if resp.Msg.GetError().GetNoStandingOffer() == nil {
		t.Fatalf("result = %v, want no_standing_offer", resp.Msg.GetResult())
	}
}

// TestAnswerPermissionHandsTheVerbTheDecisionAsked pins that each answer arm
// reaches the verb as the decision it names, addressed to the card's own ask.
func TestAnswerPermissionHandsTheVerbTheDecisionAsked(t *testing.T) {
	tests := []struct {
		name   string
		req    *agentreplv1.AnswerPermissionRequest
		assert func(t *testing.T, decision *conversationv1.AgentPermissionDecision)
	}{
		{
			name: "allow once",
			req: &agentreplv1.AnswerPermissionRequest{
				Answer: &agentreplv1.AnswerPermissionRequest_AllowOnce{AllowOnce: &agentreplv1.AnswerPermissionAllowOnce{}},
			},
			assert: func(t *testing.T, decision *conversationv1.AgentPermissionDecision) {
				if decision.GetAllowed().GetOnce() == nil {
					t.Fatalf("decision = %v, want allowed.once", decision)
				}
			},
		},
		{
			name: "deny with a reason",
			req: &agentreplv1.AnswerPermissionRequest{
				Answer: &agentreplv1.AnswerPermissionRequest_Deny{Deny: &agentreplv1.AnswerPermissionDeny{
					Reason: &agentreplv1.AnswerPermissionDenyReason{Text: "not needed"},
				}},
			},
			assert: func(t *testing.T, decision *conversationv1.AgentPermissionDecision) {
				if decision.GetDenied() == nil || decision.GetDenied().GetMessage() != "not needed" {
					t.Fatalf("decision = %v, want denied{message: %q}", decision, "not needed")
				}
			},
		},
		{
			name: "deny with no reason",
			req: &agentreplv1.AnswerPermissionRequest{
				Answer: &agentreplv1.AnswerPermissionRequest_Deny{Deny: &agentreplv1.AnswerPermissionDeny{}},
			},
			assert: func(t *testing.T, decision *conversationv1.AgentPermissionDecision) {
				if decision.GetDenied() == nil || decision.GetDenied().GetMessage() != "" {
					t.Fatalf("decision = %v, want denied with no message", decision)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tt.req.Workspace = ref()
			tt.req.Permission = permissionIDFor(testWorkspaceID, "ask-1")

			// Act.
			resp, err := h.Client.AnswerPermission(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("AnswerPermission = %v, %v; want a success", resp, err)
			}
			if len(h.Verbs.permissionAnswers) != 1 {
				t.Fatalf("the verb was handed %d answers, want 1", len(h.Verbs.permissionAnswers))
			}
			decision := h.Verbs.permissionAnswers[0].GetPermissionDecision()
			if decision.GetAsk().GetValue() != "ask-1" {
				t.Fatalf("decision ask = %q, want ask-1", decision.GetAsk().GetValue())
			}
			tt.assert(t, decision)
		})
	}
}

// TestAnswerPermissionAnswersAShimUnknownAgentAsUnlanded pins that the shim's
// unknown_agent, which AnswerPermissionError does not carry, reaches the caller
// as a FailedPrecondition naming the intended arm.
func TestAnswerPermissionAnswersAShimUnknownAgentAsUnlanded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.answerPermissionErr = &workspace.Refusal{
		Rpc: "AnswerPermission", Arm: workspace.ArmShimUnknownAgent, Reason: "no such agent",
	}

	// Act.
	_, err := h.Client.AnswerPermission(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
			Workspace:  ref(),
			Permission: permissionIDFor(testWorkspaceID, "ask-1"),
			Answer:     &agentreplv1.AnswerPermissionRequest_Deny{Deny: &agentreplv1.AnswerPermissionDeny{}},
		}))

	// Assert.
	if code := connect.CodeOf(err); code != connect.CodeFailedPrecondition {
		t.Fatalf("err = %v, want FailedPrecondition", err)
	}
	if msg := err.Error(); !strings.Contains(msg, "intended arm: ") || !strings.Contains(msg, "AnswerPermissionError.unknown_agent") {
		t.Fatalf("err = %q, want it to name the intended arm AnswerPermissionError.unknown_agent", msg)
	}
}

// TestAnswerPermissionRenamesUnservedToAskNotStanding pins the per-rpc rename:
// the verbs' ArmUnservedAnswer is AnswerPermissionError.ask_not_standing.
func TestAnswerPermissionRenamesUnservedToAskNotStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.answerPermissionErr = &workspace.Refusal{
		Arm: workspace.ArmUnservedAnswer, Reason: "no ask standing", NotFound: true,
	}

	// Act.
	resp, err := h.Client.AnswerPermission(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
			Workspace:  ref(),
			Permission: permissionIDFor(testWorkspaceID, "ask-1"),
			Answer: &agentreplv1.AnswerPermissionRequest_AllowOnce{
				AllowOnce: &agentreplv1.AnswerPermissionAllowOnce{},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerPermission: %v", err)
	}
	if resp.Msg.GetError().GetAskNotStanding() == nil {
		t.Fatalf("result = %v, want ask_not_standing", resp.Msg.GetResult())
	}
}

// TestAnswerQuestionAnswersAskNotStanding pins that the verb's own
// ask_not_standing arm reaches AnswerQuestionError under that very name, with
// no NotFound-driven rename in between.
func TestAnswerQuestionAnswersAskNotStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.answerQuestionErr = &workspace.Refusal{
		Arm: workspace.ArmAskNotStanding, Reason: "no question batch is standing", NotFound: true,
	}

	// Act.
	resp, err := h.Client.AnswerQuestion(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
			Workspace: ref(),
			Question:  questionIDFor(testWorkspaceID, "ask-2"),
			Answers: []*agentreplv1.AnswerQuestionAnswer{
				{QuestionText: "which?", Chosen: []string{"a"}},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQuestion: %v", err)
	}
	if resp.Msg.GetError().GetAskNotStanding() == nil {
		t.Fatalf("result = %v, want ask_not_standing", resp.Msg.GetResult())
	}
}

// TestAnswerQuestionHandsTheVerbTheServedTextsAndLabels pins that the
// request's question text and chosen labels reach the verb echoed, addressed
// to the card's own ask.
func TestAnswerQuestionHandsTheVerbTheServedTextsAndLabels(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.AnswerQuestion(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
			Workspace: ref(),
			Question:  questionIDFor(testWorkspaceID, "q-1"),
			Answers:   []*agentreplv1.AnswerQuestionAnswer{{QuestionText: "Pick one", Chosen: []string{"A"}}},
		}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerQuestion = %v, %v; want a success", resp, err)
	}
	if len(h.Verbs.questionAnswers) != 1 {
		t.Fatalf("the verb was handed %d answers, want 1", len(h.Verbs.questionAnswers))
	}
	given := h.Verbs.questionAnswers[0].GetQuestionAnswer()
	if given.GetAsk().GetValue() != "q-1" {
		t.Fatalf("answer ask = %q, want q-1", given.GetAsk().GetValue())
	}
	sels := given.GetAnswers().GetAnswers()
	if len(sels) != 1 || sels[0].GetQuestion().GetText() != "Pick one" {
		t.Fatalf("selections = %v, want one echoing %q", sels, "Pick one")
	}
	if len(sels[0].GetChosen()) != 1 || sels[0].GetChosen()[0].GetLabel().GetLabel() != "A" {
		t.Fatalf("chosen = %v, want [A]", sels[0].GetChosen())
	}
}

// TestAnswerQuestionOnAnUndecodableCardAnswersAskNotStanding pins that a
// question id that decodes to no card is the typed ask_not_standing, answered
// before the verb is reached, with no warning.
func TestAnswerQuestionOnAnUndecodableCardAnswersAskNotStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.Surfaces.global = log

	// Act.
	resp, err := h.Client.AnswerQuestion(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
			Workspace: ref(),
			Question:  &frontendv1.FeedId{Value: "not-a-real-feed-id"},
			Answers:   []*agentreplv1.AnswerQuestionAnswer{{QuestionText: "Pick one", Chosen: []string{"A"}}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQuestion = error %v, want the typed ask_not_standing", err)
	}
	if resp.Msg.GetError().GetAskNotStanding() == nil {
		t.Fatalf("result = %v, want ask_not_standing", resp.Msg.GetResult())
	}
	if len(h.Verbs.questionAnswers) != 0 {
		t.Fatalf("the verb was handed %d answers, want none", len(h.Verbs.questionAnswers))
	}
	for _, level := range []string{dlog.LevelWarn, dlog.LevelError} {
		if rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: level}); ok {
			t.Fatalf("record %+v, want no warning or error", rec)
		}
	}
}

// TestAnswerQuestionRenamesUnservedToUnservedValue pins the OTHER half of the
// rename: an unserved VALUE (not an unknown ask) is unserved_value.
func TestAnswerQuestionRenamesUnservedToUnservedValue(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.answerQuestionErr = &workspace.Refusal{
		Arm: workspace.ArmUnservedAnswer, Reason: "that label was never offered",
	}

	// Act.
	resp, err := h.Client.AnswerQuestion(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
			Workspace: ref(),
			Question:  questionIDFor(testWorkspaceID, "ask-2"),
			Answers: []*agentreplv1.AnswerQuestionAnswer{
				{QuestionText: "which?", Chosen: []string{"invented"}},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQuestion: %v", err)
	}
	if got := resp.Msg.GetError().GetUnservedValue(); got == nil {
		t.Fatalf("result = %v, want unserved_value", resp.Msg.GetResult())
	}
}

// TestAnswerColdGateRequiresACompactScope pins that a compaction scope is never
// defaulted: the shim's remediation needs it and the trace carries only a model.
func TestAnswerColdGateRequiresACompactScope(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.AnswerColdGate(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
			Workspace: ref(),
			Gate:      coldGateIDFor(testWorkspaceID),
			Choice: &agentreplv1.AnswerColdGateRequest_Compact{
				Compact: &agentreplv1.AnswerColdGateCompact{
					Model: &conversationv1.AgentModel{Name: "claude-opus-5"},
				},
			},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestAnswerColdGateRequiresACompactAndResumeScope pins that the
// compact-and-resume arm's scope is never defaulted either.
func TestAnswerColdGateRequiresACompactAndResumeScope(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.AnswerColdGate(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
			Workspace: ref(),
			Gate:      coldGateIDFor(testWorkspaceID),
			Choice: &agentreplv1.AnswerColdGateRequest_CompactAndResume{
				CompactAndResume: &agentreplv1.AnswerColdGateCompact{
					Model: &conversationv1.AgentModel{Name: "claude-opus-5"},
				},
			},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}

// TestAnswerColdGateHandsTheVerbTheRequestsChoice pins that the handler
// passes the answer through whole, so the verb sees which compact button was
// pressed.
func TestAnswerColdGateHandsTheVerbTheRequestsChoice(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := &agentreplv1.AnswerColdGateRequest{
		Workspace: ref(),
		Gate:      coldGateIDFor(testWorkspaceID),
		Choice: &agentreplv1.AnswerColdGateRequest_CompactAndResume{
			CompactAndResume: &agentreplv1.AnswerColdGateCompact{
				Model: &conversationv1.AgentModel{Name: "claude-opus-5"},
				Scope: conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL,
			},
		},
	}

	// Act.
	resp, err := h.Client.AnswerColdGate(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerColdGate = %v, %v; want success", resp, err)
	}
	if got := h.Verbs.coldGateAnswers; len(got) != 1 || !proto.Equal(got[0], req) {
		t.Fatalf("verb answers = %v, want the request handed through whole", got)
	}
}

// TestAnswerColdGateAnswersTheVerbsRefusalInBand pins that a verb refusing
// the answer (no gate stands) is answered as its typed arm, and recorded.
func TestAnswerColdGateAnswersTheVerbsRefusalInBand(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.Verbs.answerColdGateErr = &workspace.Refusal{Arm: workspace.ArmNoColdGate, Reason: "no cold gate stands"}

	// Act.
	resp, err := h.Client.AnswerColdGate(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
			Workspace: ref(),
			Gate:      coldGateIDFor(testWorkspaceID),
			Choice:    &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerColdGate: %v", err)
	}
	if resp.Msg.GetError().GetNoColdGate() == nil {
		t.Fatalf("result = %v, want no_cold_gate", resp.Msg.GetResult())
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "AnswerColdGate", Message: "answered a typed refusal",
		Match: func(r dlog.Record) bool { return r.Context["arm"] == workspace.ArmNoColdGate }}); !ok {
		t.Fatalf("records = %+v, want the no_cold_gate refusal recorded", log.Records())
	}
}

// TestInterruptAnswersNotDetachedWorkAsTheTypedArm pins that the arm
// endpoint_interrupt.proto carries for "the FeedId names no detached item" is
// answered as InterruptError.not_detached_work by BOTH layers that can raise it
// — the transport, when the id will not decode, and the verb, when the decoded
// row is not detached work — rather than as a Connect error under an arm the
// contract does not carry.
func TestInterruptAnswersNotDetachedWorkAsTheTypedArm(t *testing.T) {
	tests := []struct {
		name      string
		detached  *frontendv1.FeedId
		refusedBy *workspace.Refusal
	}{
		{
			name:     "the transport, on an id it cannot decode",
			detached: &frontendv1.FeedId{Value: "not-a-feed-id"},
		},
		{
			name:     "the verb, on a row that is not detached work",
			detached: feedIDFor(testWorkspaceID),
			refusedBy: &workspace.Refusal{
				Arm:    workspace.ArmNotDetachedWork,
				Reason: `row kind "prompt" addresses no detached work`,
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tc.refusedBy != nil {
				h.Verbs.interruptErr = tc.refusedBy
			}

			// Act.
			resp, err := h.Client.Interrupt(context.Background(),
				connect.NewRequest(&agentreplv1.InterruptRequest{
					Workspace: ref(),
					Target:    &agentreplv1.InterruptRequest_Detached{Detached: tc.detached},
				}))

			// Assert.
			if err != nil {
				t.Fatalf("Interrupt = error %v, want the typed not_detached_work arm", err)
			}
			if resp.Msg.GetError().GetNotDetachedWork() == nil {
				t.Fatalf("Interrupt = %v, want InterruptError.not_detached_work", resp.Msg)
			}
		})
	}
}

// TestInterruptAnswersTheShimsNoSessionAsTheTypedArm pins that a turn kill
// the shim refuses for want of a session is InterruptError.no_session.
func TestInterruptAnswersTheShimsNoSessionAsTheTypedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptErr = &workspace.Refusal{
		Rpc: "Interrupt", Arm: workspace.ArmShimNoSession, Reason: "no session is open on this shim",
	}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt = error %v, want the typed no_session arm", err)
	}
	if resp.Msg.GetError().GetNoSession() == nil {
		t.Fatalf("result = %v, want no_session", resp.Msg.GetResult())
	}
}

// passColdGateRequest is a pass of the test workspace's gate by workspace
// "w-parent".
func passColdGateRequest() *agentreplv1.PassColdGateRequest {
	return &agentreplv1.PassColdGateRequest{
		Workspace: ref(),
		Requester: &agentreplv1.WorkspaceRequesterWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "w-parent", Dir: "/tmp/w-parent"}},
	}
}

// TestPassColdGateHandsTheVerbTheGatedWorkspace pins that a pass reaches the
// verb for the gated workspace, and that its record names who passed it.
func TestPassColdGateHandsTheVerbTheGatedWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log

	// Act.
	resp, err := h.Client.PassColdGate(context.Background(), connect.NewRequest(passColdGateRequest()))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("PassColdGate = %v, %v; want success", resp, err)
	}
	if got := h.Verbs.passedColdGates; len(got) != 1 || got[0] != testWorkspaceID {
		t.Fatalf("verb passes = %v, want exactly %q", got, testWorkspaceID)
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.server.pass_cold_gate"})
	if !ok || record.Context["requester"] != "w-parent" {
		t.Fatalf("records = %+v, want the pass recorded with its requester", log.Records())
	}
}

// TestPassColdGateRequiresTheRequestingWorkspace pins that only a workspace
// passes a gate: a request naming none is malformed.
func TestPassColdGateRequiresTheRequestingWorkspace(t *testing.T) {
	tests := []struct {
		name      string
		requester *agentreplv1.WorkspaceRequesterWorkspace
	}{
		{name: "no requester"},
		{name: "a requester naming no workspace", requester: &agentreplv1.WorkspaceRequesterWorkspace{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			req := passColdGateRequest()
			req.Requester = tt.requester

			// Act.
			_, err := h.Client.PassColdGate(context.Background(), connect.NewRequest(req))

			// Assert.
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
			if len(h.Verbs.passedColdGates) != 0 {
				t.Fatalf("verb passes = %v, want none", h.Verbs.passedColdGates)
			}
		})
	}
}

// TestPassColdGateAnswersTheVerbsRefusalAsItsArm pins that the verb's
// refusals reach the caller as PassColdGateError's own arms.
func TestPassColdGateAnswersTheVerbsRefusalAsItsArm(t *testing.T) {
	tests := []struct {
		name string
		arm  string
		got  func(*agentreplv1.PassColdGateError) bool
	}{
		{name: "no gate stands", arm: workspace.ArmNoColdGate,
			got: func(e *agentreplv1.PassColdGateError) bool { return e.GetNoColdGate() != nil }},
		{name: "the gate served no Haiku compaction", arm: workspace.ArmUnservedRemediation,
			got: func(e *agentreplv1.PassColdGateError) bool { return e.GetUnservedRemediation() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.passColdGateErr = &workspace.Refusal{Arm: tt.arm, Reason: tt.name}

			// Act.
			resp, err := h.Client.PassColdGate(context.Background(), connect.NewRequest(passColdGateRequest()))

			// Assert.
			if err != nil {
				t.Fatalf("PassColdGate: %v", err)
			}
			if !tt.got(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.arm)
			}
		})
	}
}
