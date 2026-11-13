//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

func TestAnAllowStandingAnswerSettlesTheAskAllowedStandingAndTheGatedBashDrawsSucceeded(t *testing.T) {
	t.Parallel()
	// Arrange: a gated Bash whose ask offers a standing rule, the order the
	// shim emits them in.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-allow-standing", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const gated = "gated-standing"
	const line = "ls -la"
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(gated),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: line}, StartedAt: startedAt(1)},
		}}},
	}))
	offer := standingPermission(gated, gated)
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: offer},
	}))
	ask := awaitRow(t, f, tail, "the open ask", func(r *frontendv1.FeedRow) bool { return r.GetPermission().GetOpen() != nil })
	if ask.GetPermission().GetStandingOffered() == nil {
		t.Fatalf("the open ask = %v, want the offered standing drawn", ask.GetPermission())
	}

	// Act: the user allows it standing, and the shim reports the ask settled
	// and the gated call run.
	resp, err := f.d.Client().AnswerPermission(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
		Workspace:  f.ws,
		Permission: ask.GetId(),
		Answer:     &agentreplv1.AnswerPermissionRequest_AllowStanding{AllowStanding: &agentreplv1.AnswerPermissionAllowStanding{}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerPermission{allow_standing} = %v, %v, want a success", resp, err)
	}
	f.shim.ExpectUpdateAgent()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: &conversationv1.AgentPermission{
			Id:        &conversationv1.AgentPermissionId{Value: gated},
			GatedCall: activityID(gated),
			Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{
				Decision: &conversationv1.AgentPermissionSuccess_Allowed{Allowed: &conversationv1.AgentPermissionAllowed{
					Scope: &conversationv1.AgentPermissionAllowed_Standing{Standing: &conversationv1.AgentPermissionAllowedStanding{
						Standing: offer.GetStart().GetOfferedStanding(),
					}},
				}},
			}},
		}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(gated),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{
			Success: &conversationv1.AgentBashSuccess{
				Command: &conversationv1.AgentBashCommand{Line: line},
				Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
					Output: &conversationv1.AgentBashOutput{Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{Stdout: "ok\n"}}},
				}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert: the ask's own card settles allowed_standing, and the gated Bash
	// settles succeeded.
	answered := awaitRow(t, f, tail, "the ask settled allowed standing", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == ask.GetId().GetValue() && r.GetPermission().GetAnswered() != nil
	})
	if answered.GetPermission().GetAnswered().GetAllowedStanding() == nil {
		t.Fatalf("answered permission = %v, want allowed_standing", answered.GetPermission().GetAnswered())
	}
	call := awaitRow(t, f, tail, "the gated Bash settled", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned() != nil
	})
	if call.GetActivity().GetSimpleToolCall().GetReturned().GetSucceeded() == nil {
		t.Fatalf("gated Bash outcome = %v, want returned.succeeded", call.GetActivity().GetSimpleToolCall())
	}
}
