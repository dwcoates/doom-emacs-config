//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

func TestAQueryThatDiesWithAnAskOpenDrawsTheAskDeniedAndTheTurnErrored(t *testing.T) {
	t.Parallel()
	// Arrange: the death's own records are the fault under test.
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.sessionwatcher.query_died", "daemon.feed.query_died")
	f.submit("go", "k-querydied-mid-ask", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const gated = "gated-mid-ask"

	// Act: the order the shim emits them in. The ask opens, the dying query's
	// stand-down settles it denied by the user, then the death is reported.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: standingPermission(gated, gated)},
	}))
	asked := awaitRow(t, f, tail, "the ask opened before the death", func(r *frontendv1.FeedRow) bool {
		return r.GetPermission().GetOpen() != nil
	})
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: &conversationv1.AgentPermission{
			Id:        &conversationv1.AgentPermissionId{Value: gated},
			GatedCall: activityID(gated),
			Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{
				Decision: &conversationv1.AgentPermissionSuccess_Denied{Denied: &conversationv1.AgentPermissionDenied{
					By: &conversationv1.AgentPermissionDenied_User{User: &conversationv1.AgentPermissionDeniedByUser{Message: "the query died"}},
				}},
			}},
		}},
	}))
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{
			Cause: &conversationv1.SessionQueryDied_UnexpectedEof{UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{}},
		}},
	})

	// Assert: the same row that opened settles denied_by_user, and the turn
	// ends on the query_died arm.
	settled := awaitRow(t, f, tail, "the open ask settling denied", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == asked.GetId().GetValue() && r.GetPermission().GetAnswered() != nil
	})
	if settled.GetPermission().GetAnswered().GetDeniedByUser() == nil {
		t.Fatalf("settled permission = %v, want denied_by_user", settled.GetPermission().GetAnswered())
	}
	ended := awaitRow(t, f, tail, "the query-died terminal", func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded().GetErrored().GetQueryDied() != nil
	})
	if ended.GetTurnEnded().GetErrored().GetQueryDied().GetUnexpectedEof() == nil {
		t.Fatalf("query_died cause = %v, want unexpected_eof", ended.GetTurnEnded().GetErrored().GetQueryDied())
	}
}
