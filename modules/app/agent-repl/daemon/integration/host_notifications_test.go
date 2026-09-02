//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// ==========================================================================
// Host notifications (critique 6): the kind is typed
// (agentrepl/v1/endpoint_watch_host_workspace.proto HostNotificationKind),
// and the composed line rides the envelope (HostWorkspaceNotification.text)
// rather than the per-kind message, which is why question_asked's own
// payload is `header` (HostNotificationQuestionAsked.header) and
// agent_addressed's is empty (HostNotificationAgentAddressed{}).
// ==========================================================================

// TestQuestionAskedNotificationCarriesItsHeaderAndSetsAttention pins
// HostNotificationKind.question_asked: the first question's chip label rides
// the notification's own `header` field
// (internal/sessionwatcher/route.go's notifyQuestionLocked), and the same
// notification is what raises the roster's attention marker — every
// notification kind does, alike (internal/workspace/notify.go's Notify sets
// it unconditionally once note.Kind is set).
func TestQuestionAskedNotificationCarriesItsHeaderAndSetsAttention(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-notify-question", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	roster := f.d.WatchRoster()

	// Act: a question batch blocks the agent, per feed_test.go's
	// TestQuestionStartDrawsAnOpenRow pushing idiom.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: "q-notify"},
			Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
				Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
					Question: &conversationv1.AgentQuestionText{Text: "Which auth method?"},
					Header:   "Auth method",
					Choices: &conversationv1.AgentQuestionAsked_SingleSelect{SingleSelect: &conversationv1.AgentQuestionSingleSelect{
						Options: []*conversationv1.AgentQuestionOption{{Label: &conversationv1.AgentQuestionOptionLabel{Label: "OAuth"}}},
					}},
				}}},
				StartedAt: startedAt(1),
			}},
		}},
	}))

	// Assert: the host notification carries the header.
	push := harness.AwaitView(t, f.d.Ctx(), f.host, "the question_asked host notification", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetNotification().GetKind().GetQuestionAsked() != nil
	})
	if got := push.GetNotification().GetKind().GetQuestionAsked().GetHeader(); got != "Auth method" {
		t.Fatalf("question_asked.header = %q, want %q", got, "Auth method")
	}

	// Assert: the same notification set the roster's attention marker.
	awaitRoster(t, f.d, roster, "the attention marker set by the question_asked notification", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetAttention() != nil
	})
}

// TestAgentAddressedNotificationCarriesItsText pins
// HostNotificationKind.agent_addressed: an AgentPushNotification start frame
// is the agent reaching an ABSENT user, and the local attention presentation
// is this system's own fan-out of it (agent_activity.proto). The pushed
// message rides the envelope's `text`; the kind arm itself carries no fields.
func TestAgentAddressedNotificationCarriesItsText(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-notify-addressed", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act: the agent sends a push notification directly to the user.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("push-1"),
		Item: &conversationv1.AgentActivity_PushNotification{PushNotification: &conversationv1.AgentPushNotification{
			State: &conversationv1.AgentPushNotification_Start{Start: &conversationv1.AgentPushNotificationStart{
				Message:   "the deploy finished",
				StartedAt: startedAt(1),
			}},
		}},
	}))

	// Assert
	push := harness.AwaitView(t, f.d.Ctx(), f.host, "the agent_addressed host notification",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			return r.GetNotification().GetKind().GetAgentAddressed() != nil
		})
	if got := push.GetNotification().GetText(); got != "the deploy finished" {
		t.Fatalf("agent_addressed notification text = %q, want the pushed message verbatim", got)
	}
}
