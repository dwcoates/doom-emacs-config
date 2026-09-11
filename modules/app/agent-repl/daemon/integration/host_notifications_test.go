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
	t.Parallel()
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
	t.Parallel()
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

// TestOpeningAWorkspaceLogsNoHostIdentityError guards the realtest-1 regression:
// right after Emacs subscribes to WatchHostWorkspace, the shim may not yet have
// described the session, so a briefly missing host identity is a STARTUP
// TRANSIENT and must never be recorded as the compose_host_workspace ERROR that
// names a mint defect. Once the host view carries an identity, the boot is past
// that window, so neither the workspace daemon log nor the run log may hold that
// ERROR.
func TestOpeningAWorkspaceLogsNoHostIdentityError(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace whose host stream is held from open.
	f := newOpened(t, harness.Opts{})

	// Act: wait until the session is fully described — the host view carries a
	// minted identity — so the boot's identity window has certainly closed.
	harness.AwaitView(t, f.d.Ctx(), f.host, "the host session identity",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			return r.GetHost().GetExisting().GetId().GetValue() != ""
		})

	// Assert: no host-identity ERROR was recorded on either sink.
	const wantOp = "daemon.server.compose_host_workspace"
	const wantMsg = "a session record carries no host identity; the host view was withheld"
	assertNoHostIdentityError := func(where string, records []harness.LogRecord) {
		for _, r := range records {
			if r.Level == "ERROR" && r.Operation == wantOp && r.Message == wantMsg {
				t.Fatalf("the %s recorded a host-identity ERROR during a healthy boot: %+v", where, r)
			}
		}
	}
	assertNoHostIdentityError("workspace daemon log",
		f.d.WorkspaceLog(f.repo.Dir, "daemon"))
	assertNoHostIdentityError("run log", f.d.RunLog())
}
