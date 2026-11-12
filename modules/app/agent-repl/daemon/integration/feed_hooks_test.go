//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

// hookFrame wraps one AgentHook arm as the frame the shim pushes for the firing.
func hookFrame(id string, hook *conversationv1.AgentHook) *conversationv1.AgentFrame {
	return activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item:       &conversationv1.AgentActivity_Hook{Hook: hook},
	})
}

func hookStart(name string, event conversationv1.AgentHookEvent) *conversationv1.AgentHook {
	return &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Start{Start: &conversationv1.AgentHookStart{
		HookName: name, Event: event, StartedAt: startedAt(1),
	}}}
}

func hookCards(page *frontendv1.FeedPage) []*frontendv1.FeedHook {
	var cards []*frontendv1.FeedHook
	for _, row := range page.GetSuccess().GetRows() {
		if hook := row.GetActivity().GetHook(); hook != nil {
			cards = append(cards, hook)
		}
	}
	return cards
}

func TestABlockingHookDrawsOneCardCarryingItsReason(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-hook-blocked", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, hookFrame("hook-blocked-1", hookStart("PostToolUse:Edit", conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_POST_TOOL_USE)))
	f.shim.PushAgentFrame(mainAgent, hookFrame("hook-blocked-1", &conversationv1.AgentHook{Result: &conversationv1.AgentHook_BlockingError{
		BlockingError: &conversationv1.AgentHookBlockingError{Command: "PostToolUse:Edit", BlockingText: "the suite failed after the edit"},
	}}))

	// Assert
	row := awaitRow(t, f, tail, "the blocked hook's card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetHook().GetBlocked() != nil
	})
	if got := row.GetActivity().GetHook().GetBlocked().GetReason(); got != "the suite failed after the edit" {
		t.Fatalf("blocked hook reason = %q, want the hook's stated refusal", got)
	}
	page, _ := f.openFeed(nil)
	if cards := hookCards(page); len(cards) != 1 {
		t.Fatalf("hook cards = %d (%v), want exactly one for the one firing", len(cards), cards)
	}
}

func TestAFailedHookDrawsOneCardCarryingItsExitCodeAndOutput(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-hook-failed", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, hookFrame("hook-failed-1", hookStart("SessionStart:startup", conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_SESSION_START)))
	stderr := "Failed to run: no interpreter on PATH."
	f.shim.PushAgentFrame(mainAgent, hookFrame("hook-failed-1", &conversationv1.AgentHook{Result: &conversationv1.AgentHook_NonBlockingError{
		NonBlockingError: &conversationv1.AgentHookNonBlockingError{
			Command: "SessionStart:startup", ExitCode: 1, Output: &conversationv1.AgentHookOutput{Stderr: stderr},
		},
	}}))

	// Assert
	row := awaitRow(t, f, tail, "the failed hook's card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetHook().GetFailed() != nil
	})
	failed := row.GetActivity().GetHook().GetFailed()
	if failed.GetExitCode() != 1 || failed.GetOutput().GetText() != stderr {
		t.Fatalf("failed hook = exit %d output %q, want exit 1 and %q", failed.GetExitCode(), failed.GetOutput().GetText(), stderr)
	}
	page, _ := f.openFeed(nil)
	if cards := hookCards(page); len(cards) != 1 {
		t.Fatalf("hook cards = %d (%v), want exactly one for the one firing", len(cards), cards)
	}
}
