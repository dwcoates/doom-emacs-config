//go:build integration

package integration

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

func TestACompletedForegroundBashDrawsOneSucceededToolCardAndNoDetachedShell(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	f.submit("go", "k-bash-foreground", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const line = "pwd; ls | head"
	bash := func(result *conversationv1.AgentBash) *conversationv1.AgentFrame {
		return activityFrame(mainAgent, &conversationv1.AgentActivity{
			ActivityId: activityID("bash-foreground-1"),
			Item:       &conversationv1.AgentActivity_Bash{Bash: result},
		})
	}

	// Act: the start and the completed settle the shim emits, in that order.
	f.shim.PushAgentFrame(mainAgent, bash(&conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
		Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: line}, StartedAt: startedAt(1)},
	}}))
	f.shim.PushAgentFrame(mainAgent, bash(&conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
		Command: &conversationv1.AgentBashCommand{Line: line},
		Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
			Output: &conversationv1.AgentBashOutput{Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{
				Stdout: "one\ntwo\n",
				Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}},
			}}},
		}},
		SettledAt: settledAt(2),
	}}}))

	// Assert: the one Bash card, headed by its command, settled succeeded with
	// the command's stdout.
	row := awaitRow(t, f, tail, "the Bash card's settle", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned() != nil
	})
	card := row.GetActivity().GetSimpleToolCall()
	if card.GetName().GetText() != "Bash" || !strings.Contains(card.GetInput().GetText(), line) {
		t.Fatalf("card = %v, want a Bash card carrying %q", card, line)
	}
	if card.GetReturned().GetSucceeded() == nil {
		t.Fatalf("Bash outcome = %v, want succeeded (a completed command draws succeeded whatever its exit code)", card.GetReturned())
	}
	if got := card.GetReturned().GetText().GetText(); !strings.Contains(got, "one") || !strings.Contains(got, "two") {
		t.Fatalf("returned text = %q, want the command's stdout", got)
	}
	page, _ := f.openFeed(nil)
	if shell := findRow(page, func(r *frontendv1.FeedRow) bool { return r.GetShellHead() != nil || r.GetDetachedShell() != nil }); shell != nil {
		t.Fatalf("a foreground Bash round trip drew a detached shell row: %v", shell)
	}
}
