//go:build integration

package integration

import (
	"os"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/integration/harness"
	"claude-repld/internal/classifier"
	"claude-repld/internal/prompts"
)

// rewriteLine is the rule the fake rewrite adds to the routing brief.
const rewriteLine = "Interrupt whenever a message says something must happen after something else."

// rewritingClaudeEnv names a fake claude that answers the rewrite call with
// the brief it was shown, plus rewriteLine: a usable rewrite, slots intact.
func rewritingClaudeEnv(t *testing.T) []string {
	t.Helper()
	script := "#!/bin/sh\n" +
		"sed -n '/^<current-prompt>$/,/^<\\/current-prompt>$/p' | sed '1d;$d'\n" +
		"printf '%s\\n' '" + rewriteLine + "'\n"
	return []string{scriptedClaudeEnv(t, script)}
}

func updateClassifierPromptRequest() *agentreplv1.UpdateClassifierPromptRequest {
	return &agentreplv1.UpdateClassifierPromptRequest{
		Instruction: "always interrupt when a prompt says something must happen after something else",
		Example: &agentreplv1.ClassifiedPromptExample{
			Text:  "after the tests pass, bump the version",
			Route: agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_HOLD_FOR_TURN_END,
		},
	}
}

// TestAnUpdatedClassifierPromptIsRewrittenAndCommitted is UpdateClassifierPrompt
// end to end: the daemon asks its headless rewrite, writes the answer under
// the brief's own header, and commits that one path in the checkout holding
// it.
func TestAnUpdatedClassifierPromptIsRewrittenAndCommitted(t *testing.T) {
	t.Parallel()
	// Arrange: the prompts directory is a checkout of its own.
	d := newDaemon(t, harness.Opts{ExtraEnv: rewritingClaudeEnv(t)})
	repo := harness.NewRepoAt(t, d.PromptsDir)

	// Act.
	resp, err := d.Client().UpdateClassifierPrompt(d.Ctx(), connect.NewRequest(updateClassifierPromptRequest()))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateClassifierPrompt = (%v, %v), want success", resp, err)
	}
	if got, want := resp.Msg.GetSuccess().GetCommit(), repo.BranchHead(harness.DefaultBranch); got != want {
		t.Fatalf("answered commit %q, want the checkout's new head %q", got, want)
	}
	brief, err := prompts.Load(d.PromptsDir, classifier.BriefRouting)
	if err != nil {
		t.Fatalf("the rewritten brief does not load: %v", err)
	}
	if !strings.Contains(brief.Body, rewriteLine) || !strings.Contains(brief.Body, "{{token_hold}}") {
		t.Fatalf("the rewritten brief lacks the new rule or a placeholder:\n%s", brief.Body)
	}
}

// TestAnUnusableClassifierRewriteLeavesTheBriefAlone pins the refusal path end
// to end: the harness's own fake claude answers a routing token, which is no
// brief, so nothing is written or committed.
func TestAnUnusableClassifierRewriteLeavesTheBriefAlone(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{})
	// The rejection is recorded at ERROR where the updater refuses it.
	d.RequireWarnings("daemon.classifierupdate.update")
	repo := harness.NewRepoAt(t, d.PromptsDir)
	path := prompts.Path(d.PromptsDir, classifier.BriefRouting)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the brief: %v", err)
	}
	head := repo.BranchHead(harness.DefaultBranch)

	// Act.
	resp, err := d.Client().UpdateClassifierPrompt(d.Ctx(), connect.NewRequest(updateClassifierPromptRequest()))

	// Assert.
	if err != nil || resp.Msg.GetError().GetRewriteRejected() == nil {
		t.Fatalf("UpdateClassifierPrompt = (%v, %v), want rewrite_rejected", resp, err)
	}
	after, _ := os.ReadFile(path)
	if string(after) != string(before) || repo.BranchHead(harness.DefaultBranch) != head {
		t.Fatalf("a rejected rewrite changed the brief or committed")
	}
}
