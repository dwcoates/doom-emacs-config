//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/deployconfig"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// support_merge_test.go holds helpers shared only by this suite's newly added
// tests (audit1-c), kept separate from merge_test.go's own long-standing
// merge-prefixed helpers at that file's bottom.

// mergeBlockedRepoOn builds one MORE blocked-queue repository under an
// EXISTING daemon: a front child whose merge stops on a configured
// before-merge prompt nobody answers (so its run holds the repository lock
// indefinitely, pinning the queue) and a second child queued behind it.
//
// The pre-prompt is what blocks it, NOT a scripted conflict: a conflict only
// arises under the no-ff merge of the daemon's OWN checkout (internal/merge's
// emacsMethod), and at most one repository under one daemon can be that
// checkout — so a scripted conflict on a second repository lands a clean,
// instantly finished merge instead of a park. The before-merge prompt runs
// under BOTH methods, which is what lets several repositories hold their
// queues open at once.
func mergeBlockedRepoOn(t *testing.T, d *harness.Daemon, namePrefix string) (front, behind *fixture, repo *harness.Repo, repoRef *workspacev1.RepositoryRef) {
	t.Helper()
	repo = harness.NewRepo(t)
	repoRef = mergeRepositoryRef(t, d, repo)

	front = mergeCreateChild(t, d, repoRef, namePrefix+"-front", namePrefix+" front work",
		preDeployPrompts("hold "+namePrefix+"'s queue open"))
	behind = mergeCreateChild(t, d, repoRef, namePrefix+"-behind", namePrefix+" behind work", nil)

	harness.CommitWork(t, front.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(front.ws))); err != nil {
		t.Fatalf("MergeWorkspace(%s front) = error %v, want the merge enqueued", namePrefix, err)
	}
	// The pre-prompt's turn is started and NEVER answered: the run sits in it
	// holding the repository lock.
	front.shim.ExpectStartTurn()

	harness.CommitWork(t, behind.ws.GetDir())
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(behind.ws))); err != nil {
		t.Fatalf("MergeWorkspace(%s behind) = error %v, want the merge enqueued", namePrefix, err)
	}
	return front, behind, repo, repoRef
}

// promptHooks are TEXTS as prompt hooks, in order.
func promptHooks(texts ...string) []*repoconfigv1.DeployHook {
	var out []*repoconfigv1.DeployHook
	for _, text := range texts {
		out = append(out, deployconfig.PromptHook(text))
	}
	return out
}

// preDeployPrompts are a request's deploy hooks: TEXTS as its pre-deploy
// prompts.
func preDeployPrompts(texts ...string) *agentreplv1.CreateWorkspaceDeployHooks {
	return &agentreplv1.CreateWorkspaceDeployHooks{PreDeploy: promptHooks(texts...)}
}

// postDeployPrompts are a request's deploy hooks: TEXTS as its post-deploy
// prompts.
func postDeployPrompts(texts ...string) *agentreplv1.CreateWorkspaceDeployHooks {
	return &agentreplv1.CreateWorkspaceDeployHooks{PostDeploy: promptHooks(texts...)}
}

// isMergeTerminal reports whether a feed row is a merge bubble's terminal:
// its head landed (success) or ended in error. It is the one predicate every
// test awaiting a merge's end reads.
func isMergeTerminal(row *frontendv1.FeedRow) bool {
	m := row.GetActivity().GetMerge()
	return m.GetSuccess() != nil || m.GetError() != nil
}

func TestIsMergeTerminalReadsALandingOrAnErrorAndNothingElse(t *testing.T) {
	tests := []struct {
		name string
		row  *frontendv1.FeedRow
		want bool
	}{
		{name: "landed", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{Result: &frontendv1.FeedMerge_Success{Success: &frontendv1.FeedMergeSuccess{}}}}}}}, want: true},
		{name: "errored", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{Result: &frontendv1.FeedMerge_Error{Error: &frontendv1.FeedMergeError{}}}}}}}, want: true},
		{name: "running", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{}}}}}, want: false},
		{name: "no merge", row: &frontendv1.FeedRow{}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert.
			if got := isMergeTerminal(tt.row); got != tt.want {
				t.Fatalf("isMergeTerminal = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestEveryAwaitOfAMergesEndReadsIsMergeTerminal(t *testing.T) {
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, file := range files {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}

		// Act / Assert: the predicate is spelled once, in isMergeTerminal.
		if n := strings.Count(string(body), "GetSuccess() != nil || m.GetError() != nil") + strings.Count(string(body), "GetMerge().GetSuccess() != nil || row.GetActivity().GetMerge().GetError() != nil"); n > 0 && file != "support_merge_test.go" {
			t.Fatalf("%s spells a merge's end by hand %d times instead of isMergeTerminal", file, n)
		}
	}
}
