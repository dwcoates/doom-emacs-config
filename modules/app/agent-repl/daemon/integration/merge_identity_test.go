//go:build integration

package integration

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// TestALandingCarryingAnOutsideAuthorFailsAndLeavesMainUnmoved proves the
// merge queue's identity check end to end: a branch whose commit another
// identity authored passes its gate, and the landing still refuses it before
// main moves (internal/merge/identity.go).
func TestALandingCarryingAnOutsideAuthorFailsAndLeavesMainUnmoved(t *testing.T) {
	t.Parallel()
	// Arrange: a passing gate, and the workspace's one commit authored by an
	// outside contributor.
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	repo.DeploysAsAgentRepl()
	// A merge-spanning wait chains dozens of subprocesses; see harness.MergeChainTimeout.
	d := harness.StartDaemon(t, harness.Opts{Timeout: harness.MergeChainTimeout, ExtraEnv: []string{harness.TestAllScriptEnv(script.Path)}})
	repoRef := mergeRepositoryRef(t, d, repo)
	f := mergeCreateChild(t, d, repoRef, "outsideauthor", "land the contribution", nil)
	root := f.watchRootFeed()
	sha := harness.CommitWork(t, f.ws.GetDir())
	repo.SetCommitAuthor(sha, "Outside Contributor", "oc@example.org")
	mainBefore := repo.BranchHead(harness.DefaultBranch)
	d.RequireWarnings("daemon.merge.identity")

	// Act.
	if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
		t.Fatalf("FinishWorkspace = error %v, want the deploy enqueued", err)
	}

	// Assert: the merge fails, and main never moved.
	awaitRow(t, f, root, "the merge's failed terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetError() != nil
	})
	if got := repo.BranchHead(harness.DefaultBranch); got != mainBefore {
		t.Fatalf("main moved to %s after a refused landing, want it still at %s", got, mainBefore)
	}
	// The failure is handed back to the requester as a turn of its own.
	f.shim.ExpectStartTurn()
	pushConcludedTurn(f.shim, mainAgent, "noted")
}
