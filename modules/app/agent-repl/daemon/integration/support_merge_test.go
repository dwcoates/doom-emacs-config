//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// support_merge_test.go holds helpers shared only by this suite's newly added
// tests (audit1-c), kept separate from merge_test.go's own long-standing
// merge-prefixed helpers at that file's bottom.

// mergeBlockedRepoOn builds one MORE blocked-queue repository under an
// EXISTING daemon: a front child whose branch is scripted to conflict (so its
// merge parks forever, pinning the queue) and a second child queued behind it.
// It mirrors merge_test.go's own mergeBlockedQueueFixture, parameterized on a
// daemon the caller already started, so a test can hold several repositories'
// queues open at once under one daemon.
func mergeBlockedRepoOn(t *testing.T, d *harness.Daemon, namePrefix string) (front, behind *fixture, repo *harness.Repo, repoRef *workspacev1.RepositoryRef) {
	t.Helper()
	repo = harness.NewRepo(t)
	repoRef = mergeRepositoryRef(t, d, repo)

	front = mergeCreateChild(t, d, repoRef, namePrefix+"-front", namePrefix+" front work", nil)
	behind = mergeCreateChild(t, d, repoRef, namePrefix+"-behind", namePrefix+" behind work", nil)

	frontBranch := mergeBranchOf(t, front.ws)
	repo.ScriptConflict(repo.Dir, frontBranch, "conflict.txt")

	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: front.ws})); err != nil {
		t.Fatalf("MergeWorkspace(%s front) = error %v, want the merge enqueued", namePrefix, err)
	}
	front.shim.ExpectStartTurn()
	front.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID(namePrefix+"-front-conflict-brief")))

	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: behind.ws})); err != nil {
		t.Fatalf("MergeWorkspace(%s behind) = error %v, want the merge enqueued", namePrefix, err)
	}
	return front, behind, repo, repoRef
}
