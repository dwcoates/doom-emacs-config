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
		&agentreplv1.CreateWorkspaceMergeActions{BeforeWsMerge: said("hold " + namePrefix + "'s queue open")})
	behind = mergeCreateChild(t, d, repoRef, namePrefix+"-behind", namePrefix+" behind work", nil)

	harness.CommitWork(t, front.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: front.ws, Source: ownBranch()})); err != nil {
		t.Fatalf("MergeWorkspace(%s front) = error %v, want the merge enqueued", namePrefix, err)
	}
	// The pre-prompt's turn is started and NEVER answered: the run sits in it
	// holding the repository lock.
	front.shim.ExpectStartTurn()

	harness.CommitWork(t, behind.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: behind.ws, Source: ownBranch()})); err != nil {
		t.Fatalf("MergeWorkspace(%s behind) = error %v, want the merge enqueued", namePrefix, err)
	}
	return front, behind, repo, repoRef
}

// awaitLandingDeployed waits for the ONE deploy a landing in the daemon's own
// checkout runs. A landing now always carries work (harness.CommitWork), so it
// always deploys, and a test that ends mid-build kills the build under the
// deploy and reads the kill as a failed deploy.
func awaitLandingDeployed(t *testing.T, d *harness.Daemon) {
	t.Helper()
	d.AwaitLogRecord(d.RunLogPath(), "the landing's deploy", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.deploy.landing" && r.Message == "the landing is deployed"
	})
}

// ownBranch is the requester's own branch, closed once it lands: what every
// MergeWorkspace a test makes for a workspace's own work merges.
func ownBranch() *agentreplv1.MergeWorkspaceSource {
	return &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_OwnBranch{OwnBranch: &agentreplv1.MergeWorkspaceSourceOwnBranch{}}}
}
