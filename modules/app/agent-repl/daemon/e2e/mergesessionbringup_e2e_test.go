// THE SESSION A MERGE RUN NEEDS — PART OF THE ACCEPTANCE GATE (see
// mergepipeline_e2e_test.go for what that means and how to run it).
//
// The pipeline's actions are AGENT TURNS, and a turn needs a live shim. So the
// run has to answer a question the queue and the cherry-pick never had to:
// what if the workspace's session is not currently up?
//
// There are two answers, and they are opposite, which is why both are here:
//
//   - HIBERNATED is recoverable. Hibernation is a deliberate reclamation of
//     memory from an idle session, not a death: the registry record is intact
//     and rehydratable. So the run brings the session UP and proceeds. Failing
//     here would mean a workspace became unmergeable by sitting idle, which is
//     the one thing hibernation must never cost.
//   - DELETED is not. The user destroyed the session on purpose. Bringing one
//     back to run a merge action would resurrect exactly what they asked to be
//     rid of, so the run fails and SAYS SO.
//
// WHY BOTH ROWS CONFIGURE A BEFORE-ACTION. Without one the pipeline never
// needs the session at all, and every assertion here would pass against a
// producer that ignored sessions entirely. The action is what makes the
// session load-bearing.
//
// Reuses mergepipeline's helpers, mergequeue's fixtures, clearcompact's
// liveSession and connectivitystate's observedStates READ-ONLY.
package e2e

import (
	"fmt"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// noLiveShimComplaints are the phrasings a run must NOT fail with when the
// session was merely hibernated. They are the shapes the daemon uses when it
// genuinely has no route to a shim, and seeing one here means the run treated
// a recoverable absence as a terminal one instead of rehydrating.
var noLiveShimComplaints = []string{
	"no live shim",
	"no matching live session controller",
	"no live session",
	"hibernated",
}

// assertNotANoLiveShimFailure fails when cause reads like the run gave up on a
// session it should have brought back.
func assertNotANoLiveShimFailure(t *testing.T, ws, cause string) {
	t.Helper()
	lower := strings.ToLower(cause)
	for _, complaint := range noLiveShimComplaints {
		if strings.Contains(lower, complaint) {
			t.Errorf("the merge of %s failed with %q: a hibernated session is rehydratable, so the run must bring it up rather than report its absence as terminal",
				ws, cause)
			return
		}
	}
}

// TestE2EMergePipelineBringsUpAHibernatedSessionAndProceeds is SPEC ITEM 3: a
// merge against a hibernated workspace rehydrates the session first, then runs
// the pipeline to completion.
//
// THE PHASE STREAM IS THE PROOF, not a liveness probe. `before_action` can
// only be published by a run that had a session to submit the action's turn
// to, and `merged` can only follow a run that then did the whole job. A test
// that instead asked "is the session up now?" would be satisfied by any
// bring-up from any cause, including one this merge did not perform.
func TestE2EMergePipelineBringsUpAHibernatedSessionAndProceeds(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a real session on the source worktree, driven into hibernation
	// by the daemon's own idle sweeper (the production trigger), and a
	// before-action so the run genuinely needs that session back.
	const (
		branch  = "feature-hibernated-merge"
		commits = 2
	)
	h := newUDSHarness(t, withIdleSweeper(), withMergeActions("summarize what this workspace changed", ""))
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	wsDir := repo.multiCommitWorktree(branch, commits)
	_, conn, _, _ := liveSession(t, h, wsDir)
	w := newMergeWatch(t, conn)
	h.sweepIdle <- time.Now()
	w.until("a WorkspaceState reporting "+wsDir+" hibernated", func() bool {
		for _, state := range w.states {
			if state.GetWorkspace() == wsDir && state.GetState() == frontendv1.RenderState_RENDER_STATE_HIBERNATED {
				return true
			}
		}
		return false
	})

	// Act — merge the workspace whose session is now asleep.
	sendMerge(t, conn, "r-merge-hibernated", mergeCmdFor(t, h.geometry, repo, wsDir, branch))
	w.awaitOKAck("r-merge-hibernated")

	// Assert — the action's turn ran (so the session came back) and the run
	// completed, with no failure blaming the absence.
	w.awaitStatusArm(wsDir, armBeforeAction)
	w.awaitStatusArm(wsDir, armMerged)
	assertArmOrder(t, w, wsDir, []string{armEnqueued, armBeforeAction, armCherryPicking, armMerged})
	assertStatusStreamWellFormed(t, w, wsDir)
	if failed := w.firstStatusArm(wsDir, armFailed); failed != nil {
		assertNotANoLiveShimFailure(t, wsDir, failed.GetFailed().GetCause())
		t.Errorf("the merge of the hibernated workspace %s failed: %s", wsDir, failed.GetFailed().GetCause())
	}
	for i := 1; i <= commits; i++ {
		name := fmt.Sprintf("%s-%d.txt", branch, i)
		if !strings.Contains(readMergeFile(t, repo.target, name), "hello from "+branch) {
			t.Errorf("%s reported merged but %s is not in the target", wsDir, name)
		}
	}
}

// TestE2EMergePipelineFailsAMergeOfADeletedSession is SPEC ITEM 4: the
// workspace's newest session is terminal BY DELETION, so the run fails with a
// cause that says so.
//
// WHY IT MUST SAY SO. Every terminal cause reaches a user, and "the merge
// failed" is unactionable where "the session you deleted was the one this
// merge needed" is a next step. The assertion is on the WORDING for exactly
// that reason — a generic cause here is the defect, not a cosmetic issue.
//
// WHY NOT SIMPLY REHYDRATE. A deleted session is not an absent one. Bringing
// it back to run a merge action would resurrect precisely what the user
// destroyed, which is why this row and the hibernation row above must resolve
// in opposite directions rather than through one "is there a shim?" check.
func TestE2EMergePipelineFailsAMergeOfADeletedSession(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a real session on the source worktree, then deleted through the
	// frontend's own command, and a before-action so the run needs it.
	const branch = "feature-deleted-session-merge"
	h := newUDSHarness(t, withMergeActions("summarize what this workspace changed", ""))
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	wsDir := repo.multiCommitWorktree(branch, 2)
	headBefore := strings.TrimSpace(mergeGit(t, repo.target, "rev-parse", "HEAD"))
	sessionID, conn, _, _ := liveSession(t, h, wsDir)
	w := newMergeWatch(t, conn)
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":"r-delete-session","workspace":%q,"deleteSession":{"sessionId":%q}}`,
		wsDir, sessionID))
	w.awaitOKAck("r-delete-session")

	// Act.
	sendMerge(t, conn, "r-merge-deleted", mergeCmdFor(t, h.geometry, repo, wsDir, branch))
	w.awaitOKAck("r-merge-deleted")
	failed := w.awaitStatusArm(wsDir, armFailed)

	// Assert — failed, with a cause naming the deletion, and nothing landed.
	assertStatusStreamWellFormed(t, w, wsDir)
	cause := failed.GetFailed().GetCause()
	if cause == "" {
		t.Fatal("the failed status carries an empty cause: the user is told nothing about why their merge could not run")
	}
	if !strings.Contains(strings.ToLower(cause), "delet") {
		t.Errorf("the failure cause for %s reads %q, which never says the session was DELETED: the user cannot tell this from a merge that broke for an unrelated reason",
			wsDir, cause)
	}
	assertNoArm(t, w, wsDir, armMerged, "the session the merge's action needed was deleted, so the run cannot have completed")
	if got := strings.TrimSpace(mergeGit(t, repo.target, "rev-parse", "HEAD")); got != headBefore {
		t.Errorf("the target HEAD moved from %s to %s although the run failed on a deleted session", headBefore, got)
	}
}
