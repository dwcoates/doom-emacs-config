// THE MERGE PIPELINE'S BEFORE- AND AFTER-ACTIONS — PART OF THE ACCEPTANCE GATE
// (see mergepipeline_e2e_test.go for what that means and how to run it).
//
// A workspace can be CREATED carrying two agent turns the merge is supposed to
// run for it: `before_ws_merge`, which runs before any commit is picked, and
// `postprocessing_prompt`, which runs once every commit has landed. Both are
// creation-time facts read back at merge time, keyed by the worktree path.
//
// WHY THE TWO FAILURES ARE CLASSIFIED DIFFERENTLY, which is the whole point of
// this file:
//
//   - A BEFORE-action failure FAILS THE RUN. Nothing has been picked yet, so
//     there is nothing to preserve and no reason to proceed: the action was
//     the user's stated precondition for merging at all. Landing commits after
//     it errored would merge work the user asked to gate.
//   - An AFTER-action failure DOES NOT. Every commit is already on the target
//     before the action starts. Reporting the run failed would say the merge
//     did not happen, when it demonstrably did, and no frontend could tell the
//     difference from a run that landed nothing. So the terminal phase stays
//     `merged` and the error rides on it as after_action_error.
//
// Reuses mergepipeline's helpers and mergequeue's fixtures READ-ONLY.
package e2e

import (
	"fmt"
	"strings"
	"testing"
)

// BeforeWSMergePrompt satisfies the pipeline's before-action source on the
// e2e's workspace-creation stub, the way PostprocessingPrompt (postmerge_e2e)
// satisfies the after-action's.
//
// The two answers come from the SAME stub because they come from the same
// place in production: both are fields of the create Request, recorded when
// the workspace was created and read back at merge time keyed by worktree
// path. Answering them from different seams here would be testing a shape
// production does not have.
func (e *emptyWorkspaceCreation) BeforeWSMergePrompt(string) (string, error) {
	return e.beforeWSMerge, nil
}

// TestE2EMergePipelineFailsTheRunWhenTheBeforeActionErrors is SPEC ITEM 2: the
// before-action's turn errors, so the run reports `failed` with a cause, NO
// commit is landed, and the lease the run took is given back.
//
// WHY THE NO-COMMITS HALF IS ASSERTED ON DISK. A `failed` status that landed
// commits anyway is the single worst outcome available here — the user's gate
// did not pass and their branch merged regardless — and it is invisible from
// the pushed state alone, since the status would look identical either way.
// So the target repository is asked directly.
//
// WHY THE LEASE HALF IS HERE. The lease is released on EVERY terminal phase
// (merge/contract.go Lease.Acquire), and a `failed` that leaked it leaves a
// workspace the user can never prompt again, with nothing on the wire to say
// why. A failure path is exactly where a release is most likely to be missed.
func TestE2EMergePipelineFailsTheRunWhenTheBeforeActionErrors(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a live session (the action needs a shim to run its turn in),
	// a clean two-commit branch that could otherwise only succeed, and a
	// before-action whose turn is made to error.
	const branch = "feature-before-action-fails"
	h := newUDSHarness(t, withMergeActions(erroringActionPrompt, ""))
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	wsDir := repo.multiCommitWorktree(branch, 2)
	headBefore := strings.TrimSpace(mergeGit(t, repo.target, "rev-parse", "HEAD"))
	_, conn, _, _ := liveSession(t, h, wsDir)
	w := newMergeWatch(t, conn)

	// Act.
	sendMerge(t, conn, "r-before-action-fails", mergeCmdFor(t, h.geometry, repo, wsDir, branch))
	w.awaitOKAck("r-before-action-fails")
	failed := w.awaitStatusArm(wsDir, armFailed)

	// Assert — the phase, the cause, the untouched target, the released lease.
	assertArmOrder(t, w, wsDir, []string{armEnqueued, armBeforeAction, armFailed})
	assertStatusStreamWellFormed(t, w, wsDir)
	if cause := failed.GetFailed().GetCause(); cause == "" {
		t.Error("the failed status carries an empty cause: the user is told their merge died and nothing about why")
	}
	if got := failed.GetFailed().GetCommitsLanded(); got != 0 {
		t.Errorf("the failed status reports commits_landed=%d, want 0: the before-action is the user's precondition for merging at all", got)
	}
	assertNoArm(t, w, wsDir, armCherryPicking,
		"the before-action errored, so no commit may be picked — the action IS the gate")
	assertNoArm(t, w, wsDir, armMerged, "a run whose before-action errored must not report merged")

	if got := strings.TrimSpace(mergeGit(t, repo.target, "rev-parse", "HEAD")); got != headBefore {
		t.Errorf("the target HEAD moved from %s to %s while the run reported failed: commits landed through a gate that did not pass",
			headBefore, got)
	}
	w.awaitLeaseReleasedAtOrAfter(wsDir, phaseMergeFailed)
}

// TestE2EMergePipelineReportsAnAfterActionFailureOnTheMergedStatus is SPEC
// ITEM 5: the after-action's turn errors with every commit already landed, so
// the terminal phase is `merged` carrying after_action_error — not `failed`.
//
// The commits are asserted to be on the target AFTER the terminal status, not
// merely to have been landed at some point: the claim is that they STAY, and a
// producer that rolled the target back on an after-action failure would still
// have published a landed cursor on its way there.
func TestE2EMergePipelineReportsAnAfterActionFailureOnTheMergedStatus(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a live session, a clean two-commit branch, and an after-action
	// whose turn is made to error.
	const (
		branch  = "feature-after-action-fails"
		commits = 2
	)
	h := newUDSHarness(t, withMergeActions("", erroringActionPrompt))
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	wsDir := repo.multiCommitWorktree(branch, commits)
	_, conn, _, _ := liveSession(t, h, wsDir)
	w := newMergeWatch(t, conn)

	// Act.
	sendMerge(t, conn, "r-after-action-fails", mergeCmdFor(t, h.geometry, repo, wsDir, branch))
	w.awaitOKAck("r-after-action-fails")
	merged := w.awaitStatusArm(wsDir, armMerged)

	// Assert — merged is the terminal phase, the error rides on it, and the
	// work is on the target and stays there.
	assertArmOrder(t, w, wsDir, []string{armEnqueued, armCherryPicking, armAfterAction, armMerged})
	assertStatusStreamWellFormed(t, w, wsDir)
	assertNoArm(t, w, wsDir, armFailed,
		"the commits are on the target before the after-action starts, so reporting the RUN failed would deny a merge that demonstrably happened")
	if got := merged.GetMerged().GetAfterActionError(); got == "" {
		t.Error("the merged status carries an empty after_action_error although the after-action errored: the failure was swallowed, and no frontend can report a nudge that never fired")
	}
	if got := merged.GetMerged().GetCommitsTotal(); got != commits {
		t.Errorf("the merged status reports commits_total=%d, want %d", got, commits)
	}
	for i := 1; i <= commits; i++ {
		name := fmt.Sprintf("%s-%d.txt", branch, i)
		if !strings.Contains(readMergeFile(t, repo.target, name), "hello from "+branch) {
			t.Errorf("%s is not in the target after the terminal merged status: the run un-landed work it had already reported landed", name)
		}
	}
}

// erroringActionPrompt is the prompt every action-failure row here is
// configured with.
//
// The shim runs --fake offline, so what makes a turn ERROR is a property of
// the fake harness rather than of the model. This constant is the agreed
// spelling: the fake shim fails the turn for a prompt carrying it, which is
// how an e2e provokes a turn failure without a vendor call and without
// stubbing out the prompt path it is trying to exercise.
const erroringActionPrompt = "e2e-fail-this-turn: the merge action the acceptance gate makes fail"
