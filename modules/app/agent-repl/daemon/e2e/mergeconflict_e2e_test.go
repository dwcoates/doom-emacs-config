// THE CONFLICTING REBASE, end to end: a merge whose replay collides with work
// the target has since taken must be LEFT PARKED IN THE REBASE WORKTREE, not
// aborted, and must report itself as merge_conflict.
//
// WHY NOT-ABORTED IS THE ASSERTION THAT MATTERS. `git cherry-pick --abort` is
// the tempting cleanup: it returns the tree to a tidy state and makes the
// failure go away. It also destroys the only artifact a human can act on — the
// conflicted index and the markers in the working tree — and turns a resolvable
// merge into a lost one. So the contract is that the replay STAYS (merge.go
// Merge: "A conflict is LEFT IN THE REBASE WORKTREE (never aborted)"), and this
// file asserts it against the real repository on disk rather than against the
// pushed state alone: a state frame saying merge_conflict while the tree had
// been swept clean would be a lie no frontend could detect.
//
// WHERE THE PARK LIVES IS THE OTHER HALF, and it is the newer assertion of the
// two. The pipeline replays commits in a temporary rebase worktree and touches
// the TARGET exactly once, at the very end, so a conflict must be parked there
// and the target must be byte-for-byte as the merge found it. A park in the
// target would mean the tree every other workspace cuts from was carrying an
// unfinished merge for as long as a human took to resolve it.
//
// Reuses mergequeue_e2e_test.go's fixtures and mergeWatch READ-ONLY.
package e2e

import (
	"strings"
	"testing"
)

// TestE2EAConflictingCherryPickReportsMergeConflictAsItsTerminalPhase covers
// the STATE half: the conflict is the merge's terminal phase, and neither of
// the other two terminal phases is ever claimed for it.
func TestE2EAConflictingCherryPickReportsMergeConflictAsItsTerminalPhase(t *testing.T) {
	// Arrange — a worktree whose one commit rewrites a file the target has
	// since rewritten differently.
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	wsDir := repo.conflictingWorktree("feature-conflict")
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)

	// Act.
	sendMerge(t, conn, "r-merge-conflict", mergeCmdFor(t, h.geometry, repo, wsDir, "feature-conflict"))

	// Assert — merge_conflict, and not a disguised success or failure.
	w.awaitOKAck("r-merge-conflict")
	w.awaitPhase(wsDir, phaseMergeConflict)
	if w.sawPhase(wsDir, phaseMerged) {
		t.Error("a conflicting cherry-pick reported merged")
	}
	if w.sawPhase(wsDir, phaseMergeFailed) {
		t.Error("a conflicting cherry-pick reported merge_failed: a conflict is resolvable and must not be classified as a hard failure")
	}
}

// TestE2EAConflictingRebaseIsLeftParkedInTheRebaseWorktree covers the DISK
// half. AMENDED FROM TestE2EAConflictingCherryPickIsLeftParkedInTheTargetTree,
// whose every assertion is kept and re-aimed at the tree the replay now happens
// in — plus one the old shape could not make: the target has NO parked pick of
// its own, which is the guarantee the whole rebase design exists to provide.
func TestE2EAConflictingRebaseIsLeftParkedInTheRebaseWorktree(t *testing.T) {
	// Arrange.
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	wsDir := repo.conflictingWorktree("feature-parked-in-tree")
	headBefore := strings.TrimSpace(mergeGit(t, repo.target, "rev-parse", "HEAD"))
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)

	// Act.
	sendMerge(t, conn, "r-merge-parked", mergeCmdFor(t, h.geometry, repo, wsDir, "feature-parked-in-tree"))
	w.awaitOKAck("r-merge-parked")
	w.awaitPhase(wsDir, phaseMergeConflict)

	// Assert — the replay is still in progress in the rebase worktree (an abort
	// would have cleared CHERRY_PICK_HEAD), the conflicted file there carries
	// markers for a human, and the TARGET is exactly as the merge found it.
	work := rebaseWorktreeOf(t, repo)
	if exit, out := mergeGitExit(t, work, "rev-parse", "--verify", "--quiet", "CHERRY_PICK_HEAD"); exit != 0 {
		t.Errorf("CHERRY_PICK_HEAD is gone from %s (exit=%d %s): the conflicting replay was aborted instead of left for a human to resolve",
			work, exit, out)
	}
	conflicted := readMergeFile(t, work, "shared.txt")
	if !strings.Contains(conflicted, "<<<<<<<") {
		t.Errorf("shared.txt in the rebase worktree carries no conflict markers, so there is nothing to resolve:\n%s", conflicted)
	}
	if exit, _ := mergeGitExit(t, repo.target, "rev-parse", "--verify", "--quiet", "CHERRY_PICK_HEAD"); exit == 0 {
		t.Error("the TARGET is parked on a cherry-pick: this pipeline must never leave the shared tree holding an unfinished merge")
	}
	if got := readMergeFile(t, repo.target, "shared.txt"); strings.Contains(got, "<<<<<<<") {
		t.Errorf("the TARGET's shared.txt carries conflict markers:\n%s", got)
	}
	if got := strings.TrimSpace(mergeGit(t, repo.target, "rev-parse", "HEAD")); got != headBefore {
		t.Errorf("target HEAD moved from %s to %s: a conflicting merge must not commit", headBefore, got)
	}
}
