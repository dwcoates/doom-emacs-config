// THE CONFLICTING CHERRY-PICK, end to end: a merge whose pick collides with
// work the target has since taken must be LEFT PARKED IN THE TARGET TREE, not
// aborted, and must report itself as merge_conflict.
//
// WHY NOT-ABORTED IS THE ASSERTION THAT MATTERS. `git cherry-pick --abort` is
// the tempting cleanup: it returns the target to a tidy state and makes the
// failure go away. It also destroys the only artifact a human can act on — the
// conflicted index and the markers in the working tree — and turns a resolvable
// merge into a lost one. So the contract is that the pick STAYS (merge.go
// Merge: "A conflict is LEFT IN THE TARGET TREE (never aborted)"), and this
// file asserts it against the real repository on disk rather than against the
// pushed state alone: a state frame saying merge_conflict while the tree had
// been swept clean would be a lie no frontend could detect.
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
	sendMerge(t, conn, "r-merge-conflict", mergeCmdFor(repo, wsDir, "feature-conflict"))

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

// TestE2EAConflictingCherryPickIsLeftParkedInTheTargetTree covers the DISK
// half: once the conflict has been reported, the target repository still has
// the pick in progress, the conflict markers in its working tree, and no commit
// of its own.
func TestE2EAConflictingCherryPickIsLeftParkedInTheTargetTree(t *testing.T) {
	// Arrange.
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	wsDir := repo.conflictingWorktree("feature-parked-in-tree")
	headBefore := strings.TrimSpace(mergeGit(t, repo.target, "rev-parse", "HEAD"))
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)

	// Act.
	sendMerge(t, conn, "r-merge-parked", mergeCmdFor(repo, wsDir, "feature-parked-in-tree"))
	w.awaitOKAck("r-merge-parked")
	w.awaitPhase(wsDir, phaseMergeConflict)

	// Assert — the pick is still in progress (an abort would have cleared
	// CHERRY_PICK_HEAD), the conflicted file carries markers for a human, and
	// the target committed nothing.
	if exit, out := mergeGitExit(t, repo.target, "rev-parse", "--verify", "--quiet", "CHERRY_PICK_HEAD"); exit != 0 {
		t.Errorf("CHERRY_PICK_HEAD is gone from %s (exit=%d %s): the conflicting pick was aborted instead of left for a human to resolve",
			repo.target, exit, out)
	}
	conflicted := readMergeFile(t, repo.target, "shared.txt")
	if !strings.Contains(conflicted, "<<<<<<<") {
		t.Errorf("shared.txt in the target carries no conflict markers, so there is nothing to resolve:\n%s", conflicted)
	}
	if got := strings.TrimSpace(mergeGit(t, repo.target, "rev-parse", "HEAD")); got != headBefore {
		t.Errorf("target HEAD moved from %s to %s: a conflicting pick must not commit", headBefore, got)
	}
}
