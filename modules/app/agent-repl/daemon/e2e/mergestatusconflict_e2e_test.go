// THE CONFLICT ARM AND ITS RESUME — PART OF THE ACCEPTANCE GATE (see
// mergepipeline_e2e_test.go for what that means and how to run it).
//
// mergeconflict_e2e_test.go already gates the CLASSIFICATION: a colliding pick
// reports merge_conflict rather than merged or merge_failed, and it is left
// parked in the target tree. That is everything a one-word phase can say.
//
// This file gates what only the structured status can say — WHICH commit
// collided. A conflict is the one phase whose entire purpose is to hand work
// to a human, and a human handed "your merge conflicted" without a sha and a
// subject has to go find the collision themselves in a tree of pick residue.
// The identifying fields are therefore the contract, not decoration.
//
// It also gates the RESUME, because the conflict arm is the only non-terminal
// off-ramp: a run that parks there must be able to come back and finish, and
// the status is how a frontend knows it did.
//
// Reuses mergepipeline's helpers, mergequeue's fixtures and mergedurability's
// resolveConflict READ-ONLY.
package e2e

import (
	"strings"
	"testing"
)

// TestE2EMergePipelinePublishesWhichCommitConflicted is SPEC ITEM 6's first
// half: the conflict arm names the colliding commit and accounts for the picks
// that preceded it.
//
// THE FIXTURE PUTS THE COLLISION LAST ON PURPOSE. A branch whose FIRST commit
// conflicts cannot distinguish a correct commits_landed=0 from a producer that
// hardcodes zero, and it cannot show that earlier picks survive the park. With
// a clean commit ahead of the colliding one, both facts are forced.
func TestE2EMergePipelinePublishesWhichCommitConflicted(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a sibling whose first commit lands cleanly and whose second
	// rewrites a file the target has since rewritten differently.
	const (
		branch          = "feature-conflict-status"
		conflictSubject = branch + " rewrites shared.txt"
	)
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	wsDir := repo.worktree(branch)
	commitIn(t, wsDir, branch+"-clean.txt", "a commit that collides with nothing\n", branch+" adds its own file")
	commitIn(t, wsDir, "shared.txt", "from "+branch+"\n", conflictSubject)
	repo.divergeTarget("from main\n")
	conflictSHA := strings.TrimSpace(mergeGit(t, wsDir, "rev-parse", "HEAD"))
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)

	// Act.
	sendMerge(t, conn, "r-conflict-status", mergeCmdFor(t, h.geometry, repo, wsDir, branch))
	w.awaitOKAck("r-conflict-status")
	conflict := w.awaitStatusArm(wsDir, armConflict)

	// Assert — the arm identifies the collision and accounts for the range.
	assertArmOrder(t, w, wsDir, []string{armEnqueued, armCherryPicking, armConflict})
	assertStatusStreamWellFormed(t, w, wsDir)
	payload := conflict.GetConflict()
	if got := payload.GetConflictedSha(); got != conflictSHA {
		t.Errorf("the conflict status names conflicted_sha=%q, want %q (the commit that actually collided): a human handed the wrong sha goes looking in the wrong place",
			got, conflictSHA)
	}
	if got := payload.GetConflictedSubject(); got != conflictSubject {
		t.Errorf("the conflict status names conflicted_subject=%q, want %q", got, conflictSubject)
	}
	if got := payload.GetCommitsTotal(); got != 2 {
		t.Errorf("the conflict status reports commits_total=%d, want 2", got)
	}
	if got := payload.GetCommitsLanded(); got != 1 {
		t.Errorf("the conflict status reports commits_landed=%d, want 1: the clean commit ahead of the collision landed and stays landed",
			got)
	}
	assertNoArm(t, w, wsDir, armFailed,
		"a conflict is resolvable and must not also be classified as a hard failure")
	assertNoArm(t, w, wsDir, armMerged, "the run is parked at a conflict, so it has not merged")
}

// TestE2EMergePipelineResumesFromAConflictToCompletion is SPEC ITEM 6's second
// half: after a human resolves the collision,
// conflict_resolved_continue drives the SAME run to `merged`.
//
// THE SAME RUN, which is what run_id is for and what this test's central
// assertion checks. A resume that minted a fresh run_id would look to every
// frontend like a second merge of the workspace, and the conflict a user just
// resolved would appear to have been abandoned rather than continued.
func TestE2EMergePipelineResumesFromAConflictToCompletion(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a run parked at a conflict, and a human's resolution on disk.
	const branch = "feature-conflict-resume"
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	wsDir := repo.conflictingWorktree(branch)
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)
	cmd := mergeCmdFor(t, h.geometry, repo, wsDir, branch)
	sendMerge(t, conn, "r-resume-conflict", cmd)
	w.awaitOKAck("r-resume-conflict")
	parked := w.awaitStatusArm(wsDir, armConflict)
	resolveConflict(t, repo, "resolved by the acceptance gate\n")

	// Act — the resolve-and-continue handoff.
	sendMergeResume(t, conn, "r-resume-continue", cmd)
	w.awaitOKAck("r-resume-continue")
	merged := w.awaitStatusArm(wsDir, armMerged)

	// Assert — the run continued rather than restarted, and it finished.
	if got, want := merged.GetRunId(), parked.GetRunId(); got != want {
		t.Errorf("the resumed run publishes run_id %q while the conflict it resumed carried %q: a resume CONTINUES a run, and a fresh id makes the user's resolution look like an abandoned merge followed by a new one",
			got, want)
	}
	assertStatusStreamWellFormed(t, w, wsDir)
	assertNoArm(t, w, wsDir, armFailed, "the conflict was resolved, so the run must complete rather than fail")
	if got := merged.GetMerged().GetCommitsTotal(); got != 1 {
		t.Errorf("the merged status reports commits_total=%d, want 1 (the fixture branch's single commit)", got)
	}
	if !strings.Contains(readMergeFile(t, repo.target, "shared.txt"), "resolved by the acceptance gate") {
		t.Error("the run reported merged but the target does not carry the resolved content: the resume reported work it did not do")
	}
}
