// THE STATUS STREAM ACROSS A DAEMON BOUNCE — PART OF THE ACCEPTANCE GATE (see
// mergepipeline_e2e_test.go for what that means and how to run it).
//
// mergedurability_e2e_test.go already gates the QUEUE's survival: an entry
// enqueued behind a parked head is reconstructed by merge.Coordinator.Drain on
// the next boot and drains without anyone re-submitting it. That test's proof
// is the merge axis reaching `merged`.
//
// This file extends the same pattern to the STRUCTURED status, which is a
// strictly stronger claim and a genuinely separate risk. A run's MergeStatus
// is not one flag but a stream — a run_id, a monotonic clock, and a commit
// cursor — and every one of those is the kind of thing a naive reconstruction
// drops on the floor. A second boot that resumed the queue but published a
// status with no run_id, or a cursor that restarted at zero, would satisfy the
// merge-axis test completely while leaving every frontend unable to say what
// the resumed run is doing.
//
// WHY THE BOUNCE IS THE ONLY WAY TO ASK. The daemon merges its own repository,
// so a self-merge bounces the daemon mid-queue as a matter of course. A status
// that only survives an uninterrupted process is a status that is absent
// exactly when the system is used as designed.
//
// Reuses mergedurability's bounceStateDir / bootMergeDaemon / resolveConflict,
// mergepipeline's helpers, and mergequeue's fixtures READ-ONLY.
package e2e

import (
	"fmt"
	"strings"
	"testing"
)

// TestE2EMergePipelineStatusStreamResumesAfterADaemonBounce is SPEC ITEM 8: a
// run enqueued against one daemon publishes a complete, well-formed
// MergeStatus stream under the NEXT one, with nobody re-submitting it.
//
// THE PROOF IS NEGATIVE AND THEREFORE STRONG, exactly as in
// mergedurability_e2e_test.go: the second boot is handed precisely ONE merge
// command — the resolve-and-continue handoff for the workspace the first boot
// left parked — so every status the QUEUED workspace publishes there can only
// come from an entry read back off the durable record.
//
// WHY THE QUEUED FIXTURE CARRIES SEVERAL COMMITS. The cursor is the fragile
// part. A single-commit fixture would report commits_total=1 whether the
// resumed run planned the range properly or simply defaulted, and it could
// never show the cursor advancing on the far side of the bounce.
func TestE2EMergePipelineStatusStreamResumesAfterADaemonBounce(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a parked head and a three-commit sibling queued behind it,
	// both submitted to the FIRST boot.
	const (
		parkedBranch = "feature-status-parked"
		queuedBranch = "feature-status-queued"
		queuedCount  = 3
	)
	stateFile := bounceStateDir(t)
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	parkedDir := repo.conflictingWorktree(parkedBranch)
	queuedDir := repo.multiCommitWorktree(queuedBranch, queuedCount)

	first := bootMergeDaemon(t, stateFile)
	firstConn := first.dialFrontend(t)
	w1 := newMergeWatch(t, firstConn)
	parkedCmd := parkTheQueueHead(t, first.geometry, w1, repo, parkedDir, parkedBranch, "r-status-parked")
	queuedCmd := mergeCmdFor(t, first.geometry, repo, queuedDir, queuedBranch)
	sendMerge(t, firstConn, "r-status-queued", queuedCmd)
	w1.awaitOKAck("r-status-queued")
	enqueued := w1.awaitStatusArm(queuedDir, armEnqueued)
	runIDBefore := enqueued.GetRunId()
	if runIDBefore == "" {
		t.Error("the queued run's enqueued status carries an empty run_id before the bounce, so nothing on the far side can be matched back to it")
	}
	if got := enqueued.GetEnqueued().GetPosition(); got != 2 {
		t.Errorf("the queued run was admitted at position %d, want 2 (behind the parked head)", got)
	}

	// Act — the daemon goes away with the queue outstanding and comes back,
	// handed nothing but the resolution of the head it left parked.
	_ = firstConn.Close()
	first.bounce()

	second := bootMergeDaemon(t, stateFile)
	secondConn := second.dialFrontend(t)
	defer secondConn.Close()
	w2 := newMergeWatch(t, secondConn)
	// AMENDED for the rebase pipeline, exactly as its sibling in
	// mergedurability was: the boot replay re-parks the conflict in a fresh
	// rebase worktree, so the resolution waits for that park and is written
	// there. Every assertion below is unchanged.
	w2.awaitStatusArm(parkedDir, armConflict)
	resolveConflict(t, repo, "resolved by the acceptance gate\n")
	sendMergeResume(t, secondConn, "r-status-resume-parked", parkedCmd)
	w2.awaitOKAck("r-status-resume-parked")
	w2.awaitStatusArm(parkedDir, armMerged)

	// Assert — the entry nobody re-submitted published a complete status
	// stream on the far side, and its work is on disk.
	merged := w2.awaitStatusArm(queuedDir, armMerged)
	assertStatusStreamWellFormed(t, w2, queuedDir)
	assertNoArm(t, w2, queuedDir, armFailed,
		"the queued run's commits collide with nothing and its suite passes, so a bounce must not turn it into a failure")
	if got := merged.GetMerged().GetCommitsTotal(); got != queuedCount {
		t.Errorf("the resumed run reports commits_total=%d, want %d: the bounce lost the range the run was planning to land",
			got, queuedCount)
	}
	assertCommitsLandedAdvances(t, w2, queuedDir, queuedCount)
	if got := merged.GetRunId(); got != runIDBefore {
		t.Errorf("the resumed run publishes run_id %q while the enqueued status before the bounce carried %q: a daemon bounce interrupts a run, it does not start a new one, and a fresh id makes the queued merge look abandoned",
			got, runIDBefore)
	}
	for i := 1; i <= queuedCount; i++ {
		name := fmt.Sprintf("%s-%d.txt", queuedBranch, i)
		if !strings.Contains(readMergeFile(t, repo.target, name), "hello from "+queuedBranch) {
			t.Errorf("the resumed run reported merged but %s is not in the target: the drain reported work it did not do", name)
		}
	}
}
