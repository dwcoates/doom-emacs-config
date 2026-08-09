// THE FAILED HEAD GATE — PART OF THE ACCEPTANCE GATE (see
// mergepipeline_e2e_test.go for what that means and how to run it).
//
// Every other pipeline suite declares a PASSING fixture suite, so the whole of
// what the gate does when it says no is untested end to end: the phase sequence
// a frontend renders on the way to the failure, the identity the failed arm
// hands the user, and — the property the whole rebase design exists for — that
// the target checkout carries not one line of the refused work.
//
// WHAT THIS FILE DELIBERATELY STOPS SHORT OF. The remediation loop that follows
// a failed gate needs a resolving agent, and a scripted shim fix turn is not
// something this harness can stage cheaply. The merging workspace here has no
// live session, so the fix dispatch is refused at once and the run reaches its
// terminal — which is the state this file gates. The loop's own behavior (it
// turns until the gate passes, or until the agent's escalation record ends it)
// is gated in the merge package's driver and coordinator suites.
//
// Reuses mergepipeline's helpers and mergequeue's fixtures READ-ONLY.
package e2e

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestE2EMergePipelineFailsOnABrokenHeadGate gates the failed arm of the head
// gate, end to end, against a target repository whose declared suite exits
// non-zero.
func TestE2EMergePipelineFailsOnABrokenHeadGate(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a target whose suite always fails, and a sibling with one
	// commit that collides with nothing, so the ONLY thing that can stop this
	// merge is the gate.
	const (
		branch  = "feature-gate-failure"
		subject = branch + " adds a file the suite hates"
		file    = "gate-failure.txt"
	)
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	repo.declareTestSuite(1)
	wsDir := repo.worktree(branch)
	commitIn(t, wsDir, file, "hello from "+branch+"\n", subject)
	sourceSHA := strings.TrimSpace(mergeGit(t, wsDir, "rev-parse", "HEAD"))
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)

	// Act.
	sendMerge(t, conn, "r-gate-failure", mergeCmdFor(t, h.geometry, repo, wsDir, branch))
	w.awaitOKAck("r-gate-failure")
	failed := w.awaitStatusArm(wsDir, armFailed)

	// Assert — the sequence a frontend renders on the way to a refused merge.
	assertArmOrder(t, w, wsDir, []string{armEnqueued, armCherryPicking, armTesting, armFailed})
	assertStatusStreamWellFormed(t, w, wsDir)
	assertNoArm(t, w, wsDir, armMerged,
		"the head gate refused this merge, so nothing may report it as landed")

	// THE FAILED ARM NAMES THE REBASED HEAD, which is what the gate judged — not
	// the source commit, whose sha names a commit on the branch that was never
	// tested as such.
	payload := failed.GetFailed()
	if got := payload.GetFailingSha(); got == "" {
		t.Errorf("the failed status carries no failing_sha, so nothing can correlate the failure with the tree the suite ran on")
	} else if got == sourceSHA {
		t.Errorf("the failed status names failing_sha=%q, which is the SOURCE commit: the gate judges the rebased head, and the head is a different commit", got)
	}
	// THE SUBJECT IS WHAT THE USER READS. A sha identifies nothing to the person
	// whose merge just died; the subject they wrote does.
	if got := payload.GetFailingSubject(); got != subject {
		t.Errorf("the failed status names failing_subject=%q, want %q — the commit named in the words its author wrote", got, subject)
	}
	if got := payload.GetCause(); !strings.Contains(got, subject) {
		t.Errorf("the merge_failed cause is %q, want it to name the failing commit by subject", got)
	}
	if got := payload.GetCause(); strings.Contains(got, payload.GetFailingSha()) {
		t.Errorf("the merge_failed cause is %q, want no sha in the copy a user reads", got)
	}

	// THE TARGET WAS NEVER MODIFIED, which is the whole reason the replay happens
	// in a temporary worktree. Nothing of the refused merge may be found there.
	if _, err := os.Stat(filepath.Join(repo.target, file)); !os.IsNotExist(err) {
		t.Errorf("%s is present in the target after a REFUSED merge (stat err = %v): the gate said no and the work landed anyway", file, err)
	}
	if got := mergeGit(t, repo.target, "log", "--oneline", "-20"); strings.Contains(got, subject) {
		t.Errorf("the target's history carries %q after a refused merge:\n%s", subject, got)
	}
}
