// THE MERGE PIPELINE'S STRUCTURED STATUS, end to end — THE ACCEPTANCE GATE.
//
// ============================================================================
// THIS SUITE IS WRITTEN AGAINST THE SPEC, NOT AGAINST THE IMPLEMENTATION.
//
// Everything named TestE2EMergePipeline* in this package is the acceptance
// gate for WorkspaceState.merge_status (frontend.proto MergeStatus, frozen).
// The producer is being built on sibling branches, so these tests are EXPECTED
// TO FAIL until that work lands. They are deliberately not skipped and not
// build-tagged: a gate that passes vacuously is not a gate, and a gate nobody
// can run is not a gate either.
//
// The integration wave runs exactly:
//
//	go test ./e2e/ -count=1 -run TestE2EMergePipeline
//
// Every failure here is phrased as WHAT THE SPEC REQUIRES and WHAT ARRIVED
// INSTEAD, so a run against an unfinished producer reads as a to-do list
// rather than as a pile of nil dereferences.
// ============================================================================
//
// WHAT MERGE_STATUS ADDS, and why this suite exists separately from the merge
// AXIS suites beside it. The axis is one word out of six. It can say `merging`,
// and it can say `merged`, and it cannot say WHICH commit of how many is being
// picked, which one conflicted, whether the configured before-action ran, or
// why a failure failed. That is why the flat `merge_phase` projection of the
// axis is retired from the wire and MergeStatus is the only merge-run surface:
// MergeStatus is the run's own structured account of itself, and the properties
// this suite gates are the ones a coarse word could never have carried:
//
//   - the ORDER of the phases (mergepipeline, this file),
//   - the ACTIONS around the picks (mergeactions),
//   - the SESSION the actions need (mergesessionbringup),
//   - the CONFLICT payload and its resume (mergestatusconflict),
//   - the DISPATCH-JSON route into the queue (mergedispatch),
//   - and its SURVIVAL of a daemon bounce (mergestatusdurability).
//
// This file is the HELPER HOME for all six, the way mergequeue_e2e_test.go is
// the helper home for the merge-axis suites. It reuses mergequeue's fixtures,
// mergeWatch and geometry recording READ-ONLY.
package e2e

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
)

// --- the acceptance marker ---------------------------------------------------

// acceptanceGate stamps a test as part of the merge-pipeline acceptance gate.
//
// It logs rather than skips ON PURPOSE. The marker's job is to make a red run
// legible — "this failure is the gate reporting unbuilt work, not a regression
// in something that used to pass" — which a skip would achieve by destroying
// the signal instead of labelling it.
func acceptanceGate(t *testing.T) {
	t.Helper()
	t.Log("ACCEPTANCE GATE: this test is written against the frozen MergeStatus spec and fails until the merge-pipeline producer lands.")
}

// --- the MergeStatus phase vocabulary ----------------------------------------

// The oneof arm names, as this suite reports them. They are the proto field
// names of MergeStatus.phase, so a failure message names the thing the
// implementer has to go set.
const (
	armEnqueued      = "enqueued"
	armBeforeAction  = "before_action"
	armCherryPicking = "cherry_picking"
	armTesting       = "testing"
	armConflict      = "conflict"
	armAfterAction   = "after_action"
	armMerged        = "merged"
	armFailed        = "failed"
	// armNone is what an unset oneof reports as. It is never a phase: an unset
	// MergeStatus.phase is a malformed status, since a status exists precisely
	// to say which phase a run is in.
	armNone = "<none>"
)

// statusArm reports which oneof arm a MergeStatus carries.
func statusArm(status *frontendv1.MergeStatus) string {
	switch status.GetPhase().(type) {
	case *frontendv1.MergeStatus_Enqueued:
		return armEnqueued
	case *frontendv1.MergeStatus_BeforeAction:
		return armBeforeAction
	case *frontendv1.MergeStatus_CherryPicking:
		return armCherryPicking
	case *frontendv1.MergeStatus_Testing:
		return armTesting
	case *frontendv1.MergeStatus_Conflict:
		return armConflict
	case *frontendv1.MergeStatus_AfterAction:
		return armAfterAction
	case *frontendv1.MergeStatus_Merged:
		return armMerged
	case *frontendv1.MergeStatus_Failed:
		return armFailed
	default:
		return armNone
	}
}

// armPayload returns the arm's own message, for identity comparison. A nil
// result means the arm is unset.
func armPayload(status *frontendv1.MergeStatus) proto.Message {
	switch p := status.GetPhase().(type) {
	case *frontendv1.MergeStatus_Enqueued:
		return p.Enqueued
	case *frontendv1.MergeStatus_BeforeAction:
		return p.BeforeAction
	case *frontendv1.MergeStatus_CherryPicking:
		return p.CherryPicking
	case *frontendv1.MergeStatus_Testing:
		return p.Testing
	case *frontendv1.MergeStatus_Conflict:
		return p.Conflict
	case *frontendv1.MergeStatus_AfterAction:
		return p.AfterAction
	case *frontendv1.MergeStatus_Merged:
		return p.Merged
	case *frontendv1.MergeStatus_Failed:
		return p.Failed
	default:
		return nil
	}
}

// statusIdentity is everything about a status EXCEPT its timestamps: the arm
// and that arm's payload.
//
// It is what separates a REPUBLICATION of an unchanged status (which may carry
// the same updated_at_ms, because nothing happened) from a TICK (which must
// carry a later one, because something did). Without the distinction, "strictly
// monotonic" is unassertable: an unrelated connectivity push republishes the
// workspace's state verbatim, and demanding a bump there would be demanding the
// daemon invent progress it did not make.
func statusIdentity(status *frontendv1.MergeStatus) string {
	arm := statusArm(status)
	payload := armPayload(status)
	if payload == nil {
		return arm
	}
	encoded, err := protojson.Marshal(payload)
	if err != nil {
		return arm + "|<unencodable>"
	}
	return arm + "|" + string(encoded)
}

// --- MergeStatus observation over mergeWatch ---------------------------------

// mergeStatuses returns every MergeStatus recorded for ws, in arrival order.
// States carrying no status are skipped: a WorkspaceState is pushed for a great
// many reasons that have nothing to do with a merge, and an unset merge_status
// on one of those is the contract working ("UNSET means this workspace has no
// merge to report"), not a data point.
func (w *mergeWatch) mergeStatuses(ws string) []*frontendv1.MergeStatus {
	var out []*frontendv1.MergeStatus
	for _, state := range w.states {
		if state.GetWorkspace() != ws {
			continue
		}
		if status := state.GetMergeStatus(); status != nil {
			out = append(out, status)
		}
	}
	return out
}

// firstStatusArm returns the first recorded status for ws on the given arm, or
// nil.
func (w *mergeWatch) firstStatusArm(ws, arm string) *frontendv1.MergeStatus {
	for _, status := range w.mergeStatuses(ws) {
		if statusArm(status) == arm {
			return status
		}
	}
	return nil
}

func (w *mergeWatch) sawStatusArm(ws, arm string) bool {
	return w.firstStatusArm(ws, arm) != nil
}

// awaitStatusArm blocks until ws has been seen on arm and returns the FIRST
// such status — the one describing the transition rather than a later tick
// within the same phase.
//
// Its failure message reports the whole arm sequence observed so far, because
// against an unbuilt producer the useful question is never "was it enqueued"
// but "what, if anything, did this run publish at all".
func (w *mergeWatch) awaitStatusArm(ws, arm string) *frontendv1.MergeStatus {
	w.t.Helper()
	w.until(
		fmt.Sprintf("a WorkspaceState for %s whose merge_status.phase is %s (observed so far: %v)",
			ws, arm, w.statusArmSequence(ws)),
		func() bool { return w.sawStatusArm(ws, arm) })
	return w.firstStatusArm(ws, arm)
}

// statusArmSequence is the arm sequence for ws with consecutive repeats
// collapsed: the PHASE ORDER, with within-phase ticks folded away.
//
// Collapsing is what makes the order assertable at all. cherry_picking is
// republished once per commit and testing once per gate, so the raw sequence's
// length is a property of the fixture's commit count rather than of the
// pipeline. What the spec constrains is which phase follows which.
func (w *mergeWatch) statusArmSequence(ws string) []string {
	var seq []string
	for _, status := range w.mergeStatuses(ws) {
		arm := statusArm(status)
		if len(seq) > 0 && seq[len(seq)-1] == arm {
			continue
		}
		seq = append(seq, arm)
	}
	return seq
}

// latestStatus returns the most recent status recorded for ws, or nil.
func (w *mergeWatch) latestStatus(ws string) *frontendv1.MergeStatus {
	statuses := w.mergeStatuses(ws)
	if len(statuses) == 0 {
		return nil
	}
	return statuses[len(statuses)-1]
}

// --- shared assertions --------------------------------------------------------

// assertStatusStreamWellFormed checks the three properties every merge_status
// stream must have regardless of which phases it went through. Each is checked
// separately so a run that violates one still reports the others.
func assertStatusStreamWellFormed(t *testing.T, w *mergeWatch, ws string) {
	t.Helper()
	statuses := w.mergeStatuses(ws)
	if len(statuses) == 0 {
		t.Fatalf("no WorkspaceState for %s ever carried a merge_status: the merge ran without publishing the structured status at all", ws)
	}
	assertStatusRunIDStable(t, statuses, ws)
	assertUpdatedAtMonotonic(t, statuses, ws)
	assertPhaseStartedTracksTheArm(t, statuses, ws)
}

// assertStatusRunIDStable: one run publishes one run_id, and never an empty
// one. An empty id makes a fresh run's `enqueued` indistinguishable from a
// redelivery of the previous run's, which is the ambiguity run_id exists to
// remove.
func assertStatusRunIDStable(t *testing.T, statuses []*frontendv1.MergeStatus, ws string) {
	t.Helper()
	first := statuses[0].GetRunId()
	if first == "" {
		t.Errorf("the first merge_status for %s carries an empty run_id; the spec mints one per run", ws)
		return
	}
	for i, status := range statuses {
		if got := status.GetRunId(); got != first {
			t.Errorf("merge_status[%d] for %s carries run_id %q while the run began as %q: one merge run publishes ONE run_id",
				i, ws, got, first)
			return
		}
	}
}

// assertUpdatedAtMonotonic: updated_at_ms never goes backwards, and it strictly
// advances every time the status CONTENT changes — which is what makes a
// per-pick tick observable as progress rather than as a redelivery.
func assertUpdatedAtMonotonic(t *testing.T, statuses []*frontendv1.MergeStatus, ws string) {
	t.Helper()
	for i := 1; i < len(statuses); i++ {
		prev, cur := statuses[i-1], statuses[i]
		switch {
		case cur.GetUpdatedAtMs() < prev.GetUpdatedAtMs():
			t.Errorf("merge_status for %s went BACKWARDS in time at index %d: updated_at_ms %d follows %d (arms %s then %s)",
				ws, i, cur.GetUpdatedAtMs(), prev.GetUpdatedAtMs(), statusArm(prev), statusArm(cur))
			return
		case statusIdentity(cur) != statusIdentity(prev) && cur.GetUpdatedAtMs() == prev.GetUpdatedAtMs():
			t.Errorf("merge_status for %s changed content at index %d without advancing updated_at_ms (%d): %s -> %s. A receiver cannot order these two, so a reordered delivery would silently overwrite the newer one",
				ws, i, cur.GetUpdatedAtMs(), statusIdentity(prev), statusIdentity(cur))
			return
		}
	}
}

// assertPhaseStartedTracksTheArm: phase_started_at_ms moves on a phase
// transition and ONLY on a phase transition. A value that also moved on a
// within-phase tick would make "cherry-picking for 4s" unrenderable, which is
// the field's whole purpose.
func assertPhaseStartedTracksTheArm(t *testing.T, statuses []*frontendv1.MergeStatus, ws string) {
	t.Helper()
	for i := 1; i < len(statuses); i++ {
		prev, cur := statuses[i-1], statuses[i]
		prevArm, curArm := statusArm(prev), statusArm(cur)
		switch {
		case prevArm != curArm && cur.GetPhaseStartedAtMs() == prev.GetPhaseStartedAtMs():
			t.Errorf("merge_status for %s moved %s -> %s at index %d without advancing phase_started_at_ms (%d): the new phase claims to have begun when the old one did",
				ws, prevArm, curArm, i, cur.GetPhaseStartedAtMs())
			return
		case prevArm == curArm && cur.GetPhaseStartedAtMs() != prev.GetPhaseStartedAtMs():
			t.Errorf("merge_status for %s stayed on %s at index %d but moved phase_started_at_ms from %d to %d: a within-phase tick must advance updated_at_ms only",
				ws, curArm, i, prev.GetPhaseStartedAtMs(), cur.GetPhaseStartedAtMs())
			return
		}
	}
}

// assertArmOrder checks that want appears as a SUBSEQUENCE of the observed
// collapsed arm sequence, in order.
//
// Subsequence rather than equality, because the spec's optional phases are
// genuinely optional and a producer is free to publish an arm this particular
// assertion does not name. What is NOT free is the relative order of the arms
// the caller does name, and that is exactly what a subsequence check pins.
func assertArmOrder(t *testing.T, w *mergeWatch, ws string, want []string) {
	t.Helper()
	got := w.statusArmSequence(ws)
	next := 0
	for _, arm := range got {
		if next < len(want) && arm == want[next] {
			next++
		}
	}
	if next != len(want) {
		t.Errorf("the merge of %s published phases %v, which does not contain %v in order (first unmatched: %s)",
			ws, got, want, want[next])
	}
}

// assertNoArm fails when ws was ever seen on arm. It is how a test says "this
// run must not have gone down that road" — a merged run that also published
// `failed`, a failed run that also landed commits.
func assertNoArm(t *testing.T, w *mergeWatch, ws, arm, why string) {
	t.Helper()
	if w.sawStatusArm(ws, arm) {
		t.Errorf("the merge of %s published merge_status.phase=%s: %s (full sequence %v)",
			ws, arm, why, w.statusArmSequence(ws))
	}
}

// --- fixtures the pipeline suites add ----------------------------------------

// multiCommitWorktree is a sibling worktree carrying n commits that collide
// with nothing, so its picks can only be held up by the pipeline itself.
//
// SEVERAL commits rather than one, because the per-pick facts are the point:
// commits_total, an ADVANCING commits_landed, and a current_sha that changes.
// A single-commit fixture makes commits_landed's increment unobservable and
// lets a producer that hardcodes 1/1 pass.
func (r *mergeRepo) multiCommitWorktree(branch string, n int) string {
	r.t.Helper()
	dir := r.worktree(branch)
	for i := 1; i <= n; i++ {
		commitIn(r.t, dir,
			fmt.Sprintf("%s-%d.txt", branch, i),
			fmt.Sprintf("hello from %s commit %d\n", branch, i),
			fmt.Sprintf("%s adds file %d", branch, i))
	}
	return dir
}

// declareTestSuite gives the TARGET repository a real test entrypoint at the
// path RepoSuiteRunner looks for, so the pipeline's per-commit gate genuinely
// RUNS rather than reporting SuiteResult{Skipped: true}.
//
// It matters that the suite is real. `testing` is a phase of the spec's
// sequence, and a fixture with no entrypoint cannot distinguish "the producer
// publishes the testing phase" from "there was nothing to test" — the two look
// identical from the frontend, and only one of them is the contract.
//
// The script is committed on main BEFORE any worktree is cut, so every sibling
// inherits it and the target carries it in its working tree, which is where
// entrypoint resolution stats for it.
func (r *mergeRepo) declareTestSuite(exitCode int) {
	r.t.Helper()
	rel := filepath.Join("modules", "app", "agent-repl", "bin")
	if err := os.MkdirAll(filepath.Join(r.target, rel), 0o755); err != nil {
		r.t.Fatalf("make fixture suite dir: %v", err)
	}
	script := filepath.Join(rel, "test-all.sh")
	body := fmt.Sprintf("#!/bin/sh\necho 'fixture suite for the merge pipeline acceptance gate'\nexit %d\n", exitCode)
	if err := os.WriteFile(filepath.Join(r.target, script), []byte(body), 0o755); err != nil {
		r.t.Fatalf("write fixture suite: %v", err)
	}
	mergeGit(r.t, r.target, "add", ".")
	mergeGit(r.t, r.target, "commit", "-q", "-m", "declare the fixture test suite")
}

// --- the test -----------------------------------------------------------------

// TestE2EMergePipelinePublishesItsPhasesInOrder is SPEC ITEM 1: the happy path
// walks enqueued → cherry_picking → testing → merged, one WorkspaceState
// revision per transition, with the structured payload the coarse merge axis
// could never carry.
//
// The before/after action arms are absent here BY CONFIGURATION — this
// workspace was created with neither — which is half of the "iff configured"
// claim. The other half (they appear when configured) is
// mergeactions_e2e_test.go's, because one test asserts one edge.
//
// WHY THE FIXTURE HAS THREE COMMITS AND A REAL SUITE. The properties under
// test are per-commit: commits_total must be the range's real size,
// commits_landed must ADVANCE, and testing must be entered for the picks
// rather than once for the run. A one-commit fixture with no suite would pass
// against a producer that hardcoded a single 1/1 tick and never tested
// anything.
func TestE2EMergePipelinePublishesItsPhasesInOrder(t *testing.T) {
	acceptanceGate(t)

	// Arrange — a target that declares a passing suite, and a sibling with
	// three non-colliding commits to land through it.
	const (
		branch  = "feature-pipeline-order"
		commits = 3
	)
	h := newUDSHarness(t)
	repo := newMergeRepo(t)
	repo.declareTestSuite(0)
	wsDir := repo.multiCommitWorktree(branch, commits)
	conn := h.dialFrontend(t)
	defer conn.Close()
	w := newMergeWatch(t, conn)

	// Act.
	sendMerge(t, conn, "r-pipeline-order", mergeCmdFor(t, h.geometry, repo, wsDir, branch))
	w.awaitOKAck("r-pipeline-order")
	w.awaitStatusArm(wsDir, armMerged)

	// Assert — the order, the well-formedness, the per-pick progress, and the
	// absence of the arms this configuration must not produce.
	assertArmOrder(t, w, wsDir, []string{armEnqueued, armCherryPicking, armTesting, armMerged})
	assertStatusStreamWellFormed(t, w, wsDir)
	assertNoArm(t, w, wsDir, armBeforeAction,
		"this workspace was created with no before_ws_merge action, and the spec runs that phase iff one is configured")
	assertNoArm(t, w, wsDir, armAfterAction,
		"this workspace was created with no postprocessing prompt, and the spec runs that phase iff one is configured")
	assertNoArm(t, w, wsDir, armFailed, "a clean pick of non-colliding commits through a passing suite must not fail")
	assertNoArm(t, w, wsDir, armConflict, "the fixture's commits touch files nothing else touches, so nothing can conflict")

	assertCommitsLandedAdvances(t, w, wsDir, commits)

	if got := w.latestStatus(wsDir).GetMerged().GetCommitsTotal(); got != commits {
		t.Errorf("the terminal merged status for %s reports commits_total=%d, want %d (the fixture branch's real range)",
			wsDir, got, commits)
	}
	if got := w.latestStatus(wsDir).GetMerged().GetAfterActionError(); got != "" {
		t.Errorf("the terminal merged status for %s carries after_action_error=%q with no after action configured", wsDir, got)
	}
	// The work is genuinely on the target, not merely reported as such.
	for i := 1; i <= commits; i++ {
		name := fmt.Sprintf("%s-%d.txt", branch, i)
		if !strings.Contains(readMergeFile(t, repo.target, name), "hello from "+branch) {
			t.Errorf("%s reported merged but %s is not in the target: the run reported work it did not do", wsDir, name)
		}
	}
}

// assertCommitsLandedAdvances is SPEC ITEM 1's per-pick half: the picking and
// testing arms carry a cursor that MOVES, ending at the full range.
//
// It reads the cursor off both arms together, because the two share one cursor
// by design (MergeStatusTesting: "the commit under test is landed but
// ungated") and a frontend renders one progress bar across them.
func assertCommitsLandedAdvances(t *testing.T, w *mergeWatch, ws string, wantTotal int32) {
	t.Helper()
	var (
		landed  []int32
		shas    = map[string]bool{}
		highest int32
	)
	for _, status := range w.mergeStatuses(ws) {
		var total, done int32
		var sha string
		switch arm := statusArm(status); arm {
		case armCherryPicking:
			p := status.GetCherryPicking()
			total, done, sha = p.GetCommitsTotal(), p.GetCommitsLanded(), p.GetCurrentSha()
		case armTesting:
			p := status.GetTesting()
			total, done, sha = p.GetCommitsTotal(), p.GetCommitsLanded(), p.GetCurrentSha()
		default:
			continue
		}
		if total != wantTotal {
			t.Errorf("a %s status for %s reports commits_total=%d, want %d", statusArm(status), ws, total, wantTotal)
			return
		}
		if done < highest {
			t.Errorf("commits_landed for %s went backwards: %d after %d", ws, done, highest)
			return
		}
		if sha == "" {
			t.Errorf("a %s status for %s carries an empty current_sha, so a frontend cannot say which commit is in flight",
				statusArm(status), ws)
			return
		}
		highest = done
		landed = append(landed, done)
		shas[sha] = true
	}
	if len(landed) == 0 {
		t.Errorf("the merge of %s published no cherry_picking or testing status at all, so no per-commit progress was ever observable (sequence %v)",
			ws, w.statusArmSequence(ws))
		return
	}
	if highest != wantTotal {
		t.Errorf("commits_landed for %s peaked at %d, want %d: the run reported merged without accounting for every commit",
			ws, highest, wantTotal)
	}
	if landed[0] == highest {
		t.Errorf("commits_landed for %s never moved off %d across %d picking/testing ticks: the cursor is not tracking the picks",
			ws, highest, len(landed))
	}
	if int32(len(shas)) != wantTotal {
		t.Errorf("the picking/testing statuses for %s named %d distinct commits, want %d: the run did not step through the range one commit at a time",
			ws, len(shas), wantTotal)
	}
}
