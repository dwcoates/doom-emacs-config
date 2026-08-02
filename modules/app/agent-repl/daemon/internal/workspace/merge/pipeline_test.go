package merge

import (
	"context"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// waitForPhase drains the sink's transition channel until phase arrives, and
// fails the test if the run terminates on something else first.
//
// It is a channel rendezvous rather than a poll: the drain goroutine publishes
// on its own schedule, and sleeping until it "probably" got there is exactly
// the shape of test flakiness this avoids.
func waitForPhase(t *testing.T, h *harness, phase Phase) transition {
	t.Helper()
	for {
		tr := <-h.sink.ch
		if tr.phase == phase {
			return tr
		}
		if tr.phase == PhaseMerged || tr.phase == PhaseMergeFailed {
			t.Fatalf("the run reached %s before %s (cause: %s)", tr.phase, phase, tr.cause)
		}
	}
}

// lastStatusOfPhase returns the newest published status whose oneof matches
// want, or fails.
func lastStatusOfPhase(t *testing.T, h *harness, want func(*frontendv1.MergeStatus) bool) *frontendv1.MergeStatus {
	t.Helper()
	statuses := h.sink.publishedStatuses()
	for i := len(statuses) - 1; i >= 0; i-- {
		if want(statuses[i]) {
			return statuses[i]
		}
	}
	t.Fatalf("no published status matched; published %d", len(statuses))
	return nil
}

// --- session liveness ----------------------------------------------------

// THE GUARANTEE: the run brings the workspace's session up BEFORE taking the
// lease. A merge drives that session, and a bring-up held under the exclusivity
// lease would lock the user out of their own session for its whole duration.
func TestARunBringsTheSessionUpBeforeAcquiringTheLease(t *testing.T) {
	// Arrange.
	brought := make(chan string, 1)
	h := newHarnessWith(t, harnessOpts{sessions: &fakeSessionBringUp{done: brought}})

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert — the bring-up completes, and only then does the lease move.
	if got := <-brought; got != "/ws/a" {
		t.Fatalf("brought up %q, want /ws/a", got)
	}
	if got := <-h.lease.acquires; got != "/ws/a" {
		t.Fatalf("lease acquired for %q, want /ws/a", got)
	}
}

// THE FAILURE EDGE: a session that cannot be brought up fails the run, and the
// lease is never taken — there is nothing to drive under it.
func TestASessionThatCannotBeBroughtUpFailsTheRun(t *testing.T) {
	// Arrange — the workspace's newest session was deleted, so the bring-up
	// path has nothing to establish.
	h := newHarnessWith(t, harnessOpts{sessions: &fakeSessionBringUp{
		err: sentinelError("the workspace's newest session was deleted"),
	}})

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert.
	tr := waitForPhase(t, h, PhaseMergeFailed)
	if !strings.Contains(tr.cause, "session was deleted") {
		t.Fatalf("merge_failed cause = %q, want it to name the bring-up failure", tr.cause)
	}
	if len(h.lease.acquires) != 0 {
		t.Fatal("the lease was acquired for a run whose session could not be brought up")
	}
}

// The failure's PUBLISHED CONTENT names the cause, so a user reading the card
// learns why without going to the log.
func TestABringUpFailurePublishesItsCause(t *testing.T) {
	// Arrange.
	h := newHarnessWith(t, harnessOpts{sessions: &fakeSessionBringUp{
		err: sentinelError("shim refused to start"),
	}})

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	waitForPhase(t, h, PhaseMergeFailed)

	// Assert.
	status := lastStatusOfPhase(t, h, func(s *frontendv1.MergeStatus) bool { return s.GetFailed() != nil })
	if !strings.Contains(status.GetFailed().GetCause(), "shim refused to start") {
		t.Fatalf("failed cause = %q, want it to name the bring-up failure", status.GetFailed().GetCause())
	}
}

// --- the before-action ---------------------------------------------------

// THE COMMON CASE: a workspace with no recorded action goes straight to the
// plan, and nothing is delivered to its session.
func TestAWorkspaceWithNoBeforeActionRunsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Assert.
	if got := h.beforeRunner.calls(); len(got) != 0 {
		t.Fatalf("delivered %d before-actions to a workspace that has none: %+v", len(got), got)
	}
}

// THE GUARANTEE: a recorded action is delivered to the workspace's OWN session,
// and the cherry-pick plan is computed only after that turn has ended — the
// action may have created the very commits the plan has to carry.
func TestARecordedBeforeActionRunsBeforeThePlanIsComputed(t *testing.T) {
	// Arrange.
	delivered := make(chan BeforeAction, 1)
	h := newHarnessWith(t, harnessOpts{
		beforeActions: fakeBeforeActions{prompt: "bump the version"},
		beforeRunner:  &fakeBeforeActionRunner{done: delivered},
	})

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert — the action's turn completes, and only then is the driver called.
	act := <-delivered
	if act.Workspace != "/ws/a" || act.Prompt != "bump the version" {
		t.Fatalf("delivered %+v, want the recorded action on /ws/a", act)
	}
	<-h.picker.merges
}

// The before_action phase's PUBLISHED CONTENT carries the prompt, so a user can
// see what their session was asked to do.
func TestTheBeforeActionPhasePublishesItsPrompt(t *testing.T) {
	// Arrange.
	h := newHarnessWith(t, harnessOpts{beforeActions: fakeBeforeActions{prompt: "bump the version"}})

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Assert.
	status := lastStatusOfPhase(t, h, func(s *frontendv1.MergeStatus) bool { return s.GetBeforeAction() != nil })
	if got := status.GetBeforeAction().GetPrompt(); got != "bump the version" {
		t.Fatalf("before_action prompt = %q, want the recorded action text", got)
	}
}

// THE FAILURE EDGE: an action whose turn does not end cleanly FAILS the run.
// Cherry-picking a plan the action was meant to change would land the wrong
// commits.
func TestABeforeActionThatDoesNotCompleteFailsTheRun(t *testing.T) {
	// Arrange.
	h := newHarnessWith(t, harnessOpts{
		beforeActions: fakeBeforeActions{prompt: "bump the version"},
		beforeRunner:  &fakeBeforeActionRunner{err: sentinelError("the turn never ended")},
	})

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert.
	tr := waitForPhase(t, h, PhaseMergeFailed)
	if !strings.Contains(tr.cause, "before-merge action did not complete") {
		t.Fatalf("merge_failed cause = %q, want it to name the before-action", tr.cause)
	}
	if len(h.picker.merges) != 0 {
		t.Fatal("the cherry-pick ran after a before-action that did not complete")
	}
}

// THE OTHER FAILURE EDGE: an action that cannot be READ also fails the run.
// "There is no action" and "the record could not be read" are different
// answers, and running the merge on the second would silently skip an action
// the user asked for.
func TestAnUnreadableBeforeActionFailsTheRun(t *testing.T) {
	// Arrange.
	h := newHarnessWith(t, harnessOpts{
		beforeActions: fakeBeforeActions{err: sentinelError("the geometry record is unreadable")},
	})

	// Act.
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert.
	tr := waitForPhase(t, h, PhaseMergeFailed)
	if !strings.Contains(tr.cause, "could not be read") {
		t.Fatalf("merge_failed cause = %q, want it to name the unreadable record", tr.cause)
	}
}

// --- the after-action ----------------------------------------------------

// THE GUARANTEE: a merged run publishes the after_action phase and then the
// terminal merged.
func TestAMergedRunPublishesTheAfterActionThenMerged(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}

	// Assert.
	waitForPhase(t, h, PhaseMergeAfterAction)
	tr := <-h.sink.ch
	if tr.phase != PhaseMerged {
		t.Fatalf("phase after the after-action = %s, want merged", tr.phase)
	}
}

// THE FAILURE EDGE, and the load-bearing one: a failed after-action does NOT
// fail the run. The commits are on the target either way, and reporting
// merge_failed would make the pushed status lie about the tree.
func TestAFailedAfterActionStillEndsTheRunMerged(t *testing.T) {
	// Arrange.
	hook := newFakePostMergeHook(4)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-hook.calls

	// Act — the handoff refuses.
	hook.results <- sentinelError("the parent refused the prompt")

	// Assert.
	waitForPhase(t, h, PhaseMerged)
	status := lastStatusOfPhase(t, h, func(s *frontendv1.MergeStatus) bool { return s.GetMerged() != nil })
	if got := status.GetMerged().GetAfterActionError(); !strings.Contains(got, "refused the prompt") {
		t.Fatalf("after_action_error = %q, want the handoff's failure carried on the merged status", got)
	}
}
