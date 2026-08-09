package merge

import (
	"context"
	"strings"
	"testing"
	"time"
)

// --- Standing ---------------------------------------------------------------

// THE TWO STANDINGS, one per row: what Standing reports for a merge in flight
// and for one waiting behind it. The distinction is the whole content of the
// dequeue offer's oneof, so it is asserted directly rather than inferred from a
// position number at the call site.
func TestStandingReportsWhereTheMergeSits(t *testing.T) {
	tests := []struct {
		name         string
		workspace    func(head, waiting Request) string
		wantPosition int
		wantAhead    int
		wantHead     bool
	}{
		{
			name:         "the merge in flight is the head",
			workspace:    func(head, _ Request) string { return head.Workspace },
			wantPosition: 1,
			wantAhead:    0,
			wantHead:     true,
		},
		{
			name:         "the merge behind it is waiting",
			workspace:    func(_, waiting Request) string { return waiting.Workspace },
			wantPosition: 2,
			wantAhead:    1,
			wantHead:     false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — one merge running, one queued behind it.
			h := newHarness(t)
			head, waiting := testRequest("a"), testRequest("b")
			if _, err := h.coord.Enqueue(context.Background(), head); err != nil {
				t.Fatalf("Enqueue(a): %v", err)
			}
			<-h.picker.merges
			if _, err := h.coord.Enqueue(context.Background(), waiting); err != nil {
				t.Fatalf("Enqueue(b): %v", err)
			}
			h.sink.awaitPhase(t, PhaseMergeQueued)

			// Act.
			got, queued := h.coord.Standing(tc.workspace(head, waiting))

			// Assert.
			if !queued {
				t.Fatal("Standing() reported nothing queued, want the workspace's entry")
			}
			if got.Position != tc.wantPosition || got.Depth != 2 {
				t.Fatalf("Standing() = position %d of %d, want %d of 2", got.Position, got.Depth, tc.wantPosition)
			}
			if got.Ahead() != tc.wantAhead {
				t.Fatalf("Standing().Ahead() = %d, want %d", got.Ahead(), tc.wantAhead)
			}
			if got.Head != tc.wantHead {
				t.Fatalf("Standing().Head = %v, want %v", got.Head, tc.wantHead)
			}
			if got.Repo != testRepoKey {
				t.Fatalf("Standing().Repo = %q, want %q", got.Repo, testRepoKey)
			}
			if got.RunID == "" {
				t.Fatal("Standing().RunID is empty, want the run the MergeStatus carries")
			}
		})
	}
}

// A workspace with nothing on any queue reports nothing, which is what makes an
// interrupt over an ordinary workspace raise no question at all.
func TestStandingReportsNothingForAnUnqueuedWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	got, queued := h.coord.Standing("/nowhere/ws")

	// Assert.
	if queued {
		t.Fatalf("Standing() = %+v, true; want nothing queued", got)
	}
}

// An unnamed workspace is a construction bug at the call site, not a workspace
// to scan every queue for.
func TestStandingRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, queued := h.coord.Standing("")

	// Assert.
	if queued {
		t.Fatal("Standing(\"\") reported a standing, want nothing")
	}
}

// --- AbortRunning -----------------------------------------------------------

// Aborting the merge in flight retires it: the run publishes its terminal
// `failed` word under the DEQUEUE's cause, the entry leaves the queue, and the
// lease goes back.
func TestAbortRunningRetiresTheMergeInFlight(t *testing.T) {
	// Arrange — a merge sitting inside its cherry-pick.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	runID := enqueuedRunID(t, h.sink.publishedStatuses(), 1)

	// Act.
	aborted, err := h.coord.AbortRunning(context.Background(), req.Workspace)

	// Assert.
	if err != nil || !aborted {
		t.Fatalf("AbortRunning() = (%v, %v), want (true, nil)", aborted, err)
	}
	failed := failedStatus(t, h.sink.publishedStatuses(), runID)
	if failed.GetCause() != dequeuedCause {
		t.Fatalf("failed cause = %q, want the dequeue's own cause %q", failed.GetCause(), dequeuedCause)
	}
	if got := h.queue.Snapshot()[testRepoKey]; len(got) != 0 {
		t.Fatalf("queue after abort = %+v, want empty — a dequeued merge's entry must not survive to be replayed", got)
	}
	select {
	case released := <-h.lease.releases:
		if released != req.Workspace {
			t.Fatalf("released lease = %q, want %q", released, req.Workspace)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("the lease was never released; a lease must not outlive the merge that took it")
	}
}

// THE ABORT WAITS FOR THE RUN TO UNWIND. It returns only once the terminal word
// is published and the entry is gone — never merely once the cancel was
// delivered — so an answered dequeue can never ack while the merge it named is
// still cherry-picking.
func TestAbortRunningWaitsForTheRunToUnwind(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	if _, err := h.coord.AbortRunning(context.Background(), req.Workspace); err != nil {
		t.Fatalf("AbortRunning: %v", err)
	}

	// Assert — nothing is pending at the instant the abort answered, which is
	// only true if it waited for the whole unwind rather than for the cancel.
	if got := h.queue.Snapshot()[testRepoKey]; len(got) != 0 {
		t.Fatalf("queue = %+v at the instant the abort returned, want the entry already gone", got)
	}
}

// A merge PARKED ON A CONFLICT is aborted too. The park serves an abandon
// rendezvous, and a dequeue deliberately does not use it: the cancel reaches
// both the parked case and the running one, so there is one mechanism rather
// than two that have to agree.
func TestAbortRunningRetiresAMergeParkedOnAConflict(t *testing.T) {
	// Arrange — drive the merge to a conflict so it parks.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	runID := enqueuedRunID(t, h.sink.publishedStatuses(), 1)
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "deadbeef", WorkDir: testRebaseWorkDir, BaseHead: baseHeadOfFailure}}
	// The park is live once the coordinator has handed the conflict to the
	// workspace's own shim, which it does from inside the park. Waiting on that
	// call is what makes the abort below land on a PARKED merge rather than
	// racing the park into existence.
	<-h.resolver.calls

	// Act.
	aborted, err := h.coord.AbortRunning(context.Background(), req.Workspace)

	// Assert.
	if err != nil || !aborted {
		t.Fatalf("AbortRunning() = (%v, %v), want (true, nil)", aborted, err)
	}
	failed := failedStatus(t, h.sink.publishedStatuses(), runID)
	if failed.GetCause() != dequeuedCause {
		t.Fatalf("failed cause = %q, want %q", failed.GetCause(), dequeuedCause)
	}
	if got := h.queue.Snapshot()[testRepoKey]; len(got) != 0 {
		t.Fatalf("queue after abort = %+v, want empty", got)
	}
}

// A workspace with NO run in flight reports false without error. It is the
// ordinary answer for a merge that is merely waiting its turn, and the eviction
// half of the dequeue is what reaches that one.
func TestAbortRunningWithNoRunInFlightIsANoOp(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	aborted, err := h.coord.AbortRunning(context.Background(), "/nowhere/ws")

	// Assert.
	if err != nil || aborted {
		t.Fatalf("AbortRunning() = (%v, %v), want (false, nil)", aborted, err)
	}
}

// An unnamed workspace is refused rather than matched against every park.
func TestAbortRunningRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	aborted, err := h.coord.AbortRunning(context.Background(), "")

	// Assert.
	if err == nil || aborted {
		t.Fatalf("AbortRunning(\"\") = (%v, %v), want (false, error)", aborted, err)
	}
}

// A CALLER THAT STOPS WAITING SAYS SO, and says that the cancel still stands.
// Reporting the abort as "did not happen" would be a lie in the other
// direction: the run is going down either way.
func TestAbortRunningReportsACallerThatStoppedWaiting(t *testing.T) {
	// Arrange — a merge in flight whose picker the test never releases, and a
	// caller whose context is already dead.
	h := newHarness(t)
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	aborted, err := h.coord.AbortRunning(ctx, req.Workspace)

	// Assert.
	if err == nil {
		t.Fatal("AbortRunning() error = nil, want the caller's own deadline reported")
	}
	if aborted {
		t.Fatal("AbortRunning() reported an abort it did not observe complete")
	}
	if !strings.Contains(err.Error(), "the cancel stands") {
		t.Fatalf("AbortRunning() error = %q, want it to say the cancel stands", err)
	}
}

// --- Dequeue ----------------------------------------------------------------

// Dequeue takes BOTH halves off: the merge in flight and the one waiting behind
// it. A user answering "take my merge off the queue" means all of it.
func TestDequeueTakesTheRunningAndTheWaitingMergesOff(t *testing.T) {
	// Arrange — one workspace with a merge in flight and a second merge of its
	// own queued behind another workspace's.
	h := newHarness(t)
	head, other, behind := testRequest("a"), testRequest("b"), testRequest("a2")
	behind.Workspace = head.Workspace
	for _, req := range []Request{head, other, behind} {
		if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
			t.Fatalf("Enqueue(%s): %v", req.Name, err)
		}
		if req.Name == head.Name {
			<-h.picker.merges
			continue
		}
		h.sink.awaitPhase(t, PhaseMergeQueued)
	}

	// Act.
	removed, err := h.coord.Dequeue(context.Background(), head.Workspace)

	// Assert — both of this workspace's merges left, the other workspace's did
	// not.
	if err != nil {
		t.Fatalf("Dequeue() error = %v, want nil", err)
	}
	if removed != 2 {
		t.Fatalf("Dequeue() removed = %d, want 2 (the run in flight and the one waiting)", removed)
	}
	remaining := h.queue.Snapshot()[testRepoKey]
	if len(remaining) != 1 || remaining[0].Workspace != other.Workspace {
		t.Fatalf("queue after dequeue = %+v, want only %s", remaining, other.Workspace)
	}
}

// A workspace with nothing queued is the ordinary case, not an error: a dequeue
// answered a heartbeat after the merge landed on its own removes nothing.
func TestDequeueWithNothingQueuedIsANoOp(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	removed, err := h.coord.Dequeue(context.Background(), "/nowhere/ws")

	// Assert.
	if err != nil || removed != 0 {
		t.Fatalf("Dequeue() = (%d, %v), want (0, nil)", removed, err)
	}
}

// An unnamed workspace is refused rather than swept across every queue.
func TestDequeueRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	removed, err := h.coord.Dequeue(context.Background(), "")

	// Assert.
	if err == nil || removed != 0 {
		t.Fatalf("Dequeue() = (%d, %v), want (0, error)", removed, err)
	}
}

// THE DRAIN SURVIVES A DEQUEUE. The repository's queue must go on to the next
// merge afterwards; a dequeue that stopped the drain would strand every merge
// behind it on that repository forever.
func TestTheDrainContinuesAfterADequeue(t *testing.T) {
	// Arrange — a merge in flight and another workspace's merge behind it.
	h := newHarness(t)
	head, next := testRequest("a"), testRequest("b")
	if _, err := h.coord.Enqueue(context.Background(), head); err != nil {
		t.Fatalf("Enqueue(a): %v", err)
	}
	<-h.picker.merges
	if _, err := h.coord.Enqueue(context.Background(), next); err != nil {
		t.Fatalf("Enqueue(b): %v", err)
	}
	h.sink.awaitPhase(t, PhaseMergeQueued)

	// Act.
	if _, err := h.coord.Dequeue(context.Background(), head.Workspace); err != nil {
		t.Fatalf("Dequeue: %v", err)
	}

	// Assert — the next merge is handed to the picker.
	select {
	case got := <-h.picker.merges:
		if !sameRequest(got, next) {
			t.Fatalf("next merge = %+v, want %+v", got, next)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("the drain never handed the next merge to the picker after a dequeue")
	}
}
