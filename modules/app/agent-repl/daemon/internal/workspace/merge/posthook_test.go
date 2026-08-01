package merge

import (
	"context"
	"errors"
	"testing"
)

// --- which outcomes fire the hook ---------------------------------------

func TestPostMergeHookFiresExactlyOnceOnAMergedOutcome(t *testing.T) {
	// Arrange.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act — the cherry-pick lands.
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}

	// Assert — the hook saw exactly this merge, once.
	got := <-hook.calls
	if got.Workspace != req.Workspace {
		t.Fatalf("hook request workspace = %q, want %q", got.Workspace, req.Workspace)
	}
	hook.results <- nil
	if extra := len(hook.calls); extra != 0 {
		t.Fatalf("hook calls pending = %d, want the hook fired exactly once", extra)
	}
}

func TestPostMergeHookNeverFiresOnAFailedOutcome(t *testing.T) {
	// Arrange.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act — the pick aborted with nothing to resolve.
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeFailed}}
	<-h.lease.releases

	// Assert — a failed merge has no aftermath to hand anyone.
	if len(hook.calls) != 0 {
		t.Fatalf("hook calls = %d, want none for a failed merge", len(hook.calls))
	}
}

func TestPostMergeHookNeverFiresOnADriverError(t *testing.T) {
	// Arrange — merge.Driver rejected the request outright.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act.
	<-h.picker.merges
	h.picker.results <- pickResult{err: sentinelError("dirty source worktree")}
	<-h.lease.releases

	// Assert.
	if len(hook.calls) != 0 {
		t.Fatalf("hook calls = %d, want none when the driver refused", len(hook.calls))
	}
}

func TestPostMergeHookNeverFiresWhileAConflictIsParked(t *testing.T) {
	// Arrange.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act — the pick conflicts, and the shim attempt is left pending.
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	<-h.resolver.calls

	// Assert — the merge has not landed, so nothing is owed to a parent.
	if len(hook.calls) != 0 {
		t.Fatalf("hook calls = %d, want none while the merge is still parked", len(hook.calls))
	}
}

func TestPostMergeHookNeverFiresOnAnAbandonedConflict(t *testing.T) {
	// Arrange — a parked conflict whose workspace is then closed.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	<-h.resolver.calls

	// Act.
	abandoned, err := h.coord.Abandon(context.Background(), req.Workspace)
	if err != nil {
		t.Fatalf("Abandon: %v", err)
	}
	if !abandoned {
		t.Fatalf("Abandon reported nothing to give up, want the parked merge abandoned")
	}
	<-h.lease.releases

	// Assert — an abandoned merge never merged.
	if len(hook.calls) != 0 {
		t.Fatalf("hook calls = %d, want none for an abandoned merge", len(hook.calls))
	}
}

func TestPostMergeHookFiresOnAMergeThatLandedAfterAResume(t *testing.T) {
	// Arrange — a conflict a human resolved and continued.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeConflict, ConflictCommit: "abc1234"}}
	<-h.resolver.calls

	// Act — the human resume drives it to merged.
	resumed := make(chan error, 1)
	go func() { resumed <- h.coord.Resume(context.Background(), req) }()
	<-h.picker.resumes
	h.picker.resumeResults <- pickResult{res: Result{Outcome: OutcomeMerged}}
	if err := <-resumed; err != nil {
		t.Fatalf("Resume: %v", err)
	}

	// Assert — a merge that landed late is merged all the same.
	got := <-hook.calls
	if got.Workspace != req.Workspace {
		t.Fatalf("hook request workspace = %q, want %q", got.Workspace, req.Workspace)
	}
	hook.results <- nil
}

// --- off the drain goroutine --------------------------------------------

func TestPostMergeHookDoesNotStallTheRepositorysQueue(t *testing.T) {
	// Arrange — two merges on ONE repository, and a hook that never returns
	// for the first one.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	first, second := testRequest("a"), testRequest("b")
	if _, err := h.coord.Enqueue(context.Background(), first); err != nil {
		t.Fatalf("Enqueue first: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	// The hook is now blocked inside AfterMerged, holding nothing.
	<-hook.calls
	if _, err := h.coord.Enqueue(context.Background(), second); err != nil {
		t.Fatalf("Enqueue second: %v", err)
	}

	// Act — the second merge must reach the picker while the hook is blocked.
	got := <-h.picker.merges

	// Assert.
	if got.Workspace != second.Workspace {
		t.Fatalf("second merge workspace = %q, want %q", got.Workspace, second.Workspace)
	}
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeFailed}}
	hook.results <- nil
}

func TestPostMergeHookRunsAfterTheLeaseIsReleased(t *testing.T) {
	// Arrange — the hook prompts the PARENT, so the child's lease must already
	// be back before it runs.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	<-h.lease.acquires

	// Act.
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-hook.calls

	// Assert — by the time the hook is running the lease is no longer held.
	if h.lease.Held(req.Workspace) {
		t.Fatalf("lease still held while the post-merge hook runs, want it released first")
	}
	hook.results <- nil
}

func TestPostMergeHookRunsAfterTheQueueEntryIsDropped(t *testing.T) {
	// Arrange.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges

	// Act.
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-hook.calls

	// Assert — the durable queue no longer carries the merged entry.
	for repo, reqs := range h.queue.Snapshot() {
		for _, entry := range reqs {
			if entry.Workspace == req.Workspace {
				t.Fatalf("repo %s still carries the merged entry %q while the hook runs", repo, entry.Workspace)
			}
		}
	}
	hook.results <- nil
}

// --- hook failures ------------------------------------------------------

func TestPostMergeHookFailureIsRetainedNotSwallowed(t *testing.T) {
	// Arrange.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	req := testRequest("a")
	if _, err := h.coord.Enqueue(context.Background(), req); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-hook.calls

	// Act — the handoff fails.
	boom := sentinelError("parent session unreachable")
	hook.results <- boom
	// Close waits for the hook goroutine, which is what makes the retained
	// record readable without a poll.
	if err := h.coord.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	failures := h.coord.PostMergeFailures()
	if len(failures) != 1 {
		t.Fatalf("PostMergeFailures() = %v, want exactly the one hook failure", failures)
	}
	if !errors.Is(failures[0].Err, boom) {
		t.Fatalf("retained failure err = %v, want %v", failures[0].Err, boom)
	}
	if failures[0].Workspace != req.Workspace {
		t.Fatalf("retained failure workspace = %q, want %q", failures[0].Workspace, req.Workspace)
	}
}

func TestPostMergeHookFailureRecordsNoMergeFailedTransition(t *testing.T) {
	// Arrange — the commits are on the target, so a failed handoff must not
	// make the pushed state claim otherwise.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-hook.calls

	// Act.
	hook.results <- sentinelError("parent session unreachable")
	if err := h.coord.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	h.sink.mu.Lock()
	defer h.sink.mu.Unlock()
	for _, tr := range h.sink.got {
		if tr.phase == PhaseMergeFailed {
			t.Fatalf("sink recorded %v, want no merge_failed for a failed post-merge handoff", tr)
		}
	}
}

func TestPostMergeHookSuccessRetainsNoFailure(t *testing.T) {
	// Arrange.
	hook := newFakePostMergeHook(4)
	defer close(hook.stop)
	h := newHarnessWith(t, harnessOpts{postMerge: hook})
	if _, err := h.coord.Enqueue(context.Background(), testRequest("a")); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	<-h.picker.merges
	h.picker.results <- pickResult{res: Result{Outcome: OutcomeMerged}}
	<-hook.calls

	// Act.
	hook.results <- nil
	if err := h.coord.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if failures := h.coord.PostMergeFailures(); len(failures) != 0 {
		t.Fatalf("PostMergeFailures() = %v, want none", failures)
	}
}

// --- construction -------------------------------------------------------

func TestNewCoordinatorRequiresThePostMergeHook(t *testing.T) {
	// Arrange — every other dependency present.
	q, _ := newTestQueue(t)

	// Act.
	coord, err := NewCoordinator(CoordinatorConfig{
		Logf:     func(string, ...any) {},
		Sink:     newSyncSink(1),
		Queue:    q,
		Keyer:    fakeKeyer{},
		Picker:   newFakePicker(1),
		Lease:    newFakeLease(1),
		Resolver: newFakeResolver(1),
	})

	// Assert — a coordinator that drops the parent handoff is broken, not
	// degraded.
	if err == nil {
		t.Fatalf("NewCoordinator() error = nil, want the missing hook refused")
	}
	if coord != nil {
		t.Fatalf("NewCoordinator() = %v, want no coordinator", coord)
	}
}
