package merge

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// headRowID is the row value of WS's bubble head for its current ledger.
func headRowID(t *testing.T, h *harness, ws ids.WorkspaceID) string {
	t.Helper()
	lease, ok := h.o.leaseOf(ws)
	if !ok {
		t.Fatalf("%s has no ledger identity", ws)
	}
	return headFeedID(ws, lease).GetValue()
}

// lastQueueTabOf answers the newest queue snapshot drawn on WS's bubble.
func (f *fakeFeed) lastQueueTabOf(ws ids.WorkspaceID) *frontendv1.FeedMergeQueue {
	f.mu.Lock()
	defer f.mu.Unlock()
	var snap *frontendv1.FeedMergeQueue
	for _, r := range f.rows {
		if q := r.Row.GetMergeTab().GetQueue(); r.WS == ws && q != nil {
			snap = q.GetQueue()
		}
	}
	return snap
}

// lastHeadOf answers WS's newest published bubble head row.
func (f *fakeFeed) lastHeadOf(ws ids.WorkspaceID, row string) (synthesized, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	var last synthesized
	found := false
	for _, r := range f.rows {
		if r.WS == ws && r.Row.GetId().GetValue() == row {
			last, found = r, true
		}
	}
	return last, found
}

func TestEvictingAQueuedMergeClosesItsBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	head := headRowID(t, h, theWorkspace)

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	folds := h.feed.foldsOf(theWorkspace)
	if len(folds) != 1 || folds[0].row != head || !folds[0].folded {
		t.Fatalf("folds = %+v, want the released bubble's head closed once", folds)
	}
}

func TestDequeuingAMergeClosesItsBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	head := headRowID(t, h, theWorkspace)

	// Act.
	if err := h.o.dequeue(context.Background(), theWorkspace, CauseUserDequeue); err != nil {
		t.Fatalf("dequeue: %v", err)
	}

	// Assert.
	if folds := h.feed.foldsOf(theWorkspace); len(folds) != 1 || folds[0].row != head || !folds[0].folded {
		t.Fatalf("folds = %+v, want the released bubble's head closed once", folds)
	}
}

func TestEvictingARunningMergeClosesItsBubble(t *testing.T) {
	// Arrange: the merge holds inside its gate.
	c := newChain(t)
	inGate, _ := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	head := headRowID(t, c.h, theWorkspace)
	done := admitAsync(c.h, context.Background())
	<-inGate

	// Act.
	if err := c.h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}
	<-done

	// Assert.
	if folds := c.h.feed.foldsOf(theWorkspace); len(folds) != 1 || folds[0].row != head || !folds[0].folded {
		t.Fatalf("folds = %+v, want the running merge's bubble closed once", folds)
	}
}

func TestAMergeGivenUpUnderAClosedWorkspaceKeepsItsBubbleOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert.
	if folds := h.feed.foldsOf(theWorkspace); len(folds) != 0 {
		t.Fatalf("folds = %+v, want none: a merge given up under its workspace closing is no release", folds)
	}
}

func TestAReleasedMergesBubbleSettlesOutOfItsPinnedSpot(t *testing.T) {
	// Arrange: a queued sync merge's head is drawn pinned.
	h := newHarness(t)
	enqueue(t, h)
	head := headRowID(t, h, theWorkspace)
	if before, ok := h.feed.lastHeadOf(theWorkspace, head); !ok || !before.Pinned {
		t.Fatalf("the queued head = %+v (found %v), want it pinned before the release", before, ok)
	}

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	after, _ := h.feed.lastHeadOf(theWorkspace, head)
	if after.Pinned || after.Row.GetActivity().GetMerge().GetError().GetAbandoned() == nil {
		t.Fatalf("the released head = pinned %v, abandoned %v; want it settled as an ended row",
			after.Pinned, after.Row.GetActivity().GetMerge().GetError().GetAbandoned())
	}
}

func TestReleasingAMergeRedrawsTheQueueTabOfEveryMemberStillInLine(t *testing.T) {
	// Arrange: three merges in line.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-b", "b")
	queueAnother(t, h, "ws-c", "c")
	for _, ws := range []ids.WorkspaceID{theWorkspace, "ws-c"} {
		if !listsWorkspace(h.feed.lastQueueTabOf(ws), "ws-b") {
			t.Fatalf("%s's queue tab does not list ws-b before the release; the test proves nothing", ws)
		}
	}

	// Act: the middle one is released.
	if err := h.o.Evict(context.Background(), "ws-b"); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert: both members still in line see a queue without it.
	for _, ws := range []ids.WorkspaceID{theWorkspace, "ws-c"} {
		snap := h.feed.lastQueueTabOf(ws)
		if snap.GetCurrent().GetWorkspace().GetRef().GetId() != string(ws) {
			t.Fatalf("%s's newest queue tab = %v, want one drawn for it", ws, snap)
		}
		if listsWorkspace(snap, "ws-b") {
			t.Fatalf("%s's queue tab still lists the released ws-b: %v", ws, snap)
		}
	}
}

// listsWorkspace reports whether SNAP names WS anywhere in line.
func listsWorkspace(snap *frontendv1.FeedMergeQueue, ws ids.WorkspaceID) bool {
	entries := append(append([]*frontendv1.FeedMergeQueueEntry{}, snap.GetAhead()...), snap.GetBehind()...)
	if c := snap.GetCurrent(); c != nil {
		entries = append(entries, c)
	}
	for _, entry := range entries {
		if entry.GetWorkspace().GetRef().GetId() == string(ws) {
			return true
		}
	}
	return false
}

func TestAnAskedForReleaseIsRecordedAtInfoNotWarn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	for _, r := range h.logs.Records() {
		if r.Operation == "daemon.merge.drop_queued" && r.Level != "info" {
			t.Fatalf("the eviction recorded %s %q, want info", r.Level, r.Message)
		}
	}
	if r, ok := h.recordFor("info", "daemon.merge.drop_queued"); !ok || r.Message != "released a queued merge" {
		t.Fatalf("the eviction's release record = %+v (found %v), want it at info", r, ok)
	}
}

func TestEvictingAMergeWithARebaseStandingAbortsItAndSaysSo(t *testing.T) {
	// Arrange: the merge holds inside its gate, and a rebase stands in its
	// worktree, as one taken out mid conflict resolution leaves it.
	c := newChain(t)
	inGate, _ := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	done := admitAsync(c.h, context.Background())
	<-inGate
	dir := c.dirs[theWorkspace]
	c.git.cmu.Lock()
	c.git.rebases[dir] = &chainRebase{}
	c.git.cmu.Unlock()

	// Act.
	if err := c.h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}
	<-done

	// Assert.
	c.git.cmu.Lock()
	_, standing := c.git.rebases[dir]
	c.git.cmu.Unlock()
	if standing {
		t.Fatal("the rebase standing in the evicted merge's worktree was not aborted")
	}
	if !loggedLike(c.h, logLike{level: "info", op: "daemon.merge.abandoned", text: "abandoned a running merge", context: map[string]string{"rebase_aborted": "true"}}) {
		t.Fatalf("no record says the rebase was aborted: %+v", c.h.logs.Records())
	}
}

func TestAReleasedMergeIsHandedBackReleasedAndRaisesNoResolvedBanner(t *testing.T) {
	tests := []struct {
		name  string
		end   func(t *testing.T) *harness
		cause AbandonCause
	}{
		{name: "an evicted queued in-place merge", cause: CauseUserDrop, end: func(t *testing.T) *harness {
			h := newHarness(t)
			enqueue(t, h)
			if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
				t.Fatalf("Evict: %v", err)
			}
			return h
		}},
		{name: "an evicted queued detached merge", cause: CauseUserDrop, end: func(t *testing.T) *harness {
			h := newHarness(t)
			if err := requestDetached(t, h, RequestedByUser); err != nil {
				t.Fatalf("Enqueue: %v", err)
			}
			if err := h.o.Evict(context.Background(), theMergeWorkspace); err != nil {
				t.Fatalf("Evict: %v", err)
			}
			return h
		}},
		{name: "an evicted running in-place merge", cause: CauseUserDrop, end: func(t *testing.T) *harness {
			return ended(t, phaseRunning, resolutions[0])
		}},
		{name: "an evicted running detached merge", cause: CauseUserDrop, end: func(t *testing.T) *harness {
			return endedObserved(t, phaseRunning, resolutions[0], func(h *harness) { markMergeWorkspace(h, otherWorkspace) })
		}},
		{name: "a dequeued running in-place merge", cause: CauseUserDequeue, end: func(t *testing.T) *harness {
			return ended(t, phaseRunning, resolutions[1])
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange and Act.
			h := tt.end(t)

			// Assert: one handback, marked released with its cause.
			sent := h.detached.sent()
			if len(sent) != 1 || sent[0].outcome.Cause != tt.cause || !sent[0].outcome.Released() {
				t.Fatalf("handbacks = %+v, want one marked released by %s", sent, tt.cause)
			}
			// Assert: no resolved banner, and the skip recorded at INFO.
			for _, b := range h.banners.kinds() {
				if b.kind != "queued" {
					t.Fatalf("banners = %+v, want no resolved banner for a released merge", h.banners.kinds())
				}
			}
			rec, ok := h.recordFor(dlog.LevelInfo, "daemon.merge.ended")
			if !ok || rec.Context["cause"] != string(tt.cause) || rec.Context["outcome"] != "abandoned" {
				t.Fatalf("record = %+v (found %v), want the skipped banner recorded with its cause", rec, ok)
			}
		})
	}
}

func TestOnlyAnAbandonmentReleasedByItsCauseIsReleased(t *testing.T) {
	tests := []struct {
		name string
		out  DetachedOutcome
		want bool
	}{
		{name: "evicted", out: DetachedOutcome{Kind: DetachedAbandoned, Cause: CauseUserDrop}, want: true},
		{name: "dequeued", out: DetachedOutcome{Kind: DetachedAbandoned, Cause: CauseUserDequeue}, want: true},
		{name: "its workspace closed", out: DetachedOutcome{Kind: DetachedAbandoned, Cause: CauseWorkspaceClosed}},
		{name: "the daemon shut down", out: DetachedOutcome{Kind: DetachedAbandoned, Cause: CauseDaemonShutdown}},
		{name: "the merge ahead did not land", out: DetachedOutcome{Kind: DetachedAbandoned, Cause: CauseAheadDidNotLand}},
		{name: "never forked", out: DetachedOutcome{Kind: DetachedAbandoned}},
		{name: "failed", out: DetachedOutcome{Kind: DetachedFailed}},
		{name: "landed", out: DetachedOutcome{Kind: DetachedLanded}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.out.Released()

			// Assert
			if got != tt.want {
				t.Fatalf("Released() = %v, want %v", got, tt.want)
			}
		})
	}
}
