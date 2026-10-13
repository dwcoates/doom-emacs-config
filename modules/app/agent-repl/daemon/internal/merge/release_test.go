package merge

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// headRowID is the row value of WS's bubble head for its current ledger.
func headRowID(t *testing.T, h *harness, ws ids.WorkspaceID) string {
	t.Helper()
	lease, ok := h.o.leaseOf(ws)
	if !ok {
		t.Fatalf("%s has no ledger identity", ws)
	}
	return feedid.Encode(headRef(ws, lease)).GetValue()
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
