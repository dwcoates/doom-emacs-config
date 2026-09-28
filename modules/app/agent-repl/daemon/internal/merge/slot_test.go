package merge

import (
	"context"
	"testing"
	"time"
)

// The owner's ruling (2026-09-28): a parked merge does NOT block its
// repository's queue. It yields its slot; the merges behind it proceed; once
// resumed it waits for the slot and makes its merge afresh on the new tip.

// TestAParkedMergeDoesNotBlockTheNextMerge covers the ruling itself.
func TestAParkedMergeDoesNotBlockTheNextMerge(t *testing.T) {
	// Arrange: the first merge parked on its gate, a second queued behind it.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	parkOnABrokenGate(t, h, ctx, 1)
	h.gatePasses("daemon")
	if err := h.o.Enqueue(context.Background(), "ws-2", RequestedByUser); err != nil {
		t.Fatalf("enqueueing the second merge: %v", err)
	}

	// Act.
	if _, err := h.o.pumpOnce(ctx, h.repoKey()); err != nil {
		t.Fatalf("the pump: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts("ws-2"); facts.State != StateMerged {
		t.Fatalf("the merge behind the parked one is %q, want it landed", facts.State)
	}
}

// TestAParkedMergeHoldsNoRepositoryLock covers the cross-process half: the
// kernel lock goes with the slot.
func TestAParkedMergeHoldsNoRepositoryLock(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	parkOnABrokenGate(t, h, ctx, 1)

	// Assert.
	if !h.lockFree(t) {
		t.Fatal("the repository's queue lock is held by a parked merge")
	}
}

// TestAParkedMergeIsOutOfTheLineTheQueueDraws covers the queue a waiting user
// reads: the merge behind a parked one is at the front.
func TestAParkedMergeIsOutOfTheLineTheQueueDraws(t *testing.T) {
	// Arrange: a parked merge, and a second merge enqueued while it is parked.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	parkOnABrokenGate(t, h, ctx, 1)

	// Act.
	if err := h.o.Enqueue(context.Background(), "ws-2", RequestedByUser); err != nil {
		t.Fatalf("enqueueing the second merge: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts("ws-2"); facts.QueuePosition != 1 || facts.QueueDepth != 1 {
		t.Fatalf("the second merge is at %d of %d, want 1 of 1 with the parked one out of line", facts.QueuePosition, facts.QueueDepth)
	}
}

// TestAResumedMergeRebasesOntoTheTargetsNewTip covers the ruling's other half:
// the merges that proceeded moved the target, and the resumed merge is made
// on where it now stands.
func TestAResumedMergeRebasesOntoTheTargetsNewTip(t *testing.T) {
	// Arrange: the target reads "base" until the second merge lands, then
	// "moved"; the first merge parks, the second lands, the first resumes.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	h.git.refSeqs["HEAD"] = []string{"base", "base", "base", "moved"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	parkOnABrokenGate(t, h, ctx, 1)
	h.gatePasses("daemon")
	h.gatePasses("daemon")
	if err := h.o.Enqueue(context.Background(), "ws-2", RequestedByUser); err != nil {
		t.Fatalf("enqueueing the second merge: %v", err)
	}
	if _, err := h.o.pumpOnce(ctx, h.repoKey()); err != nil {
		t.Fatalf("the second merge: %v", err)
	}
	if err := route(h, ctx, "g-1", "go again"); err != nil {
		t.Fatalf("the guidance: %v", err)
	}

	// Act.
	resume(t, h, ctx)

	// Assert.
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if last := h.git.queueBases[len(h.git.queueBases)-1]; last != "moved" {
		t.Fatalf("the resumed merge was made on %q (bases %v), want the target's new tip", last, h.git.queueBases)
	}
}

// TestAResumedMergeAsksThePumpForItsSlot covers the one-grantor rule: a parked
// run whose guidance ended does not take the slot itself.
func TestAResumedMergeAsksThePumpForItsSlot(t *testing.T) {
	// Arrange: a parked merge.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	parkOnABrokenGate(t, h, ctx, 1)

	// Act.
	if err := route(h, ctx, "g-1", "go again"); err != nil {
		t.Fatalf("the guidance: %v", err)
	}
	select {
	case <-h.waiting:
	case <-time.After(5 * time.Second):
		t.Fatal("the resumed merge never asked for its slot")
	}

	// Assert.
	h.o.mu.Lock()
	defer h.o.mu.Unlock()
	if h.o.running[h.repoKey()] != nil || len(h.o.waiters[h.repoKey()]) != 1 {
		t.Fatalf("slot holder %v, waiters %d; want the resumed run waiting for the pump's grant", h.o.running[h.repoKey()], len(h.o.waiters[h.repoKey()]))
	}
}

// TestAKickToARunningPumpIsRemembered pins the lost-wakeup fix: a merge
// enqueued (or a parked run asking for its slot back) in the instant the pump
// is finishing is still admitted, because the pump reads the mark before it
// goes idle.
func TestAKickToARunningPumpIsRemembered(t *testing.T) {
	// Arrange: an asynchronous orchestrator whose pump is running.
	h := newHarness(t)
	h.o.async = true
	h.o.mu.Lock()
	h.o.pumping[h.repoKey()] = true
	h.o.mu.Unlock()

	// Act.
	h.o.kick(h.repoKey())

	// Assert.
	h.o.mu.Lock()
	defer h.o.mu.Unlock()
	if !h.o.kicked[h.repoKey()] {
		t.Fatal("a kick to a running pump was dropped")
	}
}
