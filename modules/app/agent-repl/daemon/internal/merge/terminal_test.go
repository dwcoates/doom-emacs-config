package merge

import (
	"context"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TestTerminalIsPublishedBeforeTheWorktreeIsRemoved covers the teardown's
// order: removing the worktree first would delete the tree a reader is still
// looking at.
func TestTerminalIsPublishedBeforeTheWorktreeIsRemoved(t *testing.T) {
	// Arrange: a landing merge.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	heads := h.feed.heads()
	if len(heads) == 0 || heads[len(heads)-1] != "success" {
		t.Fatalf("the head's arms are %v, want a success terminal", heads)
	}
	if !h.feed.terminalPrecedes(h.git, "remove_worktree") {
		t.Fatal("the worktree was removed before the terminal was published")
	}
}

// TestMergedWorktreeIsRemovedByTheDaemon covers the ruling that moved the
// removal off Emacs: post-merge teardown is the orchestrator's.
func TestMergedWorktreeIsRemovedByTheDaemon(t *testing.T) {
	// Arrange: a landing merge.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if len(h.git.removedWorktrees) != 1 || h.git.removedWorktrees[0] != h.sourceD {
		t.Fatalf("removed worktrees are %v, want the merged source alone", h.git.removedWorktrees)
	}
}

// TestFailedMergeKeepsItsWorktree covers the other half: a failed merge's branch
// still holds work, so its tree is not destroyed.
func TestFailedMergeKeepsItsWorktree(t *testing.T) {
	// Arrange: a merge whose before-action fails.
	h := newHarness(t)
	h.emacsRepo()
	h.configureActions([]string{"before"}, nil)
	h.briefs["before"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert.
	if h.git.seen("remove_worktree") {
		t.Fatal("a failed merge's worktree was removed")
	}
}

// TestMergedWorkspaceIsClosedAndStamped covers the roster's consequence: a
// merged workspace's row carries closed = true and the instant it landed.
func TestMergedWorkspaceIsClosedAndStamped(t *testing.T) {
	// Arrange: a landing merge.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if !h.db.closed[theWorkspace] {
		t.Fatal("the merged workspace's row was not closed")
	}
	if h.db.mergedAt[theWorkspace].IsZero() {
		t.Fatal("the merged workspace was not stamped with when it landed")
	}
}

// TestSelfReloadFiresOnlyAfterTheLeaseIsReleased covers the trigger's ordering:
// bouncing while the lease is still held would leave a lease to recover.
func TestSelfReloadFiresOnlyAfterTheLeaseIsReleased(t *testing.T) {
	// Arrange: a merge into the daemon's own checkout.
	h := newHarness(t)
	h.ownCheckout()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.rollout.mu.Lock()
	defer h.rollout.mu.Unlock()
	if h.rollout.fired != 1 {
		t.Fatalf("the self-reload fired %d times, want exactly once", h.rollout.fired)
	}
	if h.rollout.leasesAtFire == 0 {
		t.Fatal("the self-reload fired before the merge lease was released")
	}
}

// TestSelfReloadCarriesTheLandedRange covers what the trigger classifies: the
// merge commit's own second-parent history, read off the commit.
func TestSelfReloadCarriesTheLandedRange(t *testing.T) {
	// Arrange: a merge landing two commits.
	h := newHarness(t)
	h.ownCheckout()
	h.git.outcomes = append(h.git.outcomes, gitclient.MergeOutcome{Landed: &gitclient.Commit{SHA: "abc123def4567"}})
	h.git.landed = []gitclient.Commit{{SHA: "one"}, {SHA: "two"}}
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.rollout.mu.Lock()
	defer h.rollout.mu.Unlock()
	if len(h.rollout.landed) != 2 {
		t.Fatalf("the trigger got %d commit(s), want the merge's whole landed range", len(h.rollout.landed))
	}
}

// TestSelfReloadSkipsASiblingWorktree covers the exclusion: a sibling worktree
// shares the repository's common dir but is not the tree the running binary was
// built from.
func TestSelfReloadSkipsASiblingWorktree(t *testing.T) {
	// Arrange: the same repository, a different checkout.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.rollout.mu.Lock()
	defer h.rollout.mu.Unlock()
	if h.rollout.fired != 0 {
		t.Fatal("a merge into a sibling worktree triggered the self-reload")
	}
}

// TestSelfReloadSkipsAFailedMerge covers the other guard: nothing landed, so
// there is nothing to deploy.
func TestSelfReloadSkipsAFailedMerge(t *testing.T) {
	// Arrange: a merge into the daemon's own checkout whose before-action fails.
	h := newHarness(t)
	h.ownCheckout()
	h.configureActions([]string{"before"}, nil)
	h.briefs["before"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert.
	h.rollout.mu.Lock()
	defer h.rollout.mu.Unlock()
	if h.rollout.fired != 0 {
		t.Fatal("a failed merge triggered the self-reload")
	}
}

// TestDisplacedTurnIsResubmittedExactlyOnce covers the capture's whole purpose:
// the user's interrupted work comes back, and comes back once.
func TestDisplacedTurnIsResubmittedExactlyOnce(t *testing.T) {
	// Arrange: a merge that displaced a user turn.
	h := newHarness(t)
	h.emacsRepo()
	h.displaceTurn("carry on with the refactor")
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME)
	if n != 1 {
		t.Fatalf("the displaced turn was resubmitted %d times, want exactly once", n)
	}
}

// TestNoDisplacedTurnResubmitsNothing covers the sessionless merge, which
// skips the capture entirely.
func TestNoDisplacedTurnResubmitsNothing(t *testing.T) {
	// Arrange: a merge with nothing in flight.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME); n != 0 {
		t.Fatalf("%d displaced turns were resubmitted, want none", n)
	}
}

// TestOutputAddressIsClearedAtTeardown covers the address's lifetime: the
// session's rows go back to the root feed once the bubble stops being where
// its output belongs.
func TestOutputAddressIsClearedAtTeardown(t *testing.T) {
	// Arrange: a landing merge with a configured prompt, so an address is set.
	h := newHarness(t)
	h.emacsRepo()
	h.configureActions([]string{"before"}, nil)
	h.briefs["before"] = nil
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	if len(h.feed.addresses) < 2 {
		t.Fatalf("only %d addresses were set, want one for the tab and one clearing it", len(h.feed.addresses))
	}
	if last := h.feed.addresses[len(h.feed.addresses)-1]; last != nil {
		t.Fatalf("the last address is %+v, want it cleared", last)
	}
}

// TestLedgerRecordsEveryTabInterval covers the ledger's whole content: the
// intervals a replay reconstructs from, and nothing of the phases' content.
func TestLedgerRecordsEveryTabInterval(t *testing.T) {
	// Arrange: a merge that opens the merge and tests tabs.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	entries, err := h.db.MergeLedger(context.Background(), theWorkspace)
	if err != nil || len(entries) != 1 {
		t.Fatalf("the ledger holds %d entries, want one per lease: %v", len(entries), err)
	}
	var kinds []string
	for _, interval := range entries[0].Intervals {
		kinds = append(kinds, interval.Kind)
	}
	want := []string{TabQueue, TabQueue, TabMerge, TabMerge, TabTests, TabTests}
	if !equal(kinds, want) {
		t.Fatalf("the ledger recorded %v, want an open and a close per tab: %v", kinds, want)
	}
}

// TestLedgerRecordsEachRoundsOutcome covers what a closed interval carries: the
// round's outcome, which is what makes a replayed ledger legible.
func TestLedgerRecordsEachRoundsOutcome(t *testing.T) {
	// Arrange: a merge that conflicts and is resolved.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.git.conflicted = [][]string{{}}
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeLedger(context.Background(), theWorkspace)
	var outcomes []string
	for _, interval := range entries[0].Intervals {
		if interval.EndedAt != nil {
			outcomes = append(outcomes, interval.Kind+":"+interval.Outcome)
		}
	}
	want := []string{TabQueue + ":succeeded", TabMerge + ":conflicted", TabConflicts + ":succeeded", TabTests + ":succeeded"}
	if !equal(outcomes, want) {
		t.Fatalf("the ledger's outcomes are %v, want %v", outcomes, want)
	}
}

// TestInterruptRaisesTheDequeueOffer covers the ruling that an interrupt no
// longer silently yanks a merge off the queue: the daemon asks.
func TestInterruptRaisesTheDequeueOffer(t *testing.T) {
	// Arrange: a queued merge.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Assert.
	offer := h.holds.standing()
	if offer == nil || offer.GetMergeDequeue() == nil {
		t.Fatalf("the tray holds %v, want the merge dequeue offer", offer)
	}
	if !strings.Contains(offer.GetMergeDequeue().GetHeadline().GetText(), "ws-one") {
		t.Fatalf("the offer's sentence is %q, want it to name the workspace",
			offer.GetMergeDequeue().GetHeadline().GetText())
	}
}

// TestInterruptRaisesNothingWithoutAQueuedMerge covers the guard: an interrupt
// of an ordinary workspace poses no question.
func TestInterruptRaisesNothingWithoutAQueuedMerge(t *testing.T) {
	// Arrange: a workspace with no queued merge.
	h := newHarness(t)

	// Act.
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Assert.
	if h.holds.standing() != nil {
		t.Fatal("an interrupt raised a dequeue offer for a workspace with no merge")
	}
}

// TestAnswerDequeueKeepKeepsTheSlot covers the keep answer: the merge stays
// exactly where it was.
func TestAnswerDequeueKeepKeepsTheSlot(t *testing.T) {
	// Arrange: a queued merge with the offer standing.
	h := newHarness(t)
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Act.
	err := h.o.AnswerDequeue(context.Background(), theWorkspace, true)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerDequeue failed: %v", err)
	}
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 {
		t.Fatalf("the queue holds %d entries, want the kept merge", len(entries))
	}
	if h.holds.standing() != nil {
		t.Fatal("the answered offer is still standing")
	}
}

// TestAnswerDequeueReleaseTakesTheMergeOff covers the release answer, which is
// a DIFFERENT end from an eviction and records its own cause.
func TestAnswerDequeueReleaseTakesTheMergeOff(t *testing.T) {
	// Arrange: a queued merge with the offer standing.
	h := newHarness(t)
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Act.
	err := h.o.AnswerDequeue(context.Background(), theWorkspace, false)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerDequeue failed: %v", err)
	}
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 0 {
		t.Fatalf("the queue still holds %d entries", len(entries))
	}
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if len(h.db.dropped) != 1 || h.db.dropped[0] != string(theWorkspace)+":dequeued" {
		t.Fatalf("the drop was recorded as %v, want the dequeued cause", h.db.dropped)
	}
}

// TestAnswerDequeueRefusesWithNoOfferStanding covers the refusal an answer gets
// when the question it answers is gone.
func TestAnswerDequeueRefusesWithNoOfferStanding(t *testing.T) {
	// Arrange: a queued merge with no offer raised.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	err := h.o.AnswerDequeue(context.Background(), theWorkspace, false)

	// Assert.
	arm, refused := Refused(err)
	if !refused || arm != ArmNoOfferStanding {
		t.Fatalf("AnswerDequeue answered %v, want the %s refusal", err, ArmNoOfferStanding)
	}
}

// TestAbandonedMergeDrawsNoTerminalCommit covers the third end: a merge taken
// off the queue before it ran has no commit and no failure, because nothing of
// it ever ran.
func TestAbandonedMergeDrawsNoTerminalCommit(t *testing.T) {
	// Arrange: a queued merge.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict failed: %v", err)
	}

	// Assert.
	if facts, has := h.o.Facts(theWorkspace); has && facts.State != "none" {
		t.Fatalf("an abandoned merge left facts %+v", facts)
	}
	if h.git.seen("merge_no_ff") {
		t.Fatal("an abandoned merge ran the merge")
	}
}

// TestOccupancyIsReleasedAtTeardown covers the guard that backs the lease row:
// it is dropped with everything else the run held.
func TestOccupancyIsReleasedAtTeardown(t *testing.T) {
	// Arrange: a landing merge.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.occupancyReleases != 1 {
		t.Fatalf("the occupancy guard was released %d times, want once", h.occupancyReleases)
	}
}

// TestRepositoryLockIsReleasedAtTeardown covers the queue's handoff: the next
// merge in a repository cannot start until the lock comes back.
func TestRepositoryLockIsReleasedAtTeardown(t *testing.T) {
	// Arrange: a completed merge.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Act.
	lock, taken, err := acquireRepoLock(h.o.lockDir, string(h.repoKey()))

	// Assert.
	if err != nil || !taken {
		t.Fatalf("the repository's lock was not released: taken=%v err=%v", taken, err)
	}
	lock.Release()
}

// displaceTurn records a user turn the merge will displace.
func (h *harness) displaceTurn(text string) {
	turn := wsm.NewTurnID()
	h.displaced = &Displaced{Turn: turn, Text: text}
	h.db.mu.Lock()
	h.db.turns[theWorkspace] = append(h.db.turns[theWorkspace], wsm.Turn{
		ID: turn, Workspace: theWorkspace, Text: text, Displaced: true,
	})
	h.db.mu.Unlock()
}

// terminalPrecedes reports whether the head's terminal was published BEFORE git
// was asked for a call, on the shared sequence both fakes stamp.
func (f *fakeFeed) terminalPrecedes(g *fakeGit, call string) bool {
	f.mu.Lock()
	terminalAt := -1
	for _, row := range f.rows {
		activity := row.Row.GetActivity()
		if activity == nil || activity.GetMerge() == nil {
			continue
		}
		if activity.GetMerge().GetSuccess() != nil || activity.GetMerge().GetError() != nil {
			terminalAt = row.At
			break
		}
	}
	f.mu.Unlock()
	g.mu.Lock()
	callAt, called := g.at[call]
	g.mu.Unlock()
	return terminalAt > 0 && called && terminalAt < callAt
}

// unusedIDs keeps the id alias named in this file's imports honest.
var _ ids.WorkspaceID = theWorkspace

// TestLedgerRecordsTheQueueInterval covers that the QUEUE is a tab like every
// other: its interval opens at admission and closes when the run leaves the
// queue for its first phase.
func TestLedgerRecordsTheQueueInterval(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	entries, err := h.db.MergeLedger(context.Background(), theWorkspace)
	if err != nil || len(entries) != 1 {
		t.Fatalf("the ledger holds %d entries, want one per lease: %v", len(entries), err)
	}
	var closed *wsm.TabInterval
	for i, interval := range entries[0].Intervals {
		if interval.Kind == TabQueue && interval.EndedAt != nil {
			closed = &entries[0].Intervals[i]
		}
	}
	if closed == nil {
		t.Fatalf("the ledger holds no closed queue interval: %+v", entries[0].Intervals)
	}
	if closed.EndedAt.Before(closed.StartedAt) {
		t.Fatalf("the queue interval = %+v, want a well-formed [started, ended] interval", closed)
	}
}
