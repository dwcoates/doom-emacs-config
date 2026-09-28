package merge

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
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

// TestMergedWorkspaceSessionStopsBeforeWorktreeRemoval covers the process/tree
// boundary: the shim can write through its working directory until it is
// reaped, so git cannot remove that directory first.
func TestMergedWorkspaceSessionStopsBeforeWorktreeRemoval(t *testing.T) {
	// Arrange.
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
	stopped := append([]ids.WorkspaceID(nil), h.stoppedSessions...)
	forces := append([]bool(nil), h.stopForces...)
	stopAt := h.stopAt
	h.mu.Unlock()
	h.git.mu.Lock()
	removeAt := h.git.at["remove_worktree"]
	h.git.mu.Unlock()
	if len(stopped) != 1 || stopped[0] != theWorkspace || len(forces) != 1 || !forces[0] {
		t.Fatalf("session stops = %v forces=%v, want one forced stop for %s", stopped, forces, theWorkspace)
	}
	if stopAt == 0 || removeAt == 0 || stopAt >= removeAt {
		t.Fatalf("stop sequence=%d remove sequence=%d, want the session reaped before removal", stopAt, removeAt)
	}
	record, found := recordWith(h, "debug", "daemon.merge.teardown")
	if !found || record.Context["workspace"] != string(theWorkspace) || record.Context["worktree"] != h.sourceD || record.Context["force"] != true {
		t.Fatalf("teardown record = %+v found=%v, want the workspace, worktree, and force=true", record, found)
	}
}

// TestFailedSessionStopKeepsTheMergedWorktree covers the failure edge: a live
// process's working directory remains intact and the teardown records why.
func TestFailedSessionStopKeepsTheMergedWorktree(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	h.stopErr = errors.New("shim did not reap")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if h.git.seen("remove_worktree") {
		t.Fatal("the merged worktree was removed after its session failed to stop")
	}
	record, found := recordWith(h, "error", "daemon.merge.teardown")
	if !found || record.Context["error"] != "shim did not reap" || record.Context["force"] != true {
		t.Fatalf("teardown record = %+v found=%v, want the stand-down failure and force=true", record, found)
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
	// Arrange: a merge that conflicts, is resolved on the branch, and lands on
	// its second attempt.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.landsCleanly("abc123def4567")
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
	want := []string{TabQueue + ":succeeded", TabMerge + ":conflicted", TabConflicts + ":succeeded", TabMerge + ":succeeded", TabTests + ":succeeded"}
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

// TestAnswerDequeuesAbandonedTerminalCarriesTheUsersCause is the other reachable
// cause: the user released the slot, and the summary says so rather than
// reading like the operator's eviction.
func TestAnswerDequeuesAbandonedTerminalCarriesTheUsersCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Act.
	if err := h.o.AnswerDequeue(context.Background(), theWorkspace, false); err != nil {
		t.Fatalf("AnswerDequeue failed: %v", err)
	}

	// Assert.
	if got := h.feed.lastAbandonedSummary(); got != "the user released this merge's queue slot" {
		t.Fatalf("the dequeued merge's summary = %q, want the user's cause", got)
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
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmNoOfferStanding {
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

// TestResubmittingTheDisplacedTurnClearsItsDurableMark covers the handoff to
// the boot recovery: a turn this run put back is no longer owed, so the next
// boot's sweep must find nothing to put back.
func TestResubmittingTheDisplacedTurnClearsItsDurableMark(t *testing.T) {
	// Arrange: a merge that displaced a user turn and lands.
	h := newHarness(t)
	h.emacsRepo()
	h.displaceTurn("carry on with the refactor")
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Act: a later boot sweeps the displaced marks.
	left, err := h.db.AllDisplacedTurns(context.Background())
	if err != nil {
		t.Fatalf("AllDisplacedTurns: %v", err)
	}

	// Assert.
	if len(left) != 0 {
		t.Fatalf("turns still marked displaced after the release = %v, want none", left)
	}
}

// TestAReleaseResubmitsNothingWhenTheDisplacedRecordIsAlreadyClaimed covers
// the loser of the two owners: a boot recovery that already put the turn back
// leaves no mark, and the release must not put it back a second time.
func TestAReleaseResubmitsNothingWhenTheDisplacedRecordIsAlreadyClaimed(t *testing.T) {
	// Arrange: a run holding the capture, with NO durable mark left — which is
	// exactly what a sweep that already claimed the record leaves behind.
	h := newHarness(t)
	h.emacsRepo()
	h.displaced = &Displaced{Turn: wsm.NewTurnID(), Text: "carry on with the refactor"}
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
		t.Fatalf("the release resubmitted %d turns for a record it does not own, want none", n)
	}
}

// ---------------------------------------------------------------------------
// The shutdown drain: what a SIGTERM landing mid-merge does.
// ---------------------------------------------------------------------------

// TestTheDrainLetsATerminalFinishItsDurableStamps covers the defect the drain
// exists for: a stop landing inside a run's terminal must not take the state
// client away from the landing's own stamps.
func TestTheDrainLetsATerminalFinishItsDurableStamps(t *testing.T) {
	// Arrange: a merge held at the top of its terminal, with the drain
	// releasing it from inside its own wait — so the wait is real rather
	// than assumed.
	h := newHarness(t)
	held, release := make(chan struct{}), make(chan struct{})
	h.pauseInTerminal = func(context.Context, ids.WorkspaceID) {
		close(held)
		<-release
	}
	o, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the orchestrator: %v", err)
	}
	h.o = o
	o.onDrainWait = func() { close(release) }
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-held

	// Act: the orderly exit's drain.
	o.Drain(context.Background())

	// Assert: the terminal's durable stamps all landed, and nothing failed.
	if err := <-done; err != nil {
		t.Fatalf("the merge failed: %v", err)
	}
	h.db.mu.Lock()
	_, stamped := h.db.mergedAt[theWorkspace]
	closed := h.db.closed[theWorkspace]
	h.db.mu.Unlock()
	if !stamped {
		t.Fatal("merged_at was never stamped, want the drain to have held the store open for it")
	}
	if !closed {
		t.Fatal("the merged workspace was never closed, want the drain to have held the store open for it")
	}
	if failures := recordsAtLevel(h, "error"); len(failures) > 0 {
		t.Fatalf("the drained terminal produced %d error records, want none: %v", len(failures), failures)
	}
}

// TestTheDrainAnnouncesAMidPhaseMergeInsteadOfWaitingForIt covers what is NOT
// drained: a merge in its long phases is abandoned to the boot recovery, and
// the record of that is an INFO naming the phase rather than a pile of failed
// writes.
func TestTheDrainAnnouncesAMidPhaseMergeInsteadOfWaitingForIt(t *testing.T) {
	// Arrange: a merge held inside its test gate, which is a long phase.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	inGate, release := make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	h.o.Drain(context.Background())

	// Assert: one INFO naming the phase, and no error record at all.
	record, found := recordWith(h, "info", "daemon.merge.drain")
	if !found {
		t.Fatal("the drain wrote no INFO record for the mid-phase merge")
	}
	if record.Context["phase"] != TabTests {
		t.Fatalf("the mid-phase record names phase %v, want %q", record.Context["phase"], TabTests)
	}
	if record.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("the mid-phase record names workspace %v, want %q", record.Context["workspace"], theWorkspace)
	}
	if failures := recordsAtLevel(h, "error"); len(failures) > 0 {
		t.Fatalf("the abandoned mid-phase merge produced %d error records, want none: %v", len(failures), failures)
	}
	close(release)
	if err := <-done; err != nil {
		t.Fatalf("the merge failed: %v", err)
	}
}

// TestTheDrainWarnsWhatOneExpiredBoundLeftUnstamped covers the bound itself:
// the drain gives up, and what it gave up on is named so the boot recovery's
// work is visible before that boot rather than after it.
func TestTheDrainWarnsWhatOneExpiredBoundLeftUnstamped(t *testing.T) {
	tests := []struct {
		name          string
		owed          string
		wantUnstamped string
	}{
		{name: "a landing's terminal", owed: terminalOwedLanded, wantUnstamped: terminalOwedLanded},
		{name: "a failure's terminal", owed: terminalOwedFailed, wantUnstamped: terminalOwedFailed},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a registered terminal that never completes, under a
			// bound short enough that the assertion is not a wait.
			h := newHarness(t)
			h.o.drainBound = time.Millisecond
			h.o.terminals = map[ids.WorkspaceID]*terminal{
				theWorkspace: {ws: theWorkspace, done: make(chan struct{}), owed: tc.owed, lease: "lease-1"},
			}

			// Act.
			h.o.Drain(context.Background())

			// Assert.
			record, found := recordWith(h, "warn", "daemon.merge.drain")
			if !found {
				t.Fatal("the expired bound wrote no WARN record")
			}
			if record.Context["workspace"] != string(theWorkspace) {
				t.Fatalf("the WARN names workspace %v, want %q", record.Context["workspace"], theWorkspace)
			}
			if record.Context["unstamped"] != tc.wantUnstamped {
				t.Fatalf("the WARN names %v as unstamped, want %q", record.Context["unstamped"], tc.wantUnstamped)
			}
		})
	}
}

// TestADrainingOrchestratorAdmitsNothing covers the drain's other half: the
// bound cannot be outrun by a merge admitted inside it.
func TestADrainingOrchestratorAdmitsNothing(t *testing.T) {
	// Arrange: a queued merge under a draining orchestrator.
	h := newHarness(t)
	enqueue(t, h)
	h.o.mu.Lock()
	h.o.draining = true
	h.o.mu.Unlock()

	// Act.
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert.
	if err != nil {
		t.Fatalf("the pump errored while draining: %v", err)
	}
	if ran {
		t.Fatal("the pump admitted a merge while draining, want nothing admitted")
	}
}

// recordsAtLevel is every captured record at one level, which is how the
// drain's tests assert the ABSENCE of the failed-transaction records the
// defect produced.
func recordsAtLevel(h *harness, level string) []dlog.Record {
	var out []dlog.Record
	for _, record := range h.logs.Global().(*dlog.TestLogger).Records() {
		if record.Level == level {
			out = append(out, record)
		}
	}
	return out
}

// recordWith finds the first captured record at one level and operation.
func recordWith(h *harness, level, operation string) (dlog.Record, bool) {
	for _, record := range h.logs.Global().(*dlog.TestLogger).Records() {
		if record.Level == level && record.Operation == operation {
			return record, true
		}
	}
	return dlog.Record{}, false
}

// TestAMergeEndingAfterTheDrainWritesNothingToTheClosedStore is the OTHER side
// of the drain's window. A merge left mid-phase does not stop when the drain
// announces it: it stops later, when the orderly exit takes away the git and
// the shim its phase was using — by which time the state client is closed and
// nothing is waiting. Its give-back used to run anyway, and every write in it
// failed against the closed handle.
func TestAMergeEndingAfterTheDrainWritesNothingToTheClosedStore(t *testing.T) {
	// Arrange: a merge held inside its test gate, so the drain finds it
	// mid-phase and waits for nothing.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	inGate, release := make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate
	h.o.Drain(context.Background())
	// The orderly exit closes the state client the moment the drain returns.
	h.db.mu.Lock()
	h.db.shut = true
	h.db.mu.Unlock()

	// Act: the phase finishes after all of that.
	close(release)
	<-done

	// Assert: nothing was written, so nothing failed.
	if failures := recordsAtLevel(h, "error"); len(failures) > 0 {
		t.Fatalf("the merge that ended after the drain produced %d error records, want none: %v", len(failures), failures)
	}
}

// TestAMergeEndingAfterTheDrainNamesWhatItLeftToTheRecovery is the record that
// replaces those failures: the give-back is not silent, it is stated once, at
// INFO, naming the durable work the next boot's recovery owns.
func TestAMergeEndingAfterTheDrainNamesWhatItLeftToTheRecovery(t *testing.T) {
	// Arrange: as above — a merge held mid-gate across the drain.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	inGate, release := make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate
	h.o.Drain(context.Background())
	h.db.mu.Lock()
	h.db.shut = true
	h.db.mu.Unlock()

	// Act.
	close(release)
	<-done

	// Assert.
	record, found := recordWith(h, "info", "daemon.merge.teardown")
	if !found {
		t.Fatal("the merge that ended after the drain wrote no INFO teardown record")
	}
	if record.Context["unstamped"] != terminalOwedLanded {
		t.Fatalf("the teardown record names %v as unstamped, want %q", record.Context["unstamped"], terminalOwedLanded)
	}
}

// TestAMergeEndingAfterTheDrainReleasesTheRepositoryLock covers what the
// give-back must still do: everything that lives in THIS process is handed
// back, so a successor's recovery is the only thing left to redo.
func TestAMergeEndingAfterTheDrainReleasesTheRepositoryLock(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	inGate, release := make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate
	h.o.Drain(context.Background())
	h.db.mu.Lock()
	h.db.shut = true
	h.db.mu.Unlock()

	// Act.
	close(release)
	<-done

	// Assert: the run is off the orchestrator's books, so nothing thinks the
	// repository is still busy.
	h.o.mu.Lock()
	running := h.o.running[h.repoKey()]
	h.o.mu.Unlock()
	if running != nil {
		t.Fatal("the run is still registered as running after it ended past the drain")
	}
}

// TestAMergeAbortingAfterTheDrainIsNotRecordedAsAFailure covers the cause: a
// phase that broke because the daemon took its git away did not FAIL, and
// recording an abort there claimed a fault the next boot contradicts.
func TestAMergeAbortingAfterTheDrainIsNotRecordedAsAFailure(t *testing.T) {
	// Arrange: a merge held mid-gate whose gate then errors outright, which
	// is the abort path rather than the failed-verdict path.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.runner.runs = append(h.runner.runs, scriptedRun{Err: errStateClientClosed})
	inGate, release := make(chan struct{}), make(chan struct{})
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate
	h.o.Drain(context.Background())
	h.db.mu.Lock()
	h.db.shut = true
	h.db.mu.Unlock()

	// Act.
	close(release)
	<-done

	// Assert: no abort record, and the stop record names the daemon's exit.
	if _, found := recordWith(h, "error", "daemon.merge.abort"); found {
		t.Fatal("a merge that ended because the daemon exited was recorded as an abort, want no fault")
	}
	if _, found := recordWith(h, "info", "daemon.merge.stop"); !found {
		t.Fatal("the merge that ended when the daemon exited wrote no INFO stop record")
	}
}

// TestTheDrainWaitsForAnAdmissionStepInFlight pins the check-then-act the
// drain used to lose: the pump read `draining` as false, the drain began and
// the exit closed the state client, and the pump's next store read hit the
// closed handle (`daemon.wsm.merge_queue: refused the read ... sql: database
// is closed`). A step that began before the drain now holds it.
func TestTheDrainWaitsForAnAdmissionStepInFlight(t *testing.T) {
	// Arrange: an admission step held inside its first store read.
	h := newHarness(t)
	entered, release := make(chan struct{}), make(chan struct{})
	h.db.onPausedRead = func() {
		close(entered)
		<-release
	}
	done := admitAsync(h, context.Background())
	<-entered

	// Act
	drained := make(chan struct{})
	go func() {
		h.o.Drain(context.Background())
		close(drained)
	}()

	// Assert: the drain holds while the step reads, and returns once it left.
	select {
	case <-drained:
		t.Fatal("the drain returned while an admission step was still reading the store")
	case <-time.After(50 * time.Millisecond):
	}
	close(release)
	<-drained
	if err := <-done; err != nil {
		t.Fatalf("the admission step failed: %v", err)
	}
}

// TestAnAdmissionStepOutlivingTheDrainsBoundIsAnError pins the loud half: the
// drain is bounded, and a step still reading when the bound expires is about
// to read a closed store, which is said at ERROR.
func TestAnAdmissionStepOutlivingTheDrainsBoundIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.o.drainBound = 20 * time.Millisecond
	entered, release := make(chan struct{}), make(chan struct{})
	h.db.onPausedRead = func() {
		close(entered)
		<-release
	}
	done := admitAsync(h, context.Background())
	<-entered

	// Act
	h.o.Drain(context.Background())
	close(release)
	<-done

	// Assert
	if _, found := recordWith(h, "error", "daemon.merge.drain"); !found {
		t.Fatal("the drain gave up on an admission step in flight without an ERROR record")
	}
}

// TestNoAdmissionStepBeginsOnceTheDrainHasBegun pins the other half: the
// registration and the drain's flag are one lock, so a step that starts after
// the drain reads nothing at all.
func TestNoAdmissionStepBeginsOnceTheDrainHasBegun(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	h.o.Drain(context.Background())
	h.db.mu.Lock()
	before := h.db.pausedReads
	h.db.mu.Unlock()

	// Act
	ran, err := h.o.pumpOnce(context.Background(), h.repoKey())

	// Assert
	h.db.mu.Lock()
	after := h.db.pausedReads
	h.db.mu.Unlock()
	if ran || err != nil || after != before {
		t.Fatalf("pumpOnce after the drain = (%v, %v) with %d store reads, want nothing admitted and nothing read", ran, err, after-before)
	}
}

// --- dequeue, evict, workspace close and resume each release everything ---
//
// Before this a dequeue of a RUNNING merge only dropped a queue row that the
// run then held on to: the lease (parked), the queue entry and the repository
// lock all outlived it (2026-09-28, lease c8a3a664006f46c1).

// resolution is one way a running merge is ended from outside, or resumed.
type resolution struct {
	name    string
	resolve func(t *testing.T, h *harness, ctx context.Context)
}

// resolutions are the ends every release test runs.
var resolutions = []resolution{
	{name: "evicted", resolve: func(t *testing.T, h *harness, ctx context.Context) {
		if err := h.o.Evict(ctx, theWorkspace); err != nil {
			t.Errorf("Evict: %v", err)
		}
	}},
	{name: "dequeued", resolve: func(t *testing.T, h *harness, ctx context.Context) {
		h.o.OnInterrupt(ctx, theWorkspace)
		if err := h.o.AnswerDequeue(ctx, theWorkspace, false); err != nil {
			t.Errorf("AnswerDequeue: %v", err)
		}
	}},
	{name: "its workspace closed", resolve: func(t *testing.T, h *harness, ctx context.Context) {
		h.o.OnWorkspaceClosed(ctx, theWorkspace)
	}},
}

// ended runs one resolution against a merge in one of its running phases: a
// PARKED merge (on a broken gate), a RUNNING one (held inside its gate), or a
// parked one RESUMED to its landing.
func ended(t *testing.T, phase string, res resolution) *harness {
	t.Helper()
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	switch phase {
	case phaseParked:
		parkOnABrokenGate(t, h, ctx, 1)
		res.resolve(t, h, ctx)
	case phaseRunning:
		h.emacsRepo()
		h.landsCleanly("abc123def4567")
		h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
		inGate, release := holdTheGate(h)
		enqueue(t, h)
		done := admitAsync(h, ctx)
		<-inGate
		r, running := h.o.runFor(theWorkspace)
		if !running {
			t.Fatal("no run is held inside its gate")
		}
		// The resolution blocks until the run has ended, which it cannot
		// while its gate is held; the gate is released only once the
		// resolution has reached the run (its context carries the cause).
		resolved := make(chan struct{})
		go func() {
			defer close(resolved)
			res.resolve(t, h, context.Background())
		}()
		select {
		case <-r.ctx.Done():
		case <-time.After(5 * time.Second):
			t.Fatal("the resolution never reached the running merge")
		}
		close(release)
		<-resolved
		<-done
	case "resumed":
		parkOnABrokenGate(t, h, ctx, 1)
		h.gatePasses("daemon")
		if err := route(h, ctx, "g-1", "go again"); err != nil {
			t.Fatalf("the guidance: %v", err)
		}
		resume(t, h, ctx)
	}
	return h
}

// releaseCases are every (phase, resolution) pair, plus the resume.
func releaseCases() []struct {
	name  string
	phase string
	res   resolution
} {
	var cases []struct {
		name  string
		phase string
		res   resolution
	}
	for _, phase := range []string{phaseParked, phaseRunning} {
		for _, res := range resolutions {
			cases = append(cases, struct {
				name  string
				phase string
				res   resolution
			}{name: phase + " and " + res.name, phase: phase, res: res})
		}
	}
	cases = append(cases, struct {
		name  string
		phase string
		res   resolution
	}{name: "parked, resumed and landed", phase: "resumed"})
	return cases
}

func TestEveryEndOfARunningMergeReleasesItsLease(t *testing.T) {
	for _, tc := range releaseCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, tc.phase, tc.res)

			// Assert.
			if h.leaseHeld() {
				t.Fatal("the merge's lease outlived it")
			}
		})
	}
}

func TestEveryEndOfARunningMergeTakesItOffTheQueue(t *testing.T) {
	for _, tc := range releaseCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, tc.phase, tc.res)

			// Assert.
			if entries := h.queuedEntries(); len(entries) != 0 {
				t.Fatalf("the queue still holds %v", entries)
			}
		})
	}
}

func TestEveryEndOfARunningMergeClosesItsLedger(t *testing.T) {
	for _, tc := range releaseCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, tc.phase, tc.res)

			// Assert.
			if open := h.openIntervals(); len(open) != 0 {
				t.Fatalf("the ledger still has open intervals %v", open)
			}
		})
	}
}

func TestEveryEndOfARunningMergeReleasesTheRepositoryLock(t *testing.T) {
	for _, tc := range releaseCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, tc.phase, tc.res)

			// Assert.
			if !h.lockFree(t) {
				t.Fatal("the repository's queue lock outlived the merge")
			}
		})
	}
}

func TestEveryEndOfARunningMergeLeavesNoRunBehind(t *testing.T) {
	for _, tc := range releaseCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, tc.phase, tc.res)

			// Assert.
			if _, running := h.o.runFor(theWorkspace); running {
				t.Fatal("the orchestrator still holds the run")
			}
		})
	}
}

// TestAnAbandonedRunningMergeNeverLands covers the landing's guard: a merge
// taken out while its gate ran never moves the target, even though the gate
// then passed.
func TestAnAbandonedRunningMergeNeverLands(t *testing.T) {
	// Arrange and Act: evicted while held inside a gate that then passes.
	h := ended(t, phaseRunning, resolutions[0])

	// Assert.
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if len(h.git.fastForwards) != 0 {
		t.Fatalf("an abandoned merge fast-forwarded the target: %v", h.git.fastForwards)
	}
}

// TestAnAbandonedRunningMergeEndsOnTheAbandonedTerminal covers the bubble: the
// merge ends as abandoned, never as failed and never mid-tab.
func TestAnAbandonedRunningMergeEndsOnTheAbandonedTerminal(t *testing.T) {
	for _, phase := range []string{phaseParked, phaseRunning} {
		t.Run(phase, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, phase, resolutions[1])

			// Assert.
			if arm := h.feed.lastMergeErrorArm(); arm != "abandoned" {
				t.Fatalf("the bubble ended on %q, want abandoned", arm)
			}
		})
	}
}

// TestAnAbandonedMergesSummarySaysWhatItWasDoing covers "summaries must be
// true": a merge that ran is never described as one that waited.
func TestAnAbandonedMergesSummarySaysWhatItWasDoing(t *testing.T) {
	tests := []struct {
		phase string
		want  string
	}{
		{phase: phaseParked, want: "the user took this merge out of the queue while it was parked for input"},
		{phase: phaseRunning, want: "the user took this merge out of the queue while it was running"},
	}
	for _, tc := range tests {
		t.Run(tc.phase, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, tc.phase, resolutions[1])

			// Assert.
			if got := h.feed.lastAbandonedSummary(); got != tc.want {
				t.Fatalf("the abandoned summary is %q, want %q", got, tc.want)
			}
		})
	}
}

// TestAnAbandonedRunningMergeIsNeverLoggedAsNotHavingRun pins the record that
// was false: "a merge left the queue without running".
func TestAnAbandonedRunningMergeIsNeverLoggedAsNotHavingRun(t *testing.T) {
	// Arrange and Act.
	h := ended(t, phaseParked, resolutions[0])

	// Assert.
	for _, record := range h.logs.Records() {
		if record.Message == "a merge left the queue without running" {
			t.Fatalf("a merge that ran was logged as leaving the queue without running: %+v", record)
		}
	}
}

// TestEveryAbandonCauseOfARunningMergeResolvesItsOwnSentence pins the cause
// vocabulary for a merge taken out after it was admitted.
func TestEveryAbandonCauseOfARunningMergeResolvesItsOwnSentence(t *testing.T) {
	for _, phase := range []string{phaseRunning, phaseParked} {
		for _, cause := range []AbandonCause{CauseUserDrop, CauseUserDequeue, CauseWorkspaceClosed} {
			t.Run(phase+"/"+string(cause), func(t *testing.T) {
				// Act.
				sentence, declared := cause.summaryWhile(phase)

				// Assert.
				if !declared || !strings.Contains(sentence, "while") {
					t.Fatalf("the %s cause while %s resolves %q (declared %v)", cause, phase, sentence, declared)
				}
			})
		}
	}
}

// TestRouteParkedAnswersNoRunOnceTheRunEnded covers a prompt racing an
// abandon: it is answered, never left waiting on a run that is gone.
func TestRouteParkedAnswersNoRunOnceTheRunEnded(t *testing.T) {
	// Arrange: a parked merge that was evicted.
	h := ended(t, phaseParked, resolutions[0])

	// Act.
	err := h.o.RouteParked(context.Background(), theWorkspace, "g-late", saidText("too late"))

	// Assert.
	if err != errNoRun {
		t.Fatalf("RouteParked after the run ended = %v, want the no-run answer", err)
	}
}
