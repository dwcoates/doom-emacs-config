package merge

import (
	"context"
	"errors"
	"os"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
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
	record, found := recordWith(h, "info", "daemon.merge.teardown")
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
	if closure := h.db.closures[theWorkspace]; closure.By != wsm.CloseByMerge || closure.At.IsZero() {
		t.Fatalf("close record = %+v, want the merge's with its instant", closure)
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

// TestSelfReloadSkipsAHeldRollout covers the hold: the merge lands into the
// daemon's own checkout, and the deploy is not told.
func TestSelfReloadSkipsAHeldRollout(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ownCheckout()
	h.db.rolloutHold = wsm.RolloutHold{Held: true, Since: time.Unix(1, 0)}
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
	fired := h.rollout.fired
	h.rollout.mu.Unlock()
	if fired != 0 {
		t.Fatalf("the self-reload fired %d times while the rollouts are held, want none", fired)
	}
	record, ok := h.recordFor("info", "daemon.merge.self_reload")
	if !ok || record.Message != "a merge landed in this daemon's own checkout while the rollouts are held; the deploy is not told" {
		t.Fatalf("self_reload INFO record = %+v (found %v), want the held landing named", record, ok)
	}
}

// TestSelfReloadSkipsAnUnreadableHold covers the read failure: the deploy is
// not told, because a deploy could replace the build a hold keeps.
func TestSelfReloadSkipsAnUnreadableHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ownCheckout()
	h.db.rolloutHoldErr = errors.New("state database unreadable")
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert.
	h.rollout.mu.Lock()
	defer h.rollout.mu.Unlock()
	if h.rollout.fired != 0 {
		t.Fatal("the self-reload fired although the rollout hold could not be read")
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

// TestInterruptRaisesTheQueueOfferInThePinnedHead covers the ruling that an
// interrupt never yanks a merge off the queue: it asks, in the merge's own
// pinned bubble.
func TestInterruptRaisesTheQueueOfferInThePinnedHead(t *testing.T) {
	// Arrange: a queued merge.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	offered := h.o.OnInterrupt(context.Background(), theWorkspace)

	// Assert.
	if !offered {
		t.Fatal("OnInterrupt answered false for a queued sync merge")
	}
	head := h.feed.lastHead()
	if head == nil || !head.Pinned {
		t.Fatalf("the last head push is %+v, want a pinned head", head)
	}
	if head.Row.GetActivity().GetMerge().GetQueueOffer().GetHeadline().GetText() == "" {
		t.Fatalf("the pinned head carries no question: %v", head.Row)
	}
}

// TestInterruptLeavesTheQueueAsItIs covers the other half of that ruling: the
// interrupt itself changes nothing on the queue.
func TestInterruptLeavesTheQueueAsItIs(t *testing.T) {
	// Arrange: a queued merge.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Assert.
	if got := h.queuedEntries(); !equal(got, []string{string(theWorkspace)}) {
		t.Fatalf("the queue holds %v, want the merge still queued", got)
	}
}

// TestInterruptRaisesNothingWithoutAQueuedMerge covers the guard: an interrupt
// of an ordinary workspace poses no question and draws nothing.
func TestInterruptRaisesNothingWithoutAQueuedMerge(t *testing.T) {
	// Arrange: a workspace with no queued merge.
	h := newHarness(t)

	// Act.
	offered := h.o.OnInterrupt(context.Background(), theWorkspace)

	// Assert.
	if offered || h.feed.lastHead() != nil {
		t.Fatalf("an interrupt with no merge answered %v and drew %+v", offered, h.feed.lastHead())
	}
}

// TestInterruptRaisesNothingForAnAsyncMerge covers the in-place rule: an async
// merge's bubble is placed in its requester's feed, never pinned, so the
// question has nowhere to stand.
func TestInterruptRaisesNothingForAnAsyncMerge(t *testing.T) {
	// Arrange: the queued workspace is a merge workspace forked from another.
	h := newHarness(t)
	h.db.mu.Lock()
	record := h.db.workspaces[theWorkspace]
	record.MergeOf = &wsm.MergeOrigin{Requester: otherWorkspace}
	h.db.workspaces[theWorkspace] = record
	h.db.mu.Unlock()
	enqueue(t, h)
	before := h.feed.lastHead()

	// Act.
	offered := h.o.OnInterrupt(context.Background(), theWorkspace)

	// Assert.
	if offered {
		t.Fatal("OnInterrupt raised the question for an async merge")
	}
	if after := h.feed.lastHead(); after != nil && (before == nil || after.At != before.At) {
		t.Fatalf("an async merge's head was redrawn: %+v", after)
	}
}

// TestAnswerQueueOfferCancelLeavesTheQueue covers the cancel answer: the merge
// stays exactly where it was and the question leaves the bubble.
func TestAnswerQueueOfferCancelLeavesTheQueue(t *testing.T) {
	// Arrange: a queued merge with the question standing.
	h := newHarness(t)
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Act.
	err := h.o.AnswerQueueOffer(context.Background(), theWorkspace, QueueOfferCancel)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQueueOffer failed: %v", err)
	}
	if got := h.queuedEntries(); !equal(got, []string{string(theWorkspace)}) {
		t.Fatalf("the queue holds %v, want the merge still queued", got)
	}
	head := h.feed.lastHead()
	if head == nil || !head.Pinned || head.Row.GetActivity().GetMerge().GetQueueOffer() != nil {
		t.Fatalf("the last head push is %+v, want the pinned head without the question", head)
	}
}

// TestAnswerQueueOfferEvictTakesTheMergeOff covers the evict answer, which is a
// DIFFERENT end from an operator's eviction and records its own cause.
func TestAnswerQueueOfferEvictTakesTheMergeOff(t *testing.T) {
	// Arrange: a queued merge with the question standing.
	h := newHarness(t)
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Act.
	err := h.o.AnswerQueueOffer(context.Background(), theWorkspace, QueueOfferEvict)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQueueOffer failed: %v", err)
	}
	if got := h.queuedEntries(); len(got) != 0 {
		t.Fatalf("the queue still holds %v", got)
	}
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if len(h.db.dropped) != 1 || h.db.dropped[0] != string(theWorkspace)+":dequeued" {
		t.Fatalf("the drop was recorded as %v, want the dequeued cause", h.db.dropped)
	}
}

// TestAnswerQueueOfferEvictsAbandonedTerminalCarriesTheUsersCause covers the
// bubble's end: the summary says the user released the slot rather than
// reading like the operator's eviction.
func TestAnswerQueueOfferEvictsAbandonedTerminalCarriesTheUsersCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Act.
	if err := h.o.AnswerQueueOffer(context.Background(), theWorkspace, QueueOfferEvict); err != nil {
		t.Fatalf("AnswerQueueOffer failed: %v", err)
	}

	// Assert.
	if got := h.feed.lastAbandonedSummary(); got != "the user released this merge's queue slot" {
		t.Fatalf("the evicted merge's summary = %q, want the user's cause", got)
	}
}

// TestAnswerQueueOfferPrioritizeRunsTheMergeNext covers the prioritize answer:
// the merge moves ahead of every merge waiting, every bubble on the queue is
// told its new place, and the question leaves the bubble.
func TestAnswerQueueOfferPrioritizeRunsTheMergeNext(t *testing.T) {
	// Arrange: three merges wait, the asking one last.
	h := newHarness(t)
	queueAnother(t, h, "ws-2", "two")
	queueAnother(t, h, "ws-3", "three")
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Act.
	err := h.o.AnswerQueueOffer(context.Background(), theWorkspace, QueueOfferPrioritize)

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQueueOffer failed: %v", err)
	}
	if got, want := h.queuedEntries(), []string{string(theWorkspace), "ws-2", "ws-3"}; !equal(got, want) {
		t.Fatalf("the queue reads %v, want %v", got, want)
	}
	for ws, want := range map[wsm.WorkspaceID]int{theWorkspace: 1, "ws-2": 2, "ws-3": 3} {
		if got := h.factsOf(ws).QueuePlace; got != want {
			t.Fatalf("%s's place = %d, want %d", ws, got, want)
		}
	}
	h.o.mu.Lock()
	standing := h.o.offers[theWorkspace]
	h.o.mu.Unlock()
	if standing {
		t.Fatal("the answered question is still standing")
	}
}

// TestAnswerQueueOfferPrioritizeFailureIsRecordedAndKeepsTheQuestion covers the
// store failing the move: the error reaches the caller, is recorded at ERROR
// with its context, and the question stands so the reader can answer again.
func TestAnswerQueueOfferPrioritizeFailureIsRecordedAndKeepsTheQuestion(t *testing.T) {
	// Arrange: two merges wait, and the store fails every prioritize.
	h := newHarness(t)
	queueAnother(t, h, "ws-2", "two")
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)
	h.db.prioritizeErr = errors.New("database is locked")

	// Act.
	err := h.o.AnswerQueueOffer(context.Background(), theWorkspace, QueueOfferPrioritize)

	// Assert.
	if err == nil {
		t.Fatal("AnswerQueueOffer succeeded over a failing store")
	}
	record, logged := h.recordFor("error", "daemon.merge.prioritize")
	if !logged || record.Context["error"] != "database is locked" || record.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("the failure was not recorded at ERROR with its context: %+v", h.logs.Records())
	}
	if _, twice := h.recordFor("error", "daemon.merge.answer_queue_offer"); twice {
		t.Fatalf("the failure was recorded again by the answer: %+v", h.logs.Records())
	}
	if got, want := h.queuedEntries(), []string{"ws-2", string(theWorkspace)}; !equal(got, want) {
		t.Fatalf("the queue reads %v, want %v", got, want)
	}
	h.o.mu.Lock()
	standing := h.o.offers[theWorkspace]
	h.o.mu.Unlock()
	if !standing {
		t.Fatal("the question was retired although its answer failed")
	}
}

// TestAnswerQueueOfferRefusesWithNoOfferStanding covers the refusal an answer
// gets when the question it answers is gone, for every answer.
func TestAnswerQueueOfferRefusesWithNoOfferStanding(t *testing.T) {
	for _, answer := range []QueueOfferAnswer{QueueOfferEvict, QueueOfferPrioritize, QueueOfferCancel} {
		t.Run(answer.String(), func(t *testing.T) {
			// Arrange: a queued merge with no question raised.
			h := newHarness(t)
			enqueue(t, h)

			// Act.
			err := h.o.AnswerQueueOffer(context.Background(), theWorkspace, answer)

			// Assert.
			refusal, refused := Refused(err)
			if !refused || refusal.Arm != ArmNoOfferStanding {
				t.Fatalf("AnswerQueueOffer answered %v, want the %s refusal", err, ArmNoOfferStanding)
			}
			if got := h.queuedEntries(); !equal(got, []string{string(theWorkspace)}) {
				t.Fatalf("the queue holds %v, want it untouched", got)
			}
		})
	}
}

// TestTheMergesEndRetiresTheQuestion covers the question's other exit: a merge
// that leaves the queue on its own takes its question with it.
func TestTheMergesEndRetiresTheQuestion(t *testing.T) {
	// Arrange: a queued merge with the question standing.
	h := newHarness(t)
	enqueue(t, h)
	h.o.OnInterrupt(context.Background(), theWorkspace)

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	err := h.o.AnswerQueueOffer(context.Background(), theWorkspace, QueueOfferCancel)
	if refusal, refused := Refused(err); !refused || refusal.Arm != ArmNoOfferStanding {
		t.Fatalf("an answer after the merge ended answered %v, want the %s refusal", err, ArmNoOfferStanding)
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

// TestTheDrainCutsAMergesTestGateAndSaysItRunsAgain covers the one long
// phase the exit does not wait out: the test gate is cut, and the record says
// the resumed merge runs it again from its start.
func TestTheDrainCutsAMergesTestGateAndSaysItRunsAgain(t *testing.T) {
	// Arrange: a merge held inside its test gate, which is a long phase.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	inGate, release := make(chan struct{}), make(chan struct{})
	t.Cleanup(func() { close(release) })
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	h.o.Drain(context.Background())
	<-done

	// Assert: one INFO naming the cut gate, and no error record at all.
	record, found := recordWith(h, "info", "daemon.merge.drain")
	if !found || !strings.Contains(record.Message, "runs it again from its start") {
		t.Fatalf("the drain's record is %+v (found %v), want the test gate's cut", record, found)
	}
	if record.Context["tab"] != TabTests || record.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("the drain's record names %v / %v, want %q / %q", record.Context["tab"], record.Context["workspace"], TabTests, theWorkspace)
	}
	if failures := recordsAtLevel(h, "error"); len(failures) > 0 {
		t.Fatalf("the suspended merge produced %d error records, want none: %v", len(failures), failures)
	}
}

// TestASuspendedMergeKeepsItsLeaseQueueEntryAndRecord covers what the drain
// leaves for the next boot: the merge is suspended, not ended, so everything
// the resume needs stays where it is.
func TestASuspendedMergeKeepsItsLeaseQueueEntryAndRecord(t *testing.T) {
	// Arrange: a merge held inside its test gate.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.gatePasses("daemon")
	inGate, release := make(chan struct{}), make(chan struct{})
	t.Cleanup(func() { close(release) })
	h.runner.before = func() {
		close(inGate)
		<-release
	}
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-inGate

	// Act.
	h.o.Drain(context.Background())
	<-done

	// Assert.
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); !held {
		t.Fatal("the suspended merge released its lease, want it left for the resume")
	}
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || entries[0].State != wsm.MergeAdmitted {
		t.Fatalf("queue = %+v, want the merge still admitted", entries)
	}
	if step := recordedStep(t, h); step != TabTests {
		t.Fatalf("the progress record stands at %q, want %q", step, TabTests)
	}
}

// heldAtTheLanding runs a clean landing up to its fast-forward and holds that
// git command in flight, answering its release.
func heldAtTheLanding(t *testing.T, h *harness) (chan struct{}, <-chan error) {
	t.Helper()
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.gatePasses("daemon")
	entered, release := h.git.hold("fast_forward")
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-entered
	return release, done
}

// TestTheDrainWaitsForTheMergesGitCommandInFlight covers the stopping point:
// the exit never kills a merge's git halfway, it lets the command end.
func TestTheDrainWaitsForTheMergesGitCommandInFlight(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, done := heldAtTheLanding(t, h)
	h.o.onGitStopWait = func() { close(release) }

	// Act.
	h.o.Drain(context.Background())
	<-done

	// Assert.
	record, found := recordWith(h, "info", "daemon.merge.drain")
	if !found || record.Context["git"] != "FastForward" {
		t.Fatalf("the drain's record is %+v (found %v), want the FastForward it waited for", record, found)
	}
	if len(h.git.fastForwards) != 1 {
		t.Fatalf("fast-forwards = %v, want the one in flight finished", h.git.fastForwards)
	}
}

// TestTheDrainStartsNoGitAfterTheCommandInFlight covers the other half: once
// the gate stops, the run's next git is never started.
func TestTheDrainStartsNoGitAfterTheCommandInFlight(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, done := heldAtTheLanding(t, h)
	h.o.onGitStopWait = func() { close(release) }

	// Act.
	h.o.Drain(context.Background())
	<-done

	// Assert.
	if last := h.git.calls[len(h.git.calls)-1]; last != "fast_forward" {
		t.Fatalf("calls end %v, want nothing after the fast-forward in flight", h.git.calls)
	}
}

// TestTheDrainNamesAGitCommandThatOutlivesItsBound covers the bound: the exit
// goes on, and says which command it left running.
func TestTheDrainNamesAGitCommandThatOutlivesItsBound(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, done := heldAtTheLanding(t, h)
	h.o.gitBound = time.Millisecond
	h.o.drainBound = time.Millisecond

	// Act.
	h.o.Drain(context.Background())

	// Assert.
	close(release)
	<-done
	record, found := recordWith(h, "error", "daemon.merge.drain")
	if !found || record.Context["git"] != "FastForward" {
		t.Fatalf("the drain's error record is %+v (found %v), want the FastForward named", record, found)
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

// TestADrainWithNoAdmissionInFlightReportsNone covers the defect that made
// the expired-bound test flaky: with nothing in flight the drain knows so
// under its own lock, so an already-expired bound can never be read as an
// admission step outliving it.
func TestADrainWithNoAdmissionInFlightReportsNone(t *testing.T) {
	// Arrange: no admission and no terminal, under a bound that expires
	// before the drain could possibly look at it.
	h := newHarness(t)
	h.o.drainBound = time.Nanosecond

	// Act.
	h.o.Drain(context.Background())

	// Assert.
	if failures := recordsAtLevel(h, "error"); len(failures) > 0 {
		t.Fatalf("the drain with nothing in flight wrote %d error records, want none: %v", len(failures), failures)
	}
}

// TestADrainWaitsForTheAdmissionStepInFlight covers the other side: a step
// registered before the drain is waited for, so a drain cancelled while it is
// still running says so, naming how many steps it left.
func TestADrainWaitsForTheAdmissionStepInFlight(t *testing.T) {
	// Arrange: one admission step in flight, and a drain whose context is
	// already gone so its wait ends on the cancellation, not on a clock.
	h := newHarness(t)
	if !h.o.enterAdmission() {
		t.Fatal("enterAdmission refused before any drain")
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	h.o.Drain(ctx)

	// Assert.
	record, found := recordWith(h, "error", "daemon.merge.drain")
	if !found {
		t.Fatal("the drain did not report the admission step it was waiting for")
	}
	if record.Context["in_flight"] != 1 {
		t.Fatalf("the record names %v steps in flight, want 1", record.Context["in_flight"])
	}
	h.o.leaveAdmission()
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
	return dlog.FilterRecords(h.logs.Global().(*dlog.TestLogger).Records(), dlog.RecordQuery{Level: level})
}

// recordWith finds the first captured record at one level and operation.
func recordWith(h *harness, level, operation string) (dlog.Record, bool) {
	return dlog.FindRecord(h.logs.Global().(*dlog.TestLogger).Records(), dlog.RecordQuery{Level: level, Operation: operation})
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

// TestASuspendedMergeNamesTheStepTheBootResumesAt is the record that replaces
// those failures: the suspension is stated once, at INFO, naming the step the
// next boot resumes the merge at.
func TestASuspendedMergeNamesTheStepTheBootResumesAt(t *testing.T) {
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
	record, found := recordWith(h, "info", "daemon.merge.suspend")
	if !found {
		t.Fatal("the suspended merge wrote no INFO suspend record")
	}
	if record.Context["step"] != TabTests {
		t.Fatalf("the suspend record names step %v, want %q", record.Context["step"], TabTests)
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

	// Assert: no abort record, and the suspend record names the daemon's exit.
	if _, found := recordWith(h, "info", "daemon.merge.abort"); found {
		t.Fatal("a merge that ended because the daemon exited was recorded as an abort, want no fault")
	}
	if _, found := recordWith(h, "info", "daemon.merge.suspend"); !found {
		t.Fatal("the merge that ended when the daemon exited wrote no INFO suspend record")
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
		h.o.mu.Lock()
		h.o.offers[theWorkspace] = true
		h.o.mu.Unlock()
		if err := h.o.AnswerQueueOffer(ctx, theWorkspace, QueueOfferEvict); err != nil {
			t.Errorf("AnswerQueueOffer: %v", err)
		}
	}},
	{name: "its workspace closed", resolve: func(t *testing.T, h *harness, ctx context.Context) {
		h.o.OnWorkspaceClosed(ctx, theWorkspace)
	}},
}

// phaseRunning and phaseLanded are the two ends a release test drives: a
// RUNNING merge (held inside its gate) that a resolution ends, and a merge that
// runs to its landing.
const (
	phaseRunning = "running"
	phaseLanded  = "landed"
)

// ended runs one resolution against a merge held inside its gate, or lets the
// merge run to its landing.
func ended(t *testing.T, phase string, res resolution) *harness {
	t.Helper()
	return endedObserved(t, phase, res, nil)
}

// endedObserved is ended with observe, when set, run on the fresh harness
// before the merge is enqueued.
func endedObserved(t *testing.T, phase string, res resolution, observe func(*harness)) *harness {
	t.Helper()
	h := newHarness(t)
	if observe != nil {
		observe(h)
	}
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	switch phase {
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
	case phaseLanded:
		h.emacsRepo()
		h.landsCleanly("abc123def4567")
		h.gatePasses("daemon")
		enqueue(t, h)
		if err := h.admit(ctx); err != nil {
			t.Fatalf("admitting: %v", err)
		}
	}
	return h
}

// releaseCases are every resolution of a running merge, plus the landing.
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
	for _, res := range resolutions {
		cases = append(cases, struct {
			name  string
			phase string
			res   resolution
		}{name: phaseRunning + " and " + res.name, phase: phaseRunning, res: res})
	}
	cases = append(cases, struct {
		name  string
		phase string
		res   resolution
	}{name: "landed", phase: phaseLanded})
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

// --- a concluded merge's state retires once the workspace moves on --------

// TestAConcludedMergesStateRetiresWhenTheWorkspaceMovesOn covers the lingering
// state: a failed or landed merge's standing state is retired by the next
// submission of the workspace's own.
func TestAConcludedMergesStateRetiresWhenTheWorkspaceMovesOn(t *testing.T) {
	tests := []struct {
		name     string
		conclude func(h *harness)
	}{
		{name: "a failed merge", conclude: func(h *harness) {
			h.git.mergeErr = errors.New("refusing to merge unrelated histories")
		}},
		{name: "a landed merge", conclude: func(h *harness) {
			h.landsCleanly("abc123def4567")
			h.gatePasses("daemon")
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a merge that concluded.
			h := newHarness(t)
			h.emacsRepo()
			h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
			tc.conclude(h)
			enqueue(t, h)
			_ = h.admit(context.Background())

			// Act.
			h.o.RetireConcluded(context.Background(), theWorkspace)

			// Assert.
			if _, known := h.o.Facts(theWorkspace); known || h.footer.last().State != "none" {
				t.Fatalf("the concluded state still stands (footer %q)", h.footer.last().State)
			}
		})
	}
}

// TestAQueuedMergeIsNeverRetiredByTheWorkspaceMovingOn covers a merge waiting
// in the queue: it is not concluded either.
func TestAQueuedMergeIsNeverRetiredByTheWorkspaceMovingOn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	h.o.RetireConcluded(context.Background(), theWorkspace)

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateQueued {
		t.Fatalf("the queued merge's state is %q, want it still queued", facts.State)
	}
}

// landedAs runs the harness workspace's merge of one source to its landing.
func landedAs(t *testing.T, h *harness, source wsm.MergeSource) {
	t.Helper()
	if source.Kind == wsm.MergeSourceMergedUpstream {
		mergedUpstream(h)
	} else {
		landing(h, 1)
	}
	if source.Kind == wsm.MergeSourceBranch {
		h.git.refs["refs/heads/"+source.Branch] = "branch0000head"
		h.git.mu.Lock()
		h.git.branches[source.Branch] = source.Branch
		h.git.mu.Unlock()
	}
	if err := h.request(t, source, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}
}

func TestAnOwnBranchKeptOpenLandsWithoutClosingTheRequester(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	landedAs(t, h, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true})

	// Assert.
	if h.db.closed[theWorkspace] || len(h.git.removedWorktrees) != 0 || len(h.stoppedSessions) != 0 {
		t.Fatalf("closed %v, removed %v, stopped %v; want the requester kept open", h.db.closed, h.git.removedWorktrees, h.stoppedSessions)
	}
	if got := h.footer.last().State; got != StateMerged {
		t.Fatalf("state = %q, want merged", got)
	}
}

// TestAPromptHeldDuringTheMergeKeepsTheRequesterOpen covers the sources a
// landing closes the requester for: a prompt held under the merge, bound to
// its entry before the release, leaves the requester open and merged.
func TestAPromptHeldDuringTheMergeKeepsTheRequesterOpen(t *testing.T) {
	tests := []struct {
		name   string
		source wsm.MergeSource
	}{
		{"own branch", wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}},
		{"merged upstream", wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a prompt is held the instant before the release.
			h := newHarness(t)
			h.db.beforeRelease = func(f *fakeDB) { f.bindHeldPrompt(theWorkspace) }

			// Act.
			landedAs(t, h, tt.source)

			// Assert.
			if h.db.closed[theWorkspace] || len(h.stoppedSessions) != 0 {
				t.Fatalf("closed %v, stopped %v; want the requester kept open for its held prompt", h.db.closed, h.stoppedSessions)
			}
			if got := h.footer.last().State; got != StateMerged {
				t.Fatalf("state = %q, want merged", got)
			}
			if _, ok := h.recordFor("info", "daemon.merge.teardown"); !ok {
				t.Fatal("keeping the requester open was not recorded")
			}
		})
	}
}

// TestAnUnreadableKeepOpenKeepsTheRequesterOpenLoudly covers the read failing
// after the release: the requester is kept open, since closing cannot be
// undone, and the failure is an ERROR record.
func TestAnUnreadableKeepOpenKeepsTheRequesterOpenLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.beforeRelease = func(f *fakeDB) { f.queueReadErr = errors.New("disk I/O error") }

	// Act.
	landedAs(t, h, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch})

	// Assert.
	if h.db.closed[theWorkspace] || len(h.stoppedSessions) != 0 {
		t.Fatalf("closed %v, stopped %v; want the requester kept open", h.db.closed, h.stoppedSessions)
	}
	record, ok := h.recordFor("error", "daemon.merge.teardown")
	if !ok || record.Context["error"] != "disk I/O error" {
		t.Fatalf("the unreadable keep-open was not an ERROR naming its cause: %+v", record)
	}
}

// TestTheMergeLeaseHoldsSubmissions covers the lease the merge takes: it holds
// what the user submits rather than refusing it.
func TestTheMergeLeaseHoldsSubmissions(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	landedAs(t, h, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch})

	// Assert.
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if len(h.db.releasedLeases) != 1 {
		t.Fatalf("released leases = %v, want the merge's one", h.db.releasedLeases)
	}
	if got := h.db.policies[h.db.releasedLeases[0]]; got != wsm.PolicyHold {
		t.Fatalf("the merge lease's policy = %v, want hold", got)
	}
}

// TestTheMergeRestampsTheHoldsWhenItTakesItsLease covers the prompts held
// before the merge began: the queue is told the lease changed while the merge
// lease stands, so they wait for the merge and keep the requester open, and
// told again once it is released.
func TestTheMergeRestampsTheHoldsWhenItTakesItsLease(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	landedAs(t, h, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch})

	// Assert.
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	if want := []bool{true, false}; !slices.Equal(h.queue.leaseEvents, want) {
		t.Fatalf("lease events saw the merge lease %v, want %v", h.queue.leaseEvents, want)
	}
}

// teardownRecorded reports whether the teardown recorded MESSAGE at LEVEL.
func teardownRecorded(h *harness, level, message string) bool {
	for _, r := range h.logs.Records() {
		if r.Level == level && r.Operation == "daemon.merge.teardown" && r.Message == message {
			return true
		}
	}
	return false
}

// landAsync hands the harness workspace's async request, as RUN asks, to its
// merge workspace and lands it.
func landAsync(t *testing.T, h *harness, run *DetachedRun) {
	t.Helper()
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	if err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByUser, Detached: run}); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}
}

func TestAnAsyncOneShotLandingClosesTheWorkspaceItNames(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	dir := h.registerOther(otherWorkspace, "ws-two", "other-branch")

	// Act.
	landAsync(t, h, &DetachedRun{Name: "reconnect-fix", CloseOnLanding: otherWorkspace})

	// Assert: the named workspace is stamped merged and closed and its
	// worktree removed, as the merge workspace's own is; the requester stays.
	if !h.db.closed[otherWorkspace] || h.db.mergedAt[otherWorkspace].IsZero() {
		t.Fatal("the one-shot workspace was not stamped merged and closed")
	}
	if !slices.Contains(h.git.removedWorktrees, dir) {
		t.Fatalf("removed worktrees = %v, want the one-shot workspace's %s", h.git.removedWorktrees, dir)
	}
	if h.db.closed[theWorkspace] {
		t.Fatal("the requester was closed by its own async merge")
	}
}

func TestAnAsyncOneShotWhoseWorkspaceClosedMeanwhileClosesNothingMore(t *testing.T) {
	// Arrange: the named workspace is registered, then closed before landing.
	h := newHarness(t)
	dir := h.registerOther(otherWorkspace, "ws-two", "other-branch")
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	if err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByUser,
		Detached: &DetachedRun{Name: "reconnect-fix", CloseOnLanding: otherWorkspace}}); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.db.mu.Lock()
	w := h.db.workspaces[otherWorkspace]
	w.Closed = true
	h.db.workspaces[otherWorkspace] = w
	h.db.mu.Unlock()

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if slices.Contains(h.git.removedWorktrees, dir) {
		t.Fatalf("removed worktrees = %v, want the already-closed workspace's left alone", h.git.removedWorktrees)
	}
	if !teardownRecorded(h, "info", "the workspace an async merge's one-shot closes is already closed") {
		t.Fatal("the pass-over was not recorded")
	}
}

func TestAnAsyncCleanupRemovesItsRecordedWorktreeOnceItLands(t *testing.T) {
	// Arrange: the branch is checked out in a worktree no workspace owns.
	h := newHarness(t)
	h.git.mu.Lock()
	h.git.worktrees = append(h.git.worktrees, gitclient.Worktree{Dir: "/wt/agent-fix", Branch: asyncBranch})
	h.git.clean = true
	h.git.mu.Unlock()

	// Act.
	landAsync(t, h, &DetachedRun{Name: "reconnect-fix", CleanupWorktree: true})

	// Assert.
	if len(h.git.cleanedWorktrees) != 1 || h.git.cleanedWorktrees[0] != "/wt/agent-fix" {
		t.Fatalf("cleaned worktrees = %v, want the branch's /wt/agent-fix", h.git.cleanedWorktrees)
	}
}

func TestAnAsyncCleanupKeepsAWorktreeWithUncommittedChanges(t *testing.T) {
	// Arrange: the branch's worktree holds uncommitted changes.
	h := newHarness(t)
	h.git.mu.Lock()
	h.git.worktrees = append(h.git.worktrees, gitclient.Worktree{Dir: "/wt/agent-fix", Branch: asyncBranch})
	h.git.clean = true
	h.git.cleanByDir = map[string]bool{"/wt/agent-fix": false}
	h.git.mu.Unlock()

	// Act.
	landAsync(t, h, &DetachedRun{Name: "reconnect-fix", CleanupWorktree: true})

	// Assert.
	if len(h.git.cleanedWorktrees) != 0 {
		t.Fatalf("cleaned worktrees = %v, want the dirty one kept", h.git.cleanedWorktrees)
	}
	if !teardownRecorded(h, "info", "kept the worktree an async merge asked to clean up: it holds uncommitted changes") {
		t.Fatal("the kept worktree was not recorded")
	}
}

func TestAnAsyncCleanupThatGitRefusesIsLoggedAndTheMergeStillLands(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.mu.Lock()
	h.git.worktrees = append(h.git.worktrees, gitclient.Worktree{Dir: "/wt/agent-fix", Branch: asyncBranch})
	h.git.clean = true
	h.git.cleanRemoveErr = errors.New("locked")
	h.git.mu.Unlock()

	// Act.
	landAsync(t, h, &DetachedRun{Name: "reconnect-fix", CleanupWorktree: true})

	// Assert.
	if !teardownRecorded(h, "error", "could not remove the worktree an async merge asked to clean up") {
		t.Fatal("the refused cleanup was not recorded at ERROR")
	}
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].outcome.Kind != DetachedLanded {
		t.Fatalf("handbacks = %+v, want the landing handed back", sent)
	}
}

func TestAMadeBranchWorktreeIsRemovedAfterTheMerge(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	landedAs(t, h, wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix"})

	// Assert.
	made := worktreeFor(h.stateDir, h.db.ledger[theWorkspace][0].Lease)
	if len(h.git.removedWorktrees) != 1 || h.git.removedWorktrees[0] != made {
		t.Fatalf("removed worktrees = %v, want the made %s", h.git.removedWorktrees, made)
	}
	if h.db.closed[theWorkspace] {
		t.Fatal("a branch's merge closed the requester")
	}
}

func TestAMadeBranchWorktreeIsKeptWhenItsConflictResolutionGaveUp(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	h.git.conflicted = everyAttemptLeaves("a.go")

	// Act.
	landedAs(t, h, wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix"})

	// Assert.
	if len(h.git.removedWorktrees) != 0 {
		t.Fatalf("removed worktrees = %v, want the rebase left in progress where it stopped", h.git.removedWorktrees)
	}
}

func TestAFailedMergeLeavesTheQueueAtOnceForTheMergeBehindIt(t *testing.T) {
	// Arrange: the front's conflict resolution gives up.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	h.git.conflicted = everyAttemptLeaves("a.go")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}

	// Assert.
	if entries := h.queuedEntries(); len(entries) != 0 {
		t.Fatalf("the queue still holds %v", entries)
	}
	if !h.lockFree(t) {
		t.Fatal("the failed merge still holds its repository")
	}
}

func TestAFailedMergesStateRetiresAtTheWorkspacesNextSubmission(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.clean = false
	admitted(t, h)

	// Act.
	h.o.RetireConcluded(context.Background(), theWorkspace)

	// Assert.
	if _, standing := h.o.Facts(theWorkspace); standing {
		t.Fatal("the failed merge's state outlived the workspace moving on")
	}
}

// THE TERMINAL FOLLOWS THE RELEASE: whichever way a running merge ends, no
// client is told it ended while its lease still refuses the workspace's
// prompts.
func TestEveryEndOfARunningMergeIsPublishedOnlyAfterItsLeaseIsReleased(t *testing.T) {
	for _, tc := range releaseCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: record whether the lease was held as each terminal head
			// was pushed.
			var mu sync.Mutex
			var heldAtTerminal []bool
			observe := func(h *harness) {
				h.feed.onUpsert = func(row *frontendv1.FeedRow) {
					merge := row.GetActivity().GetMerge()
					if merge.GetSuccess() != nil || merge.GetError() != nil {
						held := h.leaseHeld()
						mu.Lock()
						heldAtTerminal = append(heldAtTerminal, held)
						mu.Unlock()
					}
				}
			}

			// Act
			endedObserved(t, tc.phase, tc.res, observe)

			// Assert
			mu.Lock()
			defer mu.Unlock()
			if len(heldAtTerminal) == 0 {
				t.Fatal("no terminal head was published")
			}
			for _, held := range heldAtTerminal {
				if held {
					t.Fatal("a terminal head was published while the merge's lease was still held")
				}
			}
		})
	}
}

func TestAFailedMergesTerminalIsPublishedOnlyAfterItsLeaseIsReleased(t *testing.T) {
	// Arrange: the conflict resolution gives up.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	h.git.conflicted = everyAttemptLeaves("a.go")
	heldAtFailure := []bool{}
	h.feed.onUpsert = func(row *frontendv1.FeedRow) {
		if row.GetActivity().GetMerge().GetError() != nil {
			heldAtFailure = append(heldAtFailure, h.leaseHeld())
		}
	}
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}

	// Assert
	if len(heldAtFailure) != 1 || heldAtFailure[0] {
		t.Fatalf("lease held as the failed terminal was pushed = %v, want one push after the release", heldAtFailure)
	}
}

func TestAbandonedSentenceReadsTheCausesSentenceForWhetherTheMergeRan(t *testing.T) {
	// Arrange.
	tests := []struct {
		name string
		ran  bool
		want string
	}{
		{name: "never ran", ran: false, want: abandonSummaries[CauseUserDrop]},
		{name: "ran", ran: true, want: runningSummaries[CauseUserDrop]},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := newHarness(t)

			// Act.
			got := h.o.abandonedSentence(theWorkspace, CauseUserDrop, tt.ran)

			// Assert.
			if got != tt.want {
				t.Fatalf("abandonedSentence = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAbandonedSentenceOfAnUndeclaredCauseNamesItAndRecordsTheGap(t *testing.T) {
	// Arrange.
	tests := []struct {
		name string
		ran  bool
		want string
	}{
		{name: "never ran", ran: false, want: "this merge left the queue before it ran (no-such-cause)"},
		{name: "ran", ran: true, want: "this merge was abandoned while it was running (no-such-cause)"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := newHarness(t)

			// Act.
			got := h.o.abandonedSentence(theWorkspace, AbandonCause("no-such-cause"), tt.ran)

			// Assert.
			if got != tt.want {
				t.Fatalf("abandonedSentence = %q, want %q", got, tt.want)
			}
			rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.abandoned"})
			if !ok || rec.Context["cause"] != "no-such-cause" || rec.Context["workspace"] != string(theWorkspace) {
				t.Fatalf("the undeclared cause was recorded as %+v, want an ERROR naming it and the workspace", rec)
			}
		})
	}
}

func TestEveryAbandonedSentenceIsReadThroughAbandonedSentence(t *testing.T) {
	// Arrange.
	for _, file := range []string{"terminal.go", "queue.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}

		// Act.
		uses := strings.Count(string(body), "cause.summary()") + strings.Count(string(body), "cause.summaryRunning()")

		// Assert: only abandonedSentence (terminal.go) reads the sentences.
		if want := map[string]int{"terminal.go": 2, "queue.go": 0}[file]; uses != want {
			t.Fatalf("%s reads a cause's sentence %d times, want %d: every reading goes through abandonedSentence", file, uses, want)
		}
	}
}
