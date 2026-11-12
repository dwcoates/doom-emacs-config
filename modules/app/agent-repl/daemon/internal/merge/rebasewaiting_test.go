package merge

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// waitingWorld is a queue of the head, the merge behind it (ws-b), which
// heads the line once the head lands, and the waiting members named queued
// behind ws-b; the members' rebases and the passes' ends are reported on
// channels. The head is held inside its gate until land.
type waitingWorld struct {
	*chain
	rebased     chan ids.WorkspaceID
	passes      chan wsm.RepoKey
	releaseHead chan struct{}
	done        <-chan error
}

func newWaitingWorld(t *testing.T, waiting ...ids.WorkspaceID) *waitingWorld {
	t.Helper()
	c := newChain(t)
	w := &waitingWorld{chain: c, rebased: make(chan ids.WorkspaceID, 16), passes: make(chan wsm.RepoKey, 16)}
	c.h.o.onWaitingRebased = func(ws ids.WorkspaceID) { w.rebased <- ws }
	c.h.o.onWaitingPass = func(repo wsm.RepoKey) { w.passes <- repo }
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	for i, ws := range waiting {
		c.queue(t, ws, "w"+string(rune('1'+i)))
	}
	headGate, release := c.runner.hold(c.dirs[theWorkspace])
	w.releaseHead, w.done = release, admitAsync(c.h, context.Background())
	<-headGate
	return w
}

// entryOf reads WS's queue entry.
func (w *waitingWorld) entryOf(t *testing.T, ws ids.WorkspaceID) wsm.MergeQueueEntry {
	t.Helper()
	stored, err := w.h.db.MergeQueue(context.Background(), w.h.repoKey())
	if err != nil {
		t.Fatal(err)
	}
	for _, entry := range stored {
		if entry.Workspace == ws {
			return entry
		}
	}
	t.Fatalf("%s has no queue entry", ws)
	return wsm.MergeQueueEntry{}
}

// waitGone waits, bounded, for every run the orchestrator started to end.
func waitGone(t *testing.T, o *orchestrator) {
	t.Helper()
	receiveWithin(t, liveGone(o), "the exiting orchestrator's runs ending")
}

// land lets the head's gate pass, while the merge behind it still waits in
// line for the test turn on the old tip, and waits for the head to land.
func (w *waitingWorld) land(t *testing.T) {
	t.Helper()
	close(w.releaseHead)
	if err := <-w.done; err != nil {
		t.Fatalf("the head ended on: %v", err)
	}
}

// awaitPass waits for one pass over the waiting members to end.
func (w *waitingWorld) awaitPass(t *testing.T) {
	t.Helper()
	receiveWithin(t, w.passes, "a pass over the waiting members ending")
}

// finish drains the queue.
func (w *waitingWorld) finish(t *testing.T) {
	t.Helper()
	w.drain(t)
}

func TestALandingRebasesEveryWaitingMergeOntoTheNewTip(t *testing.T) {
	// Arrange
	w := newWaitingWorld(t, "ws-c", "ws-d")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	for ws, want := range map[ids.WorkspaceID]string{"ws-c": "a1+base+w1", "ws-d": "a1+base+w2"} {
		if got := w.git.tree("b-" + string(ws)); got != want {
			t.Errorf("%s holds %s after the landing, want %s", ws, got, want)
		}
	}
	w.finish(t)
}

func TestAWaitingMergesRebaseIsCountedFromWhenItWasQueued(t *testing.T) {
	// Arrange
	w := newWaitingWorld(t, "ws-c")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	w.h.footer.mu.Lock()
	var told int
	for i, facts := range w.h.footer.facts {
		if w.h.footer.told[i] != "ws-c" || (facts.State != StateQueued && facts.State != StateMerging) {
			continue
		}
		told++
		if facts.QueuedAt.UnixMilli() <= 0 {
			t.Errorf("ws-c was told %s at step %s with no queue instant", facts.State, facts.Step)
		}
	}
	w.h.footer.mu.Unlock()
	if told == 0 {
		t.Fatal("ws-c's footer was told no merge in line or in flight")
	}
	w.finish(t)
}

func TestAWaitingMergeWithNoQueueInstantIsNotRebasedAndSaysSo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	entry := wsm.MergeQueueEntry{Workspace: "ws-c", Repo: h.repoKey(), State: wsm.MergeQueued}

	// Act
	h.o.rebaseWaitingMember(context.Background(), h.repoKey(), entry)

	// Assert
	rec, ok := h.recordFor("error", "daemon.merge.rebase_waiting")
	if !ok || rec.Context["workspace"] != "ws-c" || rec.Context["error"] == nil {
		t.Fatalf("records = %+v, want the waiting merge's missing queue instant at ERROR", h.logs.Records())
	}
	if h.footer.toldTo("ws-c") {
		t.Fatal("ws-c's footer was told a merge whose rebase never started")
	}
}

func TestWaitingMergesAreRebasedOneAtATimeInQueueOrder(t *testing.T) {
	// Arrange
	w := newWaitingWorld(t, "ws-c", "ws-d")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	first, second := <-w.rebased, <-w.rebased
	if first != "ws-c" || second != "ws-d" {
		t.Fatalf("rebased %s then %s, want ws-c then ws-d", first, second)
	}
	w.finish(t)
}

func TestAWaitingMergeRunsNoTests(t *testing.T) {
	// Arrange
	w := newWaitingWorld(t, "ws-c")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if got := w.runner.ran(w.dirs["ws-c"]); got != 0 {
		t.Fatalf("the waiting merge ran its gate %d times, want none", got)
	}
	w.finish(t)
}

func TestAWaitingMergesConflictStepsItAsideForTheResolvingSlot(t *testing.T) {
	// Arrange: C's commit conflicts with what the head lands.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if got := w.prompts("ws-c", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 0 {
		t.Fatalf("C's session got %d conflict prompts from its git-only rebase, want none", got)
	}
	entry := w.entryOf(t, "ws-c")
	if entry.State != wsm.MergeRemediating || entry.Aside != wsm.MergeAsideWaitingConflicts || entry.Slot != wsm.MergeSlotResolvingConflicts {
		t.Fatalf("C's entry = %+v, want it out of line holding the resolving-conflicts slot", entry)
	}
	if standing, _ := w.git.RebaseInProgress(context.Background(), w.dirs["ws-c"]); standing {
		t.Fatal("the conflicted rebase still stands in C's worktree")
	}
	if got := w.git.tree("b-ws-c"); got != "base+w1" {
		t.Fatalf("C holds %s, want its base kept (base+w1)", got)
	}
	w.h.db.mu.Lock()
	_, held := w.h.db.leases["ws-c"]
	w.h.db.mu.Unlock()
	if held {
		t.Fatal("C's lease outlived its git-only rebase")
	}
	if !w.recorded("info", "daemon.merge.rebase_waiting", "the merge holds the resolving-conflicts slot") {
		t.Fatal("the step aside was not recorded")
	}
}

func TestASecondWaitingConflictWaitsForTheResolvingSlot(t *testing.T) {
	// Arrange: C's and D's commits both conflict with what the head lands.
	w := newWaitingWorld(t, "ws-c", "ws-d")
	w.git.conflict("w1", "a1", "x.go")
	w.git.conflict("w2", "a1", "y.go")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if got := w.entryOf(t, "ws-c").Slot; got != wsm.MergeSlotResolvingConflicts {
		t.Fatalf("C = %s, want it holding the resolving-conflicts slot", got)
	}
	if got := w.entryOf(t, "ws-d").Slot; got != wsm.MergeSlotAwaitingResolvingConflicts {
		t.Fatalf("D = %s, want it awaiting the resolving-conflicts slot", got)
	}
	if !w.recorded("info", "daemon.merge.rebase_waiting", "waits out of line for the resolving-conflicts slot") {
		t.Fatal("the wait for the slot was not recorded")
	}
}

func TestTheResolvingSlotsHolderResolvesInItsSessionAndReturnsToTheLine(t *testing.T) {
	// Arrange: C stepped aside for the resolving-conflicts slot.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")
	w.land(t)
	w.awaitPass(t)

	// Act
	w.h.o.resolveWaiting(context.Background(), w.h.repoKey(), "ws-c")

	// Assert
	if got := w.prompts("ws-c", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 1 {
		t.Fatalf("C's session got %d conflict prompts in the slot, want 1", got)
	}
	stored, err := w.h.db.MergeQueue(context.Background(), w.h.repoKey())
	if err != nil {
		t.Fatal(err)
	}
	line := inLine(stored)
	if len(line) != 2 || line[0].Workspace != "ws-c" || line[0].State != wsm.MergeQueued || line[0].Aside != wsm.MergeAsideNone {
		t.Fatalf("queue = %+v, want C back in line waiting, at the first waiting place", stored)
	}
	if w.h.o.isRemediatingLocked("ws-c") {
		t.Fatal("C is still registered as remediating out of line")
	}
	w.h.db.mu.Lock()
	_, held := w.h.db.leases["ws-c"]
	w.h.db.mu.Unlock()
	if held {
		t.Fatal("C's lease outlived its resolving rebase")
	}
}

func TestABusyResolvingSlotHolderReturnsToTheLineAndSaysSo(t *testing.T) {
	// Arrange: C holds the resolving-conflicts slot; its workspace is busy.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")
	w.land(t)
	w.awaitPass(t)
	w.h.freeness.mu.Lock()
	w.h.freeness.busyWS = map[ids.WorkspaceID]bool{"ws-c": true}
	w.h.freeness.mu.Unlock()

	// Act
	w.h.o.resolveWaiting(context.Background(), w.h.repoKey(), "ws-c")

	// Assert
	if got := w.entryOf(t, "ws-c"); got.State != wsm.MergeQueued || got.Aside != wsm.MergeAsideNone {
		t.Fatalf("C = %+v, want it back in line waiting", got)
	}
	if !w.recorded("info", "daemon.merge.rebase_waiting", "has a busy workspace; it returns to the line") {
		t.Fatal("the return was not recorded")
	}
}

func TestAConflictedWaitingMergeWhoseStepAsideFailsKeepsItsPlaceAndSaysSo(t *testing.T) {
	// Arrange: C's commit conflicts; the store refuses its step aside.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")
	w.h.db.mu.Lock()
	w.h.db.stepAsideErr = errors.New("the store is unwell")
	w.h.db.mu.Unlock()

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if got := w.entryOf(t, "ws-c"); got.State != wsm.MergeQueued {
		t.Fatalf("C = %+v, want it still waiting in its place", got)
	}
	if !w.recorded("error", "daemon.merge.rebase_waiting", "could not step the conflicted waiting merge aside") {
		t.Fatal("the failed step aside was not recorded at ERROR")
	}
	if standing, _ := w.git.RebaseInProgress(context.Background(), w.dirs["ws-c"]); standing {
		t.Fatal("the conflicted rebase still stands in C's worktree")
	}
}

func TestABusyWaitingMergeIsSkippedAndSaysSo(t *testing.T) {
	// Arrange: C's workspace has a turn in flight.
	w := newWaitingWorld(t, "ws-c")
	w.h.freeness.mu.Lock()
	w.h.freeness.busyWS = map[ids.WorkspaceID]bool{"ws-c": true}
	w.h.freeness.mu.Unlock()

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if got := w.git.tree("b-ws-c"); got != "base+w1" {
		t.Fatalf("the busy merge holds %s, want its base kept (base+w1)", got)
	}
	if !w.recorded("info", "daemon.merge.rebase_waiting", "a waiting merge's workspace is busy") {
		t.Fatal("the skip was not recorded")
	}
	w.h.freeness.mu.Lock()
	w.h.freeness.busyWS = nil
	w.h.freeness.mu.Unlock()
	w.finish(t)
}

func TestAWaitingMergeHeldByAnotherLeaseIsSkippedAtInfo(t *testing.T) {
	// Arrange: a shim restart holds C's workspace.
	w := newWaitingWorld(t, "ws-c")
	restart, err := w.h.db.AcquireLease(context.Background(), "ws-c", wsm.HolderRestart, wsm.PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if got := w.git.tree("b-ws-c"); got != "base+w1" {
		t.Fatalf("the held merge holds %s, want its base kept (base+w1)", got)
	}
	if !w.recorded("info", "daemon.merge.rebase_waiting", "a waiting merge's workspace is held by another lease") {
		t.Fatal("the skip was not recorded at INFO")
	}
	if w.recorded("error", "daemon.merge.rebase_waiting", "") {
		t.Fatal("the skip was recorded at ERROR")
	}
	if err := w.h.db.ReleaseLease(context.Background(), restart.ID); err != nil {
		t.Fatalf("ReleaseLease: %v", err)
	}
	w.finish(t)
}

func TestAWaitingMergesRebaseKeepsItsPlaceInLine(t *testing.T) {
	// Arrange
	w := newWaitingWorld(t, "ws-c")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	stored, err := w.h.db.MergeQueue(context.Background(), w.h.repoKey())
	if err != nil {
		t.Fatal(err)
	}
	line := inLine(stored)
	if len(line) != 2 || line[1].Workspace != "ws-c" || line[1].State != wsm.MergeQueued {
		t.Fatalf("queue after the rebase = %+v, want ws-c still waiting second", line)
	}
	w.finish(t)
}

func TestTheDaemonsExitAbortsAResolvingRebaseAndKeepsItsBase(t *testing.T) {
	// Arrange: C holds the resolving-conflicts slot, and its session's
	// conflict turn is still running when the daemon exits.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")
	w.land(t)
	w.awaitPass(t)
	prompted := make(chan struct{})
	w.h.queue.mu.Lock()
	w.h.queue.onSubmit = func(sub promptqueue.Submission) {
		if sub.WS != "ws-c" {
			return
		}
		w.h.mu.Lock()
		w.h.awaitGate = make(chan struct{})
		w.h.mu.Unlock()
		close(prompted)
	}
	w.h.queue.mu.Unlock()
	resolved := make(chan struct{})
	go func() {
		defer close(resolved)
		w.h.o.resolveWaiting(context.Background(), w.h.repoKey(), "ws-c")
	}()
	<-prompted

	// Act
	w.h.o.Drain(context.Background())
	receiveWithin(t, resolved, "the resolving rebase ending at the exit")

	// Assert
	if !w.recorded("info", "daemon.merge.rebase_waiting", "the daemon's exit cut a waiting merge's rebase") {
		t.Fatal("the abort of the cut rebase was not recorded")
	}
	if standing, _ := w.git.RebaseInProgress(context.Background(), w.dirs["ws-c"]); standing {
		t.Fatal("the cut rebase still stands in C's worktree")
	}
	if got := w.git.tree("b-ws-c"); got != "base+w1" {
		t.Fatalf("C holds %s after the exit, want its base kept (base+w1)", got)
	}
	if got := w.entryOf(t, "ws-c").Slot; got != wsm.MergeSlotResolvingConflicts {
		t.Fatalf("C = %s after the exit, want it still holding the slot for the next boot", got)
	}
	w.h.db.mu.Lock()
	_, held := w.h.db.leases["ws-c"]
	w.h.db.mu.Unlock()
	if held {
		t.Fatal("C's lease outlived the exit")
	}
}
