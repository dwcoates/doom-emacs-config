package merge

import (
	"context"
	"errors"
	"slices"
	"strings"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
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

func TestEveryWaitingConflictResolvesAtOnce(t *testing.T) {
	// Arrange: C's and D's commits both conflict with what the head lands.
	w := newWaitingWorld(t, "ws-c", "ws-d")
	w.git.conflict("w1", "a1", "x.go")
	w.git.conflict("w2", "a1", "y.go")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert: neither waits for the other (owner ruling, 2026-11-14).
	for _, ws := range []ids.WorkspaceID{"ws-c", "ws-d"} {
		if got := w.entryOf(t, ws).Slot; got != wsm.MergeSlotResolvingConflicts {
			t.Fatalf("%s = %s, want it resolving its conflicts", ws, got)
		}
	}
	if w.recorded("info", "daemon.merge.rebase_waiting", "waits out of line for the resolving-conflicts slot") {
		t.Fatal("a conflicted merge was recorded waiting for another to resolve")
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

func TestAWaitingRebaseGitRefusesOverChangesKeepsItsBaseAndPromptsNothing(t *testing.T) {
	// Arrange: git refuses C's rebase over changes its clean check missed.
	w := newWaitingWorld(t, "ws-c")
	w.git.refuseDirty(w.dirs["ws-c"], 1)

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if got := w.git.tree("b-ws-c"); got != "base+w1" {
		t.Fatalf("C holds %s, want its base kept (base+w1)", got)
	}
	if got := w.prompts("ws-c", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 0 {
		t.Fatalf("C's session got %d prompts, want none from a git-only rebase", got)
	}
	if !w.recorded("info", "daemon.merge.rebase_waiting", "a waiting merge's worktree is not ready to rebase") {
		t.Fatal("the kept base was not recorded")
	}
	if got := w.entryOf(t, "ws-c"); got.State != wsm.MergeQueued {
		t.Fatalf("C = %+v, want it still waiting in its place", got)
	}
	w.finish(t)
}

func TestAWaitingMergesFreenessIsReadOnlyUnderItsLease(t *testing.T) {
	// Arrange: every freeness read of C records whether C's lease was held.
	w := newWaitingWorld(t, "ws-c")
	var mu sync.Mutex
	var heldAtRead []bool
	w.h.freeness.mu.Lock()
	w.h.freeness.onFree = func(ws ids.WorkspaceID) {
		if ws != "ws-c" {
			return
		}
		w.h.db.mu.Lock()
		_, held := w.h.db.leases["ws-c"]
		w.h.db.mu.Unlock()
		mu.Lock()
		heldAtRead = append(heldAtRead, held)
		mu.Unlock()
	}
	w.h.freeness.mu.Unlock()

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert: the hook watches the pass alone; the queue's drain reads
	// freeness with the store's lock held.
	w.h.freeness.mu.Lock()
	w.h.freeness.onFree = nil
	w.h.freeness.mu.Unlock()
	mu.Lock()
	defer mu.Unlock()
	if len(heldAtRead) == 0 {
		t.Fatal("C's freeness was never read")
	}
	for i, held := range heldAtRead {
		if !held {
			t.Fatalf("freeness read %d of C came before its lease was held: %v", i+1, heldAtRead)
		}
	}
	w.finish(t)
}

func TestAWaitingMergeThisDaemonNoLongerServesIsNotRebased(t *testing.T) {
	// Arrange: C was handed to a successor once its pass began.
	w := newWaitingWorld(t, "ws-c")
	r := liveRun(t, w.h, "ws-c")
	w.h.standing.mu.Lock()
	w.h.standing.notServed["ws-c"] = true
	w.h.standing.mu.Unlock()

	// Act
	got := r.excludedUnderLease()

	// Assert
	if got != "this daemon no longer serves the workspace" {
		t.Fatalf("excludedUnderLease = %q, want the unserved workspace named", got)
	}
}

func TestAResolvingSlotHolderThisDaemonNoLongerServesTakesNothing(t *testing.T) {
	// Arrange: C holds the resolving-conflicts slot; its workspace went to a
	// successor.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")
	w.land(t)
	w.awaitPass(t)
	w.h.standing.mu.Lock()
	w.h.standing.notServed["ws-c"] = true
	w.h.standing.mu.Unlock()

	// Act
	w.h.o.resolveWaiting(context.Background(), w.h.repoKey(), "ws-c")

	// Assert
	if got := w.prompts("ws-c", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 0 {
		t.Fatalf("C's session got %d conflict prompts here, want none", got)
	}
	if got := w.entryOf(t, "ws-c"); got.Slot != wsm.MergeSlotResolvingConflicts {
		t.Fatalf("C = %+v, want it still holding the slot for the daemon that serves it", got)
	}
	if !w.recorded("info", "daemon.merge.rebase_waiting", "belongs to a workspace this daemon no longer serves") {
		t.Fatal("the unserved slot holder was not recorded")
	}
	w.h.db.mu.Lock()
	_, held := w.h.db.leases["ws-c"]
	w.h.db.mu.Unlock()
	if held {
		t.Fatal("C's lease outlived the refusal")
	}
}

func TestTheDaemonsExitLeavesAResolvingRebaseStandingWhileItsTurnStillWorks(t *testing.T) {
	// Arrange: C's resolving rebase prompted its conflict turn, which is
	// still working in the worktree when the daemon exits.
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
		w.h.freeness.mu.Lock()
		w.h.freeness.busyWS = map[ids.WorkspaceID]bool{"ws-c": true}
		w.h.freeness.mu.Unlock()
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
	if !w.recorded("info", "daemon.merge.rebase_waiting", "the rebase is left standing for the next run to clear") {
		t.Fatal("the rebase left standing was not recorded")
	}
	if standing, _ := w.git.RebaseInProgress(context.Background(), w.dirs["ws-c"]); !standing {
		t.Fatal("the exit aborted the rebase under the turn still working in it")
	}
	w.h.db.mu.Lock()
	_, held := w.h.db.leases["ws-c"]
	w.h.db.mu.Unlock()
	if held {
		t.Fatal("C's lease outlived the exit")
	}
}

func TestAResolvingRebaseClearsARebaseACutRunLeftStandingOnceFree(t *testing.T) {
	// Arrange: C holds the resolving-conflicts slot, and a cut run left a
	// rebase standing in its worktree; the workspace is free.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")
	w.land(t)
	w.awaitPass(t)
	dir := w.dirs["ws-c"]
	w.git.cmu.Lock()
	w.git.rebases[dir] = &chainRebase{branch: "b-ws-c"}
	w.git.cmu.Unlock()

	// Act
	w.h.o.resolveWaiting(context.Background(), w.h.repoKey(), "ws-c")

	// Assert
	if !w.recorded("info", "daemon.merge.rebase", "a rebase a cut run left standing for its turn was aborted") {
		t.Fatal("the leftover was not cleared")
	}
	if got := w.prompts("ws-c", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 1 {
		t.Fatalf("C's session got %d conflict prompts, want the fresh rebase's one", got)
	}
}

func TestAbortStandingRebase(t *testing.T) {
	failure := errors.New("git broke")
	tests := []struct {
		name        string
		noDir       bool
		standing    bool
		standingErr error
		abortErr    error
		busy        bool
		// keep is abortStandingRebase's keepResolved; carries puts the
		// merge's entry in line carrying a resolved rebase, and queueErr
		// fails reading the queue.
		keep      bool
		carries   bool
		queueErr  error
		want      bool
		wantAbort bool
		wantError string
		wantInfo  string
	}{
		{name: "a run with no worktree yet aborts nothing", noDir: true, standing: true},
		{name: "a worktree with no rebase standing aborts nothing"},
		{name: "a standing rebase is aborted", standing: true, want: true, wantAbort: true},
		{name: "a rebase the session's turn still works under is left standing", standing: true, busy: true,
			wantInfo: "the rebase is left standing for the next run to clear once the turn ended"},
		{name: "an unreadable worktree is left as git left it", standingErr: failure, wantError: "could not read whether a rebase stands in the merge's worktree"},
		{name: "a rebase that will not abort is left as git left it", standing: true, abortErr: failure, wantError: "could not abort the rebase standing in the merge's worktree"},
		{name: "a rebase the merge's own repair resolved is left standing for the merge that keeps its entry", standing: true, keep: true, carries: true,
			wantInfo: "the rebase standing in the merge's worktree is the one its own conflict repair worked on; it is left standing for the next run to continue"},
		{name: "a rebase no repair resolved is aborted for the merge that keeps its entry", standing: true, keep: true, want: true, wantAbort: true},
		{name: "a resolved rebase of a merge that leaves its queue is aborted", standing: true, carries: true, want: true, wantAbort: true},
		{name: "an entry that cannot be read leaves the rebase as git left it", standing: true, keep: true, queueErr: failure,
			wantError: "could not read whether the merge's own repair resolved the rebase standing in its worktree"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			r := ledgeredRun(t, h)
			if !tt.noDir {
				r.subject = subject{dir: h.sourceD, branch: "feature", targetDir: h.targetD}
			}
			h.git.mu.Lock()
			h.git.standing, h.git.standingErr, h.git.abortErr = tt.standing, tt.standingErr, tt.abortErr
			h.git.mu.Unlock()
			h.freeness.busy = tt.busy
			if tt.keep || tt.carries {
				enqueue(t, h)
			}
			if tt.carries {
				h.db.carryResolved(t, h.repoKey(), theWorkspace, &wsm.ResolvedRebase{Tip: "tip-1", BranchHead: "head-1", Turn: "turn-1"})
			}
			h.db.mu.Lock()
			h.db.queueReadErr = tt.queueErr
			h.db.mu.Unlock()

			// Act.
			got := r.abortStandingRebase(context.Background(), "daemon.merge.abandoned", dlog.Context{"workspace": string(theWorkspace)}, tt.keep)

			// Assert.
			if got != tt.want {
				t.Fatalf("abortStandingRebase = %v, want %v", got, tt.want)
			}
			h.git.mu.Lock()
			aborted := slices.Contains(h.git.rebaseCommands, "abort")
			h.git.mu.Unlock()
			if aborted != tt.wantAbort {
				t.Fatalf("aborted = %v, want %v", aborted, tt.wantAbort)
			}
			if tt.wantInfo != "" && !loggedLike(h, logLike{level: "info", op: "daemon.merge.abandoned", text: tt.wantInfo}) {
				t.Fatalf("no INFO record %q: %+v", tt.wantInfo, h.logs.Records())
			}
			if tt.wantError != "" && !loggedLike(h, logLike{level: "error", op: "daemon.merge.abandoned", text: tt.wantError, context: map[string]string{"dir": h.sourceD, "error": failure.Error()}}) {
				t.Fatalf("no ERROR record %q: %+v", tt.wantError, h.logs.Records())
			}
		})
	}
}

func TestDecideStandingRebase(t *testing.T) {
	resolved := &wsm.ResolvedRebase{Tip: "tip-1", BranchHead: "head-1", Turn: "turn-1"}
	tests := []struct {
		name     string
		standing bool
		resolved *wsm.ResolvedRebase
		tip      string
		want     standingVerdict
	}{
		{name: "nothing standing rebases afresh", tip: "tip-1", want: noRebaseStanding},
		{name: "nothing standing with a stale resolved rebase rebases afresh", resolved: resolved, tip: "tip-1", want: noRebaseStanding},
		{name: "a rebase no repair of the merge's own worked on is a cut run's leftover", standing: true, tip: "tip-1", want: abortCutLeftover},
		{name: "the merge's own resolved rebase on the same tip is continued", standing: true, resolved: resolved, tip: "tip-1", want: continueResolved},
		{name: "the merge's own resolved rebase whose target moved is continued, then rebased", standing: true, resolved: resolved, tip: "tip-2", want: continueThenRebase},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := decideStandingRebase(tc.standing, tc.resolved, tc.tip)

			// Assert
			if got != tc.want {
				t.Fatalf("decideStandingRebase = %s, want %s", got, tc.want)
			}
		})
	}
}

// settleWorld is a run of the harness's queued merge whose worktree is /wt,
// its entry carrying RESOLVED.
func settleWorld(t *testing.T, resolved *wsm.ResolvedRebase) (*harness, *run) {
	t.Helper()
	h := newHarness(t)
	enqueue(t, h)
	h.db.carryResolved(t, h.repoKey(), theWorkspace, resolved)
	r := ledgeredRun(t, h)
	r.subject = subject{branch: "feature", dir: "/wt", targetDir: "/target"}
	return h, r
}

// resolvedOf answers what the harness's entry records as resolved.
func resolvedOf(t *testing.T, h *harness) *wsm.ResolvedRebase {
	t.Helper()
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	return h.db.queues[h.repoKey()][0].Resolved
}

func TestSettleStandingRebase(t *testing.T) {
	resolvedOn := func(tip string) *wsm.ResolvedRebase {
		return &wsm.ResolvedRebase{Tip: tip, BranchHead: "head-1", Turn: "turn-1"}
	}
	tests := []struct {
		name         string
		standing     bool
		resolved     *wsm.ResolvedRebase
		wantContinue bool
		wantAbort    bool
		wantKept     bool
		wantVerdict  string
		wantText     string
	}{
		{name: "the merge's own resolved rebase on the tip is continued and kept", standing: true, resolved: resolvedOn("tip-1"),
			wantContinue: true, wantKept: true, wantVerdict: "continue_resolved", wantText: "it is continued, never started over"},
		{name: "a cut run's leftover is aborted", standing: true,
			wantAbort: true, wantVerdict: "abort_cut_leftover", wantText: "a rebase a cut run left standing for its turn was aborted"},
		{name: "a resolved rebase whose target moved is continued and kept, never aborted", standing: true, resolved: resolvedOn("tip-0"),
			wantContinue: true, wantKept: true, wantVerdict: "continue_then_rebase", wantText: "it is continued and kept, never aborted, and the branch then rebases onto the new tip"},
		{name: "a resolved rebase no longer standing is cleared", resolved: resolvedOn("tip-1")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h, r := settleWorld(t, tc.resolved)
			h.git.leftStanding = tc.standing

			// Act
			got, err := r.settleStandingRebase(context.Background(), "tip-1")

			// Assert
			if err != nil {
				t.Fatalf("settleStandingRebase: %v", err)
			}
			if (got != nil) != tc.wantContinue || (tc.wantContinue && *got != *tc.resolved) {
				t.Fatalf("settleStandingRebase = %+v, want continuing %v", got, tc.wantContinue)
			}
			h.git.mu.Lock()
			aborted := slices.Contains(h.git.rebaseCommands, "abort")
			h.git.mu.Unlock()
			if aborted != tc.wantAbort {
				t.Fatalf("aborted = %v, want %v", aborted, tc.wantAbort)
			}
			if kept := resolvedOf(t, h) != nil; kept != tc.wantKept {
				t.Fatalf("the entry still carries its resolved rebase = %v, want %v", kept, tc.wantKept)
			}
			if tc.wantVerdict != "" && !loggedLike(h, logLike{level: "info", op: "daemon.merge.rebase", text: tc.wantText,
				context: map[string]string{"verdict": tc.wantVerdict, "worktree": "/wt", "tip": "tip-1", "workspace": string(theWorkspace)}}) {
				t.Fatalf("no INFO record of the %s decision with its context", tc.wantVerdict)
			}
		})
	}
}

func TestSettleStandingRebaseFailures(t *testing.T) {
	failure := errors.New("broke")
	resolved := &wsm.ResolvedRebase{Tip: "tip-0", BranchHead: "head-1", Turn: "turn-1"}
	tests := []struct {
		name string
		// carries is whether the entry records a resolved rebase, and
		// standing whether a rebase stands in the worktree.
		carries  bool
		standing bool
		arrange  func(h *harness)
		wantText string
		wantKept bool
	}{
		{name: "a rebase that cannot be read fails and changes nothing", carries: true, standing: true, arrange: func(h *harness) { h.git.standingErr = failure },
			wantText: "reading whether a rebase stands", wantKept: true},
		{name: "an entry that cannot be read fails and changes nothing", carries: true, standing: true, arrange: func(h *harness) { h.db.queueReadErr = failure },
			wantText: "reading whether the merge's own repair resolved", wantKept: true},
		{name: "a resolved rebase no longer standing that cannot be cleared fails", carries: true, arrange: func(h *harness) { h.db.clearResolvedErr = failure },
			wantText: "clearing the resolved rebase recorded", wantKept: true},
		{name: "a cut run's leftover whose abort fails is surfaced", standing: true, arrange: func(h *harness) { h.git.abortErr = failure },
			wantText: "aborting the rebase a cut run left standing"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var carried *wsm.ResolvedRebase
			if tc.carries {
				carried = resolved
			}
			h, r := settleWorld(t, carried)
			h.git.leftStanding = tc.standing
			tc.arrange(h)

			// Act
			got, err := r.settleStandingRebase(context.Background(), "tip-1")

			// Assert
			if got != nil || !errors.Is(err, failure) || !strings.Contains(err.Error(), tc.wantText) {
				t.Fatalf("settleStandingRebase = %+v, %v, want the failure naming %q", got, err, tc.wantText)
			}
			h.db.mu.Lock()
			h.db.queueReadErr = nil
			h.db.mu.Unlock()
			if kept := resolvedOf(t, h) != nil; kept != tc.wantKept {
				t.Fatalf("the entry still carries its resolved rebase = %v, want %v", kept, tc.wantKept)
			}
			h.git.mu.Lock()
			aborted := slices.Contains(h.git.rebaseCommands, "abort")
			standing := h.git.leftStanding
			h.git.mu.Unlock()
			if aborted || standing != tc.standing {
				t.Fatalf("aborted = %v, still standing = %v; want no abort, the worktree left as it stood", aborted, standing)
			}
		})
	}
}

func TestAWaitingMergeContinuesAndKeepsTheRebaseItsOwnRepairResolvedOnceTheTargetMoved(t *testing.T) {
	// Arrange: C's worktree holds the rebase its own conflict repair
	// resolved onto the target's tip, which the head's landing then moves.
	w := newWaitingWorld(t, "ws-c")
	dir, resolvedOn := w.dirs["ws-c"], w.git.ref("master")
	original := w.git.ref("b-ws-c")
	w.git.standResolved(dir, resolvedOn)
	w.h.db.carryResolved(t, w.h.repoKey(), "ws-c", &wsm.ResolvedRebase{Tip: resolvedOn, BranchHead: original, Turn: "turn-c"})

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if standing, _ := w.git.RebaseInProgress(context.Background(), dir); standing {
		t.Fatal("a rebase still stands in C's worktree")
	}
	w.git.cmu.Lock()
	aborts := w.git.aborts
	w.git.cmu.Unlock()
	if aborts != 0 {
		t.Fatalf("aborts = %d, want the resolved rebase never aborted", aborts)
	}
	commits, parents := w.git.lastStart(dir)
	if len(commits) != 1 || commits[0] == original || parents[0] != resolvedOn {
		t.Fatalf("the rebase onto the new tip replays %v (made on %v), want the resolved commit made on %s, not the original %s", commits, parents, resolvedOn, original)
	}
	if got := w.git.own(w.git.ref("master"), "b-ws-c"); !slices.Equal(got, []string{"w1"}) {
		t.Fatalf("C carries %v on the new tip, want its one resolved commit", got)
	}
	if !w.recorded("info", "daemon.merge.rebase", "it is continued and kept, never aborted") ||
		!w.recorded("info", "daemon.merge.rebase_waiting", "the waiting merge stands on the target's new tip") {
		t.Fatal("the continuation and the rebase onto the new tip were not recorded")
	}
	if got := w.entryOf(t, "ws-c").Resolved; got != nil {
		t.Fatalf("resolved = %+v, want it retired once its rebase finished", got)
	}
	if n := w.prompts("ws-c", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); n != 0 {
		t.Fatalf("C's session got %d conflict prompts, want none", n)
	}
}

func TestAWaitingMergeWhoseResolvedRebaseMeetsAConflictGitOnlyLeavesItStanding(t *testing.T) {
	// Arrange: C's resolved rebase stands with a later commit that
	// conflicts, which a git-only pass cannot resolve.
	w := newWaitingWorld(t, "ws-c")
	w.git.commitOn("b-ws-c", "w2")
	w.git.conflict("w2", "base", "y.go")
	dir, resolvedOn := w.dirs["ws-c"], w.git.ref("master")
	w.git.standResolved(dir, resolvedOn)
	w.h.db.carryResolved(t, w.h.repoKey(), "ws-c", &wsm.ResolvedRebase{Tip: resolvedOn, BranchHead: w.git.ref("b-ws-c"), Turn: "turn-c"})

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	w.git.cmu.Lock()
	aborts := w.git.aborts
	w.git.cmu.Unlock()
	if aborts != 0 {
		t.Fatalf("aborts = %d, want the resolved rebase left standing", aborts)
	}
	if !loggedLike(w.h, logLike{level: "info", op: "daemon.merge.rebase_waiting", text: "the rebase its own repair resolved is left standing with its resolution",
		context: map[string]string{"workspace": "ws-c", "kept": "true"}}) {
		t.Fatal("leaving the resolved rebase standing was not recorded with its context")
	}
}
