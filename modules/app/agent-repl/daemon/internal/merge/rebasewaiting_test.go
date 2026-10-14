package merge

import (
	"context"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// waitingWorld is a chain of the head, the merge behind it (ws-b) and one more
// inside the working window (ws-in), with the waiting members named queued
// beyond the window; the members' rebases and the passes' ends are reported
// on channels.
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
	c.queue(t, "ws-in", "in1")
	for i, ws := range waiting {
		c.queue(t, ws, "w"+string(rune('1'+i)))
	}
	w.releaseHead, w.done = c.headHoldsTheTurn(t, "ws-b")
	return w
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
	select {
	case <-w.passes:
	case <-time.After(5 * time.Second):
		t.Fatal("no pass over the waiting members ended")
	}
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

func TestAWaitingMergesConflictIsResolvedByItsOwnSession(t *testing.T) {
	// Arrange: C's commit conflicts with what the head lands.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")

	// Act
	w.land(t)
	w.awaitPass(t)

	// Assert
	if got := w.prompts("ws-c", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 1 {
		t.Fatalf("C's session got %d conflict prompts while it waited in line, want 1", got)
	}
	w.finish(t)
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
	if len(line) != 3 || line[2].Workspace != "ws-c" || line[2].State != wsm.MergeQueued {
		t.Fatalf("queue after the rebase = %+v, want ws-c still waiting third", line)
	}
	w.finish(t)
}

func TestTheDaemonsExitAbortsAWaitingMergesRebaseAndKeepsItsBase(t *testing.T) {
	// Arrange: C's commit conflicts with what the head lands, and its
	// session's conflict turn is still running when the daemon exits.
	w := newWaitingWorld(t, "ws-c")
	w.git.conflict("w1", "a1", "x.go")
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
	w.land(t)
	<-prompted

	// Act
	w.h.o.Drain(context.Background())
	waitGone(t, w.h.o)

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
	w.h.db.mu.Lock()
	_, held := w.h.db.leases["ws-c"]
	w.h.db.mu.Unlock()
	if held {
		t.Fatal("C's lease outlived the exit")
	}
}
