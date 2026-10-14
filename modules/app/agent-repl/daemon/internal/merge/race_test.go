package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// behindWins runs the race the merge behind the head wins: the head's first
// ask for its repository's test turn is parked, so the merge behind it takes
// the turn, passes on the target's tip and overtakes the head before the head
// ever tests. However the scheduler orders the two, the head cannot pass
// first: it never holds the turn. It answers once the overtaken head has let
// go, then drains the queue.
func (c *chain) behindWins(t *testing.T) {
	t.Helper()
	release := c.parkTurn(theWorkspace)
	done := admitAsync(c.h, context.Background())
	if err := <-done; err != nil {
		t.Fatalf("the overtaken head's admission ended on: %v", err)
	}
	release()
	c.drain(t)
}

func TestTheMergeBehindTheHeadWinsTheRaceWhenItsGatePassesFirst(t *testing.T) {
	// Arrange: the head holds in its gate; the merge behind it passes.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")

	// Act: the head yields to the winner, the winner lands, and the head
	// starts over on the new tip and lands.
	c.behindWins(t)

	// Assert.
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"b1"}) || !slices.Equal(got[1], []string{"a1"}) {
		t.Fatalf("landings brought %v, want [b1] then [a1]", got)
	}
	if tree := c.git.tree("master"); tree != "a1+b1+base" {
		t.Fatalf("master holds %s, want a1+b1+base", tree)
	}
}

func TestTheHeadWinsTheRaceWhenItsGatePassesFirst(t *testing.T) {
	// Arrange: the head holds in its gate, so the merge behind it waits in
	// line for the test turn.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	releaseHead, done := c.headHoldsTheTurn(t, "ws-b")

	// Act: the head lands; the merge behind it starts over on the new tip as
	// the head and lands.
	close(releaseHead)
	if err := <-done; err != nil {
		t.Fatalf("the head's admission ended on: %v", err)
	}
	c.promote(t)

	// Assert.
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"a1"}) || !slices.Equal(got[1], []string{"b1"}) {
		t.Fatalf("landings brought %v, want [a1] then [b1]", got)
	}
	if got := c.runner.overlapped(); got != 1 {
		t.Fatalf("%d gates of the repository ran at once, want one at a time", got)
	}
}

func TestAHeadThatLosesTheRaceMidConflictFinishesItsStepThenRebasesOntoTheNewTip(t *testing.T) {
	// Arrange: master moved, and A's commit conflicts with the move; A's
	// session is still resolving it when B, behind it, passes its gate.
	c := newChain(t)
	c.git.commitOn("master", "m1")
	c.git.conflict("a1", "m1", "x.go")
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	turnRuns := make(chan struct{})
	conflictPrompted := make(chan struct{}, 1)
	c.h.mu.Lock()
	c.h.awaitGate = turnRuns
	c.h.mu.Unlock()
	c.h.queue.mu.Lock()
	c.h.queue.onSubmit = func(sub promptqueue.Submission) {
		if sub.WS != theWorkspace || sub.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR {
			return
		}
		// THE SESSION HAS THE TURN: a resume reattaches to it.
		c.h.db.mu.Lock()
		c.h.db.turns[theWorkspace] = append(c.h.db.turns[theWorkspace], wsm.Turn{ID: sub.Turn, Workspace: theWorkspace})
		c.h.db.mu.Unlock()
		conflictPrompted <- struct{}{}
	}
	c.h.queue.mu.Unlock()
	// B's gate holds until A is in its turn, so B overtakes A mid-turn and
	// never before A reached it.
	behindGate, releaseBehind := c.runner.hold(c.dirs["ws-b"])
	done := admitAsync(c.h, context.Background())
	<-conflictPrompted
	<-behindGate

	// Act: B overtakes A mid-turn and lands; A's turn then ends, and A, at
	// the head again, resumes.
	close(releaseBehind)
	if err := <-done; err != nil {
		t.Fatalf("the overtaken head's admission ended on: %v", err)
	}
	c.promote(t)
	landedFirst := c.git.landedOwn()
	close(turnRuns)
	c.drain(t)

	// Assert.
	if len(landedFirst) != 1 || !slices.Equal(landedFirst[0], []string{"b1"}) {
		t.Fatalf("landings while A was suspended = %v, want B's alone", landedFirst)
	}
	if got := c.prompts(theWorkspace, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 1 {
		t.Fatalf("A's session got %d conflict prompts, want 1: its resume reattaches to the turn it was on", got)
	}
	if !c.recorded("info", "daemon.merge.resume", "the target moved while the merge was suspended") {
		t.Fatal("A did not rebase onto the new tip once it finished its step")
	}
	if tree := c.git.tree("master"); tree != "a1+b1+base+m1" {
		t.Fatalf("master holds %s, want a1+b1+base+m1", tree)
	}
}

func TestATakeBackThatFailsAfterTheRaceIsRecordedAndLeavesTheLoserSuspended(t *testing.T) {
	// Arrange: the head waits to ask for its test turn while the merge behind
	// it holds in its gate; the store cannot answer the take-back's read once
	// the head is suspended.
	c := newChain(t)
	releaseHeadTurn := c.parkTurn(theWorkspace)
	defer releaseHeadTurn()
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	behindGate, releaseBehind := c.runner.hold(c.dirs["ws-b"])
	done := admitAsync(c.h, context.Background())
	<-behindGate
	c.h.db.mu.Lock()
	c.h.db.allQueuesErr = errors.New("the store is closed")
	c.h.db.mu.Unlock()

	// Act.
	close(releaseBehind)
	if err := <-done; err != nil {
		t.Fatalf("the overtaken head's admission ended on: %v", err)
	}
	c.awaitIdle(t, "ws-b", func() bool {
		return c.recorded("error", "daemon.merge.race", "could not take back the head that lost the race to land")
	})

	// Assert.
	rec, found := dlog.FindRecord(c.h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.race",
		Message: "could not take back the head that lost the race to land"})
	if !found || rec.Context["head"] != string(theWorkspace) || rec.Context["workspace"] != "ws-b" || rec.Context["error"] == nil {
		t.Fatalf("error record = %v (found %v), want the winner, the head and the cause", rec.Context, found)
	}
	if _, resumes := c.h.o.resumes[theWorkspace]; resumes || c.h.o.inChain(c.h.repoKey(), theWorkspace) {
		t.Fatal("a loser the take-back failed for was resumed")
	}
	c.h.db.mu.Lock()
	c.h.db.allQueuesErr = nil
	c.h.db.mu.Unlock()
	if err := c.h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("taking the loser back: %v", err)
	}
	c.drain(t)
}

func TestASuspensionThatFailsAfterTheRaceIsRecorded(t *testing.T) {
	// Arrange: the head is parked in a rebase that will not let go, past the
	// suspension's bounds; the merge behind it passes its gate.
	c := newChain(t)
	c.h.o.drainBound, c.h.o.gitBound = time.Millisecond, time.Millisecond
	c.git.commitOn("master", "m1")
	headRebase, releaseHead := c.git.holdRebaseStubbornly(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	behindGate, releaseBehind := c.runner.hold(c.dirs["ws-b"])
	done := admitAsync(c.h, context.Background())
	<-headRebase
	<-behindGate

	// Act: B passes while A is parked in its rebase.
	close(releaseBehind)
	c.awaitIdle(t, "ws-b", func() bool {
		return c.recorded("error", "daemon.merge.race", "could not suspend the head that lost the race to land")
	})

	// Assert.
	rec, found := dlog.FindRecord(c.h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.race",
		Message: "could not suspend the head that lost the race to land"})
	if !found || rec.Context["head"] != string(theWorkspace) || rec.Context["workspace"] != "ws-b" || rec.Context["error"] == nil {
		t.Fatalf("error record = %v (found %v), want the head and the cause", rec.Context, found)
	}
	close(releaseHead)
	<-done
	if err := c.h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("taking the loser back: %v", err)
	}
	c.drain(t)
}
