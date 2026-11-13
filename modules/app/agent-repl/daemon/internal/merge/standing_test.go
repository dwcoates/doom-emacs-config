package merge

import (
	"context"
	"errors"
	"testing"
)

func TestQueueHeld(t *testing.T) {
	tests := []struct {
		name   string
		paused bool
		serves bool
		want   bool
	}{
		{name: "a running queue this daemon serves", serves: true},
		{name: "a paused queue", paused: true, serves: true, want: true},
		{name: "a queue a handover holds", want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			seedQueue(h, h.repoKey(), tt.paused)
			h.standing.serves = tt.serves

			// Act
			got, err := h.o.queueHeld(context.Background(), h.repoKey())

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("queueHeld = (%v, %v), want %v", got, err, tt.want)
			}
		})
	}
}

func TestQueueHeldAnswersAnUnreadablePause(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.pausedReadErr = errors.New("db gone")
	h.db.mu.Unlock()

	// Act
	_, err := h.o.queueHeld(context.Background(), h.repoKey())

	// Assert
	if err == nil {
		t.Fatal("queueHeld = nil error, want the read's failure")
	}
}

func TestAQueueAHandoverHoldsAdmitsNothing(t *testing.T) {
	// Arrange
	c := newChain(t)
	enqueue(t, c.h)
	c.h.standing.serves = false

	// Act
	if err := c.h.admit(context.Background()); err != nil {
		t.Fatalf("the pump ended on: %v", err)
	}

	// Assert
	if got := c.runner.ran(c.dirs[theWorkspace]); got != 0 {
		t.Fatalf("the gate ran %d times while a handover held the queue, want none", got)
	}
}

func TestAFrontWhoseWorkspaceIsNotServedHereYetIsNotAdmitted(t *testing.T) {
	// Arrange
	c := newChain(t)
	enqueue(t, c.h)
	c.h.standing.serve(theWorkspace, false)

	// Act
	if err := c.h.admit(context.Background()); err != nil {
		t.Fatalf("the pump ended on: %v", err)
	}

	// Assert
	if got := c.runner.ran(c.dirs[theWorkspace]); got != 0 {
		t.Fatalf("the gate ran %d times for a workspace not served here, want none", got)
	}
}

func TestAHandoverSuspendsTheRunningMergeAndItsEndTakesItOnAtItsStep(t *testing.T) {
	// Arrange: the head holds in its gate.
	c, done := headInGate(t)

	// Act: a handover holds the queue, then gives it back.
	c.h.standing.set(false)
	serves, _ := c.h.o.followStandingOnce(context.Background(), true)
	<-done
	cut := c.runner.ran(c.dirs[theWorkspace])
	c.h.standing.set(true)
	serves, _ = c.h.o.followStandingOnce(context.Background(), serves)
	c.promote(t)

	// Assert
	if cut != 1 || !serves {
		t.Fatalf("gate ran %d before the hold ended, serves %v; want one cut run and the queue taken on", cut, serves)
	}
	if got := c.git.landedOwn(); len(got) != 1 {
		t.Fatalf("landings after the handover ended %v, want the merge landed once", got)
	}
	if !c.recorded("info", opStanding, "a handover holds every queue: running merges are suspended at their step and nothing is admitted") {
		t.Fatal("the handover's hold was not recorded")
	}
}

func TestFollowStandingOnceActsOnlyOnAChange(t *testing.T) {
	// Arrange
	h := newHarness(t)
	seedQueue(h, h.repoKey(), false)

	// Act
	serves, changed := h.o.followStandingOnce(context.Background(), true)

	// Assert
	if !serves || changed == nil {
		t.Fatalf("followStandingOnce = (%v, %v), want the standing as it was and its edge", serves, changed)
	}
	if _, ok := h.recordFor("info", opStanding); ok {
		t.Fatal("an unchanged standing was acted on")
	}
}

func TestFollowStandingEndsWithItsContext(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := h.o.FollowStanding(ctx)

	// Assert
	if err != nil {
		t.Fatalf("FollowStanding = %v, want nil at its context's end", err)
	}
}

func TestAStandingMoveRecordsQueuesItCannotRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.allQueuesErr = errors.New("db gone")
	h.db.mu.Unlock()

	// Act
	h.o.standingMoved(context.Background(), false)

	// Assert
	if rec, ok := h.recordFor("error", opStanding); !ok || rec.Context["serves"] != false {
		t.Fatalf("no error record of the unreadable queues: %+v", rec)
	}
}
