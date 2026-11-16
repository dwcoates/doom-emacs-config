package merge

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/wsm"
)

func TestTheHoldsSplitTheHeadFromSpeculativeWork(t *testing.T) {
	tests := []struct {
		name            string
		paused          bool
		serves          bool
		check           string
		wantHead        bool
		wantSpeculation bool
	}{
		{name: "a running queue this daemon serves", serves: true},
		{name: "a paused queue", paused: true, serves: true, wantHead: true, wantSpeculation: true},
		{name: "a queue a handover holds", wantHead: true, wantSpeculation: true},
		{name: "a queue whose deploy's check is assessed holds only its head", serves: true, check: "owed", wantHead: true},
		{name: "a queue whose deploy has a fix dispatched holds everything new", serves: true, check: "dispatched", wantHead: true, wantSpeculation: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ctx := context.Background()
			seedQueue(h, h.repoKey(), tt.paused)
			h.standing.serves = tt.serves
			if tt.check != "" {
				if _, err := h.db.OweMediation(ctx, h.repoKey(), epochPause, ""); err != nil {
					t.Fatal(err)
				}
			}
			if tt.check == "dispatched" {
				if _, err := h.db.DispatchMediation(ctx, h.repoKey(), epochPause, epochPause, "ws-remedy"); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			head, headErr := h.o.queueHeld(ctx, h.repoKey())
			speculation, specErr := h.o.speculationHeld(ctx, h.repoKey())

			// Assert
			if headErr != nil || head != tt.wantHead {
				t.Fatalf("queueHeld = (%v, %v), want %v", head, headErr, tt.wantHead)
			}
			if specErr != nil || speculation != tt.wantSpeculation {
				t.Fatalf("speculationHeld = (%v, %v), want %v", speculation, specErr, tt.wantSpeculation)
			}
		})
	}
}

func TestQueueHeldAnswersAnUnreadableMediation(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.mediationErr = errors.New("db gone")
	h.db.mu.Unlock()

	// Act
	_, err := h.o.queueHeld(context.Background(), h.repoKey())
	_, specErr := h.o.speculationHeld(context.Background(), h.repoKey())

	// Assert
	if err == nil || specErr == nil {
		t.Fatalf("queueHeld = %v, speculationHeld = %v; want the check read's failure from both", err, specErr)
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

// A MERGE WHOSE WORKSPACE ANOTHER DAEMON SERVES IS PASSED OVER, at any place
// in line (owner ruling, 2026-11-15): the merge behind it runs, and it keeps
// its place: once the merge admitted past it ends, it stands at the front.
func TestAMergeWhoseWorkspaceIsServedElsewhereIsPassedOverAndKeepsItsPlace(t *testing.T) {
	// Arrange: the front's workspace is still served by an older daemon.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	c.h.standing.serve(theWorkspace, false)

	// Act
	if err := c.h.admit(context.Background()); err != nil {
		t.Fatalf("the pump ended on: %v", err)
	}

	// Assert
	if got := c.runner.ran(c.dirs[theWorkspace]); got != 0 {
		t.Fatalf("the gate ran %d times for a workspace not served here, want none", got)
	}
	c.h.db.mu.Lock()
	line := slices.Clone(c.h.db.queues[c.h.repoKey()])
	c.h.db.mu.Unlock()
	if got := c.runner.ran(c.dirs["ws-b"]); got == 0 {
		t.Fatal("the merge behind the passed-over one never ran its gate")
	}
	if len(line) != 1 || line[0].Workspace != theWorkspace || line[0].State != wsm.MergeQueued {
		t.Fatalf("line = %+v, want ws-b gone through and the passed-over merge waiting at the front", line)
	}
}

func TestAHandoverSuspendsTheRunningMergeAndItsEndTakesItOnAtItsStep(t *testing.T) {
	// Arrange: the head holds in its gate.
	c, done, release := headHeldInGate(t)

	// Act: a handover holds the queue, the gate's run finishes on the test
	// service meanwhile, then the handover gives the queue back.
	c.h.standing.set(false)
	serves, _ := c.h.o.followStandingOnce(context.Background(), true)
	<-done
	close(release)
	c.h.standing.set(true)
	serves, _ = c.h.o.followStandingOnce(context.Background(), serves)
	c.promote(t)

	// Assert: the run the handover left going is the one the resume read.
	if ran := c.runner.ran(c.dirs[theWorkspace]); ran != 1 || !serves {
		t.Fatalf("gate ran %d, serves %v; want one run and the queue taken on", ran, serves)
	}
	if got := c.git.landedOwn(); len(got) != 1 {
		t.Fatalf("landings after the handover ended %v, want the merge landed once", got)
	}
	if !c.recorded("info", opStanding, "a handover holds every queue: running merges are suspended at their step and nothing is admitted") {
		t.Fatal("the handover's hold was not recorded")
	}
}

func TestAHandoverSuspendsTheRunningMergeAsTheHandoversNotAPause(t *testing.T) {
	// Arrange: the head holds in its gate.
	c, done := headInGate(t)

	// Act: a handover holds the queue.
	c.h.standing.set(false)
	c.h.o.followStandingOnce(context.Background(), true)
	<-done

	// Assert
	if !c.recorded("info", suspendHandover.op, suspendHandover.moved) {
		t.Fatal("the merge's suspension was not recorded as the handover's")
	}
	if _, paused := c.h.recordFor("info", suspendPause.op); paused {
		t.Fatal("the handover's suspension was recorded as a pause")
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
