package merge

import (
	"context"
	"strings"
	"testing"

	"agent-repld/internal/wsm"
)

func TestAPausedQueueAdmitsNothing(t *testing.T) {
	// Arrange.
	c := newChain(t)
	enqueue(t, c.h)
	c.h.db.paused[c.h.repoKey()] = true

	// Act.
	if err := c.h.admit(context.Background()); err != nil {
		t.Fatalf("the pump ended on: %v", err)
	}

	// Assert.
	if got := c.runner.ran(c.dirs[theWorkspace]); got != 0 {
		t.Fatalf("the gate ran %d times on a paused queue, want none", got)
	}
}

func TestAPauseSuspendsTheRunningMergeAndTheQueuesResumeTakesItOnAtItsStep(t *testing.T) {
	// Arrange, Act: pause while the head holds in its gate (the running gate
	// is cut), then resume the queue.
	c := pausedUnderGate(t)
	cut := c.runner.ran(c.dirs[theWorkspace])
	queued, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatal(err)
	}
	if err := c.h.o.Unpause(context.Background(), nil); err != nil {
		t.Fatalf("Unpause: %v", err)
	}
	c.promote(t)

	// Assert.
	if cut != 1 || len(queued) != 1 || queued[0].State != wsm.MergeAdmitted {
		t.Fatalf("after the pause: gate ran %d, queue %v; want one cut run and the merge still admitted in line", cut, queued)
	}
	if got := c.git.landedOwn(); len(got) != 1 {
		t.Fatalf("landings after the resume %v, want the merge landed once", got)
	}
	if !c.recorded("info", "daemon.merge.pause", "suspended for its queue's pause") {
		t.Fatal("the pause's suspension was not recorded")
	}
}

// pausedUnderGate pauses every queue while the head holds in its test gate,
// and answers once the paused run has let go.
func pausedUnderGate(t *testing.T) *chain {
	t.Helper()
	c, done := headInGate(t)
	if err := c.h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("Pause: %v", err)
	}
	<-done
	return c
}

func TestAGateThePauseCutNamesThePause(t *testing.T) {
	// Arrange, Act.
	c := pausedUnderGate(t)

	// Assert.
	got := gateRecord(t, c.h, "test gate was cut before its verdict")
	if got["by"] != "its queue's pause" {
		t.Fatalf("cut by = %v, want the queue's pause", got["by"])
	}
	if gateRecords(c.h, "it runs again from its start once the queue is resumed") != 1 {
		t.Fatal("the cut does not say the gate runs again once the queue is resumed")
	}
}

func TestAPausedMergesSuspendRecordNamesThePause(t *testing.T) {
	// Arrange, Act.
	c := pausedUnderGate(t)

	// Assert.
	rec, found := recordWith(c.h, "info", "daemon.merge.suspend")
	if !found {
		t.Fatal("the paused merge wrote no INFO suspend record")
	}
	if rec.Context["by"] != "its queue's pause" || rec.Context["cause"] != errPaused.Error() {
		t.Fatalf("suspend record context = %v, want the pause as its cause", rec.Context)
	}
	if !strings.Contains(rec.Message, "until the queue is resumed") {
		t.Fatalf("suspend record says %q, want that it stays suspended until the queue is resumed", rec.Message)
	}
}

func TestAPausedMergesSuspendRecordKeepsTheStepsErrorApart(t *testing.T) {
	// Arrange, Act.
	c := pausedUnderGate(t)

	// Assert.
	rec, found := recordWith(c.h, "info", "daemon.merge.suspend")
	if !found {
		t.Fatal("the paused merge wrote no INFO suspend record")
	}
	if stepErr, ok := rec.Context["step_error"].(string); !ok || stepErr == "" || stepErr == rec.Context["cause"] {
		t.Fatalf("suspend record context = %v, want the cut step's own error as step_error", rec.Context)
	}
}

func TestThePausesTakeBackRecordsThatTheMergeStaysSuspended(t *testing.T) {
	// Arrange, Act.
	c := pausedUnderGate(t)

	// Assert.
	rec, found := recordWith(c.h, "info", "daemon.merge.recover")
	if !found {
		t.Fatal("the pause's take-back wrote no INFO recover record")
	}
	if rec.Context["paused"] != true || !strings.Contains(rec.Message, "stays suspended until the queue is resumed") {
		t.Fatalf("recover record = %q %v, want that the merge stays suspended on its paused queue", rec.Message, rec.Context)
	}
}
