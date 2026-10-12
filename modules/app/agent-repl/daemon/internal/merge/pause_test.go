package merge

import (
	"context"
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
	// Arrange: the head holds in its gate.
	c := newChain(t)
	inGate, _ := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	done := admitAsync(c.h, context.Background())
	<-inGate

	// Act: pause (the running gate is cut), then resume the queue.
	if err := c.h.o.Pause(context.Background(), nil); err != nil {
		t.Fatalf("Pause: %v", err)
	}
	<-done
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
