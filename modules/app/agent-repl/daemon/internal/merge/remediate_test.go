package merge

import (
	"context"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// failingSuiteRun is a gate run whose whole suite finished with one failure.
var failingSuiteRun = scriptedRun{Output: "[agent-repl-tests] ERROR: daemon failed after 4s with exit code 1\n", Code: 1}

// stepAsideScenario runs a head whose suite fails with a merge behind it,
// until the failing head stepped aside and its step-aside settled.
func stepAsideScenario(t *testing.T) *chain {
	t.Helper()
	c := newChainOfWidth(t, 1)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	c.runner.script(c.dirs[theWorkspace], failingSuiteRun)
	if err := receiveWithin(t, admitAsync(c.h, context.Background()), "the failing head's admission ending"); err != nil {
		t.Fatalf("the failing head's admission ended on: %v", err)
	}
	settled := make(chan struct{})
	go func() { c.h.o.live.Wait(); close(settled) }()
	receiveWithin(t, settled, "the step aside settling")
	return c
}

// drainUntil pumps the queue, as drain does, until done holds.
func (c *chain) drainUntil(t *testing.T, what string, done func() bool) {
	t.Helper()
	bound := time.NewTimer(10 * time.Second)
	defer bound.Stop()
	for !done() {
		c.promote(t)
		if done() {
			return
		}
		select {
		case <-c.idle:
		case <-c.kicks:
		case <-bound.C:
			t.Fatalf("%s never happened", what)
		}
	}
}

func TestAHeadWhoseSuiteFailsStepsAsideOutOfLine(t *testing.T) {
	// Arrange + Act
	c := stepAsideScenario(t)

	// Assert
	queue, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	line := inLine(queue)
	if len(line) != 1 || line[0].Workspace != "ws-b" {
		t.Fatalf("line = %+v, want only ws-b in line", line)
	}
	if !slices.ContainsFunc(queue, func(e wsm.MergeQueueEntry) bool {
		return e.Workspace == theWorkspace && e.State == wsm.MergeRemediating
	}) {
		t.Fatalf("queue = %+v, want the failed head remediating", queue)
	}
}

func TestTheMergeBehindAFailedHeadTakesTheHeadAndLands(t *testing.T) {
	// Arrange
	c := stepAsideScenario(t)

	// Act
	c.drainUntil(t, "ws-b landing", func() bool { return len(c.git.landedOwn()) == 1 })

	// Assert
	if got := c.git.landedOwn(); !slices.Equal(got[0], []string{"b1"}) {
		t.Fatalf("first landing brought %v, want b1", got[0])
	}
}

func TestARemediatedMergeReturnsAndLandsAfterTheMergeAtTheHead(t *testing.T) {
	// Arrange: the failed head fixes while ws-b lands.
	c := stepAsideScenario(t)
	c.drainUntil(t, "ws-b landing", func() bool { return len(c.git.landedOwn()) == 1 })

	// Act: its fixing turn ends; it returns to the line and lands.
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("returnRemediated: %v", err)
	}
	c.drain(t)

	// Assert
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[1], []string{"a1"}) {
		t.Fatalf("landings brought %v, want b1 then a1", got)
	}
}

func TestAFailingHeadWithNobodyBehindItFixesInPlace(t *testing.T) {
	// Arrange: the head alone, its first suite failing.
	c := newChainOfWidth(t, 1)
	enqueue(t, c.h)
	c.runner.script(c.dirs[theWorkspace], failingSuiteRun)

	// Act
	c.drain(t)

	// Assert
	if got := c.git.landedOwn(); len(got) != 1 || !slices.Equal(got[0], []string{"a1"}) {
		t.Fatalf("landings brought %v, want a1 fixed in place", got)
	}
}

func TestAMergeRemediatingOutOfLineIsNeverResumed(t *testing.T) {
	// Arrange: the failed head remediating, nothing admitted in line yet.
	c := stepAsideScenario(t)

	// Act
	ws, resuming, err := c.h.o.resumeIn(context.Background(), c.h.repoKey())

	// Assert
	if err != nil {
		t.Fatalf("resumeIn: %v", err)
	}
	if resuming && ws == theWorkspace {
		t.Fatal("the merge remediating out of line was resumed before its fixing turn ended")
	}
}

func TestDequeuingARemediatingMergeDropsItsEntryAndItsLease(t *testing.T) {
	// Arrange
	c := stepAsideScenario(t)

	// Act
	if err := c.h.o.dequeue(context.Background(), theWorkspace, CauseUserDequeue); err != nil {
		t.Fatalf("dequeue: %v", err)
	}

	// Assert
	queue, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if slices.ContainsFunc(queue, func(e wsm.MergeQueueEntry) bool { return e.Workspace == theWorkspace }) {
		t.Fatalf("queue = %+v, want the remediating merge gone", queue)
	}
	c.h.db.mu.Lock()
	_, held := c.h.db.leases[theWorkspace]
	c.h.db.mu.Unlock()
	if held {
		t.Fatal("the dequeued remediating merge still holds its lease")
	}
}

func TestARemediatingMergeReturnsOnceItsFixingTurnEnded(t *testing.T) {
	// Arrange: the failed head remediating; its fixing turn is recorded.
	c := stepAsideScenario(t)
	c.h.o.mu.Lock()
	doc := c.h.o.resumes[theWorkspace]
	c.h.o.mu.Unlock()

	// Act
	c.h.o.waitThenReturn(context.Background(), theWorkspace, c.h.repoKey(), ids.TurnID(doc.Turn))

	// Assert
	queue, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if !slices.ContainsFunc(inLine(queue), func(e wsm.MergeQueueEntry) bool { return e.Workspace == theWorkspace }) {
		t.Fatalf("line = %+v, want the remediated merge back in line", inLine(queue))
	}
}

func TestTheRosterIsToldTheMergeRemediatingOutOfLine(t *testing.T) {
	// Arrange + Act
	c := stepAsideScenario(t)
	if err := c.h.o.republishQueue(context.Background(), c.h.repoKey()); err != nil {
		t.Fatalf("republishQueue: %v", err)
	}

	// Assert
	queue := c.h.sidebar.queueOf(c.h.repoKey())
	if len(queue.Remediating) != 1 || queue.Remediating[0].Workspace != theWorkspace || queue.Remediating[0].RemediatingSince.IsZero() {
		t.Fatalf("remediating = %+v, want the failed head with when it stepped aside", queue.Remediating)
	}
	if slices.ContainsFunc(queue.Line, func(e sidebar.QueueEntry) bool { return e.Workspace == theWorkspace }) {
		t.Fatalf("line = %+v, want the remediating merge out of it", queue.Line)
	}
}
