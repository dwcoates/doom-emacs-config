package sessioncontroller

import (
	"testing"

	protocolv1 "agentrepl/proto/protocol/v1"
)

// ---------------------------------------------------------------------------
// THE ASYNC DRAIN EDGE (asyncrefresh.go, phantomtask.go).
//
// A stale-shim roll deferred on detached work fires the moment that work
// drains. The edge has to be exact: reported once, when the set really goes
// from holding work to holding none, and never off an event that changed
// nothing — a lease fired twice races two restarts onto one session, and a
// lease never fired leaves a superseded shim running forever.
// ---------------------------------------------------------------------------

// drainConsumer is a bare consumer with no hooks: these tests are about the
// set's own transition, which is what the hook is fired FROM.
func drainConsumer() *consumer {
	return newConsumer("ws", "s", &fakePusher{}, &fakeApplier{}, nil,
		newFakeClearCompactStore(), emptyTurnAccountingStore{},
		func(string, ...any) {}, nil, nil, nil, nil, nil)
}

func taskStarted(id string) *protocolv1.Event {
	return &protocolv1.Event{Payload: &protocolv1.Event_TaskStarted{TaskStarted: &protocolv1.TaskStarted{TaskId: id}}}
}

func taskEnded(id string) *protocolv1.Event {
	return &protocolv1.Event{Payload: &protocolv1.Event_TaskEnded{TaskEnded: &protocolv1.TaskEnded{TaskId: id}}}
}

func TestTheLastTaskEndingDrainsTheLiveSet(t *testing.T) {
	// Arrange: one task running.
	c := drainConsumer()
	c.observeTaskLifecycle(taskStarted("agent-0"))

	// Act.
	drained := c.observeTaskLifecycle(taskEnded("agent-0"))

	// Assert: the edge a deferred roll is waiting for.
	if !drained {
		t.Fatal("the last task ending did not report a drain, so a deferred refresh would wait forever")
	}
}

func TestATaskEndingWithOthersStillRunningDoesNotDrain(t *testing.T) {
	// Arrange: two tasks running.
	c := drainConsumer()
	c.observeTaskLifecycle(taskStarted("agent-0"))
	c.observeTaskLifecycle(taskStarted("agent-1"))

	// Act.
	drained := c.observeTaskLifecycle(taskEnded("agent-0"))

	// Assert: work remains, so the lease must keep waiting.
	if drained {
		t.Fatal("a drain was reported while another task is still running; the refresh would cut live work")
	}
}

func TestARepeatedTaskEndDoesNotDrainASecondTime(t *testing.T) {
	// Arrange: a task that has already ended and drained the set.
	c := drainConsumer()
	c.observeTaskLifecycle(taskStarted("agent-0"))
	if !c.observeTaskLifecycle(taskEnded("agent-0")) {
		t.Fatal("arrange: the first end should have drained the set")
	}

	// Act: the same end is observed again (a replay, a re-fold).
	drained := c.observeTaskLifecycle(taskEnded("agent-0"))

	// Assert: the set did not change, so nothing may be reported off it.
	if drained {
		t.Fatal("a re-observed end reported a second drain; the lease would be claimed twice")
	}
}

func TestAnEndForAnUnknownTaskDoesNotDrainAnEmptySet(t *testing.T) {
	// Arrange: nothing has ever run on this session.
	c := drainConsumer()

	// Act.
	drained := c.observeTaskLifecycle(taskEnded("agent-never-started"))

	// Assert: an empty set that stays empty is not a transition.
	if drained {
		t.Fatal("an end for a task that was never open reported a drain out of an already-empty set")
	}
}

func TestATaskStartNeverReportsADrain(t *testing.T) {
	// Arrange.
	c := drainConsumer()

	// Act.
	drained := c.observeTaskLifecycle(taskStarted("agent-0"))

	// Assert: only an END can empty the set.
	if drained {
		t.Fatal("a task START reported a drain")
	}
}
