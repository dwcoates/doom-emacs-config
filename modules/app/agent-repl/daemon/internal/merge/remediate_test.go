package merge

import (
	"context"
	"errors"
	"slices"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/dlog"
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
	return stepAsideScenarioWith(t, func(*chain) {})
}

// stepAsideScenarioWith is stepAsideScenario, with ARRANGE run on the chain
// before the failing head is admitted.
func stepAsideScenarioWith(t *testing.T, arrange func(*chain)) *chain {
	t.Helper()
	c := newChainOfWidth(t, 1)
	arrange(c)
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

func TestAMergeRacingBehindAFailedHeadKeepsItsRunningTestsAndLands(t *testing.T) {
	// Arrange: on the two-merge window, ws-b tests behind the head; the
	// head's suite fails.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	c.runner.script(c.dirs[theWorkspace], failingSuiteRun)

	// Act
	c.drainUntil(t, "ws-b landing", func() bool { return len(c.git.landedOwn()) == 1 })

	// Assert
	if got := c.git.landedOwn(); !slices.Equal(got[0], []string{"b1"}) {
		t.Fatalf("first landing brought %v, want b1", got[0])
	}
	if n := c.runner.ran(c.dirs["ws-b"]); n != 1 {
		t.Fatalf("ws-b's tests ran %d times, want once: the head stepping aside must not cut them", n)
	}
}

func TestASpeculativeMergeFirstInItsChainStandsOnTheTargetsTip(t *testing.T) {
	// Arrange: a speculative run alone in its chain, its head gone.
	h := newHarness(t)
	r := &run{o: h.o, ws: theWorkspace, repo: h.repoKey(), speculative: true}
	h.o.mu.Lock()
	h.o.chains[h.repoKey()] = []*run{r}
	h.o.targetTips[h.repoKey()] = "tip-1"
	h.o.mu.Unlock()

	// Act
	place := h.o.chainPosition(r)

	// Assert
	if place.base != "tip-1" {
		t.Fatalf("base = %q, want the target's tip, tip-1", place.base)
	}
}

func TestASuspensionWithdrawsTheWaitOnAFixingTurnRatherThanReturningTheMerge(t *testing.T) {
	// Arrange: the failed head remediating, its wait armed.
	c := stepAsideScenario(t)
	c.h.o.mu.Lock()
	_, armed := c.h.o.remediating[theWorkspace]
	c.h.o.mu.Unlock()
	if !armed {
		t.Fatal("no wait armed for the remediating merge")
	}

	// Act: its workspace is handed on.
	if err := c.h.o.suspendFor(context.Background(), theWorkspace, suspendTransfer); err != nil {
		t.Fatalf("suspendFor: %v", err)
	}

	// Assert
	c.h.o.mu.Lock()
	_, still := c.h.o.remediating[theWorkspace]
	c.h.o.mu.Unlock()
	queue, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if still || !slices.ContainsFunc(queue, func(e wsm.MergeQueueEntry) bool {
		return e.Workspace == theWorkspace && e.State == wsm.MergeRemediating
	}) {
		t.Fatalf("wait still armed=%v, queue %+v; want the wait withdrawn and the merge still remediating", still, queue)
	}
}

func TestAHeadThatStepsAsideNeverRunsItsTestsAgainBeforeItReturns(t *testing.T) {
	// Arrange: the step aside's goroutine is held, and the fake's fixing turn
	// ends at once -- so only a run cut before it reads that end stops.
	c := newChainOfWidth(t, 1)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	c.runner.script(c.dirs[theWorkspace], failingSuiteRun)
	held := make(chan struct{})
	c.h.o.onStepAside = func(ids.WorkspaceID) { <-held }

	// Act
	err := receiveWithin(t, admitAsync(c.h, context.Background()), "the failing head's admission ending")
	ran := c.runner.ran(c.dirs[theWorkspace])
	close(held)

	// Assert
	if err != nil {
		t.Fatalf("the failing head's admission ended on: %v", err)
	}
	if ran != 1 {
		t.Fatalf("the failed head's tests ran %d times before its step aside, want once: its run went on", ran)
	}
}

func TestAHeadSteppingAsideIsCutBeforeItWaitsForItsFixingTurn(t *testing.T) {
	// Arrange: every wait on a turn's end records how its context stood when
	// the wait began, then ends at once, as a fixing turn that ended at once.
	var mu sync.Mutex
	causes := map[ids.TurnID]error{}
	c := stepAsideScenarioWith(t, func(c *chain) {
		await := c.h.o.deps.AwaitTurnEnd
		c.h.o.deps.AwaitTurnEnd = func(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (wsm.TurnClose, error) {
			mu.Lock()
			if _, seen := causes[turn]; !seen {
				causes[turn] = context.Cause(ctx)
			}
			mu.Unlock()
			return await(ctx, ws, turn)
		}
	})

	// Act
	c.h.o.mu.Lock()
	doc := c.h.o.resumes[theWorkspace]
	c.h.o.mu.Unlock()

	// Assert
	mu.Lock()
	cause, waited := causes[ids.TurnID(doc.Turn)]
	mu.Unlock()
	if !waited || !errors.Is(cause, errSteppedAside) {
		t.Fatalf("the fixing turn's wait began with cause %v (waited %v), want it already cut by the step aside", cause, waited)
	}
	if got := c.git.landedOwn(); len(got) != 0 {
		t.Fatalf("landings brought %v, want none: the head stepped aside", got)
	}
}

// stepAsideRun answers a run of the harness's merge, its entry admitted at
// the head, and the cause its context was cut with.
func stepAsideRun(t *testing.T, h *harness, state wsm.MergeQueueState) (*run, func() error) {
	t.Helper()
	enqueue(t, h)
	h.db.mu.Lock()
	for i := range h.db.queues[h.repoKey()] {
		h.db.queues[h.repoKey()][i].State = state
	}
	h.db.mu.Unlock()
	ctx, cancel := context.WithCancelCause(context.Background())
	t.Cleanup(func() { cancel(nil) })
	r := h.o.newRun(ctx, cancel, h.repoKey(), theWorkspace, 1)
	return r, func() error { return context.Cause(ctx) }
}

// entryState answers the state of the harness's merge's entry.
func entryState(t *testing.T, h *harness) wsm.MergeQueueState {
	t.Helper()
	entry, err := h.o.entryOf(context.Background(), h.repoKey(), theWorkspace)
	if err != nil {
		t.Fatalf("entryOf: %v", err)
	}
	return entry.State
}

func TestAStepAsideLeavesARunAnotherEndTookFirstToThatEnd(t *testing.T) {
	cases := []struct {
		name string
		took func(o *orchestrator, r *run)
	}{
		{name: "a pause suspended the run first", took: func(o *orchestrator, r *run) { r.suspendLocked(suspendPause) }},
		{name: "a dequeue abandoned the run first", took: func(o *orchestrator, r *run) { r.abandonedBy = CauseUserDequeue }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			r, cause := stepAsideRun(t, h, wsm.MergeAdmitted)
			h.o.mu.Lock()
			tc.took(h.o, r)
			h.o.mu.Unlock()

			// Act
			h.o.stepAside(r)

			// Assert
			if err := cause(); err != nil {
				t.Fatalf("the run was cut with %v, want it left to the end that took it", err)
			}
			h.o.mu.Lock()
			why, away := h.o.takenAway[theWorkspace]
			h.o.mu.Unlock()
			if away && why.by == suspendStepAside.by {
				t.Fatal("the step aside marked the run taken away, want it left to the end that took it")
			}
			if got := entryState(t, h); got != wsm.MergeRemediating {
				t.Fatalf("entry state = %v, want remediating for that end to read", got)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opRemediate,
				Message: "the failing head stepped aside as another end took its run; that end settles the merge remediating"}); !ok {
				t.Fatal("no INFO record of the step aside left to another end")
			}
		})
	}
}

func TestAStepAsideThatCannotLeaveTheLineFixesAtTheHead(t *testing.T) {
	// Arrange: the entry is not admitted, so the store refuses the step aside.
	h := newHarness(t)
	r, cause := stepAsideRun(t, h, wsm.MergeQueued)

	// Act
	h.o.stepAside(r)

	// Assert
	if err := cause(); err != nil {
		t.Fatalf("the run was cut with %v, want it fixing at the head", err)
	}
	h.o.mu.Lock()
	_, away := h.o.takenAway[theWorkspace]
	suspending := r.suspending
	h.o.mu.Unlock()
	if away || suspending {
		t.Fatalf("taken away %v, suspending %v, want neither", away, suspending)
	}
	if got := entryState(t, h); got != wsm.MergeQueued {
		t.Fatalf("entry state = %v, want it unchanged", got)
	}
	record, ok := h.recordFor("error", opRemediate)
	if !ok || record.Context["workspace"] != string(theWorkspace) || record.Context["error"] == nil {
		t.Fatalf("ERROR record = %+v (found %v), want one naming the workspace and the error", record, ok)
	}
}

func TestATakeBackAfterAStepAsideRecordsWhatFailed(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(h *harness, r *run)
		message string
	}{
		{name: "the run never lets go", arrange: func(h *harness, r *run) { h.o.drainBound = time.Nanosecond },
			message: "the failing head that stepped aside did not let go"},
		{name: "the take-back cannot read the queues", arrange: func(h *harness, r *run) {
			close(r.finished)
			h.db.mu.Lock()
			h.db.allQueuesErr = errors.New("store gone")
			h.db.mu.Unlock()
		}, message: "could not take back the merge that stepped aside to remediate"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			r, _ := stepAsideRun(t, h, wsm.MergeRemediating)
			tc.arrange(h, r)

			// Act
			h.o.takeBackSteppedAside(r, dlog.Context{"workspace": string(theWorkspace)})

			// Assert
			record, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediate, Message: tc.message})
			if !ok || record.Context["workspace"] != string(theWorkspace) || record.Context["error"] == nil {
				t.Fatalf("ERROR record = %+v (found %v), want %q naming the workspace and the error", record, ok, tc.message)
			}
		})
	}
}
