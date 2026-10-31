package merge

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"slices"
	"strings"
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
	return causedRun(t, h)
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
	suspending, left := r.suspending, r.leftLine
	h.o.mu.Unlock()
	if away || suspending || left {
		t.Fatalf("taken away %v, suspending %v, marked out of line %v, want none", away, suspending, left)
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

// offFront is the record of a chain checked against a line it does not
// stand at the front of (extendOnce).
var offFront = logLike{level: "error", op: "daemon.merge.chain", text: "does not stand at the front of its queue"}

// TestAStepAsideWaitsForTheChainDecisionThatReadItsLine covers the 2026-10-31
// interleaving of TestTwoMergesOfARepositoryAlternateTheTestTurnAndTheirGatesNeverOverlap:
// the chain read its line with the failing head still admitted first, the
// head's step aside then took it out of line, and the chain checked that read
// against a chain that no longer held the head. The step aside's write waits
// for the decision that read the line.
func TestAStepAsideWaitsForTheChainDecisionThatReadItsLine(t *testing.T) {
	// Arrange: ws-a heads ws-b, both admitted in line. The chain's queue read
	// starts ws-a's step aside and returns only once its write waits for the
	// line; the paused read, still inside the decision, records the line.
	h := resumeWorld(t, []wsm.MergeQueueEntry{admittedEntry("ws-a"), admittedEntry("ws-b")})
	runs := standChain(t, h, []ids.WorkspaceID{"ws-a", "ws-b"}, "")
	waiting := make(chan struct{})
	var waitOnce, readOnce sync.Once
	h.o.onLineWait = func(wsm.RepoKey) { waitOnce.Do(func() { close(waiting) }) }
	stepped := make(chan error, 1)
	h.db.onQueueRead = func() {
		readOnce.Do(func() {
			go func() {
				stepped <- h.o.leaveLine(runs[0], func() error {
					return h.db.StepAsideMerge(context.Background(), h.repoKey(), "ws-a")
				})
			}()
			<-waiting
		})
	}
	var duringDecision []wsm.MergeQueueEntry
	h.db.onPausedRead = func() {
		h.db.mu.Lock()
		duringDecision = append([]wsm.MergeQueueEntry(nil), h.db.queues[h.repoKey()]...)
		h.db.mu.Unlock()
	}

	// Act
	h.o.extendOnce(context.Background(), h.repoKey())
	err := receiveWithin(t, stepped, "the step aside's line write")

	// Assert
	if err != nil {
		t.Fatalf("the step aside's line write = %v, want nil", err)
	}
	if len(duringDecision) == 0 || duringDecision[0].Workspace != "ws-a" || duringDecision[0].State != wsm.MergeAdmitted {
		t.Fatalf("the line inside the chain's decision = %+v, want ws-a still admitted first: the step aside wrote under it", duringDecision)
	}
	if loggedLike(h, offFront) {
		t.Fatal("the chain was recorded as off its queue's front")
	}
	queue, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if !slices.ContainsFunc(queue, func(e wsm.MergeQueueEntry) bool { return e.Workspace == "ws-a" && e.State == wsm.MergeRemediating }) {
		t.Fatalf("queue %+v, want ws-a remediating once the decision let the step aside write", queue)
	}
}

// TestALineReadAfterAStepAsideMeetsTheHeadMarked covers the read on the far
// side of the step aside's write: the head, still in its chain until it
// releases its slot, is marked as having left its line, so the chain agrees
// with a line that no longer holds it.
func TestALineReadAfterAStepAsideMeetsTheHeadMarked(t *testing.T) {
	// Arrange
	h := resumeWorld(t, []wsm.MergeQueueEntry{admittedEntry("ws-a"), admittedEntry("ws-b")})
	runs := standChain(t, h, []ids.WorkspaceID{"ws-a", "ws-b"}, "")
	if err := h.o.leaveLine(runs[0], func() error {
		return h.db.StepAsideMerge(context.Background(), h.repoKey(), "ws-a")
	}); err != nil {
		t.Fatalf("leaveLine: %v", err)
	}

	// Act
	h.o.extendOnce(context.Background(), h.repoKey())

	// Assert
	if loggedLike(h, offFront) {
		t.Fatal("the chain was recorded as off its queue's front")
	}
}

// TestLeaveLine covers the mark and the lock leaveLine gives every write that
// takes a run's entry out of its line.
func TestLeaveLine(t *testing.T) {
	refused := errors.New("refused")
	cases := []struct {
		name     string
		writeErr error
		wantLeft bool
	}{
		{name: "a write that took the entry out leaves the run marked", wantLeft: true},
		{name: "a write that failed takes the mark back and answers its error", writeErr: refused},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := resumeWorld(t, []wsm.MergeQueueEntry{admittedEntry("ws-a")})
			runs := standChain(t, h, []ids.WorkspaceID{"ws-a"}, "")
			var markedAtWrite, lockedAtWrite bool

			// Act
			err := h.o.leaveLine(runs[0], func() error {
				h.o.mu.Lock()
				markedAtWrite = runs[0].leftLine
				h.o.mu.Unlock()
				line := h.o.lineLock(h.repoKey())
				if line.TryLock() {
					line.Unlock()
				} else {
					lockedAtWrite = true
				}
				return tc.writeErr
			})

			// Assert
			if !errors.Is(err, tc.writeErr) || (tc.writeErr == nil) != (err == nil) {
				t.Fatalf("leaveLine = %v, want %v", err, tc.writeErr)
			}
			if !markedAtWrite || !lockedAtWrite {
				t.Fatalf("at its write the run was marked=%v under the line lock=%v, want both", markedAtWrite, lockedAtWrite)
			}
			h.o.mu.Lock()
			left := runs[0].leftLine
			h.o.mu.Unlock()
			if left != tc.wantLeft {
				t.Fatalf("the run's mark after the write = %v, want %v", left, tc.wantLeft)
			}
		})
	}
}

// TestAStepAsideTakesTheEntryOutOfLineBeforeItCutsTheRun covers stepAside's
// order: the head's entry leaves the line, marked, while its run still stands
// uncut, so the run never releases its slot ahead of its entry.
func TestAStepAsideTakesTheEntryOutOfLineBeforeItCutsTheRun(t *testing.T) {
	// Arrange: the take-back is held until the test ends.
	h := newHarness(t)
	r, cause := stepAsideRun(t, h, wsm.MergeAdmitted)
	held := make(chan struct{})
	t.Cleanup(func() { close(held); h.o.live.Wait() })
	h.o.onStepAside = func(ids.WorkspaceID) { <-held }
	var cutAtWrite, markedAtWrite bool
	h.db.beforeStepAside = func(ids.WorkspaceID) {
		cutAtWrite = cause() != nil
		h.o.mu.Lock()
		markedAtWrite = r.leftLine
		h.o.mu.Unlock()
	}

	// Act
	h.o.stepAside(r)

	// Assert
	if cutAtWrite || !markedAtWrite {
		t.Fatalf("at the line write the run was cut=%v marked=%v, want uncut and marked", cutAtWrite, markedAtWrite)
	}
	if !errors.Is(cause(), errSteppedAside) {
		t.Fatalf("the run's cause after the step aside = %v, want errSteppedAside", cause())
	}
}

// TestEveryWriteThatMovesTheChainsEntriesHoldsTheLineLock covers the shape
// every such write shares: it runs inside a lineWrite or leaveLine closure,
// so no hand-rolled write moves the line under a decision that read it.
func TestEveryWriteThatMovesTheChainsEntriesHoldsTheLineLock(t *testing.T) {
	// Arrange: the writes that move a chain member's entry, and the files
	// whose every call of them must hold the lock (nil: every file).
	guarded := map[string][]string{
		"StepAsideMerge":        nil,
		"ReturnRemediatedMerge": nil,
		"YieldMergeHead":        nil,
		"RemoveMergeQueueEntry": {"terminal.go"},
	}
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("listing the package's files: %v", err)
	}
	fset := token.NewFileSet()
	seen := map[string]int{}
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		parsed, err := parser.ParseFile(fset, file, nil, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", file, err)
		}

		// Act: walk every call, tracking the lock closures it stands in.
		var stack []ast.Node
		ast.Inspect(parsed, func(n ast.Node) bool {
			if n == nil {
				stack = stack[:len(stack)-1]
				return true
			}
			stack = append(stack, n)
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			only, guardedCall := guarded[sel.Sel.Name]
			if !guardedCall || only != nil && !slices.Contains(only, file) || forwards(stack, sel.Sel.Name) {
				return true
			}
			seen[sel.Sel.Name]++

			// Assert
			if !underLineLock(stack) {
				t.Errorf("%s: %s is called outside a lineWrite or leaveLine closure", fset.Position(call.Pos()), sel.Sel.Name)
			}
			return true
		})
	}
	for name := range guarded {
		if seen[name] == 0 {
			t.Errorf("no call of %s was found; the guarded set is stale", name)
		}
	}
}

// forwards reports whether the innermost node of STACK stands in a method
// named NAME: a store decorator (rosterQueueDB, queueEventsDB) handing the
// write on to the store it wraps, which its caller's lock already covers.
func forwards(stack []ast.Node, name string) bool {
	for _, n := range stack {
		if decl, ok := n.(*ast.FuncDecl); ok {
			return decl.Recv != nil && decl.Name.Name == name
		}
	}
	return false
}

// underLineLock reports whether the innermost node of STACK stands inside a
// function literal passed to lineWrite or leaveLine.
func underLineLock(stack []ast.Node) bool {
	for i := len(stack) - 2; i > 0; i-- {
		lit, ok := stack[i].(*ast.FuncLit)
		if !ok {
			continue
		}
		call, ok := stack[i-1].(*ast.CallExpr)
		if !ok || !slices.Contains(call.Args, ast.Expr(lit)) {
			continue
		}
		if sel, ok := call.Fun.(*ast.SelectorExpr); ok && (sel.Sel.Name == "lineWrite" || sel.Sel.Name == "leaveLine") {
			return true
		}
	}
	return false
}
