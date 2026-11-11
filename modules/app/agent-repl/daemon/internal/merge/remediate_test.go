package merge

import (
	"context"
	"encoding/json"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"slices"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/sourcescan"
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
	c := newChain(t)
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
	c := newChain(t)
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
	// Arrange: the failed head holds the fixing slot; its brief is recorded
	// and was never handed its session.
	c := stepAsideScenario(t)
	c.h.o.mu.Lock()
	doc := c.h.o.resumes[theWorkspace]
	c.h.o.mu.Unlock()

	// Act
	c.h.o.waitThenReturn(context.Background(), theWorkspace, c.h.repoKey(), doc)

	// Assert
	queue, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if !slices.ContainsFunc(inLine(queue), func(e wsm.MergeQueueEntry) bool { return e.Workspace == theWorkspace }) {
		t.Fatalf("line = %+v, want the remediated merge back in line", inLine(queue))
	}
	if n := c.prompts(theWorkspace, fixOrigin); n != 1 {
		t.Fatalf("fixing prompts = %d, want the recorded brief delivered once by the slot's wait", n)
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
	if len(queue.Remediating) != 1 || queue.Remediating[0].Workspace != theWorkspace || queue.Remediating[0].SlotSince.IsZero() {
		t.Fatalf("remediating = %+v, want the failed head with when it stepped aside", queue.Remediating)
	}
	if slices.ContainsFunc(queue.Line, func(e sidebar.QueueEntry) bool { return e.Workspace == theWorkspace }) {
		t.Fatalf("line = %+v, want the remediating merge out of it", queue.Line)
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
	c := newChain(t)
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

func TestAHeadThatStepsAsideHandsItsSessionNothingAndRecordsItsBrief(t *testing.T) {
	// Arrange + Act: the failing head stepped aside with ws-b behind it.
	c := stepAsideScenario(t)

	// Assert
	if n := c.prompts(theWorkspace, fixOrigin); n != 0 {
		t.Fatalf("fixing prompts = %d, want none: the slot's wait delivers the brief, never the run", n)
	}
	stored, found, err := c.h.db.MergeProgressOf(context.Background(), theWorkspace)
	if err != nil || !found {
		t.Fatalf("MergeProgressOf = %v, %v; want the record", found, err)
	}
	doc, err := decodeProgress(stored)
	if err != nil {
		t.Fatalf("decodeProgress: %v", err)
	}
	if doc.Step != TabFixes || doc.Turn == "" || doc.Brief == "" {
		t.Fatalf("record = step %q turn %q brief %q, want the fixing step, its turn and its brief", doc.Step, doc.Turn, doc.Brief)
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
			h.o.stepAside(context.Background(), r, AsideForTests)

			// Assert
			if err := cause(); err != nil {
				t.Fatalf("the run was cut with %v, want it left to the end that took it", err)
			}
			h.o.mu.Lock()
			why, away := h.o.takenAway[theWorkspace]
			h.o.mu.Unlock()
			if away && why.by == suspendAsideForTests.by {
				t.Fatal("the step aside marked the run taken away, want it left to the end that took it")
			}
			if got := entryState(t, h); got != wsm.MergeRemediating {
				t.Fatalf("entry state = %v, want remediating for that end to read", got)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opRemediate,
				Message: "the merge stepped aside as another end took its run; that end settles the merge remediating"}); !ok {
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
	h.o.stepAside(context.Background(), r, AsideForTests)

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
			message: "the merge that stepped aside did not let go"},
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
			h.o.takeBackSteppedAside(r, suspendAsideForTests, dlog.Context{"workspace": string(theWorkspace)})

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
					_, err := h.db.StepAsideMerge(context.Background(), h.repoKey(), "ws-a", wsm.MergeAsideTests)
					return err
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
		_, err := h.db.StepAsideMerge(context.Background(), h.repoKey(), "ws-a", wsm.MergeAsideTests)
		return err
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
	h.o.stepAside(context.Background(), r, AsideForTests)

	// Assert
	if cutAtWrite || !markedAtWrite {
		t.Fatalf("at the line write the run was cut=%v marked=%v, want uncut and marked", cutAtWrite, markedAtWrite)
	}
	if !errors.Is(cause(), errAsideForTests) {
		t.Fatalf("the run's cause after the step aside = %v, want errAsideForTests", cause())
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
		"RemoveMergeQueueEntry": {"terminal.go"},
	}
	fset := token.NewFileSet()
	seen := map[string]int{}
	for _, source := range sourcescan.Production(t) {
		file := source.Name
		parsed, err := parser.ParseFile(fset, file, source.Source, 0)
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

// ---- Conflict step aside (owner request, 2026-11-03) ----

// keepMergeTurns records every merge prompt's turn as the session's, so a
// resumed run reattaches to the turn it handed off rather than delivering it
// again, as a live session's turn store says.
func (c *chain) keepMergeTurns() {
	c.h.queue.mu.Lock()
	defer c.h.queue.mu.Unlock()
	c.h.queue.onSubmit = func(sub promptqueue.Submission) {
		c.h.db.mu.Lock()
		c.h.db.turns[sub.WS] = append(c.h.db.turns[sub.WS], wsm.Turn{ID: sub.Turn, Workspace: sub.WS})
		c.h.db.mu.Unlock()
	}
}

// steppedAside answers a channel closed once ws's step aside begins its
// take-back (onStepAside).
func (c *chain) steppedAside(ws ids.WorkspaceID) <-chan struct{} {
	aside := make(chan struct{})
	var once sync.Once
	c.h.o.onStepAside = func(stepped ids.WorkspaceID) {
		if stepped == ws {
			once.Do(func() { close(aside) })
		}
	}
	return aside
}

// settle waits for every goroutine the orchestrator started to end: a step
// aside's take-back among them.
func (c *chain) settle(t *testing.T, what string) {
	t.Helper()
	settled := make(chan struct{})
	go func() { c.h.o.live.Wait(); close(settled) }()
	receiveWithin(t, settled, what)
}

// conflictAsideScenario runs a head whose rebase conflicts with what master
// moved to (m1) with ws-b in line behind it, on a one-merge window, until the
// head stepped aside and its step aside settled.
func conflictAsideScenario(t *testing.T) *chain {
	t.Helper()
	return conflictAsideScenarioWith(t, func(*chain) {})
}

// conflictAsideScenarioWith is conflictAsideScenario, with ARRANGE run on the
// chain before the conflicting head is admitted.
func conflictAsideScenarioWith(t *testing.T, arrange func(*chain)) *chain {
	t.Helper()
	c := newChain(t)
	arrange(c)
	c.keepMergeTurns()
	c.git.commitOn("master", "m1")
	c.git.conflict("a1", "m1", "x.go")
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	if err := receiveWithin(t, admitAsync(c.h, context.Background()), "the conflicting head's admission ending"); err != nil {
		t.Fatalf("the conflicting head's admission ended on: %v", err)
	}
	c.settle(t, "the conflict step aside settling")
	return c
}

// entryOf answers ws's entry in the chain's queue, and whether it stands.
func (c *chain) entryOf(t *testing.T, ws ids.WorkspaceID) (wsm.MergeQueueEntry, bool) {
	t.Helper()
	queue, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	at := slices.IndexFunc(queue, func(e wsm.MergeQueueEntry) bool { return e.Workspace == ws })
	if at < 0 {
		return wsm.MergeQueueEntry{}, false
	}
	return queue[at], true
}

// lineOrder answers the workspaces in line, front first.
func (c *chain) lineOrder(t *testing.T) []ids.WorkspaceID {
	t.Helper()
	queue, err := c.h.db.MergeQueue(context.Background(), c.h.repoKey())
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	var out []ids.WorkspaceID
	for _, e := range inLine(queue) {
		out = append(out, e.Workspace)
	}
	return out
}

// steppedAsideFor counts the records of ws stepping aside for its conflicts.
func (c *chain) steppedAsideFor(ws ids.WorkspaceID, why AsideReason) int {
	n := 0
	for _, rec := range c.h.logs.Records() {
		if rec.Level == "info" && rec.Operation == opRemediate && rec.Message == why.suspension().moved &&
			rec.Context["workspace"] == string(ws) && rec.Context["reason"] == why.String() {
			n++
		}
	}
	return n
}

func TestAHeadWhoseRebaseConflictsStepsAsideOutOfLine(t *testing.T) {
	// Arrange + Act
	c := conflictAsideScenario(t)

	// Assert
	entry, ok := c.entryOf(t, theWorkspace)
	if !ok || entry.State != wsm.MergeRemediating {
		t.Fatalf("entry = %+v (standing %v), want the conflicting head remediating", entry, ok)
	}
	if got := c.lineOrder(t); !slices.Equal(got, []ids.WorkspaceID{"ws-b"}) {
		t.Fatalf("line = %v, want only ws-b in line", got)
	}
	if got := c.prompts(theWorkspace, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 0 {
		t.Fatalf("conflict prompts = %d, want none: the slot's wait delivers the resolving brief", got)
	}
	if entry.Aside != wsm.MergeAsideConflicts || entry.Slot != wsm.MergeSlotResolvingConflicts {
		t.Fatalf("entry = %+v, want it holding the resolving-conflicts slot", entry)
	}
	if got := c.steppedAsideFor(theWorkspace, AsideForConflicts); got != 1 {
		t.Fatalf("conflict step aside records = %d, want 1", got)
	}
}

func TestAConflictStepAsideIsShownAsRemediatingItsConflicts(t *testing.T) {
	// Arrange
	c := conflictAsideScenario(t)

	// Act
	snapshot, err := c.h.o.readQueue(context.Background(), c.h.o.deps.DB, c.h.repoKey())
	if err == nil {
		err = c.h.o.republishQueue(context.Background(), c.h.repoKey())
	}

	// Assert
	if err != nil {
		t.Fatalf("reading the queue: %v", err)
	}
	if len(snapshot.Remediating) != 1 || snapshot.Remediating[0].Repair != sidebar.RepairResolvingConflicts {
		t.Fatalf("remediating = %+v, want the head holding the resolving-conflicts slot", snapshot.Remediating)
	}
	roster := c.h.sidebar.queueOf(c.h.repoKey()).Remediating
	if len(roster) != 1 || roster[0].Repair != sidebar.RepairResolvingConflicts {
		t.Fatalf("roster remediating = %+v, want the head drawn resolving its conflicts", roster)
	}
}

func TestTheMergeBehindAConflictingHeadTakesTheHeadAndLandsFirst(t *testing.T) {
	// Arrange
	c := conflictAsideScenario(t)

	// Act
	c.drainUntil(t, "ws-b landing", func() bool { return len(c.git.landedOwn()) == 1 })
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("returnRemediated: %v", err)
	}
	c.drain(t)

	// Assert
	got := c.git.landedOwn()
	if len(got) != 2 || !slices.Equal(got[0], []string{"b1"}) || !slices.Equal(got[1], []string{"a1"}) {
		t.Fatalf("landings brought %v, want b1 then a1", got)
	}
	if n := c.prompts(theWorkspace, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); n != 1 {
		t.Fatalf("conflict prompts = %d, want 1: the return reads the resolving turn's end and rerere replays it onto the new tip", n)
	}
}

func TestAConflictingHeadWithNobodyBehindItResolvesInPlace(t *testing.T) {
	// Arrange: the head alone, its commit conflicting with master's move.
	c := newChain(t)
	c.keepMergeTurns()
	c.git.commitOn("master", "m1")
	c.git.conflict("a1", "m1", "x.go")
	enqueue(t, c.h)

	// Act
	c.drain(t)

	// Assert
	if got := c.git.landedOwn(); len(got) != 1 || !slices.Equal(got[0], []string{"a1"}) {
		t.Fatalf("landings brought %v, want a1 resolved in place", got)
	}
	if got := c.steppedAsideFor(theWorkspace, AsideForConflicts); got != 0 {
		t.Fatalf("conflict step aside records = %d, want none: nobody stood behind the head", got)
	}
}

func TestAMergeReturningFromAConflictStepAsideNeverEvictsTheRunningMerge(t *testing.T) {
	// Arrange: ws-b took the head and holds inside its gate; ws-c waits.
	c := conflictAsideScenario(t)
	c.queue(t, "ws-c", "c1")
	entered, release := c.runner.hold(c.dirs["ws-b"])
	done := admitAsync(c.h, context.Background())
	receiveWithin(t, entered, "ws-b's gate starting at the head")

	// Act
	err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey())
	order := c.lineOrder(t)
	close(release)

	// Assert
	if err != nil {
		t.Fatalf("returnRemediated: %v", err)
	}
	if want := []ids.WorkspaceID{"ws-b", theWorkspace, "ws-c"}; !slices.Equal(order, want) {
		t.Fatalf("line = %v, want %v: back at the first waiting place, behind the merge running", order, want)
	}
	if err := receiveWithin(t, done, "ws-b's admission ending"); err != nil {
		t.Fatalf("ws-b's admission ended on: %v", err)
	}
}

func TestAConflictStepAsideSpendsNoFixingAttempt(t *testing.T) {
	// Arrange: the head conflicts and steps aside; once it returns, its first
	// suite fails.
	c := conflictAsideScenario(t)
	c.runner.script(c.dirs[theWorkspace], failingSuiteRun)
	stored, _, _ := c.h.db.MergeProgressOf(context.Background(), theWorkspace)
	doc, err := decodeProgress(stored)
	if err != nil {
		t.Fatalf("decodeProgress: %v", err)
	}

	// Act
	c.drainUntil(t, "ws-b landing", func() bool { return len(c.git.landedOwn()) == 1 })
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("returnRemediated: %v", err)
	}
	c.drain(t)

	// Assert
	if doc.FixesUsed != 0 {
		t.Fatalf("the step aside's record counts %d fixing attempts used, want none", doc.FixesUsed)
	}
	if n := c.prompts(theWorkspace, fixOrigin); n != 1 {
		t.Fatalf("fixing prompts = %d, want its one fixing attempt still to spend", n)
	}
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[1], []string{"a1"}) {
		t.Fatalf("landings brought %v, want a1 to land once fixed", got)
	}
}

func TestAMergeThatConflictsAgainOnItsReturnStepsAsideAgain(t *testing.T) {
	// Arrange: a1 conflicts with master's m1; a2 conflicts with b1, which
	// lands while a1 is resolved out of line; ws-c waits behind.
	c := conflictAsideScenarioWith(t, func(c *chain) {
		c.git.commitOn("feature", "a2")
		c.git.conflict("a2", "b1", "y.go")
	})
	c.queue(t, "ws-c", "c1")
	c.drainUntil(t, "ws-b landing", func() bool { return len(c.git.landedOwn()) == 1 })

	// Act: the return rebases onto b1's landing, conflicts on a2 and steps
	// aside again; ws-c lands; the second return lands it.
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("the first returnRemediated: %v", err)
	}
	c.drainUntil(t, "ws-c landing", func() bool { return len(c.git.landedOwn()) == 2 })
	c.settle(t, "the second step aside settling")
	if err := c.h.o.returnRemediated(context.Background(), theWorkspace, c.h.repoKey()); err != nil {
		t.Fatalf("the second returnRemediated: %v", err)
	}
	c.drain(t)

	// Assert
	if got := c.steppedAsideFor(theWorkspace, AsideForConflicts); got != 2 {
		t.Fatalf("conflict step aside records = %d, want 2", got)
	}
	got := c.git.landedOwn()
	if len(got) != 3 || !slices.Equal(got[0], []string{"b1"}) || !slices.Equal(got[1], []string{"c1"}) || !slices.Equal(got[2], []string{"a1", "a2"}) {
		t.Fatalf("landings brought %v, want b1, c1, then a1 a2", got)
	}
}

func TestARestartMidConflictStepAsideTakesTheMergeBackRemediatingItsConflicts(t *testing.T) {
	// Arrange: the head stepped aside for its conflicts; the daemon restarts.
	c := conflictAsideScenario(t)
	c.h.restart(t)
	c.wire(c.h.o)

	// Act
	if err := c.h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}

	// Assert
	c.h.o.mu.Lock()
	_, armed := c.h.o.remediating[theWorkspace]
	c.h.o.mu.Unlock()
	if !armed {
		t.Fatal("the boot armed no wait on the resolving turn")
	}
	snapshot, err := c.h.o.readQueue(context.Background(), c.h.o.deps.DB, c.h.repoKey())
	if err != nil {
		t.Fatalf("readQueue: %v", err)
	}
	if len(snapshot.Remediating) != 1 || snapshot.Remediating[0].Repair != sidebar.RepairResolvingConflicts {
		t.Fatalf("remediating = %+v, want the merge still holding the resolving-conflicts slot", snapshot.Remediating)
	}
	if rec, ok := c.h.recordFor("info", "daemon.merge.recover"); !ok || rec.Context["reason"] != "conflicts" {
		t.Fatalf("recover record = %+v (found %v), want the take-back naming its reason", rec, ok)
	}
}

func TestStepsAside(t *testing.T) {
	cases := []struct {
		name    string
		line    []wsm.MergeQueueEntry
		head    bool
		why     AsideReason
		readErr error
		want    bool
	}{
		{name: "a head with a merge behind it steps aside for its tests", line: []wsm.MergeQueueEntry{admittedEntry("ws-a"), queuedEntry("ws-b")},
			head: true, why: AsideForTests, want: true},
		{name: "a head with a merge behind it steps aside for its conflicts", line: []wsm.MergeQueueEntry{admittedEntry("ws-a"), queuedEntry("ws-b")},
			head: true, why: AsideForConflicts, want: true},
		{name: "a head alone in line repairs in place", line: []wsm.MergeQueueEntry{admittedEntry("ws-a")},
			head: true, why: AsideForConflicts},
		{name: "a run that does not head its queue never steps aside", line: []wsm.MergeQueueEntry{admittedEntry("ws-a"), queuedEntry("ws-b")},
			why: AsideForConflicts},
		{name: "a queue that cannot be read keeps the head where it stands", line: []wsm.MergeQueueEntry{admittedEntry("ws-a"), queuedEntry("ws-b")},
			head: true, why: AsideForConflicts, readErr: errors.New("store gone")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := resumeWorld(t, tc.line)
			ctx, cancel := context.WithCancelCause(context.Background())
			t.Cleanup(func() { cancel(nil) })
			r := h.o.newRun(ctx, cancel, h.repoKey(), "ws-a", 1)
			if tc.head {
				h.o.mu.Lock()
				h.o.running[h.repoKey()] = r
				h.o.mu.Unlock()
			}
			h.db.mu.Lock()
			h.db.queueReadErr = tc.readErr
			h.db.mu.Unlock()

			// Act
			got := h.o.stepsAside(context.Background(), r, tc.why)

			// Assert
			if got != tc.want {
				t.Fatalf("stepsAside = %v, want %v", got, tc.want)
			}
			if tc.readErr == nil {
				return
			}
			rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediate,
				Message: "could not read the queue to decide whether the merge steps aside; it repairs where it stands"})
			if !ok || rec.Context["workspace"] != "ws-a" || rec.Context["reason"] != tc.why.String() || rec.Context["error"] == nil {
				t.Fatalf("ERROR record = %+v (found %v), want the workspace, the reason and the error", rec, ok)
			}
		})
	}
}

func TestAsideReasonOf(t *testing.T) {
	cases := []struct {
		step    string
		want    AsideReason
		wantErr bool
	}{
		{step: TabFixes, want: AsideForTests},
		{step: TabConflicts, want: AsideForConflicts},
		{step: TabTests, wantErr: true},
		{step: TabRebasing, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.step, func(t *testing.T) {
			// Act
			got, err := asideReasonOf(progressDoc{Step: tc.step})

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("asideReasonOf(%q) = (%v, %v), want (%v, error %v)", tc.step, got, err, tc.want, tc.wantErr)
			}
		})
	}
}

// putProgressAt stores a progress record of WS's merge at STEP.
func putProgressAt(t *testing.T, h *harness, ws ids.WorkspaceID, step string) {
	t.Helper()
	encoded, err := json.Marshal(progressDoc{Version: progressVersion, Step: step, Repo: string(h.repoKey()),
		Subject: subjectDoc{TargetDir: h.targetD}, Active: roundDoc{Kind: step, N: 1}})
	if err != nil {
		t.Fatalf("encoding the record: %v", err)
	}
	if err := h.db.PutMergeProgress(context.Background(), wsm.MergeProgress{Workspace: ws, Lease: deadLease, Document: encoded}); err != nil {
		t.Fatalf("PutMergeProgress: %v", err)
	}
}

func TestATakeBackOfARemediatingMergeWhoseRecordWaitsOnNoTurnIsRefused(t *testing.T) {
	// Arrange: a remediating entry whose record stands at its tests step.
	h := newHarness(t)
	enqueue(t, h)
	ctx := context.Background()
	if _, err := h.db.AcquireLeaseAs(ctx, theWorkspace, deadLease, wsm.HolderMerge, wsm.PolicyHold); err != nil {
		t.Fatalf("taking the lease: %v", err)
	}
	putProgressAt(t, h, theWorkspace, TabTests)
	entry, err := h.o.entryOf(ctx, h.repoKey(), theWorkspace)
	if err != nil {
		t.Fatalf("entryOf: %v", err)
	}
	entry.State, entry.Aside = wsm.MergeRemediating, wsm.MergeAsideTests

	// Act
	err = h.o.recoverRemediating(ctx, h.repoKey(), entry)

	// Assert
	if err == nil {
		t.Fatal("the take-back of a remediating merge waiting on no turn was accepted")
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediate,
		Message: "a merge remediating out of line has a progress record that waits on no repairing turn of its repair; it is not taken back"})
	if !ok || rec.Context["workspace"] != string(theWorkspace) || rec.Context["step"] != TabTests || rec.Context["error"] == nil {
		t.Fatalf("ERROR record = %+v (found %v), want the workspace, the step and the error", rec, ok)
	}
	h.o.mu.Lock()
	_, armed := h.o.remediating[theWorkspace]
	h.o.mu.Unlock()
	if armed {
		t.Fatal("a refused take-back armed a wait")
	}
}

// TestEveryStepAsideGoesThroughHandOff covers the one path both triggers
// share: stepAside is reached only through handOff, and handOff is called for
// every reason there is.
func TestEveryStepAsideGoesThroughHandOff(t *testing.T) {
	// Arrange
	fset := token.NewFileSet()
	reasons := map[string]int{}
	for _, source := range sourcescan.Production(t) {
		parsed, err := parser.ParseFile(fset, source.Name, source.Source, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", source.Name, err)
		}

		// Act
		var enclosing string
		ast.Inspect(parsed, func(n ast.Node) bool {
			if decl, ok := n.(*ast.FuncDecl); ok {
				enclosing = decl.Name.Name
			}
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			switch sel.Sel.Name {
			case "stepAside":
				// Assert
				if enclosing != "handOff" {
					t.Errorf("%s: stepAside is called from %s, not through handOff", fset.Position(call.Pos()), enclosing)
				}
			case "handOff":
				if len(call.Args) >= 2 {
					if id, ok := call.Args[1].(*ast.Ident); ok {
						reasons[id.Name]++
					}
				}
			}
			return true
		})
	}
	for _, why := range []string{"AsideForTests", "AsideForConflicts"} {
		if reasons[why] == 0 {
			t.Errorf("no step asks handOff to step aside for %s", why)
		}
	}
}

// count answers how many rows the feed has been pushed.
func (f *fakeFeed) count() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.rows)
}

// lastMergeOf answers the last head row pushed for WS's bubble.
func (f *fakeFeed) lastMergeOf(ws ids.WorkspaceID) *frontendv1.FeedMerge {
	f.mu.Lock()
	defer f.mu.Unlock()
	for i := len(f.rows) - 1; i >= 0; i-- {
		if merge := f.rows[i].Row.GetActivity().GetMerge(); merge != nil && f.rows[i].WS == ws {
			return merge
		}
	}
	return nil
}
