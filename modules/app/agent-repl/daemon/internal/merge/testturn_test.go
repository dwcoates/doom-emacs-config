package merge

import (
	"context"
	"errors"
	"os"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
)

// turnWorld is one repository's test turn with runs to take it, each a
// stand-alone run of the harness's orchestrator.
type turnWorld struct {
	h     *harness
	lined chan ids.WorkspaceID
}

func newTurnWorld(t *testing.T) *turnWorld {
	t.Helper()
	h := newHarness(t)
	w := &turnWorld{h: h, lined: make(chan ids.WorkspaceID, 16)}
	h.o.onTestTurnLine = func(ws ids.WorkspaceID) { w.lined <- ws }
	return w
}

// run makes a run of ws in the harness's repository, registered under name.
func (w *turnWorld) run(ws ids.WorkspaceID, name string) *run {
	w.h.register(ws, name)
	ctx, cancel := context.WithCancelCause(context.Background())
	w.h.t.Cleanup(func() { cancel(nil) })
	return w.h.o.newRun(ctx, cancel, w.h.repoKey(), ws)
}

// round is a tests round of r's for the wait to draw on.
func (w *turnWorld) round(r *run) tabRound {
	return tabRound{kind: TabTests, n: 1, started: time.UnixMilli(1000)}
}

// await asks for r's turn on its own goroutine, and answers the ask's end.
func (w *turnWorld) await(r *run) <-chan error {
	done := make(chan error, 1)
	go func() { done <- r.awaitTestTurn(r.ctx, w.round(r)) }()
	return done
}

// awaitLined waits, bounded, for ws to join the line.
func (w *turnWorld) awaitLined(t *testing.T, ws ids.WorkspaceID) {
	t.Helper()
	select {
	case got := <-w.lined:
		if got != ws {
			t.Fatalf("%s joined the line, want %s", got, ws)
		}
	case <-time.After(5 * time.Second):
		t.Fatalf("%s never joined the line", ws)
	}
}

// granted waits, bounded, for an ask to end, and answers how.
func granted(t *testing.T, done <-chan error) error {
	t.Helper()
	select {
	case err := <-done:
		return err
	case <-time.After(5 * time.Second):
		t.Fatal("the ask for the test turn never ended")
		return nil
	}
}

// holder answers the run holding the repository's turn, nil when none does.
func (w *turnWorld) holder() *run {
	w.h.o.mu.Lock()
	defer w.h.o.mu.Unlock()
	if line := w.h.o.testTurns[w.h.repoKey()]; line != nil {
		return line.holder
	}
	return nil
}

// recorded reports whether an INFO test-turn record holding text names ws.
func (w *turnWorld) recorded(ws ids.WorkspaceID, text string) bool {
	for _, r := range w.h.logs.Records() {
		if r.Level == "info" && r.Operation == "daemon.merge.test_turn" && strings.Contains(r.Message, text) && r.Context["workspace"] == string(ws) {
			return true
		}
	}
	return false
}

func TestAMergeTakesAFreeTestTurnAtOnce(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a := w.run("ws-a", "alpha")

	// Act.
	err := granted(t, w.await(a))

	// Assert.
	if err != nil || w.holder() != a {
		t.Fatalf("ask ended %v holding %v, want the turn taken at once", err, w.holder())
	}
	if !w.recorded("ws-a", "took its repository's test turn at once") {
		t.Fatal("taking the free turn was not recorded")
	}
}

func TestAMergeWaitsInLineWhileAnotherHoldsTheTurn(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a, b := w.run("ws-a", "alpha"), w.run("ws-b", "beta")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}

	// Act.
	done := w.await(b)
	w.awaitLined(t, "ws-b")

	// Assert.
	select {
	case err := <-done:
		t.Fatalf("B's ask ended (%v) while A held the turn", err)
	default:
	}
	if !w.recorded("ws-b", "joined the line") {
		t.Fatal("joining the line was not recorded")
	}
}

func TestTheWaitDrawsTheRunWaitingForTheTurnAndNamesItsHolder(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a, b := w.run("ws-a", "alpha"), w.run("ws-b", "beta")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}

	// Act.
	bDone := w.await(b)
	w.awaitLined(t, "ws-b")

	// Assert: the draw follows the line join on the waiter's goroutine, so
	// it is read once the holder's release has let the waiter go.
	w.h.o.releaseTestTurn(context.Background(), a, "passed")
	if err := granted(t, bDone); err != nil {
		t.Fatal(err)
	}
	waiting := lastRunWaiting(t, w.h, "ws-b")
	if waiting.GetHolder().GetWorkspaceName() != "alpha" || waiting.GetStartedAtMs() != 1000 {
		t.Fatalf("the waiting run drew %v, want alpha holding it from 1000", waiting)
	}
}

// lastRunWaiting is the last waiting-for-turn arm ws's run subtab drew.
func lastRunWaiting(t *testing.T, h *harness, ws ids.WorkspaceID) *frontendv1.FeedMergeTabTestsRunWaitingForTurn {
	t.Helper()
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	var found *frontendv1.FeedMergeTabTestsRunWaitingForTurn
	for _, row := range h.feed.rows {
		if w := row.Row.GetMergeTab().GetTestsRun().GetWaitingForTurn(); w != nil {
			found = w
		}
	}
	if found == nil {
		t.Fatalf("%s's run never drew itself waiting for the turn", ws)
	}
	return found
}

func TestGivingBackTheTurnPassesItToTheFrontOfTheLine(t *testing.T) {
	// Arrange: A holds; B then C join the line.
	w := newTurnWorld(t)
	a, b, c := w.run("ws-a", "alpha"), w.run("ws-b", "beta"), w.run("ws-c", "gamma")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}
	bDone := w.await(b)
	w.awaitLined(t, "ws-b")
	cDone := w.await(c)
	w.awaitLined(t, "ws-c")

	// Act.
	w.h.o.releaseTestTurn(context.Background(), a, "passed")

	// Assert.
	if err := granted(t, bDone); err != nil || w.holder() != b {
		t.Fatalf("B's ask ended %v, holder %v; want B first in line to hold it", err, w.holder())
	}
	select {
	case err := <-cDone:
		t.Fatalf("C's ask ended (%v) behind B", err)
	default:
	}
	if !w.recorded("ws-a", "it passes to the merge first in line") || !w.recorded("ws-b", "got its repository's test turn") {
		t.Fatal("the turn's passing was not recorded on both sides")
	}
}

func TestTwoMergesAlternateTheTurnWhileEachFixes(t *testing.T) {
	// Arrange: A holds the turn and B waits for it.
	w := newTurnWorld(t)
	a, b := w.run("ws-a", "alpha"), w.run("ws-b", "beta")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}
	bDone := w.await(b)
	w.awaitLined(t, "ws-b")

	// Act: A's run fails and gives the turn back, A fixes and asks again,
	// then B's run fails and gives the turn back.
	w.h.o.releaseTestTurn(context.Background(), a, "failed")
	if err := granted(t, bDone); err != nil {
		t.Fatal(err)
	}
	aDone := w.await(a)
	w.awaitLined(t, "ws-a")
	heldWhileAWaited := w.holder()
	w.h.o.releaseTestTurn(context.Background(), b, "failed")
	if err := granted(t, aDone); err != nil {
		t.Fatal(err)
	}

	// Assert.
	if heldWhileAWaited != b || w.holder() != a {
		t.Fatalf("holders went %v then %v, want B while A fixed, then A", heldWhileAWaited, w.holder())
	}
}

func TestTheLastMergeGivingBackTheTurnLeavesItFree(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a := w.run("ws-a", "alpha")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}

	// Act.
	w.h.o.releaseTestTurn(context.Background(), a, "passed")

	// Assert.
	if w.holder() != nil || !w.recorded("ws-a", "nobody is in line for it") {
		t.Fatalf("holder %v after the last give-back, want the turn free and recorded so", w.holder())
	}
}

func TestGivingBackATurnNeverHeldChangesNothing(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a, b := w.run("ws-a", "alpha"), w.run("ws-b", "beta")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}

	// Act.
	w.h.o.releaseTestTurn(context.Background(), b, "left its chain")

	// Assert.
	if w.holder() != a {
		t.Fatalf("holder %v, want A untouched", w.holder())
	}
}

func TestAMergeStoppedInLineLeavesIt(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a, b := w.run("ws-a", "alpha"), w.run("ws-b", "beta")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}
	bDone := w.await(b)
	w.awaitLined(t, "ws-b")
	stopped := errors.New("stopped by the test")

	// Act.
	b.cancel(stopped)
	err := granted(t, bDone)
	w.h.o.releaseTestTurn(context.Background(), a, "passed")

	// Assert.
	if !errors.Is(err, stopped) {
		t.Fatalf("B's ask ended %v, want its stop's cause", err)
	}
	if w.holder() != nil {
		t.Fatalf("the turn passed to %v, want nobody: B left the line", w.holder())
	}
	if !w.recorded("ws-b", "left the line") {
		t.Fatal("leaving the line was not recorded")
	}
}

func TestARunLeavingItsChainGivesBackItsTurnAndItsPlaceInLine(t *testing.T) {
	// Arrange: A holds and B waits; both are members of the chain.
	w := newTurnWorld(t)
	a, b, c := w.run("ws-a", "alpha"), w.run("ws-b", "beta"), w.run("ws-c", "gamma")
	w.h.o.mu.Lock()
	for _, r := range []*run{a, b, c} {
		r.joined = true
	}
	w.h.o.chains[w.h.repoKey()] = []*run{a, b, c}
	w.h.o.mu.Unlock()
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}
	// B stands in line with no ask of its own waiting: a run leaves its chain
	// only from its own goroutine, never while that goroutine waits in line.
	w.h.o.joinTestTurn(b)
	cDone := w.await(c)
	w.awaitLined(t, "ws-c")

	// Act: B leaves from its place in line, then A leaves holding the turn.
	w.h.o.releaseSlot(context.Background(), b)
	w.h.o.releaseSlot(context.Background(), a)

	// Assert.
	if err := granted(t, cDone); err != nil || w.holder() != c {
		t.Fatalf("C's ask ended %v, holder %v; want C to hold the turn", err, w.holder())
	}
}

func TestAMergeAskingTwiceForTheTurnIsADefect(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a := w.run("ws-a", "alpha")
	w.h.o.joinTestTurn(a)

	// Act / Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("a second ask for a held turn did not fail hard")
		}
	}()
	w.h.o.joinTestTurn(a)
}

func TestAWaitingRunReadingNoLineIsADefect(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a := w.run("ws-a", "alpha")

	// Act / Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("reading the place of a run in no line did not fail hard")
		}
	}()
	w.h.o.testTurnPlace(a)
}

func TestARestartsBoundaryStopTakesARunInLine(t *testing.T) {
	// Arrange: A holds; a restart asked B to stop at its next boundary.
	w := newTurnWorld(t)
	a, b := w.run("ws-a", "alpha"), w.run("ws-b", "beta")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}
	w.h.o.mu.Lock()
	b.stopAtBoundary = true
	w.h.o.mu.Unlock()

	// Act.
	err := granted(t, w.await(b))

	// Assert.
	if !errors.Is(err, errMergeStopping) {
		t.Fatalf("B's ask ended %v, want errMergeStopping", err)
	}
	select {
	case <-b.atBoundary:
	default:
		t.Fatal("the restart waiting on B's boundary was not told")
	}
	w.h.o.releaseTestTurn(context.Background(), a, "passed")
	if w.holder() != nil {
		t.Fatalf("the turn passed to %v, want nobody: B let go", w.holder())
	}
}

func TestARunInLineStandsIdling(t *testing.T) {
	// Arrange.
	w := newTurnWorld(t)
	a, b := w.run("ws-a", "alpha"), w.run("ws-b", "beta")
	if err := granted(t, w.await(a)); err != nil {
		t.Fatal(err)
	}

	// Act.
	bDone := w.await(b)
	w.awaitLined(t, "ws-b")
	w.h.o.mu.Lock()
	idling := b.idling
	w.h.o.mu.Unlock()
	w.h.o.releaseTestTurn(context.Background(), a, "passed")
	if err := granted(t, bDone); err != nil {
		t.Fatal(err)
	}

	// Assert.
	w.h.o.mu.Lock()
	defer w.h.o.mu.Unlock()
	if !idling || b.idling {
		t.Fatalf("idling in line %v, after the grant %v; want true then false", idling, b.idling)
	}
}

func TestTheHeadInLineCutsTheGovernedWaitOfTheTurnsHolder(t *testing.T) {
	// Arrange: B, behind the head, holds the turn and waits for the governor.
	w := newTurnWorld(t)
	b, head := w.run("ws-b", "beta"), w.run("ws-a", "alpha")
	b.speculative = true
	if err := granted(t, w.await(b)); err != nil {
		t.Fatal(err)
	}
	ctx, cut := context.WithCancelCause(context.Background())
	stop := w.h.o.watchHeadInLine(ctx, b, cut)
	defer stop()

	// Act.
	_ = w.await(head)
	w.awaitLined(t, "ws-a")
	<-ctx.Done()

	// Assert.
	if !errors.Is(context.Cause(ctx), errTurnToHead) {
		t.Fatalf("the governed wait was cut by %v, want errTurnToHead", context.Cause(ctx))
	}
}

func TestAMergeBehindTheHeadInLineDoesNotCutTheHoldersWait(t *testing.T) {
	// Arrange: B holds the turn waiting for the governor; C, also behind
	// the head, joins the line.
	w := newTurnWorld(t)
	b, c := w.run("ws-b", "beta"), w.run("ws-c", "gamma")
	b.speculative, c.speculative = true, true
	if err := granted(t, w.await(b)); err != nil {
		t.Fatal(err)
	}
	ctx, cut := context.WithCancelCause(context.Background())
	stop := w.h.o.watchHeadInLine(ctx, b, cut)

	// Act.
	_ = w.await(c)
	w.awaitLined(t, "ws-c")
	stop()

	// Assert: only the stop ended the watch.
	if errors.Is(context.Cause(ctx), errTurnToHead) {
		t.Fatal("a merge behind the head cut the holder's governed wait")
	}
}

func TestGateEndedNamesHowTheRunEnded(t *testing.T) {
	// Arrange.
	moved, movedCut := context.WithCancelCause(context.Background())
	movedCut(errBaseMoved)
	stopped, stoppedCut := context.WithCancelCause(context.Background())
	stoppedCut(errDaemonExiting)
	live := context.Background()
	tests := []struct {
		name    string
		ctx     context.Context
		verdict gateVerdict
		err     error
		want    string
	}{
		{name: "passed", ctx: live, verdict: gateVerdict{result: GateResult{Passed: true}}, want: "passed"},
		{name: "failed", ctx: live, want: "failed"},
		{name: "broken", ctx: live, verdict: gateVerdict{broken: "x"}, want: "the gate failed to run"},
		{name: "cut", ctx: moved, err: errBaseMoved, want: "cut: the target moved"},
		{name: "stopped", ctx: stopped, err: errDaemonExiting, want: "stopped"},
		{name: "unrun", ctx: live, err: errors.New("no log"), want: "could not run"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := gateEnded(tt.ctx, tt.verdict, tt.err)

			// Assert.
			if got != tt.want {
				t.Fatalf("gateEnded = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestARunWaitingForItsTurnIsDrawnWithItsHolder(t *testing.T) {
	// Arrange.
	round := tabRound{kind: TabTests, n: 2, started: time.UnixMilli(1000)}

	// Act.
	tab := runTab(2, round.waitingForTurn("alpha"), nil, nil)

	// Assert.
	waiting := tab.GetTestsRun().GetWaitingForTurn()
	if waiting.GetStartedAtMs() != 1000 || waiting.GetHolder().GetWorkspaceName() != "alpha" {
		t.Fatalf("the run drew %v, want waiting from 1000 behind alpha", tab)
	}
}

func TestEveryGateOfTheQueueTakesTheTestTurn(t *testing.T) {
	// Arrange: the gate's source.
	body, err := os.ReadFile("gate.go")
	if err != nil {
		t.Fatalf("reading gate.go: %v", err)
	}
	src := string(body)

	// Act / Assert: the run's one way to the runner passes the turn first.
	turn, runner := strings.Index(src, "r.awaitTestTurn("), strings.Index(src, "g.run(ctx, dir, argv")
	if turn < 0 || runner < 0 || turn > runner {
		t.Fatal("gate.go no longer takes the test turn before it starts the runner")
	}
	if strings.Count(src, "RunLinesMeasured(") != 1 {
		t.Fatal("gate.go starts the runner from more than one place; every start must stand behind the turn")
	}
}
