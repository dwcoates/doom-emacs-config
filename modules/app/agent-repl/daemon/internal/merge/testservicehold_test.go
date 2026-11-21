package merge

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/testsclient"
)

// takenOffMidGate runs the chain's head into its gate, its run held on the
// test service, and takes the gates off the service as its restart does,
// answering the chain, the hold's release and the pump's end. The rerun is
// the default scripted pass.
func takenOffMidGate(t *testing.T) (*chain, func(error), <-chan error) {
	t.Helper()
	c, done, _ := headHeldInGate(t)
	release, err := c.h.o.TakeTestRunsOff(context.Background())
	if err != nil {
		t.Fatalf("TakeTestRunsOff: %v", err)
	}
	return c, release, done
}

// heldServiceRestart takes the gates off the harness's test service with no
// gate on it yet, and answers the hold's release and a channel told each
// time a gate begins waiting the hold out.
func heldServiceRestart(t *testing.T, h *harness) (func(error), <-chan struct{}) {
	t.Helper()
	waiting := make(chan struct{}, 4)
	h.o.onServiceHoldWait = func() { waiting <- struct{}{} }
	release, err := h.o.TakeTestRunsOff(context.Background())
	if err != nil {
		t.Fatalf("TakeTestRunsOff: %v", err)
	}
	return release, waiting
}

// assertNoHostFault fails unless the chain's queue stands unpaused, its merge
// never reached a fixing step, and nothing was recorded at WARN or ERROR.
func assertNoHostFault(t *testing.T, c *chain) {
	t.Helper()
	c.h.db.mu.Lock()
	pause := c.h.db.pauses[c.h.repoKey()]
	c.h.db.mu.Unlock()
	if pause != nil {
		t.Fatalf("pause = %+v, want the queue left as it stood", pause)
	}
	for _, f := range c.h.footer.all() {
		if f.Step == "fixing" || f.State == StateFailed {
			t.Fatalf("facts %+v: the restart was taken as the branch's failure", f)
		}
	}
	for _, level := range []string{"warn", "error"} {
		if records := recordsAtLevel(c.h, level); len(records) > 0 {
			t.Fatalf("records at %s %v, want none", level, records)
		}
	}
}

func TestARestartOfTheTestServiceTakesTheRunningGatesRunOffBeforeItRuns(t *testing.T) {
	// Arrange, Act.
	c, release, done := takenOffMidGate(t)
	t.Cleanup(func() {
		release(nil)
		<-done
	})

	// Assert: the run is stopped and no record names it before the hold is
	// answered.
	if stops := c.h.tests.stopped(); len(stops) != 1 || stops[0] != "run-1" {
		t.Fatalf("stopped %v, want the gate's run taken off the service", stops)
	}
	if doc := recordedDoc(t, c.h); doc.TestRun != "" {
		t.Fatalf("recorded run %q, want none for a resume to watch", doc.TestRun)
	}
	rec, ok := serviceRecord(c.h, "info", "stopped a gate's run on the test service no gate watches again")
	if !ok || rec.Context["run"] != "run-1" || rec.Context["why"] != whyTestServiceRestart {
		t.Fatalf("record = %+v (%v), want the stop naming the restart", rec, ok)
	}
}

func TestAGateTheTestServicesRestartTookOffRunsAgainFromItsStartOnceItAnswers(t *testing.T) {
	// Arrange.
	c, release, done := takenOffMidGate(t)

	// Act.
	release(nil)
	if err := <-done; err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}

	// Assert.
	if started := c.h.tests.started(); len(started) != 2 || started[1].Label != started[0].Label {
		t.Fatalf("started %+v, want the taken-off run and its rerun in the same round", started)
	}
	if got := c.git.landedOwn(); len(got) != 1 {
		t.Fatalf("landings %v, want the merge landed once on its rerun", got)
	}
	if rec, ok := serviceRecord(c.h, "info", "the test service's restart took the gate's run off it; the gate runs again from its start once the service answers, and no fixing attempt is spent"); !ok || rec.Context["round"] != 1 {
		t.Fatalf("record = %+v (%v), want the take-off recorded in round 1", rec, ok)
	}
}

func TestAGateTheTestServicesRestartTookOffIsNoHostFault(t *testing.T) {
	// Arrange.
	c, release, done := takenOffMidGate(t)

	// Act.
	release(nil)
	<-done

	// Assert.
	assertNoHostFault(t, c)
}

func TestAGateWaitsOutTheTestServicesRestartBeforeItsRunStarts(t *testing.T) {
	// Arrange: the hold stands before the gate begins.
	c := newChain(t)
	release, waiting := heldServiceRestart(t, c.h)
	enqueue(t, c.h)
	done := admitAsync(c.h, context.Background())
	receiveWithin(t, waiting, "the gate waiting out the hold")
	before := len(c.h.tests.started())

	// Act.
	release(nil)
	if err := <-done; err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}

	// Assert.
	if before != 0 {
		t.Fatalf("started %d runs while the hold stood, want none", before)
	}
	if started := c.h.tests.started(); len(started) != 1 {
		t.Fatalf("started %d runs, want the gate's one run once the service answered", len(started))
	}
	assertNoHostFault(t, c)
}

func TestARestartOfTheTestServiceWithNoGateOnItTakesNothingOff(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	release, err := h.o.TakeTestRunsOff(context.Background())
	if err != nil {
		t.Fatalf("TakeTestRunsOff: %v", err)
	}
	release(nil)

	// Assert.
	if stops := h.tests.stopped(); len(stops) != 0 {
		t.Fatalf("stopped %v, want nothing", stops)
	}
	if rec, ok := serviceRecord(h, "info", "the test service is restarting: every gate's run is taken off it, and no gate runs on it until it answers again"); !ok || rec.Context["gates"] != 0 {
		t.Fatalf("record = %+v (%v), want no gate taken off", rec, ok)
	}
}

func TestATestServiceThatDoesNotAnswerAfterItsRestartHoldsTheQueueInTheHostsName(t *testing.T) {
	// Arrange: the hold stands before the gate begins, and the service never
	// comes back.
	c := newChain(t)
	release, waiting := heldServiceRestart(t, c.h)
	enqueue(t, c.h)
	done := admitAsync(c.h, context.Background())
	receiveWithin(t, waiting, "the gate waiting out the hold")
	c.h.tests.mu.Lock()
	c.h.tests.startErr = fmt.Errorf("StartRun: %w", testsclient.ErrUnavailable)
	c.h.tests.mu.Unlock()

	// Act.
	release(errors.New("the test service did not answer within 25s of its restart"))
	if err := <-done; err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}

	// Assert.
	if cause := hostPauseCause(t, c); !strings.Contains(cause, testsclient.ErrUnavailable.Error()) {
		t.Fatalf("cause = %q, want the service's unavailability named", cause)
	}
	if rec, ok := serviceRecord(c.h, "info", "the test service's restart ended without its answer; the gate's run starts, and a service still down holds the queue in the host's name"); !ok ||
		!strings.Contains(rec.Context["cause"].(string), "did not answer") {
		t.Fatalf("record = %+v (%v), want the unanswered restart named", rec, ok)
	}
}

func TestAWatchThatFailsAfterARestartEndedStillHoldsTheQueueInTheHostsName(t *testing.T) {
	// Arrange: a restart held and released the gates before this gate began;
	// its own watch then fails as a real fault of the host.
	c := newChain(t)
	release, _ := heldServiceRestart(t, c.h)
	release(nil)
	c.h.tests.watchErr = errors.New("stream reset")
	enqueue(t, c.h)

	// Act.
	if err := <-admitAsync(c.h, context.Background()); err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}

	// Assert.
	if cause := hostPauseCause(t, c); !strings.Contains(cause, "watch the gate's run run-1: stream reset") {
		t.Fatalf("cause = %q, want the failed watch named", cause)
	}
}

func TestASecondRestartOfTheTestServiceIsRefusedWhileOneHoldsTheGates(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, err := h.o.TakeTestRunsOff(context.Background())
	if err != nil {
		t.Fatalf("TakeTestRunsOff: %v", err)
	}
	t.Cleanup(func() { release(nil) })

	// Act.
	_, err = h.o.TakeTestRunsOff(context.Background())

	// Assert.
	if err == nil {
		t.Fatal("a second hold was granted while the first stood")
	}
	if _, ok := h.recordFor("error", opTestService); !ok {
		t.Fatal("the refusal was not recorded at ERROR")
	}
}

func TestAGateThatDoesNotLetGoOfItsRunRefusesTheRestart(t *testing.T) {
	// Arrange: the head's run on the service, whose stop does not answer
	// until the test lets it.
	c, done, _ := headHeldInGate(t)
	c.h.o.drainBound = time.Nanosecond
	stopping, stopAnswers := make(chan struct{}), make(chan struct{})
	c.h.tests.mu.Lock()
	c.h.tests.onStop = func(string) {
		close(stopping)
		<-stopAnswers
	}
	c.h.tests.mu.Unlock()

	// Act.
	_, err := c.h.o.TakeTestRunsOff(context.Background())
	close(stopAnswers)
	<-stopping
	<-done

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "did not let go of its run") {
		t.Fatalf("TakeTestRunsOff = %v, want the restart refused", err)
	}
	if !c.recorded("error", opTestService, "a gate did not let go of its run on the test service within its bound; the restart is refused, and the gates go on") {
		t.Fatal("the refusal was not recorded at ERROR")
	}
	if started := c.h.tests.started(); len(started) != 2 {
		t.Fatalf("started %d runs, want the gate's rerun once the refused hold ended", len(started))
	}
}

func TestAMergeHandedOverWhileTheTestServiceRestartsRerunsItsGateInTheSuccessor(t *testing.T) {
	// Arrange: the gate's run is taken off the service, and the merge is
	// suspended for its workspace's transfer while it waits out the hold.
	c := newChain(t)
	inGate, held := c.runner.hold(c.dirs[theWorkspace])
	t.Cleanup(func() { close(held) })
	waiting := make(chan struct{}, 4)
	c.h.o.onServiceHoldWait = func() { waiting <- struct{}{} }
	enqueue(t, c.h)
	done := admitAsync(c.h, context.Background())
	<-inGate
	release, err := c.h.o.TakeTestRunsOff(context.Background())
	if err != nil {
		t.Fatalf("TakeTestRunsOff: %v", err)
	}
	receiveWithin(t, waiting, "the gate waiting out the hold")
	if err := c.h.o.SuspendForTransfer(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForTransfer: %v", err)
	}
	<-done
	release(nil)
	c.h.restart(t)
	c.wire(c.h.o)

	// Act: the successor takes the workspace's merge.
	if err := c.h.o.AdoptWorkspace(context.Background(), theWorkspace); err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}
	c.promote(t)

	// Assert: the gate ran again from its start on a run of its own, and the
	// merge landed with its queue never paused.
	if started := c.h.tests.started(); len(started) != 2 {
		t.Fatalf("started %d runs, want the taken-off run and the successor's own", len(started))
	}
	if got := c.git.landedOwn(); len(got) != 1 {
		t.Fatalf("landings %v, want the merge landed once in the successor", got)
	}
	if !c.recorded("info", "daemon.merge.recover", "a merge a restart interrupted resumes at the step it recorded") {
		t.Fatal("the successor did not resume the merge at its recorded step")
	}
	c.h.db.mu.Lock()
	defer c.h.db.mu.Unlock()
	if c.h.db.paused[c.h.repoKey()] {
		t.Fatal("the queue was paused, want the merge resumed unpaused")
	}
}
