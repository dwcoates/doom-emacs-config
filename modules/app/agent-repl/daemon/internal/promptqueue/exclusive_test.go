package promptqueue

import (
	"context"
	"errors"
	"sync/atomic"
	"testing"

	"agent-repld/internal/bounce"
	"agent-repld/internal/ids"
)

func TestExclusivelyRunsTheWorkOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	calls := 0

	// Act
	err := h.q.Exclusively(context.Background(), theWorkspace, "fork", func(context.Context) error {
		calls++
		return nil
	})

	// Assert
	if err != nil || calls != 1 {
		t.Fatalf("Exclusively = %v with %d calls, want nil and one call", err, calls)
	}
}

// TestASubmissionDuringExclusiveWorkIsDeliveredOnlyOnceItSettles proves the
// workspace starts no turn while the work runs: a submission made meanwhile is
// held, and delivered once the work has returned.
func TestASubmissionDuringExclusiveWorkIsDeliveredOnlyOnceItSettles(t *testing.T) {
	// Arrange
	h := newHarness(t)
	var during Disposition
	var startedDuring []ids.TurnID
	perform := func(context.Context) error {
		var err error
		during, err = h.q.Submit(context.Background(), submission("t-other", "a concurrent submission"))
		if err != nil {
			t.Errorf("Submit during the exclusive work: %v", err)
		}
		startedDuring = h.sender.started()
		return nil
	}

	// Act
	err := h.q.Exclusively(context.Background(), theWorkspace, "fork", perform)

	// Assert
	if err != nil {
		t.Fatalf("Exclusively: %v", err)
	}
	if during.Delivered || len(startedDuring) != 0 {
		t.Fatalf("submission during the work = %+v with %v started, want it held", during, startedDuring)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t-other" {
		t.Fatalf("started = %v, want the held submission delivered once the work settled", started)
	}
}

func TestExclusivelyReturnsTheWorksErrorAndRecordsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	cause := errors.New("the fork failed")

	// Act
	err := h.q.Exclusively(context.Background(), theWorkspace, "fork", func(context.Context) error { return cause })

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("Exclusively = %v, want the work's error", err)
	}
	if !logged(h.log.Records(), "info", opExclusive, "the work done while the workspace's turn sequence was held failed; deliveries resume") {
		t.Fatalf("records = %+v, want the failure logged", h.log.Records())
	}
}

// exclusiveWhileDraining starts a bounce that holds the workspace draining,
// then starts an exclusive call that records whether the workspace was still
// draining when its work ran. It answers once the call is waiting on the
// bounce, with the call's result channel.
func exclusiveWhileDraining(t *testing.T, h *harness, g *gate, req bounce.Request) (drainingWhenRun *atomic.Bool, ran chan struct{}, done chan error) {
	t.Helper()
	if _, err := h.q.RequestBounce(context.Background(), theWorkspace, req); err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	g.awaitStart(t)
	waiting := make(chan struct{}, 4)
	h.q.onDrainWait = func() { waiting <- struct{}{} }
	drainingWhenRun, ran, done = &atomic.Bool{}, make(chan struct{}, 1), make(chan error, 1)
	go func() {
		done <- h.q.Exclusively(context.Background(), theWorkspace, "fork", func(context.Context) error {
			drainingWhenRun.Store(h.q.isDraining(theWorkspace))
			ran <- struct{}{}
			return nil
		})
	}()
	<-waiting
	return drainingWhenRun, ran, done
}

func TestAnExclusiveCallWaitsOutADrainingBounceAndRunsAfterIt(t *testing.T) {
	// Arrange: a shim replacement is draining the workspace when the call
	// arrives.
	h := newHarness(t)
	g := newGate()
	drainingWhenRun, ran, done := exclusiveWhileDraining(t, h, g, g.request("build_stale", false))

	// Act: the replacement finishes.
	select {
	case <-ran:
		t.Fatal("the call's work ran while the bounce was draining the workspace")
	default:
	}
	g.finish(h, nil)
	err := <-done

	// Assert
	if err != nil || drainingWhenRun.Load() {
		t.Fatalf("Exclusively = %v, draining when run = %v; want the work run after the bounce", err, drainingWhenRun.Load())
	}
	if !logged(h.log.Records(), "info", opExclusive, "a bounce is draining the workspace; the call waits for it to finish so it runs on what serves the workspace after") {
		t.Fatalf("records = %+v, want the wait recorded", h.log.Records())
	}
}

func TestAnExclusiveCallWaitingOnAReplacementTheShimRefusedRunsOnTheShimThatKeepsServing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	g := newGate()
	drainingWhenRun, _, done := exclusiveWhileDraining(t, h, g, g.request("build_stale", false))

	// Act: the shim refuses to stand down over live work.
	g.finish(h, errDeferredRun)
	err := <-done

	// Assert
	if err != nil || drainingWhenRun.Load() {
		t.Fatalf("Exclusively = %v, draining when run = %v; want the work run once the drain ended", err, drainingWhenRun.Load())
	}
}

func TestAnExclusiveCallWaitingOnAMoveIsRefusedOnceTheWorkspaceMoved(t *testing.T) {
	// Arrange
	h := newHarness(t)
	g := newGate()
	req := g.request("handover_transfer", false)
	req.KeepDraining = true
	_, ran, done := exclusiveWhileDraining(t, h, g, req)

	// Act: the move hands the workspace to its new owner.
	g.finish(h, nil)
	err := <-done

	// Assert
	if !errors.Is(err, bounce.ErrMovedAway) {
		t.Fatalf("Exclusively = %v, want the move's refusal", err)
	}
	select {
	case <-ran:
		t.Fatal("the call's work ran on a workspace this daemon no longer serves")
	default:
	}
	if !logged(h.log.Records(), "info", opExclusive, "the workspace was moved off this daemon; the call is refused so it is asked of the daemon it moved to") {
		t.Fatalf("records = %+v, want the refusal recorded", h.log.Records())
	}
}

func TestAnExclusiveCallWaitingOnABounceEndsWithItsContext(t *testing.T) {
	// Arrange
	h := newHarness(t)
	g := newGate()
	if _, err := h.q.RequestBounce(context.Background(), theWorkspace, g.request("build_stale", false)); err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	g.awaitStart(t)
	ctx, cancel := context.WithCancel(context.Background())
	h.q.onDrainWait = cancel

	// Act
	err := h.q.Exclusively(ctx, theWorkspace, "fork", func(context.Context) error {
		t.Error("the call's work ran although its caller gave up")
		return nil
	})

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Exclusively = %v, want the context's end", err)
	}
	g.finish(h, nil)
}

// exclusiveDuringStartTurn holds a submission's StartTurn standing, then
// starts an exclusive call that records which shim call stood when its work
// ran: its own, once the StartTurn has settled. It answers once the call is waiting on the StartTurn, with the
// channel that lets the StartTurn return and the call's result channel.
func exclusiveDuringStartTurn(t *testing.T, h *harness, ctx context.Context) (standingWhenRun *atomic.Value, release chan struct{}, submitted, done chan error) {
	t.Helper()
	inside, release := make(chan struct{}), make(chan struct{})
	h.sender.startHook = func() {
		close(inside)
		<-release
	}
	submitted = make(chan error, 1)
	go func() {
		_, err := h.q.Submit(context.Background(), submission("t1", "go"))
		submitted <- err
	}()
	<-inside
	waiting := make(chan struct{}, 4)
	h.q.onCallWait = func() { waiting <- struct{}{} }
	standingWhenRun, done = &atomic.Value{}, make(chan error, 1)
	go func() {
		done <- h.q.Exclusively(ctx, theWorkspace, "fork", func(context.Context) error {
			call, _ := h.q.standingCall(theWorkspace)
			standingWhenRun.Store(call.what)
			return nil
		})
	}()
	<-waiting
	return standingWhenRun, release, submitted, done
}

// TestAnExclusiveCallWaitsOutAStandingStartTurnAndRunsAfterIt is the
// 2026-10-20 crash: a bounce's end delivered the next held prompt, and the
// fork that had waited on the bounce woke into the lock beside that
// delivery's StartTurn, which outside refuses by panicking.
func TestAnExclusiveCallWaitsOutAStandingStartTurnAndRunsAfterIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	standingWhenRun, release, submitted, done := exclusiveDuringStartTurn(t, h, context.Background())

	// Act
	close(release)
	err := <-done

	// Assert
	if serr := <-submitted; serr != nil {
		t.Fatalf("Submit: %v", serr)
	}
	if standing := standingWhenRun.Load(); err != nil || standing != "fork" {
		t.Fatalf("Exclusively = %v, call standing when run = %v; want the work run as the only call, once the StartTurn settled", err, standing)
	}
	if !logged(h.log.Records(), "info", opExclusive, "a shim call is in flight on the workspace; the call waits for it to settle so it never runs beside it") {
		t.Fatalf("records = %+v, want the wait recorded", h.log.Records())
	}
}

func TestAnExclusiveCallWaitingOnAStandingCallEndsWithItsContext(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	standingWhenRun, release, submitted, done := exclusiveDuringStartTurn(t, h, ctx)

	// Act
	cancel()
	err := <-done
	close(release)

	// Assert
	if serr := <-submitted; serr != nil {
		t.Fatalf("Submit: %v", serr)
	}
	if ran := standingWhenRun.Load(); !errors.Is(err, context.Canceled) || ran != nil {
		t.Fatalf("Exclusively = %v, ran beside %v; want the context's end and no work run", err, ran)
	}
}
