package drain

import (
	"context"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/wsm"
)

func TestSweepHibernatesAnIdleFreeSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want [%s]", hibernated, ws)
	}
}

func TestSweepSendsTheHibernateDirectiveBeforeTheStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(h.stand.hibernated) != 1 || h.stand.hibernated[0] != ws {
		t.Fatalf("hibernate directives = %v, want one for %s", h.stand.hibernated, ws)
	}
}

func TestSweepStandsTheShimDownGracefully(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force {
		t.Fatalf("stand-down calls = %+v, want one graceful KillSession for %s", killed, ws)
	}
}

func TestSweepRecordsARehydratableSessionTerminal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	session, _, err := h.db.Session(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if session.Terminal == nil || session.Terminal.Kind != TerminalHibernated {
		t.Fatalf("terminal = %+v, want the rehydratable %q kind", session.Terminal, TerminalHibernated)
	}
}

func TestSweepClearsTheHibernatedSessionsShimPid(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	session, _, err := h.db.Session(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if session.ShimPID != nil {
		t.Fatalf("shim pid = %d after a stand-down, want cleared", *session.ShimPID)
	}
}

func TestSweepReleasesTheHibernationLease(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	_, held, err := h.db.Lease(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if held {
		t.Fatalf("the hibernation lease is still held after the sweep")
	}
}

func TestSweepLeavesASessionEngagedInsideTheCutoffAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t, instant.Add(-time.Minute))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want nothing inside the cutoff", hibernated)
	}
}

func TestSweepDefersAnIdleSessionThatIsNotFree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.freeness.SetFree(ws, false)

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 || len(h.stand.hibernated) != 0 {
		t.Fatalf("hibernated = %v (directives %v), want the busy session deferred", hibernated, h.stand.hibernated)
	}
}

func TestSweepDefersOnATurnInFlightRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.answer[ws] = &shimv1.HibernateResponse{
		Result: &shimv1.HibernateResponse_Error{Error: &shimv1.HibernateError{
			Kind: &shimv1.HibernateError_TurnInFlight{TurnInFlight: &shimv1.HibernateTurnInFlight{}},
		}},
	}

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want the refusal to defer", hibernated)
	}
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("stand-down calls after a refusal = %+v, want none", killed)
	}
}

func TestSweepDefersWhenTheHibernateDirectiveFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.hibernateErr[ws] = errFake

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want the failure to defer rather than fail the pass", hibernated)
	}
}

func TestSweepDefersWhenTheStandDownFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.killErr[ws] = errFake

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)
	session, _, sessErr := h.db.Session(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if sessErr != nil {
		t.Fatalf("Session: %v", sessErr)
	}
	if len(hibernated) != 0 || session.Terminal != nil {
		t.Fatalf("hibernated = %v terminal = %+v, want no stand-down recorded for a failed kill", hibernated, session.Terminal)
	}
}

func TestSweepDefersAWorkspaceWhoseLeaseAnotherHolderHas(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	if _, err := h.db.AcquireLease(context.Background(), ws, wsm.HolderMerge, wsm.PolicyRefuse); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want the merge's lease to defer the sweep", hibernated)
	}
}

func TestSweepSkipsAWorkspaceWithNoSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.bare(t)

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 || len(h.stand.hibernated) != 0 {
		t.Fatalf("hibernated = %v, want nothing for a workspace with no session", hibernated)
	}
}

func TestSweepSkipsAnAlreadyTerminalSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	if err := h.db.SetSessionTerminal(context.Background(), ws, wsm.SessionTerminal{
		Kind: TerminalHibernated, Detail: "already down", At: instant,
	}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want nothing for a session already terminal", hibernated)
	}
}

func TestRunSweepsOnItsCadence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.Run(ctx) }()
	h.clock.Tick()
	waitForHibernation(t, h, ws)
	cancel()
	<-done

	// Assert
	if len(h.stand.Killed()) != 1 {
		t.Fatalf("stand-down calls = %d, want one from the cadence's sweep", len(h.stand.Killed()))
	}
}

func TestRunFiresTheScheduleWhenItsDeadlinePasses(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t, instant)
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Minute), SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.Run(ctx) }()
	h.clock.Set(instant.Add(2 * time.Minute))
	h.clock.Tick()
	<-h.exits
	<-done

	// Assert
	if len(h.announcer.Shutdowns()) != 1 {
		t.Fatalf("shutdown announcements = %d, want 1 from the fired schedule", len(h.announcer.Shutdowns()))
	}
}

func TestRunSizesItsWaitToTheDeadlineRatherThanTheCadence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Minute), SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.Run(ctx) }()
	waitForWait(t, h)
	cancel()
	<-done

	// Assert
	if got := h.clock.Waits()[0]; got != time.Minute {
		t.Fatalf("first wait = %v, want the minute until the deadline rather than the 5m cadence", got)
	}
}

func TestRunStopsWithItsContext(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.Run(ctx) }()
	waitForWait(t, h)
	cancel()
	err := <-done

	// Assert
	if err == nil {
		t.Fatalf("Run returned nil on a cancelled context, want the context's error")
	}
}
