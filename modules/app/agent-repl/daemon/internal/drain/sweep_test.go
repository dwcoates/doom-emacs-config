package drain

import (
	"context"
	"errors"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
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

// TestTheHibernationReleaseTellsTheQueue pins the drain of the intake: a prompt
// that arrives inside the hibernation window is held against the hibernation
// lease, and the release alone changes a row the queue is not watching — so
// without this the prompt that should have revived the session waits forever.
func TestTheHibernationReleaseTellsTheQueue(t *testing.T) {
	// Arrange
	var told []ids.WorkspaceID
	h := newHarness(t, func(d *Deps) {
		d.LeaseChanged = func(ws ids.WorkspaceID) { told = append(told, ws) }
	})
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(hibernated) != 1 {
		t.Fatalf("hibernated = %v, want the one idle session", hibernated)
	}
	if len(told) != 1 || told[0] != ws {
		t.Fatalf("LeaseChanged calls = %v, want exactly the hibernated workspace %q", told, ws)
	}
}

// TestTheSweepWritesItsPerWorkspaceRecordsToThatWorkspacesSink covers the
// logging invariant: a record about one workspace's session belongs in that
// workspace's own durable sink, never in the global run log.
func TestTheSweepWritesItsPerWorkspaceRecordsToThatWorkspacesSink(t *testing.T) {
	// Arrange: one idle, free session whose hibernate directive fails, so the
	// sweep is guaranteed to record something about it.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.freeness.SetFree(ws, true)
	h.stand.hibernateErr[ws] = errors.New("transport blew up")

	// Act.
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert: the deferral record was written through a workspace-resolved
	// logger, which is what carries the workspace_dir key.
	found := false
	for _, r := range h.log.Records() {
		if r.Operation != opSweep || r.Context[dlog.KeyWorkspaceDir] == nil {
			continue
		}
		found = true
	}
	if !found {
		t.Fatalf("no %s record carried a workspace_dir; the sweep wrote its per-workspace records globally", opSweep)
	}
}

// TestSweepGivesUpOnAShimThatNeverAnswersTheDirective covers the hang the
// idle sweep was found in: the shim ACCEPTS a call and never answers, and the
// sweep runs on the drain loop's own goroutine, so an unbounded call there
// stops every later pass and every standing schedule -- not just this
// workspace.
func TestSweepGivesUpOnAShimThatNeverAnswersTheDirective(t *testing.T) {
	// Arrange: an idle session whose shim takes the directive and goes quiet.
	h := newHarness(t, func(d *Deps) { d.StandBound = 5 * time.Millisecond })
	h.stand.wedge = true
	h.workspace(t, instant.Add(-2*time.Hour))

	// Act: the pass must RETURN. The test's own deadline is the assertion.
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert.
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want nothing hibernated behind a wedged shim", hibernated)
	}
}

// TestSweepRecordsTheShimThatNeverAnsweredAsAFailedDirective is that give-up's
// account: the deferral is never silent, because a shim that stopped answering
// is a fault worth finding.
func TestSweepRecordsTheShimThatNeverAnsweredAsAFailedDirective(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps) { d.StandBound = 5 * time.Millisecond })
	h.stand.wedge = true
	h.workspace(t, instant.Add(-2*time.Hour))

	// Act.
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert.
	found := false
	for _, r := range records(h.log, opSweep) {
		if r.Level == "error" && r.Message == "the hibernate directive failed; deferring the hibernation" {
			found = true
		}
	}
	if !found {
		t.Fatal("the sweep gave up on a wedged shim without recording it")
	}
}

// TestSweepReachesTheNextWorkspaceAfterAWedgedOne is the guarantee the bound
// exists for, spelled as the package's own comment already claims it: one
// wedged session must not stop the sweep reaching the rest.
func TestSweepReachesTheNextWorkspaceAfterAWedgedOne(t *testing.T) {
	// Arrange: two idle sessions, both behind the same wedged shim, so the
	// second is reached only if the first was given up on.
	h := newHarness(t, func(d *Deps) { d.StandBound = 5 * time.Millisecond })
	h.stand.wedge = true
	first := h.workspace(t, instant.Add(-2*time.Hour))
	second := h.workspace(t, instant.Add(-2*time.Hour))

	// Act.
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert: both were tried.
	h.stand.mu.Lock()
	tried := append([]ids.WorkspaceID(nil), h.stand.hibernated...)
	h.stand.mu.Unlock()
	if len(tried) != 2 {
		t.Fatalf("hibernate directives = %v, want one for each of %s and %s", tried, first, second)
	}
}

// TestSweepGivesUpOnAShimThatNeverAnswersTheStandDown covers the OTHER round
// trip, which is the one the hang was actually observed in: the directive is
// acked and the graceful KillSession never answers.
func TestSweepGivesUpOnAShimThatNeverAnswersTheStandDown(t *testing.T) {
	// Arrange: the directive succeeds; only the stand-down goes quiet.
	h := newHarness(t, func(d *Deps) { d.StandBound = 5 * time.Millisecond })
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.wedgeKillOnly()

	// Act.
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert.
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want %s deferred behind a stand-down that never answered", hibernated, ws)
	}
}

// TestTheHibernationReleaseRepublishesTheHostView pins the composer's way back
// open. The host view's composer arm is composed from the OCCUPANCY LEASE, so
// every push taken while the hibernation lease stood said `draining`, and the
// server never sees a lease released — without this republish the last push a
// host client holds keeps the composer shut, and Emacs refuses the prompt that
// would have revived the session.
func TestTheHibernationReleaseRepublishesTheHostView(t *testing.T) {
	// Arrange
	var published []ids.WorkspaceID
	h := newHarness(t, func(d *Deps) {
		d.PublishHost = func(ws ids.WorkspaceID) { published = append(published, ws) }
	})
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(hibernated) != 1 {
		t.Fatalf("hibernated = %v, want the one idle session", hibernated)
	}
	if len(published) != 1 || published[0] != ws {
		t.Fatalf("PublishHost calls = %v, want exactly the hibernated workspace %q", published, ws)
	}
}

// TestTheHibernationRepublishHappensAfterTheLeaseIsReleased is the ORDERING
// half: a republish composed while the lease still stood would compose the
// draining arm all over again, so the fix would push the very state it exists
// to clear. The assertion reads the lease row from inside the callback, which
// is exactly what the composer would read.
func TestTheHibernationRepublishHappensAfterTheLeaseIsReleased(t *testing.T) {
	// Arrange
	var heldAtPublish []wsm.LeaseHolder
	h := newHarness(t)
	h.c.deps.PublishHost = func(published ids.WorkspaceID) {
		lease, held, err := h.db.Lease(context.Background(), published)
		if err != nil {
			t.Errorf("Lease: %v", err)
			return
		}
		if held {
			heldAtPublish = append(heldAtPublish, lease.Holder)
		}
	}
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want the one idle session %q", hibernated, ws)
	}
	if len(heldAtPublish) != 0 {
		t.Fatalf("a lease was still held at the republish (holders %v), want the release to come first", heldAtPublish)
	}
}

// TestARefusedHibernationStillRepublishesTheHostView covers the deferral path:
// the lease was taken, so a push taken meanwhile already said `draining`, and
// the release runs on every path out of the hibernation. The republish must
// follow it there too, or a turn_in_flight refusal leaves the composer shut.
func TestARefusedHibernationStillRepublishesTheHostView(t *testing.T) {
	// Arrange
	var published []ids.WorkspaceID
	h := newHarness(t, func(d *Deps) {
		d.PublishHost = func(ws ids.WorkspaceID) { published = append(published, ws) }
	})
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.answer[ws] = &shimv1.HibernateResponse{
		Result: &shimv1.HibernateResponse_Error{Error: &shimv1.HibernateError{
			Kind: &shimv1.HibernateError_TurnInFlight{TurnInFlight: &shimv1.HibernateTurnInFlight{}},
		}},
	}

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want the refusal to defer", hibernated)
	}
	if len(published) != 1 || published[0] != ws {
		t.Fatalf("PublishHost calls = %v, want the refused workspace %q republished", published, ws)
	}
}

// TestAHibernationWithNoHostSurfaceWiredIsTolerated pins the nil dep: the
// controller is built in tests and in a boot order where no host surface
// exists yet, and a sweep must not take the daemon down for it.
func TestAHibernationWithNoHostSurfaceWiredIsTolerated(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) { d.PublishHost = nil })
	h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 {
		t.Fatalf("hibernated = %v, want the idle session hibernated with no host surface wired", hibernated)
	}
}
