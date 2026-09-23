package drain

import (
	"context"
	"errors"
	"fmt"
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

// TestSweepDefersASessionAPromptIsReviving pins that a session the prompt
// queue is reviving is never hibernated: its record reads idle since the
// engagement before the last hibernation until the revived prompt lands, and
// standing it down there killed the shim the prompt was about to reach.
func TestSweepDefersASessionAPromptIsReviving(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.reviving.Set(ws, true)

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 || len(h.stand.hibernated) != 0 {
		t.Fatalf("hibernated = %v (directives %v), want the reviving session deferred", hibernated, h.stand.hibernated)
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

// TestTheHibernationRepublishesTheRoster pins the roster's half of the park.
// The roster's arm for a parked session is a function of the SESSION TERMINAL
// record ("A PARKED SESSION IS IDLE, NOT BROKEN", resolve/sidebar/status.go),
// and the last event the roster resolver is handed during a stand-down is the
// shim link going dead — handed BEFORE the terminal is written. Without this
// republish the row the daemon parked on purpose stays resolved as `dead`.
func TestTheHibernationRepublishesTheRoster(t *testing.T) {
	// Arrange
	published := 0
	h := newHarness(t, func(d *Deps) {
		d.PublishRegistry = func(context.Context) error { published++; return nil }
	})
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
	if published != 1 {
		t.Fatalf("PublishRegistry calls = %d, want exactly 1 for the one hibernation", published)
	}
}

// TestTheHibernationRepublishFollowsTheTerminalRecord is the ORDERING half,
// and it is the whole defect: a republish composed before the terminal landed
// would resolve the row from a session record that carries none, which is
// exactly what the link-dead event already did. The assertion reads the
// session record from inside the callback, which is what the roster resolver
// itself reads.
func TestTheHibernationRepublishFollowsTheTerminalRecord(t *testing.T) {
	// Arrange
	var terminalsAtPublish []string
	h := newHarness(t)
	var ws ids.WorkspaceID
	h.c.deps.PublishRegistry = func(ctx context.Context) error {
		session, found, err := h.db.Session(ctx, ws)
		if err != nil {
			return err
		}
		if !found || session.Terminal == nil {
			terminalsAtPublish = append(terminalsAtPublish, "")
			return nil
		}
		terminalsAtPublish = append(terminalsAtPublish, session.Terminal.Kind)
		return nil
	}
	ws = h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(terminalsAtPublish) != 1 || terminalsAtPublish[0] != TerminalHibernated {
		t.Fatalf("session terminals seen at the republish = %q, want one %q: the republish must follow the record",
			terminalsAtPublish, TerminalHibernated)
	}
}

// TestARefusedHibernationDoesNotRepublishTheRoster is the negative half:
// nothing DURABLE changed on a deferral, so the roster has nothing new to say
// and a republish would be a push that carries no news.
func TestARefusedHibernationDoesNotRepublishTheRoster(t *testing.T) {
	// Arrange
	published := 0
	h := newHarness(t, func(d *Deps) {
		d.PublishRegistry = func(context.Context) error { published++; return nil }
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
	if published != 0 {
		t.Fatalf("PublishRegistry calls = %d, want 0: a deferral changed nothing durable", published)
	}
}

// TestARosterRepublishFailureStillReportsTheHibernation pins the failure
// arm's shape. The stand-down HAPPENED — the shim is gone and the terminal is
// written — so reporting the pass as refused would leave the sweep retrying a
// session that is already parked. The stale view is what the record names.
func TestARosterRepublishFailureStillReportsTheHibernation(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) {
		d.PublishRegistry = func(context.Context) error { return errors.New("the roster surface is gone") }
	})
	ws := h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want the hibernation to stand despite the republish failure", hibernated)
	}
	found := false
	for _, r := range h.log.Records() {
		if r.Level == "error" && r.Operation == opSweep &&
			r.Message == "could not republish the roster after the hibernation" {
			found = true
		}
	}
	if !found {
		t.Fatalf("no ERROR record named the failed roster republish; records = %v", h.log.Records())
	}
}

// TestAHibernationWithNoRosterSurfaceWiredIsTolerated pins the nil dep: the
// controller is built in tests, and in a boot order where the workspace verbs
// do not exist yet, so a sweep must not take the daemon down for it.
func TestAHibernationWithNoRosterSurfaceWiredIsTolerated(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) { d.PublishRegistry = nil })
	h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 {
		t.Fatalf("hibernated = %v, want the idle session hibernated with no roster surface wired", hibernated)
	}
}

// TestTheHibernationTellsTheSessionScopedViewsAfterTheTerminalRecord is the
// footer's and the indicator's half of the park, and it is one assertion
// because the ordering IS the defect: both are in-memory accumulations fed by
// events, so a park announced before the terminal record exists would be a
// surface asserting something the durable state does not yet say.
//
// The reason they must be told at all is that the last event either one is
// handed during a stand-down is the shim link going dead, and the footer's
// disconnected step reads that as `disconnected · dead` — the word the
// webapp's composer gate closes on (webapp/src/main.ts).
func TestTheHibernationTellsTheSessionScopedViewsAfterTheTerminalRecord(t *testing.T) {
	// Arrange
	var terminalsAtPark []string
	var parked []bool
	h := newHarness(t)
	var ws ids.WorkspaceID
	h.c.deps.SetParked = func(told ids.WorkspaceID, park bool) {
		parked = append(parked, park)
		session, found, err := h.db.Session(context.Background(), told)
		if err != nil || !found || session.Terminal == nil {
			terminalsAtPark = append(terminalsAtPark, "")
			return
		}
		terminalsAtPark = append(terminalsAtPark, session.Terminal.Kind)
	}
	ws = h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want the one idle session %q", hibernated, ws)
	}
	if len(parked) != 1 || !parked[0] {
		t.Fatalf("SetParked calls = %v, want exactly one park for the one hibernation", parked)
	}
	if terminalsAtPark[0] != TerminalHibernated {
		t.Fatalf("session terminal seen at the park = %q, want %q: the views must be told after the record",
			terminalsAtPark[0], TerminalHibernated)
	}
}

// TestARefusedHibernationDoesNotParkTheSessionScopedViews is the negative
// half: the shim is still serving, so a park told here would make the footer
// treat the NEXT real link death as a deliberate stand-down.
func TestARefusedHibernationDoesNotParkTheSessionScopedViews(t *testing.T) {
	// Arrange
	var parked []bool
	h := newHarness(t, func(d *Deps) {
		d.SetParked = func(_ ids.WorkspaceID, park bool) { parked = append(parked, park) }
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
	if len(parked) != 0 {
		t.Fatalf("SetParked calls = %v, want none: the shim the sweep deferred is still serving", parked)
	}
}

// TestAHibernationWithNoSessionScopedViewsWiredIsTolerated pins the nil dep:
// the controller is built in tests without any view surface, and a sweep must
// not take the daemon down for it.
func TestAHibernationWithNoSessionScopedViewsWiredIsTolerated(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) { d.SetParked = nil })
	h.workspace(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 {
		t.Fatalf("hibernated = %v, want the idle session hibernated with no view surface wired", hibernated)
	}
}

// unsinkableWorkspace makes the workspace's directory unable to host a durable
// log sink, the way a scratch or deleted worktree is: the minted-id lookup the
// sink resolution needs cannot name it.
func unsinkableWorkspace(h *harness, dir string) {
	h.log.BindWorkspaceIDs(func(candidate string) (string, error) {
		if candidate == dir {
			return "", errors.New("the workspace directory is gone")
		}
		return "wsxxxxxxxxxxxxxx", nil
	})
}

func TestSweepHibernatesAWorkspaceWhoseDirectoryCannotHostASink(t *testing.T) {
	// Arrange: the sweep must not stop at a workspace whose worktree is gone.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	unsinkableWorkspace(h, record.Dir)

	// Act.
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert.
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want [%s]", hibernated, ws)
	}
}

func TestSweepRecordsNoErrorForAWorkspaceThatCannotHostASink(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	unsinkableWorkspace(h, record.Dir)

	// Act.
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert: an unavailable directory is an ordinary outcome, never a fault.
	for _, rec := range records(h.log, opSweep) {
		if rec.Level == "error" {
			t.Fatalf("the sweep recorded an error for an unsinkable workspace: %+v", rec)
		}
	}
}

func TestSweepNamesTheWorkspaceOnItsCentrallyRoutedRecords(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	unsinkableWorkspace(h, record.Dir)

	// Act.
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert: the records still say which workspace they are about.
	for _, rec := range records(h.log, opSweep) {
		if rec.Context[dlog.KeyUnroutableWorkspace] == record.Dir {
			return
		}
	}
	t.Fatalf("no sweep record named the unroutable workspace %q", record.Dir)
}

// TestSweepSkipsAWorkspaceItHoldsNoShimFor covers the selection invariant: a
// workspace whose session this daemon cannot address -- a bring-up still in
// flight, a close already under way, a durable row this process never adopted
// -- is never sent a directive that cannot land.
func TestSweepSkipsAWorkspaceItHoldsNoShimFor(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.standDown(ws)

	// Act.
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert.
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 || len(h.stand.hibernated) != 0 {
		t.Fatalf("hibernated = %v (directives %v), want no directive sent to a workspace with no shim",
			hibernated, h.stand.hibernated)
	}
}

// TestSweepRecordsTheSkippedWorkspaceAtDebug covers the skip's account: the
// sweep says WHY it passed a workspace over, at the level a state deserves.
// It cost an ERROR every five minutes forever when it did not.
func TestSweepRecordsTheSkippedWorkspaceAtDebug(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.standDown(ws)

	// Act.
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert.
	found := false
	for _, r := range records(h.log, opSweep) {
		if r.Level == "error" {
			t.Fatalf("the skipped workspace was recorded at error: %+v", r)
		}
		if r.Level == "debug" && r.Message == "the workspace has no shim to address; skipping its hibernation" {
			found = true
		}
	}
	if !found {
		t.Fatal("the sweep skipped a workspace without recording why")
	}
}

// TestSweepRecordsALostSessionDirectiveAtDebug covers the RACE the skip above
// cannot close: the session went away between the selection and the call. The
// typed state is an expected outcome, not a fault.
func TestSweepRecordsALostSessionDirectiveAtDebug(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.hibernateErr[ws] = fmt.Errorf("workspace: hibernate %q: %w", ws, ErrNoLiveSession)

	// Act.
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert.
	found := false
	for _, r := range records(h.log, opSweep) {
		if r.Level == "error" {
			t.Fatalf("a session lost before the directive was recorded at error: %+v", r)
		}
		if r.Level == "debug" && r.Message == "the session went away before the hibernate directive; deferring the hibernation" {
			found = true
		}
	}
	if !found {
		t.Fatal("the sweep deferred a lost session without recording the state")
	}
}

// TestSweepStillRecordsAGenuineDirectiveFailureAtError is the other side of
// that narrowing: a directive that failed against a shim that WAS there is a
// fault, and stays one.
func TestSweepStillRecordsAGenuineDirectiveFailureAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.hibernateErr[ws] = errors.New("transport blew up")

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
		t.Fatal("a genuine directive failure on a serving workspace was not recorded at error")
	}
}

// scheduleErrDB fails every schedule read with one error, leaving the rest of
// the store exactly as it is.
type scheduleErrDB struct {
	wsm.DB
	err error
}

func (d scheduleErrDB) DrainSchedule(context.Context) (*wsm.DrainSchedule, error) {
	return nil, d.err
}

// TestRunLevelsTheScheduleReadByWhetherTheLoopWasCancelled pins the level of
// the drain loop's schedule read: the serving lifetime ending under the read is
// this daemon's own exit withdrawing the loop, so it records at debug; a read
// that fails while serving is still an error.
func TestRunLevelsTheScheduleReadByWhetherTheLoopWasCancelled(t *testing.T) {
	tests := []struct {
		name      string
		readErr   error
		wantLevel string
	}{
		{name: "the read was cancelled by the exit", readErr: context.Canceled, wantLevel: "debug"},
		{name: "the read timed out", readErr: context.DeadlineExceeded, wantLevel: "debug"},
		{name: "the read failed while serving", readErr: errFake, wantLevel: "error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, func(d *Deps) { d.DB = scheduleErrDB{DB: d.DB, err: tt.readErr} })

			// Act
			err := h.c.Run(context.Background())

			// Assert
			if !errors.Is(err, tt.readErr) {
				t.Fatalf("Run err = %v, want %v", err, tt.readErr)
			}
			var levels []string
			for _, rec := range records(h.log, opRun) {
				if rec.Level == "debug" && rec.Message == "the drain loop is running" {
					continue
				}
				levels = append(levels, rec.Level)
			}
			if len(levels) != 1 || levels[0] != tt.wantLevel {
				t.Fatalf("schedule-read levels = %v, want exactly [%s]", levels, tt.wantLevel)
			}
		})
	}
}

// TestSweepDefersOnACompactingAnswer is the first half of the two-phase
// directive. A compaction is a real vendor turn and StandBound is a sum of
// TEARDOWN bounds, so the shim answers that the work has STARTED and finishes
// it past the rpc. This pass has nothing to stand down yet.
func TestSweepDefersOnACompactingAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.answer[ws] = compactingAnswer()

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want the pass deferred behind a compaction in flight", hibernated)
	}
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("stand-down calls while a compaction was in flight = %+v, want none", killed)
	}
}

// TestACompactingAnswerIsRecordedAtDebugAndNotAsAFailure is the defect this
// arm exists for. While the answer WAS the compaction's completion, every
// sweep pass hit its own deadline, recorded a failed directive, never stood
// the session down, and re-ran the whole summary turn five minutes later --
// thirteen of them on one workspace in one morning, each with an ERROR beside
// it naming nothing anybody could fix.
func TestACompactingAnswerIsRecordedAtDebugAndNotAsAFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.answer[ws] = compactingAnswer()

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	deferred := false
	for _, r := range records(h.log, opSweep) {
		if r.Level == "error" || r.Level == "warn" {
			t.Fatalf("a compaction in flight was recorded at %s: %q", r.Level, r.Message)
		}
		if r.Level == "debug" && r.Message == "compaction in flight; deferring the hibernation" {
			deferred = true
		}
	}
	if !deferred {
		t.Fatal("the sweep deferred behind a compaction without recording that it had")
	}
}

// TestTheNextPassStandsDownOnceTheCompactionHasLanded is the other half: the
// deferral is only correct because the ask after it is ACKED, and the shim
// never compacts one transcript twice.
func TestTheNextPassStandsDownOnceTheCompactionHasLanded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.answer[ws] = compactingAnswer()
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("the first Sweep: %v", err)
	}
	delete(h.stand.answer, ws)

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want %s stood down on the pass after the compaction landed", hibernated, ws)
	}
}

// TestAFailedDirectiveIsStillAnError pins what the compacting arm did NOT
// soften. A directive that fails for any other reason is a shim that stopped
// answering, and that is still worth finding.
func TestAFailedDirectiveIsStillAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.hibernateErr[ws] = errFake

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	found := false
	for _, r := range records(h.log, opSweep) {
		if r.Level == "error" && r.Message == "the hibernate directive failed; deferring the hibernation" {
			found = true
		}
	}
	if !found {
		t.Fatal("a failed hibernate directive was not recorded as an error")
	}
}

// TestSweepDefersOnAnArmItCannotRead covers the answer nobody here understood.
// Standing a session down on one would kill a session whose transcript was
// never compacted, which is the one thing the directive exists to prevent.
func TestSweepDefersOnAnArmItCannotRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.answer[ws] = &shimv1.HibernateResponse{}

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want an unreadable answer deferred", hibernated)
	}
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("stand-down calls after an unreadable answer = %+v, want none", killed)
	}
}

// TestAnArmItCannotReadIsRecordedAsAnError is that deferral's account: a shim
// speaking a contract this daemon was not taught is a deployment fault.
func TestAnArmItCannotReadIsRecordedAsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.answer[ws] = &shimv1.HibernateResponse{}

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	found := false
	for _, r := range records(h.log, opSweep) {
		if r.Level == "error" && r.Message == "the shim answered the hibernate directive with an arm this daemon cannot read; deferring the hibernation" {
			found = true
		}
	}
	if !found {
		t.Fatal("an unreadable hibernate answer was deferred without being recorded")
	}
}

// compactingAnswer is the shim's "the work has started and outlives this rpc".
func compactingAnswer() *shimv1.HibernateResponse {
	return &shimv1.HibernateResponse{
		Result: &shimv1.HibernateResponse_Compacting{Compacting: &shimv1.HibernateCompacting{}},
	}
}
