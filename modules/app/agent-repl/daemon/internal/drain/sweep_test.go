package drain

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwhy"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
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

func TestSweepSkipsAWorkspaceWhoseStartIsUnderWay(t *testing.T) {
	// Arrange: a reopened workspace whose row still reads idle for hours.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.startUnderWay(ws)

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil || len(hibernated) != 0 {
		t.Fatalf("Sweep = %v, %v; want nothing hibernated while its start is under way", hibernated, err)
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
	if session.Terminal == nil || session.Terminal.Kind != wsm.TerminalParked {
		t.Fatalf("terminal = %+v, want the rehydratable %q kind", session.Terminal, wsm.TerminalParked)
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

// TestASweepCutShortStillReleasesTheHibernationLease pins the hibernation
// lease's lifetime against the daemon leaving mid-hibernation: its release is
// written on a context the sweep's cancellation cannot refuse.
func TestASweepCutShortStillReleasesTheHibernationLease(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	ctx, cancel := context.WithCancel(context.Background())
	h.stand.onHibernate = cancel

	// Act
	_, _ = h.c.Sweep(ctx, instant)

	// Assert
	if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
		t.Fatalf("Lease after the cut-short sweep = (held %v, %v), want the hibernation lease released", held, err)
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

// TestSweepDefersASessionAPromptIsBeingDeliveredTo pins the stand-down's half
// of the delivery race: a submission that read no lease before the stand-down
// took one stands its shim call, and the stand-down, asking AFTER it holds
// the lease, sees it and defers. No directive goes out, and the lease is
// released for the prompts held against it.
func TestSweepDefersASessionAPromptIsBeingDeliveredTo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	var leaseHeldWhenAsked []bool
	h.delivering = func(asked ids.WorkspaceID) bool {
		_, held, err := h.db.Lease(context.Background(), asked)
		if err != nil {
			t.Errorf("reading the lease while asked: %v", err)
		}
		leaseHeldWhenAsked = append(leaseHeldWhenAsked, held)
		return true
	}

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	_, leaseAfter, err := h.db.Lease(context.Background(), ws)
	if err != nil {
		t.Fatalf("reading the lease after: %v", err)
	}
	got := []any{len(hibernated), len(h.stand.hibernated), leaseHeldWhenAsked, leaseAfter}
	want := []any{0, 0, []bool{true}, false}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("(hibernated, directives, lease held when asked, lease held after) = %v, want %v", got, want)
	}
	var logged bool
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opSweep && r.Context["workspace"] == string(ws) &&
			r.Message == "a prompt is being delivered to the shim; deferring the park" {
			logged = true
		}
	}
	if !logged {
		t.Fatalf("no info record of the deferral under %s naming %s; want it logged", opSweep, ws)
	}
}

// TestSweepDefersASessionABounceIsReplacing pins the stand-down's half of the
// bounce race: a bounce decided on the free workspace before the stand-down
// took its lease owns it, and the stand-down, asking AFTER it holds the
// lease, sees it and defers. No directive goes out, and the lease is released
// for the bounce's restart hold.
func TestSweepDefersASessionABounceIsReplacing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	var leaseHeldWhenAsked []bool
	h.bouncing = func(asked ids.WorkspaceID) bool {
		_, held, err := h.db.Lease(context.Background(), asked)
		if err != nil {
			t.Errorf("reading the lease while asked: %v", err)
		}
		leaseHeldWhenAsked = append(leaseHeldWhenAsked, held)
		return true
	}

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	_, leaseAfter, err := h.db.Lease(context.Background(), ws)
	if err != nil {
		t.Fatalf("reading the lease after: %v", err)
	}
	got := []any{len(hibernated), len(h.stand.hibernated), leaseHeldWhenAsked, leaseAfter}
	want := []any{0, 0, []bool{true}, false}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("(hibernated, directives, lease held when asked, lease held after) = %v, want %v", got, want)
	}
	var logged bool
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opSweep && r.Context["workspace"] == string(ws) &&
			r.Message == "a bounce is replacing the workspace's shim; deferring the park" {
			logged = true
		}
	}
	if !logged {
		t.Fatalf("no info record of the deferral under %s naming %s; want it logged", opSweep, ws)
	}
}

// TestSweepHibernatesWithNoDeliveryUnderWay pins that the delivery check
// passes a session nothing is being delivered to.
func TestSweepHibernatesWithNoDeliveryUnderWay(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.delivering = func(ids.WorkspaceID) bool { return false }

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want %v", hibernated, ws)
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

// TestSweepRecordsTheParkWhenTheStandDownWasSIGKILLed pins the escalated
// stop: the shim acked the hibernate and its kill ran out of the bound, so the
// SIGKILL ended it, and the park is recorded rather than deferred.
func TestSweepRecordsTheParkWhenTheStandDownWasSIGKILLed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.killErr[ws] = fmt.Errorf("stop session: kill the shim: pgid 1 was SIGKILLed: %w: %w", shimclient.ErrSIGKILLed, context.DeadlineExceeded)

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)
	session, _, sessErr := h.db.Session(context.Background(), ws)

	// Assert
	if err != nil || sessErr != nil {
		t.Fatalf("Sweep = %v, Session = %v", err, sessErr)
	}
	if len(hibernated) != 1 || session.Terminal == nil || session.Terminal.Kind != wsm.TerminalParked {
		t.Fatalf("hibernated = %v terminal = %+v, want the park recorded", hibernated, session.Terminal)
	}
	if errs := dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Level: dlog.LevelError}); len(errs) != 0 {
		t.Fatalf("errors = %v, want none for a stop the SIGKILL ended", errs)
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
		Kind: wsm.TerminalParked, Detail: "already down", At: instant,
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
		if r.Level == "error" && r.Message == "the hibernate directive failed; deferring the park" {
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
	if len(terminalsAtPublish) != 1 || terminalsAtPublish[0] != wsm.TerminalParked {
		t.Fatalf("session terminals seen at the republish = %q, want one %q: the republish must follow the record",
			terminalsAtPublish, wsm.TerminalParked)
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
			r.Message == "could not republish the roster after the park" {
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

// TestTheSweepsParkTellsTheSessionScopedViewsAfterTheTerminalRecord is the
// footer's and the indicator's half of the park, and it is one assertion
// because the ordering IS the defect: both are in-memory accumulations fed by
// events, so a park announced before the terminal record exists would be a
// surface asserting something the durable state does not yet say.
//
// The reason they must be told at all is that the last event either one is
// handed during a stand-down is the shim link going dead, and the footer's
// disconnected step reads that as `disconnected · dead` — the word the
// webapp's composer gate closes on (webapp/src/main.ts).
func TestTheSweepsParkTellsTheSessionScopedViewsAfterTheTerminalRecord(t *testing.T) {
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
		t.Fatalf("SetParked calls = %v, want the one park raised once for the one stand-down", parked)
	}
	if terminalsAtPark[0] != wsm.TerminalParked {
		t.Fatalf("session terminal seen at the park = %q, want %q: the views must be told after the record",
			terminalsAtPark[0], wsm.TerminalParked)
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
		if r.Level == "debug" && r.Message == "the workspace has no shim to address; skipping its park" {
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
		if r.Level == "debug" && r.Message == "the session went away before the hibernate directive; deferring the park" {
			found = true
		}
	}
	if !found {
		t.Fatal("the sweep deferred a lost session without recording the state")
	}
}

// TestSweepRecordsAnOrderedStandDownUnderTheDirectiveAtInfo covers the
// other race: a teardown this daemon ordered (a landed merge closing its
// workspace) ended the shim while the hibernate directive was in flight. The
// session is gone by this daemon's own choice, so it is no fault.
func TestSweepRecordsAnOrderedStandDownUnderTheDirectiveAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.hibernateErr[ws] = fmt.Errorf("%w: %w", shimclient.ErrStandDownOrdered, errors.New("unavailable: use of closed network connection"))

	// Act.
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert.
	found := false
	for _, r := range records(h.log, opSweep) {
		if r.Level == "error" {
			t.Fatalf("a stand-down this daemon ordered was recorded at error: %+v", r)
		}
		if r.Level == "info" && r.Message == "this daemon stood the session down under the hibernate directive; deferring the park" && r.Context["workspace"] == string(ws) {
			found = true
		}
	}
	if !found {
		t.Fatal("the sweep deferred an ordered stand-down without recording it")
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
		if r.Level == "error" && r.Message == "the hibernate directive failed; deferring the park" {
			found = true
		}
	}
	if !found {
		t.Fatal("a genuine directive failure on a serving workspace was not recorded at error")
	}
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want the session never forced down over a failed hibernate", killed)
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
		if r.Level == "error" && r.Message == "the hibernate directive failed; deferring the park" {
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
		if r.Level == "error" && r.Message == "the shim answered the hibernate directive with an arm this daemon cannot read; deferring the park" {
			found = true
		}
	}
	if !found {
		t.Fatal("an unreadable hibernate answer was deferred without being recorded")
	}
}

// queuedAt puts ws's own-branch merge in line, requested at, answering the
// queue entry a sync merge's park is scoped to.
func queuedAt(t *testing.T, h *harness, ws ids.WorkspaceID, at time.Time) queuedMerge {
	t.Helper()
	repo := wsm.RepoKey(t.TempDir())
	if err := h.db.RequestMerge(context.Background(), repo, ws, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, at); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	ledger := wsm.NewLeaseID()
	if _, err := h.db.QueueMerge(context.Background(), repo, ws, ledger); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
	return queuedMerge{repo: repo, ledger: ledger}
}

func TestSweepKeepsAnIdleSessionWhoseMergeJoinedTheQueueInsideTheCutoff(t *testing.T) {
	// Arrange: engaged two hours ago, its merge queued half an hour ago.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	queuedAt(t, h, ws, instant.Add(-30*time.Minute))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want the queued merge's session kept warm", hibernated)
	}
}

func TestSweepKeepsAQueuedSessionIdlePastOneCutoffButInsideTwo(t *testing.T) {
	// Arrange: idle ninety minutes, past the harness's one-hour cutoff and
	// inside the two a queued merge's session is kept warm for (owner ruling,
	// 2026-11-11, bounding the 2026-10-26 one).
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-90*time.Minute))
	queuedAt(t, h, ws, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 0 {
		t.Fatalf("hibernated = %v, want the queued merge's session kept warm", hibernated)
	}
}

func TestSweepParksAQueuedSessionIdlePastTwiceTheCutoff(t *testing.T) {
	// Arrange: idle three hours, past the two one-hour cutoffs a queued
	// merge's session is kept warm for.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-3*time.Hour))
	queuedAt(t, h, ws, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want [%s]: its merge waits at its cold gate from here", hibernated, ws)
	}
}

// sweepReadErrDB fails one of the sweep's reads with one error, leaving the
// rest of the store exactly as it is.
type sweepReadErrDB struct {
	wsm.DB
	read string
	err  error
}

func (d sweepReadErrDB) ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error) {
	if d.read == "list" {
		return nil, d.err
	}
	return d.DB.ListWorkspaces(ctx)
}

func (d sweepReadErrDB) AllMergeQueues(ctx context.Context) (map[wsm.RepoKey][]wsm.MergeQueueEntry, error) {
	if d.read == "queues" {
		return nil, d.err
	}
	return d.DB.AllMergeQueues(ctx)
}

func (d sweepReadErrDB) Sessions(ctx context.Context) ([]wsm.Session, error) {
	if d.read == "sessions" {
		return nil, d.err
	}
	return d.DB.Sessions(ctx)
}

// TestSweepLevelsEachReadByWhetherTheLoopWasCancelled pins the level of every
// read the sweep makes: the daemon's own exit cutting a read under it records
// at debug, and a read that fails while serving is still an error. Either way
// the sweep answers the read's error.
func TestSweepLevelsEachReadByWhetherTheLoopWasCancelled(t *testing.T) {
	tests := []struct {
		name      string
		read      string
		readErr   error
		wantLevel string
	}{
		{name: "the workspace list cut by the exit", read: "list", readErr: context.Canceled, wantLevel: "debug"},
		{name: "the workspace list failing while serving", read: "list", readErr: errFake, wantLevel: "error"},
		{name: "the merge queues cut by the exit", read: "queues", readErr: context.Canceled, wantLevel: "debug"},
		{name: "the merge queues failing while serving", read: "queues", readErr: errFake, wantLevel: "error"},
		{name: "the session read cut by the exit", read: "sessions", readErr: context.Canceled, wantLevel: "debug"},
		{name: "the session read timing out", read: "sessions", readErr: context.DeadlineExceeded, wantLevel: "debug"},
		{name: "the session read failing while serving", read: "sessions", readErr: errFake, wantLevel: "error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, func(d *Deps) { d.DB = sweepReadErrDB{DB: d.DB, read: tt.read, err: tt.readErr} })
			h.workspace(t, instant.Add(-2*time.Hour))

			// Act
			_, err := h.c.Sweep(context.Background(), instant)

			// Assert
			if !errors.Is(err, tt.readErr) {
				t.Fatalf("Sweep err = %v, want %v", err, tt.readErr)
			}
			var levels []string
			for _, rec := range h.log.Records() {
				if rec.Operation == opSweep && (rec.Level == "error" || rec.Context["cause"] == tt.readErr.Error()) {
					levels = append(levels, rec.Level)
				}
			}
			if len(levels) != 1 || levels[0] != tt.wantLevel {
				t.Fatalf("read levels = %v, want exactly [%s]", levels, tt.wantLevel)
			}
		})
	}
}

// sweepReadCountDB counts the sweep's session reads, single and bulk.
type sweepReadCountDB struct {
	wsm.DB
	single, bulk *int
}

func (d sweepReadCountDB) Session(ctx context.Context, ws wsm.WorkspaceID) (wsm.Session, bool, error) {
	*d.single++
	return d.DB.Session(ctx, ws)
}

func (d sweepReadCountDB) Sessions(ctx context.Context) ([]wsm.Session, error) {
	*d.bulk++
	return d.DB.Sessions(ctx)
}

// TestSweepReadsEverySessionAtOnce pins the sweep onto the shared bulk index:
// a read per workspace queued each behind every other reader on the store's
// one connection, on every pass.
func TestSweepReadsEverySessionAtOnce(t *testing.T) {
	// Arrange
	var single, bulk int
	h := newHarness(t, func(d *Deps) { d.DB = sweepReadCountDB{DB: d.DB, single: &single, bulk: &bulk} })
	for range 3 {
		h.workspace(t, instant.Add(-2*time.Hour))
	}

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if bulk != 1 || single != 0 {
		t.Fatalf("session reads: bulk = %d, single = %d; want one bulk read and no single reads", bulk, single)
	}
}

// THE IDLE CUTOFF IS UNCHANGED BY REMOTE CONTROL (owner ruling, 2026-10-31): a
// workspace whose shim stays up between keep-alives is still hibernated once
// it has gone unengaged past the cutoff.
func TestSweepHibernatesAnIdleSessionInRemoteControl(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.mainWorktree(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil || len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("Sweep = %v, %v; want [%s] hibernated", hibernated, err, ws)
	}
}

// TestEveryTriggerLeavesTheOnePark pins the single park (owner ruling,
// 2026-11-09): whatever stands an idle session down records the same
// terminal and raises the same park in the views, and only the terminal's
// detail says which trigger it was.
func TestEveryTriggerLeavesTheOnePark(t *testing.T) {
	tests := []struct {
		name       string
		trigger    func(t *testing.T, h *harness) ids.WorkspaceID
		wantDetail string
	}{
		{name: "the idle sweep", wantDetail: "idle past the", trigger: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant.Add(-2*time.Hour))
			if _, err := h.c.Sweep(context.Background(), instant); err != nil {
				t.Fatalf("Sweep: %v", err)
			}
			return ws
		}},
		{name: "a withheld workspace's stand-down", wantDetail: "backlogged, collapsed or closed", trigger: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.backlogged(t)
			h.c.StandDownWithheld(ws, WithheldCacheLapse)
			return ws
		}},
		{name: "the keep-alive cycle", wantDetail: "between keep-alives", trigger: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.c.CycleDown(ws, beatAfter)
			return ws
		}},
		{name: "a settings visit", wantDetail: "settings pick", trigger: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.c.VisitDown(ws, beatAfter)
			return ws
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var told []bool
			h := newHarness(t, func(d *Deps) {
				d.SetParked = func(_ ids.WorkspaceID, park bool) { told = append(told, park) }
			})

			// Act
			ws := tt.trigger(t, h)

			// Assert
			term := h.terminal(t, ws)
			if term == nil || term.Kind != wsm.TerminalParked {
				t.Fatalf("terminal = %+v, want %q", term, wsm.TerminalParked)
			}
			if !strings.Contains(term.Detail, tt.wantDetail) {
				t.Errorf("terminal detail = %q, want the trigger named by %q", term.Detail, tt.wantDetail)
			}
			if len(told) != 1 || !told[0] {
				t.Errorf("SetParked calls = %v, want the one park raised once", told)
			}
		})
	}
}

// remoteControlFailingDB fails every remote-control read.
type remoteControlFailingDB struct{ wsm.DB }

func (d remoteControlFailingDB) RemoteControl(context.Context, ids.WorkspaceID) (wsm.RemoteControl, error) {
	return wsm.RemoteControl{}, errFake
}

// standDownPath is one trigger of the one park, asked for WS.
type standDownPath struct {
	name    string
	op      string
	trigger string
	// withhold arranges what makes the path take WS: withheld paths need
	// the workspace withheld, the cycle's need nothing.
	withhold func(t *testing.T, h *harness, ws ids.WorkspaceID)
	ask      func(t *testing.T, h *harness, ws ids.WorkspaceID)
}

// sparingPaths is every stand-down path whose reason spares a session in
// remote control: every one but the idle sweep's.
var sparingPaths = []standDownPath{
	{name: "a collapse's withheld stand-down", op: opWithheld, trigger: "collapse",
		withhold: func(t *testing.T, h *harness, ws ids.WorkspaceID) { h.inCollapsedSubsection(t, ws) },
		ask:      func(_ *testing.T, h *harness, ws ids.WorkspaceID) { h.c.StandDownWithheld(ws, WithheldCollapse) }},
	{name: "a backlogged workspace's cache lapse", op: opWithheld, trigger: "cache_lapse",
		withhold: func(t *testing.T, h *harness, ws ids.WorkspaceID) { h.hide(t, ws) },
		ask:      func(_ *testing.T, h *harness, ws ids.WorkspaceID) { h.c.StandDownWithheld(ws, WithheldCacheLapse) }},
	{name: "a hidden merge workspace's ended merge", op: opWithheld, trigger: "merge_ended",
		withhold: func(t *testing.T, h *harness, ws ids.WorkspaceID) { h.hide(t, ws) },
		ask:      func(_ *testing.T, h *harness, ws ids.WorkspaceID) { h.c.StandDownWithheld(ws, WithheldMergeEnded) }},
	{name: "the keep-alive cycle", op: opCycle, trigger: "keepalive_cycle",
		withhold: func(*testing.T, *harness, ids.WorkspaceID) {},
		ask:      func(_ *testing.T, h *harness, ws ids.WorkspaceID) { h.c.CycleDown(ws, beatAfter) }},
	{name: "a cold cache's park", op: opCold, trigger: "cold_park",
		withhold: func(*testing.T, *harness, ids.WorkspaceID) {},
		ask:      func(_ *testing.T, h *harness, ws ids.WorkspaceID) { h.c.ColdDown(ws, beatAfter, false) }},
	{name: "a settings visit's end", op: opVisit, trigger: "settings_visit",
		withhold: func(*testing.T, *harness, ids.WorkspaceID) {},
		ask:      func(_ *testing.T, h *harness, ws ids.WorkspaceID) { h.c.VisitDown(ws, beatAfter) }},
	{name: "a sync merge put in line", op: opMergeQueued, trigger: "merge_queued",
		withhold: func(*testing.T, *harness, ids.WorkspaceID) {},
		ask: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			h.mergeQueuedDown(ws, queuedAt(t, h, ws, instant))
		}},
}

// recordWith answers the first record under OP whose message is MESSAGE.
func recordWith(t *testing.T, h *harness, op, message string) dlog.Record {
	t.Helper()
	if record, ok := dlog.FindRecord(records(h.log, op), dlog.RecordQuery{Message: message}); ok {
		return record
	}
	t.Fatalf("records = %+v, want %q under %s", records(h.log, op), message, op)
	return dlog.Record{}
}

// A WORKSPACE IN REMOTE CONTROL KEEPS ITS SHIM (owner ruling, 2026-10-31),
// whichever trigger but the idle sweep's asks.
func TestEveryStandDownPathSparesARemoteControlledSession(t *testing.T) {
	for _, path := range sparingPaths {
		t.Run(path.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := h.remoteControlOn(t)
			path.withhold(t, h, ws)

			// Act
			path.ask(t, h, ws)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none for a session in remote control", killed)
			}
			if term := h.terminal(t, ws); term != nil {
				t.Fatalf("terminal = %+v, want the session left live", term)
			}
			if held := h.remote.heldCount(ws); held != 1 {
				t.Fatalf("the remote-control gate was taken %d times, want once", held)
			}
			record := recordWith(t, h, path.op, remoteSpared)
			if record.Level != "info" || record.Context["trigger"] != path.trigger || record.Context["remote_control"] != true {
				t.Fatalf("record = %+v, want the spare at info naming trigger %q and remote control on", record, path.trigger)
			}
		})
	}
}

// THE IDLE SWEEP PARKS A SESSION IN REMOTE CONTROL (owner ruling,
// 2026-11-09) through the same gate, which records the mode it read.
func TestTheIdleSweepParksARemoteControlledSessionThroughTheGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.mainWorktree(t, instant.Add(-2*time.Hour))

	// Act
	parked, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil || len(parked) != 1 || parked[0] != ws {
		t.Fatalf("Sweep = %v, %v; want [%s] parked", parked, err, ws)
	}
	if held := h.remote.heldCount(ws); held != 1 {
		t.Fatalf("the remote-control gate was taken %d times, want once", held)
	}
	record := recordWith(t, h, opSweep, "parked a session idle past the cutoff")
	if record.Context["remote_control"] != true || record.Context["remote_control_policy"] != "parks" || record.Context["trigger"] != triggerIdle {
		t.Fatalf("record = %+v, want the park naming the idle cutoff and the mode it read", record)
	}
}

// A TURN-ON THAT LANDS BETWEEN THE ASK AND THE ACT IS HONORED: the mode is
// read under the gate a change of it holds (park), so a change under way when
// the stand-down reaches the act is waited for and read.
func TestARemoteControlTurnOnBetweenTheAskAndTheActIsHonored(t *testing.T) {
	for _, path := range sparingPaths {
		t.Run(path.name, func(t *testing.T) {
			// Arrange: the mode is off at the ask, and a turn-on holds the gate.
			ran := make(chan struct{})
			h := newHarness(t, func(d *Deps) {
				d.Detach = func(_ ids.WorkspaceID, run func(context.Context)) {
					go func() {
						defer close(ran)
						run(context.Background())
					}()
				}
			})
			ws := h.workspace(t, instant)
			path.withhold(t, h, ws)
			turnOn := h.remote.hold(ws)
			asking := make(chan ids.WorkspaceID, 1)
			h.remote.mu.Lock()
			h.remote.asking = asking
			h.remote.mu.Unlock()

			// Act
			path.ask(t, h, ws)
			select {
			case <-asking:
			case <-time.After(5 * time.Second):
				t.Fatal("the stand-down never reached the remote-control gate")
			}
			if _, err := h.db.SetRemoteControlMode(context.Background(), ws, wsm.RemoteControlOn); err != nil {
				t.Fatalf("SetRemoteControlMode: %v", err)
			}
			turnOn()
			select {
			case <-ran:
			case <-time.After(5 * time.Second):
				t.Fatal("the stand-down never settled")
			}

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none for a mode turned on before the act", killed)
			}
			if record := recordWith(t, h, path.op, remoteSpared); record.Context["remote_control"] != true {
				t.Fatalf("record = %+v, want the spare reading the mode turned on", record)
			}
		})
	}
}

// EVERY STAND-DOWN RECORD NAMES ITS TRIGGER and the mode it decided on.
func TestEveryStandDownRecordNamesItsTrigger(t *testing.T) {
	idle := standDownPath{name: "the idle sweep", op: opSweep, trigger: triggerIdle,
		withhold: func(*testing.T, *harness, ids.WorkspaceID) {},
		ask: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if _, err := h.c.Sweep(context.Background(), instant.Add(2*time.Hour)); err != nil {
				t.Fatalf("Sweep: %v", err)
			}
		}}
	for _, path := range append(append([]standDownPath{}, sparingPaths...), idle) {
		t.Run(path.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := h.workspace(t, instant)
			path.withhold(t, h, ws)

			// Act
			path.ask(t, h, ws)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 1 {
				t.Fatalf("kills = %+v, want the session stood down", killed)
			}
			var done dlog.Record
			for _, record := range records(h.log, path.op) {
				if record.Level == "info" && record.Context["trigger"] == path.trigger && record.Context["remote_control"] == false {
					done = record
				}
			}
			if done.Message == "" {
				t.Fatalf("records = %+v, want the stand-down at info naming trigger %q and remote control off", records(h.log, path.op), path.trigger)
			}
		})
	}
}

// A COLLAPSE'S RECORD NAMES THE FOLD it decided on.
func TestACollapseStandDownNamesItsFold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.inCollapsedSubsection(t, ws)

	// Act
	h.c.StandDownWithheld(ws, WithheldCollapse)

	// Assert
	record := recordWith(t, h, opWithheld, "stood a withheld workspace's session down")
	if record.Context["fold"] != "subsection" || record.Context["fold_subsection"] != "folded" || record.Context["trigger"] != "collapse" {
		t.Fatalf("record = %+v, want the collapse naming its subsection", record)
	}
}

// AN UNREADABLE MODE STANDS NOTHING DOWN: the act is not taken on a mode
// nobody read, and the failure is recorded once at ERROR with its trigger.
func TestAnUnreadableRemoteControlModeStandsNothingDown(t *testing.T) {
	for _, path := range sparingPaths {
		t.Run(path.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, func(d *Deps) { d.DB = remoteControlFailingDB{DB: d.DB} })
			ws := h.workspace(t, instant)
			path.withhold(t, h, ws)

			// Act
			path.ask(t, h, ws)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none on an unreadable mode", killed)
			}
			if term := h.terminal(t, ws); term != nil {
				t.Fatalf("terminal = %+v, want no park recorded", term)
			}
			record := recordWith(t, h, path.op, "could not read the workspace's remote-control mode; its stand-down is dropped")
			if record.Level != "error" || record.Context["trigger"] != path.trigger || !strings.Contains(fmt.Sprint(record.Context["cause"]), errFake.Error()) {
				t.Fatalf("record = %+v, want the failure at error naming its trigger and cause", record)
			}
		})
	}
}

// allReasons is every stand-down reason the package declares.
func allReasons(c *controller) []standDownReason {
	return []standDownReason{
		c.idleReason(), cycleReason, idleParkReason, visitReason, coldReason, coldGateReason, mergeQueuedReason,
		withheldReason(WithheldCollapse), withheldReason(WithheldCacheLapse), withheldReason(WithheldMergeEnded),
	}
}

// EVERY STAND-DOWN REASON NAMES ITS TRIGGER AND ITS REMOTE-CONTROL POLICY,
// which only the idle sweep's and a cold gate's let park a session in remote
// control. That every stand-down goes through park, the one gate, is a rule
// over the package's sources, held in reposcans
// (daemonrules.TestEveryStandDownPathGoesThroughTheRemoteControlGate), which
// also pins the eight standDownReason literals allReasons builds from.
func TestEveryStandDownReasonNamesItsTriggerAndPolicy(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	reasons := allReasons(h.c)

	// Assert
	for _, reason := range reasons {
		if reason.trigger == "" {
			t.Errorf("the %s reason names no trigger", reason.op)
		}
		want := remoteSpares
		switch {
		case reason.op == opSweep:
			want = remoteParks
		case reason.atGate:
			want = remoteBridges
		}
		if reason.remote != want {
			t.Errorf("the %s reason's remote-control policy = %s, want %s", reason.op, reason.remote, want)
		}
	}
}

func TestParkRefusesAReasonWithNoRemoteControlPolicy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	reason := cycleReason
	reason.remote = 0

	// Act
	defer func() {
		// Assert
		if recovered := recover(); recovered == nil || !strings.Contains(fmt.Sprint(recovered), "no remote-control policy") {
			t.Fatalf("recovered = %v, want the missing policy refused", recovered)
		}
		if killed := h.stand.Killed(); len(killed) != 0 {
			t.Fatalf("kills = %+v, want none", killed)
		}
	}()
	h.c.park(context.Background(), h.c.log, ws, dlog.Context{}, reason, instant)
}

func TestNewRefusesWithoutARemoteControlGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.c.deps
	deps.HoldRemoteControl = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "remote-control gate") {
		t.Fatalf("New = %v, want the missing remote-control gate refused", err)
	}
}

// fakeParks records every idle-before-park observation.
type fakeParks struct {
	mu   sync.Mutex
	seen []parkObservation
}

type parkObservation struct {
	reason shimlife.StandDownReason
	idle   time.Duration
}

func (p *fakeParks) ObserveIdleBeforePark(reason shimlife.StandDownReason, idle time.Duration) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.seen = append(p.seen, parkObservation{reason: reason, idle: idle})
}

func (p *fakeParks) all() []parkObservation {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]parkObservation(nil), p.seen...)
}

func TestAParkObservesHowLongTheSessionStoodUpIdle(t *testing.T) {
	tests := []struct {
		name string
		// turnEnded is the session's last turn's end, zero for none.
		turnEnded time.Time
		// restartedAt is a bring-up of the session after its work, zero for none.
		restartedAt time.Time
		want        time.Duration
	}{
		{name: "from its last real work, a turn's end", turnEnded: instant.Add(time.Minute), want: 29 * time.Minute},
		{name: "from its last real work, which a turn ending earlier does not move back", turnEnded: instant.Add(-time.Hour), want: 30 * time.Minute},
		{name: "from its last real work, with no turn since", want: 30 * time.Minute},
		{name: "from its last real work, which a later session start does not move", restartedAt: instant.Add(10 * time.Minute), want: 30 * time.Minute},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			parks := &fakeParks{}
			h := newHarness(t, func(d *Deps) { d.Parks = parks })
			ws := h.workspace(t, instant)
			if !tt.turnEnded.IsZero() {
				h.turnClosed(t, ws, tt.turnEnded)
			}
			if !tt.restartedAt.IsZero() {
				h.restarted(t, ws, tt.restartedAt)
			}
			h.clock.Set(instant.Add(30 * time.Minute))

			// Act
			h.c.CycleDown(ws, beatAfter)

			// Assert
			got := parks.all()
			if len(got) != 1 || got[0].reason != shimlife.StandDownKeepaliveCycle || got[0].idle != tt.want {
				t.Fatalf("observations = %+v, want one keepalive_cycle park idle %s", got, tt.want)
			}
		})
	}
}

func TestADeferredParkObservesNothingAndHoldsDeferredBusy(t *testing.T) {
	// Arrange
	parks := &fakeParks{}
	why := sessionwhy.NewTable()
	h := newHarness(t, func(d *Deps) { d.Parks, d.Why = parks, why })
	ws := h.workspace(t, instant)
	h.delivering = func(ids.WorkspaceID) bool { return true }

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if got := parks.all(); len(got) != 0 {
		t.Fatalf("observations = %+v, want none for a deferred park", got)
	}
	if got := why.Facts(ws).Hold; got != sessionwhy.HoldDeferredBusy {
		t.Fatalf("hold = %q, want %q", got, sessionwhy.HoldDeferredBusy)
	}
}

func TestAParkWithNoIdleStartObservesNothingAndSaysSo(t *testing.T) {
	// Arrange
	parks := &fakeParks{}
	h := newHarness(t, func(d *Deps) { d.Parks = parks })
	log := h.log.Global()

	// Act
	h.c.observeIdleBeforePark(log, dlog.Context{"workspace": "ws-x"}, h.c.idleReason(), time.Time{})

	// Assert
	if got := parks.all(); len(got) != 0 {
		t.Fatalf("observations = %+v, want none", got)
	}
	record := recordWith(t, h, opSweep, "the parked workspace has neither work nor a creation on record; its idle time before the park is not observed")
	if record.Level != "error" || record.Context["invariant_violation"] == nil {
		t.Fatalf("record = %+v, want an ERROR naming the broken invariant", record)
	}
}

func TestNewRefusesWithoutAColdGateBridge(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.c.deps
	deps.BridgeGate = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "cold gate's bridge") {
		t.Fatalf("New = %v, want the missing cold gate bridge refused", err)
	}
}

// cancelAfterParkDB cancels the park's context once its last write lands:
// the sweep pass or the daemon leaving the moment the park returns.
type cancelAfterParkDB struct {
	wsm.DB
	cancel context.CancelFunc
}

func (d cancelAfterParkDB) SetSpawnedShim(ctx context.Context, ws ids.WorkspaceID, spawn *wsm.ProcessIdentity) error {
	err := d.DB.SetSpawnedShim(ctx, ws, spawn)
	d.cancel()
	return err
}

func TestAParkObservesItsIdleTimeThoughItsCallerHasLeft(t *testing.T) {
	// Arrange
	parks := &fakeParks{}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	h := newHarness(t, func(d *Deps) {
		d.Parks = parks
		d.DB = cancelAfterParkDB{DB: d.DB, cancel: cancel}
		d.Detach = func(_ ids.WorkspaceID, run func(context.Context)) { run(ctx) }
	})
	ws := h.workspace(t, instant)
	h.clock.Set(instant.Add(30 * time.Minute))

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if got := parks.all(); len(got) != 1 || got[0].idle != 30*time.Minute {
		t.Fatalf("observations = %+v, want the park's 30 minutes idle observed", got)
	}
	if errs := records(h.log, opCycle); len(errs) > 0 {
		for _, rec := range errs {
			if rec.Level == "error" {
				t.Fatalf("an ERROR was recorded: %+v", rec)
			}
		}
	}
}
