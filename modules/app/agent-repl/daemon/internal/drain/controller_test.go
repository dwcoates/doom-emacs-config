package drain

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

func TestSchedulePersistsTheScheduleInForce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deadline := instant.Add(time.Hour)

	// Act
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: deadline, SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	got, err := h.c.Current(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Current: %v", err)
	}
	if got == nil || !got.Deadline.Equal(deadline) {
		t.Fatalf("current = %+v, want the schedule just armed", got)
	}
}

func TestSchedulePublishesDrainScheduledToEveryWatchDaemonSubscriber(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Hour), SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Assert
	pushed := h.announcer.Scheduled()
	if len(pushed) != 1 {
		t.Fatalf("drain_scheduled pushes = %d, want 1", len(pushed))
	}
	if pushed[0].GetReason().GetDeploy() == nil {
		t.Fatalf("push reason = %v, want the deploy arm the schedule named", pushed[0].GetReason())
	}
}

func TestANewerScheduleReplacesTheStandingOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Hour), SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	replacement := instant.Add(2 * time.Hour)

	// Act
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: replacement, SetAt: instant.Add(time.Minute),
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	got, err := h.c.Current(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Current: %v", err)
	}
	if got == nil || !got.Deadline.Equal(replacement) {
		t.Fatalf("current deadline = %v, want the replacement %v", got, replacement)
	}
}

func TestScheduleRefusesAReasonThatWillNotDecode(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: "not a drain reason", Deadline: instant.Add(time.Hour), SetAt: instant,
	})

	// Assert
	if err == nil {
		t.Fatalf("Schedule accepted a reason that will not decode")
	}
}

func TestCancelClearsTheScheduleAndPublishesTheCancellation(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Hour), SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act
	if err := h.c.Cancel(context.Background()); err != nil {
		t.Fatalf("Cancel: %v", err)
	}
	got, err := h.c.Current(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Current: %v", err)
	}
	if got != nil {
		t.Fatalf("current = %+v after a cancel, want nil", got)
	}
	if h.announcer.Cancelled() != 1 {
		t.Fatalf("drain_cancelled pushes = %d, want 1", h.announcer.Cancelled())
	}
}

func TestCancelRefusesWhenNothingIsScheduled(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.c.Cancel(context.Background())

	// Assert
	if !errors.Is(err, ErrNothingScheduled) {
		t.Fatalf("Cancel error = %v, want ErrNothingScheduled", err)
	}
}

func TestShutdownNowAnnouncesTheImmediateCauseAndExits(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reason := &agentreplv1.DrainReason{
		Kind: &agentreplv1.DrainReason_Operator{
			Operator: &agentreplv1.DrainReasonOperator{Note: "the operator said so"},
		},
	}

	// Act
	if err := h.c.ShutdownNow(context.Background(), reason); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	pushed := h.announcer.Shutdowns()
	if len(pushed) != 1 {
		t.Fatalf("shutdown announcements = %d, want 1", len(pushed))
	}
	if pushed[0].GetCause().GetImmediate().GetReason().GetOperator().GetNote() != "the operator said so" {
		t.Fatalf("cause = %v, want the immediate arm carrying the operator's note", pushed[0].GetCause())
	}
	select {
	case <-h.exits:
	default:
		t.Fatalf("the orderly exit was never started")
	}
}

func TestShutdownNowCarriesNoSuccessorAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.c.ShutdownNow(context.Background(), &agentreplv1.DrainReason{
		Kind: &agentreplv1.DrainReason_Maintenance{Maintenance: &agentreplv1.DrainReasonMaintenance{}},
	}); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	if h.announcer.Shutdowns()[0].Address != nil {
		t.Fatalf("address = %v on an immediate shutdown, want unset (a plain bounce)", *h.announcer.Shutdowns()[0].Address)
	}
}

// TestShutdownNowForcesEverySessionDownBeforeExiting pins the process-tree
// half of the immediate stop: the daemon's own exit reclaims nothing but the
// daemon, because every shim was spawned into a process group of its own so a
// BOUNCE could hand it to an adopting successor. `now` has no successor, so
// the shims are this call's to stand down.
func TestShutdownNowForcesEverySessionDownBeforeExiting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first := h.workspace(t, instant)
	second := h.workspace(t, instant)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	stood := map[ids.WorkspaceID]bool{}
	for _, call := range h.stand.Killed() {
		if !call.Force {
			t.Fatalf("stand-down of %s was graceful, want forced: an immediate stop bought no freeness to wait on", call.WS)
		}
		stood[call.WS] = true
	}
	for _, ws := range []ids.WorkspaceID{first, second} {
		if !stood[ws] {
			t.Fatalf("workspace %s was never stood down; its shim and both shim-lock holders would outlive the daemon (stood down: %v)", ws, h.stand.Killed())
		}
	}
	select {
	case <-h.exits:
	default:
		t.Fatalf("the orderly exit was never started")
	}
}

// TestShutdownNowStandsDownAWorkspaceParkedAtAPermissionGate is the defect's
// own shape: a turn parked at a permission gate never falls free on its own,
// so a stop that waited for freeness would never come. `now` waits for none —
// it never asks — and the parked workspace is stood down like any other.
func TestShutdownNowStandsDownAWorkspaceParkedAtAPermissionGate(t *testing.T) {
	// Arrange: the workspace is NOT free, and the gate that would release it
	// is never opened.
	h := newHarness(t)
	parked := h.workspace(t, instant)
	h.freeness.SetFree(parked, false)
	h.freeness.Gate(parked)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	if awaited := h.freeness.Awaited(); len(awaited) != 0 {
		t.Fatalf("ShutdownNow waited on freeness for %v; an immediate stop waits for none", awaited)
	}
	if killed := h.stand.Killed(); len(killed) != 1 || killed[0].WS != parked {
		t.Fatalf("stand-downs = %v, want exactly the parked workspace %s", killed, parked)
	}
	select {
	case <-h.exits:
	default:
		t.Fatalf("the orderly exit was never started for a workspace parked at a permission gate")
	}
}

// TestShutdownNowExitsWhenAShimWillNotStandDown pins the failure policy: the
// leaked shim is REPORTED at ERROR, its siblings are still stood down, and the
// exit happens regardless — an exit skipped over one bad shim leaks the daemon
// too.
func TestShutdownNowExitsWhenAShimWillNotStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	stubborn := h.workspace(t, instant)
	healthy := h.workspace(t, instant)
	h.stand.failKill(stubborn, errFake)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	stood := map[ids.WorkspaceID]bool{}
	for _, call := range h.stand.Killed() {
		stood[call.WS] = true
	}
	if !stood[healthy] {
		t.Fatalf("the healthy workspace %s was not stood down after a sibling refused", healthy)
	}
	var reported bool
	for _, rec := range records(h.log, opNow) {
		if rec.Level == "error" && rec.Context["workspace"] == string(stubborn) {
			reported = true
		}
	}
	if !reported {
		t.Fatalf("%s recorded no ERROR naming the workspace whose shim will outlive the daemon", opNow)
	}
	select {
	case <-h.exits:
	default:
		t.Fatalf("the orderly exit was never started after a stand-down failed")
	}
}

// TestShutdownNowIsNotHeldByAWedgedShim pins the bound: a shim that ACCEPTS
// the stand-down and never answers costs StandBound and no more. Without the
// bound the immediate stop would hang on exactly the shim it exists to reclaim.
func TestShutdownNowIsNotHeldByAWedgedShim(t *testing.T) {
	// Arrange
	bound := 50 * time.Millisecond
	h := newHarness(t, func(d *Deps) { d.StandBound = bound })
	h.workspace(t, instant)
	h.stand.wedgeKillOnly()

	// Act
	started := time.Now()
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}
	elapsed := time.Since(started)

	// Assert
	if elapsed < bound {
		t.Fatalf("ShutdownNow returned after %s, short of its own %s stand-down bound", elapsed, bound)
	}
	// Ten times the bound: the call is one bounded stand-down plus bookkeeping,
	// so anything near a second means the bound was not applied at all.
	if elapsed > 10*bound {
		t.Fatalf("ShutdownNow took %s on one wedged shim, want it bounded by %s", elapsed, bound)
	}
	select {
	case <-h.exits:
	default:
		t.Fatalf("the orderly exit was never started after a wedged stand-down")
	}
}

// TestShutdownNowStandsEverySessionDownWhenTheCallerOutlivesTheBound is the
// first half of the bound-derivation contract: with room to spare in the
// caller's budget, one wedged shim costs StandBound, is REPORTED, and its
// sibling is still stood down.
func TestShutdownNowStandsEverySessionDownWhenTheCallerOutlivesTheBound(t *testing.T) {
	// Arrange
	bound := 50 * time.Millisecond
	h := newHarness(t, func(d *Deps) { d.StandBound = bound })
	wedged := h.workspace(t, instant)
	healthy := h.workspace(t, instant)
	h.stand.wedgeKillOn(wedged)
	caller, cancel := context.WithTimeout(context.Background(), 20*bound)
	defer cancel()

	// Act
	started := time.Now()
	if err := h.c.ShutdownNow(caller, maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}
	elapsed := time.Since(started)

	// Assert
	assertSteppedOverTheWedgedShim(t, h, wedged, healthy, bound, elapsed)
}

// TestShutdownNowStandsEverySessionDownWhenTheCallerCannotOutliveTheBound is
// the half the derivation used to get wrong.
func TestShutdownNowStandsEverySessionDownWhenTheCallerCannotOutliveTheBound(t *testing.T) {
	// Arrange: a caller whose whole budget is a fraction of ONE stand-down's.
	// The daemon's own e2e harness is this caller — a single context created at
	// the daemon's process start and spent by every call before this one — and
	// with the bound derived as a child of it, the wedged workspace would eat
	// the remainder and every sibling after it would be given up on with a
	// context error before its shim was ever asked.
	bound := 50 * time.Millisecond
	h := newHarness(t, func(d *Deps) { d.StandBound = bound })
	wedged := h.workspace(t, instant)
	healthy := h.workspace(t, instant)
	h.stand.wedgeKillOn(wedged)
	caller, cancel := context.WithTimeout(context.Background(), bound/5)
	defer cancel()

	// Act
	started := time.Now()
	if err := h.c.ShutdownNow(caller, maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}
	elapsed := time.Since(started)

	// Assert
	assertSteppedOverTheWedgedShim(t, h, wedged, healthy, bound, elapsed)
}

// assertSteppedOverTheWedgedShim is the shared assertion of the two tests
// above: the daemon spent ITS OWN bound on the shim that would not go, said so
// at ERROR, stood the sibling down anyway, and exited. It is one helper rather
// than two copies because the two tests differ in exactly one arrangement —
// the caller's budget — and that difference is the subject.
func assertSteppedOverTheWedgedShim(
	t *testing.T,
	h *harness,
	wedged, healthy ids.WorkspaceID,
	bound, elapsed time.Duration,
) {
	t.Helper()
	if elapsed < bound {
		t.Fatalf("ShutdownNow returned after %s, short of the daemon's own %s stand-down bound; the bound came from the caller, not from the daemon", elapsed, bound)
	}
	// Ten times the bound: one wedged stand-down plus bookkeeping.
	if elapsed > 10*bound {
		t.Fatalf("ShutdownNow took %s on one wedged shim, want it bounded by %s", elapsed, bound)
	}
	stood := map[ids.WorkspaceID]bool{}
	for _, call := range h.stand.Killed() {
		stood[call.WS] = true
	}
	if !stood[healthy] {
		t.Fatalf("the healthy workspace %s was never stood down; a workspace behind a wedged sibling must be stepped TO, not skipped", healthy)
	}
	var reported bool
	for _, rec := range records(h.log, opNow) {
		if rec.Level == "error" && rec.Context["workspace"] == string(wedged) {
			reported = true
		}
	}
	if !reported {
		t.Fatalf("%s recorded no ERROR naming %s, whose shim will outlive this daemon", opNow, wedged)
	}
	select {
	case <-h.exits:
	default:
		t.Fatalf("the orderly exit was never started after a wedged stand-down")
	}
}

// maintenanceReason is the typed reason the immediate-shutdown tests state
// when the reason itself is not the subject.
func maintenanceReason() *agentreplv1.DrainReason {
	return &agentreplv1.DrainReason{
		Kind: &agentreplv1.DrainReason_Maintenance{Maintenance: &agentreplv1.DrainReasonMaintenance{}},
	}
}

func TestShutdownNowRefusesAnArmlessReason(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.c.ShutdownNow(context.Background(), &agentreplv1.DrainReason{})

	// Assert
	if err == nil {
		t.Fatalf("ShutdownNow accepted a reason with no arm")
	}
}

func TestFireHoldsEveryWorkspaceUnderTheDrainLease(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first := h.workspace(t, instant)
	second := h.workspace(t, instant)
	schedule := wsm.DrainSchedule{Reason: deployReason(t), Deadline: instant, SetAt: instant}
	// Neither is free, so the hold phase is followed by a wait this test can
	// synchronize on: fire takes EVERY hold before it waits on any workspace.
	h.freeness.SetFree(first, false)
	h.freeness.SetFree(second, false)
	gate := h.freeness.Gate(first)
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.fire(context.Background(), schedule) }()
	<-h.freeness.calls
	leases := map[wsm.WorkspaceID]wsm.Lease{}
	for _, ws := range []wsm.WorkspaceID{first, second} {
		lease, held, err := h.db.Lease(context.Background(), ws)
		if err != nil || !held {
			t.Fatalf("workspace %s lease held = %v (err %v), want held before the first wait", ws, held, err)
		}
		leases[ws] = lease
	}
	close(gate)
	h.freeness.Gate(second)
	<-h.freeness.calls

	// Assert
	for ws, lease := range leases {
		if lease.Holder != wsm.HolderDrain || lease.Policy != wsm.PolicyHold {
			t.Fatalf("workspace %s lease = holder %v policy %v, want HolderDrain/PolicyHold", ws, lease.Holder, lease.Policy)
		}
	}
}

func TestFireWaitsForFreenessBeforeAnnouncing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	schedule := wsm.DrainSchedule{Reason: deployReason(t), Deadline: instant, SetAt: instant}
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.fire(context.Background(), schedule) }()
	<-h.freeness.calls
	announcedEarly := len(h.announcer.Shutdowns())
	close(gate)
	if err := <-done; err != nil {
		t.Fatalf("fire: %v", err)
	}

	// Assert
	if announcedEarly != 0 {
		t.Fatalf("shutdown announcements before freeness = %d, want 0", announcedEarly)
	}
	if len(h.announcer.Shutdowns()) != 1 {
		t.Fatalf("shutdown announcements = %d, want 1 once the workspace fell free", len(h.announcer.Shutdowns()))
	}
}

func TestFireNeverInterruptsTheVendor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	schedule := wsm.DrainSchedule{Reason: deployReason(t), Deadline: instant, SetAt: instant}
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.fire(context.Background(), schedule) }()
	<-h.freeness.calls
	close(gate)
	if err := <-done; err != nil {
		t.Fatalf("fire: %v", err)
	}

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("stand-down calls during a drain = %+v, want none: teardown never interrupts", killed)
	}
}

func TestFireAnnouncesTheScheduledDrainCauseWithNoAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t, instant)
	schedule := wsm.DrainSchedule{Reason: deployReason(t), Deadline: instant, SetAt: instant}

	// Act
	if err := h.c.fire(context.Background(), schedule); err != nil {
		t.Fatalf("fire: %v", err)
	}

	// Assert
	pushed := h.announcer.Shutdowns()[0]
	if pushed.GetCause().GetScheduledDrain() == nil {
		t.Fatalf("cause = %v, want the scheduled_drain arm", pushed.GetCause())
	}
	if pushed.Address != nil {
		t.Fatalf("address = %q on a scheduled drain, want unset", *pushed.Address)
	}
}

func TestFireExitsOnceEveryWorkspaceIsQuiet(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t, instant)
	schedule := wsm.DrainSchedule{Reason: deployReason(t), Deadline: instant, SetAt: instant}

	// Act
	if err := h.c.fire(context.Background(), schedule); err != nil {
		t.Fatalf("fire: %v", err)
	}

	// Assert
	select {
	case <-h.exits:
	default:
		t.Fatalf("the orderly exit was never started")
	}
}

func TestFireReleasesTheDrainHoldsBeforeExiting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	schedule := wsm.DrainSchedule{Reason: deployReason(t), Deadline: instant, SetAt: instant}

	// Act
	if err := h.c.fire(context.Background(), schedule); err != nil {
		t.Fatalf("fire: %v", err)
	}
	_, held, err := h.db.Lease(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if held {
		t.Fatalf("the drain hold is still held after the exit was started")
	}
}

func TestResolveIdleCutoffLetsTheEnvironmentBeatTheFlag(t *testing.T) {
	// Arrange
	t.Setenv(IdleCutoffEnv, "250")

	// Act
	got, err := ResolveIdleCutoff(time.Hour)

	// Assert
	if err != nil {
		t.Fatalf("ResolveIdleCutoff: %v", err)
	}
	if got != 250*time.Millisecond {
		t.Fatalf("cutoff = %v, want the environment's 250ms", got)
	}
}

func TestResolveIdleCutoffRefusesAMalformedEnvironmentValue(t *testing.T) {
	// Arrange
	t.Setenv(IdleCutoffEnv, "soon")

	// Act
	_, err := ResolveIdleCutoff(time.Hour)

	// Assert
	if err == nil {
		t.Fatalf("ResolveIdleCutoff accepted a value that is not a whole number of milliseconds")
	}
}

func TestResolveIdleCutoffFallsBackToTheFlag(t *testing.T) {
	// Arrange
	t.Setenv(IdleCutoffEnv, "")

	// Act
	got, err := ResolveIdleCutoff(90 * time.Minute)

	// Assert
	if err != nil {
		t.Fatalf("ResolveIdleCutoff: %v", err)
	}
	if got != 90*time.Minute {
		t.Fatalf("cutoff = %v, want the flag's 90m", got)
	}
}

func TestResolveIdleCutoffFallsBackToTheDefault(t *testing.T) {
	// Arrange
	t.Setenv(IdleCutoffEnv, "")

	// Act
	got, err := ResolveIdleCutoff(0)

	// Assert
	if err != nil {
		t.Fatalf("ResolveIdleCutoff: %v", err)
	}
	if got != DefaultIdleCutoff {
		t.Fatalf("cutoff = %v, want the default %v", got, DefaultIdleCutoff)
	}
}

// TestSchedulingTakesTheDrainHoldOnEveryWorkspace pins that intake is held from
// the moment the shutdown is announced, not from its deadline: a prompt
// submitted meanwhile would otherwise be delivered into a session the daemon is
// about to stand down.
func TestSchedulingTakesTheDrainHoldOnEveryWorkspace(t *testing.T) {
	// Arrange
	var told []ids.WorkspaceID
	h := newHarness(t, func(d *Deps) {
		d.LeaseChanged = func(ws ids.WorkspaceID) { told = append(told, ws) }
	})
	ws := h.workspace(t, instant)

	// Act
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Hour), SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Assert
	lease, held, err := h.db.Lease(context.Background(), ws)
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if !held || lease.Holder != wsm.HolderDrain || lease.Policy != wsm.PolicyHold {
		t.Fatalf("lease = (%+v, %v), want a drain lease holding the intake", lease, held)
	}
	if len(told) != 1 || told[0] != ws {
		t.Fatalf("LeaseChanged calls = %v, want the held workspace %q", told, ws)
	}
}

// TestCancellingAScheduleReleasesItsDrainHolds is the other half: a cancelled
// shutdown must not leave the intake held, or every workspace refuses prompts
// forever for a drain that is not coming.
func TestCancellingAScheduleReleasesItsDrainHolds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Hour), SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act
	if err := h.c.Cancel(context.Background()); err != nil {
		t.Fatalf("Cancel: %v", err)
	}

	// Assert
	if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
		t.Fatalf("Lease after the cancel = (held %v, %v), want no lease", held, err)
	}
}

func TestRepublishAnnouncesAScheduleThatOutlivedTheProcessThatArmedIt(t *testing.T) {
	// Arrange: a schedule persisted by an earlier process, so the topic this
	// process owns has never carried its banner.
	h := newHarness(t)
	deadline := instant.Add(time.Hour)
	if err := h.db.PutDrainSchedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: deadline, SetAt: instant,
	}); err != nil {
		t.Fatalf("PutDrainSchedule: %v", err)
	}

	// Act
	err := h.c.Republish(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Republish: %v", err)
	}
	pushed := h.announcer.Scheduled()
	if len(pushed) != 1 {
		t.Fatalf("drain_scheduled pushes = %d, want exactly the republished banner", len(pushed))
	}
	if pushed[0].GetAtMs() != deadline.UnixMilli() {
		t.Fatalf("drain_scheduled.at_ms = %d, want the persisted deadline %d", pushed[0].GetAtMs(), deadline.UnixMilli())
	}
}

func TestRepublishAnnouncesNothingWhenNoScheduleSurvivedTheRestart(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.c.Republish(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Republish with nothing scheduled = %v, want no error", err)
	}
	if pushed := h.announcer.Scheduled(); len(pushed) != 0 {
		t.Fatalf("drain_scheduled pushes = %d, want none", len(pushed))
	}
}

func TestRepublishRetakesTheIntakeHoldTheStandingScheduleOwns(t *testing.T) {
	// Arrange: a workspace and a schedule the previous process left behind.
	h := newHarness(t)
	ws := h.workspace(t, instant)
	if err := h.db.PutDrainSchedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Hour), SetAt: instant,
	}); err != nil {
		t.Fatalf("PutDrainSchedule: %v", err)
	}

	// Act
	if err := h.c.Republish(context.Background()); err != nil {
		t.Fatalf("Republish: %v", err)
	}

	// Assert: the workspace's lease is held by the drain again.
	lease, held, err := h.db.Lease(context.Background(), ws)
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if !held || lease.Holder != wsm.HolderDrain {
		t.Fatalf("lease = %+v held=%v, want the drain holding it after the republish", lease, held)
	}
}

func TestRepublishRefusesAPersistedScheduleWhoseReasonWillNotDecode(t *testing.T) {
	// Arrange: a row whose reason is not a decodable DrainReason.
	h := newHarness(t)
	if err := h.db.PutDrainSchedule(context.Background(), wsm.DrainSchedule{
		Reason: "{not json", Deadline: instant.Add(time.Hour), SetAt: instant,
	}); err != nil {
		t.Fatalf("PutDrainSchedule: %v", err)
	}

	// Act
	err := h.c.Republish(context.Background())

	// Assert
	if err == nil {
		t.Fatal("Republish over an undecodable reason = nil, want a loud refusal")
	}
	if pushed := h.announcer.Scheduled(); len(pushed) != 0 {
		t.Fatalf("drain_scheduled pushes = %d, want none from a corrupt row", len(pushed))
	}
}

// TestShutdownNowStandsDownASpawnStillInFlight is the leak's own case: a shim
// that has been spawned and has not finished coming up is in NO workspace's
// session map, so the walk over the registered sessions steps past it and it
// outlives the daemon holding the workspace lock. The supervisor's sweep is
// what reaches it.
func TestShutdownNowStandsDownASpawnStillInFlight(t *testing.T) {
	// Arrange: no workspace has a registered session at all, which is exactly
	// the state a spawn in flight leaves behind.
	h := newHarness(t)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	if reasons := h.spawns.Reasons(); len(reasons) != 1 {
		t.Fatalf("spawn sweeps = %d, want exactly 1; a spawn in flight is nothing else's to stand down", len(reasons))
	}
}

// TestShutdownNowSweepsTheSpawnsOnlyAfterTheRegisteredSessions pins the order:
// a registered session still goes down the ORDINARY way, through the fleet,
// and the sweep catches only what that walk could not reach.
func TestShutdownNowSweepsTheSpawnsOnlyAfterTheRegisteredSessions(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t, instant)
	h.workspace(t, instant)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	seen := h.spawns.Seen()
	if len(seen) != 1 {
		t.Fatalf("spawn sweeps = %d, want exactly 1", len(seen))
	}
	if seen[0] != 2 {
		t.Fatalf("the sweep ran after %d of 2 registered stand-downs; it must follow the ordinary path, never replace it", seen[0])
	}
}

// TestShutdownNowReportsASpawnThatWouldNotStandDown pins the failure policy
// for the sweep, which is the same as for the ordinary walk: the leak is
// RECORDED at ERROR and the exit happens anyway, because an exit skipped over
// one stubborn shim leaks the daemon too.
func TestShutdownNowReportsASpawnThatWouldNotStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.spawns.fail(errFake)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	var reported bool
	for _, rec := range records(h.log, opNow) {
		if rec.Level == "error" && strings.Contains(rec.Context["cause"].(string), errFake.Error()) {
			reported = true
		}
	}
	if !reported {
		t.Fatalf("a spawn that would not stand down was swallowed; records = %v", records(h.log, opNow))
	}
	select {
	case <-h.exits:
	default:
		t.Fatalf("the orderly exit was never started after a spawn refused to stand down")
	}
}

// TestShutdownNowBoundsTheSpawnSweep pins the bound. The sweep runs on the
// request's own goroutine ahead of the exit, so an unbounded one is a daemon
// that never goes; it gets StandBound, the same budget one registered
// session's stand-down gets.
func TestShutdownNowBoundsTheSpawnSweep(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	bounds := h.spawns.Bounds()
	if len(bounds) != 1 {
		t.Fatalf("bounded sweeps = %d, want exactly 1; an unbounded sweep is a daemon that does not exit", len(bounds))
	}
	if bounds[0] <= 0 || bounds[0] > DefaultStandBound {
		t.Fatalf("sweep budget = %v, want a positive bound no larger than StandBound (%v)", bounds[0], DefaultStandBound)
	}
}

// TestTheScheduledDrainNeverSweepsTheSpawns is the BOUNCE-shaped contract at
// this seam: forcing is `now`'s alone. Everything graceful — the scheduled
// drain here, and the handover that hands its shims to an adopting successor —
// must leave a running process alone, and the sweep is never reached from
// them.
func TestTheScheduledDrainNeverSweepsTheSpawns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.freeness.SetFree(ws, true)

	// Act
	if err := h.c.fire(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant, SetAt: instant,
	}); err != nil {
		t.Fatalf("fire: %v", err)
	}

	// Assert
	if reasons := h.spawns.Reasons(); len(reasons) != 0 {
		t.Fatalf("the scheduled drain swept the spawns %v; only an immediate shutdown forces", reasons)
	}
}

// TestNewRefusesWithoutASpawnSweep pins that the sweep is REQUIRED wiring: a
// controller built without it cannot stand an in-flight spawn down, and that
// is the leak this seam exists to close.
func TestNewRefusesWithoutASpawnSweep(t *testing.T) {
	// Arrange & Act
	_, err := New(Deps{
		DB:        newHarness(t).db,
		Stand:     newFakeStand(),
		Freeness:  newFakeFreeness(),
		Announcer: &fakeAnnouncer{},
		Exit:      func(context.Context) error { return nil },
		Log:       dlog.NewTestSurfaces(),
	})

	// Assert
	if err == nil {
		t.Fatalf("New accepted a controller with no spawn sweep")
	}
}

// TestNewRefusesWithoutARevivalAnswer pins that the prompt queue's revival
// state is REQUIRED wiring: a sweep that cannot see a revival hibernates the
// session the revival is bringing up.
func TestNewRefusesWithoutARevivalAnswer(t *testing.T) {
	// Arrange & Act
	_, err := New(Deps{
		DB:        newHarness(t).db,
		Stand:     newFakeStand(),
		Spawns:    newFakeSpawns(),
		Freeness:  newFakeFreeness(),
		Announcer: &fakeAnnouncer{},
		Exit:      func(context.Context) error { return nil },
		Log:       dlog.NewTestSurfaces(),
	})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "revival") {
		t.Fatalf("New = %v, want the missing revival answer refused", err)
	}
}

// TestCancellingAScheduleRepublishesEveryHostView is the composer's other way
// back open. The host view's composer arm is composed from the occupancy
// lease, so every push taken while the schedule's drain hold stood said
// `draining`, and the server cannot see the cancellation release it — without
// this republish a cancelled shutdown leaves every host client's composer shut
// for a drain that is not coming.
func TestCancellingAScheduleRepublishesEveryHostView(t *testing.T) {
	// Arrange
	var published []ids.WorkspaceID
	h := newHarness(t, func(d *Deps) {
		d.PublishHost = func(ws ids.WorkspaceID) { published = append(published, ws) }
	})
	ws := h.workspace(t, instant)
	if err := h.c.Schedule(context.Background(), wsm.DrainSchedule{
		Reason: deployReason(t), Deadline: instant.Add(time.Hour), SetAt: instant,
	}); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act
	if err := h.c.Cancel(context.Background()); err != nil {
		t.Fatalf("Cancel: %v", err)
	}

	// Assert
	if len(published) != 1 || published[0] != ws {
		t.Fatalf("PublishHost calls = %v, want the released workspace %q republished", published, ws)
	}
}

// TestShutdownNowLatchesTheStandDownBeforeTheSessionWalk is the ordering the
// realtest's ERROR pair came from. The latch is the ONE signal every shim
// client reads to tell a departure this daemon ordered from one that happened
// to it, and the walk below it CAUSES departures -- so a latch raised only
// when the spawn sweep is reached is raised after every one of them.
func TestShutdownNowLatchesTheStandDownBeforeTheSessionWalk(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t, instant)
	h.workspace(t, instant)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	if at := h.spawns.LatchedAt(); at != 0 {
		t.Fatalf("the stand-down latched after %d session stand-downs, want it up before the walk begins", at)
	}
}

// TestShutdownNowLeavesTheStandDownLatched is the latch's other half: it never
// clears, because the process is exiting and there is no state after it in
// which a new spawn is wanted.
func TestShutdownNowLeavesTheStandDownLatched(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.c.ShutdownNow(context.Background(), maintenanceReason()); err != nil {
		t.Fatalf("ShutdownNow: %v", err)
	}

	// Assert
	if !h.spawns.StandingDown() {
		t.Fatal("the supervisor does not read as standing down after an immediate shutdown")
	}
}

// TestDefaultIdleCutoffIsTwelveHours pins the hibernation floor: a session goes
// unengaged for twelve hours before the idle sweep hibernates it, when no flag
// or environment override names a different cutoff.
func TestDefaultIdleCutoffIsTwelveHours(t *testing.T) {
	// Arrange
	want := 12 * time.Hour

	// Act
	got := DefaultIdleCutoff

	// Assert
	if got != want {
		t.Fatalf("DefaultIdleCutoff = %v, want %v", got, want)
	}
}
