package server

import (
	"fmt"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/statedb"
)

// --- doubles ----------------------------------------------------------------

// fakeScheduleStore is an in-memory ShutdownScheduleStore whose write and read
// can each be made to fail.
type fakeScheduleStore struct {
	mu       sync.Mutex
	rec      statedb.ShutdownSchedule
	held     bool
	putErr   error
	readErr  error
	clearErr error
	cleared  int
}

func (f *fakeScheduleStore) PutSchedule(rec statedb.ShutdownSchedule) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.putErr != nil {
		return f.putErr
	}
	f.rec, f.held = rec, true
	return nil
}

func (f *fakeScheduleStore) Schedule() (statedb.ShutdownSchedule, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.readErr != nil {
		return statedb.ShutdownSchedule{}, false, f.readErr
	}
	return f.rec, f.held, nil
}

func (f *fakeScheduleStore) ClearSchedule() (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.cleared++
	if f.clearErr != nil {
		return false, f.clearErr
	}
	was := f.held
	f.held = false
	return was, nil
}

func (f *fakeScheduleStore) isHeld() bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.held
}

// fakeHoldSource is a controllable DrainHoldSource. It records the bind and the
// acquire/release calls, which is how a test proves the lease reached the fleet.
type fakeHoldSource struct {
	mu        sync.Mutex
	holds     []sessioncontroller.DrainHold
	bound     sessioncontroller.ShutdownLease
	bindErr   error
	acquired  []string
	released  []string
	acquireN  int
	holdsRead int
}

func (f *fakeHoldSource) DrainHolds(sessioncontroller.LiveTaskCounter) []sessioncontroller.DrainHold {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.holdsRead++
	return append([]sessioncontroller.DrainHold(nil), f.holds...)
}

func (f *fakeHoldSource) BindShutdownLease(l sessioncontroller.ShutdownLease) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.bindErr != nil {
		return f.bindErr
	}
	f.bound = l
	return nil
}

func (f *fakeHoldSource) AcquireShutdownHolds(scheduleID string) int {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.acquired = append(f.acquired, scheduleID)
	return f.acquireN
}

func (f *fakeHoldSource) ReleaseShutdownHolds(scheduleID string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.released = append(f.released, scheduleID)
}

func (f *fakeHoldSource) set(holds ...sessioncontroller.DrainHold) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.holds = holds
}

func (f *fakeHoldSource) releases() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.released...)
}

func (f *fakeHoldSource) boundLease() sessioncontroller.ShutdownLease {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.bound
}

// fakeTaskCounter answers the live-task half of a hold. The engine only passes
// it through to DrainHolds, so the fake need do nothing else.
type fakeTaskCounter struct{}

func (fakeTaskCounter) LiveTasks(string) (int64, bool) { return 0, false }

// schedulerHarness is a ShutdownScheduler over controllable doubles.
type schedulerHarness struct {
	t        *testing.T
	s        *ShutdownScheduler
	store    *fakeScheduleStore
	holds    *fakeHoldSource
	mu       sync.Mutex
	views    []*frontendv1.ShutdownScheduleView
	stops    []bool
	stopped  chan struct{}
	stopOnce sync.Once
	logLines []string
}

func newSchedulerHarness(t *testing.T) *schedulerHarness {
	t.Helper()
	h := &schedulerHarness{
		t: t, store: &fakeScheduleStore{}, holds: &fakeHoldSource{},
		stopped: make(chan struct{}),
	}
	s, err := NewShutdownScheduler(ShutdownSchedulerConfig{
		Store: h.store, Holds: h.holds, LiveTasks: fakeTaskCounter{},
		Broadcast: func(v *frontendv1.ShutdownScheduleView) {
			h.mu.Lock()
			h.views = append(h.views, v)
			h.mu.Unlock()
		},
		Shutdown: func(stopShims bool) {
			h.mu.Lock()
			h.stops = append(h.stops, stopShims)
			h.mu.Unlock()
			h.stopOnce.Do(func() { close(h.stopped) })
		},
		Logf: func(format string, args ...any) {
			h.mu.Lock()
			h.logLines = append(h.logLines, fmt.Sprintf(format, args...))
			h.mu.Unlock()
		},
		Now:           func() int64 { return 1234 },
		NewScheduleID: func() string { return "sd_test" },
	})
	if err != nil {
		t.Fatalf("NewShutdownScheduler: %v", err)
	}
	h.s = s
	return h
}

func (h *schedulerHarness) lastView() *frontendv1.ShutdownScheduleView {
	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.views) == 0 {
		return nil
	}
	return h.views[len(h.views)-1]
}

func (h *schedulerHarness) viewCount() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return len(h.views)
}

func (h *schedulerHarness) shutdowns() []bool {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]bool(nil), h.stops...)
}

func (h *schedulerHarness) log() string {
	h.mu.Lock()
	defer h.mu.Unlock()
	return strings.Join(h.logLines, "\n")
}

// oneHold is a workspace holding the drain with a turn in flight.
func oneHold() sessioncontroller.DrainHold {
	return sessioncontroller.DrainHold{Workspace: "/ws/a", SessionID: "s1", TurnActive: true, TurnID: "t_1"}
}

// --- construction -----------------------------------------------------------

func TestNewShutdownSchedulerRequiresEachDependency(t *testing.T) {
	// Arrange.
	full := func() ShutdownSchedulerConfig {
		return ShutdownSchedulerConfig{
			Store: &fakeScheduleStore{}, Holds: &fakeHoldSource{}, LiveTasks: fakeTaskCounter{},
			Broadcast: func(*frontendv1.ShutdownScheduleView) {}, Shutdown: func(bool) {},
		}
	}
	tests := []struct {
		name string
		drop func(*ShutdownSchedulerConfig)
		want string
	}{
		{"no store", func(c *ShutdownSchedulerConfig) { c.Store = nil }, "durable store"},
		{"no holds", func(c *ShutdownSchedulerConfig) { c.Holds = nil }, "drain-hold source"},
		{"no live tasks", func(c *ShutdownSchedulerConfig) { c.LiveTasks = nil }, "live-task counter"},
		{"no broadcast", func(c *ShutdownSchedulerConfig) { c.Broadcast = nil }, "broadcast func"},
		{"no shutdown", func(c *ShutdownSchedulerConfig) { c.Shutdown = nil }, "graceful-shutdown func"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cfg := full()
			tc.drop(&cfg)

			// Act.
			_, err := NewShutdownScheduler(cfg)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("NewShutdownScheduler = %v, want a refusal naming %q", err, tc.want)
			}
		})
	}
}

func TestConstructionBindsTheEngineToTheFleet(t *testing.T) {
	// An engine the queue never heard of would hold a lease nothing enforces.
	// Arrange.
	h := newSchedulerHarness(t)

	// Act.
	got := h.holds.boundLease()

	// Assert.
	if got == nil {
		t.Fatal("NewShutdownScheduler did not bind the lease to the fleet")
	}
}

func TestConstructionFailsWhenTheFleetRefusesTheBinding(t *testing.T) {
	// Arrange.
	holds := &fakeHoldSource{bindErr: fmt.Errorf("boom")}

	// Act.
	_, err := NewShutdownScheduler(ShutdownSchedulerConfig{
		Store: &fakeScheduleStore{}, Holds: holds, LiveTasks: fakeTaskCounter{},
		Broadcast: func(*frontendv1.ShutdownScheduleView) {}, Shutdown: func(bool) {},
	})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "binding") {
		t.Fatalf("NewShutdownScheduler = %v, want the bind failure surfaced", err)
	}
}

// --- the idle view ----------------------------------------------------------

func TestAnIdleSchedulerReportsTheIdleArm(t *testing.T) {
	// Idle is a REAL value, never an absent field.
	// Arrange.
	h := newSchedulerHarness(t)

	// Act.
	view := h.s.View()

	// Assert.
	if view.GetIdle() == nil {
		t.Fatalf("View = %v, want the idle arm set", view)
	}
}

func TestAnIdleSchedulerHoldsNoLease(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)

	// Act.
	id, held := h.s.HeldSchedule()

	// Assert.
	if held || id != "" {
		t.Fatalf("HeldSchedule = %q, %v; want no lease", id, held)
	}
}

// --- scheduling -------------------------------------------------------------

func TestSchedulingTakesTheLease(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())

	// Act.
	id, err := h.s.Schedule(true, "merge rebuilt the daemon")

	// Assert.
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	got, held := h.s.HeldSchedule()
	if !held || got != id {
		t.Fatalf("HeldSchedule = %q, %v; want the minted %q", got, held, id)
	}
}

func TestSchedulingRecordsTheLeaseDurablyBeforeBroadcasting(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())

	// Act.
	if _, err := h.s.Schedule(true, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Assert.
	if !h.store.isHeld() {
		t.Fatal("the lease was not recorded durably; a crash would erase it silently")
	}
}

func TestADurableWriteFailureReleasesTheLease(t *testing.T) {
	// The daemon must not stand on a promise it cannot keep.
	// Arrange.
	h := newSchedulerHarness(t)
	h.store.putErr = fmt.Errorf("disk full")

	// Act.
	_, err := h.s.Schedule(true, "cause")

	// Assert.
	if err == nil {
		t.Fatal("Schedule succeeded over a failed durable write, want a refusal")
	}
	if _, held := h.s.HeldSchedule(); held {
		t.Fatal("the lease is still held after its durable write failed")
	}
}

func TestSchedulingParksThePromptsAlreadyQueued(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())

	// Act.
	id, err := h.s.Schedule(false, "cause")
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Assert.
	h.holds.mu.Lock()
	acquired := append([]string(nil), h.holds.acquired...)
	h.holds.mu.Unlock()
	if len(acquired) != 1 || acquired[0] != id {
		t.Fatalf("AcquireShutdownHolds calls = %v, want exactly the new schedule %q", acquired, id)
	}
}

func TestSchedulingBroadcastsTheDrainingViewWithItsHolds(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())

	// Act.
	if _, err := h.s.Schedule(true, "merge rebuilt the daemon"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Assert.
	draining := h.lastView().GetDraining()
	if draining == nil {
		t.Fatalf("last view = %v, want the draining arm", h.lastView())
	}
	if len(draining.GetHolds()) != 1 || draining.GetHolds()[0].GetWorkspace() != "/ws/a" {
		t.Fatalf("draining holds = %v, want the one workspace holding the drain", draining.GetHolds())
	}
}

func TestTheDrainingViewCarriesTheScheduleFacts(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())

	// Act.
	if _, err := h.s.Schedule(true, "merge rebuilt the daemon"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Assert.
	d := h.lastView().GetDraining()
	if d.GetScheduleId() != "sd_test" || d.GetScheduledAtMs() != 1234 ||
		d.GetCause() != "merge rebuilt the daemon" || !d.GetStopShims() {
		t.Fatalf("draining = %+v, want the schedule's own facts", d)
	}
}

func TestASecondScheduleIsALoudNack(t *testing.T) {
	// Never a silent replace: two deploy flows must not merge their intents.
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(true, "first"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	_, err := h.s.Schedule(false, "second")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "already scheduled") {
		t.Fatalf("second Schedule = %v, want a loud already-scheduled refusal", err)
	}
}

func TestARefusedSecondScheduleLeavesTheFirstStanding(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	first, err := h.s.Schedule(true, "first")
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	if _, err := h.s.Schedule(false, "second"); err == nil {
		t.Fatal("second Schedule succeeded, want a refusal")
	}

	// Assert.
	got, held := h.s.HeldSchedule()
	if !held || got != first {
		t.Fatalf("HeldSchedule = %q, %v; want the first schedule %q intact", got, held, first)
	}
}

// --- cancelling -------------------------------------------------------------

func TestCancellingReleasesTheLease(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	id, err := h.s.Schedule(true, "cause")
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	if err := h.s.Cancel(id); err != nil {
		t.Fatalf("Cancel: %v", err)
	}

	// Assert.
	if _, held := h.s.HeldSchedule(); held {
		t.Fatal("the lease is still held after a successful cancel")
	}
}

func TestCancellingBroadcastsIdle(t *testing.T) {
	// Clearing the lease must be representable on the wire.
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	id, err := h.s.Schedule(true, "cause")
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	if err := h.s.Cancel(id); err != nil {
		t.Fatalf("Cancel: %v", err)
	}

	// Assert.
	if h.lastView().GetIdle() == nil {
		t.Fatalf("last view = %v, want the idle arm broadcast", h.lastView())
	}
}

func TestCancellingFreesTheParkedPrompts(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	id, err := h.s.Schedule(true, "cause")
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	if err := h.s.Cancel(id); err != nil {
		t.Fatalf("Cancel: %v", err)
	}

	// Assert.
	if got := h.holds.releases(); len(got) != 1 || got[0] != id {
		t.Fatalf("ReleaseShutdownHolds calls = %v, want exactly %q", got, id)
	}
}

func TestCancellingClearsTheDurableRow(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	id, err := h.s.Schedule(true, "cause")
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	if err := h.s.Cancel(id); err != nil {
		t.Fatalf("Cancel: %v", err)
	}

	// Assert.
	if h.store.isHeld() {
		t.Fatal("the durable lease row survived a cancel; the next boot would restore a cancelled schedule")
	}
}

func TestCancellingAStaleScheduleIDIsALoudNack(t *testing.T) {
	// A cancel aimed at an old schedule must never kill a newer one.
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(true, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	err := h.s.Cancel("sd_stale")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "sd_stale") {
		t.Fatalf("Cancel(stale) = %v, want a loud refusal naming the stale id", err)
	}
}

func TestARefusedStaleCancelLeavesTheLeaseStanding(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	id, err := h.s.Schedule(true, "cause")
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	if err := h.s.Cancel("sd_stale"); err == nil {
		t.Fatal("Cancel(stale) succeeded, want a refusal")
	}

	// Assert.
	got, held := h.s.HeldSchedule()
	if !held || got != id {
		t.Fatalf("HeldSchedule = %q, %v; want the live schedule %q intact", got, held, id)
	}
}

func TestCancellingWithNoScheduleHeldIsALoudNack(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)

	// Act.
	err := h.s.Cancel("sd_anything")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no shutdown is scheduled") {
		t.Fatalf("Cancel on an idle scheduler = %v, want a loud refusal", err)
	}
}

// --- the drain --------------------------------------------------------------

func TestTheShutdownDoesNotRunWhileAHoldStands(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())

	// Act.
	if _, err := h.s.Schedule(true, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	h.s.NoteDrainActivity()

	// Assert.
	if got := h.shutdowns(); len(got) != 0 {
		t.Fatalf("shutdown ran %v times while a hold stood, want 0", got)
	}
}

func TestTheShutdownRunsWhenTheLastHoldClears(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(true, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	h.holds.set()
	h.s.NoteDrainActivity()

	// Assert.
	<-h.stopped
	if got := h.shutdowns(); len(got) != 1 {
		t.Fatalf("shutdown ran %v, want exactly once", got)
	}
}

func TestTheExecutedShutdownHonorsTheSchedulesStopShims(t *testing.T) {
	// The decision is a property of WHAT was rebuilt, fixed at schedule time.
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(true, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	h.holds.set()
	h.s.NoteDrainActivity()

	// Assert.
	<-h.stopped
	if got := h.shutdowns(); got[0] != true {
		t.Fatalf("shutdown stop_shims = %v, want the schedule's true", got[0])
	}
}

func TestADrainedScheduleClearsItsDurableRowBeforeBouncing(t *testing.T) {
	// A lease surviving its own shutdown would block the next daemon forever.
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(false, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	h.holds.set()
	h.s.NoteDrainActivity()

	// Assert.
	<-h.stopped
	if h.store.isHeld() {
		t.Fatal("the durable lease row survived the drain; the next boot would restore a completed schedule")
	}
}

func TestADrainedScheduleBroadcastsIdle(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(false, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	h.holds.set()
	h.s.NoteDrainActivity()

	// Assert.
	<-h.stopped
	if h.lastView().GetIdle() == nil {
		t.Fatalf("last view = %v, want idle after the drain completed", h.lastView())
	}
}

func TestAnUnchangedHoldsListDoesNotRebroadcast(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(false, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	before := h.viewCount()

	// Act.
	h.s.NoteDrainActivity()

	// Assert.
	if h.viewCount() != before {
		t.Fatalf("view count went %d -> %d on an unchanged holds list, want no redundant frame", before, h.viewCount())
	}
}

func TestAChangedHoldsListRebroadcasts(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold(), sessioncontroller.DrainHold{Workspace: "/ws/b", SessionID: "s2", LiveTasks: 2})
	if _, err := h.s.Schedule(false, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	before := h.viewCount()

	// Act.
	h.holds.set(oneHold())
	h.s.NoteDrainActivity()

	// Assert.
	if h.viewCount() <= before {
		t.Fatalf("view count stayed %d after a hold cleared, want a rebroadcast", before)
	}
}

func TestActivityOnAnIdleSchedulerBroadcastsNothing(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())

	// Act.
	h.s.NoteDrainActivity()

	// Assert.
	if h.viewCount() != 0 {
		t.Fatalf("view count = %d with no lease held, want 0", h.viewCount())
	}
}

func TestCancellingADrainedScheduleIsRefused(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	id, err := h.s.Schedule(false, "cause")
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	h.holds.set()
	h.s.NoteDrainActivity()
	<-h.stopped

	// Act.
	err = h.s.Cancel(id)

	// Assert.
	if err == nil {
		t.Fatal("Cancel of a schedule whose shutdown already ran succeeded, want a refusal")
	}
}

// --- hold translation -------------------------------------------------------

func TestATurnOnlyHoldSetsOnlyTheTurnArm(t *testing.T) {
	// Arrange.
	holds := []sessioncontroller.DrainHold{{Workspace: "/ws/a", SessionID: "s1", TurnActive: true, TurnID: "t_1"}}

	// Act.
	got := holdViews(holds)

	// Assert.
	if len(got) != 1 || got[0].GetTurn().GetTurnId() != "t_1" || got[0].GetTasks() != nil {
		t.Fatalf("holdViews = %v, want a turn-only hold naming t_1", got)
	}
}

func TestATaskOnlyHoldSetsOnlyTheTasksArm(t *testing.T) {
	// Arrange.
	holds := []sessioncontroller.DrainHold{{Workspace: "/ws/a", SessionID: "s1", LiveTasks: 3}}

	// Act.
	got := holdViews(holds)

	// Assert.
	if len(got) != 1 || got[0].GetTurn() != nil || got[0].GetTasks().GetCount() != 3 {
		t.Fatalf("holdViews = %v, want a task-only hold of 3", got)
	}
}

func TestAHoldWithBothFactsSetsBothArms(t *testing.T) {
	// Siblings, not alternatives.
	// Arrange.
	holds := []sessioncontroller.DrainHold{{Workspace: "/ws/a", SessionID: "s1", TurnActive: true, TurnID: "t_1", LiveTasks: 2}}

	// Act.
	got := holdViews(holds)

	// Assert.
	if len(got) != 1 || got[0].GetTurn() == nil || got[0].GetTasks().GetCount() != 2 {
		t.Fatalf("holdViews = %v, want both arms set", got)
	}
}

func TestAnAdoptedTurnsHoldSetsTheTurnArmWithAnEmptyID(t *testing.T) {
	// The arm being set IS the fact that a turn is running.
	// Arrange.
	holds := []sessioncontroller.DrainHold{{Workspace: "/ws/a", SessionID: "s1", TurnActive: true}}

	// Act.
	got := holdViews(holds)

	// Assert.
	if len(got) != 1 || got[0].GetTurn() == nil || got[0].GetTurn().GetTurnId() != "" {
		t.Fatalf("holdViews = %v, want the turn arm set with an empty id", got)
	}
}

func TestAHoldExplainingNothingIsDropped(t *testing.T) {
	// The proto requires at least one arm.
	// Arrange.
	holds := []sessioncontroller.DrainHold{{Workspace: "/ws/a", SessionID: "s1"}}

	// Act.
	got := holdViews(holds)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("holdViews = %v, want the empty hold dropped", got)
	}
}

// --- restore ----------------------------------------------------------------

func TestRestoreOnAStoreWithNoScheduleLeavesTheDaemonIdle(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)

	// Act.
	if err := h.s.Restore(); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Assert.
	if _, held := h.s.HeldSchedule(); held {
		t.Fatal("Restore took a lease with no durable schedule to restore")
	}
}

func TestRestoreRetakesADurableLease(t *testing.T) {
	// The deploy that asked for the bounce is still waiting for it.
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if err := h.store.PutSchedule(statedb.ShutdownSchedule{
		ScheduleID: "sd_prev", ScheduledAtMs: 99, Cause: "previous daemon", StopShims: true,
	}); err != nil {
		t.Fatalf("PutSchedule: %v", err)
	}

	// Act.
	if err := h.s.Restore(); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Assert.
	got, held := h.s.HeldSchedule()
	if !held || got != "sd_prev" {
		t.Fatalf("HeldSchedule = %q, %v; want the restored sd_prev", got, held)
	}
}

func TestRestoreRebroadcastsTheDrainingView(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if err := h.store.PutSchedule(statedb.ShutdownSchedule{ScheduleID: "sd_prev", Cause: "previous daemon"}); err != nil {
		t.Fatalf("PutSchedule: %v", err)
	}

	// Act.
	if err := h.s.Restore(); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Assert.
	if h.lastView().GetDraining().GetScheduleId() != "sd_prev" {
		t.Fatalf("last view = %v, want the restored schedule rebroadcast", h.lastView())
	}
}

func TestRestoreExecutesTheShutdownWhenNothingHoldsTheDrain(t *testing.T) {
	// The crash may have taken the last hold with it.
	// Arrange.
	h := newSchedulerHarness(t)
	if err := h.store.PutSchedule(statedb.ShutdownSchedule{ScheduleID: "sd_prev", StopShims: true}); err != nil {
		t.Fatalf("PutSchedule: %v", err)
	}

	// Act.
	if err := h.s.Restore(); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Assert.
	<-h.stopped
	if got := h.shutdowns(); len(got) != 1 || !got[0] {
		t.Fatalf("shutdown = %v, want one honoring the restored stop_shims", got)
	}
}

func TestRestoreSurfacesAnUnreadableStore(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.store.readErr = fmt.Errorf("corrupt")

	// Act.
	err := h.s.Restore()

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "corrupt") {
		t.Fatalf("Restore = %v, want the read failure surfaced", err)
	}
}

func TestRestoreRefusesToRunOverALiveSchedule(t *testing.T) {
	// Restore runs once, at boot, before anything can schedule.
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(false, "live"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	err := h.s.Restore()

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "live schedule") {
		t.Fatalf("Restore over a live lease = %v, want a refusal", err)
	}
}

// --- provenance -------------------------------------------------------------

func TestLeaseProvenanceReportsNoLeaseWhenIdle(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)

	// Act.
	_, _, held := h.s.LeaseProvenance()

	// Assert.
	if held {
		t.Fatal("LeaseProvenance reported a held lease on an idle scheduler")
	}
}

func TestLeaseProvenanceNamesTheScheduleAndCause(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(false, "merge rebuilt the daemon"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	id, cause, held := h.s.LeaseProvenance()

	// Assert.
	if !held || id != "sd_test" || cause != "merge rebuilt the daemon" {
		t.Fatalf("LeaseProvenance = %q, %q, %v; want the live schedule's facts", id, cause, held)
	}
}

func TestTheDrainLogsWhoStoppedTheShimsAndWhy(t *testing.T) {
	// A log trace alone must answer "why did this shim stop".
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(true, "merge rebuilt the daemon"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	h.holds.set()
	h.s.NoteDrainActivity()
	<-h.stopped

	// Assert.
	log := h.log()
	for _, want := range []string{"SHIM STOP DECIDED", "initiator=scheduled_shutdown", "sd_test", "merge rebuilt the daemon"} {
		if !strings.Contains(log, want) {
			t.Fatalf("daemon log is missing %q; got:\n%s", want, log)
		}
	}
}

func TestAPreservingDrainLogsThatTheShimsWereKept(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(false, "manual restart"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Act.
	h.holds.set()
	h.s.NoteDrainActivity()
	<-h.stopped

	// Assert.
	if !strings.Contains(h.log(), "SHIM STOP DECLINED") {
		t.Fatalf("daemon log does not record that the shims were preserved; got:\n%s", h.log())
	}
}

func TestAHoldingDrainLogsWhatItIsWaitingOn(t *testing.T) {
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())

	// Act.
	if _, err := h.s.Schedule(false, "cause"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}

	// Assert.
	log := h.log()
	for _, want := range []string{"drain HOLDING", "/ws/a", "t_1", "drain_timeout=none"} {
		if !strings.Contains(log, want) {
			t.Fatalf("daemon log is missing %q; got:\n%s", want, log)
		}
	}
}
