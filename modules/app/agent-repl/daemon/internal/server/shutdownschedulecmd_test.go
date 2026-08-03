package server

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// fakeSchedules is a controllable ShutdownScheduleController, so the command
// handler's own behavior is tested apart from the lease engine's.
type fakeSchedules struct {
	mu         sync.Mutex
	id         string
	err        error
	cancelErr  error
	sawStop    bool
	sawCause   string
	cancelledA string
}

func (f *fakeSchedules) Schedule(stopShims bool, cause string) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.sawStop, f.sawCause = stopShims, cause
	return f.id, f.err
}

func (f *fakeSchedules) Cancel(scheduleID string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.cancelledA = scheduleID
	return f.cancelErr
}

// newScheduleHandler builds a command handler with the given schedule
// controller wired (nil leaves the capability unconfigured).
func newScheduleHandler(t *testing.T, schedules ShutdownScheduleController) *commandHandler {
	t.Helper()
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Schedules: schedules})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h
}

func TestScheduleShutdownIsALoudNackWhenUnconfigured(t *testing.T) {
	// A caller told its bounce was scheduled when nothing took a lease would
	// wait forever for a shutdown that is never coming.
	// Arrange.
	h := newScheduleHandler(t, nil)

	// Act.
	err := h.ScheduleShutdown(context.Background(), "/ws/a", "r1", &frontendv1.ScheduleShutdownCmd{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not supported") {
		t.Fatalf("ScheduleShutdown with no controller = %v, want a loud unsupported-capability refusal", err)
	}
}

func TestCancelScheduledShutdownIsALoudNackWhenUnconfigured(t *testing.T) {
	// Arrange.
	h := newScheduleHandler(t, nil)

	// Act.
	err := h.CancelScheduledShutdown(context.Background(), "/ws/a", "r1", &frontendv1.CancelScheduledShutdownCmd{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not supported") {
		t.Fatalf("CancelScheduledShutdown with no controller = %v, want a loud unsupported-capability refusal", err)
	}
}

func TestScheduleShutdownCarriesTheCommandsStopShims(t *testing.T) {
	// Arrange.
	sched := &fakeSchedules{id: "sd_1"}
	h := newScheduleHandler(t, sched)

	// Act.
	if err := h.ScheduleShutdown(context.Background(), "/ws/a", "r1",
		&frontendv1.ScheduleShutdownCmd{StopShims: true, Cause: "merge rebuilt the daemon"}); err != nil {
		t.Fatalf("ScheduleShutdown: %v", err)
	}

	// Assert.
	sched.mu.Lock()
	defer sched.mu.Unlock()
	if !sched.sawStop {
		t.Fatal("the schedule was taken without the command's stop_shims")
	}
}

func TestScheduleShutdownCarriesTheCommandsCause(t *testing.T) {
	// Arrange.
	sched := &fakeSchedules{id: "sd_1"}
	h := newScheduleHandler(t, sched)

	// Act.
	if err := h.ScheduleShutdown(context.Background(), "/ws/a", "r1",
		&frontendv1.ScheduleShutdownCmd{Cause: "merge rebuilt the daemon"}); err != nil {
		t.Fatalf("ScheduleShutdown: %v", err)
	}

	// Assert.
	sched.mu.Lock()
	defer sched.mu.Unlock()
	if sched.sawCause != "merge rebuilt the daemon" {
		t.Fatalf("schedule cause = %q, want the command's", sched.sawCause)
	}
}

func TestARefusedScheduleFailsTheCommand(t *testing.T) {
	// The ack is the caller's only report that the lease is actually theirs.
	// Arrange.
	sched := &fakeSchedules{err: fmt.Errorf("already scheduled")}
	h := newScheduleHandler(t, sched)

	// Act.
	err := h.ScheduleShutdown(context.Background(), "/ws/a", "r1", &frontendv1.ScheduleShutdownCmd{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "already scheduled") {
		t.Fatalf("ScheduleShutdown = %v, want the engine's refusal surfaced verbatim", err)
	}
}

func TestCancelScheduledShutdownCarriesTheCommandsScheduleID(t *testing.T) {
	// Arrange.
	sched := &fakeSchedules{}
	h := newScheduleHandler(t, sched)

	// Act.
	if err := h.CancelScheduledShutdown(context.Background(), "/ws/a", "r1",
		&frontendv1.CancelScheduledShutdownCmd{ScheduleId: "sd_7"}); err != nil {
		t.Fatalf("CancelScheduledShutdown: %v", err)
	}

	// Assert.
	sched.mu.Lock()
	defer sched.mu.Unlock()
	if sched.cancelledA != "sd_7" {
		t.Fatalf("cancelled schedule = %q, want sd_7", sched.cancelledA)
	}
}

func TestARefusedCancelFailsTheCommand(t *testing.T) {
	// Arrange.
	sched := &fakeSchedules{cancelErr: fmt.Errorf("stale schedule id")}
	h := newScheduleHandler(t, sched)

	// Act.
	err := h.CancelScheduledShutdown(context.Background(), "/ws/a", "r1",
		&frontendv1.CancelScheduledShutdownCmd{ScheduleId: "sd_stale"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "stale schedule id") {
		t.Fatalf("CancelScheduledShutdown = %v, want the engine's refusal surfaced verbatim", err)
	}
}

func TestTheConnectSnapshotCarriesTheDrainLease(t *testing.T) {
	// A client that connects mid-drain must see the lease without waiting for
	// an edge that may never come.
	// Arrange.
	h := newSchedulerHarness(t)
	h.holds.set(oneHold())
	if _, err := h.s.Schedule(true, "merge rebuilt the daemon"); err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	p := &ssmSnapshotProvider{shutdownSchedule: h.s, workspaceCreation: &fakeWorkspaceCreation{}}

	// Act.
	snap := p.Snapshot()

	// Assert.
	if snap.GetShutdownSchedule().GetDraining().GetScheduleId() != "sd_test" {
		t.Fatalf("snapshot shutdown_schedule = %v, want the live draining lease", snap.GetShutdownSchedule())
	}
}

func TestTheConnectSnapshotCarriesTheIdleLease(t *testing.T) {
	// Idle is a real value: a client must never confuse it with no information.
	// Arrange.
	h := newSchedulerHarness(t)
	p := &ssmSnapshotProvider{shutdownSchedule: h.s, workspaceCreation: &fakeWorkspaceCreation{}}

	// Act.
	snap := p.Snapshot()

	// Assert.
	if snap.GetShutdownSchedule().GetIdle() == nil {
		t.Fatalf("snapshot shutdown_schedule = %v, want the idle arm", snap.GetShutdownSchedule())
	}
}
