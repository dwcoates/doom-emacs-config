package server

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"syscall"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"
)

// --- SessionLocator -------------------------------------------------------

func TestSessionLocatorPicksNewestNonTerminalForWorkspace(t *testing.T) {
	// Arrange — two live sessions on /w (older + newer) plus a terminal one
	// and one on a different workspace.
	reg := openTestRegistry(t)
	put := func(id, cwd, created string, terminal bool) {
		if err := reg.Put(registry.Record{SessionID: id, CWD: cwd, CreatedAt: created, Terminal: terminal}); err != nil {
			t.Fatalf("put %s: %v", id, err)
		}
	}
	put("s_old", "/w", "2026-07-23T10:00:00Z", false)
	put("s_new", "/w", "2026-07-23T12:00:00Z", false)
	put("s_dead", "/w", "2026-07-23T13:00:00Z", true)
	put("s_other", "/other", "2026-07-23T14:00:00Z", false)
	loc := &SessionLocator{Reg: reg}

	// Act
	got, ok := loc.Locate("/w")

	// Assert — the newest NON-terminal record on /w wins.
	if !ok || got != "s_new" {
		t.Fatalf("Locate(/w) = %q,%v; want s_new,true", got, ok)
	}
}

func TestSessionLocatorMissForUnknownWorkspace(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", CreatedAt: "2026-07-23T10:00:00Z"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	loc := &SessionLocator{Reg: reg}

	// Act
	_, ok := loc.Locate("/nope")

	// Assert
	if ok {
		t.Fatal("Locate for an unknown workspace should miss")
	}
}

// --- ShimSpawner ----------------------------------------------------------

func TestShimSpawnerDoesNotSpawnWhenTheShimIsConnected(t *testing.T) {
	// Arrange — the listener already has this session's shim, so there is
	// nothing to bring up.
	reg := openTestRegistry(t)
	spawned := 0
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return true, nil },
		nil,
		func(string, CreateOpts) (ShimHandle, error) { spawned++; return ShimHandle{}, nil },
		nil)

	// Act
	_, err := sp.EnsureShim(context.Background(), "s1")

	// Assert
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if spawned != 0 {
		t.Fatalf("a connected shim must not be re-spawned (spawned=%d)", spawned)
	}
}

func TestShimSpawnerRefusesToGuessWhenConnectionProbeFails(t *testing.T) {
	// Arrange — an unreadable listener state is neither connected nor absent.
	reg := openTestRegistry(t)
	spawned := 0
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, errors.New("probe unavailable") },
		nil,
		func(string, CreateOpts) (ShimHandle, error) { spawned++; return ShimHandle{}, nil },
		nil)

	// Act.
	_, err := sp.EnsureShim(context.Background(), "s1")

	// Assert — spawning on unknown liveness could create two transcript writers.
	if err == nil || !strings.Contains(err.Error(), "cannot determine whether a shim is connected") {
		t.Fatalf("EnsureShim error = %v", err)
	}
	if spawned != 0 {
		t.Fatalf("connection-probe failure spawned %d shims", spawned)
	}
}

func TestShimSpawnerSpawnsFromRegistryRecordWhenNothingIsAlive(t *testing.T) {
	// Arrange — nothing connected and no lock held; the record supplies the
	// spawn's CreateOpts.
	reg := openTestRegistry(t)
	cfg := t.TempDir()
	writeTranscript(t, cfg, "cli-uuid")
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", Model: "haiku",
		ConfigDir: cfg, ClaudeSessionID: "cli-uuid",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var gotOpts CreateOpts
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil },
		nil,
		func(_ string, opts CreateOpts) (ShimHandle, error) {
			gotOpts = opts
			return ShimHandle{}, nil
		},
		nil)

	// Act
	_, err := sp.EnsureShim(context.Background(), "s1")

	// Assert — CreateOpts reconstructed from the record (resume = the CLI uuid).
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if gotOpts.CWD != "/w" || gotOpts.Model != "haiku" ||
		gotOpts.ConfigDir != cfg || gotOpts.Resume != "cli-uuid" {
		t.Fatalf("spawn opts = %+v", gotOpts)
	}
}

func TestShimSpawnerTreatsALegacyPlaceholderRecordLikeAnEmptyModel(t *testing.T) {
	// Arrange — a row written by an older daemon still carries the marker.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", Model: "<synthetic>",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var gotOpts CreateOpts
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil },
		nil,
		func(_ string, opts CreateOpts) (ShimHandle, error) {
			gotOpts = opts
			return ShimHandle{}, nil
		},
		nil)

	// Act
	_, err := sp.EnsureShim(context.Background(), "s1")

	// Assert — legacy poison has exactly the same spawn semantics as empty.
	if err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}
	if gotOpts.Model != "" {
		t.Fatalf("spawn model = %q, want empty", gotOpts.Model)
	}
}

func TestShimSpawnerErrorsWhenNoRecordToSpawnFrom(t *testing.T) {
	// Arrange — no listener AND no registry record: nothing to reconstruct
	// CreateOpts from, so it is a loud error.
	reg := openTestRegistry(t)
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil },
		nil,
		func(string, CreateOpts) (ShimHandle, error) { return ShimHandle{}, nil },
		nil)

	// Act
	_, err := sp.EnsureShim(context.Background(), "ghost")

	// Assert
	if err == nil {
		t.Fatal("spawning a session with no registry record must error")
	}
}

// --- PushForwarder --------------------------------------------------------

func TestPushForwarderDropsUntilTargetSet(t *testing.T) {
	// Arrange — a push before SetTarget loud-logs and drops (no panic, no
	// swallow-into-a-fake-target).
	var logged int
	f := &PushForwarder{Logf: func(string, ...any) { logged++ }}

	// Act — no target yet.
	f.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "/w"})

	// Assert — the miss was reported, not silently swallowed.
	if logged != 1 {
		t.Fatalf("pre-target push should log once; logged=%d", logged)
	}
}

func TestPushForwarderNamesADroppedSynchronousProgressFrame(t *testing.T) {
	var logged string
	f := &PushForwarder{Logf: func(format string, args ...any) { logged = fmt.Sprintf(format, args...) }}

	f.PushProgressView(&frontendv1.ProgressView{Workspace: "/w"})

	if !strings.Contains(logged, "progress-view") {
		t.Fatalf("pre-target progress log = %q, want frame identity", logged)
	}
}

func TestPushForwarderForwardsAfterTargetSet(t *testing.T) {
	// Arrange — a real frontend.Server target with one connected client.
	var logged int
	f := &PushForwarder{Logf: func(string, ...any) { logged++ }}
	srv := frontend.New(frontend.Config{
		Logf:        func(string, ...any) {},
		LogVerbosef: func(string, ...any) {},
		State:       staticState{},
		Handler:     nopHandler{},
	})
	t.Cleanup(func() { _ = srv.Close() })
	f.SetTarget(srv)

	// Act — after SetTarget, a push reaches the server (no miss logged). We
	// assert indirectly: the miss counter stays zero because the target load
	// is non-nil.
	f.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "/w"})

	// Assert
	if logged != 0 {
		t.Fatalf("post-target push must not log a miss; logged=%d", logged)
	}
}

// --- test doubles for frontend.Server construction ------------------------

type staticState struct{}

func (staticState) Snapshot() *frontendv1.StateSnapshot { return &frontendv1.StateSnapshot{} }

type nopHandler struct{}

func (nopHandler) WorkspaceMaterialized(context.Context, string, string, *frontendv1.WorkspaceMaterializedCmd) error {
	return nil
}
func (nopHandler) HostActionCompleted(context.Context, string, string, *frontendv1.HostActionCompletedCmd) error {
	return nil
}

func (nopHandler) SubmitPrompt(context.Context, string, string, *frontendv1.SubmitPromptCmd) error {
	return nil
}
func (nopHandler) Interrupt(context.Context, string, string, *frontendv1.InterruptCmd) error {
	return nil
}
func (nopHandler) CancelDetachedAgents(context.Context, string, string, *frontendv1.CancelDetachedAgentsCmd) (*frontendv1.DetachedCancelOutcome, error) {
	return nil, nil
}
func (nopHandler) AnswerPermission(context.Context, string, string, *frontendv1.PermissionAnswerCmd) error {
	return nil
}
func (nopHandler) SetModel(context.Context, string, string, *frontendv1.SetModelCmd) (string, error) {
	return "opus", nil
}
func (nopHandler) MergeWorkspace(context.Context, string, string, *frontendv1.MergeWorkspaceCmd) error {
	return nil
}
func (nopHandler) CloseWorkspace(context.Context, string, string, *frontendv1.CloseWorkspaceCmd) error {
	return nil
}
func (nopHandler) OpenWorkspace(context.Context, string, string, *frontendv1.OpenWorkspaceCmd) error {
	return nil
}
func (nopHandler) Resync(context.Context, string, string, *frontendv1.ResyncCmd) error { return nil }
func (nopHandler) CreateSession(context.Context, string, string, *frontendv1.CreateSessionCmd) (string, error) {
	return "", nil
}
func (nopHandler) DeleteSession(context.Context, string, string, *frontendv1.DeleteSessionCmd) error {
	return nil
}
func (nopHandler) Shutdown(context.Context, string, string, *frontendv1.ShutdownCmd) error {
	return nil
}
func (nopHandler) ScheduleShutdown(context.Context, string, string, *frontendv1.ScheduleShutdownCmd) error {
	return nil
}
func (nopHandler) CancelScheduledShutdown(context.Context, string, string, *frontendv1.CancelScheduledShutdownCmd) error {
	return nil
}
func (nopHandler) RestartSession(context.Context, string, string, *frontendv1.RestartSessionCmd) error {
	return nil
}
func (nopHandler) HibernateWorkspace(context.Context, string, string, *frontendv1.HibernateWorkspaceCmd) error {
	return nil
}
func (nopHandler) ReviveSession(context.Context, string, string, *frontendv1.ReviveSessionCmd) error {
	return nil
}
func (nopHandler) AnswerMergeDequeue(context.Context, string, string, *frontendv1.AnswerMergeDequeueCmd) error {
	return nil
}
func (nopHandler) ClientLog(context.Context, string, string, *frontendv1.ClientLogCmd) error {
	return nil
}
func (nopHandler) ForceQueueEntry(context.Context, string, string, *frontendv1.QueueForceCmd) error {
	return nil
}
func (nopHandler) AcceptQueueEntry(context.Context, string, string, *frontendv1.QueueAcceptCmd) error {
	return nil
}
func (nopHandler) CancelQueueEntry(context.Context, string, string, *frontendv1.QueueCancelCmd) error {
	return nil
}
func (nopHandler) DaemonHealth(_ context.Context, _ string, requestID string, _ *frontendv1.DaemonHealthCmd) (*frontendv1.DaemonHealthView, error) {
	return &frontendv1.DaemonHealthView{RequestId: requestID, Healthy: true}, nil
}
func (nopHandler) SessionHealth(_ context.Context, workspace, requestID string, cmd *frontendv1.SessionHealthCmd) (*frontendv1.SessionHealthView, error) {
	return &frontendv1.SessionHealthView{RequestId: requestID, Workspace: workspace, SessionId: cmd.GetSessionId(), Healthy: true}, nil
}

// --- RegistryRegistrar.SessionDied (F4) -------------------------------------
//
// The write that never existed: a shim death resolved the workspace dead
// through the SSM while the record still claimed the session was alive.

func TestSessionDiedMarksTheRecordTerminal(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	r.SessionDied("s1", errclass.DeathReasonShimDied)

	// Assert.
	rec, ok := reg.Get("s1")
	if !ok || !rec.Terminal {
		t.Fatalf("record terminal = %v (found=%v), want true", rec.Terminal, ok)
	}
}

func TestSessionDiedRecordsTheReason(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	r.SessionDied("s1", errclass.DeathReasonShimDied)

	// Assert.
	rec, _ := reg.Get("s1")
	if rec.DeathReason != errclass.DeathReasonShimDied {
		t.Fatalf("death_reason = %q, want %q", rec.DeathReason, errclass.DeathReasonShimDied)
	}
}

func TestSessionDiedLeavesAnAlreadyTerminalRecordAlone(t *testing.T) {
	// Arrange: the user deleted the session, THEN its shim exited. The first
	// reason is the true one — "the user deleted this" must not become "the
	// process exited".
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", Terminal: true, DeathReason: errclass.DeathReasonDeleted}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg}

	// Act.
	r.SessionDied("s1", errclass.DeathReasonShimDied)

	// Assert.
	rec, _ := reg.Get("s1")
	if rec.DeathReason != errclass.DeathReasonDeleted {
		t.Fatalf("death_reason = %q, want the first reason %q preserved", rec.DeathReason, errclass.DeathReasonDeleted)
	}
}

func TestSessionDiedRepushesTheSessionView(t *testing.T) {
	// Arrange: without the push the dead-state card would wait for whatever
	// unrelated event next pushed a view.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var pushed []string
	r := &RegistryRegistrar{Reg: reg, PushView: func(id string) { pushed = append(pushed, id) }}

	// Act.
	r.SessionDied("s1", errclass.DeathReasonShimDied)

	// Assert.
	if len(pushed) != 1 || pushed[0] != "s1" {
		t.Fatalf("pushed = %v, want [s1]", pushed)
	}
}

func TestSessionDiedOnAnUnknownSessionIsLoud(t *testing.T) {
	// Arrange: a death for a session that was never registered.
	reg := openTestRegistry(t)
	var logged []string
	r := &RegistryRegistrar{Reg: reg, Logf: func(f string, a ...any) { logged = append(logged, f) }}

	// Act.
	r.SessionDied("ghost", errclass.DeathReasonShimDied)

	// Assert.
	if len(logged) == 0 {
		t.Fatal("a death write for an unknown session passed SILENTLY")
	}
}

// THE SURVIVING SHIM'S ONLY STOP HANDLE.
//
// StopShim was a permanent no-op for a shim this daemon never spawned — the
// survivor of a previous daemon — which is exactly the shim a stale-build
// bounce or an explicit restart needs to reach. The pid it announced on its
// ShimHello closes that, and it is safe here (rather than a pid-reuse hazard)
// because the caller only holds one while the connection that carried it lives.

// unitTestStop is the attribution these unit stops travel under. It stands in
// for the cause the session controller's stop funnel renders in production;
// what matters here is that it is a COMPLETE attribution, because an incomplete
// one is refused before anything is signalled.
var unitTestStop = ShimStop{Initiator: "unit_test", Reason: "a unit test commanded this stop"}

func TestStopShimPrefersItsOwnProcessHandle(t *testing.T) {
	// Arrange — a shim THIS spawner launched, plus an announced pid.
	stopped := false
	var signalled []int
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	// No processes and no lock files here: the exit wait has nothing real to
	// observe, so it is stubbed out rather than made to time out.
	s.awaitStopped = func(string, time.Duration) error { return nil }
	s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { stopped = true; return nil }}
	s.signal = func(pid int, _ syscall.Signal) error { signalled = append(signalled, pid); return nil }

	// Act.
	if err := s.StopShim("s1", 4242, unitTestStop); err != nil {
		t.Fatalf("StopShim: %v", err)
	}

	// Assert — the exact handle wins; the pid is not consulted at all.
	if !stopped {
		t.Fatal("the daemon's own process handle was not used")
	}
	if len(signalled) != 0 {
		t.Fatalf("signalled %v; the pid must only be used when there is no handle", signalled)
	}
}

// A shim that dies with no record of who ordered it is indistinguishable, in
// the daemon log, from one that crashed. StopShim is the daemon's ONLY route to
// a spawned shim's stop func, so the attribution it hands over is what labels
// every commanded death.

func TestStopShimAttributesTheStopItCommands(t *testing.T) {
	// Arrange
	var got ShimStop
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return nil }
	s.handles["s1"] = ShimHandle{Stop: func(by ShimStop) error { got = by; return nil }}

	// Act
	if err := s.StopShim("s1", 0, unitTestStop); err != nil {
		t.Fatalf("StopShim: %v", err)
	}

	// Assert — the CALLER's attribution reached the shim verbatim. It used to be
	// replaced here by one coarse package constant, which made an idle sweep and
	// a merged teardown identical at the record.
	if got != unitTestStop {
		t.Fatalf("stop func received %+v, want the caller's attribution %+v", got, unitTestStop)
	}
}

// AN UNATTRIBUTED STOP IS REFUSED, and the refusal reaches the caller: nothing
// is signalled, no handle is consumed, and the error names the missing half.
func TestStopShimRefusesAnUnattributedStop(t *testing.T) {
	// Arrange
	stopped := false
	var signalled []int
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return nil }
	s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { stopped = true; return nil }}
	s.signal = func(pid int, _ syscall.Signal) error { signalled = append(signalled, pid); return nil }

	// Act
	err := s.StopShim("s1", 4242, ShimStop{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "unattributed shim stop") {
		t.Fatalf("StopShim error = %v, want the unattributed-stop refusal", err)
	}
	if stopped || len(signalled) != 0 {
		t.Fatalf("a refused stop still acted: stopped=%v signalled=%v", stopped, signalled)
	}
	if _, retained := s.handles["s1"]; !retained {
		t.Fatal("a refused stop consumed the process handle, so the session can never be stopped again")
	}
}

func TestStopShimSignalsASurvivingShimByItsAnnouncedPid(t *testing.T) {
	// Arrange — no handle (a shim that outlived a previous daemon).
	var signalled []int
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return nil }
	s.signal = func(pid int, sig syscall.Signal) error {
		if sig != syscall.SIGTERM {
			t.Errorf("signal = %v, want SIGTERM (a clean stop)", sig)
		}
		signalled = append(signalled, pid)
		return nil
	}

	// Act.
	if err := s.StopShim("s1", 4242, unitTestStop); err != nil {
		t.Fatalf("StopShim: %v", err)
	}

	// Assert.
	if len(signalled) != 1 || signalled[0] != 4242 {
		t.Fatalf("signalled = %v, want exactly the announced pid 4242", signalled)
	}
}

func TestStopShimWithNeitherHandleNorPidRequiresSessionLockEvidence(t *testing.T) {
	// Arrange — an older daemon can lose both its child handle and the current
	// connection's pid while the shim remains alive.  Success must therefore
	// come from the lock predicate that gates restoration, not from absence of
	// bookkeeping.
	var lines []string
	var waited string
	s := NewShimSpawner(nil, nil, nil, nil, func(f string, a ...any) { lines = append(lines, fmt.Sprintf(f, a...)) })
	s.awaitStopped = func(sessionID string, _ time.Duration) error { waited = sessionID; return nil }
	s.signal = func(int, syscall.Signal) error { t.Fatal("nothing should be signalled without a pid"); return nil }

	// Act.
	if err := s.StopShim("s1", 0, unitTestStop); err != nil {
		t.Fatalf("StopShim with nothing to stop must not fail: %v", err)
	}

	// Assert — hibernation may only publish after the same proof a following
	// restore relies upon.
	if waited != "s1" {
		t.Fatalf("StopShim returned without lock evidence; awaited session = %q", waited)
	}
	var said bool
	for _, l := range lines {
		if strings.Contains(l, "proving the session lock is absent") {
			said = true
		}
	}
	if !said {
		t.Fatalf("the proof requirement was not logged; lines: %v", lines)
	}
}

func TestStopShimWithNeitherHandleNorPidFailsWithoutSessionLockEvidence(t *testing.T) {
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return errors.New("session lock is still held") }

	if err := s.StopShim("s1", 0, unitTestStop); err == nil || !strings.Contains(err.Error(), "session lock is still held") {
		t.Fatalf("StopShim without lock-release proof = %v", err)
	}
}

// A pid whose process has already exited is a stop that ALREADY HAPPENED, not a
// failure — a restart must not abort because the thing it wanted gone is gone.
func TestStopShimTreatsAnAlreadyExitedPidAsStopped(t *testing.T) {
	// Arrange.
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return nil }
	s.signal = func(int, syscall.Signal) error { return os.ErrProcessDone }

	// Act / Assert.
	if err := s.StopShim("s1", 4242, unitTestStop); err != nil {
		t.Fatalf("an already-exited shim reported a stop failure: %v", err)
	}
}

// STOPPED MEANS GONE. A SIGTERM only asks, and EnsureShim refuses to spawn
// while the session lock is held — so a StopShim that returned before the
// process died made every stop-then-start (a bounce onto a new bundle, a hard
// restart) race the kernel and reliably fail to respawn.
func TestStopShimWaitsForTheShimToActuallyExit(t *testing.T) {
	// Arrange.
	waited := ""
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(sessionID string, _ time.Duration) error { waited = sessionID; return nil }
	s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { return nil }}

	// Act.
	if err := s.StopShim("s1", 0, unitTestStop); err != nil {
		t.Fatalf("StopShim: %v", err)
	}

	// Assert.
	if waited != "s1" {
		t.Fatal("StopShim returned without waiting for the shim to exit; the respawn would race its session lock")
	}
}

func TestStopShimFailsBeforeMutationWithoutAnExitObserver(t *testing.T) {
	// Arrange — losing the observer must not consume the only exact stop handle
	// or signal a process before the caller learns cleanup cannot be proved.
	stopped := false
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = nil
	s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { stopped = true; return nil }}

	// Act.
	err := s.StopShim("s1", 0, unitTestStop)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "exit observer is nil") {
		t.Fatalf("StopShim error = %v", err)
	}
	if stopped {
		t.Fatal("stop handle ran before the exit-observer precondition failed")
	}
	if _, retained := s.handles["s1"]; !retained {
		t.Fatal("stop handle was consumed before the exit-observer precondition failed")
	}
}

func TestStopShimEvictsAParkedReconnectAfterProcessExit(t *testing.T) {
	// Arrange — the shim reconnects during teardown and parks just before its
	// process exits. StopShim must clear that transport after the exit proof.
	var evictedSession, evictedReason string
	s := NewShimSpawner(nil, nil,
		func(sessionID, reason string) bool {
			evictedSession, evictedReason = sessionID, reason
			return true
		}, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return nil }
	s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { return nil }}

	// Act.
	if err := s.StopShim("s1", 0, unitTestStop); err != nil {
		t.Fatalf("StopShim: %v", err)
	}

	// Assert.
	if evictedSession != "s1" || evictedReason != "shim_process_stop_completed" {
		t.Fatalf("eviction = session %q reason %q", evictedSession, evictedReason)
	}
}

func TestStopShimDoesNotEvictParkedStateBeforeProcessExitIsProven(t *testing.T) {
	// Arrange.
	evicted := false
	s := NewShimSpawner(nil, nil,
		func(string, string) bool { evicted = true; return true }, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return errors.New("still alive") }
	s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { return nil }}

	// Act.
	err := s.StopShim("s1", 0, unitTestStop)

	// Assert — closing a still-live shim's parked transport would only provoke
	// reconnect churn while claiming the stop had completed.
	if err == nil {
		t.Fatal("StopShim reported success without process-exit proof")
	}
	if evicted {
		t.Fatal("parked state was evicted before the shim process stopped")
	}
}

// A shim that will not die FAILS the stop, rather than reporting success and
// leaving the caller to spawn into a lock that is still held.
func TestStopShimFailsWhenTheShimNeverExits(t *testing.T) {
	// Arrange.
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return errors.New("still holds the session lock") }
	s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { return nil }}

	// Act / Assert.
	if err := s.StopShim("s1", 0, unitTestStop); err == nil {
		t.Fatal("a shim that ignored SIGTERM reported a successful stop")
	}
}

// THE STOP IS NOT OVER UNTIL THE REAPER IS. The lock wait ends in the kernel,
// at the dying process's last breath; the reaper's hooks run afterwards, here.
// A stop that returned on the first of those let a respawn start while shim 1's
// exit was still being reported, which is how one shim's death gets recorded
// against the shim that replaced it.
func TestStopShimReturnsOnlyAfterTheReaperCompletes(t *testing.T) {
	// Arrange — a deliberately slow reaper: this channel is never closed, so
	// the ONLY way the send below can complete is StopShim actually waiting on
	// it. A rendezvous, not a timing assumption.
	reaped := make(chan struct{})
	stopped := make(chan struct{})
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { return nil }
	s.handles["s1"] = ShimHandle{
		Stop:   func(ShimStop) error { close(stopped); return nil },
		Reaped: reaped,
	}

	// Act.
	done := make(chan error, 1)
	go func() { done <- s.StopShim("s1", 0, unitTestStop) }()
	<-stopped            // The SIGTERM has been delivered.
	reaped <- struct{}{} // The reaper finishes — and completes only because StopShim is waiting for it.

	// Assert.
	if err := <-done; err != nil {
		t.Fatalf("StopShim: %v", err)
	}
}

func TestStopShimDoesNotEvictWhenItsOwnStopHandleFails(t *testing.T) {
	// A failed local stop has not established process exit.  Evicting a parked
	// reconnect here would destroy the only route to a still-live shim.
	evicted := false
	s := NewShimSpawner(nil, nil, func(string, string) bool { evicted = true; return true }, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { t.Fatal("exit wait ran after stop failure"); return nil }
	s.handles["s1"] = ShimHandle{Stop: func(ShimStop) error { return errors.New("SIGTERM delivery failed") }}

	err := s.StopShim("s1", 0, unitTestStop)

	if err == nil || !strings.Contains(err.Error(), "SIGTERM delivery failed") {
		t.Fatalf("StopShim error = %v", err)
	}
	if evicted {
		t.Fatal("parked transport was evicted before the process stop succeeded")
	}
}

func TestStopShimDoesNotEvictWhenSurvivingShimSignalFails(t *testing.T) {
	// The announced PID is authoritative only while its stop succeeds.  A
	// signal error leaves lifecycle ownership unresolved and must not clean up
	// the parked route.
	evicted := false
	s := NewShimSpawner(nil, nil, func(string, string) bool { evicted = true; return true }, nil, nil)
	s.awaitStopped = func(string, time.Duration) error { t.Fatal("exit wait ran after signal failure"); return nil }
	s.signal = func(int, syscall.Signal) error { return errors.New("permission denied") }

	err := s.StopShim("s1", 4242, unitTestStop)

	if err == nil || !strings.Contains(err.Error(), "permission denied") {
		t.Fatalf("StopShim error = %v", err)
	}
	if evicted {
		t.Fatal("parked transport was evicted after a failed survivor signal")
	}
}
