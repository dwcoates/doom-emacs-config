package server

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"syscall"
	"testing"

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
		func(string, CreateOpts) (func() error, error) { spawned++; return nil, nil },
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
		func(string, CreateOpts) (func() error, error) { spawned++; return nil, nil },
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
		func(_ string, opts CreateOpts) (func() error, error) {
			gotOpts = opts
			return nil, nil
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
		func(_ string, opts CreateOpts) (func() error, error) {
			gotOpts = opts
			return nil, nil
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
		func(string, CreateOpts) (func() error, error) { return nil, nil },
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

func TestStopShimPrefersItsOwnProcessHandle(t *testing.T) {
	// Arrange — a shim THIS spawner launched, plus an announced pid.
	stopped := false
	var signalled []int
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	// No processes and no lock files here: the exit wait has nothing real to
	// observe, so it is stubbed out rather than made to time out.
	s.awaitStopped = func(string) error { return nil }
	s.stops["s1"] = func() error { stopped = true; return nil }
	s.signal = func(pid int, _ syscall.Signal) error { signalled = append(signalled, pid); return nil }

	// Act.
	if err := s.StopShim("s1", 4242); err != nil {
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

func TestStopShimSignalsASurvivingShimByItsAnnouncedPid(t *testing.T) {
	// Arrange — no handle (a shim that outlived a previous daemon).
	var signalled []int
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string) error { return nil }
	s.signal = func(pid int, sig syscall.Signal) error {
		if sig != syscall.SIGTERM {
			t.Errorf("signal = %v, want SIGTERM (a clean stop)", sig)
		}
		signalled = append(signalled, pid)
		return nil
	}

	// Act.
	if err := s.StopShim("s1", 4242); err != nil {
		t.Fatalf("StopShim: %v", err)
	}

	// Assert.
	if len(signalled) != 1 || signalled[0] != 4242 {
		t.Fatalf("signalled = %v, want exactly the announced pid 4242", signalled)
	}
}

func TestStopShimWithNeitherHandleNorPidIsALoggedNoOp(t *testing.T) {
	// Arrange.
	var lines []string
	s := NewShimSpawner(nil, nil, nil, nil, func(f string, a ...any) { lines = append(lines, fmt.Sprintf(f, a...)) })
	s.awaitStopped = func(string) error { return nil }
	s.signal = func(int, syscall.Signal) error { t.Fatal("nothing should be signalled"); return nil }

	// Act.
	if err := s.StopShim("s1", 0); err != nil {
		t.Fatalf("StopShim with nothing to stop must not fail: %v", err)
	}

	// Assert — silence here would hide a shim nobody can reach.
	var said bool
	for _, l := range lines {
		if strings.Contains(l, "StopShim no-op") {
			said = true
		}
	}
	if !said {
		t.Fatalf("the no-op was not logged; lines: %v", lines)
	}
}

// A pid whose process has already exited is a stop that ALREADY HAPPENED, not a
// failure — a restart must not abort because the thing it wanted gone is gone.
func TestStopShimTreatsAnAlreadyExitedPidAsStopped(t *testing.T) {
	// Arrange.
	s := NewShimSpawner(nil, nil, nil, nil, nil)
	s.awaitStopped = func(string) error { return nil }
	s.signal = func(int, syscall.Signal) error { return os.ErrProcessDone }

	// Act / Assert.
	if err := s.StopShim("s1", 4242); err != nil {
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
	s.awaitStopped = func(sessionID string) error { waited = sessionID; return nil }
	s.stops["s1"] = func() error { return nil }

	// Act.
	if err := s.StopShim("s1", 0); err != nil {
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
	s.stops["s1"] = func() error { stopped = true; return nil }

	// Act.
	err := s.StopShim("s1", 0)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "exit observer is nil") {
		t.Fatalf("StopShim error = %v", err)
	}
	if stopped {
		t.Fatal("stop handle ran before the exit-observer precondition failed")
	}
	if _, retained := s.stops["s1"]; !retained {
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
	s.awaitStopped = func(string) error { return nil }
	s.stops["s1"] = func() error { return nil }

	// Act.
	if err := s.StopShim("s1", 0); err != nil {
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
	s.awaitStopped = func(string) error { return errors.New("still alive") }
	s.stops["s1"] = func() error { return nil }

	// Act.
	err := s.StopShim("s1", 0)

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
	s.awaitStopped = func(string) error { return errors.New("still holds the session lock") }
	s.stops["s1"] = func() error { return nil }

	// Act / Assert.
	if err := s.StopShim("s1", 0); err == nil {
		t.Fatal("a shim that ignored SIGTERM reported a successful stop")
	}
}

func TestStopShimDoesNotEvictWhenItsOwnStopHandleFails(t *testing.T) {
	// A failed local stop has not established process exit.  Evicting a parked
	// reconnect here would destroy the only route to a still-live shim.
	evicted := false
	s := NewShimSpawner(nil, nil, func(string, string) bool { evicted = true; return true }, nil, nil)
	s.awaitStopped = func(string) error { t.Fatal("exit wait ran after stop failure"); return nil }
	s.stops["s1"] = func() error { return errors.New("SIGTERM delivery failed") }

	err := s.StopShim("s1", 0)

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
	s.awaitStopped = func(string) error { t.Fatal("exit wait ran after signal failure"); return nil }
	s.signal = func(int, syscall.Signal) error { return errors.New("permission denied") }

	err := s.StopShim("s1", 4242)

	if err == nil || !strings.Contains(err.Error(), "permission denied") {
		t.Fatalf("StopShim error = %v", err)
	}
	if evicted {
		t.Fatal("parked transport was evicted after a failed survivor signal")
	}
}
