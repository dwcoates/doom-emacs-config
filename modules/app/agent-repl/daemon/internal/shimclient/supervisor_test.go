package shimclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"syscall"
	"testing"
	"time"

	"claude-repld/internal/envc"
	"claude-repld/internal/ids"
)

// TestSpawnArgvIsTheContractVerbatim asserts the shim's argv is exactly the
// common spawn contract's.
func TestSpawnArgvIsTheContractVerbatim(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)

	// Act.
	_ = spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert.
	want := []string{
		spec.NodeBin,
		spec.MainJS,
		"--listen", spec.UDSPath,
		"--store-socket", spec.StoreSocket,
		"--log-fd", "3",
		"--fake",
	}
	if !reflect.DeepEqual(record.Argv, want) {
		t.Fatalf("argv = %v, want %v", record.Argv, want)
	}
}

// TestSpawnOmitsFakeWhenNotAsked asserts --fake appears only when the Spec
// asks for it.
func TestSpawnOmitsFakeWhenNotAsked(t *testing.T) {
	// Arrange: the vendor guard permits a non-fake spawn only when vendor
	// calls are not forbidden, which is exactly what this test arranges.
	t.Setenv(envc.EnvForbidVendorCalls, "")
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	spec.Fake = false

	// Act.
	_ = spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert.
	for _, arg := range record.Argv {
		if arg == "--fake" {
			t.Fatalf("argv = %v, want no --fake", record.Argv)
		}
	}
}

// TestSpawnSetsTheContractedEnvironment asserts every contracted variable is
// on the child's environment with the Spec's value.
func TestSpawnSetsTheContractedEnvironment(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	spec.ForbidVendor = true
	spec.SessionID = "host-session-7"

	// Act.
	_ = spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert.
	want := map[string]string{
		EnvConfigDir:              spec.ConfigDir,
		envc.EnvOwned:             "1",
		envc.EnvStateDir:          spec.StateDir,
		EnvShimBuildSHA:           spec.ShimBuildSHA,
		EnvSessionID:              spec.SessionID,
		envc.EnvForbidVendorCalls: "1",
	}
	for name, value := range want {
		if got := record.Env[name]; got != value {
			t.Fatalf("child %s = %q, want %q", name, got, value)
		}
	}
}

// TestSpawnInheritsArbitraryEnvironment asserts the daemon's own environment
// is passed through, not filtered by an allowlist: a test-only channel must
// reach the child.
func TestSpawnInheritsArbitraryEnvironment(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	t.Setenv("FAKESHIM_SCRIPT", "/some/script.json")
	spec, sink := newTestSpec(t, dir, uds, helperIdle)

	// Act.
	_ = spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert.
	if got := record.Env["FAKESHIM_SCRIPT"]; got != "/some/script.json" {
		t.Fatalf("child FAKESHIM_SCRIPT = %q, want the inherited value", got)
	}
}

// TestSpawnOverridesInheritedContractedValue asserts a contracted name the
// daemon's own environment already carries is OVERRIDDEN, exactly once.
func TestSpawnOverridesInheritedContractedValue(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	t.Setenv(EnvConfigDir, "/inherited/account")
	spec, sink := newTestSpec(t, dir, uds, helperIdle)

	// Act.
	_ = spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert.
	if got := record.Env[EnvConfigDir]; got != spec.ConfigDir {
		t.Fatalf("child %s = %q, want the Spec's %q", EnvConfigDir, got, spec.ConfigDir)
	}
}

// TestSpawnUsesTheWorkspaceDirAsCwd asserts the child's cwd is the workspace.
func TestSpawnUsesTheWorkspaceDirAsCwd(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)

	// Act.
	_ = spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert: temp dirs can be symlinked, so compare resolved paths.
	wantDir, err := filepath.EvalSymlinks(spec.WorkspaceDir)
	if err != nil {
		t.Fatalf("EvalSymlinks(%q): %v", spec.WorkspaceDir, err)
	}
	gotDir, err := filepath.EvalSymlinks(record.Cwd)
	if err != nil {
		t.Fatalf("EvalSymlinks(%q): %v", record.Cwd, err)
	}
	if gotDir != wantDir {
		t.Fatalf("cwd = %q, want %q", gotDir, wantDir)
	}
}

// TestSpawnPassesTheLogSinkAsFD3 asserts fd 3 is the already-open sink: the
// child's own write landed in the parent's file.
func TestSpawnPassesTheLogSinkAsFD3(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)

	// Act.
	_ = spawnReady(t, f, spec)

	// Assert: readHelperRecord fails when fd 3 was not the sink.
	record := sink.record(t)
	if record.PID == 0 {
		t.Fatal("record from fd 3 has no pid")
	}
}

// TestSpawnGivesTheShimItsOwnProcessGroup asserts process-group discipline:
// the child leads its own group, so a kill reaches everything it spawned.
func TestSpawnGivesTheShimItsOwnProcessGroup(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)

	// Act.
	client := spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert.
	if record.PGID != record.PID {
		t.Fatalf("child pgid = %d, want its own pid %d", record.PGID, record.PID)
	}
	if client.PID() != record.PID {
		t.Fatalf("PID() = %d, want %d", client.PID(), record.PID)
	}
}

// TestSpawnIsReadyOnlyAfterHealthyDiagnostics asserts an unhealthy answer is
// not readiness: bring-up keeps waiting until the health arm says healthy.
func TestSpawnIsReadyOnlyAfterHealthyDiagnostics(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	sup := newSupervisor(t)

	type result struct {
		c   Client
		err error
	}
	done := make(chan result, 1)
	go func() {
		c, err := sup.Spawn(context.Background(), spec)
		done <- result{c: c, err: err}
	}()
	waitForSessionOpen(t, f)

	// Act: a non-health frame and an unhealthy answer are both delivered
	// before any healthy one.
	f.push(compactingUpdate())
	f.push(unhealthyUpdate())

	// Assert: still not ready.
	select {
	case r := <-done:
		t.Fatalf("Spawn() returned before a healthy diagnostics push: %+v", r)
	default:
	}

	f.push(healthyUpdate())
	r := <-done
	if r.err != nil {
		t.Fatalf("Spawn() error = %v", r.err)
	}
	t.Cleanup(func() { _ = r.c.Kill(KillAttribution{Actor: "test", Reason: "cleanup"}) })
}

// TestSpawnDeathDuringBringUpSurfacesExitAndStderr asserts a process that dies
// while being dialed ends bring-up at once with its exit decoding and stderr
// ring — never a timeout.
func TestSpawnDeathDuringBringUpSurfacesExitAndStderr(t *testing.T) {
	// Arrange: nothing ever listens on the socket, and the child exits 7.
	dir := shortDir(t)
	spec, _ := newTestSpec(t, dir, filepath.Join(dir, "never.sock"), helperDie)
	t.Setenv(helperExitEnv, "7")
	t.Setenv(helperErrEnv, "shim could not open the store")
	sup := newSupervisor(t)

	// Act.
	_, err := sup.Spawn(context.Background(), spec)

	// Assert.
	var death *BringUpDeathError
	if !errors.As(err, &death) {
		t.Fatalf("Spawn() error = %v, want *BringUpDeathError", err)
	}
	if death.Exit.Code != 7 {
		t.Fatalf("exit code = %d, want 7", death.Exit.Code)
	}
	if !strings.Contains(death.Exit.Stderr, "shim could not open the store") {
		t.Fatalf("stderr evidence = %q, want the child's stderr", death.Exit.Stderr)
	}
}

// TestSpawnRefusedWhenVendorCallsForbidden asserts the shim spawn is a guarded
// vendor exec site: without --fake it is refused, naming the site.
func TestSpawnRefusedWhenVendorCallsForbidden(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvForbidVendorCalls, "1")
	dir := shortDir(t)
	_, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	spec.Fake = false
	sup := newSupervisor(t)

	// Act.
	_, err := sup.Spawn(context.Background(), spec)

	// Assert.
	var forbidden *envc.ForbiddenError
	if !errors.As(err, &forbidden) {
		t.Fatalf("Spawn() error = %v, want *envc.ForbiddenError", err)
	}
	if forbidden.Site != "shim-spawn" {
		t.Fatalf("site = %q, want \"shim-spawn\"", forbidden.Site)
	}
}

// TestSpawnValidatesSpec asserts every part of the spawn contract is required.
func TestSpawnValidatesSpec(t *testing.T) {
	tests := []struct {
		name  string
		mut   func(*Spec)
		field string
	}{
		{name: "workspace id", mut: func(s *Spec) { s.WorkspaceID = "" }, field: "WorkspaceID"},
		{name: "workspace dir", mut: func(s *Spec) { s.WorkspaceDir = "" }, field: "WorkspaceDir"},
		{name: "uds path", mut: func(s *Spec) { s.UDSPath = "" }, field: "UDSPath"},
		{name: "store socket", mut: func(s *Spec) { s.StoreSocket = "" }, field: "StoreSocket"},
		{name: "config dir", mut: func(s *Spec) { s.ConfigDir = "" }, field: "ConfigDir"},
		{name: "shim build sha", mut: func(s *Spec) { s.ShimBuildSHA = "" }, field: "ShimBuildSHA"},
		{name: "node bin", mut: func(s *Spec) { s.NodeBin = "" }, field: "NodeBin"},
		{name: "main js", mut: func(s *Spec) { s.MainJS = "" }, field: "MainJS"},
		{name: "log sink", mut: func(s *Spec) { s.LogSink = nil }, field: "LogSink"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := shortDir(t)
			spec, _ := newTestSpec(t, dir, filepath.Join(dir, "shim.sock"), helperIdle)
			tc.mut(&spec)
			sup := newSupervisor(t)

			// Act.
			_, err := sup.Spawn(context.Background(), spec)

			// Assert.
			var specErr *SpecError
			if !errors.As(err, &specErr) {
				t.Fatalf("Spawn() error = %v, want *SpecError", err)
			}
			if specErr.Field != tc.field {
				t.Fatalf("field = %q, want %q", specErr.Field, tc.field)
			}
		})
	}
}

// TestAdoptDoesNotSpawn asserts an adopted shim is dialed, never started.
//
// THE PID IT REPORTS IS THE SOCKET'S PEER, not a child's. An adopted client
// used to answer 0, which was honest about having no child and useless about
// everything else: nothing could name the process in a record, and nothing
// could stop it. It now comes from the kernel's peer credential, so here —
// where the fake shim is served in-process — it is this very test's pid.
func TestAdoptDoesNotSpawn(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)

	// Act.
	client := adoptReady(t, f, dir, uds)

	// Assert.
	if got, want := client.PID(), os.Getpid(); got != want {
		t.Fatalf("PID() = %d, want the socket's peer %d", got, want)
	}
	if f.count("WatchSession") == 0 {
		t.Fatal("WatchSession was never opened; the adopted shim was not dialed")
	}
}

// TestAdoptKillRefusesTheDaemonsOwnProcessGroup asserts the guard that keeps a
// stop of an adopted shim from ever becoming a stop of the daemon.
//
// IT REPLACES A TEST THAT PINNED THE DEFECT. An adopted client used to refuse
// EVERY kill with ErrNoProcess, which is why a successor daemon could not
// stand down the shims a handover had just handed it. Now it signals the
// process its socket's peer credential names — and the one process it must
// never signal is itself, which is exactly what this fake, served in-process,
// makes it try.
func TestAdoptKillRefusesTheDaemonsOwnProcessGroup(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	client := adoptReady(t, f, dir, uds)

	// Act.
	err := client.Kill(KillAttribution{Actor: "test", Reason: "no process"})

	// Assert.
	if err == nil {
		t.Fatal("Kill() error = nil, want a refusal: the socket's peer is this very process")
	}
	if !strings.Contains(err.Error(), "process group") {
		t.Fatalf("Kill() error = %v, want it to name the process group it refused", err)
	}
}

// TestAdoptValidatesItsArguments asserts adoption's own required arguments.
func TestAdoptValidatesItsArguments(t *testing.T) {
	tests := []struct {
		name  string
		ws    ids.WorkspaceID
		dir   string
		uds   string
		field string
	}{
		{name: "workspace id", ws: "", dir: "/tmp", uds: "/tmp/a.sock", field: "WorkspaceID"},
		{name: "workspace dir", ws: "ws-1", dir: "", uds: "/tmp/a.sock", field: "WorkspaceDir"},
		{name: "uds path", ws: "ws-1", dir: "/tmp", uds: "", field: "UDSPath"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			sup := newSupervisor(t)

			// Act.
			_, err := sup.Adopt(context.Background(), tc.ws, tc.dir, tc.uds)

			// Assert.
			var specErr *SpecError
			if !errors.As(err, &specErr) {
				t.Fatalf("Adopt() error = %v, want *SpecError", err)
			}
			if specErr.Field != tc.field {
				t.Fatalf("field = %q, want %q", specErr.Field, tc.field)
			}
		})
	}
}

// TestNewSupervisorRequiresLogSurfaces asserts the supervisor cannot be built
// without somewhere to log.
func TestNewSupervisorRequiresLogSurfaces(t *testing.T) {
	// Arrange, Act.
	_, err := NewSupervisor(nil)

	// Assert.
	if err == nil {
		t.Fatal("NewSupervisor(nil) error = nil, want an error")
	}
}

// TestSpawnStopsTheShimWhenBringUpIsCanceled asserts a failed bring-up never
// leaves an orphan holding the workspace lock.
func TestSpawnStopsTheShimWhenBringUpIsCanceled(t *testing.T) {
	// Arrange: nothing listens, so bring-up keeps dialing until it is canceled.
	dir := shortDir(t)
	spec, sink := newTestSpec(t, dir, filepath.Join(dir, "never.sock"), helperIdle)
	sup := newSupervisor(t, WithKillGrace(50*time.Millisecond))
	ctx, cancel := context.WithCancel(context.Background())

	type result struct{ err error }
	done := make(chan result, 1)
	go func() {
		_, err := sup.Spawn(ctx, spec)
		done <- result{err: err}
	}()

	// Act: cancel once the child has reported itself on fd 3.
	record := sink.record(t)
	cancel()
	r := <-done

	// Assert.
	if !errors.Is(r.err, context.Canceled) {
		t.Fatalf("Spawn() error = %v, want context.Canceled", r.err)
	}
	waitForExit(t, record.PID)
}

// waitForExit blocks until the pid is gone.
func waitForExit(t *testing.T, pid int) {
	t.Helper()

	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		if !alive(pid) {
			return
		}
	}
	t.Fatalf("pid %d is still alive; the abandoned shim was not stopped", pid)
}

// TestStandDownEverySpawnKillsASpawnStillBringingUp is the leak's own case: a
// process that has been started and has NOT finished bring-up is known to
// nobody but the supervisor, and the sweep is what stops it.
func TestStandDownEverySpawnKillsASpawnStillBringingUp(t *testing.T) {
	// Arrange: the fake never pushes the healthy diagnostics, so bring-up is
	// still waiting and the client has reached no caller.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	sup := newSupervisor(t, WithKillGrace(50*time.Millisecond))

	done := make(chan error, 1)
	go func() {
		_, err := sup.Spawn(context.Background(), spec)
		done <- err
	}()
	record := sink.record(t)
	waitForSessionOpen(t, f)

	// Act.
	if err := sup.StandDownEverySpawn(context.Background(), "an immediate shutdown was requested"); err != nil {
		t.Fatalf("StandDownEverySpawn() error = %v", err)
	}

	// Assert.
	waitForExit(t, record.PID)
	if err := <-done; err == nil {
		t.Fatalf("Spawn() succeeded after its process was stood down")
	}
}

// TestStandDownEverySpawnLeavesADetachedShimRunning is the BOUNCE's case: a
// shim handed to a successor is detached from, and the sweep must not be able
// to find it.
func TestStandDownEverySpawnLeavesADetachedShimRunning(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	sup := newSupervisor(t, WithKillGrace(50*time.Millisecond))
	c := spawnReadyOn(t, sup, f, spec)
	record := sink.record(t)
	c.Detach()

	// Act.
	if err := sup.StandDownEverySpawn(context.Background(), "an immediate shutdown was requested"); err != nil {
		t.Fatalf("StandDownEverySpawn() error = %v", err)
	}

	// Assert: the handover's process is still there for the successor.
	if !alive(record.PID) {
		t.Fatalf("pid %d was killed; a detached shim is the successor's to adopt", record.PID)
	}
	_ = syscall.Kill(-record.PID, syscall.SIGKILL)
}

// TestStandDownEverySpawnReportsAKillThatFailed pins that a kill which cannot
// be performed is RETURNED, never swallowed: a leaked shim holds the workspace
// lock, and this error is the only thing that will ever say so.
func TestStandDownEverySpawnReportsAKillThatFailed(t *testing.T) {
	// Arrange: a held client whose socket is served by a process leading no
	// group of its own, which is the one shape the kill refuses to signal.
	// (An absent socket no longer serves here: a shim whose socket is gone is
	// the state the caller asked for, and the sweep says so with nil.)
	leader := startPeer(t, 0)
	p := startPeer(t, leader.pid)
	sup := newSupervisor(t).(*supervisor)
	c := newClient(newTestSurfaces().Global(), ids.WorkspaceID("ws-1"), p.uds, defaultBackoff, nil)
	sup.hold(c)

	// Act.
	err := sup.StandDownEverySpawn(context.Background(), "an immediate shutdown was requested")

	// Assert.
	if err == nil {
		t.Fatal("StandDownEverySpawn() error = nil, want the refused kill reported")
	}
	if !strings.Contains(err.Error(), "process group") {
		t.Fatalf("StandDownEverySpawn() error = %v, want it to carry the kill's own refusal", err)
	}
}

// TestStandDownEverySpawnSweepsNothingWhenNoSpawnIsHeld pins the ordinary
// shutdown: every shim reached the fleet and went down through it, so the
// sweep has nothing left to do and says so without error.
func TestStandDownEverySpawnSweepsNothingWhenNoSpawnIsHeld(t *testing.T) {
	// Arrange.
	sup := newSupervisor(t)

	// Act.
	err := sup.StandDownEverySpawn(context.Background(), "an immediate shutdown was requested")

	// Assert.
	if err != nil {
		t.Fatalf("StandDownEverySpawn() error = %v, want nil", err)
	}
}

// TestStandDownEverySpawnForgetsAShimThatAlreadyExited pins that the registry
// empties itself on death: a process that is already gone is not swept, and
// the sweep does not report the absence as a failure.
func TestStandDownEverySpawnForgetsAShimThatAlreadyExited(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	sup := newSupervisor(t, WithKillGrace(50*time.Millisecond))
	c := spawnReadyOn(t, sup, f, spec)
	record := sink.record(t)
	if err := c.Kill(KillAttribution{Actor: "test", Reason: "already stopped", Force: true}); err != nil {
		t.Fatalf("Kill() error = %v", err)
	}
	waitForExit(t, record.PID)
	<-c.Exited()

	// Act.
	err := sup.StandDownEverySpawn(context.Background(), "an immediate shutdown was requested")

	// Assert: the registry emptied itself on the death, so the sweep finds
	// nothing at all rather than merely finding a corpse it may not signal.
	if err != nil {
		t.Fatalf("StandDownEverySpawn() error = %v, want nil", err)
	}
	if held := sup.(*supervisor).heldNow(); len(held) != 0 {
		t.Fatalf("the supervisor still holds %d spawn(s) after one exited", len(held))
	}
}

// spawnReadyOn is spawnReady against a supervisor the caller keeps, which is
// what a sweep test needs: the sweep is asked of the very supervisor that
// started the process.
func spawnReadyOn(t *testing.T, sup Supervisor, f *fakeShim, spec Spec) Client {
	t.Helper()

	type result struct {
		c   Client
		err error
	}
	done := make(chan result, 1)
	go func() {
		c, err := sup.Spawn(context.Background(), spec)
		done <- result{c: c, err: err}
	}()
	waitForSessionOpen(t, f)
	f.push(healthyUpdate())

	r := <-done
	if r.err != nil {
		t.Fatalf("Spawn() error = %v", r.err)
	}
	return r.c
}

// TestSpawnIsRefusedOnceTheSupervisorHasStoodDown asserts the latch the sweep
// sets, which is what makes the sweep total.
//
// THE SWEEP ALONE IS NOT ENOUGH. It snapshots the processes that have already
// STARTED, so a bring-up still short of cmd.Start when the shutdown lands would
// start its shim just after the sweep walked past — and nothing would ever
// stand that process down, because the supervisor is the only thing that knew
// it was coming. Measured on the Emacs e2e layer: a SubmitPrompt whose spawn
// was still probing the workspace lock left a node shim running with no daemon
// left to own it.
func TestSpawnIsRefusedOnceTheSupervisorHasStoodDown(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	_, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	sup := newSupervisor(t)
	if err := sup.StandDownEverySpawn(context.Background(), "an immediate shutdown was requested"); err != nil {
		t.Fatalf("StandDownEverySpawn() error = %v", err)
	}

	// Act.
	c, err := sup.Spawn(context.Background(), spec)

	// Assert.
	if !errors.Is(err, ErrStandingDown) {
		t.Fatalf("Spawn() error = %v, want ErrStandingDown", err)
	}
	if c != nil {
		t.Fatal("Spawn() answered a client while the supervisor was standing down")
	}
}
