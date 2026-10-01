package shimclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"syscall"
	"testing"
	"time"

	"claude-repld/internal/dlog"
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

// TestSpawnIsNotReadyBeforeAnyDiagnosticsArm asserts a session frame that is
// not diagnostics is not an answer: bring-up is still waiting for the health
// verdict the shim owes it.
func TestSpawnIsNotReadyBeforeAnyDiagnosticsArm(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	sup := newSupervisor(t)
	done := spawnAsync(t, sup, spec)
	waitForSessionOpen(t, f)

	// Act.
	f.push(compactingUpdate())

	// Assert.
	select {
	case r := <-done:
		t.Fatalf("Spawn() returned before any diagnostics push: %+v", r)
	case <-time.After(bringUpProbeWindow):
	}
	f.push(healthyUpdate())
	adoptTestClient(t, <-done)
}

// TestSpawnIsReadyOnAnUnhealthyDiagnosticsArm asserts the realtest-1 finding:
// a shim that pushes UNHEALTHY has ANSWERED, so bring-up completes on it
// rather than burning the caller's whole bound waiting for a verdict the shim
// has already given and will not revise.
func TestSpawnIsReadyOnAnUnhealthyDiagnosticsArm(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	sup := newSupervisor(t)
	done := spawnAsync(t, sup, spec)
	waitForSessionOpen(t, f)

	// Act.
	f.push(unhealthyUpdate())

	// Assert.
	select {
	case r := <-done:
		if r.err != nil {
			t.Fatalf("Spawn() error = %v, want the unhealthy shim brought up", r.err)
		}
		adoptTestClient(t, r)
	case <-time.After(bringUpAnswerBound):
		t.Fatal("Spawn() did not return on the unhealthy diagnostics answer")
	}
}

// TestAdoptIsReadyOnAnUnhealthyDiagnosticsArm asserts the same for the boot's
// verb: a survivor standing on a fault is adopted, never abandoned.
func TestAdoptIsReadyOnAnUnhealthyDiagnosticsArm(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	sup := newSupervisor(t)
	type result struct {
		c   Client
		err error
	}
	done := make(chan result, 1)
	go func() {
		c, err := sup.Adopt(context.Background(), ids.WorkspaceID("ws-1"), dir, uds)
		done <- result{c: c, err: err}
	}()
	waitForSessionOpen(t, f)

	// Act.
	f.push(unhealthyUpdate())

	// Assert.
	select {
	case r := <-done:
		if r.err != nil {
			t.Fatalf("Adopt() error = %v, want the unhealthy survivor adopted", r.err)
		}
		t.Cleanup(r.c.Detach)
	case <-time.After(bringUpAnswerBound):
		t.Fatal("Adopt() did not return on the unhealthy diagnostics answer")
	}
}

// TestUnhealthyBringUpRecordsTheFaultKinds asserts the adoption is auditable:
// the WARN that the shim reported unhealthy stands, and an INFO names how many
// faults it is standing on and which arms they are.
func TestUnhealthyBringUpRecordsTheFaultKinds(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, _ := newTestSpec(t, dir, uds, helperIdle)
	sup, surfaces := newSupervisorLogging(t)
	done := spawnAsync(t, sup, spec)
	waitForSessionOpen(t, f)

	// Act.
	f.push(unhealthyUpdate())
	r := <-done
	if r.err != nil {
		t.Fatalf("Spawn() error = %v", r.err)
	}
	adoptTestClient(t, r)

	// Assert.
	var warned bool
	var adopted *dlog.Record
	for i, rec := range surfaces.log.Records() {
		if rec.Operation != "daemon.shimclient.ready" {
			continue
		}
		if rec.Level == "warn" && rec.Message == "shim reported unhealthy" {
			warned = true
		}
		if rec.Level == "info" && rec.Message == "adopted an unhealthy shim; faults reported" {
			adopted = &surfaces.log.Records()[i]
		}
	}
	if !warned {
		t.Fatal("no WARN recorded that the shim reported unhealthy")
	}
	if adopted == nil {
		t.Fatal("no INFO recorded that an unhealthy shim was adopted")
	}
	if got := adopted.Context["fault_kinds"]; got != "unclassified" {
		t.Fatalf("fault_kinds = %v, want the one fault's arm name", got)
	}
	if got := adopted.Context["faults"]; got != 1 {
		t.Fatalf("faults = %v, want 1", got)
	}
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

// TestSpawnUnderTheVendorGuardIsForcedFake asserts a guarded daemon spawns the
// shim in FAKE mode rather than refusing the spawn.
//
// This replaces the refusal this site used to raise. The refusal made a
// workspace impossible to create under the guard at all -- the create verb's
// bring-up spawns a shim -- while the guard only ever meant "never touch the
// real vendor". A forced fake honors that meaning and is strictly stronger
// than the refusal: the child cannot reach the vendor whatever the caller
// asked for.
func TestSpawnUnderTheVendorGuardIsForcedFake(t *testing.T) {
	// Arrange: the caller asks for a REAL shim while the guard is set.
	t.Setenv(envc.EnvForbidVendorCalls, "1")
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	spec.Fake = false

	// Act.
	_ = spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert.
	if !slices.Contains(record.Argv, "--fake") {
		t.Fatalf("argv = %v, want --fake forced by the vendor guard", record.Argv)
	}
}

// TestSpawnUnderTheVendorGuardStillStatesTheGuardOnTheChild asserts forcing the
// fake does not withdraw the child's own guard: the shim's vendor-guard module
// must still be armed, so a fake shim that somehow reached `createRealQuery`
// throws instead of calling out.
func TestSpawnUnderTheVendorGuardStillStatesTheGuardOnTheChild(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvForbidVendorCalls, "1")
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	spec.Fake = false

	// Act.
	_ = spawnReady(t, f, spec)
	record := sink.record(t)

	// Assert.
	if got := record.Env[envc.EnvForbidVendorCalls]; got != "1" {
		t.Fatalf("child %s = %q, want %q", envc.EnvForbidVendorCalls, got, "1")
	}
}

// TestFakeMode asserts every way a spawn becomes fake, and the one way it
// stays real.
func TestFakeMode(t *testing.T) {
	tests := []struct {
		name   string
		spec   Spec
		forbid string
		want   bool
	}{
		{name: "nothing asks for it", want: false},
		{name: "the caller asks", spec: Spec{Fake: true}, want: true},
		{name: "the caller forbids the vendor for this spawn", spec: Spec{ForbidVendor: true}, want: true},
		{name: "the daemon is under the guard", forbid: "1", want: true},
		{name: "the guard is set to a falsey value", forbid: "0", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			t.Setenv(envc.EnvForbidVendorCalls, tc.forbid)
			contracts := envc.Load()

			// Act.
			got := fakeMode(tc.spec, contracts)

			// Assert.
			if got != tc.want {
				t.Fatalf("fakeMode() = %v, want %v", got, tc.want)
			}
		})
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
	err := client.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "no process"})

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
	sup := newSupervisor(t, WithKillGrace(sweepReapBound))

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
	c := newClient(newTestSurfaces().Global(), ids.WorkspaceID("ws-1"), p.uds, defaultBackoff, nil, nil)
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
	if err := c.Kill(context.Background(), KillAttribution{Actor: "test", Reason: "already stopped", Force: true}); err != nil {
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

// TestTheSweptSpawnIsRecordedAtInfo pins the LEVEL of the case above. The latch
// leaves exactly two cases and no third, and this is the expected one: a spawn
// in flight when the shutdown landed. Catching it is the sweep succeeding, so
// it states what it did; a kill that FAILS is what stays loud, because a leaked
// shim holds the workspace lock that refuses the next session.
func TestTheSweptSpawnIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	spec, sink := newTestSpec(t, dir, uds, helperIdle)
	sup, surfaces := newSupervisorLogging(t, WithKillGrace(sweepReapBound))

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
	waitForExit(t, record.PID)
	<-done

	// Assert.
	var level string
	for _, r := range surfaces.log.Records() {
		if r.Operation == "daemon.shimclient.standdown" {
			level = r.Level
		}
	}
	if level != "info" {
		t.Fatalf("the swept spawn was recorded at %q, want info", level)
	}
}

// TestBeginStandDownLatchesWithoutSweeping pins the half of the latch the
// immediate shutdown needs before it walks anything: the flag goes up, and no
// process is touched.
func TestBeginStandDownLatchesWithoutSweeping(t *testing.T) {
	// Arrange.
	sup := &supervisor{}

	// Act.
	first := sup.BeginStandDown()

	// Assert.
	if !first {
		t.Fatal("the first BeginStandDown() = false, want the call that latched it to say so")
	}
	if !sup.StandingDown() {
		t.Fatal("StandingDown() = false after BeginStandDown()")
	}
}

// TestBeginStandDownIsIdempotent pins that a re-statement is not a transition:
// the sweep raises the same latch, and a caller that records the transition
// must not record it twice.
func TestBeginStandDownIsIdempotent(t *testing.T) {
	// Arrange.
	sup := &supervisor{}
	sup.BeginStandDown()

	// Act.
	again := sup.BeginStandDown()

	// Assert.
	if again {
		t.Fatal("a second BeginStandDown() = true, want only the first call to claim the latch")
	}
}

// TestASpawnedClientReadsTheSupervisorsLatch is the wiring's own case: every
// client the supervisor hands out must read the daemon's latch, or the whole
// invariant holds only for the clients a teardown walk happens to name.
func TestASpawnedClientReadsTheSupervisorsLatch(t *testing.T) {
	// Arrange.
	sup := &supervisor{}
	c := newClient(newTestSurfaces().Global(), ids.WorkspaceID("ws-1"), "/tmp/unused.sock", defaultBackoff, nil, nil)
	c.daemonStandDown = sup.StandingDown

	// Act.
	sup.BeginStandDown()

	// Assert.
	if !c.StandingDown() {
		t.Fatal("a client wired to the supervisor reports StandingDown() = false after the supervisor latched")
	}
}

// TestSpawnedForAnswersTheLiveSpawnRegistry pins the adoption's own guard. A
// spawn reaches the fleet's session map only after StartSession answers, so
// for the whole window before that the supervisor's registry is the ONLY thing
// that knows the process exists -- and "lock free, socket live" is exactly what
// our own inert shim looks like to the next bring-up's probe.
func TestSpawnedForAnswersTheLiveSpawnRegistry(t *testing.T) {
	held := ids.WorkspaceID("ws-1")
	tests := []struct {
		name    string
		ask     ids.WorkspaceID
		release bool
		want    bool
	}{
		{name: "a spawn this supervisor still owns is ours", ask: held, want: true},
		{name: "another workspace's spawn is not ours", ask: ids.WorkspaceID("ws-2")},
		{name: "a spawn that has left the registry is not ours", ask: held, release: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			sup := newSupervisor(t).(*supervisor)
			c := newClient(newTestSurfaces().Global(), held, "/sock/ws-1.sock", defaultBackoff, nil, nil)
			c.mu.Lock()
			c.pid = 4242
			c.mu.Unlock()
			sup.hold(c)
			if tt.release {
				c.releaseHold()
			}

			// Act.
			pid, ours := sup.SpawnedFor(tt.ask)

			// Assert.
			if ours != tt.want {
				t.Fatalf("SpawnedFor(%q) ours = %v, want %v", tt.ask, ours, tt.want)
			}
			if tt.want && pid != 4242 {
				t.Fatalf("SpawnedFor(%q) pid = %d, want the held spawn's 4242", tt.ask, pid)
			}
		})
	}
}

// ---- the adopted survivor of a refused start ----

// TestAnAdoptedClientReadsTheSupervisorsLatch is the measured shape's own
// wiring case. A bring-up whose StartSession refused leaves its shim serving,
// and the next bring-up ADOPTS that inert survivor -- so one process ends up
// with two clients, and the adopted one is the client no teardown walk can
// name. It is handed the supervisor's latch by construction, so an immediate
// shutdown is an ordered departure to it as much as to the spawn record.
func TestAnAdoptedClientReadsTheSupervisorsLatch(t *testing.T) {
	// Arrange: a shim already serving, adopted the way the inert-survivor
	// bring-up adopts one.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	sup := newSupervisor(t)
	type result struct {
		c   Client
		err error
	}
	done := make(chan result, 1)
	go func() {
		c, err := sup.Adopt(context.Background(), ids.WorkspaceID("ws-1"), dir, uds)
		done <- result{c: c, err: err}
	}()
	waitForSessionOpen(t, f)
	f.push(healthyUpdate())
	r := <-done
	if r.err != nil {
		t.Fatalf("Adopt() error = %v", r.err)
	}
	t.Cleanup(r.c.Detach)

	// Act: the immediate shutdown latches before it ends anything.
	sup.BeginStandDown()

	// Assert.
	if !r.c.StandingDown() {
		t.Fatal("the adopted client reports StandingDown() = false after this daemon latched")
	}
}

// TestAnAdoptedClientsOrderedDepartureIsNotADeath carries that wiring through
// to the record the realtest harvest reads: the adopted client of a shim this
// daemon ordered away must not report `daemon.shimclient.exit` ERROR "shim
// died", which it did eight times over one `UpdateShutdownSchedule{now}`.
func TestAnAdoptedClientsOrderedDepartureIsNotADeath(t *testing.T) {
	// Arrange.
	dir := shortDir(t)
	f, uds := startFakeShim(t, dir)
	sup, surfaces := newSupervisorLogging(t)
	type result struct {
		c   Client
		err error
	}
	done := make(chan result, 1)
	go func() {
		c, err := sup.Adopt(context.Background(), ids.WorkspaceID("ws-1"), dir, uds)
		done <- result{c: c, err: err}
	}()
	waitForSessionOpen(t, f)
	f.push(healthyUpdate())
	r := <-done
	if r.err != nil {
		t.Fatalf("Adopt() error = %v", r.err)
	}
	t.Cleanup(r.c.Detach)
	sup.BeginStandDown()

	// Act: the adopted shim's departure, which carries the -1 sentinel because
	// it is not this daemon's child.
	r.c.(*client).publishExit(ExitInfo{PID: r.c.PID(), Code: -1, Inferred: true})

	// Assert.
	if hasRecordAt(surfaces.log, "error", "daemon.shimclient.exit") {
		t.Fatal("the adopted client recorded this daemon's own teardown as a death")
	}
}

// TestSpawnNeverDisablesVendorCompaction asserts the daemon sets no vendor
// compaction switch on any shim child: the vendor's own auto-compaction is what
// keeps a session's context window from filling, for every account.
func TestSpawnNeverDisablesVendorCompaction(t *testing.T) {
	for _, name := range []string{"DISABLE_COMPACT", "DISABLE_AUTO_COMPACT"} {
		t.Run(name, func(t *testing.T) {
			// Arrange: the daemon's own environment carries no such switch.
			t.Setenv(name, "")
			dir := shortDir(t)
			f, uds := startFakeShim(t, dir)
			spec, sink := newTestSpec(t, dir, uds, helperIdle)

			// Act.
			_ = spawnReady(t, f, spec)
			record := sink.record(t)

			// Assert.
			if got := record.Env[name]; got != "" {
				t.Fatalf("child %s = %q, want unset", name, got)
			}
		})
	}
}
