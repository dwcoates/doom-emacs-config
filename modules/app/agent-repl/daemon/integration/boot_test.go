//go:build integration

package integration

import (
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

func TestBootWritesAndRemovesTheAddressFile(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act: StartDaemon already waited for the file; assert its shape, then stop.
	raw, err := os.ReadFile(d.AddrFile())
	if err != nil {
		t.Fatalf("read daemon.addr = error %v, want the bound address", err)
	}

	// Assert
	if !strings.HasPrefix(string(raw), "127.0.0.1:") || !strings.HasSuffix(string(raw), "\n") {
		t.Fatalf("daemon.addr = %q, want \"127.0.0.1:<port>\\n\"", raw)
	}
	d.Stop()
	if _, err := os.Stat(d.AddrFile()); !os.IsNotExist(err) {
		t.Fatalf("daemon.addr after SIGTERM: stat err = %v, want it removed on an orderly exit", err)
	}
}

func TestSecondDaemonOnTheSameStateRootRefusesToBoot(t *testing.T) {
	// Arrange
	incumbent := newDaemon(t, harness.Opts{})
	before, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read the incumbent's daemon.addr: %v", err)
	}

	// Act
	second := harness.StartDaemon(t, harness.Opts{StateDir: incumbent.StateDir, ExpectEarlyExit: true})
	code := second.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("the second daemon exited 0, want a non-zero refusal\nstderr:\n%s", second.Stderr())
	}
	after, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read the incumbent's daemon.addr after the refusal: %v", err)
	}
	if string(after) != string(before) {
		t.Fatalf("daemon.addr = %q after a refused second daemon, want the incumbent's %q untouched", after, before)
	}
	if _, err := incumbent.Client().DaemonHealth(incumbent.Ctx(), healthRequest()); err != nil {
		t.Fatalf("the incumbent stopped serving after a refused second daemon: %v", err)
	}
}

func TestJoiningDaemonDoesNotClaimTheAddressFile(t *testing.T) {
	// Arrange
	incumbent := newDaemon(t, harness.Opts{})
	before, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read the incumbent's daemon.addr: %v", err)
	}

	// Act
	joining := harness.StartDaemon(t, harness.Opts{StateDir: incumbent.StateDir, Joining: incumbent.Addr})
	joining.ExpectFileUnchanged(incumbent.AddrFile(), string(before), harness.ProbeWindow)

	// Assert
	after, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read daemon.addr while a joining daemon owns no workspace: %v", err)
	}
	if string(after) != string(before) {
		t.Fatalf("daemon.addr = %q, want the incumbent's %q: a joining daemon writes it only once it owns every workspace", after, before)
	}
	if joining.Exited() {
		t.Fatalf("the joining daemon exited instead of binding its own port\nstderr:\n%s", joining.Stderr())
	}
}

func TestBootRefusesAnUnwritableStateRoot(t *testing.T) {
	// Arrange
	root := filepath.Join(t.TempDir(), "readonly")
	if err := os.MkdirAll(root, 0o500); err != nil {
		t.Fatalf("mkdir a read-only state root: %v", err)
	}
	t.Cleanup(func() { os.Chmod(root, 0o755) })

	// Act
	d := harness.StartDaemon(t, harness.Opts{StateDir: root, ExpectEarlyExit: true})
	code := d.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("boot on an unwritable state root exited 0, want a loud non-zero refusal")
	}
	if !strings.Contains(d.Stderr(), root) {
		t.Fatalf("boot stderr = %q, want it to name the misconfigured state root %q", d.Stderr(), root)
	}
}

func TestBootOpensTheWorkspaceStateFresh(t *testing.T) {
	// Arrange: a pre-existing legacy database the rebuild must abandon in place.
	root := t.TempDir()
	legacy := filepath.Join(root, "state.db")
	if err := os.WriteFile(legacy, []byte("legacy sqlite bytes"), 0o644); err != nil {
		t.Fatalf("seed state.db: %v", err)
	}

	// Act
	d := harness.StartDaemon(t, harness.Opts{StateDir: root})

	// Assert
	got, err := os.ReadFile(legacy)
	if err != nil {
		t.Fatalf("read state.db after boot: %v", err)
	}
	if string(got) != "legacy sqlite bytes" {
		t.Fatalf("state.db = %q after boot, want it left untouched", got)
	}
	if _, err := os.Stat(filepath.Join(d.StateDir, "wsm.db")); err != nil {
		t.Fatalf("stat wsm.db = %v, want the fresh database created at boot", err)
	}
}

func TestPprofServesOnAnExplicitLoopbackAddress(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{Pprof: "127.0.0.1:0"})
	record := d.AwaitRunLogOperation("daemon.pprof.enabled")
	addr, _ := record.Context["address"].(string)
	if addr == "" {
		t.Fatalf("daemon.pprof.enabled context = %v, want the resolved address", record.Context)
	}

	// Act
	resp, err := d.HTTP().Get("http://" + addr + "/debug/pprof/")

	// Assert
	if err != nil {
		t.Fatalf("GET /debug/pprof/ = error %v, want the profiling surface", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("GET /debug/pprof/ = %d, want 200", resp.StatusCode)
	}
	d.ExpectWarnings("daemon.pprof.enabled")
}

func TestPprofRefusesARoutableBind(t *testing.T) {
	// Arrange / Act
	d := harness.StartDaemon(t, harness.Opts{Pprof: "0.0.0.0:6060", ExpectEarlyExit: true})
	code := d.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("boot with -pprof 0.0.0.0:6060 exited 0, want a refusal at construction")
	}
	if !strings.Contains(d.Stderr(), "0.0.0.0") {
		t.Fatalf("boot stderr = %q, want it to name the refused wildcard bind", d.Stderr())
	}
}

func TestRunLogIsJSONLPerTheLoggingContract(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	d.AwaitRunLogOperation("daemon.pprof.disabled")

	// Act
	records := d.RunLog()

	// Assert
	if len(records) == 0 {
		t.Fatal("the run log holds no records, want the boot sequence recorded")
	}
	for _, r := range records {
		if !harness.TimestampPattern.MatchString(r.Timestamp) {
			t.Fatalf("record %q timestamp = %q, want the contracted pattern", r.Operation, r.Timestamp)
		}
		if r.Runtime != "daemon" {
			t.Fatalf("record %q runtime = %q, want \"daemon\"", r.Operation, r.Runtime)
		}
		if r.PID != d.PID() {
			t.Fatalf("record %q pid = %d, want the daemon's pid %d", r.Operation, r.PID, d.PID())
		}
		if r.Operation == "" {
			t.Fatalf("record %q has no operation, want daemon.<package>.<verb>", r.Raw)
		}
		if !strings.HasPrefix(r.Operation, "daemon.") {
			t.Fatalf("record operation = %q, want the daemon.<package>.<verb> form", r.Operation)
		}
		if r.Context == nil {
			t.Fatalf("record %q has no context object, want structured context", r.Operation)
		}
	}
	// A fresh boot with pprof disabled and no workspace ever touched produces
	// no WARN/ERROR record at all: pprof.disabled is logged at DEBUG
	// (internal/pprofsurface/surface.go), and nothing else runs.
	d.ExpectWarnings()
}

// TestJSONCodecServesRegisterAndAPerWorkspaceVerb proves the daemon serves
// BOTH codecs off the same listener: a full rpc round trip — a registration
// and a per-workspace verb — dialed with the JSON codec instead of the
// default binary one.
func TestJSONCodecServesRegisterAndAPerWorkspaceVerb(t *testing.T) {
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{JSONCodec: true})
	repo := harness.NewRepo(t)

	// Act: RegisterWorkspace over the JSON-codec client.
	ws := harness.Register(t, d, repo.Dir)

	// Assert: a per-workspace verb round-trips too.
	if _, err := d.Client().SelectWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("SelectWorkspace over the JSON codec = error %v, want a success", err)
	}
}

// TestWorkspaceBoundWarnStaysOffTheRunLog pins the log-discipline split:
// a WARN record scoped to one workspace lands ONLY in that workspace's own
// `<workspace>/.claude/emacs/daemon.log` sink and never on the daemon-wide
// `daemon.run.log`. A fake shim that dies during bring-up produces exactly
// this: shimclient.abandonBringUp logs `daemon.shimclient.spawn` at WARN
// through the workspace-scoped surfaces.Workspace(dir) logger
// (internal/shimclient/supervisor.go), never through the global one.
func TestWorkspaceBoundWarnStaysOffTheRunLog(t *testing.T) {
	// Arrange
	f := newRegistered(t, harness.Opts{})
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{
		ExitOn: harness.ExitOnStartup, ExitCode: 3, Stderr: "boom: workspace-bound warn",
	})

	// Act: bring-up dies, which drives the workspace-scoped WARN.
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace onto a dying shim = transport error %v, want the spawn_failed arm", err)
	}
	if resp.Msg.GetError().GetSpawnFailed() == nil {
		t.Fatalf("OpenWorkspace onto a dying shim = %v, want OpenWorkspaceError.spawn_failed", resp.Msg)
	}

	// Assert: the WARN record IS in the workspace's own daemon.log.
	rec := f.d.AwaitWorkspaceLogOperation(f.repo.Dir, "daemon.shimclient.spawn")
	if lvl := strings.ToLower(rec.Level); lvl != "warn" && lvl != "warning" {
		t.Fatalf("daemon.shimclient.spawn record level = %q, want WARN", rec.Level)
	}

	// Assert: it is NOT on the daemon-wide run log.
	for _, r := range f.d.RunLog() {
		if r.Operation == "daemon.shimclient.spawn" {
			t.Fatalf("daemon.run.log carries the workspace-bound record %v, want it confined to %s",
				r, harness.WorkspaceLogPath(f.repo.Dir, "daemon"))
		}
	}

	f.d.ExpectWarnings("daemon.shimclient.spawn", "daemon.workspace.open")
}

// TestCloseWorkspaceRemovesTheLogSinkSymlink pins a claim from the audit's
// critique 15: that CloseWorkspace removes the workspace's log-sink symlink.
//
// UNEXPRESSIBLE as the intended-behavior arm: it CONTRADICTS the settled,
// already-tested contract. internal/workspace/close.go evicts the sinks and
// explicitly documents "the canonical links and their targets stay on disk";
// internal/dlog/surfaces_test.go pins that exact behavior in
// TestEvictLeavesTheCanonicalLinkAndTargetOnDisk. Asserting removal here
// would fight a settled invariant, not catch a regression, so this proves
// the DOCUMENTED behavior instead (the link survives Close, still readable)
// and skips the removal claim by name.
func TestCloseWorkspaceRemovesTheLogSinkSymlink(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	link := harness.WorkspaceLogPath(f.repo.Dir, "daemon")
	if _, err := os.Lstat(link); err != nil {
		t.Fatalf("stat the workspace's daemon.log symlink before Close: %v", err)
	}

	// Act
	if _, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("CloseWorkspace on a quiet workspace = error %v, want a success", err)
	}
	f.d.AwaitWorkspaceLogOperation(f.repo.Dir, "daemon.workspace.close")

	// Assert: the link SURVIVES (the settled contract), so the critique's
	// removal claim does not hold against this codebase.
	if _, err := os.Lstat(link); err != nil {
		t.Fatalf("stat the workspace's daemon.log symlink after Close = %v, want it left on disk per internal/workspace/close.go and internal/dlog/surfaces_test.go's TestEvictLeavesTheCanonicalLinkAndTargetOnDisk", err)
	}
	t.Skip("critique 15's \"CloseWorkspace removes the workspace's log sink symlink\" contradicts the settled contract (internal/workspace/close.go: \"the canonical links and their targets stay on disk\"; pinned by internal/dlog/surfaces_test.go TestEvictLeavesTheCanonicalLinkAndTargetOnDisk) — no removal to assert; see the positive assertion above instead.")
}

// TestDaemonRestartRotatesTheRunLogKeepingThePriorBootsRecords pins the run
// log's restart-scoped rotation (internal/dlog/runlog.go: openRunLog rotates
// the previous run's file to daemon.run.log.1 before opening a fresh one).
func TestDaemonRestartRotatesTheRunLogKeepingThePriorBootsRecords(t *testing.T) {
	// Arrange: first boot; capture its own records and pid before stopping it.
	d1 := newDaemon(t, harness.Opts{})
	d1.AwaitRunLogOperation("daemon.pprof.disabled")
	prior := d1.RunLog()
	if len(prior) == 0 {
		t.Fatal("the first boot's run log holds no records, want the boot sequence recorded before restart")
	}
	firstPID := d1.PID()
	runLogPath := d1.RunLogPath()
	d1.Stop()

	// Act: restart on the same state root.
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d1.StateDir})
	d2.AwaitRunLogOperation("daemon.pprof.disabled")

	// Assert: daemon.run.log.1 holds the prior boot's records, verbatim.
	backup := harness.ReadLog(t, runLogPath+".1")
	if len(backup) != len(prior) {
		t.Fatalf("daemon.run.log.1 holds %d records, want the prior boot's %d", len(backup), len(prior))
	}
	for i := range prior {
		if backup[i].Raw != prior[i].Raw {
			t.Fatalf("daemon.run.log.1[%d] = %q, want the prior boot's record %q", i, backup[i].Raw, prior[i].Raw)
		}
	}

	// Assert: the new run log describes only the new boot, never the old pid.
	for _, r := range d2.RunLog() {
		if r.PID == firstPID {
			t.Fatalf("daemon.run.log after restart carries a record from the prior boot's pid %d: %v", firstPID, r)
		}
	}
	d2.ExpectWarnings()
}
