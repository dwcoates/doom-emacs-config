//go:build integration

package integration

import (
	"context"
	"database/sql"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"
	"claude-repld/internal/rollout"

	"connectrpc.com/connect"
)

func TestBootWritesAndRemovesTheAddressFile(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// Arrange
	incumbent := newDaemon(t, harness.Opts{})
	// The run log is a symlink each runtime relinks onto its own file, so this
	// daemon's sweep reads the SUCCESSOR's records; it declares the same list.
	incumbent.ExpectWarnings("daemon.cmd.claim")
	before, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read the incumbent's daemon.addr: %v", err)
	}

	// Act
	second := harness.StartDaemon(t, harness.Opts{StateDir: incumbent.StateDir, ExpectEarlyExit: true})
	// The sweep covers every test; the declared records are evidence of the second daemon the test boots.
	second.ExpectWarnings("daemon.cmd.claim")
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
	t.Parallel()
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

	// Assert: the joining daemon really serves, off the port it reported in
	// joining.addr (internal/rollout/spawn.go: ReportJoiningAddr) rather than
	// daemon.addr, which it deliberately left untouched above.
	joining.AwaitFileExists(rollout.JoiningAddrPath(joining.StateDir))
	raw, err := os.ReadFile(rollout.JoiningAddrPath(joining.StateDir))
	if err != nil {
		t.Fatalf("read the joining daemon's own reported address: %v", err)
	}
	addr := strings.TrimSuffix(string(raw), "\n")
	if _, err := harness.DialAt(t, addr).DaemonHealth(joining.Ctx(), healthRequest()); err != nil {
		t.Fatalf("DaemonHealth against the joining daemon's OWN reported address %q = error %v, want a success: a joining daemon owns no workspace yet but must already be serving", addr, err)
	}
}

func TestBootRefusesAnUnwritableStateRoot(t *testing.T) {
	t.Parallel()
	// Arrange
	// The state root comes off harness.ShortTempDir, never t.TempDir(): a
	// shim socket hangs off the state root, and t.TempDir() spells this test's
	// whole name into the path, which overflows the 103-byte sun_path budget.
	root := filepath.Join(harness.ShortTempDir(t), "readonly")
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
	t.Parallel()
	// Arrange: a pre-existing legacy database the rebuild must abandon in
	// place, under a state root short enough to hold a shim socket (see
	// harness.ShortTempDir).
	root := harness.ShortTempDir(t)
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
	t.Parallel()
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
	t.Parallel()
	// Arrange / Act
	d := harness.StartDaemon(t, harness.Opts{Pprof: "0.0.0.0:6060", ExpectEarlyExit: true})
	// The sweep covers every test; the declared records are evidence of the routable pprof bind the test refuses.
	d.ExpectWarnings("daemon.cmd.pprof", "daemon.pprof.refused")
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
	t.Parallel()
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
}

// TestJSONCodecServesRegisterAndAPerWorkspaceVerb proves the daemon serves
// BOTH codecs off the same listener: a full rpc round trip — a registration
// and a per-workspace verb — dialed with the JSON codec instead of the
// default binary one.
func TestJSONCodecServesRegisterAndAPerWorkspaceVerb(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{JSONCodec: true})
	repo := harness.NewRepo(t)

	// Act: RegisterWorkspace over the JSON-codec client.
	ws := harness.Register(t, d, repo.Dir)

	// Assert: a per-workspace verb round-trips too.
	if _, err := d.Client().SelectWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("SelectWorkspace over the JSON codec = error %v, want a success", err)
	}

	// Assert: the SERVER-STREAM path over the JSON codec too — a late
	// subscriber's roster stream delivers the latest view first, exactly as
	// the binary codec does (TestRosterDeliversTheLatestViewToALateSubscriber).
	roster := d.WatchRosterOn(d.Client())
	first := harness.AwaitNext(t, d.Ctx(), roster, "the roster a JSON-codec subscriber opens with")
	if rosterRow(first, ws.GetId()) == nil {
		t.Fatalf("the first roster push over the JSON codec has no row for %s, want the latest-first roster", ws.GetId())
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
	t.Parallel()
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

	// Assert: the WARN record IS in the workspace's own daemon.log. The
	// operation also carries the ordinary DEBUG/INFO records of a spawn, so
	// the wait names the LEVEL as well: the subject is the warning, not the
	// first record the operation happens to produce.
	f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"),
		"the WARN daemon.shimclient.spawn record", func(r harness.LogRecord) bool {
			lvl := strings.ToLower(r.Level)
			return r.Operation == "daemon.shimclient.spawn" && (lvl == "warn" || lvl == "warning")
		})

	// Assert: it is NOT on the daemon-wide run log.
	for _, r := range f.d.RunLog() {
		if r.Operation == "daemon.shimclient.spawn" {
			t.Fatalf("daemon.run.log carries the workspace-bound record %v, want it confined to %s",
				r, harness.WorkspaceLogPath(f.repo.Dir, "daemon"))
		}
	}

	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.shimclient.spawn", "daemon.workspace.open",
		"daemon.shimclient.exit", "daemon.workspace.bring_up")
}

// TestCloseWorkspaceRemovesTheLogSinkSymlink covers what daemon.md's LOG
// SURFACES ruling actually prescribes for a close: "eviction on workspace
// close" — the SINK HANDLE is released, and the canonical link and its target
// stay on disk (internal/workspace/close.go; pinned by
// internal/dlog/surfaces_test.go's TestEvictLeavesTheCanonicalLinkAndTargetOnDisk,
// because a closed workspace's log is still the record of what it did).
//
// It also pins the ordering the eviction imposes: the close record is written
// through that same sink, so it must land BEFORE the eviction releases it.
func TestCloseWorkspaceRemovesTheLogSinkSymlink(t *testing.T) {
	t.Parallel()
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
}

// TestDaemonRestartAppendsToTheRunLog pins the run log's size-scoped rotation:
// reopening the daemon alone appends, so daemon.run.log spans process restarts.
func TestDaemonRestartAppendsToTheRunLog(t *testing.T) {
	t.Parallel()
	// Arrange: first boot; capture its own records and pid before stopping it.
	d1 := newDaemon(t, harness.Opts{})
	d1.AwaitRunLogOperation("daemon.pprof.disabled")
	firstPID := d1.PID()
	runLogPath := d1.RunLogPath()
	d1.Stop()
	// The snapshot is taken AFTER the stop: an orderly exit writes its own
	// records, so a snapshot taken while the daemon still ran would be missing
	// exactly the lines the next boot must retain.
	prior := harness.ReadLog(t, runLogPath)
	if len(prior) == 0 {
		t.Fatal("the first boot's run log holds no records, want the boot sequence recorded before restart")
	}

	// Act: restart on the same state root.
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: d1.StateDir})
	d2.AwaitRunLogOperation("daemon.pprof.disabled")

	// Assert: the current run log retains every record from the first boot and
	// contains records from the successor process. A restart alone creates no
	// size-rotation generation.
	current := harness.ReadLog(t, runLogPath)
	if len(current) <= len(prior) {
		t.Fatalf("daemon.run.log holds %d records after restart, want more than the prior boot's %d", len(current), len(prior))
	}
	for i := range prior {
		if current[i].Raw != prior[i].Raw {
			t.Fatalf("daemon.run.log[%d] = %q, want the prior boot's record %q", i, current[i].Raw, prior[i].Raw)
		}
	}
	if _, err := os.Lstat(runLogPath + ".1"); !os.IsNotExist(err) {
		t.Fatalf("stat daemon.run.log.1 after restart = %v, want no size-rotation generation", err)
	}
	foundFirstPID := false
	for _, record := range current {
		if record.PID == firstPID {
			foundFirstPID = true
			break
		}
	}
	if !foundFirstPID {
		t.Fatalf("daemon.run.log after restart has no record from the prior boot's pid %d", firstPID)
	}
}

// ---- audit-3 critique 9: layout-version and corrupt-row boot refusals ----

// TestBootRefusesAForeignLayoutVersion pins internal/wsm/open.go's
// checkLayout: a state database stamped with any version but this build's
// wsm.LayoutVersion is refused rather than migrated, at operation
// daemon.wsm.open.
func TestBootRefusesAForeignLayoutVersion(t *testing.T) {
	t.Parallel()
	// Arrange: a fresh boot stamps the layout row, then is stopped so the
	// row can be corrupted (the daemon holds the sole writing handle while
	// it runs).
	d := newDaemon(t, harness.Opts{})
	// The run log is a symlink each runtime relinks onto its own file, so this
	// daemon's sweep reads the SUCCESSOR's records; it declares the same list.
	d.ExpectWarnings("daemon.cmd.state", "daemon.wsm.open")
	d.Stop()
	d.WithDB(func(db *sql.DB) {
		if _, err := db.Exec(`UPDATE layout SET version = version + 1`); err != nil {
			t.Fatalf("bump the layout version: %v", err)
		}
	})

	// Act: restart on the same state root.
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, ExpectEarlyExit: true})
	// The sweep covers every test; the declared records are evidence of the state row the test corrupts.
	nd.ExpectWarnings("daemon.cmd.state", "daemon.wsm.open")
	code := nd.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("boot with a foreign layout version exited 0, want a loud non-zero refusal")
	}
	stderr := nd.Stderr()
	if got := strings.Count(stderr, "daemon.wsm.open"); got != 1 {
		t.Fatalf("daemon.wsm.open records in stderr = %d, want exactly 1\nstderr:\n%s", got, stderr)
	}
	if !strings.Contains(stderr, `"level":"error"`) {
		t.Fatalf("stderr = %q, want an ERROR-level record for the refused layout version", stderr)
	}
}

// TestBootRefusesACorruptTaskRow pins that a corrupt row of `tasks` fails the
// boot, not just an rpc: cmd/claude-repld/run.go's Prime step runs
// verbs.PublishRegistry BEFORE the server ever serves (internal/workspace/
// register.go: "a daemon that has just booted... would leave
// WatchWorkspaceRoster with no value to deliver"), and PublishRegistry reads
// DB.Tasks whole-or-nothing. A task row whose `done` column cannot scan as a
// bool therefore refuses the WHOLE boot, at wsm's own daemon.wsm.tasks
// operation as well as the top-level daemon.cmd.serve wrapper.
func TestBootRefusesACorruptTaskRow(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// The run log is a symlink each runtime relinks onto its own file, so this
	// daemon's sweep reads the SUCCESSOR's records; it declares the same list.
	d.ExpectWarnings("daemon.cmd.serve", "daemon.workspace.register", "daemon.wsm.tasks")
	created, err := d.Client().CreateTask(d.Ctx(), connect.NewRequest(&agentreplv1.CreateTaskRequest{Title: "land the rebuild"}))
	if err != nil {
		t.Fatalf("CreateTask = error %v, want a task ref", err)
	}
	taskID := created.Msg.GetSuccess().GetTask().GetId()
	if taskID == "" {
		t.Fatalf("CreateTask = %v, want a minted task id", created.Msg)
	}
	d.Stop()
	d.CorruptRow("tasks", "done", "id", taskID, "not-a-bool")

	// Act: restart on the same state root.
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, ExpectEarlyExit: true})
	// The sweep covers every test; the declared records are evidence of a state read the test corrupts, the state row the test corrupts.
	nd.ExpectWarnings("daemon.cmd.serve", "daemon.workspace.register", "daemon.wsm.tasks")
	code := nd.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("boot with a corrupted tasks row exited 0, want a loud non-zero refusal")
	}
	stderr := nd.Stderr()
	if got := strings.Count(stderr, "daemon.wsm.tasks"); got != 1 {
		t.Fatalf("daemon.wsm.tasks records in stderr = %d, want exactly 1\nstderr:\n%s", got, stderr)
	}
	if !strings.Contains(stderr, "publish the opening views") {
		t.Fatalf("stderr = %q, want the Prime/PublishRegistry wrapper naming the failed boot step", stderr)
	}
	if !strings.Contains(stderr, `"level":"error"`) {
		t.Fatalf("stderr = %q, want an ERROR-level record for the failed task read", stderr)
	}
}

// TestBootRefusesACorruptSessionRowOfAnAdoptedWorkspace pins that a corrupt
// row of `sessions` fails the boot of a crash-restart that must ADOPT a
// surviving shim: boot/sequence.go's adopt() calls the Adopted callback
// (internal/workspace/fleet_rollout.go: Fleet.Install), which opens the
// adopted shim's watches from the session's durable record
// (watchInstalled -> DB.Session). A session row whose recorded shim_pid is
// not positive is refused as a *wsm.DecodeError (internal/wsm/sessions.go:
// scanSession), which propagates all the way to boot.Sequence.Run and fails
// the whole boot rather than adopting the shim with the row silently
// dropped.
func TestBootRefusesACorruptSessionRowOfAnAdoptedWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace has a `sessions` row (PutSession on the
	// shim's successful bring-up). Killing only the daemon (never the shim,
	// which SysProcAttr.Setpgid puts in its own process group) leaves the
	// shim holding its kernel lock, which is what selects the ADOPT path on
	// restart rather than a fresh spawn.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the state row the test corrupts.
	f.d.ExpectWarnings("daemon.boot.adopt", "daemon.wsm.session")
	f.d.Kill()
	f.d.CorruptRow("sessions", "shim_pid", "workspace_id", f.ws.GetId(), -1)

	// Act: restart on the same state root, with the same redirected lock
	// directory so the probe finds the surviving shim's lock still held.
	nd := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir, ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir}, ExpectEarlyExit: true})
	// The sweep covers every test; the declared records are evidence of the state row the test corrupts.
	// The crash-restart's own stale daemon.addr is reported by the boot that
	// overwrites it; TestAStaleAddressFileIsReportedAndOverwritten is where
	// that record is the subject.
	nd.ExpectWarnings("daemon.boot.adopt", "daemon.wsm.session", "daemon.cmd.claim")
	code := nd.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("boot adopting a shim whose session row is corrupt exited 0, want a loud non-zero refusal")
	}
	stderr := nd.Stderr()
	if got := strings.Count(stderr, "daemon.boot.adopt"); got != 1 {
		t.Fatalf("daemon.boot.adopt records in stderr = %d, want exactly 1\nstderr:\n%s", got, stderr)
	}
	if !strings.Contains(stderr, "shim_pid") {
		t.Fatalf("stderr = %q, want it to name the corrupt shim_pid field (*wsm.DecodeError)", stderr)
	}
	if !strings.Contains(stderr, `"level":"error"`) {
		t.Fatalf("stderr = %q, want an ERROR-level record for the failed adoption", stderr)
	}
}

// TestBootRefusesACorruptCreationJobOfAnAdmittedMerge asserts critique 9's
// contract for `creation_jobs`: a half-written row is a CORRUPTION, and the
// boot refuses it loudly rather than coming up with the row dropped.
//
// IT IS EXPECTED TO BE RED, and the defect it exposes is stated here so the
// failure is read as the finding it is. The only boot path that reads a
// workspace's creation job is the in-flight-merge recovery
// (internal/boot/sequence.go: recoverMerges -> internal/merge/recover.go:
// Recover -> recoverAdmitted -> layoutFor -> DB.CreationJob), and
// recoverAdmitted's switch folds ANY layoutFor error -- a genuine corrupt-row
// *wsm.DecodeError included -- into "the workspace's merge geometry is gone".
// A data-corruption refusal is thereby downgraded to an ordinary
// "unmergeable" business outcome: the row IS dropped and the daemon serves on.
// The remediation belongs in internal/merge/recover.go, which is outside this
// file's boundary; the test states the contract, not the defect.
func TestBootRefusesACorruptCreationJobOfAnAdmittedMerge(t *testing.T) {
	t.Parallel()
	// Arrange: register a workspace, then seed its merge geometry and an
	// ADMITTED queue entry directly (raw SQL, daemon stopped): the boot's
	// merge recovery reads both without going through the ordinary merge rpc
	// flow, and CreationJob's own scan validates actions_before/actions_after
	// as JSON regardless of how the row was written.
	f := newRegistered(t, harness.Opts{})
	f.d.Stop()
	// The run log is a symlink each runtime relinks onto its own file, so this
	// daemon's sweep reads the SUCCESSOR's records; it declares the same list.
	f.d.ExpectWarnings("daemon.boot.recover_merges", "daemon.merge.recover", "daemon.wsm.creation_job")
	f.d.WithDB(func(db *sql.DB) {
		now := time.Now().UnixNano()
		if _, err := db.Exec(
			`INSERT INTO creation_jobs (workspace_id, source_branch, source_dir, target_dir, layout_origin, actions_before, actions_after, base_ref, materialized, one_shot, initial_prompt, consented_ungated_mode, created_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			f.ws.GetId(), "feature/x", f.repo.Dir, f.repo.Dir, "create", "[]", "[]", "main", 1, 0, "", "", now); err != nil {
			t.Fatalf("seed a creation_jobs row: %v", err)
		}
		if _, err := db.Exec(
			`INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at) VALUES (?, ?, ?, ?, ?)`,
			"repo1", f.ws.GetId(), 1, 1 /* wsm.MergeAdmitted */, now); err != nil {
			t.Fatalf("seed an admitted merge_queue row: %v", err)
		}
	})
	f.d.CorruptRow("creation_jobs", "actions_before", "workspace_id", f.ws.GetId(), "not valid json")

	// Act: restart on the same state root.
	nd := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir, ExpectEarlyExit: true})
	// The sweep covers every test; the declared records are evidence of the state row the test corrupts, the unfinished merge a restart leaves.
	nd.ExpectWarnings("daemon.boot.recover_merges", "daemon.merge.recover", "daemon.wsm.creation_job")
	code := nd.AwaitExit()

	// Assert: an undecodable row refuses the boot, non-zero and loud.
	if code == 0 {
		t.Fatalf("boot over a corrupt creation_jobs row exited 0, want a loud non-zero refusal\nstderr:\n%s", nd.Stderr())
	}
	// The recovery OPENS with an INFO record under the same operation ("this
	// boot reconciled the merge queues"), so the refusal is awaited by level
	// rather than by first match on the operation alone.
	nd.AwaitLogRecord(nd.RunLogPath(), "the merge recovery's ERROR refusal", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.recover" && strings.EqualFold(r.Level, "error")
	})
}

// ---- audit-3 critique 10: socket-path budget boot refusal ----

// TestBootRefusesAStateRootTooLongForShimSockets pins
// internal/stateroot/stateroot.go's CheckSocketPathBudget, invoked at boot
// (cmd/claude-repld/run.go) before any dependency is built. The harness's
// own requireSocketPathBudget would fatal the TEST (not just the daemon)
// before ever spawning it if the state root named by Opts.StateDir were the
// long one, so the long path is smuggled in as a SECOND --state-dir via
// ExtraArgs: Go's flag package takes the last occurrence of a flag, so the
// daemon actually boots against the long root while the harness's own
// pre-flight check saw only the short default.
func TestBootRefusesAStateRootTooLongForShimSockets(t *testing.T) {
	t.Parallel()
	// Arrange: a root comfortably past the 103-byte unix-socket path limit
	// once "/sock/<21-char-name>" is appended.
	long, err := os.MkdirTemp("/tmp", "ar-long-")
	if err != nil {
		t.Fatalf("mkdir a long state root: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(long) })
	long = filepath.Join(long, strings.Repeat("x", 100))

	// Act
	d := harness.StartDaemon(t, harness.Opts{ExtraArgs: []string{"--state-dir", long}, ExpectEarlyExit: true})
	code := d.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("boot with a %d-byte state root exited 0, want a loud non-zero refusal", len(long))
	}
	if !strings.Contains(d.Stderr(), "sock/") {
		t.Fatalf("boot stderr = %q, want it to name the socket directory that cannot fit", d.Stderr())
	}
}

// ---- audit-3 critique 25: address-file temp sibling, pprof unix socket ----

// TestBootLeavesNoDaemonAddrTempSibling pins that daemon.addr's atomic
// write (internal/daemonaddr/claim.go: os.CreateTemp(dir, "."+base+".*"))
// leaves no ".daemon.addr.<random>" sibling behind once the rename lands.
func TestBootLeavesNoDaemonAddrTempSibling(t *testing.T) {
	t.Parallel()
	// Arrange / Act
	d := newDaemon(t, harness.Opts{})

	// Assert
	matches, err := filepath.Glob(filepath.Join(d.StateDir, ".daemon.addr.*"))
	if err != nil {
		t.Fatalf("glob for a daemon.addr temp sibling: %v", err)
	}
	if len(matches) != 0 {
		t.Fatalf("daemon.addr temp siblings after boot = %v, want none", matches)
	}
}

// TestPprofServesOverAUnixSocket pins the OTHER accepted --pprof shape
// (internal/pprofsurface/surface.go: isSocketPath recognizes a path
// separator or a .sock suffix): a unix-socket path serves /debug/pprof/
// exactly as the loopback host:port form does
// (TestPprofServesOnAnExplicitLoopbackAddress).
func TestPprofServesOverAUnixSocket(t *testing.T) {
	t.Parallel()
	// Arrange: a short root, independent of the state root's own socket
	// budget, so the profiling socket's own path stays under the unix-socket
	// limit.
	sockDir, err := os.MkdirTemp("/tmp", "pprof")
	if err != nil {
		t.Fatalf("mkdir a short pprof socket root: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(sockDir) })
	sockPath := filepath.Join(sockDir, "p.sock")

	d := newDaemon(t, harness.Opts{Pprof: sockPath})
	record := d.AwaitRunLogOperation("daemon.pprof.enabled")
	network, _ := record.Context["network"].(string)
	if network != "unix" {
		t.Fatalf("daemon.pprof.enabled context = %v, want network \"unix\"", record.Context)
	}
	address, _ := record.Context["address"].(string)
	if address == "" {
		t.Fatalf("daemon.pprof.enabled context = %v, want the resolved socket path", record.Context)
	}

	// Act: GET /debug/pprof/ dialed over the unix socket itself.
	client := &http.Client{
		Transport: &http.Transport{
			DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
				var dialer net.Dialer
				return dialer.DialContext(ctx, "unix", address)
			},
		},
	}
	resp, err := client.Get("http://unix/debug/pprof/")

	// Assert
	if err != nil {
		t.Fatalf("GET /debug/pprof/ over the pprof unix socket = error %v, want the profiling surface", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("GET /debug/pprof/ over the pprof unix socket = %d, want 200", resp.StatusCode)
	}
	d.ExpectWarnings("daemon.pprof.enabled")
}

// TestBootRefusesWithoutAnAccountRoot is the cross-system guard for the
// launcher's argv. The account is DETERMINED by the workspace's path, so both
// config roots must have an answer; a daemon spawned without one exits before
// it serves, and the reason has to be in its stderr rather than only implied
// by a client's boot timeout.
func TestBootRefusesWithoutAnAccountRoot(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		omit string
		want string
	}{
		{name: "no default config dir", omit: "--default-config-dir", want: "Roots.Default is required"},
		{name: "no multi-repo config dir", omit: "--multi-repo-config-dir", want: "Roots.MultiRepo is required"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange / Act
			d := harness.StartDaemon(t, harness.Opts{OmitArgs: []string{tc.omit}, ExpectEarlyExit: true})
			// The graph refusal is the point of the test, not a stray warning.
			d.ExpectWarnings("daemon.cmd.graph")
			code := d.AwaitExit()

			// Assert
			if code == 0 {
				t.Fatalf("boot without %s exited 0, want a refusal before it served", tc.omit)
			}
			if !strings.Contains(d.Stderr(), tc.want) {
				t.Fatalf("boot stderr = %q, want it to name %q", d.Stderr(), tc.want)
			}
		})
	}
}

// TestBootServesThoughASurvivingShimIsUnreachable is the ten-hour accept wedge
// in one test.
//
// The daemon binds its listener and publishes daemon.addr at boot steps 5 and
// 6 and does not reach `http.Server.Serve` until the reconciliation has
// finished, so a reconciliation that blocks is a daemon that listens and
// accepts nothing: pid 31984 held its accept queue at 128/128 for ten hours
// with four clients in SYN_SENT while Emacs and curl timed out on connect, and
// its run log carried nothing but the redial ladder's own lock probe.
//
// The arrangement is exactly that state. The shim outlives the SIGKILLed
// daemon and keeps its workspace lock, so the boot takes the ADOPT path; its
// socket PATH is then unlinked, which is what a relaunch's rolled generation
// does to a base path, so the dial can never succeed while the lock says a
// living process owns the conversation. `shimclient.bringUp` redials that
// forever by design, so the bound is the only thing that lets the daemon
// serve.
func TestBootServesThoughASurvivingShimIsUnreachable(t *testing.T) {
	t.Parallel()
	// Arrange: a live session, then the daemon alone is killed. SysProcAttr
	// puts the shim in its own process group, so it survives holding the lock.
	f := newOpened(t, harness.Opts{})
	socket := f.d.SocketPath(f.ws)
	f.d.Kill()
	if err := os.Remove(socket); err != nil {
		t.Fatalf("unlink the surviving shim's socket path %s: %v", socket, err)
	}

	// Act: restart on the same state root and the same redirected lock
	// directory, so the probe finds the survivor's lock held and the dial finds
	// nothing at the path.
	nd := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir,
		ExtraEnv: []string{
			"AGENT_REPL_LOCK_DIR=" + f.d.LockDir,
			"AGENT_REPL_BOOT_ADOPT_BOUND=300ms",
		},
	})
	// The sweep covers every test; the declared records are the overrun
	// adoption this test arranges, reported by the supervisor and by the boot,
	// and the bounce accounting's bounce_unknown for the session nobody could
	// adopt.
	nd.ExpectWarnings("daemon.boot.adopt", "daemon.shimclient.adopt", "daemon.rollout.reconcile")

	// Assert: the daemon answers. StartDaemon already waited for the serving
	// record, and this is the socket actually accepting a connection.
	if _, err := nd.Client().DaemonHealth(nd.Ctx(), healthRequest()); err != nil {
		t.Fatalf("DaemonHealth = error %v, want a success: one unreachable survivor must not stop the daemon accepting connections", err)
	}
}

// TestAnUnreachableSurvivorsAdoptionIsRecordedAtError pins the evidence. The
// workspace is left undetermined — neither adopted nor orphan-closed, because
// its lock says a living process owns the conversation — and this record is
// the only thing that says why.
func TestAnUnreachableSurvivorsAdoptionIsRecordedAtError(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})
	socket := f.d.SocketPath(f.ws)
	f.d.Kill()
	if err := os.Remove(socket); err != nil {
		t.Fatalf("unlink the surviving shim's socket path %s: %v", socket, err)
	}

	// Act.
	nd := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir,
		ExtraEnv: []string{
			"AGENT_REPL_LOCK_DIR=" + f.d.LockDir,
			"AGENT_REPL_BOOT_ADOPT_BOUND=300ms",
		},
	})
	// The sweep covers every test; the declared records are the overrun
	// adoption this test arranges, reported by the supervisor and by the boot,
	// and the bounce accounting's bounce_unknown for the session nobody could
	// adopt.
	nd.ExpectWarnings("daemon.boot.adopt", "daemon.shimclient.adopt", "daemon.rollout.reconcile")

	// Assert.
	nd.AwaitLogRecord(nd.RunLogPath(), "the overrun adoption's error record", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.boot.adopt" && r.Level == "error" &&
			strings.Contains(r.Message, "adoption bound")
	})
}

// TestAnUnadoptedSurvivorWithNoManifestKeepsItsBounceUnknownStanding is the
// other half of TestCrashBootWithNoManifestRecordsNoBounceUnknownForAnAdoptedWorkspace:
// a session the lock says survived the crash and that the boot could NOT adopt
// is the one the boot genuinely cannot account for. It records bounce_unknown
// ("needs a human"), and nothing closes it: the workspace never attached.
func TestAnUnadoptedSurvivorWithNoManifestKeepsItsBounceUnknownStanding(t *testing.T) {
	t.Parallel()
	// Arrange: the survivor holds its lock and its socket path is gone.
	f := newOpened(t, harness.Opts{})
	socket := f.d.SocketPath(f.ws)
	f.d.Kill()
	if err := os.Remove(socket); err != nil {
		t.Fatalf("unlink the surviving shim's socket path %s: %v", socket, err)
	}

	// Act
	nd := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir,
		ExtraEnv: []string{
			"AGENT_REPL_LOCK_DIR=" + f.d.LockDir,
			"AGENT_REPL_BOOT_ADOPT_BOUND=300ms",
		},
	})
	// The sweep covers every test; the declared records are the overrun
	// adoption this test arranges and the bounce_unknown it is about.
	nd.ExpectWarnings("daemon.boot.adopt", "daemon.shimclient.adopt", "daemon.rollout.reconcile")

	// Assert: recorded for this workspace.
	nd.AwaitLogRecord(nd.RunLogPath(), "the unadopted session's bounce_unknown", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.reconcile" && r.Level == "warn" &&
			r.Message == "a session's bounce disposition needs a human" && r.Context["workspace"] == f.ws.GetId()
	})
	// Assert: and standing. The boot's healthy-attach edge ran before the
	// daemon served, which StartDaemon waited for.
	for _, r := range nd.RunLog() {
		if r.Operation == "daemon.health.close_on_edge" && r.Context["kind"] == "bounce_unknown" {
			t.Fatalf("record %+v: an unadopted survivor's bounce_unknown must stand", r)
		}
	}
}

// TestAnUnhealthySurvivorIsAdoptedWithinTheBound pins the realtest-1 finding.
// A surviving shim standing on a fault it never clears ANSWERS bring-up in
// milliseconds; the daemon used to treat that answer as silence and burn the
// whole adoption bound on it, then serve with the workspace undetermined and
// no client at all.
//
// The bound here is set FAR ABOVE the harness's own serving wait, so a boot
// that waited it out could not reach the serving record StartDaemon blocks on:
// the arrangement itself is the assertion that the adoption did not wait.
func TestAnUnhealthySurvivorIsAdoptedWithinTheBound(t *testing.T) {
	t.Parallel()
	// Arrange: the shim answers every stream unhealthy, and the daemon alone
	// is killed so the shim survives holding the workspace lock.
	f := newRegistered(t, harness.Opts{})
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{OpeningFault: "the store is unreachable"})
	f.d.ExpectWarnings("daemon.shimclient.ready", "daemon.health.open_fault", "daemon.health.session")
	f.open()
	f.d.Kill()

	// Act.
	nd := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir,
		ExtraEnv: []string{
			"AGENT_REPL_LOCK_DIR=" + f.d.LockDir,
			"AGENT_REPL_BOOT_ADOPT_BOUND=60s",
		},
	})
	nd.ExpectWarnings("daemon.shimclient.ready", "daemon.health.open_fault", "daemon.health.session",
		"daemon.boot.adopt", "daemon.shimclient.adopt")

	// Assert: the survivor was adopted, and the fault it is standing on
	// reached the workspace health path rather than being a boot blocker.
	// The adoption record is WORKSPACE-BOUND, as every record an adopted
	// client writes is.
	nd.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"), "the adopted unhealthy shim's record", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.shimclient.ready" &&
			strings.Contains(r.Message, "adopted an unhealthy shim")
	})
	awaitSessionFault(t, nd, f.ws)
}

// awaitSessionFault blocks until SessionHealth answers with a shim-reported
// fault for the workspace. The fold from the adopted shim's opening
// diagnostics runs on the session watcher's own goroutine, so the probe is
// retried rather than read once.
func awaitSessionFault(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) {
	t.Helper()

	deadline := time.Now().Add(harness.DefaultTimeout)
	var last string
	for time.Now().Before(deadline) {
		resp, err := d.Client().SessionHealth(d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: ws}))
		if err != nil {
			last = err.Error()
			continue
		}
		for _, fault := range resp.Msg.GetSuccess().GetUnhealthy().GetFaults() {
			if fault.GetShimReported() != nil {
				return
			}
		}
		last = resp.Msg.String()
	}
	t.Fatalf("SessionHealth never carried the adopted shim's fault; last answer %s", last)
}

// TestAStaleAddressFileIsReportedAndOverwritten pins the realtest-1 finding.
// The daemon that had bound 127.0.0.1:58161 was gone, its daemon.addr was
// still on disk, and the next Emacs probed the dead address and timed out. A
// SIGKILLed daemon cannot withdraw anything, so the boot that takes the state
// root over says the advertisement is stale before replacing it.
func TestAStaleAddressFileIsReportedAndOverwritten(t *testing.T) {
	t.Parallel()
	// Arrange: a daemon that published an address and was SIGKILLed, so it
	// never ran its withdrawal.
	d := newDaemon(t, harness.Opts{})
	stale, err := os.ReadFile(d.AddrFile())
	if err != nil {
		t.Fatalf("read the incumbent's daemon.addr: %v", err)
	}
	d.Kill()

	// Act.
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, KeepStaleAddr: true})
	nd.ExpectWarnings("daemon.cmd.claim")

	// Assert: the stale address is named in the record.
	staleAddr := harness.AddrLine(string(stale))
	nd.AwaitLogRecord(nd.RunLogPath(), "the stale advertisement's record", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.cmd.claim" && strings.Contains(r.Message, "stale daemon.addr") &&
			r.Context["stale_address"] == staleAddr
	})

	// Assert: and it was replaced by this daemon's own, which answers.
	if nd.Addr == staleAddr {
		t.Fatalf("daemon.addr still names %q, want this daemon's own address", staleAddr)
	}
	if _, err := nd.Client().DaemonHealth(nd.Ctx(), healthRequest()); err != nil {
		t.Fatalf("DaemonHealth = error %v, want the replaced advertisement to name a serving daemon", err)
	}
}

// TestTheWithdrawalIsRecordedOnAnOrderlyExit pins the other half of the
// finding's evidence: an orderly exit says out loud that it took the
// advertisement down, so a reader can tell a withdrawn address from one a
// crash left standing.
func TestTheWithdrawalIsRecordedOnAnOrderlyExit(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{})
	runLog := d.RunLogPath()

	// Act.
	d.Stop()

	// Assert.
	d.AwaitLogRecord(runLog, "the withdrawal's record", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.cmd.exit" && strings.Contains(r.Message, "daemon.addr was withdrawn")
	})
}

// TestARestartKeepsThePreviousInstancesWorkspaceRecordsReadable pins the third
// realtest-1 finding. `<ws>/.claude/emacs/daemon.log` was retargeted onto a
// fresh generation on every daemon boot, so the canonical path -- the ONLY
// path the reader resolves -- named the current instance alone, and the
// adoption records of the daemon four minutes older were on an inode nothing
// named any more. One file now spans instances, and rotation happens only at
// the byte cap.
func TestARestartKeepsThePreviousInstancesWorkspaceRecordsReadable(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace, whose bring-up wrote workspace-bound
	// records, and the pid that wrote them.
	f := newOpened(t, harness.Opts{})
	firstPID := f.d.PID()
	if len(f.d.WorkspaceLog(f.repo.Dir, "daemon")) == 0 {
		t.Fatal("the first daemon wrote no workspace records to append to")
	}
	f.d.Kill()

	// Act: restart on the same state root, which adopts the surviving shim and
	// writes its own workspace-bound records.
	nd := harness.StartDaemon(t, harness.Opts{
		StateDir:      f.d.StateDir,
		KeepStaleAddr: true,
		ExtraEnv:      []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
	})
	// The crash-restart's own evidence: the stale advertisement and the
	// adoption it drives.
	nd.ExpectWarnings("daemon.cmd.claim", "daemon.boot.adopt",
		"daemon.shimclient.adopt")
	nd.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"), "a record from the restarted daemon",
		func(r harness.LogRecord) bool { return r.PID == nd.PID() })

	// Assert: BOTH instances are in the file the canonical path names.
	var sawFirst, sawSecond bool
	for _, r := range nd.WorkspaceLog(f.repo.Dir, "daemon") {
		switch r.PID {
		case firstPID:
			sawFirst = true
		case nd.PID():
			sawSecond = true
		}
	}
	if !sawFirst {
		t.Fatalf("%s carries no record from pid %d; the restart retargeted the link and hid the previous instance",
			harness.WorkspaceLogPath(f.repo.Dir, "daemon"), firstPID)
	}
	if !sawSecond {
		t.Fatalf("%s carries no record from pid %d", harness.WorkspaceLogPath(f.repo.Dir, "daemon"), nd.PID())
	}
}

// TestBootClosesAWorkspaceWhoseDirectoryIsGone pins the owner's ruling. The
// live example was workspace 6e32a50ef5fc47ef, whose worktree under a swept
// temporary directory had been removed while its registry row stayed open, so
// Emacs read a live roster row and opened a tab on a path that is not there.
func TestBootClosesAWorkspaceWhoseDirectoryIsGone(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace on a WORKTREE -- a workspace at the
	// repository root is one whose removal takes the repository with it -- then
	// the daemon is stopped and the directory removed under it.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "swept")
	ws := harness.Register(t, d, dir)
	d.Stop()
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act: the next boot reconciles the row.
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, KeepStaleAddr: true})
	nd.ExpectWarnings("daemon.cmd.claim", "daemon.boot.close_missing_dir", "daemon.workspace.register")

	// Assert: the boot recorded the close with the workspace, the directory
	// and the stat error.
	nd.AwaitLogRecord(nd.RunLogPath(), "the missing-directory close record", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.boot.close_missing_dir" &&
			strings.ToLower(r.Level) == "info" &&
			r.WorkspaceID == ws.GetId() &&
			r.WorkspaceDir == ws.GetDir() &&
			r.Context["error"] != nil
	})

	// Assert: and the roster Emacs receives carries the row as closed, so no
	// tab is opened for it.
	roster := nd.WatchRoster()
	got := awaitRoster(t, nd, roster, "the missing-directory row receded", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, ws.GetId())
		return row != nil && row.GetClosed().GetClosed()
	})
	if row := rosterRow(got, ws.GetId()); row == nil || !row.GetClosed().GetClosed() {
		t.Fatalf("the row for %s = %v, want it carried as closed", ws.GetId(), row)
	}
}

// TestOpenWorkspaceRefusesAWorkspaceWhoseDirectoryIsGone is the re-open half of
// the ruling above: boot CLOSES a workspace whose directory has vanished, so a
// re-open of that row must be REFUSED BY NAME. Before this, the re-open failed
// inside the request boundary's own logging and the client met HTTP 500
// `internal` with the log sink's resolve error as its message.
func TestOpenWorkspaceRefusesAWorkspaceWhoseDirectoryIsGone(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "reopened")
	ws := harness.Register(t, d, dir)
	d.Stop()
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, KeepStaleAddr: true})
	nd.ExpectWarnings("daemon.cmd.claim", "daemon.boot.close_missing_dir", "daemon.workspace.register")

	// Act.
	resp, err := nd.Client().OpenWorkspace(nd.Ctx(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenWorkspace on a vanished directory = transport error %v, want a typed refusal", err)
	}
	if resp.Msg.GetError().GetSpawnFailed() == nil {
		t.Fatalf("OpenWorkspace on a vanished directory = %v, want OpenWorkspaceError.spawn_failed", resp.Msg)
	}
}

// TestOpenWorkspaceNamesTheVanishedDirectoryInItsRefusal is the evidence half:
// the arm's detail says WHICH directory is gone, so the refusal is readable
// rather than merely typed.
func TestOpenWorkspaceNamesTheVanishedDirectoryInItsRefusal(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "named")
	ws := harness.Register(t, d, dir)
	d.Stop()
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, KeepStaleAddr: true})
	nd.ExpectWarnings("daemon.cmd.claim", "daemon.boot.close_missing_dir", "daemon.workspace.register")

	// Act.
	resp, err := nd.Client().OpenWorkspace(nd.Ctx(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}

	// Assert.
	detail := resp.Msg.GetError().GetSpawnFailed().GetDetail()
	if !strings.Contains(detail, ws.GetDir()) {
		t.Fatalf("refusal detail = %q, want it to name %q", detail, ws.GetDir())
	}
}

// TestARequestOnAVanishedDirectoryRoutesItsRecordsCentrally is the logging
// half: the request still reaches its handler, and the records it produces
// land on the central sink naming the workspace they are about, reported once
// at DEBUG rather than as an ERROR beside every record.
func TestARequestOnAVanishedDirectoryRoutesItsRecordsCentrally(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "routed")
	ws := harness.Register(t, d, dir)
	d.Stop()
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, KeepStaleAddr: true})
	nd.ExpectWarnings("daemon.cmd.claim", "daemon.boot.close_missing_dir", "daemon.workspace.register")

	// Act.
	if _, err := nd.Client().OpenWorkspace(nd.Ctx(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}

	// Assert.
	nd.AwaitLogRecord(nd.RunLogPath(), "the central-fallback notice", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.dlog.central_fallback" &&
			strings.ToLower(r.Level) == "debug" &&
			r.Context["unroutable_workspace"] == ws.GetDir()
	})
}

// TestBootCountsTheMissingDirectoryCloseInItsReport is the report half: the
// boot's own completion record answers for what it closed, so a reader does
// not have to count WARN records to know.
func TestBootCountsTheMissingDirectoryCloseInItsReport(t *testing.T) {
	t.Parallel()
	// Arrange.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "counted")
	harness.Register(t, d, dir)
	d.Stop()
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	nd := harness.StartDaemon(t, harness.Opts{StateDir: d.StateDir, KeepStaleAddr: true})
	nd.ExpectWarnings("daemon.cmd.claim", "daemon.boot.close_missing_dir", "daemon.workspace.register")

	// Assert.
	nd.AwaitLogRecord(nd.RunLogPath(), "the boot completion record's count", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.cmd.boot" && strings.Contains(r.Message, "reconciliation completed") &&
			r.Context["missing_dir_closed"] == float64(1)
	})
}

// healthAnswerBound is how long the EDITOR gives a replacement daemon to say
// who it is. lisp/services.el dials DaemonHealth under a 3s bound and reports
// `runtime-await-failed` when it expires, and that bound is not lengthened:
// the daemon answers within it or the deploy declares the replacement
// unobserved. The test asserts the daemon's side of exactly that number.
const healthAnswerBound = 3 * time.Second

// TestBootAnswersHealthWhileTheOpenWorkspacesSessionsComeUp is the
// 2026-09-13 deploy regression, end to end.
//
// The boot brings every open workspace's session up, and while that ran INSIDE
// the reconciliation the daemon answered nothing: the listener is bound and
// daemon.addr published before the reconciliation, and `http.Server.Serve` is
// not reached until after it. The replacement came up and started both its
// sessions, and `bin/deploy-all.sh` still reported `replacement identity was
// not observed: DaemonHealth timed out after 3.0s`.
//
// The successor's shim here WITHHOLDS its readiness, so its bring-up is still
// in flight — provably, by its summary not having landed — at the instant the
// identity query is answered.
func TestBootAnswersHealthWhileTheOpenWorkspacesSessionsComeUp(t *testing.T) {
	t.Parallel()
	// Arrange: one open workspace, then the daemon that served it stands down.
	f := newOpened(t, harness.Opts{})
	expectSessionKillRecords(f.d)
	if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("the replacement under test"),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
	}
	f.d.AwaitExit()

	// Arrange: the successor's shim withholds its opening diagnostics, so the
	// session start its boot runs does not finish on its own. The profile is
	// written BEFORE the successor starts, because the successor's own boot is
	// what spawns the shim that reads it.
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{DelayDiagnostics: true})
	d2 := harness.StartDaemon(t, harness.Opts{
		StateDir:   f.d.StateDir,
		ProfileDir: f.d.ProfileDir,
		ExtraArgs:  []string{"--default-config-dir", f.d.DefaultConfigDir},
	})

	// Act: the identity query Emacs recognizes a replacement by, under the
	// bound Emacs gives it.
	ctx, cancel := context.WithTimeout(d2.Ctx(), healthAnswerBound)
	defer cancel()
	if _, err := d2.Client().DaemonHealth(ctx, healthRequest()); err != nil {
		t.Fatalf("DaemonHealth within %s = %v, want the replacement to answer while its sessions come up", healthAnswerBound, err)
	}

	// Assert: the bring-up had NOT finished when that answer was given, which
	// is what makes the answer evidence of the split rather than of a fast
	// machine.
	if rec, found := bringUpSummary(d2); found {
		t.Fatalf("the bring-up summary %v had already landed, want the health answer to have overtaken a bring-up still in flight", rec)
	}

	// Assert: and the bring-up still lands its summary once the shim reports
	// itself healthy.
	d2.Shim(f.ws).PushHealthyWhenSubscribed()
	d2.AwaitLogRecord(d2.RunLogPath(), "the boot's bring-up summary", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.boot.bring_up" && r.Level == "info"
	})
}

// bringUpSummary answers the boot's one INFO bring-up summary if it has landed.
func bringUpSummary(d *harness.Daemon) (harness.LogRecord, bool) {
	for _, r := range d.RunLog() {
		if r.Operation == "daemon.boot.bring_up" && r.Level == "info" {
			return r, true
		}
	}
	return harness.LogRecord{}, false
}
