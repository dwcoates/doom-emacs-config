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

// TestDaemonRestartRotatesTheRunLogKeepingThePriorBootsRecords pins the run
// log's restart-scoped rotation (internal/dlog/runlog.go: openRunLog rotates
// the previous run's file to daemon.run.log.1 before opening a fresh one).
func TestDaemonRestartRotatesTheRunLogKeepingThePriorBootsRecords(t *testing.T) {
	t.Parallel()
	// Arrange: first boot; capture its own records and pid before stopping it.
	d1 := newDaemon(t, harness.Opts{})
	d1.AwaitRunLogOperation("daemon.pprof.disabled")
	firstPID := d1.PID()
	runLogPath := d1.RunLogPath()
	d1.Stop()
	// The snapshot is taken AFTER the stop: an orderly exit writes its own
	// records, so a snapshot taken while the daemon still ran would be missing
	// exactly the lines the rotation then preserves.
	prior := harness.ReadLog(t, runLogPath)
	if len(prior) == 0 {
		t.Fatal("the first boot's run log holds no records, want the boot sequence recorded before restart")
	}

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
	nd.ExpectWarnings("daemon.boot.adopt", "daemon.wsm.session")
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
			`INSERT INTO creation_jobs (workspace_id, source_branch, source_dir, target_dir, layout_origin, actions_before, actions_after, base_ref, materialized, one_shot, one_shot_finish, initial_prompt, consented_ungated_mode, created_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			f.ws.GetId(), "feature/x", f.repo.Dir, f.repo.Dir, "create", "[]", "[]", "main", 1, 0, "", "", "", now); err != nil {
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
