// Package e2e exercises the full post-cutover daemon⇄shim stack end to end:
// a real claude-repld frontend surface + per-session driver spawning the real
// TS claude-shim in UDS mode (--uds-socket + --store-socket, --fake offline),
// writing to a real agent-shim/shim-store, with the daemon consuming the
// merged stream and rendering agentshim.frontend.v1 frames onto the existing
// GET /sessions/{id}/stream WebSocket (scope-filtered per session).
//
// The shim bundle is built FROM SOURCE into the harness temp dir on every run
// (buildShim), so the suite can never exercise a stale checked-out dist/.
//
// Prerequisites (the test SKIPS loudly, never fails, when any is absent):
//   - node on PATH
//   - the shim's deps installed: agent-shim/claude/shim/node_modules
//     (run `npm ci` there — the harness NEVER installs anything itself)
//   - a buildable agent-shim/shim-store (go build)
//
// The harness sets AGENT_REPL_FORBID_VENDOR_CALLS for the whole test binary
// (see TestMain), and shim.Spawn inherits the daemon process environment, so
// the node shim it spawns inherits it too: neither side can reach the real
// Claude SDK or CLI from here.
package e2e

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"

	"claude-repld/internal/dlog"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessiondrv"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shim"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/merge"
)

var frameTimeout = 30 * time.Second

func repoRoot(t *testing.T) string {
	t.Helper()
	// daemon/e2e -> daemon -> agent-repl
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve repo root: %v", err)
	}
	return root
}

// buildShim bundles the TS shim FROM SOURCE into this test's temp dir, the way
// buildShimStore compiles the store. It deliberately does NOT run the
// checked-out dist/main.js: dist/ is gitignored and rebuilt by hand, so running
// it meant the e2e suite could silently be exercising a bundle older than the
// source it is supposed to cover. esbuild takes milliseconds, so building per
// run costs nothing and removes the staleness question entirely.
//
// It NEVER installs anything: a missing node_modules is a loud skip telling the
// operator to run `npm ci`, not an implicit network fetch from a test.
func buildShim(t *testing.T, node, buildSHA string) string {
	t.Helper()
	shimDir := filepath.Join(repoRoot(t), "agent-shim", "claude", "shim")
	if _, err := os.Stat(filepath.Join(shimDir, "node_modules")); err != nil {
		t.Skipf("shim deps not installed (%s/node_modules missing): run `npm ci` in %s",
			shimDir, shimDir)
	}
	// Build into <tmp>/dist/main.js and copy package.json to <tmp>: the bundle
	// reads its own version via a require of "../package.json" relative to
	// itself, and reporting "unknown" would be a needless divergence from the
	// production layout.
	// EvalSymlinks: t.TempDir() hands back /var/folders/... on macOS, which is a
	// symlink to /private/var/folders/.... Node resolves a module's own URL to
	// the real path, so spawning the bundle by the unresolved path used to make
	// its is-this-the-program self-check compare unequal and exit 0 in silence.
	// The bundle now resolves argv[1] itself; this keeps the harness's path
	// canonical anyway, so a failure here is never about symlinks.
	tmp, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("resolve shim build dir: %v", err)
	}
	outDir := filepath.Join(tmp, "dist")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("make shim build dir: %v", err)
	}
	pkg, err := os.ReadFile(filepath.Join(shimDir, "package.json"))
	if err != nil {
		t.Fatalf("read shim package.json: %v", err)
	}
	if err := os.WriteFile(filepath.Join(outDir, "..", "package.json"), pkg, 0o644); err != nil {
		t.Fatalf("stage shim package.json: %v", err)
	}
	out := filepath.Join(outDir, "main.js")
	cmd := exec.Command(node, "build.mjs")
	cmd.Dir = shimDir
	// SHIM_BUILD_SHA is the bundle's BUILD IDENTITY, baked in by build.mjs
	// exactly as bin/build-frontend.sh does it. A test bakes a stale one to
	// exercise the daemon's stale-shim refresh against a real bundle rather
	// than a stubbed hello.
	cmd.Env = append(os.Environ(), "SHIM_BUILD_OUTFILE="+out, "SHIM_BUILD_SHA="+buildSHA)
	if combined, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("build shim bundle: %v\n%s", err, combined)
	}
	return out
}

func nodePath(t *testing.T) string {
	t.Helper()
	node, err := exec.LookPath("node")
	if err != nil {
		t.Skip("node not found in PATH")
	}
	return node
}

// buildShimStore compiles the shim-store into a temp binary, skipping loudly if
// the build cannot run (no go toolchain, missing module).
func buildShimStore(t *testing.T) string {
	t.Helper()
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not found in PATH")
	}
	storeDir := filepath.Join(repoRoot(t), "agent-shim", "shim-store")
	if _, err := os.Stat(storeDir); err != nil {
		t.Skipf("shim-store source not present (%s)", storeDir)
	}
	bin := filepath.Join(t.TempDir(), "shim-store")
	cmd := exec.Command("go", "build", "-o", bin, ".")
	cmd.Dir = storeDir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Skipf("shim-store build failed (skipping UDS e2e): %v\n%s", err, out)
	}
	return bin
}

// startShimStore launches the store on a temp socket and waits for it to
// listen. It is torn down at test end.
func startShimStore(t *testing.T, bin, sock string) {
	t.Helper()
	dbPath := filepath.Join(t.TempDir(), "events.db")
	logPath := filepath.Join(t.TempDir(), "shim-store.log")
	cmd := exec.Command(bin, "-socket", sock, "-db", dbPath, "-log", logPath)
	cmd.Stderr = &testLogWriter{t: t, tag: "shim-store"}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start shim-store: %v", err)
	}
	t.Cleanup(func() {
		_ = cmd.Process.Kill()
		_, _ = cmd.Process.Wait()
	})
	// Wait for the socket to appear (the store creates it on listen).
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		if _, err := os.Stat(sock); err == nil {
			return
		}
		time.Sleep(20 * time.Millisecond)
	}
	t.Fatalf("shim-store did not create %s in time", sock)
}

type testLogWriter struct {
	t   *testing.T
	tag string
}

func (w *testLogWriter) Write(p []byte) (int, error) {
	w.t.Logf("[%s] %s", w.tag, bytes.TrimRight(p, "\n"))
	return len(p), nil
}

type testShimLogger struct{ t *testing.T }

func (l testShimLogger) Log(format string, args ...any) { l.t.Logf(format, args...) }

func (l testShimLogger) LogVerbose(format string, args ...any) { l.t.Logf(format, args...) }

// --- minimal WireAgentShim stubs (merge/lifecycle unused here) --------------

type stubMergeDirs struct{}

func (stubMergeDirs) Resolve(string) (merge.Request, error) {
	return merge.Request{}, fmt.Errorf("e2e: merge not exercised")
}

type stubLifecycle struct{}

func (stubLifecycle) Close(context.Context, string) error { return nil }
func (stubLifecycle) Open(context.Context, string) error  { return nil }

// emptyWorkspaceCreation is the explicit creation seam for E2E flows that do
// not exercise workspace creation. It is deliberately strict: a create,
// materialization acknowledgement, or host-action completion received by this
// harness fails loudly rather than pretending the daemon performed it. The
// non-nil typed subscriptions preserve WireAgentShim's production invariant
// without touching disk or another process.
type emptyWorkspaceCreation struct {
	available chan *frontendv1.WorkspaceAvailable
	actions   chan *frontendv1.HostAction
	closeOnce sync.Once
}

func newEmptyWorkspaceCreation() *emptyWorkspaceCreation {
	return &emptyWorkspaceCreation{
		available: make(chan *frontendv1.WorkspaceAvailable),
		actions:   make(chan *frontendv1.HostAction),
	}
}

func (e *emptyWorkspaceCreation) CreateWorkspace(context.Context, string, *frontendv1.CreateWorkspaceCmd) error {
	return fmt.Errorf("e2e: workspace creation is not exercised by this harness")
}

func (e *emptyWorkspaceCreation) MarkWorkspaceMaterialized(context.Context, string) error {
	return fmt.Errorf("e2e: workspace materialization is not exercised by this harness")
}

func (e *emptyWorkspaceCreation) CompleteHostAction(context.Context, string, bool, string) error {
	return fmt.Errorf("e2e: host actions are not exercised by this harness")
}

func (e *emptyWorkspaceCreation) SnapshotHostWork() server.WorkspaceHostWorkSnapshot {
	return server.WorkspaceHostWorkSnapshot{}
}

func (e *emptyWorkspaceCreation) SubscribeWorkspaceAvailable() (<-chan *frontendv1.WorkspaceAvailable, func()) {
	return e.available, e.close
}

func (e *emptyWorkspaceCreation) SubscribeHostActions() (<-chan *frontendv1.HostAction, func()) {
	return e.actions, e.close
}

func (e *emptyWorkspaceCreation) close() {
	e.closeOnce.Do(func() {
		close(e.available)
		close(e.actions)
	})
}

type e2eHarness struct {
	ts *httptest.Server
	// shimSpawns reports how many shim processes this harness has exec'd.
	shimSpawns func() int64
	// sweepIdle fires the daemon's idle sweeper, which is the production
	// hibernation trigger. Non-nil only for a harness built with
	// withIdleSweeper; a test that never asks for one cannot accidentally
	// hibernate its own session.
	sweepIdle chan<- time.Time
}

// harnessTuning is the small set of knobs a test may bend on the otherwise
// production-shaped stack. Every field is zero for a normal harness, so the
// default path stays exactly what production runs.
type harnessTuning struct {
	// wedgeShim replaces the spawned shim with a process that never dials the
	// daemon, so the bring-up handshake can never complete. It is the only way
	// to observe a stuck link end to end: a shim that is merely slow eventually
	// arrives and proves nothing about the failure path.
	wedgeShim bool
	// establishTimeout bounds one createSession establishment round. Zero takes
	// the daemon's own bound.
	establishTimeout time.Duration
	// shimBuildSHA is baked into the bundle this harness builds, and
	// currentBuildSHA is what the daemon believes the CURRENT bundle to be.
	// Setting them differently is a shim that survived a deploy — the exact
	// condition the stale-shim refresh exists for — with both halves real: a
	// genuinely built bundle reporting a genuinely injected identity.
	shimBuildSHA    string
	currentBuildSHA string
	// idleSweeper hands the daemon's idle sweeper a clock the TEST drives, so
	// a hibernation is provoked by an event rather than waited out. It is the
	// production trigger — sweepIdle calls driver.Hibernate — so what it
	// exercises is the real edge and not a stand-in for one.
	idleSweeper bool
}

type harnessOption func(*harnessTuning)

func withWedgedShim() harnessOption { return func(o *harnessTuning) { o.wedgeShim = true } }

// withStaleShimBuild bakes `baked` into the bundle while telling the daemon the
// current bundle is `current` — a shim that outlived a deploy.
func withStaleShimBuild(baked, current string) harnessOption {
	return func(o *harnessTuning) { o.shimBuildSHA, o.currentBuildSHA = baked, current }
}

// withIdleSweeper gives the harness a test-driven idle-sweep clock.
func withIdleSweeper() harnessOption { return func(o *harnessTuning) { o.idleSweeper = true } }

func withEstablishTimeout(d time.Duration) harnessOption {
	return func(o *harnessTuning) { o.establishTimeout = d }
}

func assertCanonicalShimLog(t *testing.T, path string, workspace dlog.Workspace, sessionID string) {
	t.Helper()
	file, err := os.Open(path)
	if err != nil {
		t.Errorf("open inherited shim log target: %v", err)
		return
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var record struct {
			Timestamp          string         `json:"timestamp"`
			Runtime            string         `json:"runtime"`
			Level              string         `json:"level"`
			Verbosity          string         `json:"verbosity"`
			Operation          string         `json:"operation"`
			Message            string         `json:"message"`
			Context            map[string]any `json:"context"`
			PID                int            `json:"pid"`
			WorkspaceDirectory string         `json:"workspace_dir"`
			WorkspaceID        string         `json:"workspace_id"`
			AgentReplSessionID string         `json:"agent_repl_session_id"`
		}
		if json.Unmarshal(scanner.Bytes(), &record) == nil && strings.Contains(record.Timestamp, ".") && record.Runtime == "shim" && record.Level != "" && record.Verbosity != "" && record.Operation != "" && record.Message != "" && record.Context != nil && record.PID > 0 && record.WorkspaceDirectory == workspace.Directory && record.WorkspaceID == workspace.ID && record.AgentReplSessionID == sessionID {
			return
		}
	}
	if err := scanner.Err(); err != nil {
		t.Errorf("scan inherited shim log target: %v", err)
		return
	}
	t.Errorf("inherited shim log target %q contained no canonical shim record", path)
}

func newUDSHarness(t *testing.T, options ...harnessOption) *e2eHarness {
	t.Helper()
	var tuning harnessTuning
	for _, opt := range options {
		opt(&tuning)
	}
	// The LAST-RESORT shim reaper, registered before anything else so it runs
	// AFTER every other cleanup (LIFO) — in particular after driver.Close. A
	// per-session cleanup kills the shim it spawned, but the still-live driver
	// RESPAWNS one ~100ms later, and driver.Close then SIGTERMs that respawn
	// WITHOUT waiting — leaving a dying node process racing the t.TempDir
	// RemoveAll, which fails tests from cleanup with "directory not empty".
	// Waiting out every spawned process here closes that race.
	var (
		procMu sync.Mutex
		procs  []*shim.Proc
		// spawnCount counts shim EXECS. A stale-shim refresh is visible as a
		// second exec for one session, and its once-only latch as the absence
		// of a third.
		spawnCount atomic.Int64
	)
	t.Cleanup(func() {
		procMu.Lock()
		defer procMu.Unlock()
		for _, p := range procs {
			_ = p.Terminate()
			_ = p.Wait()
		}
	})
	node := nodePath(t)
	script := buildShim(t, node, tuning.shimBuildSHA)
	storeBin := buildShimStore(t)
	// The store socket cannot live under t.TempDir(): the test-name-derived
	// path exceeds the 104-byte sun_path limit, so bind(2) fails on macOS.
	sockDir, err := os.MkdirTemp("/tmp", "agent-repl-e2e-")
	if err != nil {
		t.Fatalf("make short socket dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(sockDir) })
	storeSock := filepath.Join(sockDir, "store.sock")
	// The daemon's shim socket and the session locks resolve under $HOME
	// (~/.cache/agent-repl/{sock,run}). Point HOME at the short dir so this
	// harness listens on an isolated, sun_path-sized socket rather than the
	// LIVE daemon's, and locks its own sessions rather than the real ones.
	t.Setenv("HOME", sockDir)
	// Production daemon boot creates the shared sock dir (frontend.ServeUDS
	// MkdirAll for daemon-frontend.sock) before any shim spawn; the harness
	// mirrors that guarantee — the shim itself does not mkdir, and node maps
	// a missing parent dir to a fatal EACCES on bind.
	if err := os.MkdirAll(filepath.Join(sockDir, ".cache", "agent-repl", "sock"), 0o700); err != nil {
		t.Fatalf("make session socket dir: %v", err)
	}
	startShimStore(t, storeBin, storeSock)

	// ONE state store, as production opens it: the registry's identity tables
	// and the SSM's state log share a database and a connection.
	stateStore, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	t.Cleanup(func() { _ = stateStore.Close() })
	reg := registry.OpenWith(registry.Options{DB: stateStore, Logf: t.Logf})
	ssmMgr, err := ssm.Open(ssm.Options{
		DB:       stateStore,
		Resolver: server.NewRegistryResolver(reg),
		Logf:     t.Logf,
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = ssmMgr.Close() })

	forwarder := &server.PushForwarder{Logf: t.Logf}
	// Shims dial US: start the listener before anything is brought up.
	shimSock := filepath.Join(sockDir, ".cache", "agent-repl", "sock", "daemon-shim.sock")
	shimListener := shimlisten.New(t.Logf)
	if err := shimListener.Listen(shimSock); err != nil {
		t.Fatalf("listen for shims: %v", err)
	}
	t.Cleanup(func() { _ = shimListener.Close() })
	if err := sessionlock.EnsureDir(); err != nil {
		t.Fatalf("make session lock dir: %v", err)
	}
	targets := dlog.NewTargetManager()
	t.Cleanup(func() { _ = targets.Close() })
	udsSpawn := func(sessionID string, opts server.CreateOpts) (func() error, error) {
		workspace, err := dlog.WorkspaceFromDirectory(opts.CWD)
		if err != nil {
			return nil, err
		}
		shimTarget, err := targets.OpenWorkspaceRuntime(workspace, dlog.RuntimeShim)
		if err != nil {
			return nil, err
		}
		canonicalOpts := opts
		canonicalOpts.CWD = workspace.Directory
		argv := server.ShimUDSArgv(node, script, sessionID, true /*forceFake*/, canonicalOpts, shimSock)
		argv = append(argv, "--log-fd", "3")
		argv = append(argv, "--store-socket", storeSock)
		if tuning.wedgeShim {
			// Spawns cleanly, holds no session lock, and never dials the daemon:
			// the process exists, so nothing upstream of the handshake fails.
			argv = []string{node, "-e", "setInterval(() => {}, 1000)"}
		}
		proc, spawnErr := shim.Spawn(shim.Options{Argv: argv, Dir: workspace.Directory, ExtraFiles: []*os.File{shimTarget}, Logger: testShimLogger{t: t}})
		if spawnErr != nil {
			return nil, spawnErr
		}
		spawnCount.Add(1)
		procMu.Lock()
		procs = append(procs, proc)
		procMu.Unlock()
		go func() {
			for range proc.Events() { //nolint:revive
			}
		}()
		t.Cleanup(func() {
			_ = proc.Terminate()
			_ = proc.Wait()
			if !tuning.wedgeShim {
				assertCanonicalShimLog(t, shimTarget.Name(), workspace, sessionID)
			}
		})
		return func() error { return proc.Terminate() }, nil
	}
	e2eSeqStore := server.NewRegistrySeqStore(reg, t.Logf)
	modelCatalogs := server.NewSessionModelCatalogs()
	registrar := &server.RegistryRegistrar{Reg: reg, Logf: t.Logf, ModelCatalogs: modelCatalogs}
	// ONE progress resolver, shared by the driver (which feeds it interrupts,
	// permission and queue counts) and WireAgentShim (which fans its views out
	// to frontends) — the same single-instance wiring main.go does. Two
	// instances split the brain: the driver's notes push to nobody while the
	// wired instance never hears them.
	progressMgr := progress.New(progress.Options{Logf: t.Logf})
	fileDiagnostics, err := server.NewTargetFileDiagnosticPersister(targets, os.Stderr, false)
	if err != nil {
		t.Fatalf("build file diagnostic persister: %v", err)
	}
	driver, err := sessiondrv.New(sessiondrv.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           server.NewShimSpawner(reg, shimListener.Connected, udsSpawn, t.Logf),
		Locator:           &server.SessionLocator{Reg: reg},
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		SeqStore:          e2eSeqStore,
		ClearCompactStore: e2eSeqStore,
		PermissionModes:   server.NewRegistryModeStore(reg),
		Registrar:         registrar,
		ModelCatalogs:     registrar,
		DaemonVersion:     "0.1.0-e2e",
		ProtocolVersion:   "1",
		ShimBuildSHA:      func() string { return tuning.currentBuildSHA },
		Logf:              t.Logf,
	})
	if err != nil {
		t.Fatalf("build driver: %v", err)
	}
	t.Cleanup(driver.Close)

	binding := &server.SessionCommandBinding{Logf: t.Logf}
	workspaceCreation := newEmptyWorkspaceCreation()
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		// The real resolver over the real registry: an e2e that faked this
		// would not exercise the daemon actually deciding what to resume.
		Resumes:           &server.ConversationResolver{Reg: reg, Logf: t.Logf},
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Prompts:           driver,
		Turns:             driver,
		Health:            driver,
		MergeDirs:         stubMergeDirs{},
		Lifecycle:         stubLifecycle{},
		Resyncer:          driver,
		Catalogs:          driver,
		SessionCommands:   binding,
		WorkspaceCreation: workspaceCreation,
		EstablishTimeout:  tuning.establishTimeout,
		Logf:              t.Logf,
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	forwarder.SetTarget(agentShim.Server)
	t.Cleanup(func() { _ = agentShim.Close() })

	var sweepTicks chan time.Time
	if tuning.idleSweeper {
		sweepTicks = make(chan time.Time)
	}
	srv := server.New(server.Config{
		DaemonVersion:  "0.1.0-e2e",
		Registry:       reg,
		ModelCatalogs:  modelCatalogs,
		Driver:         driver,
		SSM:            ssmMgr,
		Frontend:       agentShim.Server,
		IdleSweepTicks: sweepTicks,
		Logf:           t.Logf,
	})
	binding.SetTarget(srv)
	// Mirror main.go's mux: the session routes plus the UNFILTERED /frontend
	// socket, which is where session-creation commands ride now that
	// POST /sessions is gone.
	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	ts := httptest.NewServer(mux)
	t.Cleanup(ts.Close)
	h := &e2eHarness{ts: ts, shimSpawns: spawnCount.Load}
	if sweepTicks != nil {
		h.sweepIdle = sweepTicks
	}
	return h
}

// createSession brings a session up over the STRICT command path — the same
// one the webapp uses (webapp/src/main.ts createSessionViaWs), since the
// POST /sessions route no longer exists. It dials the unfiltered /frontend
// socket, waits for the connect StateSnapshot so the known-session set is
// populated FIRST (a pre-existing session on the same cwd must not be able to
// masquerade as the new one), sends CreateSessionCmd, and correlates the new
// id off the pushed SessionView whose cwd matches.
//
// A failing CommandAck for the create request is a hard failure, never a
// silent retry: the session genuinely did not come up.
func (h *e2eHarness) createSession(t *testing.T, cwd string) string {
	t.Helper()
	// The DEFECT'S OWN SHAPE: no permission_mode at all, exactly as the
	// workspace-generation inbox path creates one.
	return h.createSessionWithMode(t, cwd, "")
}

// createSessionWithMode is createSession with an explicit permission_mode —
// the Emacs-style create. An empty mode omits the field entirely, which is what
// makes the two callers cover both halves of the posture contract.
func (h *e2eHarness) createSessionWithMode(t *testing.T, cwd, permissionMode string) string {
	t.Helper()
	conn := h.dialFrontend(t)
	defer conn.Close()

	known := map[string]bool{}
	snap := readFrame(t, conn)
	if snap.GetSnapshot() == nil {
		t.Fatalf("first /frontend frame = %T, want a StateSnapshot", snap.GetFrame())
	}
	for _, sv := range snap.GetSnapshot().GetSessions() {
		known[sv.GetSessionId()] = true
	}

	const requestID = "e2e-create-1"
	modeField := ""
	if permissionMode != "" {
		modeField = fmt.Sprintf(`,"permissionMode":%q`, permissionMode)
	}
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"createSession":{"cwd":%q,"fake":true%s}}`, requestID, cwd, modeField))

	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_CommandAck:
			if f.CommandAck.GetRequestId() == requestID && !f.CommandAck.GetOk() {
				t.Fatalf("createSession nacked: %s", f.CommandAck.GetError())
			}
		case *frontendv1.FrontendFrame_SessionView:
			sv := f.SessionView
			if sv.GetCwd() == cwd && !known[sv.GetSessionId()] && sv.GetSessionId() != "" {
				return sv.GetSessionId()
			}
		}
	}
	t.Fatalf("no SessionView for a new session at cwd %s arrived before the deadline", cwd)
	return ""
}

// dialFrontend opens the UNFILTERED /frontend socket (every workspace's
// frames), the surface session-creation commands ride.
func (h *e2eHarness) dialFrontend(t *testing.T) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/frontend"
	conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("dial /frontend: %v", err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	return conn
}

func (h *e2eHarness) dial(t *testing.T, sessionID string) *websocket.Conn {
	t.Helper()
	wsURL := "ws" + strings.TrimPrefix(h.ts.URL, "http") + "/sessions/" + sessionID + "/stream"
	conn, resp, err := websocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("dial stream: %v", err)
	}
	if resp != nil {
		defer resp.Body.Close()
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn
}

// readFrame reads one frontend.v1 protojson FrontendFrame off the socket.
func readFrame(t *testing.T, conn *websocket.Conn) *frontendv1.FrontendFrame {
	t.Helper()
	if err := conn.SetReadDeadline(time.Now().Add(frameTimeout)); err != nil {
		t.Fatalf("deadline: %v", err)
	}
	_, data, err := conn.ReadMessage()
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(data, frame); err != nil {
		t.Fatalf("protojson unmarshal %s: %v", data, err)
	}
	return frame
}

func writeCmd(t *testing.T, conn *websocket.Conn, cmd string) {
	t.Helper()
	if err := conn.WriteMessage(websocket.TextMessage, []byte(cmd)); err != nil {
		t.Fatalf("write: %v", err)
	}
}

// TestE2EUDSTextTurnRendersFrontendFrames drives one text turn through the full
// UDS stack and asserts the daemon renders it as frontend.v1 frames on the
// scoped /stream socket: the connect StateSnapshot first, then (after the
// prompt) at least one ConversationDelta or a WorkspaceState transition for
// this session's workspace.
func TestE2EUDSTextTurnRendersFrontendFrames(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)

	// The scoped connection opens with a StateSnapshot.
	first := readFrame(t, conn)
	if first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}

	// A submit during shim bring-up is honestly nacked (no queue by design),
	// so wait for attach evidence first: the first session-scoped push, which
	// rides the same shim connection the submit needs.
	attachDeadline := time.Now().Add(frameTimeout)
	attached := false
	for !attached && time.Now().Before(attachDeadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_WorkspaceState:
			attached = f.WorkspaceState.GetSessionId() == id || f.WorkspaceState.GetWorkspace() == cwd
		case *frontendv1.FrontendFrame_SessionInit:
			attached = f.SessionInit.GetSessionId() == id
		case *frontendv1.FrontendFrame_SessionView:
			attached = f.SessionView.GetSessionId() == id && f.SessionView.GetShimAttached()
		}
	}
	if !attached {
		t.Fatal("shim never attached (no session-scoped push arrived)")
	}

	// Act — submit the prompt as a FrontendCommand frame (the /stream socket
	// is command-strict since S9; the scoped translator stamps the workspace).
	writeCmd(t, conn, `{"requestId":"r1","submitPrompt":{"text":"hello uds e2e"}}`)

	// Assert — a conversation delta or a workspace-state for this session
	// arrives within the timeout (the fake SDK echoes the prompt back).
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_ConversationDelta:
			if f.ConversationDelta.GetSessionId() == id || f.ConversationDelta.GetWorkspace() == cwd {
				return // success: the turn rendered as frontend.v1
			}
		case *frontendv1.FrontendFrame_WorkspaceState:
			if f.WorkspaceState.GetSessionId() == id || f.WorkspaceState.GetWorkspace() == cwd {
				return // success: the driver drove an SSM transition for this session
			}
		}
	}
	t.Fatal("no ConversationDelta or WorkspaceState for the session arrived before the deadline")
}
