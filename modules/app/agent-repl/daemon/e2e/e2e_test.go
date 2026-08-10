// Package e2e exercises the full post-cutover daemon⇄shim stack end to end:
// a real claude-repld frontend surface + per-session controller spawning the real
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
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"

	"claude-repld/internal/dlog"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/progress"
	"claude-repld/internal/prompts"
	"claude-repld/internal/registry"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shim"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/workspace/geometry"
	"claude-repld/internal/workspace/merge"
)

// frameTimeout is the WAIT BUDGET a test allows for a frame it has already
// caused to be produced. It is not a hang backstop — `go test -timeout` is —
// so it is deliberately short: every await in this suite is gated on a signal
// (a socket read, a readiness record, an injected clock advance), and a wait
// that needs tens of seconds is a test synchronizing by generosity rather than
// by signal.
//
// FIVE SECONDS, AND THE FIGURE IS MEASURED. The awaits this bounds are
// dominated by one real node shim spawn plus a turn through the real
// shim-store, measured at ~0.65s per await, so five is roughly an 8x margin.
// Raising it to ten was tried and bought nothing: the residual full-package
// failures are awaits whose frame the daemon provably PUSHED and an earlier
// await in the same test consumed and discarded, so they burn whatever budget
// they are given. Those tests need the combined-await fix, not a bigger number.
var frameTimeout = 5 * time.Second

// durableReplayIdle is the quiet window used by E2E readers of the in-process
// shim-store. The production reader keeps its multi-second bound, while these
// tests need only distinguish the local store's completed replay from a live
// tail that stays open indefinitely.
const durableReplayIdle = 250 * time.Millisecond

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
	// Its own process group, so the reaper can kill the store AND anything it
	// spawns as one unit rather than leaving grandchildren behind.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	// READINESS IS THE STORE'S OWN STATEMENT, NOT AN ELAPSED INTERVAL. The
	// store logs `listening` to stderr only after server.Listen has bound the
	// socket AND the accept loop owns it, so that record is strictly stronger
	// evidence than the socket file existing — the file appears at bind, with
	// nothing yet serving it. Waiting on the record also collapses the boot
	// failure case: a store that dies during startup reports its exit status
	// here instead of burning the whole timeout on a file that never arrives.
	ready := make(chan struct{})
	cmd.Stderr = &readyWriter{
		inner:  &testLogWriter{t: t, tag: "shim-store"},
		marker: []byte("listening"),
		ready:  ready,
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start shim-store: %v", err)
	}
	// Registered BEFORE anything below can call t.Fatalf: a readiness failure
	// unwinds through t.Cleanup, but a SIGPIPE while waiting for readiness does
	// not, and that window is long enough to matter (5s).
	registerChild(cmd.Process.Pid)
	// cmd.Wait (not Process.Wait) is what drains the stderr copier. childExit
	// retains one immutable outcome behind a closed-channel latch, so readiness
	// and cleanup can both observe an early exit without consuming each other's
	// only copy and deadlocking the test teardown.
	exited := observeChildExit(cmd.Wait)
	t.Cleanup(func() {
		// Kill the GROUP, matching what the reaper would have done, then drop
		// the registration so the end-of-run sweep cannot signal a recycled pid.
		_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		_ = exited.wait()
		unregisterChild(cmd.Process.Pid)
	})
	select {
	case <-ready:
	case <-exited.done:
		t.Fatalf("shim-store exited before it began listening on %s: %v", sock, exited.err)
	case <-time.After(frameTimeout):
		t.Fatalf("shim-store never reported listening on %s", sock)
	}
}

// childExit is a replayable process-exit outcome. Closing done publishes err
// to every observer; unlike receiving an error value from a one-shot channel,
// one observer cannot consume the fact needed by another observer.
type childExit struct {
	done chan struct{}
	err  error
}

func observeChildExit(wait func() error) *childExit {
	exit := &childExit{done: make(chan struct{})}
	go func() {
		exit.err = wait()
		close(exit.done)
	}()
	return exit
}

func (e *childExit) wait() error {
	<-e.done
	return e.err
}

func TestChildExitOutcomeIsReplayableAcrossReadinessAndCleanup(t *testing.T) {
	want := errors.New("child exited before readiness")
	release := make(chan struct{})
	exited := observeChildExit(func() error {
		<-release
		return want
	})
	close(release)

	// The readiness observer sees the closure first. Cleanup must receive the
	// same immutable outcome instead of blocking on an already-consumed value.
	<-exited.done
	if got := exited.wait(); !errors.Is(got, want) {
		t.Fatalf("cleanup exit outcome = %v, want %v", got, want)
	}
}

// readyWriter forwards a child process's output to inner and closes ready the
// first time the accumulated bytes contain marker. It is how the harness waits
// on a process's OWN announcement that it is up rather than on a poll interval.
//
// Accumulation (rather than matching per Write) tolerates a record split across
// writes; it stops the moment the marker lands, so the buffer cannot grow with
// the process's lifetime.
type readyWriter struct {
	inner  io.Writer
	marker []byte
	ready  chan struct{}

	mu   sync.Mutex
	seen []byte
	done bool
}

func (w *readyWriter) Write(p []byte) (int, error) {
	n, err := w.inner.Write(p)
	w.mu.Lock()
	if !w.done {
		w.seen = append(w.seen, p...)
		if bytes.Contains(w.seen, w.marker) {
			w.done, w.seen = true, nil
			close(w.ready)
		}
	}
	w.mu.Unlock()
	return n, err
}

// isolatedShimSocket claims a per-test shim socket under sockDir and makes it
// THE socket for everything this test stands up, by setting the same
// environment override the production daemon resolves through
// (shimlisten.SocketEnvVar).
//
// WHY THIS IS NOT OPTIONAL. shimlisten.DefaultSocketPath resolves
// ~/.cache/agent-repl/sock/daemon-shim.sock, which on a developer machine is
// the socket the OPERATOR'S LIVE DAEMON is listening on. Server.Listen removes
// a "stale" file before binding, because on the real path a leftover socket IS
// stale — so an e2e that resolved the real path would unlink the live daemon's
// socket, bind its own, and take delivery of every shim that dialed
// afterwards. That is a production incident caused by running a test, and it
// has happened.
//
// Moving $HOME used to be the whole isolation. It is not a stated claim on the
// socket, only a side effect of relocating eight unrelated paths, so a harness
// that set the sockets up in a different order — or a code path that resolved
// the socket before $HOME moved — silently bound the real one. Every harness
// now derives its socket HERE, through DefaultSocketPath, so the test and the
// daemon it builds cannot disagree about the path and neither can name the
// real one.
func isolatedShimSocket(t *testing.T, sockDir string) string {
	t.Helper()
	// Every harness in this package routes through here, so this is the one
	// place that can ask whether the caller actually moved off the operator's
	// live state BEFORE a socket gets derived from it — and before MkdirAll
	// below creates the directory. mergegeometry's harness did not, and handed
	// the real $HOME in; requireIsolatedHome turns that omission into a failure
	// at the offending line rather than an unlinked live socket.
	requireIsolatedHome(t, sockDir)
	// Same reasoning applies to the prompts directory: it is state the daemon
	// under test resolves for itself, and this funnel is the only place that
	// can arrange it once for every harness.
	usePrompts(t)
	sock := filepath.Join(sockDir, ".cache", "agent-repl", "sock", "daemon-shim.sock")
	// Production daemon boot creates the shared sock dir (frontend.ServeUDS
	// MkdirAll for daemon-frontend.sock) before any shim spawn; the harness
	// mirrors that guarantee — the shim itself does not mkdir, and node maps a
	// missing parent dir to a fatal EACCES on bind.
	if err := os.MkdirAll(filepath.Dir(sock), 0o700); err != nil {
		t.Fatalf("make isolated shim socket dir: %v", err)
	}
	t.Setenv(shimlisten.SocketEnvVar, sock)
	// Resolve through the PRODUCTION function rather than trusting the join: if
	// the override ever stopped being honored, the test finds out HERE, before
	// it binds, rather than after it has taken the live daemon's socket.
	resolved, err := shimlisten.DefaultSocketPath()
	if err != nil {
		t.Fatalf("resolve the isolated shim socket: %v", err)
	}
	if resolved != sock {
		t.Fatalf("shimlisten resolved %q while this test isolated %q: the daemon under test would bind a socket the test does not own", resolved, sock)
	}
	return resolved
}

// usePrompts points the prompt loader at this checkout's real prompts/
// directory for the duration of one test.
//
// prompts.Dir walks up from the running executable to the checkout holding
// bin/deploy-all.sh. This test binary lives in the go build cache, so there is
// no checkout above it and every prompt the daemon composes — merge-lease
// resolution turns, most visibly — fails to load. Resolving from the prompts
// package's own source and going through the ordinary DirEnv override means
// the e2e stack reads the SAME files a deployed daemon would, never a copy
// that could drift from them.
func usePrompts(t *testing.T) {
	t.Helper()
	dir, err := prompts.SourceDir()
	if err != nil {
		t.Fatalf("resolve the checkout's prompts directory: %v", err)
	}
	t.Setenv(prompts.DirEnv, dir)
}

type testLogWriter struct {
	t   *testing.T
	tag string
}

func (w *testLogWriter) Write(p []byte) (int, error) {
	w.t.Logf("[%s] %s", w.tag, bytes.TrimRight(p, "\n"))
	return len(p), nil
}

// e2eSpawnLog forwards the shared spawn procedure's records to the test log,
// and STOPS doing so once the test is finished: the reaper goroutine can record
// an exit after the test function has returned, and t.Logf then panics.
type e2eSpawnLog struct {
	mu   sync.Mutex
	t    *testing.T
	done bool
}

func (l *e2eSpawnLog) event(_ dlog.Level, workspace dlog.Workspace, sessionID, message string, context map[string]any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.done {
		return
	}
	l.t.Logf("e2e shim spawn: %s ws=%s session=%s %v", message, workspace.Directory, sessionID, context)
}

func (l *e2eSpawnLog) finish() {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.done = true
}

type testShimLogger struct{ t *testing.T }

func (l testShimLogger) Log(format string, args ...any) { l.t.Logf(format, args...) }

func (l testShimLogger) LogVerbose(format string, args ...any) { l.t.Logf(format, args...) }

func (l testShimLogger) LogLifecycle(format string, args ...any) { l.t.Logf(format, args...) }

// --- minimal WireAgentShim stubs (lifecycle unused here) --------------------

type stubLifecycle struct{}

func (stubLifecycle) Close(context.Context, string) error                          { return nil }
func (stubLifecycle) Open(context.Context, string, server.WorkspaceOpenOpts) error { return nil }

// OpenDriveable succeeds for the same reason Open does: the harnesses that bind
// this stub run no merge ACTION, so nothing is ever sent to the session this
// claims is driveable. The merge acceptance harness (newUDSHarness) binds the
// production server.WorkspaceOpener instead, precisely so the bring-up decision
// is exercised for real rather than stubbed past.
func (stubLifecycle) OpenDriveable(context.Context, string) error { return nil }
func (stubLifecycle) OpenForMerge(context.Context, string) error  { return nil }

// stubMergeLease is the merge.Lease for E2E flows that never drive a merge.
// The real one lives in internal/ssm and interrupts the workspace's turn; a
// release func is still returned because a nil release is a hard panic in
// merge.Coordinator, by design.
type stubMergeLease struct{}

func (stubMergeLease) Acquire(context.Context, string) (func(), error) { return func() {}, nil }
func (stubMergeLease) Held(string) bool                                { return false }

// newTestMergeQueue roots the durable merge queue in a scratch directory, so
// the harness never publishes into the operator's real state root.
func newTestMergeQueue(t *testing.T) merge.DurableQueue {
	t.Helper()
	q, err := merge.NewFileQueue(filepath.Join(t.TempDir(), "merge-queue"), t.Logf)
	if err != nil {
		t.Fatalf("merge queue: %v", err)
	}
	return q
}

// emptyWorkspaceCreation is the explicit creation seam for E2E flows that do
// not exercise workspace creation. It is deliberately strict: a create,
// materialization acknowledgement, or host-action completion received by this
// harness fails loudly rather than pretending the daemon performed it. The
// non-nil typed subscriptions preserve WireAgentShim's production invariant
// without touching disk or another process.
type emptyWorkspaceCreation struct {
	available          chan *frontendv1.WorkspaceAvailable
	actions            chan *frontendv1.HostAction
	publicationRelease chan server.SessionPublicationRelease
	closeOnce          sync.Once
	// beforeWSMerge / postprocessing are the CREATION-TIME merge actions this
	// harness reports for every workspace, empty by default.
	//
	// They live on the creation stub because that is where the real answer
	// comes from: both are fields of the create Request (before_ws_merge,
	// postprocessing_prompt), recorded when the workspace was created and read
	// back at merge time keyed by worktree path. A harness that injected them
	// anywhere else would be testing a seam production does not have.
	//
	// They are not per-workspace: every e2e that configures an action has
	// exactly one workspace merging, so a map would be ceremony around a single
	// value.
	beforeWSMerge  string
	postprocessing string
}

func newEmptyWorkspaceCreation() *emptyWorkspaceCreation {
	return &emptyWorkspaceCreation{
		available:          make(chan *frontendv1.WorkspaceAvailable),
		actions:            make(chan *frontendv1.HostAction),
		publicationRelease: make(chan server.SessionPublicationRelease),
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

func (*emptyWorkspaceCreation) SessionPublicationDecision(worktreePath, sessionID string) (server.SessionPublicationDecision, error) {
	return server.SessionPublicationDecision{WorktreePath: worktreePath, SessionID: sessionID, Materialized: true}, nil
}

func (e *emptyWorkspaceCreation) SubscribeSessionPublicationReleases() (<-chan server.SessionPublicationRelease, func()) {
	return e.publicationRelease, e.close
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
		close(e.publicationRelease)
	})
}

type e2eHarness struct {
	ts *httptest.Server
	// stateDB is the daemon's durable state database. Acceptance tests inspect
	// records through their production statedb owners after daemon persistence.
	stateDB *sql.DB
	// geometry is THE daemon's workspace -> merge-geometry map, exposed so a
	// merge test can record what the daemon would have recorded when it created
	// the worktree. MergeWorkspaceCmd no longer carries geometry (the fields are
	// reserved in frontend.proto), so this record is the only way a fixture
	// repository the daemon never created becomes mergeable.
	geometry *geometry.Store
	// merges is THE daemon's own merge ingress route — the same
	// *server.MergeDispatchBinding production binds into the workspace-command
	// inbox, bound to the same *server.MergeDispatch WireAgentShim built here.
	// A test that stands up its own create.Inbox against this harness hands it
	// this value, so a dispatch-JSON merge takes the production path (including
	// the rejection-sentinel translation the ingress quarantines on) rather than
	// a harness-local imitation of it.
	merges *server.MergeDispatchBinding
	// shimSpawns reports how many shim processes this harness has exec'd.
	shimSpawns func() int64
	// sweepIdle fires the daemon's idle sweeper, which is the production
	// hibernation trigger. Non-nil only for a harness built with
	// withIdleSweeper; a test that never asks for one cannot accidentally
	// hibernate its own session.
	sweepIdle chan<- time.Time
	// vendors announces every vendor-uuid write the daemon makes, so a test
	// waiting on an asynchronous conversation rotation waits on the WRITE
	// rather than re-reading /sessions on an interval.
	vendors *vendorIdentities
	// clock is the daemon's own wall clock, movable by the test. Non-nil only
	// for a harness built with withTestClock.
	//
	// It exists because every TIME-SINCE policy in the daemon — the idle
	// sweeper's elapsed-idle gate, and the cache keep-alive window measured
	// from the durable last turn end — is a comparison against server.Config's
	// injected Now. A test that wanted to observe one of those edges without
	// this would have to wait out the real interval, which for a one-hour cache
	// TTL is not a test at all. Moving the clock and then firing sweepIdle
	// makes the edge an EVENT the test causes.
	clock *testClock
	// pingGate is the path a gated fake turn parks on, for a harness built
	// with withGatedKeepAlivePing. Creating it is how a test ENDS the keep-alive
	// ping it arranged to still be running.
	pingGate string
}

// releasePingGate lets a parked keep-alive ping finish its turn the ordinary
// way. It is the release half of withGatedKeepAlivePing.
func (h *e2eHarness) releasePingGate(t *testing.T) {
	t.Helper()
	if h.pingGate == "" {
		t.Fatal("this harness has no keep-alive ping gate: build it with withGatedKeepAlivePing")
	}
	if err := os.WriteFile(h.pingGate, []byte("release"), 0o600); err != nil {
		t.Fatalf("release the keep-alive ping gate: %v", err)
	}
}

// testClock is the daemon's wall clock under test control: real time, plus an
// offset the test moves forward.
//
// It is an OFFSET rather than a frozen instant deliberately. Everything else
// in the stack (the shim, the store, the SSM's own timestamps) keeps running on
// the real clock, so a frozen daemon clock would put the daemon permanently
// behind facts the rest of the world is still stamping. An offset keeps the two
// in step and adds exactly the elapsed time the test wants to claim.
type testClock struct{ offset atomic.Int64 }

func newTestClock() *testClock { return &testClock{} }

// now is the func handed to server.Config.Now.
func (c *testClock) now() time.Time { return time.Now().Add(time.Duration(c.offset.Load())) }

// advance moves the daemon's clock forward by d. Every subsequent time-since
// measurement the daemon makes reads d longer.
func (c *testClock) advance(d time.Duration) { c.offset.Add(int64(d)) }

// vendorIdentities publishes the daemon's vendor-uuid writes as events.
//
// It decorates the production *server.RegistryRegistrar, which is the single
// seam the session controller adopts (AdoptVendorSessionID) and re-adopts
// (ClaudeSessionIDChanged) a conversation's uuid through — so every write that
// can move the value /sessions reports passes here first, already durable by
// the time the announcement goes out.
//
// The broadcast is a channel closed and replaced per write rather than a
// buffered signal, so any number of waiters wake on any write and none can
// consume another's wakeup.
type vendorIdentities struct {
	*server.RegistryRegistrar

	mu      sync.Mutex
	changed chan struct{}
}

func newVendorIdentities(inner *server.RegistryRegistrar) *vendorIdentities {
	return &vendorIdentities{RegistryRegistrar: inner, changed: make(chan struct{})}
}

// pending returns the channel that closes on the NEXT vendor-uuid write. A
// waiter takes it BEFORE reading the value it is waiting on, so a write landing
// between that read and the wait is not missed.
func (v *vendorIdentities) pending() <-chan struct{} {
	v.mu.Lock()
	defer v.mu.Unlock()
	return v.changed
}

func (v *vendorIdentities) announce() {
	v.mu.Lock()
	defer v.mu.Unlock()
	close(v.changed)
	v.changed = make(chan struct{})
}

func (v *vendorIdentities) ClaudeSessionIDChanged(sessionID, claudeSessionID string) bool {
	adopted := v.RegistryRegistrar.ClaudeSessionIDChanged(sessionID, claudeSessionID)
	if adopted {
		v.announce()
	}
	return adopted
}

func (v *vendorIdentities) AdoptVendorSessionID(sessionID, claudeSessionID string) (bool, string, bool) {
	rotated, previous, adopted := v.RegistryRegistrar.AdoptVendorSessionID(sessionID, claudeSessionID)
	if adopted {
		v.announce()
	}
	return rotated, previous, adopted
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
	// beforeWSMerge / postprocessing are the creation-time merge actions the
	// harness's workspace-creation stub reports. Empty is "not configured".
	beforeWSMerge  string
	postprocessing string
	// idleSweeper hands the daemon's idle sweeper a clock the TEST drives, so
	// a hibernation is provoked by an event rather than waited out. It is the
	// production trigger — sweepIdle calls controller.Hibernate — so what it
	// exercises is the real edge and not a stand-in for one.
	idleSweeper bool
	// clock gives the daemon a test-movable wall clock (server.Config.Now).
	clock bool
	// idleTimeout is the sweeper's elapsed-idle gate (server.Config.IdleTimeout).
	// Zero is the harness default and means "every sweep hibernates", which is
	// what the connectivity-state tests want; a test of a policy that must run
	// BEFORE hibernation sets a real cutoff so the sweep has room to do
	// something else first.
	idleTimeout time.Duration
	// gatedPing parks the fake's keep-alive turn until the test releases it, so
	// "a prompt arrives while the ping is STILL RUNNING" is arranged rather
	// than raced for. See withGatedKeepAlivePing.
	gatedPing bool
}

type harnessOption func(*harnessTuning)

func withWedgedShim() harnessOption { return func(o *harnessTuning) { o.wedgeShim = true } }

// withStaleShimBuild bakes `baked` into the bundle while telling the daemon the
// current bundle is `current` — a shim that outlived a deploy.
func withStaleShimBuild(baked, current string) harnessOption {
	return func(o *harnessTuning) { o.shimBuildSHA, o.currentBuildSHA = baked, current }
}

// withGatedKeepAlivePing parks every keep-alive ping the fake runs until
// h.releasePingGate is called.
//
// THE WINDOW IS THE POINT. A prompt is held only while a ping's turn is in
// flight, and the fake answers a ping in microseconds — so a test that submits
// its prompt "right after" the ping is racing, not arranging. The gate turns
// that window into one the test opens and closes.
func withGatedKeepAlivePing() harnessOption { return func(o *harnessTuning) { o.gatedPing = true } }

// withIdleSweeper gives the harness a test-driven idle-sweep clock.
func withIdleSweeper() harnessOption { return func(o *harnessTuning) { o.idleSweeper = true } }

// withTestClock hands the daemon a wall clock the test moves (h.clock).
func withTestClock() harnessOption { return func(o *harnessTuning) { o.clock = true } }

// withIdleCutoff sets the idle sweeper's elapsed-idle gate, so a sweep fired
// before the workspace has been quiet that long does NOT hibernate it.
func withIdleCutoff(d time.Duration) harnessOption {
	return func(o *harnessTuning) { o.idleTimeout = d }
}

// withMergeActions configures the creation-time merge actions every workspace
// of this harness reports: the before_ws_merge prompt that runs before any
// commit is picked, and the postprocessing prompt that runs once they all
// have. An empty string means "not configured", which is the state the spec's
// "iff configured" phases turn on.
func withMergeActions(before, after string) harnessOption {
	return func(o *harnessTuning) { o.beforeWSMerge, o.postprocessing = before, after }
}

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

// fakeShimSpawner builds the spawner every FAKE harness drives: one that knows
// its shims run the scripted offline SDK. Without that, a respawn — a bounce, a
// merge bring-up, anything after a hibernation — is refused by the resume
// viability gate over a vendor transcript the scripted SDK never writes, while
// the create that started the session waived exactly that check.
func fakeShimSpawner(
	reg *registry.Registry,
	listener *shimlisten.Server,
	spawn server.ShimSpawnFunc,
	logf func(string, ...any),
) *server.ShimSpawner {
	sp := server.NewShimSpawner(reg, listener.Connected, listener.Evict, spawn, logf)
	sp.ForceFake(true)
	return sp
}

func newUDSHarness(t *testing.T, options ...harnessOption) *e2eHarness {
	t.Helper()
	var tuning harnessTuning
	for _, opt := range options {
		opt(&tuning)
	}
	// The LAST-RESORT shim reaper, registered before anything else so it runs
	// AFTER every other cleanup (LIFO) — in particular after controller.Close. A
	// per-session cleanup kills the shim it spawned, but the still-live session controller
	// RESPAWNS one ~100ms later, and controller.Close then SIGTERMs that respawn
	// WITHOUT waiting — leaving a dying node process racing the t.TempDir
	// RemoveAll, which fails tests from cleanup with "directory not empty".
	// Waiting out every spawned process here closes that race.
	var (
		procMu sync.Mutex
		procs  []*trackedShim
		// spawnCount counts shim EXECS. A stale-shim refresh is visible as a
		// second exec for one session, and its once-only latch as the absence
		// of a third.
		spawnCount atomic.Int64
	)
	// ONE REAPER PER PROCESS lives in the shared spawn procedure
	// (server.NewUDSSpawner), so this waits on the exit it publishes rather
	// than calling Wait a second time — cmd.Wait is not re-entrant.
	t.Cleanup(func() {
		procMu.Lock()
		defer procMu.Unlock()
		for _, p := range procs {
			_ = p.proc.Terminate(shim.Stop{Initiator: "e2e_harness_cleanup", Reason: "test teardown reaps every spawned shim"})
			<-p.exited
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
	// The session locks resolve under $HOME (~/.cache/agent-repl/run). Point
	// HOME at the short, sun_path-sized dir so this harness locks its own
	// sessions rather than the real ones. The SHIM SOCKET is isolated
	// separately and explicitly, because riding on $HOME made it an unstated
	// claim — see isolatedShimSocket.
	t.Setenv("HOME", sockDir)
	pingGate := ""
	if tuning.gatedPing {
		pingGate = filepath.Join(sockDir, "keep-alive-ping.gate")
		t.Setenv("AGENT_REPL_FAKE_TURN_GATE", pingGate)
		t.Setenv("AGENT_REPL_FAKE_TURN_GATE_TEXT", keepAlivePingText)
	}
	shimSock := isolatedShimSocket(t, sockDir)
	startShimStore(t, storeBin, storeSock)

	// ONE state store, as production opens it: the registry's identity tables
	// and the SSM's state log share a database and a connection.
	stateStore, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("open state store: %v", err)
	}
	t.Cleanup(func() { _ = stateStore.Close() })
	keepAliveWindows, err := statedb.NewKeepAliveWindows(stateStore)
	if err != nil {
		t.Fatalf("open keep-alive window store: %v", err)
	}
	turnAccountings, err := statedb.NewTurnAccountings(stateStore)
	if err != nil {
		t.Fatalf("open turn accounting store: %v", err)
	}
	tokenUtilizations, err := statedb.NewTokenUtilizations(stateStore)
	if err != nil {
		t.Fatalf("open token utilization store: %v", err)
	}
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
	// THE SAME SPAWN PROCEDURE THE DAEMON RUNS (server.NewUDSSpawner): a harness
	// that spawned shims its own way would prove things about the harness. Only
	// the genuinely per-caller parts are supplied here.
	spawnLog := &e2eSpawnLog{t: t}
	t.Cleanup(spawnLog.finish)
	udsSpawn, err := server.NewUDSSpawner(server.UDSSpawnConfig{
		Targets:    targets,
		Node:       node,
		Script:     script,
		ShimSocket: shimSock,
		ForceFake:  true,
		ExtraArgv:  []string{"--store-socket", storeSock},
		ArgvOverride: func(_ string, argv []string) []string {
			if !tuning.wedgeShim {
				return argv
			}
			// Spawns cleanly, holds no session lock, and never dials the daemon:
			// the process exists, so nothing upstream of the handshake fails.
			return []string{node, "-e", "setInterval(() => {}, 1000)"}
		},
		Logger: func(dlog.Workspace, string) shim.Logger { return testShimLogger{t: t} },
		Event:  spawnLog.event,
		Spawned: func(s server.SpawnedShim) {
			spawnCount.Add(1)
			tracked := &trackedShim{workspace: s.Workspace.Directory, proc: s.Proc, exited: make(chan struct{})}
			procMu.Lock()
			procs = append(procs, tracked)
			procMu.Unlock()
			t.Cleanup(func() {
				_ = s.Proc.Terminate(shim.Stop{Initiator: "e2e_per_session_cleanup", Reason: "test teardown stops the session shim"})
				<-tracked.exited
				if !tuning.wedgeShim {
					assertCanonicalShimLog(t, s.LogTarget.Name(), s.Workspace, s.SessionID)
				}
			})
		},
		// Published AFTER the exit is recorded, so a waiter this releases cannot
		// outrun the record.
		Reaped: func(s server.SpawnedShim, _ error) {
			procMu.Lock()
			defer procMu.Unlock()
			for _, tracked := range procs {
				if tracked.proc == s.Proc {
					close(tracked.exited)
					return
				}
			}
		},
	})
	if err != nil {
		t.Fatalf("build the UDS spawner: %v", err)
	}
	e2eSeqStore := server.NewRegistrySeqStore(reg, t.Logf)
	modelCatalogs := server.NewSessionModelCatalogs()
	registrar := &server.RegistryRegistrar{Reg: reg, Logf: t.Logf, ModelCatalogs: modelCatalogs}
	// The controller drives the registrar through the DECORATED value, which is
	// what makes vendor-uuid writes observable as events. It forwards every
	// call to the production registrar above, so the behaviour under test is
	// unchanged — only the harness gains a notification.
	vendors := newVendorIdentities(registrar)
	// ONE progress resolver, shared by the session controller (which feeds it interrupts,
	// permission and queue counts) and WireAgentShim (which fans its views out
	// to frontends) — the same single-instance wiring main.go does. Two
	// instances split the brain: the session controller's notes push to nobody while the
	// wired instance never hears them.
	kaCfg, err := keepalive.FromEnv()
	if err != nil {
		t.Fatalf("keep-alive config: %v", err)
	}
	progressMgr := progress.New(progress.Options{Logf: t.Logf, UncachedAlertTokens: kaCfg.UncachedCostAlertTokens})
	fileDiagnostics, err := server.NewTargetFileDiagnosticPersister(targets, os.Stderr, false)
	if err != nil {
		t.Fatalf("build file diagnostic persister: %v", err)
	}
	// THE HARNESS'S ONE CLOCK, hoisted above the controller because the gate
	// that re-validates an idle sweep's decision lives in the session
	// controller and the sweep itself reads server.Config.Now. Mirrors the
	// daemon's own single-authority wiring (cmd/claude-repld/main.go).
	var clock *testClock
	var nowFn func() time.Time
	var nowMsFn func() int64
	if tuning.clock {
		clock = newTestClock()
		nowFn = clock.now
		nowMsFn = func() int64 { return clock.now().UnixMilli() }
	}
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Now:               nowMsFn,
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           fakeShimSpawner(reg, shimListener, udsSpawn, t.Logf),
		Locator:           &server.SessionLocator{Reg: reg},
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		SeqStore:          e2eSeqStore,
		ClearCompactStore: e2eSeqStore,
		TurnAccountings:   turnAccountings,
		HistoricalUsage:   tokenUtilizations,
		PermissionModes:   server.NewRegistryModeStore(reg),
		Registrar:         vendors,
		ModelCatalogs:     registrar,
		Hibernations:      registrar,
		VendorSessions:    vendors,
		KeepAliveWindows:  server.KeepAliveWindowStore{Windows: keepAliveWindows},
		KeepAlive:         kaCfg,
		VendorSessionOf: func(sessionID string) (string, bool) {
			rec, ok := reg.Get(sessionID)
			if !ok || rec.ClaudeSessionID == "" {
				return "", false
			}
			return rec.ClaudeSessionID, true
		},
		DaemonVersion:   "0.1.0-e2e",
		ProtocolVersion: "1",
		ShimBuildSHA:    func() string { return tuning.currentBuildSHA },
		Logf:            t.Logf,
	})
	if err != nil {
		t.Fatalf("build controller: %v", err)
	}
	t.Cleanup(controller.Close)

	binding := &server.SessionCommandBinding{Logf: t.Logf}
	workspaceCreation := newEmptyWorkspaceCreation()
	workspaceCreation.beforeWSMerge = tuning.beforeWSMerge
	workspaceCreation.postprocessing = tuning.postprocessing
	// The REAL merge.Lease over the real SSM and the real durable queue: an
	// e2e that stubbed them would never exercise merge_lease_held or the queue
	// facts the pushed WorkspaceState carries.
	mergeQueue := newTestMergeQueue(t)
	mergeLease, err := ssm.NewMergeLease(ssm.MergeLeaseConfig{
		Manager: ssmMgr, Queue: mergeQueue, Interrupter: controller,
	})
	if err != nil {
		t.Fatalf("build merge lease: %v", err)
	}
	geometryStore, err := geometry.Open(stateStore, t.Logf)
	if err != nil {
		t.Fatalf("open geometry: %v", err)
	}
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		// The boot orphan sweep scans THIS directory and no other: a test that
		// let it resolve the process temp dir deleted the live daemon's rebase
		// worktrees mid-merge.
		RebaseRoot: t.TempDir(),
		// The real resolver over the real registry: an e2e that faked this
		// would not exercise the daemon actually deciding what to resume.
		Resumes:      &server.ConversationResolver{Reg: reg, Logf: t.Logf},
		SSM:          ssmMgr,
		Progress:     progressMgr,
		Prompts:      controller,
		Turns:        controller,
		Health:       controller,
		Hibernations: controller,
		Restarts:     controller,
		// THE PRODUCTION LIFECYCLE, not a stub. A merge brings its workspace's
		// session up through this seam, and a no-op Open would report a
		// hibernated workspace live without ever rehydrating it — so the merge
		// would go on to submit its action's turn into a session that is not
		// there. The bring-up must be the same one an open_workspace command
		// performs, which is exactly what WorkspaceOpener is.
		Lifecycle:         &server.WorkspaceOpener{Reg: reg, Ensurer: controller, Failures: controller, Logf: t.Logf},
		SessionDeaths:     server.RegistrySessionDeaths{Reg: reg},
		Sessions:          server.RegistrySessions{Reg: reg, Controller: controller, ModelCatalogs: modelCatalogs, TokenUsage: tokenUtilizations, Logf: t.Logf},
		Inits:             controller,
		Resyncer:          controller,
		Queues:            controller,
		Catalogs:          controller,
		SessionCommands:   binding,
		WorkspaceCreation: workspaceCreation,
		EstablishTimeout:  tuning.establishTimeout,
		MergeLease:        mergeLease,
		MergeQueue:        mergeQueue,
		MergeGeometry:     geometryStore,
		Logf:              t.Logf,
		LogVerbosef:       t.Logf,
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	forwarder.SetTarget(agentShim.Server)
	// Mirror main.go's late bind of the inbox's merge route (main.go
	// `workspaceAssembly.Merges.SetTarget(agentShim.MergeDispatch)`). The
	// harness wires no inbox of its own, but a test that builds one must be
	// able to reach the merge surface the same way the daemon does.
	mergeBinding := &server.MergeDispatchBinding{Logf: t.Logf}
	mergeBinding.SetTarget(agentShim.MergeDispatch)
	t.Cleanup(func() { _ = agentShim.Close() })

	var sweepTicks chan time.Time
	if tuning.idleSweeper {
		sweepTicks = make(chan time.Time)
	}
	srv := server.New(server.Config{
		DaemonVersion:  "0.1.0-e2e",
		Registry:       reg,
		ModelCatalogs:  modelCatalogs,
		TokenUsage:     tokenUtilizations,
		Controller:     controller,
		AgentShim:      agentShim,
		IdleSweepTicks: sweepTicks,
		IdleTimeout:    tuning.idleTimeout,
		KeepAlive:      kaCfg,
		Now:            nowFn,
		Logf:           t.Logf,
	})
	// Server shutdown joins its idle sweeper before the harness tears down the
	// controller, SSM, and shared state database below. Registering this after
	// those owners makes it run first under testing.T's LIFO cleanup contract.
	t.Cleanup(func() { srv.ShutdownAll(false, sessioncontroller.StopCauseDaemonShutdown()) })
	binding.SetTarget(srv)
	// Mirror main.go's late bind: without it the registrar's post-change
	// SessionView repush (hibernation, vendor rotation) is a silent no-op —
	// the hook is deliberately nil-safe, so nothing else would say so.
	registrar.PushView = srv.RepushSessionView
	// Mirror main.go's mux: the session routes plus the UNFILTERED /frontend
	// socket, which is where session-creation commands ride now that
	// POST /sessions is gone.
	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	ts := httptest.NewServer(mux)
	t.Cleanup(ts.Close)
	h := &e2eHarness{ts: ts, stateDB: stateStore, geometry: geometryStore, merges: mergeBinding, shimSpawns: spawnCount.Load, vendors: vendors, clock: clock, pingGate: pingGate}
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
// id off the pushed SessionView whose cwd matches, then waits for the
// successful correlated CommandAck. That acknowledgement is the create
// establishment boundary for any subsequent StateSnapshot.
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
	var sessionID string
	acknowledged := false
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_CommandAck:
			if f.CommandAck.GetRequestId() == requestID {
				if !f.CommandAck.GetOk() {
					t.Fatalf("createSession nacked: %s", f.CommandAck.GetError())
				}
				acknowledged = true
				if sessionID != "" {
					return sessionID
				}
			}
		case *frontendv1.FrontendFrame_SessionView:
			sv := f.SessionView
			if sv.GetCwd() == cwd && !known[sv.GetSessionId()] && sv.GetSessionId() != "" {
				sessionID = sv.GetSessionId()
				if acknowledged {
					return sessionID
				}
			}
		}
	}
	t.Fatalf("createSession did not deliver both SessionView and successful CommandAck for cwd %s before the deadline", cwd)
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
	return readFrameWithin(t, conn, frameTimeout)
}

// readFrameWithin is readFrame under an explicit budget, for the few awaits
// that are bounded by something other than the suite's frame budget. Every
// caller must name the bound it is waiting on; "be generous" is not a bound.
func readFrameWithin(t *testing.T, conn *websocket.Conn, within time.Duration) *frontendv1.FrontendFrame {
	t.Helper()
	if err := conn.SetReadDeadline(time.Now().Add(within)); err != nil {
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

// tryReadFrameWithin is readFrameWithin that HANDS BACK the read error instead
// of failing the test on it. It exists so an await that runs out of time can
// name the frames it was still waiting for: a bare "read: i/o timeout" says
// only that the socket went quiet, which is the least useful half of the fact.
func tryReadFrameWithin(t *testing.T, conn *websocket.Conn, within time.Duration) (*frontendv1.FrontendFrame, error) {
	t.Helper()
	if err := conn.SetReadDeadline(time.Now().Add(within)); err != nil {
		t.Fatalf("deadline: %v", err)
	}
	_, data, err := conn.ReadMessage()
	if err != nil {
		return nil, err
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(data, frame); err != nil {
		t.Fatalf("protojson unmarshal %s: %v", data, err)
	}
	return frame, nil
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
	snapshot := first.GetSnapshot()
	if snapshot == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}

	// A submit during shim bring-up is honestly nacked (no queue by design).
	// Establishment can finish before this scoped client connects, in which
	// case the authoritative snapshot already proves attachment; otherwise the
	// first session-scoped push carries the same proof.
	attachDeadline := time.Now().Add(frameTimeout)
	attached := false
	for _, view := range snapshot.GetSessions() {
		if view.GetSessionId() == id && view.GetShimAttached() {
			attached = true
			break
		}
	}
	for !attached && time.Now().Before(attachDeadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_WorkspaceState:
			attached = f.WorkspaceState.GetSessionId() == id || f.WorkspaceState.GetWorkspace() == cwd
		case *frontendv1.FrontendFrame_SessionInit:
			attached = f.SessionInit.GetFence() == id
		case *frontendv1.FrontendFrame_SessionView:
			attached = f.SessionView.GetSessionId() == id && f.SessionView.GetShimAttached()
		}
	}
	if !attached {
		t.Fatal("shim never attached (no session-scoped push arrived)")
	}

	// Act — submit the prompt as a FrontendCommand frame (the /stream socket
	// is command-strict since S9; the scoped translator stamps the workspace).
	writeCmd(t, conn, `{"requestId":"r1","submitPrompt":{"text":"hello uds e2e","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert — a conversation delta or a workspace-state for this session
	// arrives within the timeout (the fake SDK echoes the prompt back).
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		switch f := frame.GetFrame().(type) {
		case *frontendv1.FrontendFrame_ConversationDelta:
			if f.ConversationDelta.GetFence() == id || f.ConversationDelta.GetWorkspace() == cwd {
				return // success: the turn rendered as frontend.v1
			}
		case *frontendv1.FrontendFrame_WorkspaceState:
			if f.WorkspaceState.GetSessionId() == id || f.WorkspaceState.GetWorkspace() == cwd {
				return // success: the session controller drove an SSM transition for this session
			}
		}
	}
	t.Fatal("no ConversationDelta or WorkspaceState for the session arrived before the deadline")
}
