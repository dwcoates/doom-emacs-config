package main

import (
	"context"
	"encoding/json"
	"errors"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/boot"
	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/dlog"
	"claude-repld/internal/rollout"
	"claude-repld/internal/server"
)

// shortRoot is a state root SHORT ENOUGH for a unix socket path. The daemon
// refuses at boot when the root plus a shim socket name would overflow
// sun_path, and the test runner's own temp directory is already past that on
// macOS, so a test that wants the spine has to supply a root the daemon can
// actually serve from.
func shortRoot(t *testing.T) string {
	t.Helper()
	root, err := os.MkdirTemp("/tmp", "arb")
	if err != nil {
		t.Fatalf("MkdirTemp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(root) })
	return root
}

// errServed is what the test's serve hook answers so run returns as soon as the
// spine is finished, without a real server and without a wait.
var errServed = errors.New("served")

// testHooks are hooks that stop the spine the moment it would serve, and record
// the listener it would have served on. The graph and the server are stubbed:
// this file's subject is the PROCESS spine — the claim, the advertisement, the
// joining deferral — and not the component graph.
type testHooks struct {
	hooks
	// served records that the serve hook was reached, which is the spine
	// having finished.
	served chan struct{}
}

func newTestHooks() *testHooks {
	th := &testHooks{served: make(chan struct{}, 1)}
	th.hooks = hooks{
		Graph: func(context.Context, process) (*graph, error) {
			return nil, errServed
		},
		Server: func(server.Deps) (server.Server, error) { return nil, errServed },
		Serve: func(context.Context, net.Listener, http.Handler) error {
			th.served <- struct{}{}
			return errServed
		},
	}
	return th
}

// runIn runs the spine against a state root, returning its error.
func runIn(t *testing.T, root string, adjust ...func(*options)) error {
	t.Helper()
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	t.Setenv(dlog.LevelEnvironment, "info")
	opts := options{stateDir: root}
	for _, a := range adjust {
		a(&opts)
	}
	return run(context.Background(), opts, newTestHooks().hooks)
}

func hasRunLogLevel(t *testing.T, raw []byte, operation, level string) bool {
	t.Helper()
	for _, line := range strings.Split(strings.TrimSpace(string(raw)), "\n") {
		var record struct {
			Level     string `json:"level"`
			Operation string `json:"operation"`
		}
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("parse daemon.run.log record %q: %v", line, err)
		}
		if record.Operation == operation && record.Level == level {
			return true
		}
	}
	return false
}

// TestRunRecordsProcessBringUpAtInfo pins that the process lifecycle remains
// visible at the default production threshold.
func TestRunRecordsProcessBringUpAtInfo(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	_ = runIn(t, root)

	// Assert.
	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("ReadFile daemon.run.log: %v", err)
	}
	if !hasRunLogLevel(t, raw, "daemon.cmd.boot", dlog.LevelInfo) {
		t.Fatalf("daemon.run.log = %q, want an INFO daemon.cmd.boot record", string(raw))
	}
}

// TestRunRecordsProcessShutdownAtInfo pins the matching process-ending edge.
func TestRunRecordsProcessShutdownAtInfo(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	_ = runIn(t, root)

	// Assert.
	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("ReadFile daemon.run.log: %v", err)
	}
	if !hasRunLogLevel(t, raw, "daemon.cmd.exit", dlog.LevelInfo) {
		t.Fatalf("daemon.run.log = %q, want an INFO daemon.cmd.exit record", string(raw))
	}
}

// TestTheSecondDaemonLosesTheExclusivityClaim pins the boot-exclusivity ruling:
// the address is bound FIRST, and an unflagged second daemon loses there,
// before it has bound a socket or written anything.
func TestTheSecondDaemonLosesTheExclusivityClaim(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	if err := os.MkdirAll(root, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	incumbent, err := daemonaddr.Bind(filepath.Join(root, "daemon.addr"), 0)
	if err != nil {
		t.Fatalf("the incumbent could not bind: %v", err)
	}
	defer incumbent.Close()

	// Act.
	got := runIn(t, root)

	// Assert.
	if !errors.Is(got, daemonaddr.ErrClaimed) {
		t.Fatalf("run error = %v, want it to wrap %v", got, daemonaddr.ErrClaimed)
	}
}

// TestTheLoserDoesNotDisturbTheIncumbentsAdvertisement pins the other half of
// the same ruling: the daemon that lost writes nothing, so the address file the
// incumbent published is still the incumbent's.
func TestTheLoserDoesNotDisturbTheIncumbentsAdvertisement(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	addrPath := filepath.Join(root, "daemon.addr")
	incumbent, err := daemonaddr.Bind(addrPath, 0)
	if err != nil {
		t.Fatalf("the incumbent could not bind: %v", err)
	}
	defer incumbent.Close()
	if err := incumbent.Publish(); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	_ = runIn(t, root)

	// Assert.
	published, err := os.ReadFile(addrPath)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if strings.TrimSpace(string(published)) != incumbent.Address() {
		t.Fatalf("daemon.addr = %q, want the incumbent's %q", string(published), incumbent.Address())
	}
}

// TestAnIncumbentPublishesItsAddress pins that daemon.addr carries the actually
// bound port: a port-0 bind means the file is the only place it appears.
func TestAnIncumbentPublishesItsAddress(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	if err := runIn(t, root); !errors.Is(err, errServed) {
		t.Fatalf("run error = %v, want the spine to have reached the graph", err)
	}

	// Assert: the advertisement was withdrawn on the way out, and the joining
	// report was never written, which is what distinguishes an incumbent.
	if _, err := os.Stat(filepath.Join(root, rollout.JoiningAddrFile)); !os.IsNotExist(err) {
		t.Fatalf("joining.addr exists for an incumbent (stat err = %v)", err)
	}
}

// TestTheAdvertisementIsWithdrawnOnExit pins the orderly exit: a daemon.addr
// left behind names a listener nobody is serving, and the next client dials it.
func TestTheAdvertisementIsWithdrawnOnExit(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	_ = runIn(t, root)

	// Assert.
	if _, err := os.Stat(filepath.Join(root, "daemon.addr")); !os.IsNotExist(err) {
		t.Fatalf("daemon.addr survives the exit (stat err = %v)", err)
	}
}

// TestAJoiningDaemonDefersTheAdvertisement pins the successor's rule: it binds
// a fresh port and reports it where the incumbent that spawned it is waiting,
// and daemon.addr is written only once it owns every workspace.
func TestAJoiningDaemonDefersTheAdvertisement(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	_ = runIn(t, root, func(o *options) { o.joining = "127.0.0.1:41111" })

	// Assert.
	reported, ok, err := rollout.ReadJoiningAddr(root)
	if err != nil {
		t.Fatalf("ReadJoiningAddr: %v", err)
	}
	if !ok || reported == "" {
		t.Fatalf("the successor reported no address (ok = %v, addr = %q)", ok, reported)
	}
}

// TestPprofRefusesAWildcardBind pins the local-only rule: the profiling surface
// exposes goroutine dumps and the command line of a process holding the
// operator's source tree, so a routable bind is refused rather than opened.
func TestPprofRefusesAWildcardBind(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	err := runIn(t, root, func(o *options) { o.pprof = "0.0.0.0:6060" })

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "profiling surface") {
		t.Fatalf("run error = %v, want the wildcard bind refused", err)
	}
}

// TestPprofIsOffByDefault pins that an empty setting opens no listener at all:
// there is no always-on profiling surface.
func TestPprofIsOffByDefault(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	err := runIn(t, root)

	// Assert.
	if !errors.Is(err, errServed) {
		t.Fatalf("run error = %v, want the spine to have reached the graph with pprof off", err)
	}
}

// TestTheStateRootLayoutIsCreated pins that the daemon creates every directory
// the layout names before anything writes into one.
func TestTheStateRootLayoutIsCreated(t *testing.T) {
	// Arrange.
	root := filepath.Join(shortRoot(t), "fresh")

	// Act.
	_ = runIn(t, root)

	// Assert.
	for _, dir := range []string{"logs", "sock", "intent", "output"} {
		if _, err := os.Stat(filepath.Join(root, dir)); err != nil {
			t.Fatalf("the layout's %s directory was not created: %v", dir, err)
		}
	}
}

// --- the exit joins the background loops ----------------------------------
//
// A loop's iteration reads and writes the state client off its own goroutine.
// Nothing waited for them, so a SIGTERM landing inside one left an ERROR in the
// log of an ORDERLY exit: `daemon.promptqueue.lease_changed: could not read the
// standing holds — sql: database is closed`, from the drain sweep telling the
// prompt queue about the lease its hibernation had just released.

func TestJoinBackgroundLoopsReturnsWhenEveryLoopHasLeft(t *testing.T) {
	// Arrange: a loop that has already ended.
	var loops sync.WaitGroup
	log := dlog.NewTestLogger()
	loops.Add(1)
	loops.Done()

	// Act.
	joinBackgroundLoops(&loops, loopJoinBound, log)

	// Assert.
	if !holdsRecord(log.Records(), "debug", "every background loop left before the teardown") {
		t.Fatalf("records = %+v, want the loops joined", log.Records())
	}
}

func TestJoinBackgroundLoopsReportsALoopThatOutlivesTheBound(t *testing.T) {
	// Arrange: a loop that is still running, released only at cleanup, so the
	// wait ends on the bound rather than on the loop.
	var loops sync.WaitGroup
	log := dlog.NewTestLogger()
	release := make(chan struct{})
	loops.Add(1)
	go func() {
		defer loops.Done()
		<-release
	}()
	t.Cleanup(func() { close(release); loops.Wait() })

	// Act.
	joinBackgroundLoops(&loops, 10*time.Millisecond, log)

	// Assert: reported, never waited on forever.
	if !holdsRecord(log.Records(), "error", "a background loop outlived its serving context; tearing down under it") {
		t.Fatalf("records = %+v, want the overrun reported", log.Records())
	}
}

// holdsRecord reports whether a captured record set names that level and
// message.
func holdsRecord(records []dlog.Record, level, message string) bool {
	for _, r := range records {
		if r.Level == level && r.Message == message {
			return true
		}
	}
	return false
}

func TestJoinQueueWorkReturnsWhenTheQueuesWorkHasLeft(t *testing.T) {
	// Arrange: a queue whose background work is already done.
	log := dlog.NewTestLogger()

	// Act.
	joinQueueWork(func(time.Duration) bool { return true }, loopJoinBound, log)

	// Assert.
	if !holdsRecord(log.Records(), "debug", "the prompt queue's background work left before the teardown") {
		t.Fatalf("records = %+v, want the queue's work joined", log.Records())
	}
}

func TestJoinQueueWorkReportsWorkThatOutlivesTheBound(t *testing.T) {
	// Arrange: a queue whose drain answers that its work is still running.
	log := dlog.NewTestLogger()

	// Act.
	joinQueueWork(func(time.Duration) bool { return false }, loopJoinBound, log)

	// Assert: reported, never waited on forever.
	if !holdsRecord(log.Records(), "error", "the prompt queue's background work outlived its serving context; tearing down under it") {
		t.Fatalf("records = %+v, want the overrun reported", log.Records())
	}
}

// recordingGate is a server.RequestGate that records the order of the waits the
// exit performs, so a test can assert what the exit waited for and in which
// order without standing a real daemon up.
type recordingGate struct {
	http.Handler
	mu    sync.Mutex
	calls []string
}

func (g *recordingGate) AwaitQuiet(time.Duration) int {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, "AwaitQuiet")
	return 0
}

func (g *recordingGate) AwaitWritesQuiet(time.Duration) bool {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, "AwaitWritesQuiet")
	return true
}

func (g *recordingGate) Listener(inner net.Listener) net.Listener { return inner }

func (g *recordingGate) recorded() []string {
	g.mu.Lock()
	defer g.mu.Unlock()
	return append([]string(nil), g.calls...)
}

// TestTheExitWaitsForTheStandingStreamsPushesBeforeShuttingDown pins the half
// of the exit that AwaitQuiet cannot cover: `counted` skips the standing-stream
// paths on purpose, so the daemon's last push — `DaemonShutdownAnnounced`, sent
// on every WatchDaemon stream one line before drain.fire calls Exit — is not an
// in-flight call and nothing else holds the exit for it.
func TestTheExitWaitsForTheStandingStreamsPushesBeforeShuttingDown(t *testing.T) {
	// Arrange
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	gate := &recordingGate{Handler: http.NotFoundHandler()}
	ctx, cancel := context.WithCancel(context.Background())

	// Act
	served := make(chan error, 1)
	go func() { served <- serve(ctx, listener, gate) }()
	cancel()
	if err := <-served; err != nil {
		t.Fatalf("serve() = %v, want an orderly shutdown", err)
	}

	// Assert
	got := gate.recorded()
	want := []string{"AwaitQuiet", "AwaitWritesQuiet"}
	if len(got) != len(want) || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("the exit's waits = %v, want %v — the answers being written first, then the standing streams' own last push", got, want)
	}
}

// stalledSequence is a boot reconciliation that never finishes, which is what
// an unreachable surviving shim's adoption was: the workspace lock reads held,
// so shimclient redials it forever.
type stalledSequence struct {
	// entered is closed once Run has started, so the test waits on an event
	// rather than on a clock.
	entered chan struct{}
}

func (s *stalledSequence) Run(ctx context.Context) (boot.Report, error) {
	close(s.entered)
	<-ctx.Done()
	return boot.Report{}, ctx.Err()
}

func (s *stalledSequence) Joining() bool { return false }

// answeringSequence is a reconciliation that completes at once.
type answeringSequence struct {
	report boot.Report
}

func (s *answeringSequence) Run(context.Context) (boot.Report, error) { return s.report, nil }
func (s *answeringSequence) Joining() bool                            { return false }

// TestReconcileRefusesAStalledBoot pins the watchdog. The listener is bound and
// daemon.addr published before the reconciliation runs, so a step that never
// returns leaves the daemon listening on a socket nothing accepts on — pid
// 31984's accept queue stood at 128/128 for ten hours. Exiting is what lets
// Emacs respawn it.
func TestReconcileRefusesAStalledBoot(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	stalled := &stalledSequence{entered: make(chan struct{})}

	// Act.
	_, err := reconcile(context.Background(), log, stalled, 20*time.Millisecond)

	// Assert.
	<-stalled.entered
	if err == nil {
		t.Fatalf("reconcile = nil error, want a refusal: a boot that never finishes must end the process")
	}
}

// TestReconcileDumpsTheGoroutinesOfAStalledBoot pins the evidence the next
// wedge needs: no debugger can attach on this host, so the blocked
// goroutine's own stack is the only thing that names the blocking call.
func TestReconcileDumpsTheGoroutinesOfAStalledBoot(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	stalled := &stalledSequence{entered: make(chan struct{})}

	// Act.
	_, _ = reconcile(context.Background(), log, stalled, 20*time.Millisecond)
	<-stalled.entered

	// Assert.
	var dumped bool
	for _, record := range log.Records() {
		if record.Level != dlog.LevelError || record.Operation != "daemon.cmd.boot" {
			continue
		}
		if dump, ok := record.Context["goroutine_dump"].(string); ok && strings.Contains(dump, "goroutine") {
			dumped = true
		}
	}
	if !dumped {
		t.Fatalf("records = %+v, want an error daemon.cmd.boot record carrying a goroutine dump", log.Records())
	}
}

// TestReconcileAnswersACompletedBoot pins that the watchdog is not in the way
// of an ordinary boot: the report comes back whole.
func TestReconcileAnswersACompletedBoot(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	want := boot.Report{HoldsRestored: 3}

	// Act.
	report, err := reconcile(context.Background(), log, &answeringSequence{report: want}, time.Second)

	// Assert.
	if err != nil {
		t.Fatalf("reconcile: %v", err)
	}
	if report.HoldsRestored != want.HoldsRestored {
		t.Fatalf("report.HoldsRestored = %d, want %d", report.HoldsRestored, want.HoldsRestored)
	}
}

// runLogRecord answers the first run-log record for an operation at a level,
// so a test can assert on the CONTEXT a record carries and not only that it
// exists.
func runLogRecord(t *testing.T, root, operation, level string) map[string]any {
	t.Helper()

	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("ReadFile daemon.run.log: %v", err)
	}
	for _, line := range strings.Split(strings.TrimSpace(string(raw)), "\n") {
		var record map[string]any
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("parse daemon.run.log record %q: %v", line, err)
		}
		if record["operation"] == operation && record["level"] == level {
			return record
		}
	}
	return nil
}

// TestAStaleAdvertisementIsRecordedBeforeItIsReplaced pins the realtest-1
// finding: a predecessor that went without withdrawing left Emacs an address
// to dial and time out on, and the boot that overwrites it says so.
func TestAStaleAdvertisementIsRecordedBeforeItIsReplaced(t *testing.T) {
	// Arrange: an address nothing is listening on. Port 0 can never be
	// connected to, so the probe is certain rather than merely likely.
	root := shortRoot(t)
	if err := os.WriteFile(filepath.Join(root, "daemon.addr"), []byte("127.0.0.1:0\n"), 0o644); err != nil {
		t.Fatalf("write the stale daemon.addr: %v", err)
	}

	// Act.
	_ = runIn(t, root)

	// Assert.
	record := runLogRecord(t, root, "daemon.cmd.claim", dlog.LevelWarn)
	if record == nil {
		t.Fatal("no WARN daemon.cmd.claim record, want the stale advertisement reported")
	}
	context, _ := record["context"].(map[string]any)
	if got := context["stale_address"]; got != "127.0.0.1:0" {
		t.Fatalf("stale_address = %v, want the address the predecessor left", got)
	}
}

// TestAnAnsweringAdvertisementIsNotCalledStale is the other arm: this
// daemon's OWN address, published and then withdrawn, must never be reported
// as a predecessor's leavings on a later boot of the same state root.
func TestAnAnsweringAdvertisementIsNotCalledStale(t *testing.T) {
	// Arrange: a first boot that published and withdrew.
	root := shortRoot(t)
	_ = runIn(t, root)
	if _, err := os.Stat(filepath.Join(root, "daemon.addr")); !os.IsNotExist(err) {
		t.Fatalf("the first boot left daemon.addr behind (stat err = %v)", err)
	}

	// Act.
	_ = runIn(t, root)

	// Assert.
	if record := runLogRecord(t, root, "daemon.cmd.claim", dlog.LevelWarn); record != nil {
		t.Fatalf("a WARN daemon.cmd.claim record %v was written for a state root with no advertisement", record)
	}
}

// TestTheWithdrawalIsRecordedAtInfo pins the evidence the realtest reader
// needed and did not have: which exit took the advertisement down.
func TestTheWithdrawalIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	_ = runIn(t, root)

	// Assert.
	record := runLogRecord(t, root, "daemon.cmd.exit", dlog.LevelInfo)
	if record == nil {
		t.Fatal("no INFO daemon.cmd.exit record, want the withdrawal recorded")
	}
}
