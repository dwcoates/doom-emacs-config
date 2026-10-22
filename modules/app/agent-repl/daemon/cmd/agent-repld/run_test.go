package main

import (
	"agent-repld/internal/ids"
	"agent-repld/internal/procmetrics"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/tempdirs/tempdirstest"
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/boot"
	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/restarttiming/restarttimingtest"
	"agent-repld/internal/rollout"
	"agent-repld/internal/server"
	"agent-repld/internal/wsm"

	"agentrepl/logging/metrics"
)

// shortRoot is a state root SHORT ENOUGH for a unix socket path. The daemon
// refuses at boot when the root plus a shim socket name would overflow
// sun_path, and the test runner's own temp directory is already past that on
// macOS, so a test that wants the spine has to supply a root the daemon can
// actually serve from.
func shortRoot(t *testing.T) string {
	t.Helper()
	base, err := tempdirstest.ShortBase(os.Getenv)
	if err != nil {
		t.Fatal(err)
	}
	root, err := os.MkdirTemp(base, "arb")
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
	// restarts keeps the boot phases the spine timed.
	restarts *restarttimingtest.Recorder
}

func newTestHooks() *testHooks {
	th := &testHooks{served: make(chan struct{}, 1), restarts: &restarttimingtest.Recorder{}}
	started := time.Now()
	th.hooks = hooks{
		Started:  started,
		Now:      func() time.Time { return started.Add(3 * time.Second) },
		Restarts: th.restarts,
		Graph: func(context.Context, process) (*graph, error) {
			return nil, errServed
		},
		Server: func(server.Deps) (server.Server, error) { return nil, errServed },
		Serve: func(context.Context, net.Listener, http.Handler, serveExit) error {
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
	t.Setenv(wsm.EnvTestUnsyncedWrites, "1")
	t.Setenv(dlog.LevelEnvironment, "info")
	opts := options{stateDir: root}
	for _, a := range adjust {
		a(&opts)
	}
	return run(context.Background(), opts, newTestHooks().hooks)
}

func hasRunLogLevel(t *testing.T, raw []byte, operation, level string) bool {
	t.Helper()
	for _, record := range parseRunLog(t, raw) {
		if record.Operation == operation && record.Level == level {
			return true
		}
	}
	return false
}

// parseRunLog decodes every record of a run log's bytes, failing the test on
// a line that is not one.
func parseRunLog(t *testing.T, raw []byte) []dlog.Record {
	t.Helper()
	var out []dlog.Record
	for _, line := range strings.Split(strings.TrimSpace(string(raw)), "\n") {
		var record dlog.Record
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("parse daemon.run.log record %q: %v", line, err)
		}
		out = append(out, record)
	}
	return out
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

// TestAnUnsyncedStateDatabaseIsRecordedAtBoot pins that a test run's daemon
// says, at INFO, that its state database skips SQLite's forced flushes.
func TestAnUnsyncedStateDatabaseIsRecordedAtBoot(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	_ = runIn(t, root)

	// Assert.
	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("ReadFile daemon.run.log: %v", err)
	}
	if !strings.Contains(string(raw), "skips SQLite's forced flushes") {
		t.Fatalf("daemon.run.log = %q, want the unsynced state database recorded", string(raw))
	}
}

// TestTheUnsyncedFlagWithoutTheVendorGuardRefusesToBoot pins that a live
// daemon handed the test run's flag refuses to boot rather than run its state
// database without durability.
func TestTheUnsyncedFlagWithoutTheVendorGuardRefusesToBoot(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "")
	t.Setenv(wsm.EnvTestUnsyncedWrites, "1")
	t.Setenv(dlog.LevelEnvironment, "info")

	// Act.
	got := run(context.Background(), options{stateDir: root}, newTestHooks().hooks)

	// Assert.
	if got == nil || !strings.Contains(got.Error(), wsm.EnvTestUnsyncedWrites) {
		t.Fatalf("run error = %v, want a refusal naming %s", got, wsm.EnvTestUnsyncedWrites)
	}
	if _, err := os.Stat(filepath.Join(root, "wsm.db")); !os.IsNotExist(err) {
		t.Fatalf("the refused boot opened the state database (stat err = %v)", err)
	}
}

// TestAnInMemoryStateDatabaseIsRecordedAtBootAndLeavesNoFile pins that a test
// run's daemon handed the in-memory seam says so at INFO and never creates
// wsm.db.
func TestAnInMemoryStateDatabaseIsRecordedAtBootAndLeavesNoFile(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	t.Setenv(wsm.EnvTestStateInMemory, "1")

	// Act.
	_ = runIn(t, root)

	// Assert.
	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("ReadFile daemon.run.log: %v", err)
	}
	if !strings.Contains(string(raw), "the state database lives in memory") {
		t.Fatalf("daemon.run.log = %q, want the in-memory state database recorded", string(raw))
	}
	if _, err := os.Stat(filepath.Join(root, "wsm.db")); !os.IsNotExist(err) {
		t.Fatalf("an in-memory boot created wsm.db (stat err = %v)", err)
	}
}

// TestTheInMemoryFlagWithoutTheVendorGuardRefusesToBoot pins that a live
// daemon handed the in-memory seam refuses to boot rather than run on state it
// would lose.
func TestTheInMemoryFlagWithoutTheVendorGuardRefusesToBoot(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "")
	t.Setenv(wsm.EnvTestStateInMemory, "1")
	t.Setenv(dlog.LevelEnvironment, "info")

	// Act.
	got := run(context.Background(), options{stateDir: root}, newTestHooks().hooks)

	// Assert.
	if got == nil || !strings.Contains(got.Error(), wsm.EnvTestStateInMemory) {
		t.Fatalf("run error = %v, want a refusal naming %s", got, wsm.EnvTestStateInMemory)
	}
}

// TestAJoiningDaemonRefusesTheInMemoryFlag pins that a successor cannot keep
// its state in memory: joining reads the incumbent's state file.
func TestAJoiningDaemonRefusesTheInMemoryFlag(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	t.Setenv(wsm.EnvTestStateInMemory, "1")

	// Act.
	got := runIn(t, root, func(o *options) { o.joining = "127.0.0.1:41111" })

	// Assert.
	if got == nil || !strings.Contains(got.Error(), "joining daemon") {
		t.Fatalf("run error = %v, want the joining refusal", got)
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
	if daemonaddr.ParseAdvertisement(string(published)).Address != incumbent.Address() {
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

// TestServeWithdrawsTheAdvertisementBeforeItStopsAccepting pins the shutdown
// invariant realtest 1 needed: a daemon that is shutting down is "not there"
// for clients, so its address comes down at the START of the shutdown sequence
// — before AwaitQuiet, before srv.Shutdown stops the listener accepting — not
// in the exit's deferred catch-all. The onShuttingDown callback is where the
// withdrawal happens, and here it records into the same ordering the gate does,
// so the assertion is that the withdrawal precedes every grace wait.
func TestServeWithdrawsTheAdvertisementBeforeItStopsAccepting(t *testing.T) {
	// Arrange.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	gate := &recordingGate{Handler: http.NotFoundHandler()}
	ctx, cancel := context.WithCancel(context.Background())
	onShuttingDown := func() {
		gate.mu.Lock()
		gate.calls = append(gate.calls, "withdraw")
		gate.mu.Unlock()
	}

	// Act.
	served := make(chan error, 1)
	go func() {
		served <- serve(ctx, listener, gate, serveExit{OnShuttingDown: onShuttingDown, Log: dlog.NewTestLogger()})
	}()
	cancel()
	if err := <-served; err != nil {
		t.Fatalf("serve() = %v, want an orderly shutdown", err)
	}

	// Assert.
	got := gate.recorded()
	want := []string{"withdraw", "AwaitQuiet", "AwaitWritesQuiet"}
	if len(got) != len(want) || got[0] != want[0] || got[1] != want[1] || got[2] != want[2] {
		t.Fatalf("the exit's steps = %v, want %v — the advertisement withdrawn first, before the listener stops accepting", got, want)
	}
}

// fakeWithdrawer is an addrWithdrawal.claim seam: it answers a scripted
// sequence of Withdraw outcomes so a test drives the early/final interplay
// without a real daemon.addr on disk.
type fakeWithdrawer struct {
	outcomes []withdrawOutcome
	calls    int
}

type withdrawOutcome struct {
	withdrawn bool
	err       error
}

func (f *fakeWithdrawer) Withdraw() (bool, error) {
	if f.calls >= len(f.outcomes) {
		return false, nil
	}
	o := f.outcomes[f.calls]
	f.calls++
	return o.withdrawn, o.err
}

func (f *fakeWithdrawer) Address() string { return "127.0.0.1:0" }

// spyLogger records the operation, level and message of each record so a test
// asserts what a withdrawal wrote.
type spyLogger struct {
	records []spyRecord
}

type spyRecord struct {
	level, operation, message string
}

func (s *spyLogger) Debug(operation, message string, _ dlog.Context) {
	s.records = append(s.records, spyRecord{dlog.LevelDebug, operation, message})
}
func (s *spyLogger) Info(operation, message string, _ dlog.Context) {
	s.records = append(s.records, spyRecord{dlog.LevelInfo, operation, message})
}
func (s *spyLogger) Warn(operation, message string, _ dlog.Context) {
	s.records = append(s.records, spyRecord{dlog.LevelWarn, operation, message})
}
func (s *spyLogger) Error(operation, message string, _ dlog.Context) {
	s.records = append(s.records, spyRecord{dlog.LevelError, operation, message})
}
func (s *spyLogger) With(dlog.Context) dlog.Logger { return s }

func (s *spyLogger) message(level string) (string, bool) {
	for _, r := range s.records {
		if r.level == level {
			return r.message, true
		}
	}
	return "", false
}

// TestAddrWithdrawalBeginRecordsTheEarlyWithdrawal pins that the shutdown-begin
// withdrawal removes the file and records it at INFO with the early wording.
func TestAddrWithdrawalBeginRecordsTheEarlyWithdrawal(t *testing.T) {
	// Arrange.
	claim := &fakeWithdrawer{outcomes: []withdrawOutcome{{withdrawn: true}}}
	log := &spyLogger{}
	w := &addrWithdrawal{claim: claim, log: log}

	// Act.
	w.begin()

	// Assert.
	if !w.early {
		t.Fatal("begin did not mark the early withdrawal")
	}
	msg, ok := log.message(dlog.LevelInfo)
	if !ok || !strings.Contains(msg, "at the start of shutdown") {
		t.Fatalf("begin INFO message = %q (present = %v), want the early-withdrawal wording", msg, ok)
	}
}

// TestAddrWithdrawalBeginDoesNotMarkAnUntouchedFile pins that begin does not
// claim an early withdrawal it did not perform (the file already named someone
// else or was gone).
func TestAddrWithdrawalBeginDoesNotMarkAnUntouchedFile(t *testing.T) {
	// Arrange.
	claim := &fakeWithdrawer{outcomes: []withdrawOutcome{{withdrawn: false}}}
	log := &spyLogger{}
	w := &addrWithdrawal{claim: claim, log: log}

	// Act.
	w.begin()

	// Assert.
	if w.early {
		t.Fatal("begin marked an early withdrawal it never performed")
	}
	if _, ok := log.message(dlog.LevelInfo); ok {
		t.Fatalf("begin recorded a withdrawal it never performed: %v", log.records)
	}
}

// TestAddrWithdrawalFinishIsANoOpAfterAnEarlyWithdrawal pins the catch-all's
// safety: after begin took the file down, finish sees Withdraw return false and
// records a no-op, NOT the misleading "left alone" that would suggest a
// successor took over.
func TestAddrWithdrawalFinishIsANoOpAfterAnEarlyWithdrawal(t *testing.T) {
	// Arrange: begin removes it, finish's second Withdraw finds nothing.
	claim := &fakeWithdrawer{outcomes: []withdrawOutcome{{withdrawn: true}, {withdrawn: false}}}
	log := &spyLogger{}
	w := &addrWithdrawal{claim: claim, log: log}
	w.begin()

	// Act.
	w.finish()

	// Assert.
	for _, r := range log.records {
		if strings.Contains(r.message, "left alone") {
			t.Fatalf("finish recorded a misleading 'left alone' after an early withdrawal: %v", log.records)
		}
	}
	if msg, ok := log.message(dlog.LevelDebug); !ok || !strings.Contains(msg, "already withdrawn") {
		t.Fatalf("finish DEBUG message = %q (present = %v), want the no-op wording", msg, ok)
	}
}

// TestAddrWithdrawalFinishWithdrawsWhenServingWasNeverReached pins the defer
// catch-all for a boot error before serving: begin never ran, so finish is the
// one that removes the file and records it withdrawn.
func TestAddrWithdrawalFinishWithdrawsWhenServingWasNeverReached(t *testing.T) {
	// Arrange.
	claim := &fakeWithdrawer{outcomes: []withdrawOutcome{{withdrawn: true}}}
	log := &spyLogger{}
	w := &addrWithdrawal{claim: claim, log: log}

	// Act.
	w.finish()

	// Assert.
	msg, ok := log.message(dlog.LevelInfo)
	if !ok || msg != "daemon.addr was withdrawn" {
		t.Fatalf("finish INFO message = %q (present = %v), want the plain withdrawal record", msg, ok)
	}
}

// TestAddrWithdrawalFinishReportsAnUntouchedFileAsLeftAlone pins that, with no
// early withdrawal, a file finish does not own is reported as left alone.
func TestAddrWithdrawalFinishReportsAnUntouchedFileAsLeftAlone(t *testing.T) {
	// Arrange.
	claim := &fakeWithdrawer{outcomes: []withdrawOutcome{{withdrawn: false}}}
	log := &spyLogger{}
	w := &addrWithdrawal{claim: claim, log: log}

	// Act.
	w.finish()

	// Assert.
	msg, ok := log.message(dlog.LevelInfo)
	if !ok || !strings.Contains(msg, "left alone") {
		t.Fatalf("finish INFO message = %q (present = %v), want the left-alone record", msg, ok)
	}
}

// TestAddrWithdrawalSurfacesAWithdrawError pins that a Withdraw failure is
// surfaced at ERROR rather than swallowed, on both the early and final paths.
func TestAddrWithdrawalSurfacesAWithdrawError(t *testing.T) {
	tests := []struct {
		name string
		act  func(*addrWithdrawal)
	}{
		{"begin", func(w *addrWithdrawal) { w.begin() }},
		{"finish", func(w *addrWithdrawal) { w.finish() }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			claim := &fakeWithdrawer{outcomes: []withdrawOutcome{{err: errors.New("remove refused")}}}
			log := &spyLogger{}
			w := &addrWithdrawal{claim: claim, log: log}

			// Act.
			tc.act(w)

			// Assert.
			msg, ok := log.message(dlog.LevelError)
			if !ok || !strings.Contains(msg, "could not be withdrawn") {
				t.Fatalf("%s ERROR message = %q (present = %v), want the failure surfaced", tc.name, msg, ok)
			}
		})
	}
}

// TestABootErrorBeforeServingWithdrawsViaTheDefer pins that the deferred
// catch-all still covers a path that never reached serving: a graph build that
// fails takes daemon.addr down on the way out, because onShuttingDown is never
// called on that path.
func TestABootErrorBeforeServingWithdrawsViaTheDefer(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	t.Setenv(dlog.LevelEnvironment, "info")
	th := newTestHooks()
	bootErr := errors.New("graph refused to build")
	th.hooks.Graph = func(context.Context, process) (*graph, error) { return nil, bootErr }

	// Act.
	got := run(context.Background(), options{stateDir: root}, th.hooks)

	// Assert.
	if !errors.Is(got, bootErr) {
		t.Fatalf("run error = %v, want it to wrap %v", got, bootErr)
	}
	if _, err := os.Stat(filepath.Join(root, "daemon.addr")); !os.IsNotExist(err) {
		t.Fatalf("daemon.addr survives a boot error (stat err = %v)", err)
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

// TestAnIncumbentAdvertisesItsMetricsTarget pins that the metrics target is
// written beside daemon.addr and names a listener that answers /metrics.
func TestAnIncumbentAdvertisesItsMetricsTarget(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	metricsRoot := t.TempDir()

	// Act.
	err := runIn(t, root, func(o *options) { o.metricsRoot = metricsRoot })

	// Assert.
	if !errors.Is(err, errServed) {
		t.Fatalf("run error = %v, want the spine to have reached the graph", err)
	}
	raw, readErr := os.ReadFile(metrics.TargetPath(metricsRoot, metrics.ProgramDaemon))
	if readErr != nil {
		t.Fatalf("no metrics target was advertised: %v", readErr)
	}
	if !strings.Contains(string(raw), `"instance":"daemon"`) || !strings.Contains(string(raw), "127.0.0.1:") {
		t.Fatalf("target = %s, want a loopback daemon target", raw)
	}
}

// TestAJoiningDaemonDefersItsMetricsTarget pins that a successor advertises
// its metrics surface only with daemon.addr, never at bind: until it owns the
// workspaces the target keeps naming the incumbent.
func TestAJoiningDaemonDefersItsMetricsTarget(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	metricsRoot := t.TempDir()

	// Act.
	_ = runIn(t, root, func(o *options) {
		o.joining = "127.0.0.1:41111"
		o.metricsRoot = metricsRoot
	})

	// Assert.
	if _, err := os.Stat(metrics.TargetPath(metricsRoot, metrics.ProgramDaemon)); !os.IsNotExist(err) {
		t.Fatalf("a joining daemon advertised its metrics target at bind (stat err = %v)", err)
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
	for _, dir := range []string{"logs", "sock", "intent"} {
		if _, err := os.Stat(filepath.Join(root, dir)); err != nil {
			t.Fatalf("the layout's %s directory was not created: %v", dir, err)
		}
	}
}

// runLogRecords answers every record of a state root's run log.
func runLogRecords(t *testing.T, root string) []dlog.Record {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("read daemon.run.log: %v", err)
	}
	return parseRunLog(t, raw)
}

func TestTheBootOpensAFreshWorkspaceStateAndLeavesTheLegacyDatabaseAlone(t *testing.T) {
	// Arrange: a legacy database the rebuild abandons in place.
	root := shortRoot(t)
	legacy := filepath.Join(root, "state.db")
	if err := os.WriteFile(legacy, []byte("legacy sqlite bytes"), 0o644); err != nil {
		t.Fatalf("seed state.db: %v", err)
	}

	// Act.
	_ = runIn(t, root)

	// Assert.
	if got, err := os.ReadFile(legacy); err != nil || string(got) != "legacy sqlite bytes" {
		t.Fatalf("state.db = %q (%v) after boot, want it left untouched", got, err)
	}
	if _, err := os.Stat(filepath.Join(root, "wsm.db")); err != nil {
		t.Fatalf("stat wsm.db = %v, want the fresh database created at boot", err)
	}
}

func TestTheBootRefusesAForeignLayoutVersionInOneErrorRecord(t *testing.T) {
	// Arrange: a first boot stamps the layout row, which is then moved past
	// what this build knows.
	root := shortRoot(t)
	_ = runIn(t, root)
	handle, err := sql.Open("sqlite", filepath.Join(root, "wsm.db"))
	if err != nil {
		t.Fatalf("open wsm.db: %v", err)
	}
	if _, err := handle.Exec(`UPDATE layout SET version = version + 1`); err != nil {
		t.Fatalf("bump the layout version: %v", err)
	}
	handle.Close()

	// Act.
	err = runIn(t, root)

	// Assert.
	if err == nil {
		t.Fatal("a boot over a foreign layout version succeeded, want a refusal")
	}
	refusals := 0
	for _, r := range runLogRecords(t, root) {
		if r.Operation == "daemon.wsm.open" && r.Level == "error" {
			refusals++
		}
	}
	if refusals != 1 {
		t.Fatalf("ERROR daemon.wsm.open records = %d, want exactly one", refusals)
	}
}

func TestTheBootRefusesAStateRootTooLongForAShimSocket(t *testing.T) {
	// Arrange: a root past the unix-socket path limit once "/sock/<name>" is
	// appended.
	root := filepath.Join(shortRoot(t), strings.Repeat("x", 100))

	// Act.
	err := runIn(t, root)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "sock/") {
		t.Fatalf("run = %v, want a refusal naming the socket directory that cannot fit", err)
	}
}

func TestTheBootRecordsHowLongItWaitedForTheClaim(t *testing.T) {
	// Arrange.
	root := shortRoot(t)

	// Act.
	_ = runIn(t, root)

	// Assert.
	for _, r := range runLogRecords(t, root) {
		if r.Operation == "daemon.cmd.claim" && r.Message == "took the boot claim" {
			if _, ok := r.Context["waited_ms"]; !ok {
				t.Fatalf("the claim record %+v carries no waited_ms", r.Context)
			}
			return
		}
	}
	t.Fatal("no daemon.cmd.claim record said the boot claim was taken")
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
	var loops runningLoops
	log := dlog.NewTestLogger()
	ended := make(chan struct{})
	loops.Go("ended", func() { close(ended) })
	<-ended

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
	loops, log := stuckLoop(t, "wedged")

	// Act.
	joinBackgroundLoops(loops, 10*time.Millisecond, log)

	// Assert: reported, never waited on forever.
	if !holdsRecord(log.Records(), "error", "a background loop outlived its serving context; tearing down under it") {
		t.Fatalf("records = %+v, want the overrun reported", log.Records())
	}
}

func TestJoinBackgroundLoopsNamesTheLoopThatOutlivesTheBound(t *testing.T) {
	// Arrange: one loop that ended and one still running.
	loops, log := stuckLoop(t, "wedged")
	ended := make(chan struct{})
	loops.Go("ended", func() { close(ended) })
	<-ended

	// Act.
	joinBackgroundLoops(loops, 10*time.Millisecond, log)

	// Assert: only the loop still running is named.
	rec := findRecord(t, log.Records(), "a background loop outlived its serving context; tearing down under it")
	if got := fmt.Sprint(rec.Context["loops"]); got != "[wedged]" {
		t.Fatalf("loops = %s, want [wedged]", got)
	}
}

func TestJoinBackgroundLoopsDumpsWhatTheLoopIsBlockedOn(t *testing.T) {
	// Arrange.
	loops, log := stuckLoop(t, "wedged")

	// Act.
	joinBackgroundLoops(loops, 10*time.Millisecond, log)

	// Assert: the goroutine dump names the blocking call's stack.
	rec := findRecord(t, log.Records(), "a background loop outlived its serving context; tearing down under it")
	if dump, _ := rec.Context["goroutine_dump"].(string); !strings.Contains(dump, "stuckLoop") {
		t.Fatalf("goroutine_dump does not name the blocked loop's stack:\n%s", dump)
	}
}

// stuckLoop starts one named loop that runs until the test's cleanup.
func stuckLoop(t *testing.T, name string) (*runningLoops, *dlog.TestLogger) {
	t.Helper()
	loops := &runningLoops{}
	release := make(chan struct{})
	loops.Go(name, func() { <-release })
	t.Cleanup(func() { close(release); loops.wg.Wait() })
	return loops, dlog.NewTestLogger()
}

// findRecord answers the one record with that message, failing the test when
// there is none.
func findRecord(t *testing.T, records []dlog.Record, message string) dlog.Record {
	t.Helper()
	if r, ok := dlog.FindRecord(records, dlog.RecordQuery{Message: message}); ok {
		return r
	}
	t.Fatalf("records = %+v, want one with message %q", records, message)
	return dlog.Record{}
}

// holdsRecord reports whether a captured record set names that level and
// message.
func holdsRecord(records []dlog.Record, level, message string) bool {
	_, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: level, Message: message})
	return ok
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

func TestJoinLandingDeploysReturnsWhenEveryLandingsDeployHasEnded(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	joinLandingDeploys(func(time.Duration) bool { return true }, loopJoinBound, log)

	// Assert.
	if !holdsRecord(log.Records(), "debug", "every landing's deploy recorded its end before the teardown") {
		t.Fatalf("records = %+v, want the landings' deploys joined", log.Records())
	}
}

func TestJoinLandingDeploysReportsADeployThatOutlivesTheBound(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	joinLandingDeploys(func(time.Duration) bool { return false }, loopJoinBound, log)

	// Assert: reported, never waited on forever.
	if !holdsRecord(log.Records(), "error", "a landing's deploy outlived its serving context; tearing down under it") {
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

func (g *recordingGate) EndStreams(_ time.Time, end func()) {
	g.mu.Lock()
	g.calls = append(g.calls, "EndStreams")
	g.mu.Unlock()
	end()
}

func (g *recordingGate) AwaitStreamsEnded(time.Time) int {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, "AwaitStreamsEnded")
	return 0
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
	go func() { served <- serve(ctx, listener, gate, serveExit{Log: dlog.NewTestLogger()}) }()
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

// TestTheExitEndsEveryStandingStreamBeforeShuttingDown pins the 2026-09-27
// regression: the standing streams were left to `Shutdown`, which on a hijacked
// h2c connection closes nothing, so the process exit cut them and every client
// still watching read "producer closed without an end frame". The exit ends
// them itself, after the last pushes have left, and waits for their end frames.
func TestTheExitEndsEveryStandingStreamBeforeShuttingDown(t *testing.T) {
	// Arrange
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	gate := &recordingGate{Handler: http.NotFoundHandler()}
	ctx, cancel := context.WithCancel(context.Background())
	endStreams := func() {
		gate.mu.Lock()
		gate.calls = append(gate.calls, "end")
		gate.mu.Unlock()
	}

	// Act
	served := make(chan error, 1)
	go func() {
		served <- serve(ctx, listener, gate, serveExit{EndStreams: endStreams, Log: dlog.NewTestLogger()})
	}()
	cancel()
	if err := <-served; err != nil {
		t.Fatalf("serve() = %v, want an orderly shutdown", err)
	}

	// Assert
	got := gate.recorded()
	want := []string{"AwaitQuiet", "AwaitWritesQuiet", "EndStreams", "end", "AwaitStreamsEnded"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("the exit's steps = %v, want %v — the last pushes out, then every stream ended and its end frame awaited", got, want)
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

func (s *stalledSequence) BringUp(context.Context, []wsm.Workspace) boot.BringUpReport {
	return boot.BringUpReport{}
}

// answeringSequence is a reconciliation that completes at once.
type answeringSequence struct {
	report boot.Report
}

func (s *answeringSequence) Run(context.Context) (boot.Report, error) { return s.report, nil }
func (s *answeringSequence) Joining() bool                            { return false }

func (s *answeringSequence) BringUp(context.Context, []wsm.Workspace) boot.BringUpReport {
	return boot.BringUpReport{}
}

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

// TestTheClaimLoserRecordsAtInfo pins the LEVEL of the exclusivity ruling's
// losing side. Emacs spawns a daemon whenever it cannot tell that one is
// already serving, so losing the claim is the mechanism working: the incumbent
// keeps serving, its advertisement is untouched, and this process exits having
// written nothing. A WARN said a defect had occurred on every such boot.
func TestTheClaimLoserRecordsAtInfo(t *testing.T) {
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
	_ = runIn(t, root)

	// Assert.
	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("ReadFile daemon.run.log: %v", err)
	}
	if hasRunLogLevel(t, raw, "daemon.cmd.claim", dlog.LevelWarn) {
		t.Fatalf("the claim loser was recorded at warn: %q", string(raw))
	}
	if !hasRunLogLevel(t, raw, "daemon.cmd.claim", dlog.LevelInfo) {
		t.Fatalf("daemon.run.log = %q, want an INFO daemon.cmd.claim record", string(raw))
	}
}

// TestARequestlessConnectionDoesNotHoldTheExit pins the 2026-10-06 stall: a
// webview's spare socket, accepted and never written to, kept
// `Server.Shutdown` polling for it until the grace ran out, and a replacement
// daemon waited on the boot claim behind it.
func TestARequestlessConnectionDoesNotHoldTheExit(t *testing.T) {
	// Arrange: one connection that never sends a request, then one that
	// does -- its answer proves the first was accepted (net/http marks a
	// connection StateNew in its accept loop, in accept order).
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	gate := &recordingGate{Handler: http.NotFoundHandler()}
	log := dlog.NewTestLogger()
	ctx, cancel := context.WithCancel(context.Background())
	served := make(chan error, 1)
	go func() { served <- serve(ctx, listener, gate, serveExit{Log: log}) }()
	idle, err := net.Dial("tcp", listener.Addr().String())
	if err != nil {
		t.Fatalf("dial the requestless connection: %v", err)
	}
	defer idle.Close()
	resp, err := http.Get("http://" + listener.Addr().String() + "/")
	if err != nil {
		t.Fatalf("GET: %v", err)
	}
	resp.Body.Close()

	// Act
	cancel()
	if err := <-served; err != nil {
		t.Fatalf("serve() = %v, want an orderly shutdown", err)
	}

	// Assert: the shutdown ended on the connections going idle, not on its
	// grace -- the grace running out is the stall, and it is reported.
	if holdsRecord(log.Records(), "warn", graceExpired) {
		t.Fatalf("records = %+v, want the requestless connection closed rather than waited on until the grace ran out", log.Records())
	}
}

// graceExpired is the record serve writes when the http shutdown's grace runs
// out with connections still held.
const graceExpired = "the exit's http shutdown grace expired with connections still held; closing them under their requests"

// TestServeReportsAnHTTPShutdownGraceThatRunsOut pins that the one bound the
// http shutdown can ride is reported, not only timed: a request still being
// answered when the grace ends is closed under, and the exit says so.
func TestServeReportsAnHTTPShutdownGraceThatRunsOut(t *testing.T) {
	// Arrange: a handler that answers only once the test is over, so the
	// shutdown can end only on its grace.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	entered := make(chan struct{})
	release := make(chan struct{})
	t.Cleanup(func() { close(release) })
	gate := &recordingGate{Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		close(entered)
		<-release
	})}
	log := dlog.NewTestLogger()
	ctx, cancel := context.WithCancel(context.Background())
	served := make(chan error, 1)
	go func() { served <- serve(ctx, listener, gate, serveExit{Grace: 10 * time.Millisecond, Log: log}) }()
	go func() {
		if resp, err := http.Get("http://" + listener.Addr().String() + "/"); err == nil {
			resp.Body.Close()
		}
	}()
	<-entered

	// Act
	cancel()
	if err := <-served; err != nil {
		t.Fatalf("serve() = %v, want the held connection closed under", err)
	}

	// Assert
	rec := findRecord(t, log.Records(), graceExpired)
	if rec.Level != "warn" || rec.Context["grace_ms"] != int64(10) {
		t.Fatalf("record = %+v, want a warn naming the 10ms grace", rec)
	}
}

func TestServeRefusesToRunWithoutALogger(t *testing.T) {
	// Arrange
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	defer listener.Close()
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("serve() ran with no logger, want a panic: the exit could not report a grace it ran out")
		}
	}()

	// Act
	_ = serve(context.Background(), listener, http.NotFoundHandler(), serveExit{})
}

func TestCloseRequestlessClosesOnlyConnectionsWithNoRequest(t *testing.T) {
	// Arrange
	newSide, newPeer := net.Pipe()
	idleSide, idlePeer := net.Pipe()
	defer newPeer.Close()
	defer idleSide.Close()
	defer idlePeer.Close()
	conns := &connStates{byConn: map[net.Conn]http.ConnState{}}
	conns.track(newSide, http.StateNew)
	conns.track(idleSide, http.StateIdle)

	// Act
	conns.closeRequestless()

	// Assert
	if _, err := newSide.Write([]byte("x")); err == nil {
		t.Fatal("the StateNew connection is still open, want it closed")
	}
	go func() { _, _ = idlePeer.Read(make([]byte, 1)) }()
	if _, err := idleSide.Write([]byte("x")); err != nil {
		t.Fatalf("the idle connection was closed (%v), want it left to Shutdown", err)
	}
}

// orderingClaim records the order of the shutdown's first steps, so a test
// asserts the serving lock is released before daemon.addr is withdrawn.
type orderingClaim struct {
	steps     []string
	departErr error
}

func (o *orderingClaim) Depart() error {
	o.steps = append(o.steps, "depart")
	return o.departErr
}

func (o *orderingClaim) Withdraw() (bool, error) {
	o.steps = append(o.steps, "withdraw")
	return true, nil
}

func (o *orderingClaim) Address() string { return "127.0.0.1:0" }

// TestBeginShutdownDepartsBeforeItWithdraws pins the load-bearing order: a
// daemon spawned because daemon.addr went away must find this one departed,
// or it refuses to replace a daemon on its way out. (That the whole step runs
// before any answer owed leaves is
// TestServeWithdrawsTheAdvertisementBeforeItStopsAccepting.)
func TestBeginShutdownDepartsBeforeItWithdraws(t *testing.T) {
	// Arrange.
	claim := &orderingClaim{}
	withdrawal := &addrWithdrawal{claim: claim, log: &spyLogger{}}

	// Act.
	beginShutdown(claim, withdrawal, &spyLogger{})()

	// Assert.
	if got, want := strings.Join(claim.steps, ","), "depart,withdraw"; got != want {
		t.Fatalf("the shutdown's first steps = %s, want %s", got, want)
	}
}

func TestBeginShutdownReportsAFailedDepartureAndStillWithdraws(t *testing.T) {
	// Arrange.
	claim := &orderingClaim{departErr: errors.New("flock: bad file descriptor")}
	log := &spyLogger{}
	withdrawal := &addrWithdrawal{claim: claim, log: &spyLogger{}}

	// Act.
	beginShutdown(claim, withdrawal, log)()

	// Assert.
	msg, ok := log.message(dlog.LevelError)
	if !ok || !strings.Contains(msg, "serving lock") {
		t.Fatalf("records = %+v, want an ERROR naming the serving lock", log.records)
	}
	if got, want := strings.Join(claim.steps, ","), "depart,withdraw"; got != want {
		t.Fatalf("the shutdown's first steps = %s, want %s", got, want)
	}
}

func TestBootModeFor(t *testing.T) {
	tests := []struct {
		name string
		opts options
		want daemonaddr.BootMode
	}{
		{"an ordinary boot probes the serving lock", options{}, daemonaddr.BootOrdinary},
		{"a replacement skips the probe", options{replacing: true}, daemonaddr.BootReplacing},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := bootModeFor(tt.opts)

			// Assert.
			if got != tt.want {
				t.Fatalf("bootModeFor = %v, want %v", got, tt.want)
			}
		})
	}
}

// claimRecord is the daemon.cmd.claim record's context, as the run log has it.
type claimRecord struct {
	Level     string `json:"level"`
	Operation string `json:"operation"`
	Context   struct {
		Verdict  string `json:"verdict"`
		StateDir string `json:"state_dir"`
		Probed   bool   `json:"probed"`
	} `json:"context"`
}

// TestASecondDaemonBesideAServingIncumbentRefusesAtOnceAndSaysWhy runs the
// spine with the PRODUCTION claim wait beside a serving incumbent: the refusal
// is decided by the serving lock, recorded with its verdict and state root.
func TestASecondDaemonBesideAServingIncumbentRefusesAtOnceAndSaysWhy(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	t.Setenv(wsm.EnvTestUnsyncedWrites, "1")
	t.Setenv(dlog.LevelEnvironment, "info")
	incumbent, err := daemonaddr.Bind(filepath.Join(root, "daemon.addr"), 0)
	if err != nil {
		t.Fatalf("the incumbent could not bind: %v", err)
	}
	defer incumbent.Close()
	h := newTestHooks().hooks
	h.ClaimWait = daemonaddr.ClaimWaitBound

	// Act.
	got := run(context.Background(), options{stateDir: root}, h)

	// Assert.
	if !errors.Is(got, daemonaddr.ErrClaimed) {
		t.Fatalf("run error = %v, want it to wrap %v", got, daemonaddr.ErrClaimed)
	}
	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("ReadFile daemon.run.log: %v", err)
	}
	var found *claimRecord
	for _, line := range strings.Split(strings.TrimSpace(string(raw)), "\n") {
		var r claimRecord
		if err := json.Unmarshal([]byte(line), &r); err != nil {
			t.Fatalf("parse daemon.run.log record %q: %v", line, err)
		}
		if r.Operation == "daemon.cmd.claim" && r.Context.Verdict != "" {
			found = &r
		}
	}
	if found == nil {
		t.Fatalf("daemon.run.log = %q, want a daemon.cmd.claim record with a verdict", raw)
	}
	if found.Context.Verdict != string(daemonaddr.VerdictIncumbentServing) || !found.Context.Probed || found.Context.StateDir != root {
		t.Fatalf("claim record = %+v, want verdict %q, probed, state_dir %q", found.Context, daemonaddr.VerdictIncumbentServing, root)
	}
}

// TestTheBootTimesTheClaimFromTheProcessStart pins that the spine times its
// boot claim from the process's start, as a cold boot or a successor's.
func TestTheBootTimesTheClaimFromTheProcessStart(t *testing.T) {
	tests := []struct {
		name     string
		joining  string
		wantMode string
	}{
		{name: "an incumbent's boot is cold", wantMode: "cold"},
		{name: "a successor's boot is a successor's", joining: "127.0.0.1:41111", wantMode: "successor"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := shortRoot(t)
			t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
			t.Setenv(wsm.EnvTestUnsyncedWrites, "1")
			th := newTestHooks()

			// Act
			_ = run(context.Background(), options{stateDir: root, joining: tc.joining}, th.hooks)

			// Assert
			got := th.restarts.Observations()
			if len(got) == 0 || got[0].Phase() != "claim" || got[0].Cause() != tc.wantMode || got[0].Seconds() != 3 {
				t.Fatalf("boot phases = %+v, want the claim first, %s, 3s from the start", got, tc.wantMode)
			}
		})
	}
}

// TestRunRefusesToBootWithoutItsStartOrAClock pins the required timing seams.
func TestRunRefusesToBootWithoutItsStartOrAClock(t *testing.T) {
	tests := []struct {
		name  string
		strip func(*hooks)
	}{
		{"no process start", func(h *hooks) { h.Started = time.Time{} }},
		{"no clock", func(h *hooks) { h.Now = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			th := newTestHooks()
			tc.strip(&th.hooks)

			// Act
			err := run(context.Background(), options{stateDir: shortRoot(t)}, th.hooks)

			// Assert
			if err == nil || !strings.Contains(err.Error(), "time its boot") {
				t.Fatalf("run = %v, want the missing timing seam refused", err)
			}
		})
	}
}

// TestTheLastMetricsPushPrecedesTheStateClose pins the exit's order: the
// metrics surface pushes its final values, the state collector's among them,
// before the state client closes, so the push reads an open database.
func TestTheLastMetricsPushPrecedesTheStateClose(t *testing.T) {
	// Arrange: a store import endpoint that keeps what it is pushed.
	root := shortRoot(t)
	metricsRoot := t.TempDir()
	pushed := make(chan string, 4)
	store := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		pushed <- string(body)
		w.WriteHeader(http.StatusNoContent)
	}))
	t.Cleanup(store.Close)
	storeFile := fmt.Sprintf(`{"import_url":%q,"job":"agent-repl"}`, store.URL+"/api/v1/import/prometheus")
	if err := os.WriteFile(filepath.Join(metricsRoot, metrics.StoreFile), []byte(storeFile), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act.
	_ = runIn(t, root, func(o *options) { o.metricsRoot = metricsRoot })

	// Assert.
	select {
	case body := <-pushed:
		if !strings.Contains(body, "agentrepl_log_files{") {
			t.Fatalf("the last push carries no state series:\n%s", body)
		}
	default:
		t.Fatal("the exit pushed nothing to the store")
	}
	raw, err := os.ReadFile(filepath.Join(root, "logs", "daemon.run.log"))
	if err != nil {
		t.Fatalf("read the run log: %v", err)
	}
	if strings.Contains(string(raw), "database is closed") {
		t.Fatalf("the last push read a closed state client:\n%s", raw)
	}
}

// fakeWorkspaceList answers arranged workspaces, or an error.
type fakeWorkspaceList struct {
	workspaces []wsm.Workspace
	err        error
}

func (f fakeWorkspaceList) ListWorkspaces(context.Context) ([]wsm.Workspace, error) {
	return f.workspaces, f.err
}

func TestABootExpectsEveryOpenWorkspacesStreamsBack(t *testing.T) {
	// Arrange
	started := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, func() time.Time { return started.Add(2 * time.Second) })
	db := fakeWorkspaceList{workspaces: []wsm.Workspace{
		{ID: "open"}, {ID: "hidden", Hidden: true}, {ID: "closed", Closed: true},
	}}

	// Act
	err := expectBootReconnects(context.Background(), db, r, started)
	for _, ws := range []ids.WorkspaceID{"open", "hidden", "closed"} {
		r.Connected(ws, restarttiming.ReconnectHost)
		r.Connected(ws, restarttiming.ReconnectWeb)
	}

	// Assert
	if err != nil {
		t.Fatalf("expectBootReconnects: %v", err)
	}
	got := rec.Of(restarttiming.WorkspaceReconnect)
	if len(got) != 2 || got[0].Workspace() != "open" || got[1].Workspace() != "open" || got[0].Cause() != "cold" || got[0].Seconds() != 2 {
		t.Fatalf("reconnects = %+v, want the open workspace's two streams, cold, 2s", got)
	}
}

func TestABootThatCannotListItsWorkspacesSaysWhy(t *testing.T) {
	// Arrange
	r := restarttiming.NewReconnects(&restarttimingtest.Recorder{}, time.Now)

	// Act
	err := expectBootReconnects(context.Background(), fakeWorkspaceList{err: errors.New("database is locked")}, r, time.Now())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("expectBootReconnects = %v, want the read's failure", err)
	}
}

// TestATestRunRefusesTheLiveMetricsRoot pins that a test-run daemon handed a
// metrics root outside its temporary directory refuses to boot rather than
// advertise itself to, and push into, the live metrics store.
func TestATestRunRefusesTheLiveMetricsRoot(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	live := filepath.Join(string(filepath.Separator), "nonexistent-live-home", ".cache", "agent-repl", "metrics")

	// Act.
	err := runIn(t, root, func(o *options) { o.metricsRoot = live })

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "never writes the live metrics store") {
		t.Fatalf("run = %v, want the live metrics root refused", err)
	}
	if _, statErr := os.Stat(live); !os.IsNotExist(statErr) {
		t.Fatalf("the refused root was created (stat err = %v)", statErr)
	}
}

// fakeIdentities answers Identity from a table, or with ERR.
type fakeIdentities struct {
	ids map[int]procmetrics.Identity
	err error
}

func (f fakeIdentities) Identity(pid int) (procmetrics.Identity, error) {
	if f.err != nil {
		return procmetrics.Identity{}, f.err
	}
	return f.ids[pid], nil
}

func TestABootRefusalNamesTheProcessHoldingTheLock(t *testing.T) {
	held := func(pid int) error {
		return &daemonaddr.HeldError{Path: "/state/daemon.serving", Holder: daemonaddr.Holder{PID: pid}}
	}
	tests := []struct {
		name  string
		err   error
		procs fakeIdentities
		want  dlog.Context
	}{
		{
			name:  "a holder that still runs is named with its parent",
			err:   held(4242),
			procs: fakeIdentities{ids: map[int]procmetrics.Identity{4242: {PID: 4242, PPID: 1, Name: "agent-repl-shim"}}},
			want:  dlog.Context{"held_lock": "/state/daemon.serving", "holder_pid": 4242, "holder": "agent-repl-shim", "holder_ppid": 1},
		},
		{
			name:  "a holder gone since the refusal keeps its pid",
			err:   held(4242),
			procs: fakeIdentities{err: procmetrics.ErrGone},
			want:  dlog.Context{"held_lock": "/state/daemon.serving", "holder_pid": 4242, "holder": "exited since the refusal"},
		},
		{
			name:  "a holder that cannot be read says why",
			err:   held(4242),
			procs: fakeIdentities{err: errors.New("denied")},
			want:  dlog.Context{"held_lock": "/state/daemon.serving", "holder_pid": 4242, "holder": "could not be read: denied"},
		},
		{
			name: "a lock the kernel named no holder for says so",
			err:  held(0),
			want: dlog.Context{"held_lock": "/state/daemon.serving", "holder": "the kernel named no holder"},
		},
		{
			name: "a refusal with no holder attached adds nothing",
			err:  daemonaddr.ErrClaimed,
			want: dlog.Context{},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			record := dlog.Context{}

			// Act
			nameClaimHolder(record, fmt.Errorf("bind: %w", tt.err), tt.procs)

			// Assert
			if !reflect.DeepEqual(record, tt.want) {
				t.Fatalf("record = %v, want %v", record, tt.want)
			}
		})
	}
}
