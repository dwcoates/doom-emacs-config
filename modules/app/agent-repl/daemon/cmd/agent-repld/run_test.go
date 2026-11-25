package main

import (
	"agent-repld/internal/holderensure"
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
	"maps"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"agent-repld/internal/boot"
	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/restarttiming/restarttimingtest"
	"agent-repld/internal/rollout"
	"agent-repld/internal/server"
	"agent-repld/internal/server/servertest"
	"agent-repld/internal/wsm"

	"agentrepl/logging"
	"agentrepl/logging/sockethold/holdtest"

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
	// windows keeps the resource windows the spine opened.
	windows *restarttimingtest.Windows
}

func newTestHooks() *testHooks {
	th := &testHooks{served: make(chan struct{}, 1), restarts: &restarttimingtest.Recorder{}, windows: &restarttimingtest.Windows{}}
	started := time.Now()
	th.hooks = hooks{
		Started:  started,
		Now:      func() time.Time { return started.Add(3 * time.Second) },
		Restarts: th.restarts,
		Windows:  th.windows,
		// A TEST DECLARES NO HOLDER, so its daemon never installs one: it
		// takes from the test's own holder (testHolder).
		Holders: func(dlog.Logger, string) ([]holderensure.Holder, func(context.Context, string) error, error) {
			return nil, func(context.Context, string) error { return errors.New("a test daemon installs no holder") }, nil
		},
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
	opts := options{stateDir: root, holder: testHolder(t)}
	for _, a := range adjust {
		a(&opts)
	}
	return run(context.Background(), opts, newTestHooks().hooks)
}

// testHolder serves a shared loopback listener, named "daemon", on a control
// socket for the test's life, as production's socket holder does, and
// answers the control socket's path.
func testHolder(t *testing.T) string {
	t.Helper()
	file, _ := holdtest.TCP(t)
	return holdtest.Start(t, map[string]*os.File{holderDaemonSocket: file}, nil)
}

func TestRunRefusesADaemonWithNoSocketHolderToTake(t *testing.T) {
	tests := []struct {
		name    string
		adjust  func(*options)
		message string
	}{
		{name: "no --holder and no declared holder is refused", adjust: func(o *options) { o.holder = "" },
			message: "the daemon was given no socket holder, and no requirement declares its holder"},
		{name: "a --holder no holder serves, that no requirement declares, is refused", adjust: func(o *options) { o.holder = "/tmp/agent-repl-no-holder.sock" },
			message: "no holder serves the control socket, and no requirement declares a holder there, so none is installed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := shortRoot(t)

			// Act
			err := runIn(t, root, tt.adjust)

			// Assert
			if err == nil {
				t.Fatal("run = nil, want the boot refused")
			}
			found := false
			for _, r := range runLogRecords(t, root) {
				if r.Level == "error" && r.Message == tt.message {
					found = true
				}
			}
			if !found {
				t.Fatalf("the run log holds no ERROR %q", tt.message)
			}
		})
	}
}

func TestRunWithNoHolderFlagTakesTheDeclaredHolder(t *testing.T) {
	// Arrange
	root := shortRoot(t)
	control := testHolder(t)
	th := newTestHooks()
	var workRoot string
	th.hooks.Holders = func(_ dlog.Logger, root string) ([]holderensure.Holder, func(context.Context, string) error, error) {
		workRoot = root
		return []holderensure.Holder{{ID: holderensure.DaemonHolder, Control: control}}, func(context.Context, string) error {
			return errors.New("a serving holder is never installed")
		}, nil
	}
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	t.Setenv(wsm.EnvTestUnsyncedWrites, "1")
	t.Setenv(dlog.LevelEnvironment, "info")

	// Act
	err := run(context.Background(), options{stateDir: root, multiRepoConfigDir: "/w/.claude-work"}, th.hooks)

	// Assert
	if !errors.Is(err, errServed) {
		t.Fatalf("run = %v, want the spine to reach its serve hook on the declared holder", err)
	}
	// THE INSTALLER RUNS WITH THE DAEMON'S OWN WORK ROOT
	// (deploy.ScriptRequirements.WorkConfigRoot), never an inherited one.
	if workRoot != "/w/.claude-work" {
		t.Fatalf("holder installer's work root = %q, want the daemon's -multi-repo-config-dir", workRoot)
	}
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
		if line == "" {
			continue
		}
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
	got := run(context.Background(), options{stateDir: root, holder: testHolder(t)}, newTestHooks().hooks)

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
	got := run(context.Background(), options{stateDir: root, holder: testHolder(t)}, newTestHooks().hooks)

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
	want := []string{"withdraw", "AwaitQuiet", "AwaitPushesSent"}
	if !slices.Equal(got[:min(len(got), len(want))], want) {
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
	// read is the rest of what the Withdraw answers (its Withdrawn is
	// withdrawn's).
	read daemonaddr.Withdrawal
	err  error
}

func (f *fakeWithdrawer) Withdraw() (daemonaddr.Withdrawal, error) {
	if f.calls >= len(f.outcomes) {
		return daemonaddr.Withdrawal{}, nil
	}
	o := f.outcomes[f.calls]
	f.calls++
	wd := o.read
	wd.Withdrawn = o.withdrawn
	return wd, o.err
}

// spyLogger captures each record so a test asserts what a withdrawal wrote:
// a dlog.TestLogger, usable as its zero value.
type spyLogger struct {
	dlog.TestLogger
}

func (s *spyLogger) message(level string) (string, bool) {
	for _, r := range s.Records() {
		if r.Level == level {
			return r.Message, true
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
		t.Fatalf("begin recorded a withdrawal it never performed: %v", log.Records())
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
	for _, r := range log.Records() {
		if strings.Contains(r.Message, "left alone") {
			t.Fatalf("finish recorded a misleading 'left alone' after an early withdrawal: %v", log.Records())
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

// TestEveryWithdrawalRecordNamesWhatTheFileHeld pins the fields of every
// withdrawal record: this daemon's own advertised address, pid and private
// address, and daemon.addr's pid, shared address and private address as read,
// or "file" naming why there are none. A record naming only this daemon's
// address could not tell whose file it left (2026-11-22).
func TestEveryWithdrawalRecordNamesWhatTheFileHeld(t *testing.T) {
	own := daemonaddr.Withdrawal{Address: "127.0.0.1:41000", PID: 30922, Private: "127.0.0.1:5001"}
	read := func(file daemonaddr.Advertisement) daemonaddr.Withdrawal {
		wd := own
		wd.Read, wd.Present, wd.File = true, true, file
		return wd
	}
	ownFile := read(daemonaddr.Advertisement{Address: "127.0.0.1:41000", PID: 30922, PIDKnown: true, Private: "127.0.0.1:5001"})
	absent := own
	absent.Read = true
	ownFields := dlog.Context{"address": "127.0.0.1:41000", "pid": 30922, "private_address": "127.0.0.1:5001"}
	ownFileFields := dlog.Merge(ownFields, dlog.Context{"file": "present", "file_address": "127.0.0.1:41000", "file_pid_known": true, "file_pid": 30922, "file_private_address": "127.0.0.1:5001"})
	tests := []struct {
		name     string
		outcomes []withdrawOutcome
		act      func(*addrWithdrawal)
		level    string
		message  string
		want     dlog.Context
	}{
		{name: "withdrew early", outcomes: []withdrawOutcome{{withdrawn: true, read: ownFile}}, act: (*addrWithdrawal).begin,
			level: dlog.LevelInfo, message: "daemon.addr was withdrawn at the start of shutdown, before the listener stopped accepting", want: ownFileFields},
		{name: "withdrew at finish", outcomes: []withdrawOutcome{{withdrawn: true, read: ownFile}}, act: (*addrWithdrawal).finish,
			level: dlog.LevelInfo, message: "daemon.addr was withdrawn", want: ownFileFields},
		{name: "already withdrawn", outcomes: []withdrawOutcome{{withdrawn: true, read: ownFile}, {read: absent}},
			act:   func(w *addrWithdrawal) { w.begin(); w.finish() },
			level: dlog.LevelDebug, message: "daemon.addr was already withdrawn at the start of shutdown", want: dlog.Merge(ownFields, dlog.Context{"file": "absent"})},
		{name: "left alone naming another pid", outcomes: []withdrawOutcome{{read: read(daemonaddr.Advertisement{Address: "127.0.0.1:41000", PID: 31000, PIDKnown: true, Private: "127.0.0.1:5002"})}},
			act: (*addrWithdrawal).finish, level: dlog.LevelInfo, message: "daemon.addr was left alone; it does not name this daemon",
			want: dlog.Merge(ownFields, dlog.Context{"file": "present", "file_address": "127.0.0.1:41000", "file_pid_known": true, "file_pid": 31000, "file_private_address": "127.0.0.1:5002"})},
		{name: "left alone naming no pid", outcomes: []withdrawOutcome{{read: read(daemonaddr.Advertisement{Address: "127.0.0.1:41000"})}},
			act: (*addrWithdrawal).finish, level: dlog.LevelInfo, message: "daemon.addr was left alone; it does not name this daemon",
			want: dlog.Merge(ownFields, dlog.Context{"file": "present", "file_address": "127.0.0.1:41000", "file_pid_known": false, "file_private_address": ""})},
		{name: "left alone with no file", outcomes: []withdrawOutcome{{read: absent}},
			act: (*addrWithdrawal).finish, level: dlog.LevelInfo, message: "daemon.addr was left alone; it does not name this daemon",
			want: dlog.Merge(ownFields, dlog.Context{"file": "absent"})},
		{name: "left alone unread, holding no boot claim", outcomes: []withdrawOutcome{{read: own}},
			act: (*addrWithdrawal).finish, level: dlog.LevelInfo, message: "daemon.addr was left alone; it does not name this daemon",
			want: dlog.Merge(ownFields, dlog.Context{"file": "unread"})},
		{name: "a remove that failed after the read", outcomes: []withdrawOutcome{{read: ownFile, err: errors.New("remove refused")}},
			act: (*addrWithdrawal).finish, level: dlog.LevelError, message: "daemon.addr could not be withdrawn",
			want: dlog.Merge(ownFileFields, dlog.Context{"error": "remove refused"})},
		{name: "a read that failed", outcomes: []withdrawOutcome{{read: own, err: errors.New("read refused")}},
			act: (*addrWithdrawal).begin, level: dlog.LevelError, message: "daemon.addr could not be withdrawn",
			want: dlog.Merge(ownFields, dlog.Context{"file": "unread", "error": "read refused"})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := &spyLogger{}
			w := &addrWithdrawal{claim: &fakeWithdrawer{outcomes: tt.outcomes}, log: log}

			// Act
			tt.act(w)

			// Assert
			var got *dlog.Record
			records := log.Records()
			for i := range records {
				if r := records[i]; r.Level == tt.level && r.Message == tt.message {
					got = &records[i]
				}
			}
			if got == nil {
				t.Fatalf("records = %+v, want %s %q", records, tt.level, tt.message)
			}
			if !maps.Equal(got.Context, tt.want) {
				t.Fatalf("fields = %v, want %v", got.Context, tt.want)
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
	got := run(context.Background(), options{stateDir: root, holder: testHolder(t)}, th.hooks)

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

// runLogRecords answers every record of a state root's run log: the log and
// its warnings file, where every warn and error record lands
// (proto/vocab/log-warnings-sink.json).
func runLogRecords(t *testing.T, root string) []dlog.Record {
	t.Helper()
	path := filepath.Join(root, "logs", "daemon.run.log")
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read daemon.run.log: %v", err)
	}
	warnings, err := os.ReadFile(logging.WarningsPath(path))
	if err != nil && !os.IsNotExist(err) {
		t.Fatalf("read daemon.run.warn.log: %v", err)
	}
	return append(parseRunLog(t, raw), parseRunLog(t, warnings)...)
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

// --- the opening views ---------------------------------------------------

func TestPrimeOpeningViewsRecordsHowLongThePublishTook(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	err := primeOpeningViews(context.Background(), log, func(context.Context) error { return nil })

	// Assert.
	if err != nil {
		t.Fatalf("primeOpeningViews = %v, want nil", err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.cmd.serve", Message: "published the opening views"})
	if !ok {
		t.Fatalf("records = %+v, want the publish recorded at INFO", log.Records())
	}
	if _, ok := rec.Context["took_ms"]; !ok {
		t.Fatalf("the opening views' record %+v carries no took_ms", rec.Context)
	}
}

func TestPrimeOpeningViewsWithNoPrimeDoesNothing(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	err := primeOpeningViews(context.Background(), log, nil)

	// Assert.
	if err != nil || len(log.Records()) != 0 {
		t.Fatalf("primeOpeningViews(nil) = %v with records %+v, want nil and none", err, log.Records())
	}
}

func TestPrimeOpeningViewsThatFailFailTheBootAtError(t *testing.T) {
	// Arrange: the registry publish refuses, as a corrupt tasks row makes it.
	log := dlog.NewTestLogger()
	cause := errors.New("workspace: publish the registry: read the tasks: wsm: corrupt row")

	// Act.
	err := primeOpeningViews(context.Background(), log, func(context.Context) error { return cause })

	// Assert.
	if !errors.Is(err, cause) || !strings.Contains(err.Error(), "agent-repld: publish the opening views") {
		t.Fatalf("primeOpeningViews = %v, want the failure wrapped naming the boot step", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.cmd.serve", Message: "the opening views could not be published"}); !ok {
		t.Fatalf("records = %+v, want the failure recorded at ERROR", log.Records())
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

func (g *recordingGate) AwaitPushesSent(time.Time) bool {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, "AwaitPushesSent")
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

func (g *recordingGate) GoAway(time.Time) int {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.calls = append(g.calls, "GoAway")
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
	want := []string{"AwaitQuiet", "AwaitPushesSent", "EndStreams", "AwaitStreamsEnded", "GoAway", "AwaitStreamsEnded"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("the exit's waits = %v, want %v — the answers produced first, then the standing streams' own last pushes sent, then every stream's end awaited onto the socket, then every new stream refused and the ones admitted before awaited too", got, want)
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
	want := []string{"AwaitQuiet", "AwaitPushesSent", "EndStreams", "end", "AwaitStreamsEnded", "GoAway", "AwaitStreamsEnded"}
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

	// The run log and its warnings file, where every warn and error record
	// lands (proto/vocab/log-warnings-sink.json).
	path := filepath.Join(root, "logs", "daemon.run.log")
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile daemon.run.log: %v", err)
	}
	warnings, err := os.ReadFile(logging.WarningsPath(path))
	if err != nil && !os.IsNotExist(err) {
		t.Fatalf("ReadFile daemon.run.warn.log: %v", err)
	}
	for _, line := range strings.Split(strings.TrimSpace(string(raw)+"\n"+string(warnings)), "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
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

// runCapturingDeaths runs the spine against ROOT, as runIn does, and
// answers the deaths the boot handed its graph (process.Died), which the
// graph owes the deploy mediator a check of (mediationHook.deathReaped).
func runCapturingDeaths(t *testing.T, root string, adjust ...func(*options)) []predecessorDeath {
	t.Helper()
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	t.Setenv(wsm.EnvTestUnsyncedWrites, "1")
	t.Setenv(dlog.LevelEnvironment, "info")
	opts := options{stateDir: root, holder: testHolder(t)}
	for _, a := range adjust {
		a(&opts)
	}
	th := newTestHooks()
	var died []predecessorDeath
	reached := false
	th.hooks.Graph = func(_ context.Context, p process) (*graph, error) {
		died, reached = p.Died, true
		return nil, errServed
	}
	_ = run(context.Background(), opts, th.hooks)
	if !reached {
		t.Fatal("the spine never reached its graph")
	}
	return died
}

// TestABootRecordsALeftoverLifeLockAsADeath pins the 2026-11-21 20:28 miss:
// daemon pid 78035 panicked and the next boot recorded nothing. A life lock
// standing with no holder is a daemon that ended without Release, so the boot
// records it at ERROR with its panic and hands the death to its graph, which
// owes the deploy mediator the check of it. Production's case has no
// daemon.addr at all (Emacs deleted the dead daemon's), and the death stands
// on the life lock alone.
func TestABootRecordsALeftoverLifeLockAsADeath(t *testing.T) {
	tests := []struct {
		name string
		// ad is the daemon.addr left standing, "" for none.
		ad string
	}{
		{name: "a leftover life lock with no daemon.addr at all"},
		{name: "a leftover life lock beside a standing daemon.addr", ad: "127.0.0.1:1\npid=78035\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the dead daemon's life lock, and its panic in the stdio log.
			root := shortRoot(t)
			const instance, deadPID = "641fd388dc8e496c", 78035
			if err := os.MkdirAll(filepath.Join(root, daemonaddr.LifeDirName), 0o755); err != nil {
				t.Fatal(err)
			}
			lock := daemonaddr.LifePath(filepath.Join(root, "daemon.addr"), instance)
			if err := os.WriteFile(lock, []byte("pid="+strconv.Itoa(deadPID)+"\n"), 0o644); err != nil {
				t.Fatal(err)
			}
			if err := os.MkdirAll(filepath.Join(root, "logs"), 0o755); err != nil {
				t.Fatal(err)
			}
			stdio := `{"message":"x","pid":78035}` + "\npanic: boom\n\ngoroutine 1 [running]:\nagent-repld/internal/x.f(0x1)\n\tagent-repld/internal/x/x.go:9 +0x10\n"
			if err := os.WriteFile(filepath.Join(root, "logs", dlog.StdioLogName), []byte(stdio), 0o644); err != nil {
				t.Fatal(err)
			}
			if tt.ad != "" {
				if err := os.WriteFile(filepath.Join(root, "daemon.addr"), []byte(tt.ad), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			died := runCapturingDeaths(t, root)

			// Assert
			record := runLogRecord(t, root, opPredecessorDied, dlog.LevelError)
			if record == nil {
				t.Fatalf("no ERROR %s record, want the leftover life lock recorded as a death", opPredecessorDied)
			}
			context, _ := record["context"].(map[string]any)
			if context["instance"] != instance || context["pid"] != float64(deadPID) || context["panic"] != "panic: boom" {
				t.Fatalf("record context %v, want instance %s, pid %d and its panic", context, instance, deadPID)
			}
			if len(died) != 1 || died[0].Instance != instance || !died[0].Panicked {
				t.Fatalf("the graph was handed %+v, want the one death, which owes the mediator its check", died)
			}
			if _, err := os.Stat(lock); !os.IsNotExist(err) {
				t.Fatalf("the leftover life lock = %v, want it reaped", err)
			}
		})
	}
}

// TestAStaleAdvertisementIsRecordedBeforeItIsReplaced pins the realtest-1
// finding: the boot that overwrites a standing advertisement says so. It is
// no death: daemon.addr is shared between an incumbent and its successor
// across a yield, so orderly sequences leave one standing, and the deaths
// are the life locks'. The advertisement names the SHARED address the socket
// holder keeps answering, as production's does.
func TestAStaleAdvertisementIsRecordedBeforeItIsReplaced(t *testing.T) {
	// Arrange: a standing advertisement of the holder's live address, and no
	// leftover life lock.
	root := shortRoot(t)
	file, shared := holdtest.TCP(t)
	holder := holdtest.Start(t, map[string]*os.File{holderDaemonSocket: file}, nil)
	standingPID := os.Getpid() + 1
	ad := shared + "\npid=" + strconv.Itoa(standingPID) + "\nprivate=127.0.0.1:1\n"
	if err := os.WriteFile(filepath.Join(root, "daemon.addr"), []byte(ad), 0o644); err != nil {
		t.Fatalf("write the standing daemon.addr: %v", err)
	}

	// Act
	died := runCapturingDeaths(t, root, func(o *options) { o.holder = holder })

	// Assert
	want := "a standing advertisement naming pid " + strconv.Itoa(standingPID) + " is replaced under the boot claim"
	var replaced map[string]any
	for _, r := range runLogRecords(t, root) {
		if r.Operation == "daemon.cmd.claim" && r.Level == dlog.LevelInfo && r.Message == want {
			replaced = r.Context
		}
	}
	if replaced == nil || replaced["address"] != shared {
		t.Fatalf("no INFO daemon.cmd.claim %q naming the address %s (got %v)", want, shared, replaced)
	}
	if record := runLogRecord(t, root, opPredecessorDied, dlog.LevelError); record != nil {
		t.Fatalf("an ERROR %s record %v, want a standing advertisement read as no death", opPredecessorDied, record)
	}
	if len(died) != 0 {
		t.Fatalf("the graph was handed deaths %+v, want none and no check owed", died)
	}
}

// TestAnUnreadableAdvertisementIsRecordedAtBoot pins that an advertisement
// the boot cannot read is recorded at ERROR with its path, never read as
// none.
func TestAnUnreadableAdvertisementIsRecordedAtBoot(t *testing.T) {
	// Arrange: a directory where daemon.addr belongs.
	root := shortRoot(t)
	addrPath := filepath.Join(root, "daemon.addr")
	if err := os.Mkdir(addrPath, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	_ = runIn(t, root)

	// Assert.
	record := runLogRecord(t, root, "daemon.cmd.claim", dlog.LevelError)
	if record == nil {
		t.Fatal("no ERROR daemon.cmd.claim record, want the unreadable advertisement recorded")
	}
	context, _ := record["context"].(map[string]any)
	if context["addr_path"] != addrPath || context["error"] == nil {
		t.Fatalf("record context %v, want the path %s and the read's error", context, addrPath)
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

func TestJoinedWaitReturnsOnlyAfterTheRunReturned(t *testing.T) {
	tests := []struct {
		name string
		runs int
	}{
		{name: "one run", runs: 1},
		{name: "a second run is waited on no differently", runs: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: work that finishes only once released.
			release := make(chan struct{})
			var finished atomic.Int32
			run, wait := joined(func() {
				<-release
				finished.Add(1)
			})
			var runs sync.WaitGroup
			for range tt.runs {
				runs.Add(1)
				go func() { defer runs.Done(); run() }()
			}
			waited := make(chan int32, 1)
			go func() { wait(); waited <- finished.Load() }()

			// Act
			close(release)

			// Assert: the wait saw the work finished.
			if got := <-waited; got < 1 {
				t.Fatalf("wait returned with %d run(s) finished, want it to return only after one", got)
			}
			runs.Wait()
		})
	}
}

func TestServeWaitsForTheRequestlessCloseAfterItsHTTPShutdown(t *testing.T) {
	// Arrange: a served listener with one connection that sends nothing.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	gate := &recordingGate{Handler: http.NotFoundHandler()}
	var mu sync.Mutex
	var marks []string
	ctx, cancel := context.WithCancel(context.Background())
	served := make(chan error, 1)
	go func() {
		served <- serve(ctx, listener, gate, serveExit{Log: dlog.NewTestLogger(), Mark: func(step string, _ time.Time) {
			mu.Lock()
			defer mu.Unlock()
			marks = append(marks, step)
		}})
	}()
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

	// Assert: the wait is marked right after the http shutdown's own mark.
	mu.Lock()
	defer mu.Unlock()
	at := slices.Index(marks, "serve_requestless_closed")
	if at < 1 || !strings.HasPrefix(marks[at-1], "serve_http_shutdown(") {
		t.Fatalf("marks = %q, want serve_requestless_closed right after serve_http_shutdown", marks)
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
	conns := &connStates{byConn: map[net.Conn]http.ConnState{}, born: map[net.Conn]time.Time{}, log: dlog.NewTestLogger()}
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

func (o *orderingClaim) Withdraw() (daemonaddr.Withdrawal, error) {
	o.steps = append(o.steps, "withdraw")
	return daemonaddr.Withdrawal{Withdrawn: true}, nil
}

// orderingStopper records the shared listener's stop into the claim's steps.
type orderingStopper struct {
	claim *orderingClaim
	err   error
}

func (s *orderingStopper) StopShared() error {
	s.claim.steps = append(s.claim.steps, "stop-shared")
	return s.err
}

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
	beginShutdown(claim, &orderingStopper{claim: claim}, withdrawal, &spyLogger{})()

	// Assert: the shared listener stops before the serving lock is released.
	if got, want := strings.Join(claim.steps, ","), "stop-shared,depart,withdraw"; got != want {
		t.Fatalf("the shutdown's first steps = %s, want %s", got, want)
	}
}

func TestBeginShutdownReportsAFailedDepartureAndStillWithdraws(t *testing.T) {
	// Arrange.
	claim := &orderingClaim{departErr: errors.New("flock: bad file descriptor")}
	log := &spyLogger{}
	withdrawal := &addrWithdrawal{claim: claim, log: &spyLogger{}}

	// Act.
	beginShutdown(claim, &orderingStopper{claim: claim}, withdrawal, log)()

	// Assert.
	msg, ok := log.message(dlog.LevelError)
	if !ok || !strings.Contains(msg, "serving lock") {
		t.Fatalf("records = %+v, want an ERROR naming the serving lock", log.Records())
	}
	if got, want := strings.Join(claim.steps, ","), "stop-shared,depart,withdraw"; got != want {
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
	got := run(context.Background(), options{stateDir: root, holder: testHolder(t)}, h)

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
func TestTheBootMeasuresTheDaemonUntilItEnds(t *testing.T) {
	// Arrange
	root := shortRoot(t)
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	t.Setenv(wsm.EnvTestUnsyncedWrites, "1")
	th := newTestHooks()

	// Act
	_ = run(context.Background(), options{stateDir: root, holder: testHolder(t)}, th.hooks)

	// Assert
	got := th.windows.Windows()
	if len(got) == 0 || got[0].Restart != restarttiming.DaemonBoot || got[0].Phase != "sessions_up" || got[0].Bound != bootWindowBound ||
		len(got[0].Watch) != 1 || got[0].Watch[0].Process != restarttiming.ProcessDaemon || !got[0].Closed {
		t.Fatalf("windows = %+v, want the boot's daemon window first, closed once the boot ended", got)
	}
}

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
			_ = run(context.Background(), options{stateDir: root, joining: tc.joining, holder: testHolder(t)}, th.hooks)

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
	r := restarttiming.NewReconnects(&rec, restarttiming.NoWindows{}, func() time.Time { return started.Add(2 * time.Second) })
	db := fakeWorkspaceList{workspaces: []wsm.Workspace{
		{ID: "open"}, {ID: "backlogged", Backlogged: true}, {ID: "closed", Closed: true},
	}}

	// Act
	err := expectBootReconnects(context.Background(), db, r, started)
	for _, ws := range []ids.WorkspaceID{"open", "backlogged", "closed"} {
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
	r := restarttiming.NewReconnects(&restarttimingtest.Recorder{}, restarttiming.NoWindows{}, time.Now)

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

// TestServeWaitsForTheCreatesBeforeAnyAnswerOrStreamIsEnded pins where the
// exit's wait for creates sits: after the withdrawal, and before the grace
// waits and the stream ends, so a create's outcome still reaches its client.
func TestServeWaitsForTheCreatesBeforeAnyAnswerOrStreamIsEnded(t *testing.T) {
	// Arrange.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	gate := &recordingGate{Handler: http.NotFoundHandler()}
	record := func(step string) func() {
		return func() {
			gate.mu.Lock()
			gate.calls = append(gate.calls, step)
			gate.mu.Unlock()
		}
	}
	ctx, cancel := context.WithCancel(context.Background())

	// Act.
	served := make(chan error, 1)
	go func() {
		served <- serve(ctx, listener, gate, serveExit{
			OnShuttingDown: record("withdraw"),
			AwaitCreates:   record("await_creates"),
			Log:            dlog.NewTestLogger(),
		})
	}()
	cancel()
	if err := <-served; err != nil {
		t.Fatalf("serve() = %v, want an orderly shutdown", err)
	}

	// Assert.
	got := gate.recorded()
	want := []string{"withdraw", "await_creates", "AwaitQuiet", "AwaitPushesSent"}
	if !slices.Equal(got[:min(len(got), len(want))], want) {
		t.Fatalf("the exit's steps = %v, want %v first", got, want)
	}
}

// exitListener hands out connections a test can close all at once, standing
// in for the process exit that follows serve's return.
type exitListener struct {
	net.Listener
	mu    sync.Mutex
	conns []net.Conn
}

func (l *exitListener) Accept() (net.Conn, error) {
	conn, err := l.Listener.Accept()
	if err != nil {
		return nil, err
	}
	l.mu.Lock()
	l.conns = append(l.conns, conn)
	l.mu.Unlock()
	return conn, nil
}

// exit closes every connection, as the process exit does.
func (l *exitListener) exit() {
	l.mu.Lock()
	defer l.mu.Unlock()
	for _, conn := range l.conns {
		_ = conn.Close()
	}
}

// TestACallReachingAnOpenConnectionAfterTheExitsWaitsIsAnswered reproduces
// the merge-gate failure of
// `TestARestartAskedOfAMovedWorkspaceIsForwardedToTheSuccessor`: a call
// that reached the daemon on an h2c connection the client already held,
// after the exit's waits had ended, was admitted and then cut by the process
// exit, so the caller read `unexpected EOF` instead of its answer. The call
// is made from inside the exit, once its last wait before the http shutdown
// has ended, and parks in its handler; it is released only once the client
// has been sent the GOAWAY, which is the exit saying it waits for what it
// admitted. An exit that returns first is the cut.
func TestACallReachingAnOpenConnectionAfterTheExitsWaitsIsAnswered(t *testing.T) {
	// Arrange: an h2 connection the server already holds, its first call
	// answered.
	tcp, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen: %v", err)
	}
	listener := &exitListener{Listener: tcp}
	entered := make(chan struct{})
	release := make(chan struct{})
	log := dlog.NewTestLogger()
	gate := server.H2C(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/late" {
			close(entered)
			<-release
		}
		w.WriteHeader(http.StatusOK)
	}), log)
	ctx, cancel := context.WithCancel(context.Background())
	conn := (*servertest.Conn)(nil)
	mark := func(step string, _ time.Time) {
		if step != "serve_end_streams" {
			return
		}
		conn.Request(t, 3, "/late")
		<-entered
	}
	served := make(chan error, 1)
	go func() { served <- serve(ctx, listener, gate, serveExit{Mark: mark, Log: log}) }()
	conn = servertest.Dial(t, tcp.Addr().String())
	conn.Request(t, 1, "/first")
	if _, err := conn.Await(servertest.Ended(1)); err != nil {
		t.Fatalf("the first call was never answered: %v", err)
	}

	// Act
	cancel()
	sawGoAway := make(chan servertest.Frame, 1)
	go func() {
		goAway, _ := conn.Await(servertest.GoAway)
		sawGoAway <- goAway
	}()
	var goAway servertest.Frame
	select {
	case serveErr := <-served:
		listener.exit()
		close(release)
		t.Fatalf("serve() = %v returned with the late call parked in its handler and no GOAWAY sent (%+v): the exit cuts what it admitted", serveErr, <-sawGoAway)
	case goAway = <-sawGoAway:
	}
	close(release)
	answer, answerErr := conn.Await(servertest.Ended(3))
	serveErr := <-served
	listener.exit()

	// Assert
	if serveErr != nil || goAway.LastStream != 3 || answerErr != nil {
		t.Fatalf("serve() = %v, GOAWAY = %+v, answer = %+v (%v); want an orderly exit whose GOAWAY admits the late call and which answers it before returning", serveErr, goAway, answer, answerErr)
	}
}

func TestConnStatesRecordsOnlyAConnectionThatClosedWithNoRequest(t *testing.T) {
	tests := []struct {
		name   string
		states []http.ConnState
		// addressless makes the connection one with no remote address, as
		// net.FileConn makes a handed-over one whose client reset.
		addressless bool
		want        bool
	}{
		{name: "closed with no request", states: []http.ConnState{http.StateNew, http.StateClosed}, want: true},
		{name: "closed with no request and no remote address", states: []http.ConnState{http.StateNew, http.StateClosed}, addressless: true, want: true},
		{name: "closed after a request", states: []http.ConnState{http.StateNew, http.StateActive, http.StateIdle, http.StateClosed}},
		{name: "hijacked for h2c", states: []http.ConnState{http.StateNew, http.StateActive, http.StateHijacked}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			pipe, peer := net.Pipe()
			defer pipe.Close()
			defer peer.Close()
			var side net.Conn = pipe
			if tt.addressless {
				side = addresslessConn{pipe}
			}
			log := dlog.NewTestLogger()
			conns := &connStates{byConn: map[net.Conn]http.ConnState{}, born: map[net.Conn]time.Time{}, log: log}

			// Act
			for _, state := range tt.states {
				conns.track(side, state)
			}

			// Assert
			record, got := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.server.conn_requestless"})
			if got != tt.want {
				t.Fatalf("recorded = %v, want %v (records %v)", got, tt.want, log.Records())
			}
			if tt.addressless && record.Context["remote"] != "unknown" {
				t.Fatalf("recorded remote %v, want unknown", record.Context["remote"])
			}
			if len(conns.byConn) != 0 || len(conns.born) != 0 {
				t.Fatalf("still tracked %v / %v, want the closed connection forgotten", conns.byConn, conns.born)
			}
		})
	}
}

// addresslessConn is a connection with no remote address.
type addresslessConn struct{ net.Conn }

func (addresslessConn) RemoteAddr() net.Addr { return nil }

func TestBeginShutdownReportsAFailedSharedStopAndStillDeparts(t *testing.T) {
	// Arrange.
	claim := &orderingClaim{}
	log := &spyLogger{}
	withdrawal := &addrWithdrawal{claim: claim, log: &spyLogger{}}

	// Act.
	beginShutdown(claim, &orderingStopper{claim: claim, err: errors.New("close: bad file descriptor")}, withdrawal, log)()

	// Assert.
	msg, ok := log.message(dlog.LevelError)
	if !ok || !strings.Contains(msg, "shared listener") {
		t.Fatalf("records = %+v, want an ERROR naming the shared listener", log.Records())
	}
	if got, want := strings.Join(claim.steps, ","), "stop-shared,depart,withdraw"; got != want {
		t.Fatalf("the shutdown's first steps = %s, want %s", got, want)
	}
}

func TestEveryHTTP1AnswerClosesItsConnection(t *testing.T) {
	cases := []struct {
		name       string
		protoMajor int
		wantClose  bool
	}{
		{name: "an HTTP/1.1 answer closes its connection", protoMajor: 1, wantClose: true},
		{name: "an HTTP/2 answer leaves its connection to its GOAWAY", protoMajor: 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			served := false
			h := closingHTTP1(http.HandlerFunc(func(http.ResponseWriter, *http.Request) { served = true }))
			req := httptest.NewRequest(http.MethodPost, "/x", nil)
			req.ProtoMajor = tc.protoMajor
			rec := httptest.NewRecorder()

			// Act
			h.ServeHTTP(rec, req)

			// Assert
			if !served || (rec.Header().Get("Connection") == "close") != tc.wantClose {
				t.Fatalf("served %v, Connection %q; want the handler run and close %v", served, rec.Header().Get("Connection"), tc.wantClose)
			}
		})
	}
}

// TestCloseRosterChangersRunsItsStepsInOrder pins the 2026-11-25 exit that
// wrote a roster status edge against the closed state client: the roster
// settles after the banners close and before its durable writers do, and a
// step the graph left nil is skipped as its neighbors are.
func TestCloseRosterChangersRunsItsStepsInOrder(t *testing.T) {
	tests := []struct {
		name    string
		nilStep string
		want    string
	}{
		{name: "the roster settles after the banners and before its writes close", want: "close_watchers close_banners settle_roster close_roster_writes"},
		{name: "a nil settle is skipped", nilStep: "settle_roster", want: "close_watchers close_banners close_roster_writes"},
		{name: "a nil banner close is skipped", nilStep: "close_banners", want: "close_watchers settle_roster close_roster_writes"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var ran []string
			record := func(name string) func() {
				if name == tt.nilStep {
					return nil
				}
				return func() { ran = append(ran, name) }
			}
			built := &graph{
				CloseWatchers:     record("close_watchers"),
				CloseBanners:      record("close_banners"),
				SettleRoster:      record("settle_roster"),
				CloseRosterWrites: record("close_roster_writes"),
			}
			teardown := newTeardownClock(dlog.NewTestLogger())

			// Act
			closeRosterChangers(teardown, built)

			// Assert
			if got := strings.Join(ran, " "); got != tt.want {
				t.Fatalf("ran %q, want %q", got, tt.want)
			}
			var stepped []string
			for _, s := range teardown.steps {
				stepped = append(stepped, s.name)
			}
			if got := strings.Join(stepped, " "); got != tt.want {
				t.Fatalf("teardown steps %q, want %q", got, tt.want)
			}
		})
	}
}
