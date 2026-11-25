package deploy

import (
	"agent-repld/internal/tempdirs/tempdirstest"
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/logging/buildreport"

	"agent-repld/internal/dlog"
	"agent-repld/internal/holderensure"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
)

// fakeServiceManager scripts the service manager. NO TEST EVER REACHES A LIVE
// SERVICE MANAGER: this is the whole of this package's contact with one.
type fakeServiceManager struct {
	mu    sync.Mutex
	calls []string
	// dir is where Definition says each service's definition is installed.
	dir string
	// loaded is what Status answers per label.
	loaded map[string]bool
	pid    map[string]int
	// onStopped runs when a service is stopped: the harness ends its
	// process there, as the manager's stop does.
	onStopped func(label string)
	// onRestart runs when the store is restarted (a test binds the socket,
	// or grows the log, or kills the process there).
	onRestart func()
	// statusesUntilGone is how many Statuses of the sidecar still find it
	// held after its stop.
	statusesUntilGone int
	// onStoreStatus runs on every Status of the store while it boots.
	onStoreStatus func(n int)
	storeStatuses int
	restartErr    error
	stopErr       error
	startErr      error
	statusErr     error
	// onStart runs when a service is started, with its label.
	onStart func(label string)
}

func newFakeServiceManager(dir string) *fakeServiceManager {
	return &fakeServiceManager{
		dir:    dir,
		loaded: map[string]bool{StoreLabel: true, SidecarLabel: true, ZettelLabel: true, TestsLabel: true},
		pid:    map[string]int{StoreLabel: 10, SidecarLabel: 20},
	}
}

func (l *fakeServiceManager) record(call string) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.calls = append(l.calls, call)
}

func (l *fakeServiceManager) Calls() []string {
	l.mu.Lock()
	defer l.mu.Unlock()
	return append([]string(nil), l.calls...)
}

func (l *fakeServiceManager) Status(_ context.Context, label string) (bool, int, error) {
	l.record("status " + label)
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.statusErr != nil {
		return false, 0, l.statusErr
	}
	if label == SidecarLabel && !l.loaded[label] && l.statusesUntilGone > 0 {
		l.statusesUntilGone--
		return true, 0, nil
	}
	if label == StoreLabel && l.onStoreStatus != nil {
		l.storeStatuses++
		n := l.storeStatuses
		l.mu.Unlock()
		l.onStoreStatus(n)
		l.mu.Lock()
	}
	return l.loaded[label], l.pid[label], nil
}

func (l *fakeServiceManager) Restart(_ context.Context, label string) error {
	l.record("restart " + label)
	if l.restartErr != nil {
		return l.restartErr
	}
	if label == StoreLabel && l.onRestart != nil {
		l.onRestart()
	}
	return nil
}

func (l *fakeServiceManager) Stop(_ context.Context, label string) error {
	l.record("stop " + label)
	l.mu.Lock()
	l.loaded[label] = false
	stopped, err := l.onStopped, l.stopErr
	l.mu.Unlock()
	if stopped != nil {
		stopped(label)
	}
	return err
}

func (l *fakeServiceManager) Start(_ context.Context, label string) error {
	l.record("start " + label)
	if l.startErr != nil {
		return l.startErr
	}
	l.mu.Lock()
	l.loaded[label] = true
	l.mu.Unlock()
	if l.onStart != nil {
		l.onStart(label)
	}
	// A STARTED STORE BOOTS as a restarted one does: the test's hook binds
	// its socket.
	if label == StoreLabel && l.onRestart != nil {
		l.onRestart()
	}
	return nil
}

func (l *fakeServiceManager) Definition(label string) string {
	return filepath.Join(l.dir, label+".plist")
}

// restarterHarness is one Restarter over a temp socket dir and plist dir.
type restarterHarness struct {
	restarts *restarttimingtest.Recorder
	windows  restarttimingtest.Windows
	r        *Restarter
	services *fakeServiceManager
	log      *dlog.TestLogger
	sock     string
	alive    map[int]bool
	clock    *boundClock
	// exits holds each pid's exit: closed once the process exited. A pid
	// with no entry has already exited.
	exitsMu sync.Mutex
	exits   map[int]chan struct{}
	// awaiting receives each pid ProcessExit begins waiting on.
	awaiting chan int
	// defineErr is what Define answers; nil writes the definition.
	defineErr error
	// defineWritesNothing makes a successful Define leave no definition.
	defineWritesNothing bool
	// definition is what a successful Define writes; "" writes the
	// definition every service is installed with, so a restart finds it
	// unchanged.
	definition string
	// holderErr is what EnsureHolder answers; ensuredHolders records each
	// holder it was asked for.
	holderErr      error
	ensuredHolders []string
	// holderDown makes HolderServing find no holder serving; probeErr is
	// what it answers.
	holderDown bool
	probeErr   error
	// testRuns is the merge gates' hold the test service's restart takes;
	// testsSock is the test service's socket, bound from the start (a
	// serving service); answerErr is what its answer probe says, and answers
	// counts the probes.
	testRuns  *fakeTestRuns
	testsSock string
	answerMu  sync.Mutex
	answerErr error
	answers   int
}

// fakeTestRuns is the merge orchestrator's hold of the gates across the test
// service's restart. It records how many of the service manager's acts had
// been made at each take-off and release, and what each release was told.
type fakeTestRuns struct {
	mu       sync.Mutex
	services *fakeServiceManager
	err      error
	// takenAt and releasedAt are the count of acts at each take-off and each
	// release; answered is each release's argument.
	takenAt, releasedAt []int
	answered            []error
}

func (f *fakeTestRuns) TakeTestRunsOff(context.Context) (func(error), error) {
	acts := len(actsOf(f.services))
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.err != nil {
		return nil, f.err
	}
	f.takenAt = append(f.takenAt, acts)
	return func(answered error) {
		acts := len(actsOf(f.services))
		f.mu.Lock()
		defer f.mu.Unlock()
		f.releasedAt = append(f.releasedAt, acts)
		f.answered = append(f.answered, answered)
	}, nil
}

// testsAnswer is the harness's TestsAnswer: it counts the probe and answers
// answerErr.
func (h *restarterHarness) testsAnswer(context.Context) error {
	h.answerMu.Lock()
	defer h.answerMu.Unlock()
	h.answers++
	return h.answerErr
}

// probes answers how many times the test service was asked to answer.
func (h *restarterHarness) probes() int {
	h.answerMu.Lock()
	defer h.answerMu.Unlock()
	return h.answers
}

// ensureHolder is the harness's EnsureHolder: it records "ensure-holder <id>"
// among the service manager's calls.
func (h *restarterHarness) ensureHolder(_ context.Context, id string) error {
	h.services.record("ensure-holder " + id)
	h.ensuredHolders = append(h.ensuredHolders, id)
	return h.holderErr
}

// define is the harness's Define: it records "define <label>" among the
// service manager's calls and writes the label's plist.
func (h *restarterHarness) define(_ context.Context, label string) error {
	h.services.record("define " + label)
	if h.defineErr != nil {
		return h.defineErr
	}
	if h.defineWritesNothing {
		return nil
	}
	body := h.definition
	if body == "" {
		body = "<plist/>"
	}
	return os.WriteFile(filepath.Join(h.services.dir, label+".plist"), []byte(body), 0o644)
}

// processExit is the harness's ProcessExit: it waits on the pid's entry in
// exits, or answers at once for a pid that has none.
func (h *restarterHarness) processExit(ctx context.Context, pid int) error {
	h.exitsMu.Lock()
	exit, alive := h.exits[pid]
	h.exitsMu.Unlock()
	select {
	case h.awaiting <- pid:
	default:
	}
	if !alive {
		return nil
	}
	select {
	case <-exit:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// exit ends pid's process.
func (h *restarterHarness) exit(pid int) {
	h.exitsMu.Lock()
	defer h.exitsMu.Unlock()
	select {
	case <-h.exits[pid]:
		// Already exited: a process exits once.
	default:
		close(h.exits[pid])
	}
}

func newRestarter(t *testing.T) *restarterHarness {
	t.Helper()
	// A unix socket path must stay under the 104-byte sun_path budget, which
	// the per-test temp dir on macOS overflows.
	base, err := tempdirstest.ShortBase(os.Getenv)
	if err != nil {
		t.Fatal(err)
	}
	sockDir, err := os.MkdirTemp(base, "dpl")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := os.RemoveAll(sockDir); err != nil {
			t.Errorf("remove %s: %v", sockDir, err)
		}
	})
	plists := t.TempDir()
	// The store and the sidecar are installed with their definitions; a test
	// that needs a standalone's writes it.
	for _, label := range []string{StoreLabel, SidecarLabel} {
		writeFile(t, filepath.Join(plists, label+".plist"), "<plist/>")
	}
	log := dlog.NewTestLogger()
	h := &restarterHarness{services: newFakeServiceManager(plists), log: log, sock: filepath.Join(sockDir, "store.sock"), alive: map[int]bool{},
		restarts: &restarttimingtest.Recorder{}, clock: newBoundClock(),
		// The store runs until a test ends it; the sidecar's bootout ends it.
		exits: map[int]chan struct{}{10: make(chan struct{})}, awaiting: make(chan int, 8)}
	// A STOPPED STORE'S PROCESS EXITS, as launchd's bootout ends it.
	h.services.onStopped = func(label string) {
		if label == StoreLabel {
			h.exit(10)
		}
	}
	h.r = &Restarter{
		CacheBin:        t.TempDir(),
		ReportDir:       t.TempDir(),
		Alive:           func(pid int) bool { return h.alive[pid] },
		Services:        h.services,
		StoreSocket:     h.sock,
		StoreLog:        filepath.Join(t.TempDir(), "shim-store.err.log"),
		Windows:         DefaultServiceWindows,
		Clock:           h.clock,
		Log:             log,
		Restarts:        h.restarts,
		ResourceWindows: &h.windows,
		ProcessExit:     h.processExit,
		Define:          h.define,
		EnsureHolder:    h.ensureHolder,
		HolderServing: func(context.Context, string) (bool, error) {
			return !h.holderDown, h.probeErr
		},
		StoreHolder: "store-holder",
		TestsSocket: filepath.Join(sockDir, "tests.sock"),
		TestsAnswer: h.testsAnswer,
	}
	h.testsSock = h.r.TestsSocket
	h.testRuns = &fakeTestRuns{services: h.services}
	h.r.TestRuns = h.testRuns
	listen(t, h.testsSock)
	return h
}

// listen stands a real unix socket at path for the test's length, as a
// serving service binds its own.
func listen(t *testing.T, path string) {
	t.Helper()
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("bind %s: %v", path, err)
	}
	t.Cleanup(func() {
		if err := ln.Close(); err != nil {
			t.Errorf("close the test socket %s: %v", path, err)
		}
	})
}

// bindSocket stands a real unix socket at the store's path, as the new store
// does once it serves.
func (h *restarterHarness) bindSocket(t *testing.T) {
	t.Helper()
	ln, err := net.Listen("unix", h.sock)
	if err != nil {
		t.Fatalf("bind %s: %v", h.sock, err)
	}
	t.Cleanup(func() {
		if err := ln.Close(); err != nil {
			t.Errorf("close the test socket: %v", err)
		}
	})
}

func loggedTo(log *dlog.TestLogger, level, substr string) bool {
	for _, r := range log.Records() {
		if r.Level == level && strings.Contains(r.Message, substr) {
			return true
		}
	}
	return false
}

func TestRestartStoreSettlesItsHolderAndDefinitionBeforeAnythingStops(t *testing.T) {
	tests := []struct {
		name       string
		arrange    func(h *restarterHarness)
		wantActs   []string
		wantErr    bool
		wantRecord string
	}{
		{name: "a changed definition is stopped, then started, never kickstarted", arrange: func(h *restarterHarness) { h.definition = "<plist with --holder/>" },
			wantActs:   []string{"ensure-holder store-holder", "define " + StoreLabel, "stop " + SidecarLabel, "stop " + StoreLabel, "start " + StoreLabel, "start " + SidecarLabel},
			wantRecord: "a service's definition changed since it was loaded; it is reloaded on it"},
		{name: "a holder that cannot be brought up stops nothing", arrange: func(h *restarterHarness) { h.holderErr = errors.New("install failed") },
			wantActs: []string{"ensure-holder store-holder"}, wantErr: true},
		{name: "a definition that cannot be written stops nothing", arrange: func(h *restarterHarness) { h.defineErr = errors.New("define-service exited 1") },
			wantActs: []string{"ensure-holder store-holder", "define " + StoreLabel}, wantErr: true,
			wantRecord: "a service's definition could not be written from its requirements entry; nothing was restarted"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			h.services.onRestart = func() { h.bindSocket(t) }
			tt.arrange(h)

			// Act
			err := h.r.RestartStore(context.Background())

			// Assert
			if (err != nil) != tt.wantErr || fmt.Sprint(h.acts()) != fmt.Sprint(tt.wantActs) {
				t.Fatalf("RestartStore = %v, acts %v; want error %v, acts %v", err, h.acts(), tt.wantErr, tt.wantActs)
			}
			if tt.wantRecord != "" {
				if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Operation: opServices, Message: tt.wantRecord}); !ok {
					t.Fatalf("records = %+v, want %q", h.log.Records(), tt.wantRecord)
				}
			}
		})
	}
}

func TestAStoreAtASocketNoHolderHoldsEnsuresNoHolder(t *testing.T) {
	// Arrange: no declared holder holds the store's socket.
	h := newRestarter(t)
	h.r.StoreHolder = ""
	h.services.onRestart = func() { h.bindSocket(t) }

	// Act
	err := h.r.RestartStore(context.Background())

	// Assert
	want := []string{"define " + StoreLabel, "stop " + SidecarLabel, "restart " + StoreLabel, "start " + SidecarLabel}
	if err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) || len(h.ensuredHolders) != 0 {
		t.Fatalf("RestartStore = %v, acts %v, holders %v; want %v and no holder ensured", err, h.acts(), h.ensuredHolders, want)
	}
}

func TestTheBootMovesAStoreOffItsHolderOntoIt(t *testing.T) {
	tests := []struct {
		name     string
		down     bool
		probeErr error
		wantActs []string
		wantErr  bool
	}{
		{name: "a store whose holder is not up is reloaded onto it", down: true,
			wantActs: []string{"ensure-holder store-holder", "define " + StoreLabel, "stop " + SidecarLabel, "stop " + StoreLabel, "start " + StoreLabel, "start " + SidecarLabel}},
		{name: "a store whose holder serves is left alone"},
		{name: "a probe that fails stops the boot's step and restarts nothing", probeErr: errors.New("probe hung"),
			wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the store and the sidecar run the installed build.
			h := newRestarter(t)
			h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
			h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "new sidecar", "new sidecar")
			h.services.onRestart = func() { h.bindSocket(t) }
			h.holderDown, h.probeErr = tt.down, tt.probeErr

			// Act
			_, err := h.r.EnsureStoreCurrent(context.Background())

			// Assert
			if (err != nil) != tt.wantErr || fmt.Sprint(h.acts()) != fmt.Sprint(tt.wantActs) {
				t.Fatalf("EnsureStoreCurrent = %v, acts %v; want error %v, acts %v", err, h.acts(), tt.wantErr, tt.wantActs)
			}
		})
	}
}

func TestAStoreStartedFromNothingHasItsHolderEnsuredFirst(t *testing.T) {
	// Arrange: the store is unloaded with no definition.
	h := newRestarter(t)
	h.services.loaded[StoreLabel] = false
	if err := os.Remove(filepath.Join(h.services.dir, StoreLabel+".plist")); err != nil {
		t.Fatal(err)
	}
	h.services.onStart = func(label string) {
		if label == StoreLabel {
			h.bindSocket(t)
		}
	}

	// Act
	_ = h.r.EnsureLoaded(context.Background())

	// Assert
	acts := h.acts()
	at := func(act string) int { return slices.Index(acts, act) }
	if at("ensure-holder store-holder") < 0 || at("start "+StoreLabel) < 0 || at("ensure-holder store-holder") > at("start "+StoreLabel) {
		t.Fatalf("acts %v, want the store's holder ensured before the store starts", acts)
	}
}

func TestRestartStoreTakesTheRecordedSafeOrder(t *testing.T) {
	// Arrange: the new store binds its socket as soon as it is kickstarted.
	h := newRestarter(t)
	h.services.onRestart = func() { h.bindSocket(t) }

	// Act
	err := h.r.RestartStore(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("RestartStore: %v", err)
	}
	var acts []string
	for _, c := range h.services.Calls() {
		if !strings.HasPrefix(c, "status") {
			acts = append(acts, c)
		}
	}
	want := []string{"ensure-holder store-holder", "define " + StoreLabel, "stop " + SidecarLabel, "restart " + StoreLabel, "start " + SidecarLabel}
	if strings.Join(acts, ",") != strings.Join(want, ",") {
		t.Fatalf("launchd acts = %v, want %v", acts, want)
	}
}

func TestRestartStoreKickstartsTheStoreOnlyOnceTheSidecarExited(t *testing.T) {
	// Arrange: the sidecar's process outlives its bootout until the test ends
	// it.
	h := newRestarter(t)
	h.exits[20] = make(chan struct{})
	h.services.onRestart = func() { h.bindSocket(t) }
	done := make(chan error, 1)

	// Act
	go func() { done <- h.r.RestartStore(context.Background()) }()
	for pid := range h.awaiting {
		if pid == 20 {
			break
		}
	}
	before := h.services.Calls()
	h.exit(20)
	err := <-done

	// Assert
	if err != nil {
		t.Fatalf("RestartStore: %v", err)
	}
	if slices.Contains(before, "restart "+StoreLabel) {
		t.Fatalf("calls before the sidecar exited = %v, want no store kickstart yet", before)
	}
	if !slices.Contains(h.services.Calls(), "restart "+StoreLabel) {
		t.Fatalf("calls = %v, want the store kickstarted once the sidecar exited", h.services.Calls())
	}
}

func TestRestartStoreRefusesASidecarThatWillNotLeave(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *restarterHarness)
		want    string
	}{
		{name: "a sidecar whose process outlives the upper bound", want: "did not exit within the upper bound",
			arrange: func(h *restarterHarness) {
				h.exits[20] = make(chan struct{})
				h.clock.fire(DefaultServiceWindows.Max)
			}},
		{name: "a sidecar launchd still holds once its process exited", want: "the service manager still holds the sidecar",
			arrange: func(h *restarterHarness) { h.services.statusesUntilGone = 1 }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			tc.arrange(h)

			// Act
			err := h.r.RestartStore(context.Background())

			// Assert
			if err == nil {
				t.Fatalf("RestartStore succeeded with a sidecar that never left")
			}
			if slices.Contains(h.services.Calls(), "restart "+StoreLabel) {
				t.Fatalf("calls = %v, want the store left alone", h.services.Calls())
			}
			if !loggedTo(h.log, "error", tc.want) {
				t.Fatalf("records = %+v, want an ERROR naming %q", h.log.Records(), tc.want)
			}
		})
	}
}

func TestRestartStoreRefusesWithoutTheSidecarPlist(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	if err := os.Remove(filepath.Join(h.services.dir, SidecarLabel+".plist")); err != nil {
		t.Fatal(err)
	}

	// Act
	err := h.r.RestartStore(context.Background())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "install.sh") {
		t.Fatalf("RestartStore = %v, want the missing plist named with its remedy", err)
	}
	if calls := h.services.Calls(); len(calls) != 0 {
		t.Fatalf("launchd calls = %v, want nothing touched", calls)
	}
	if !loggedTo(h.log, "error", "the sidecar's service definition is missing") {
		t.Fatalf("records = %+v, want the refusal at ERROR", h.log.Records())
	}
}

func TestAStoreBootFailureNeverBringsTheSidecarBack(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *restarterHarness)
		want    string
	}{
		{
			name: "the store is gone before its socket appears",
			arrange: func(h *restarterHarness) {
				h.services.onRestart = func() {
					h.services.mu.Lock()
					h.services.pid[StoreLabel] = 0
					h.services.mu.Unlock()
				}
			},
			want: "died before its socket appeared",
		},
		{
			name:    "the store exits before its socket appears",
			arrange: func(h *restarterHarness) { h.services.onRestart = func() { h.exit(10) } },
			want:    "died before its socket appeared",
		},
		{
			name:    "the store is wedged: alive, silent, no socket",
			arrange: func(h *restarterHarness) { h.clock.fire(DefaultServiceWindows.Stall) },
			want:    "wedged",
		},
		{
			name: "the store is past the upper bound",
			// Fired once the store is kickstarted: the sidecar's stop, before
			// it, is bounded by the same upper bound.
			arrange: func(h *restarterHarness) {
				h.services.onRestart = func() { h.clock.fire(DefaultServiceWindows.Max) }
			},
			want: "did not appear within the upper bound",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			tc.arrange(h)

			// Act
			err := h.r.RestartStore(context.Background())

			// Assert
			if err == nil {
				t.Fatalf("RestartStore succeeded with a store that never served")
			}
			for _, c := range h.services.Calls() {
				if strings.HasPrefix(c, "start") {
					t.Fatalf("calls = %v, want the sidecar left down", h.services.Calls())
				}
			}
			if !loggedTo(h.log, "error", tc.want) {
				t.Fatalf("records = %+v, want an ERROR naming %q", h.log.Records(), tc.want)
			}
		})
	}
}

func TestRestartStoreSurfacesALaunchdFailure(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *restarterHarness)
	}{
		{name: "the store kickstart fails", arrange: func(h *restarterHarness) { h.services.restartErr = errors.New("kickstart refused") }},
		{name: "the sidecar bootstrap fails", arrange: func(h *restarterHarness) {
			h.services.startErr = errors.New("bootstrap refused")
			h.services.onRestart = func() { h.bindSocket(t) }
		}},
		{name: "launchd cannot be read", arrange: func(h *restarterHarness) { h.services.statusErr = errors.New("print refused") }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			tc.arrange(h)

			// Act
			err := h.r.RestartStore(context.Background())

			// Assert
			if err == nil {
				t.Fatalf("RestartStore swallowed the launchd failure")
			}
			errors := 0
			for _, r := range h.log.Records() {
				if r.Level == "error" {
					errors++
				}
			}
			if errors == 0 {
				t.Fatalf("records = %+v, want the failure at ERROR", h.log.Records())
			}
		})
	}
}

func TestRestartSidecarKickstartsItAlone(t *testing.T) {
	// Arrange
	h := newRestarter(t)

	// Act
	err := h.r.RestartSidecar(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("RestartSidecar: %v", err)
	}
	if calls := h.acts(); fmt.Sprint(calls) != fmt.Sprint([]string{"define " + SidecarLabel, "restart " + SidecarLabel}) {
		t.Fatalf("calls = %v, want one sidecar kickstart", calls)
	}
}

func TestRestartSidecarSurfacesAFailedKickstart(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	h.services.restartErr = errors.New("kickstart refused")

	// Act
	err := h.r.RestartSidecar(context.Background())

	// Assert
	if err == nil || !loggedTo(h.log, "error", "the sidecar restart failed") {
		t.Fatalf("RestartSidecar = %v, records %+v; want the failure returned and at ERROR", err, h.log.Records())
	}
}

func TestEachServiceRestartIsTimedWithItsOutcome(t *testing.T) {
	cases := []struct {
		name        string
		arrange     func(t *testing.T, h *restarterHarness)
		restart     func(h *restarterHarness) error
		wantService string
		wantOutcome restarttiming.Outcome
	}{
		{
			name:        "a store restart that serves",
			arrange:     func(t *testing.T, h *restarterHarness) { h.services.onRestart = func() { h.bindSocket(t) } },
			restart:     func(h *restarterHarness) error { return h.r.RestartStore(context.Background()) },
			wantService: "store", wantOutcome: restarttiming.OK,
		},
		{
			name: "a store restart that fails",
			arrange: func(t *testing.T, h *restarterHarness) {
				if err := os.Remove(filepath.Join(h.services.dir, SidecarLabel+".plist")); err != nil {
					t.Fatal(err)
				}
			},
			restart:     func(h *restarterHarness) error { return h.r.RestartStore(context.Background()) },
			wantService: "store", wantOutcome: restarttiming.Failed,
		},
		{
			name:        "a sidecar restart",
			arrange:     func(*testing.T, *restarterHarness) {},
			restart:     func(h *restarterHarness) error { return h.r.RestartSidecar(context.Background()) },
			wantService: "sidecar", wantOutcome: restarttiming.OK,
		},
		{
			name:        "a sidecar restart that fails",
			arrange:     func(_ *testing.T, h *restarterHarness) { h.services.restartErr = errors.New("kickstart refused") },
			restart:     func(h *restarterHarness) error { return h.r.RestartSidecar(context.Background()) },
			wantService: "sidecar", wantOutcome: restarttiming.Failed,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			tc.arrange(t, h)

			// Act
			_ = tc.restart(h)

			// Assert
			got := h.restarts.Of(restarttiming.ServiceRestart)
			if len(got) != 1 || got[0].Phase() != tc.wantService || got[0].Outcome() != tc.wantOutcome {
				t.Fatalf("service restarts = %+v, want one %s restart, %s", got, tc.wantService, tc.wantOutcome)
			}
		})
	}
}

func TestEachServiceRestartMeasuresTheServicesItBringsBack(t *testing.T) {
	cases := []struct {
		name      string
		arrange   func(t *testing.T, h *restarterHarness)
		restart   func(h *restarterHarness) error
		wantPhase string
		wantWatch []restarttiming.Watched
		wantBound time.Duration
	}{
		{
			name:      "a store restart, which takes the sidecar with it",
			arrange:   func(t *testing.T, h *restarterHarness) { h.services.onRestart = func() { h.bindSocket(t) } },
			restart:   func(h *restarterHarness) error { return h.r.RestartStore(context.Background()) },
			wantPhase: "store", wantBound: 30 * time.Second,
			wantWatch: []restarttiming.Watched{{Process: restarttiming.ProcessStore}, {Process: restarttiming.ProcessSidecar}},
		},
		{
			name:      "a sidecar restart that fails",
			arrange:   func(_ *testing.T, h *restarterHarness) { h.services.restartErr = errors.New("kickstart refused") },
			restart:   func(h *restarterHarness) error { return h.r.RestartSidecar(context.Background()) },
			wantPhase: "sidecar", wantBound: 20 * time.Second,
			wantWatch: []restarttiming.Watched{{Process: restarttiming.ProcessSidecar}},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			tc.arrange(t, h)

			// Act
			_ = tc.restart(h)

			// Assert
			got := h.windows.Windows()
			if len(got) != 1 || got[0].Restart != restarttiming.ServiceRestart || got[0].Phase != tc.wantPhase ||
				!slices.Equal(got[0].Watch, tc.wantWatch) || got[0].Bound != tc.wantBound || !got[0].Closed {
				t.Fatalf("windows = %+v, want one closed %s window over %v", got, tc.wantPhase, tc.wantWatch)
			}
		})
	}
}

func TestRestartStoreBootoutAnswers(t *testing.T) {
	tests := []struct {
		name      string
		bootout   error
		wantLevel string
		wantMsg   string
	}{
		{
			name:      "a sidecar already gone at the bootout is recorded at INFO",
			bootout:   fmt.Errorf("%w: launchctl bootout exited 113", ErrServiceNotLoaded),
			wantLevel: "info",
			wantMsg:   "had already stopped when it was stopped",
		},
		{
			name:      "any other bootout failure is a WARN while launchd's view decides",
			bootout:   errors.New("deploy: launchctl bootout exited 5"),
			wantLevel: "warn",
			wantMsg:   "stop answered an error",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			h.services.stopErr = tc.bootout
			h.services.onRestart = func() { h.bindSocket(t) }

			// Act
			err := h.r.RestartStore(context.Background())

			// Assert
			if err != nil || !loggedTo(h.log, tc.wantLevel, tc.wantMsg) {
				t.Fatalf("RestartStore = %v, records %+v; want %s %q", err, h.log.Records(), tc.wantLevel, tc.wantMsg)
			}
		})
	}
}

func TestRestartStoreRecordsNoWarningWhenTheSidecarWasAlreadyGone(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	h.services.stopErr = fmt.Errorf("%w: launchctl bootout exited 113", ErrServiceNotLoaded)
	h.services.onRestart = func() { h.bindSocket(t) }

	// Act
	err := h.r.RestartStore(context.Background())

	// Assert
	for _, r := range h.log.Records() {
		if r.Level == "warn" || r.Level == "error" {
			t.Fatalf("RestartStore = %v, record %+v; want nothing above INFO", err, r)
		}
	}
}

// unload marks label as not held by launchd and writes its plist, so an
// EnsureLoaded has something to bootstrap it from.
func (h *restarterHarness) unload(t *testing.T, label string) {
	t.Helper()
	h.services.loaded[label] = false
	h.services.pid[label] = 0
	writeFile(t, filepath.Join(h.services.dir, label+".plist"), "<plist/>")
}

// bootstraps answers the plists EnsureLoaded bootstrapped, in order.
func (h *restarterHarness) bootstraps() []string {
	var out []string
	for _, c := range h.services.Calls() {
		if strings.HasPrefix(c, "start ") {
			out = append(out, strings.TrimPrefix(c, "start "))
		}
	}
	return out
}

func TestEnsureLoadedLeavesLoadedServicesAlone(t *testing.T) {
	// Arrange.
	h := newRestarter(t)

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("EnsureLoaded: %v", err)
	}
	if got := h.bootstraps(); len(got) != 0 {
		t.Fatalf("bootstrapped %v, want nothing: both services are loaded", got)
	}
}

func TestEnsureLoadedBootstrapsAnUnloadedStore(t *testing.T) {
	// Arrange: the bootstrapped store starts and binds its socket.
	h := newRestarter(t)
	h.unload(t, StoreLabel)
	h.services.onStart = func(string) {
		h.services.mu.Lock()
		h.services.loaded[StoreLabel], h.services.pid[StoreLabel] = true, 10
		h.services.mu.Unlock()
		h.bindSocket(t)
	}

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("EnsureLoaded: %v", err)
	}
	if got, want := h.bootstraps(), []string{StoreLabel}; fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("bootstrapped %v, want %v", got, want)
	}
}

func TestEnsureLoadedStartsTheSidecarOnlyOnceTheStoreServes(t *testing.T) {
	// Arrange: both are unloaded; the sidecar's bootstrap records whether the
	// store's socket was already up.
	h := newRestarter(t)
	h.unload(t, StoreLabel)
	h.unload(t, SidecarLabel)
	storeServedFirst := false
	h.services.onStart = func(label string) {
		switch label {
		case StoreLabel:
			h.services.mu.Lock()
			h.services.loaded[StoreLabel], h.services.pid[StoreLabel] = true, 10
			h.services.mu.Unlock()
			h.bindSocket(t)
		case SidecarLabel:
			storeServedFirst = socketExists(h.sock)
		}
	}

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("EnsureLoaded: %v", err)
	}
	if !storeServedFirst {
		t.Fatalf("the sidecar was bootstrapped before the store's socket was up (bootstraps %v)", h.bootstraps())
	}
}

func TestEnsureLoadedBootstrapsAnUnloadedSidecarWithoutTouchingTheStore(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.unload(t, SidecarLabel)

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("EnsureLoaded: %v", err)
	}
	if got, want := h.bootstraps(), []string{SidecarLabel}; fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("bootstrapped %v, want %v", got, want)
	}
}

// An unloaded service with no plist has its definition written before it is
// started (ensureLoaded); one whose definition cannot be written is refused,
// and nothing is started.
func TestEnsureLoadedRefusesAnUnloadedServiceWhosePlistCannotBeWritten(t *testing.T) {
	// Arrange: the store is unloaded with no definition.
	h := newRestarter(t)
	h.services.loaded[StoreLabel] = false
	if err := os.Remove(filepath.Join(h.services.dir, StoreLabel+".plist")); err != nil {
		t.Fatal(err)
	}
	h.defineErr = errors.New("define-service exited 1")

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "write the definition of "+StoreLabel) {
		t.Fatalf("EnsureLoaded = %v, want a refusal naming the definition write", err)
	}
	if slices.Contains(h.services.Calls(), "start "+StoreLabel) {
		t.Fatalf("calls = %v, want nothing started", h.services.Calls())
	}
}

func TestEnsureLoadedSurfacesABootstrapFailure(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.unload(t, StoreLabel)
	h.services.startErr = errors.New("launchctl bootstrap exited 5")

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err == nil || !errors.Is(err, h.services.startErr) {
		t.Fatalf("EnsureLoaded = %v, want the bootstrap failure", err)
	}
}

func TestEnsureLoadedSurfacesAnUnreadableLaunchdState(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.services.statusErr = errors.New("launchctl print exited 1")

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err == nil || !errors.Is(err, h.services.statusErr) {
		t.Fatalf("EnsureLoaded = %v, want the print failure", err)
	}
}

// running states that the service launchd runs as pid reports BUILD, and that
// the installed build is INSTALLED.
func (h *restarterHarness) running(t *testing.T, label, service string, pid int, build, installed string) {
	t.Helper()
	writeFile(t, filepath.Join(h.r.CacheBin, service), installed)
	if err := buildreport.Write(h.r.ReportDir, service, buildreport.Report{PID: pid, Build: hashOf(t, build)}); err != nil {
		t.Fatal(err)
	}
	h.alive[pid] = true
	h.services.pid[label] = pid
}

// acts answers every launchd call other than a look.
func (h *restarterHarness) acts() []string { return actsOf(h.services) }

// actsOf answers every call of SERVICES other than a look.
func actsOf(services *fakeServiceManager) []string {
	var out []string
	for _, c := range services.Calls() {
		if !strings.HasPrefix(c, "status") {
			out = append(out, c)
		}
	}
	return out
}

func TestEnsureCurrentRestartsAStaleStoreAndItsSidecarInTheSafeOrder(t *testing.T) {
	// Arrange: the running store is an older build than the installed one.
	h := newRestarter(t)
	h.running(t, StoreLabel, buildreport.ServiceStore, 10, "old store", "new store")
	h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "new sidecar", "new sidecar")
	h.services.onRestart = func() { h.bindSocket(t) }

	// Act
	err := h.r.EnsureCurrent(context.Background())

	// Assert
	want := []string{"ensure-holder store-holder", "define " + StoreLabel, "stop " + SidecarLabel, "restart " + StoreLabel, "start " + SidecarLabel}
	if err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
		t.Fatalf("EnsureCurrent = %v, acts %v; want %v", err, h.acts(), want)
	}
}

func TestEnsureCurrentLeavesFreshServicesAlone(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
	h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "new sidecar", "new sidecar")

	// Act
	err := h.r.EnsureCurrent(context.Background())

	// Assert
	if err != nil || len(h.acts()) != 0 {
		t.Fatalf("EnsureCurrent = %v, acts %v; want nothing touched", err, h.acts())
	}
}

func TestEnsureCurrentRestartsAStaleSidecarAlone(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
	h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "old sidecar", "new sidecar")

	// Act
	err := h.r.EnsureCurrent(context.Background())

	// Assert
	if want := []string{"define " + SidecarLabel, "restart " + SidecarLabel}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
		t.Fatalf("EnsureCurrent = %v, acts %v; want %v", err, h.acts(), want)
	}
}

func TestEnsureCurrentLeavesAServiceLaunchdRunsNoProcessFor(t *testing.T) {
	// Arrange: both loaded, neither running, no reports at all.
	h := newRestarter(t)
	h.services.pid[StoreLabel], h.services.pid[SidecarLabel] = 0, 0

	// Act
	err := h.r.EnsureCurrent(context.Background())

	// Assert
	if err != nil || len(h.acts()) != 0 {
		t.Fatalf("EnsureCurrent = %v, acts %v; want nothing touched", err, h.acts())
	}
}

func TestEnsureCurrentDoesNotRestartAStoreItJustBootstrapped(t *testing.T) {
	// Arrange: the store is unloaded and has never reported a build.
	h := newRestarter(t)
	h.unload(t, StoreLabel)
	h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "new sidecar", "new sidecar")
	h.services.onStart = func(label string) {
		if label == StoreLabel {
			h.services.mu.Lock()
			h.services.loaded[StoreLabel], h.services.pid[StoreLabel] = true, 10
			h.services.mu.Unlock()
			h.bindSocket(t)
		}
	}

	// Act
	err := h.r.EnsureCurrent(context.Background())

	// Assert
	if want := []string{"ensure-holder store-holder", "start " + StoreLabel}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
		t.Fatalf("EnsureCurrent = %v, acts %v; want %v", err, h.acts(), want)
	}
}

func TestEnsureCurrentSurfacesAnUnreadableInstalledBuild(t *testing.T) {
	// Arrange: the store runs, but nothing is installed to judge it against.
	h := newRestarter(t)
	h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
	if err := os.Remove(filepath.Join(h.r.CacheBin, buildreport.ServiceStore)); err != nil {
		t.Fatal(err)
	}

	// Act
	err := h.r.EnsureCurrent(context.Background())

	// Assert
	if err == nil || !loggedTo(h.log, "error", "installed build could not be read") || len(h.acts()) != 0 {
		t.Fatalf("EnsureCurrent = %v, acts %v, records %+v; want the error at ERROR and nothing touched", err, h.acts(), h.log.Records())
	}
}

// --- a service step its caller cancels is not a failure ----------------------

func levelsOf(log *dlog.TestLogger) []string {
	var out []string
	for _, r := range log.Records() {
		out = append(out, r.Level)
	}
	return out
}

func TestEnsureCurrentCutShortByItsCallerRecordsNoError(t *testing.T) {
	// Arrange: launchctl's print answers the cancellation its caller made.
	h := newRestarter(t)
	h.services.statusErr = fmt.Errorf("deploy: run launchctl print: %w", context.Canceled)

	// Act.
	_ = h.r.EnsureCurrent(context.Background())

	// Assert.
	if slices.Contains(levelsOf(h.log), "error") {
		t.Fatalf("record levels = %v, want no error for a step its caller cancelled", levelsOf(h.log))
	}
}

func TestEnsureCurrentCutShortByItsCallerRecordsTheCutAtInfo(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.services.statusErr = fmt.Errorf("deploy: run launchctl print: %w", context.Canceled)

	// Act.
	_ = h.r.EnsureCurrent(context.Background())

	// Assert.
	if !loggedTo(h.log, "info", "a service step was cut short by its caller") {
		t.Fatalf("records = %+v, want the cut recorded at info", h.log.Records())
	}
}

func TestEnsureCurrentCutShortByItsCallerAnswersTheCancellation(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.services.statusErr = fmt.Errorf("deploy: run launchctl print: %w", context.Canceled)

	// Act.
	err := h.r.EnsureCurrent(context.Background())

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("EnsureCurrent = %v, want the cancellation", err)
	}
}

func TestAServiceStepPastItsDeadlineIsStillAnError(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.services.statusErr = fmt.Errorf("deploy: run launchctl print: %w", context.DeadlineExceeded)

	// Act.
	_ = h.r.EnsureCurrent(context.Background())

	// Assert.
	if !loggedTo(h.log, "error", "could not read a service's service-manager state") {
		t.Fatalf("records = %+v, want the deadline recorded at error", h.log.Records())
	}
}

// stoppedClock is a Clock whose poll interval never elapses, so a wait ends
// only on its context.
type stoppedClock struct{}

func (stoppedClock) Now() time.Time                       { return instant }
func (stoppedClock) After(time.Duration) <-chan time.Time { return nil }

func TestAStoreSocketWaitItsCallerCancelsRecordsNoError(t *testing.T) {
	// Arrange: a bootstrapped store that runs and has not bound its socket,
	// awaited under a context its caller has cancelled.
	h := newRestarter(t)
	h.unload(t, StoreLabel)
	h.services.onStart = func(string) {
		h.services.mu.Lock()
		h.services.loaded[StoreLabel], h.services.pid[StoreLabel] = true, 10
		h.services.mu.Unlock()
	}
	h.r.Clock = stoppedClock{}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_ = h.r.EnsureLoaded(ctx)

	// Assert.
	if slices.Contains(levelsOf(h.log), "error") {
		t.Fatalf("record levels = %v, want no error for a wait its caller cancelled", levelsOf(h.log))
	}
}

func TestEnsureStoreCurrentLeavesAStaleSidecarForItsSecondHalf(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
	h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "old sidecar", "new sidecar")

	// Act
	_, err := h.r.EnsureStoreCurrent(context.Background())

	// Assert
	if err != nil || len(h.acts()) != 0 {
		t.Fatalf("EnsureStoreCurrent = %v, acts %v; want the sidecar untouched until its half runs", err, h.acts())
	}
}

func TestEnsureStoreCurrentsSecondHalfRestartsAStaleSidecar(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
	h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "old sidecar", "new sidecar")
	sidecar, err := h.r.EnsureStoreCurrent(context.Background())
	if err != nil {
		t.Fatal(err)
	}

	// Act
	err = sidecar(context.Background())

	// Assert
	if want := []string{"define " + SidecarLabel, "restart " + SidecarLabel}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
		t.Fatalf("second half = %v, acts %v; want %v", err, h.acts(), want)
	}
}

func TestEnsureStoreCurrentsSecondHalfIsANoOpAfterAStoreRestart(t *testing.T) {
	// Arrange: a stale store takes the sidecar with it in the first half.
	h := newRestarter(t)
	h.running(t, StoreLabel, buildreport.ServiceStore, 10, "old store", "new store")
	h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "old sidecar", "new sidecar")
	h.services.onRestart = func() { h.bindSocket(t) }
	sidecar, err := h.r.EnsureStoreCurrent(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	before := len(h.acts())

	// Act
	err = sidecar(context.Background())

	// Assert
	if err != nil || len(h.acts()) != before {
		t.Fatalf("second half = %v, acts %v; want nothing more after the pair's restart", err, h.acts()[before:])
	}
}

func TestEnsureStoreCurrentsSecondHalfLeavesASidecarTheFirstHalfBootstrapped(t *testing.T) {
	// Arrange: the sidecar is unloaded, so the first half bootstraps it from
	// the installed build, and its report is still the old one.
	h := newRestarter(t)
	h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
	h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "old sidecar", "new sidecar")
	h.unload(t, SidecarLabel)
	sidecar, err := h.r.EnsureStoreCurrent(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	before := len(h.acts())

	// Act
	err = sidecar(context.Background())

	// Assert
	if err != nil || len(h.acts()) != before {
		t.Fatalf("second half = %v, acts %v; want a just-bootstrapped sidecar left alone", err, h.acts()[before:])
	}
}

func TestRestartStandaloneKickstartsItAlone(t *testing.T) {
	cases := []struct {
		name       string
		restartErr error
	}{
		{name: "kickstarted"},
		{name: "a failed kickstart is surfaced", restartErr: errors.New("kickstart refused")},
	}
	for _, sv := range Standalones {
		for _, tc := range cases {
			t.Run(sv.Short+"/"+tc.name, func(t *testing.T) {
				// Arrange: the standalone is loaded from its definition.
				h := newRestarter(t)
				writeFile(t, filepath.Join(h.services.dir, sv.Label+".plist"), "<plist/>")
				h.services.restartErr = tc.restartErr

				// Act
				err := h.r.RestartStandalone(context.Background(), sv)

				// Assert
				if tc.restartErr != nil {
					if err == nil || !loggedTo(h.log, "error", "the "+sv.Short+" restart failed") {
						t.Fatalf("RestartStandalone = %v, records %+v; want the failure returned and at ERROR", err, h.log.Records())
					}
					return
				}
				if acts := h.acts(); err != nil || fmt.Sprint(acts) != fmt.Sprint([]string{"define " + sv.Label, "restart " + sv.Label}) {
					t.Fatalf("RestartStandalone = %v, acts = %v, want one %s kickstart", err, acts, sv.Label)
				}
			})
		}
	}
}

// standalonesCurrent makes every standalone service run the installed build,
// but SKIP's.
func standalonesCurrent(t *testing.T, h *restarterHarness, skip Standalone) {
	t.Helper()
	for i, sv := range Standalones {
		if sv.Component != skip.Component {
			h.running(t, sv.Label, sv.Service, 30+i, "new "+sv.Short, "new "+sv.Short)
		}
	}
}

func TestEnsureCurrentKeepsEveryStandaloneLoadedAndCurrent(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *restarterHarness, sv Standalone)
		want    func(sv Standalone) []string
	}{
		{name: "a stale one is restarted alone", arrange: func(t *testing.T, h *restarterHarness, sv Standalone) {
			writeFile(t, filepath.Join(h.services.dir, sv.Label+".plist"), "<plist/>")
			h.running(t, sv.Label, sv.Service, 40, "old "+sv.Short, "new "+sv.Short)
		}, want: func(sv Standalone) []string { return []string{"define " + sv.Label, "restart " + sv.Label} }},
		{name: "a current one is left alone", arrange: func(t *testing.T, h *restarterHarness, sv Standalone) {
			h.running(t, sv.Label, sv.Service, 40, "new "+sv.Short, "new "+sv.Short)
		}, want: func(Standalone) []string { return nil }},
		{name: "an unloaded one is bootstrapped", arrange: func(t *testing.T, h *restarterHarness, sv Standalone) {
			writeFile(t, filepath.Join(h.services.dir, sv.Label+".plist"), "<plist/>")
			h.services.loaded[sv.Label] = false
		}, want: func(sv Standalone) []string { return []string{"start " + sv.Label} }},
	}
	for _, sv := range Standalones {
		for _, tc := range cases {
			t.Run(sv.Short+"/"+tc.name, func(t *testing.T) {
				// Arrange
				h := newRestarter(t)
				h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
				h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "new sidecar", "new sidecar")
				standalonesCurrent(t, h, sv)
				tc.arrange(t, h, sv)

				// Act
				err := h.r.EnsureCurrent(context.Background())

				// Assert
				if want := tc.want(sv); err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
					t.Fatalf("EnsureCurrent = %v, acts %v; want %v", err, h.acts(), want)
				}
			})
		}
	}
}

func TestEnsureStandaloneCurrentSurfacesAnUnreadableInstalledBuild(t *testing.T) {
	for _, sv := range Standalones {
		t.Run(sv.Short, func(t *testing.T) {
			// Arrange: the service runs, but nothing is installed to judge it against.
			h := newRestarter(t)
			h.running(t, sv.Label, sv.Service, 40, "new "+sv.Short, "new "+sv.Short)
			if err := os.Remove(filepath.Join(h.r.CacheBin, sv.Service)); err != nil {
				t.Fatal(err)
			}

			// Act
			err := h.r.EnsureStandaloneCurrent(context.Background(), sv)

			// Assert
			if err == nil || !loggedTo(h.log, "error", "installed build could not be read") || len(h.acts()) != 0 {
				t.Fatalf("EnsureStandaloneCurrent = %v, acts %v, records %+v; want the error at ERROR and nothing touched", err, h.acts(), h.log.Records())
			}
		})
	}
}

func TestEnsureStandaloneLoadedNeverRestartsARunningOne(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *restarterHarness, sv Standalone)
		want    func(sv Standalone) []string
	}{
		{name: "a stale running one is left running", arrange: func(t *testing.T, h *restarterHarness, sv Standalone) {
			h.running(t, sv.Label, sv.Service, 30, "old "+sv.Short, "new "+sv.Short)
		}, want: func(Standalone) []string { return nil }},
		{name: "an unloaded one is bootstrapped", arrange: func(t *testing.T, h *restarterHarness, sv Standalone) {
			writeFile(t, filepath.Join(h.services.dir, sv.Label+".plist"), "<plist/>")
			h.services.loaded[sv.Label] = false
		}, want: func(sv Standalone) []string { return []string{"start " + sv.Label} }},
	}
	for _, sv := range Standalones {
		for _, tc := range cases {
			t.Run(sv.Short+"/"+tc.name, func(t *testing.T) {
				// Arrange
				h := newRestarter(t)
				tc.arrange(t, h, sv)

				// Act
				err := h.r.EnsureStandaloneLoaded(context.Background(), sv)

				// Assert
				if want := tc.want(sv); err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
					t.Fatalf("EnsureStandaloneLoaded = %v, acts %v; want %v", err, h.acts(), want)
				}
			})
		}
	}
}

func TestEnsureStandalonesTriesEveryOneWhateverAnotherAnswers(t *testing.T) {
	// Arrange: the first standalone's definition is missing, the second's loads.
	h := newRestarter(t)
	h.defineErr = errors.New("define-service exited 1")
	h.services.loaded[Standalones[0].Label] = false
	writeFile(t, filepath.Join(h.services.dir, Standalones[1].Label+".plist"), "<plist/>")
	h.services.loaded[Standalones[1].Label] = false

	// Act
	err := h.r.EnsureStandalonesLoaded(context.Background())

	// Assert
	if want := []string{"define " + Standalones[0].Label, "start " + Standalones[1].Label}; err == nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
		t.Fatalf("EnsureStandalonesLoaded = %v, acts %v; want the first's failure and the second started", err, h.acts())
	}
}

func TestEnsureLoadedWritesAMissingDefinitionBeforeStartingTheService(t *testing.T) {
	cases := []struct {
		name          string
		defineErr     error
		writesNothing bool
		wantErr       string
		wantActs      []string
		wantLog       string
		level         string
	}{
		{name: "a written definition starts the service",
			wantActs: []string{"define " + TestsLabel, "start " + TestsLabel},
			wantLog:  "a service is not loaded and has no definition; writing it from its requirements entry", level: "info"},
		{name: "a definition that cannot be written starts nothing", defineErr: errors.New("define-service exited 1"),
			wantErr: "write the definition of " + TestsLabel, wantActs: []string{"define " + TestsLabel},
			wantLog: "a service's definition could not be written from its requirements entry", level: "error"},
		{name: "a definition still missing once written starts nothing", writesNothing: true,
			wantErr: "is still unreadable", wantActs: []string{"define " + TestsLabel},
			wantLog: "a service's definition is still missing after it was written", level: "error"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the test service is unloaded and has no plist.
			h := newRestarter(t)
			h.defineErr, h.defineWritesNothing = tc.defineErr, tc.writesNothing
			h.services.loaded[TestsLabel] = false

			// Act
			err := h.r.EnsureStandaloneLoaded(context.Background(), Tests)

			// Assert
			if tc.wantErr == "" && err != nil || tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("EnsureStandaloneLoaded = %v, want error containing %q", err, tc.wantErr)
			}
			if fmt.Sprint(h.acts()) != fmt.Sprint(tc.wantActs) {
				t.Fatalf("acts = %v, want %v", h.acts(), tc.wantActs)
			}
			if !loggedTo(h.log, tc.level, tc.wantLog) {
				t.Fatalf("records = %+v, want %s %q", h.log.Records(), tc.level, tc.wantLog)
			}
		})
	}
}

func TestRestartStandaloneLoadsAServiceTheManagerDoesNotHold(t *testing.T) {
	cases := []struct {
		name      string
		defineErr error
		wantActs  func(Standalone) []string
		wantErr   bool
	}{
		{name: "an unloaded one with no definition is defined and started, never kickstarted",
			wantActs: func(sv Standalone) []string { return []string{"define " + sv.Label, "start " + sv.Label} }},
		{name: "one whose definition cannot be written is neither started nor kickstarted", defineErr: errors.New("define-service exited 1"),
			wantActs: func(sv Standalone) []string { return []string{"define " + sv.Label} }, wantErr: true},
	}
	for _, sv := range Standalones {
		for _, tc := range cases {
			t.Run(sv.Short+"/"+tc.name, func(t *testing.T) {
				// Arrange
				h := newRestarter(t)
				h.services.loaded[sv.Label] = false
				h.defineErr = tc.defineErr

				// Act
				err := h.r.RestartStandalone(context.Background(), sv)

				// Assert
				if (err != nil) != tc.wantErr || fmt.Sprint(h.acts()) != fmt.Sprint(tc.wantActs(sv)) {
					t.Fatalf("RestartStandalone = %v, acts %v; want error %v and acts %v", err, h.acts(), tc.wantErr, tc.wantActs(sv))
				}
			})
		}
	}
}

func TestRestartOn(t *testing.T) {
	tests := []struct {
		name      string
		held      bool
		changed   bool
		stopErr   error
		stillHeld bool
		wantActs  []string
		wantErr   bool
	}{
		{name: "a held service on an unchanged definition is restarted in place", held: true,
			wantActs: []string{"restart " + SidecarLabel}},
		{name: "a held service on a changed definition is stopped, then started", held: true, changed: true,
			wantActs: []string{"stop " + SidecarLabel, "start " + SidecarLabel}},
		{name: "a service the manager does not hold is started, never restarted", held: false,
			wantActs: []string{"start " + SidecarLabel}},
		{name: "a service the manager does not hold on a changed definition is started", held: false, changed: true,
			wantActs: []string{"start " + SidecarLabel}},
		{name: "a service the manager still holds after its stop is not started", held: true, changed: true, stillHeld: true,
			wantActs: []string{"stop " + SidecarLabel}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			h.services.loaded[SidecarLabel] = tt.held
			if tt.stillHeld {
				h.services.statusesUntilGone = 1_000
			}

			// Act
			err := h.r.restartOn(context.Background(), SidecarLabel, tt.changed, dlog.Context{})

			// Assert
			if (err != nil) != tt.wantErr || fmt.Sprint(h.acts()) != fmt.Sprint(tt.wantActs) {
				t.Fatalf("restartOn = %v, acts %v; want error %v, acts %v", err, h.acts(), tt.wantErr, tt.wantActs)
			}
		})
	}
}

func TestRestartOnWaitsForTheStoppedProcessToExitBeforeStarting(t *testing.T) {
	// Arrange: the sidecar's process (pid 20) has not exited yet.
	h := newRestarter(t)
	h.exitsMu.Lock()
	h.exits[20] = make(chan struct{})
	h.exitsMu.Unlock()
	done := make(chan error, 1)

	// Act
	go func() { done <- h.r.restartOn(context.Background(), SidecarLabel, true, dlog.Context{}) }()
	<-h.awaiting
	before := h.acts()
	h.exit(20)
	err := <-done

	// Assert
	if fmt.Sprint(before) != fmt.Sprint([]string{"stop " + SidecarLabel}) {
		t.Fatalf("acts before the exit = %v, want only the stop", before)
	}
	if err != nil || fmt.Sprint(h.acts()) != fmt.Sprint([]string{"stop " + SidecarLabel, "start " + SidecarLabel}) {
		t.Fatalf("restartOn = %v, acts %v; want the start only after the exit", err, h.acts())
	}
}

func TestRestartHolderKickstartsItThenProvesItServes(t *testing.T) {
	holder := holderensure.Holder{ID: "socket-holder-daemon", Label: "com.agentrepl.sockethold.daemon", Service: "sockethold-daemon"}
	cases := []struct {
		name       string
		restartErr error
		ensureErr  error
		wantMsg    string
	}{
		{name: "kickstarted and serving", wantMsg: "restarted the socket holder; it serves its control socket"},
		{name: "a failed kickstart is surfaced", restartErr: errors.New("kickstart refused"), wantMsg: "the socket holder's restart failed"},
		{name: "a holder that does not come back is surfaced", ensureErr: errors.New("no holder serves")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the holder is loaded from its definition.
			h := newRestarter(t)
			writeFile(t, filepath.Join(h.services.dir, holder.Label+".plist"), "<plist/>")
			h.services.loaded[holder.Label] = true
			h.services.restartErr = tc.restartErr
			h.holderErr = tc.ensureErr

			// Act
			err := h.r.RestartHolder(context.Background(), holder)

			// Assert
			failed := tc.restartErr != nil || tc.ensureErr != nil
			if (err != nil) != failed {
				t.Fatalf("RestartHolder = %v, want a failure %v", err, failed)
			}
			wantActs := []string{"restart " + holder.Label}
			if tc.restartErr == nil {
				wantActs = append(wantActs, "ensure-holder "+holder.ID)
			}
			if fmt.Sprint(h.acts()) != fmt.Sprint(wantActs) {
				t.Fatalf("acts = %v, want %v: one kickstart, then the holder proven serving", h.acts(), wantActs)
			}
			if tc.restartErr == nil && fmt.Sprint(h.ensuredHolders) != fmt.Sprint([]string{holder.ID}) {
				t.Fatalf("ensured = %v, want the holder proven serving after its restart", h.ensuredHolders)
			}
			if tc.wantMsg != "" && !loggedTo(h.log, map[bool]string{true: "error", false: "info"}[failed], tc.wantMsg) {
				t.Fatalf("records = %+v, want %q", h.log.Records(), tc.wantMsg)
			}
		})
	}
}
