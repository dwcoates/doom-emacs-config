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
		loaded: map[string]bool{StoreLabel: true, SidecarLabel: true, ZettelLabel: true},
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
	defer l.mu.Unlock()
	l.loaded[label] = false
	return l.stopErr
}

func (l *fakeServiceManager) Start(_ context.Context, label string) error {
	l.record("start " + label)
	if l.startErr == nil && l.onStart != nil {
		l.onStart(label)
	}
	return l.startErr
}

func (l *fakeServiceManager) Definition(label string) string {
	return filepath.Join(l.dir, label+".plist")
}

// restarterHarness is one Restarter over a temp socket dir and plist dir.
type restarterHarness struct {
	restarts *restarttimingtest.Recorder
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
	close(h.exits[pid])
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
	writeFile(t, filepath.Join(plists, SidecarLabel+".plist"), "<plist/>")
	log := dlog.NewTestLogger()
	h := &restarterHarness{services: newFakeServiceManager(plists), log: log, sock: filepath.Join(sockDir, "store.sock"), alive: map[int]bool{},
		restarts: &restarttimingtest.Recorder{}, clock: newBoundClock(),
		// The store runs until a test ends it; the sidecar's bootout ends it.
		exits: map[int]chan struct{}{10: make(chan struct{})}, awaiting: make(chan int, 8)}
	h.r = &Restarter{
		CacheBin:    t.TempDir(),
		ReportDir:   t.TempDir(),
		Alive:       func(pid int) bool { return h.alive[pid] },
		Services:    h.services,
		StoreSocket: h.sock,
		StoreLog:    filepath.Join(t.TempDir(), "shim-store.err.log"),
		Windows:     DefaultServiceWindows,
		Clock:       h.clock,
		Log:         log,
		Restarts:    h.restarts,
		ProcessExit: h.processExit,
	}
	return h
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
	want := []string{"stop " + SidecarLabel, "restart " + StoreLabel, "start " + SidecarLabel}
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
	if calls := h.services.Calls(); len(calls) != 1 || calls[0] != "restart "+SidecarLabel {
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

func TestEnsureLoadedRefusesAnUnloadedServiceWithNoPlist(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.services.loaded[StoreLabel] = false

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "install.sh") {
		t.Fatalf("EnsureLoaded = %v, want a refusal naming the reinstall", err)
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
func (h *restarterHarness) acts() []string {
	var out []string
	for _, c := range h.services.Calls() {
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
	want := []string{"stop " + SidecarLabel, "restart " + StoreLabel, "start " + SidecarLabel}
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
	if want := []string{"restart " + SidecarLabel}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
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
	if want := []string{"start " + StoreLabel}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
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
	if want := []string{"restart " + SidecarLabel}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
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

func TestRestartZettelKickstartsItAlone(t *testing.T) {
	cases := []struct {
		name       string
		restartErr error
	}{
		{name: "kickstarted"},
		{name: "a failed kickstart is surfaced", restartErr: errors.New("kickstart refused")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			h.services.restartErr = tc.restartErr

			// Act
			err := h.r.RestartZettel(context.Background())

			// Assert
			if tc.restartErr != nil {
				if err == nil || !loggedTo(h.log, "error", "the zettel restart failed") {
					t.Fatalf("RestartZettel = %v, records %+v; want the failure returned and at ERROR", err, h.log.Records())
				}
				return
			}
			if calls := h.services.Calls(); err != nil || len(calls) != 1 || calls[0] != "restart "+ZettelLabel {
				t.Fatalf("RestartZettel = %v, calls = %v, want one zettel kickstart", err, calls)
			}
		})
	}
}

func TestEnsureCurrentKeepsZettelLoadedAndCurrent(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *restarterHarness)
		want    []string
	}{
		{name: "a stale zettel is restarted alone", arrange: func(t *testing.T, h *restarterHarness) {
			h.running(t, ZettelLabel, buildreport.ServiceZettel, 30, "old zettel", "new zettel")
		}, want: []string{"restart " + ZettelLabel}},
		{name: "a current zettel is left alone", arrange: func(t *testing.T, h *restarterHarness) {
			h.running(t, ZettelLabel, buildreport.ServiceZettel, 30, "new zettel", "new zettel")
		}},
		{name: "an unloaded zettel is bootstrapped", arrange: func(t *testing.T, h *restarterHarness) {
			writeFile(t, filepath.Join(h.services.dir, ZettelLabel+".plist"), "<plist/>")
			h.services.loaded[ZettelLabel] = false
		}, want: []string{"start " + ZettelLabel}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			h.running(t, StoreLabel, buildreport.ServiceStore, 10, "new store", "new store")
			h.running(t, SidecarLabel, buildreport.ServiceSidecar, 20, "new sidecar", "new sidecar")
			tc.arrange(t, h)

			// Act
			err := h.r.EnsureCurrent(context.Background())

			// Assert
			if err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(tc.want) {
				t.Fatalf("EnsureCurrent = %v, acts %v; want %v", err, h.acts(), tc.want)
			}
		})
	}
}

func TestEnsureZettelLoadedNeverRestartsARunningZettel(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *restarterHarness)
		want    []string
	}{
		{name: "a stale running zettel is left running", arrange: func(t *testing.T, h *restarterHarness) {
			h.running(t, ZettelLabel, buildreport.ServiceZettel, 30, "old zettel", "new zettel")
		}},
		{name: "an unloaded zettel is bootstrapped", arrange: func(t *testing.T, h *restarterHarness) {
			writeFile(t, filepath.Join(h.services.dir, ZettelLabel+".plist"), "<plist/>")
			h.services.loaded[ZettelLabel] = false
		}, want: []string{"start " + ZettelLabel}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			tc.arrange(t, h)

			// Act
			err := h.r.EnsureZettelLoaded(context.Background())

			// Assert
			if err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(tc.want) {
				t.Fatalf("EnsureZettelLoaded = %v, acts %v; want %v", err, h.acts(), tc.want)
			}
		})
	}
}
