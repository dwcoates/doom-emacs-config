package deploy

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"agentrepl/logging/buildreport"

	"claude-repld/internal/dlog"
)

// fakeLaunchd scripts launchd. NO TEST EVER REACHES THE LIVE LAUNCHD: this is
// the whole of this package's contact with it.
type fakeLaunchd struct {
	mu    sync.Mutex
	calls []string
	// loaded is what Print answers per label.
	loaded map[string]bool
	pid    map[string]int
	// onKickstart runs when the store is kickstarted (a test binds the socket,
	// or grows the log, or kills the process there).
	onKickstart func()
	// printsUntilGone is how many Prints of the sidecar still find it loaded
	// after its bootout.
	printsUntilGone int
	// onStorePrint runs on every Print of the store while it boots.
	onStorePrint func(n int)
	storePrints  int
	kickErr      error
	bootoutErr   error
	bootstrapErr error
	printErr     error
	// onBootstrap runs when a plist is bootstrapped, with its file name.
	onBootstrap func(plist string)
}

func newFakeLaunchd() *fakeLaunchd {
	return &fakeLaunchd{
		loaded: map[string]bool{StoreLabel: true, SidecarLabel: true},
		pid:    map[string]int{StoreLabel: 10, SidecarLabel: 20},
	}
}

func (l *fakeLaunchd) record(call string) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.calls = append(l.calls, call)
}

func (l *fakeLaunchd) Calls() []string {
	l.mu.Lock()
	defer l.mu.Unlock()
	return append([]string(nil), l.calls...)
}

func (l *fakeLaunchd) Print(_ context.Context, label string) (bool, int, error) {
	l.record("print " + label)
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.printErr != nil {
		return false, 0, l.printErr
	}
	if label == SidecarLabel && !l.loaded[label] && l.printsUntilGone > 0 {
		l.printsUntilGone--
		return true, 0, nil
	}
	if label == StoreLabel && l.onStorePrint != nil {
		l.storePrints++
		n := l.storePrints
		l.mu.Unlock()
		l.onStorePrint(n)
		l.mu.Lock()
	}
	return l.loaded[label], l.pid[label], nil
}

func (l *fakeLaunchd) Kickstart(_ context.Context, label string) error {
	l.record("kickstart " + label)
	if l.kickErr != nil {
		return l.kickErr
	}
	if label == StoreLabel && l.onKickstart != nil {
		l.onKickstart()
	}
	return nil
}

func (l *fakeLaunchd) Bootout(_ context.Context, label string) error {
	l.record("bootout " + label)
	l.mu.Lock()
	defer l.mu.Unlock()
	l.loaded[label] = false
	return l.bootoutErr
}

func (l *fakeLaunchd) Bootstrap(_ context.Context, plist string) error {
	l.record("bootstrap " + filepath.Base(plist))
	if l.bootstrapErr == nil && l.onBootstrap != nil {
		l.onBootstrap(filepath.Base(plist))
	}
	return l.bootstrapErr
}

// restarterHarness is one Restarter over a temp socket dir and plist dir.
type restarterHarness struct {
	r       *Restarter
	launchd *fakeLaunchd
	log     *dlog.TestLogger
	sock    string
	alive   map[int]bool
}

func newRestarter(t *testing.T) *restarterHarness {
	t.Helper()
	// A unix socket path must stay under the 104-byte sun_path budget, which
	// the per-test temp dir on macOS overflows.
	sockDir, err := os.MkdirTemp("/tmp", "dpl")
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
	h := &restarterHarness{launchd: newFakeLaunchd(), log: log, sock: filepath.Join(sockDir, "store.sock"), alive: map[int]bool{}}
	h.r = &Restarter{
		CacheBin:    t.TempDir(),
		ReportDir:   t.TempDir(),
		Alive:       func(pid int) bool { return h.alive[pid] },
		Launchd:     h.launchd,
		PlistDir:    plists,
		StoreSocket: h.sock,
		StoreLog:    filepath.Join(t.TempDir(), "shim-store.err.log"),
		Windows:     DefaultServiceWindows,
		Clock:       newStepClock(),
		Log:         log,
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
	h.launchd.onKickstart = func() { h.bindSocket(t) }

	// Act
	err := h.r.RestartStore(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("RestartStore: %v", err)
	}
	var acts []string
	for _, c := range h.launchd.Calls() {
		if !strings.HasPrefix(c, "print") {
			acts = append(acts, c)
		}
	}
	want := []string{"bootout " + SidecarLabel, "kickstart " + StoreLabel, "bootstrap " + SidecarLabel + ".plist"}
	if strings.Join(acts, ",") != strings.Join(want, ",") {
		t.Fatalf("launchd acts = %v, want %v", acts, want)
	}
}

func TestRestartStoreWaitsForTheSidecarToLeave(t *testing.T) {
	// Arrange: launchd still holds the sidecar for three looks after its bootout.
	h := newRestarter(t)
	h.launchd.printsUntilGone = 3
	h.launchd.onKickstart = func() { h.bindSocket(t) }

	// Act
	err := h.r.RestartStore(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("RestartStore: %v", err)
	}
	calls := h.launchd.Calls()
	kick := -1
	sidecarPrints := 0
	for i, c := range calls {
		if c == "print "+SidecarLabel && kick < 0 {
			sidecarPrints++
		}
		if c == "kickstart "+StoreLabel {
			kick = i
		}
	}
	if sidecarPrints < 5 {
		t.Fatalf("calls = %v, want the store kickstarted only after the sidecar left", calls)
	}
}

func TestRestartStoreRefusesWithoutTheSidecarPlist(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	if err := os.Remove(filepath.Join(h.r.PlistDir, SidecarLabel+".plist")); err != nil {
		t.Fatal(err)
	}

	// Act
	err := h.r.RestartStore(context.Background())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "install.sh") {
		t.Fatalf("RestartStore = %v, want the missing plist named with its remedy", err)
	}
	if calls := h.launchd.Calls(); len(calls) != 0 {
		t.Fatalf("launchd calls = %v, want nothing touched", calls)
	}
	if !loggedTo(h.log, "error", "the sidecar's plist is missing") {
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
			name: "the store dies before its socket appears",
			arrange: func(h *restarterHarness) {
				h.launchd.onKickstart = func() {
					h.launchd.mu.Lock()
					h.launchd.pid[StoreLabel] = 0
					h.launchd.mu.Unlock()
				}
			},
			want: "died before its socket appeared",
		},
		{
			name:    "the store is wedged: alive, silent, no socket",
			arrange: func(*restarterHarness) {},
			want:    "wedged",
		},
		{
			name: "the store is still writing past the upper bound",
			arrange: func(h *restarterHarness) {
				h.launchd.onStorePrint = func(n int) {
					writeFile(t, h.r.StoreLog, strings.Repeat("x", n))
				}
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
			for _, c := range h.launchd.Calls() {
				if strings.HasPrefix(c, "bootstrap") {
					t.Fatalf("calls = %v, want the sidecar left down", h.launchd.Calls())
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
		{name: "the store kickstart fails", arrange: func(h *restarterHarness) { h.launchd.kickErr = errors.New("kickstart refused") }},
		{name: "the sidecar bootstrap fails", arrange: func(h *restarterHarness) {
			h.launchd.bootstrapErr = errors.New("bootstrap refused")
			h.launchd.onKickstart = func() { h.bindSocket(t) }
		}},
		{name: "launchd cannot be read", arrange: func(h *restarterHarness) { h.launchd.printErr = errors.New("print refused") }},
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
	if calls := h.launchd.Calls(); len(calls) != 1 || calls[0] != "kickstart "+SidecarLabel {
		t.Fatalf("calls = %v, want one sidecar kickstart", calls)
	}
}

func TestRestartSidecarSurfacesAFailedKickstart(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	h.launchd.kickErr = errors.New("kickstart refused")

	// Act
	err := h.r.RestartSidecar(context.Background())

	// Assert
	if err == nil || !loggedTo(h.log, "error", "the sidecar kickstart failed") {
		t.Fatalf("RestartSidecar = %v, records %+v; want the failure returned and at ERROR", err, h.log.Records())
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
			wantMsg:   "had already left the user domain",
		},
		{
			name:      "any other bootout failure is a WARN while launchd's view decides",
			bootout:   errors.New("deploy: launchctl bootout exited 5"),
			wantLevel: "warn",
			wantMsg:   "bootout answered an error",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newRestarter(t)
			h.launchd.bootoutErr = tc.bootout
			h.launchd.onKickstart = func() { h.bindSocket(t) }

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
	h.launchd.bootoutErr = fmt.Errorf("%w: launchctl bootout exited 113", ErrServiceNotLoaded)
	h.launchd.onKickstart = func() { h.bindSocket(t) }

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
	h.launchd.loaded[label] = false
	h.launchd.pid[label] = 0
	writeFile(t, filepath.Join(h.r.PlistDir, label+".plist"), "<plist/>")
}

// bootstraps answers the plists EnsureLoaded bootstrapped, in order.
func (h *restarterHarness) bootstraps() []string {
	var out []string
	for _, c := range h.launchd.Calls() {
		if strings.HasPrefix(c, "bootstrap ") {
			out = append(out, strings.TrimPrefix(c, "bootstrap "))
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
	h.launchd.onBootstrap = func(string) {
		h.launchd.mu.Lock()
		h.launchd.loaded[StoreLabel], h.launchd.pid[StoreLabel] = true, 10
		h.launchd.mu.Unlock()
		h.bindSocket(t)
	}

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("EnsureLoaded: %v", err)
	}
	if got, want := h.bootstraps(), []string{StoreLabel + ".plist"}; fmt.Sprint(got) != fmt.Sprint(want) {
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
	h.launchd.onBootstrap = func(plist string) {
		switch plist {
		case StoreLabel + ".plist":
			h.launchd.mu.Lock()
			h.launchd.loaded[StoreLabel], h.launchd.pid[StoreLabel] = true, 10
			h.launchd.mu.Unlock()
			h.bindSocket(t)
		case SidecarLabel + ".plist":
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
	if got, want := h.bootstraps(), []string{SidecarLabel + ".plist"}; fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("bootstrapped %v, want %v", got, want)
	}
}

func TestEnsureLoadedRefusesAnUnloadedServiceWithNoPlist(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.launchd.loaded[StoreLabel] = false

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
	h.launchd.bootstrapErr = errors.New("launchctl bootstrap exited 5")

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err == nil || !errors.Is(err, h.launchd.bootstrapErr) {
		t.Fatalf("EnsureLoaded = %v, want the bootstrap failure", err)
	}
}

func TestEnsureLoadedSurfacesAnUnreadableLaunchdState(t *testing.T) {
	// Arrange.
	h := newRestarter(t)
	h.launchd.printErr = errors.New("launchctl print exited 1")

	// Act.
	err := h.r.EnsureLoaded(context.Background())

	// Assert.
	if err == nil || !errors.Is(err, h.launchd.printErr) {
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
	h.launchd.pid[label] = pid
}

// acts answers every launchd call other than a look.
func (h *restarterHarness) acts() []string {
	var out []string
	for _, c := range h.launchd.Calls() {
		if !strings.HasPrefix(c, "print") {
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
	h.launchd.onKickstart = func() { h.bindSocket(t) }

	// Act
	err := h.r.EnsureCurrent(context.Background())

	// Assert
	want := []string{"bootout " + SidecarLabel, "kickstart " + StoreLabel, "bootstrap " + SidecarLabel + ".plist"}
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
	if want := []string{"kickstart " + SidecarLabel}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
		t.Fatalf("EnsureCurrent = %v, acts %v; want %v", err, h.acts(), want)
	}
}

func TestEnsureCurrentLeavesAServiceLaunchdRunsNoProcessFor(t *testing.T) {
	// Arrange: both loaded, neither running, no reports at all.
	h := newRestarter(t)
	h.launchd.pid[StoreLabel], h.launchd.pid[SidecarLabel] = 0, 0

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
	h.launchd.onBootstrap = func(plist string) {
		if plist == StoreLabel+".plist" {
			h.launchd.mu.Lock()
			h.launchd.loaded[StoreLabel], h.launchd.pid[StoreLabel] = true, 10
			h.launchd.mu.Unlock()
			h.bindSocket(t)
		}
	}

	// Act
	err := h.r.EnsureCurrent(context.Background())

	// Assert
	if want := []string{"bootstrap " + StoreLabel + ".plist"}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
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
