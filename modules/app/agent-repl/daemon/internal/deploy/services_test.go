package deploy

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

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
	bootstrapErr error
	printErr     error
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
	return nil
}

func (l *fakeLaunchd) Bootstrap(_ context.Context, plist string) error {
	l.record("bootstrap " + filepath.Base(plist))
	return l.bootstrapErr
}

// restarterHarness is one Restarter over a temp socket dir and plist dir.
type restarterHarness struct {
	r       *Restarter
	launchd *fakeLaunchd
	log     *dlog.TestLogger
	sock    string
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
	h := &restarterHarness{launchd: newFakeLaunchd(), log: log, sock: filepath.Join(sockDir, "store.sock")}
	h.r = &Restarter{
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
