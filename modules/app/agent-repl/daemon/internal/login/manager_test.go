package login_test

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/envc"
	"claude-repld/internal/ids"
	"claude-repld/internal/login"
)

// readDeadline bounds every wait for terminal output, so a broken expectation
// fails the test instead of hanging the suite. It is a failure bound, never a
// synchronization device: every assertion below waits on the CHANNEL.
const readDeadline = 15 * time.Second

// fakeVendorScript is a stand-in for `claude /login` on a pty.
//
// It announces itself (so a watcher can prove the child is up and the
// scrollback is being retained), records every spawn (so a JOIN can be told
// from a second spawn), reports the pty geometry ON DEMAND rather than off
// SIGWINCH (a request/response is deterministic where a signal race is not),
// echoes what it is sent, and exits on a magic line.
const fakeVendorScript = `#!/bin/sh
echo "$$" >> "$FAKE_SPAWNS"
printf 'READY args=%s config=%s\n' "$*" "$CLAUDE_CONFIG_DIR"
while IFS= read -r line; do
  case "$line" in
    QUIT) printf 'BYE\n'; exit 0 ;;
    SIZE) printf 'SIZE %s\n' "$(stty size | tr -d '\n')" ;;
    *) printf 'ECHO %s\n' "$line" ;;
  esac
done
`

// fixture is one test's manager over a fake vendor binary.
type fixture struct {
	m        login.Manager
	spawns   string
	routes   map[ids.WorkspaceID]string
	routeErr error
	// log is the manager's logger, so a test can assert what it recorded.
	log *dlog.TestLogger
	// observer records every flow's opening and ending.
	observer *fakeObserver
}

// fakeObserver records every opening and ending, in order, and signals each.
type fakeObserver struct {
	mu     sync.Mutex
	events []string
	seen   chan string
}

func newFakeObserver() *fakeObserver { return &fakeObserver{seen: make(chan string, 16)} }

func (o *fakeObserver) LoginOpened(configDir string) { o.note("opened " + configDir) }
func (o *fakeObserver) LoginEnded(configDir string)  { o.note("ended " + configDir) }

func (o *fakeObserver) note(event string) {
	o.mu.Lock()
	o.events = append(o.events, event)
	o.mu.Unlock()
	o.seen <- event
}

// recorded answers every event so far.
func (o *fakeObserver) recorded() []string {
	o.mu.Lock()
	defer o.mu.Unlock()
	return append([]string(nil), o.events...)
}

// await waits for event, bounded.
func (o *fakeObserver) await(t *testing.T, event string) {
	t.Helper()
	deadline := time.After(readDeadline)
	for {
		select {
		case got := <-o.seen:
			if got == event {
				return
			}
		case <-deadline:
			t.Fatalf("timed out waiting for %q; recorded %v", event, o.recorded())
		}
	}
}

// newFixture builds a manager whose fake vendor binary is an explicit path, so
// the vendor guard permits the spawn even under
// AGENT_REPL_FORBID_VENDOR_CALLS.
func newFixture(t *testing.T, routes map[ids.WorkspaceID]string) *fixture {
	t.Helper()
	t.Setenv(envc.EnvForbidVendorCalls, "1")

	dir := t.TempDir()
	f := &fixture{spawns: filepath.Join(dir, "spawns"), routes: routes}
	t.Setenv("FAKE_SPAWNS", f.spawns)

	bin := filepath.Join(dir, "fake-claude")
	if err := os.WriteFile(bin, []byte(fakeVendorScript), 0o700); err != nil {
		t.Fatalf("WriteFile() = %v", err)
	}

	f.log = dlog.NewTestLogger()
	f.observer = newFakeObserver()
	m, err := login.New(envc.NewVendorGuard(envc.Load()), bin, f.route, f.log, f.observer)
	if err != nil {
		t.Fatalf("login.New() = %v, want nil", err)
	}
	f.m = m
	t.Cleanup(func() { m.CloseAll(context.Background()) })
	return f
}

// route is the injected ConfigDirFunc.
func (f *fixture) route(ws ids.WorkspaceID) (string, error) {
	if f.routeErr != nil {
		return "", f.routeErr
	}
	dir, ok := f.routes[ws]
	if !ok {
		return "", fmt.Errorf("no account root for %q", ws)
	}
	return dir, nil
}

// spawnCount is how many times the fake vendor binary actually ran.
func (f *fixture) spawnCount(t *testing.T) int {
	t.Helper()
	body, err := os.ReadFile(f.spawns)
	if errors.Is(err, os.ErrNotExist) {
		return 0
	}
	if err != nil {
		t.Fatalf("ReadFile(%s) = %v", f.spawns, err)
	}
	return len(strings.Fields(string(body)))
}

// twoWorkspaces is the common routing table: two workspaces on ONE account
// root, and a third on the other.
func twoWorkspaces(t *testing.T) (map[ids.WorkspaceID]string, string, string) {
	t.Helper()
	base := t.TempDir()
	defaultRoot := filepath.Join(base, "default-root")
	multiRoot := filepath.Join(base, "multi-root")
	return map[ids.WorkspaceID]string{
		"ws-a": defaultRoot,
		"ws-b": defaultRoot,
		"ws-c": multiRoot,
	}, defaultRoot, multiRoot
}

// awaitText drains out until the accumulated text contains want.
func awaitText(t *testing.T, out <-chan login.Output, want string) string {
	t.Helper()
	deadline := time.After(readDeadline)
	var seen strings.Builder
	for {
		select {
		case frame, ok := <-out:
			if !ok {
				t.Fatalf("the stream closed before %q appeared; saw %q", want, seen.String())
			}
			if frame.Closed {
				t.Fatalf("the stream ended before %q appeared; saw %q", want, seen.String())
			}
			seen.Write(frame.Bytes)
			if strings.Contains(seen.String(), want) {
				return seen.String()
			}
		case <-deadline:
			t.Fatalf("timed out waiting for %q; saw %q", want, seen.String())
		}
	}
}

// awaitClosed drains out until the terminal frame arrives.
func awaitClosed(t *testing.T, out <-chan login.Output) {
	t.Helper()
	deadline := time.After(readDeadline)
	for {
		select {
		case frame, ok := <-out:
			if !ok {
				t.Fatal("the stream closed without a terminal frame")
			}
			if frame.Closed {
				return
			}
		case <-deadline:
			t.Fatal("timed out waiting for the terminal frame")
		}
	}
}

func TestOpenAnswersTheRoutedAccountRoot(t *testing.T) {
	// Arrange.
	routes, defaultRoot, _ := twoWorkspaces(t)
	f := newFixture(t, routes)

	// Act.
	got, err := f.m.Open(context.Background(), "ws-a")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil", err)
	}
	if got != defaultRoot {
		t.Fatalf("Open() = %q, want the routed root %q", got, defaultRoot)
	}
}

func TestOpenRunsTheVendorLoginSubcommandUnderTheAccountRoot(t *testing.T) {
	// Arrange.
	routes, defaultRoot, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}

	// Act.
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}

	// Assert.
	awaitText(t, out, "READY args=/login config="+defaultRoot)
}

func TestSecondOpenOnTheSameAccountRootJoins(t *testing.T) {
	// Arrange: a second click must not race a second OAuth flow against the
	// first, which is what per-account idempotence means.
	routes, defaultRoot, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("first Open() = %v", err)
	}
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")

	// Act: a DIFFERENT workspace on the same account root.
	got, err := f.m.Open(context.Background(), "ws-b")

	// Assert.
	if err != nil {
		t.Fatalf("second Open() = %v, want nil", err)
	}
	if got != defaultRoot {
		t.Fatalf("second Open() = %q, want %q", got, defaultRoot)
	}
	if n := f.spawnCount(t); n != 1 {
		t.Fatalf("spawns = %d, want 1 (the second open joins)", n)
	}
}

func TestTwoAccountRootsRunConcurrentLogins(t *testing.T) {
	// Arrange: two different accounts are genuinely independent.
	routes, _, multiRoot := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open(ws-a) = %v", err)
	}
	outA, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch(ws-a) = %v", err)
	}
	awaitText(t, outA, "READY")

	// Act.
	got, err := f.m.Open(context.Background(), "ws-c")
	if err != nil {
		t.Fatalf("Open(ws-c) = %v, want nil", err)
	}
	outC, err := f.m.Watch(context.Background(), "ws-c")
	if err != nil {
		t.Fatalf("Watch(ws-c) = %v", err)
	}

	// Assert.
	if got != multiRoot {
		t.Fatalf("Open(ws-c) = %q, want %q", got, multiRoot)
	}
	awaitText(t, outC, "config="+multiRoot)
}

func TestWatchReplaysTheScrollbackToALateViewer(t *testing.T) {
	// Arrange: the child may print the OAuth URL before anyone is watching.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	first, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, first, "READY")

	// Act: a viewer that arrives only now.
	late, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("late Watch() = %v", err)
	}

	// Assert.
	awaitText(t, late, "READY")
}

func TestSendKeystrokesReachTheChild(t *testing.T) {
	// Arrange.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")

	// Act.
	if err := f.m.SendKeystrokes(context.Background(), "ws-a", []byte("hello\n")); err != nil {
		t.Fatalf("SendKeystrokes() = %v, want nil", err)
	}

	// Assert.
	awaitText(t, out, "ECHO hello")
}

func TestDefaultGeometryIsWideEnoughForTheOAuthURL(t *testing.T) {
	// Arrange: the TUI hard-wraps at the column count and the OAuth URL runs
	// ~350 characters, so the DAEMON's default must not split it.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")

	// Act.
	if err := f.m.SendKeystrokes(context.Background(), "ws-a", []byte("SIZE\n")); err != nil {
		t.Fatalf("SendKeystrokes() = %v", err)
	}

	// Assert.
	seen := awaitText(t, out, "SIZE 60 400")
	if !strings.Contains(seen, "SIZE 60 400") {
		t.Fatalf("geometry = %q, want the daemon's 60x400 default", seen)
	}
}

func TestSendResizeIsAppliedToThePty(t *testing.T) {
	// Arrange.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")

	// Act.
	if err := f.m.SendResize(context.Background(), "ws-a", login.Resize{Rows: 24, Cols: 132}); err != nil {
		t.Fatalf("SendResize() = %v, want nil", err)
	}
	if err := f.m.SendKeystrokes(context.Background(), "ws-a", []byte("SIZE\n")); err != nil {
		t.Fatalf("SendKeystrokes() = %v", err)
	}

	// Assert: the child's own tty reports the new geometry.
	awaitText(t, out, "SIZE 24 132")
}

func TestCloseEndsTheStreamWithTheTerminalFrame(t *testing.T) {
	// Arrange.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")

	// Act.
	if err := f.m.Close(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Close() = %v, want nil", err)
	}

	// Assert.
	awaitClosed(t, out)
}

func TestChildExitEndsTheStreamWithTheTerminalFrame(t *testing.T) {
	// Arrange: the login child leaving on its own is the ordinary end.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")

	// Act.
	if err := f.m.SendKeystrokes(context.Background(), "ws-a", []byte("QUIT\n")); err != nil {
		t.Fatalf("SendKeystrokes() = %v", err)
	}

	// Assert.
	awaitClosed(t, out)
}

func TestCloseAnAbsentLoginIsSuccess(t *testing.T) {
	// Arrange: the desired state already holds.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)

	// Act.
	err := f.m.Close(context.Background(), "ws-a")

	// Assert.
	if err != nil {
		t.Fatalf("Close() = %v, want nil", err)
	}
}

func TestWatchWithoutAStandingLoginIsTyped(t *testing.T) {
	// Arrange.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)

	// Act.
	_, err := f.m.Watch(context.Background(), "ws-a")

	// Assert.
	if !errors.Is(err, login.ErrNoSession) {
		t.Fatalf("Watch() = %v, want login.ErrNoSession", err)
	}
}

func TestSendKeystrokesWithoutAStandingLoginIsNotAWarning(t *testing.T) {
	// Arrange: `no_login_open` is a LANDED SendLoginInputError arm, so the
	// refusal is an ordinary answer rather than a fault.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)

	// Act.
	_ = f.m.SendKeystrokes(context.Background(), "ws-a", []byte("x"))

	// Assert.
	for _, record := range f.log.Records() {
		if record.Level == "warn" || record.Level == "error" {
			t.Fatalf("record = %+v, want no warning for a landed typed refusal", record)
		}
	}
}

func TestSendKeystrokesWithoutAStandingLoginIsTyped(t *testing.T) {
	// Arrange.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)

	// Act.
	err := f.m.SendKeystrokes(context.Background(), "ws-a", []byte("x"))

	// Assert.
	if !errors.Is(err, login.ErrNoSession) {
		t.Fatalf("SendKeystrokes() = %v, want login.ErrNoSession", err)
	}
}

func TestOpenSurfacesARoutingFailure(t *testing.T) {
	// Arrange: a workspace whose account cannot be determined has no login to
	// address, and guessing one would log in as the wrong account.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	f.routeErr = errors.New("no such workspace")

	// Act.
	_, err := f.m.Open(context.Background(), "ws-a")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the routing failure surfaced")
	}
}

func TestOpenRefusesTheDefaultVendorBinaryWhenVendorCallsAreForbidden(t *testing.T) {
	// Arrange: `claude` on PATH IS the vendor.
	t.Setenv(envc.EnvForbidVendorCalls, "1")
	t.Setenv(login.EnvClaudeBin, "")
	route := func(ids.WorkspaceID) (string, error) { return "/roots/default", nil }
	m, err := login.New(envc.NewVendorGuard(envc.Load()), "", route, dlog.NewTestLogger(), newFakeObserver())
	if err != nil {
		t.Fatalf("login.New() = %v", err)
	}

	// Act.
	_, err = m.Open(context.Background(), "ws-a")

	// Assert.
	var forbidden *envc.ForbiddenError
	if !errors.As(err, &forbidden) {
		t.Fatalf("Open() = %v, want *envc.ForbiddenError", err)
	}
}

func TestOpenPermitsAnExplicitBinaryWhenVendorCallsAreForbidden(t *testing.T) {
	// Arrange: an explicit path is by construction not the real CLI, and
	// refusing it would forbid the very thing the knob exists to allow.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)

	// Act.
	_, err := f.m.Open(context.Background(), "ws-a")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil under AGENT_REPL_FORBID_VENDOR_CALLS with an explicit binary", err)
	}
}

func TestOpenAfterTheChildExitedStartsAFreshLogin(t *testing.T) {
	// Arrange: a second open must not join a corpse.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")
	if err := f.m.SendKeystrokes(context.Background(), "ws-a", []byte("QUIT\n")); err != nil {
		t.Fatalf("SendKeystrokes() = %v", err)
	}
	awaitClosed(t, out)

	// Act.
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("second Open() = %v, want nil", err)
	}
	fresh, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("second Watch() = %v", err)
	}

	// Assert.
	awaitText(t, fresh, "READY")
	if n := f.spawnCount(t); n != 2 {
		t.Fatalf("spawns = %d, want 2 (the exited login is not joined)", n)
	}
}

func TestWatchStopsWhenTheViewersContextIsCancelled(t *testing.T) {
	// Arrange.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	out, err := f.m.Watch(ctx, "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")

	// Act.
	cancel()

	// Assert: the channel closes without a terminal frame — the login is still
	// running, this viewer merely left.
	deadline := time.After(readDeadline)
	for {
		select {
		case frame, ok := <-out:
			if !ok {
				return
			}
			if frame.Closed {
				t.Fatal("the stream ended with a terminal frame, want a plain detach")
			}
		case <-deadline:
			t.Fatal("timed out waiting for the cancelled stream to close")
		}
	}
}

func TestCloseAllEndsEveryLogin(t *testing.T) {
	// Arrange.
	routes, _, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	for _, ws := range []ids.WorkspaceID{"ws-a", "ws-c"} {
		if _, err := f.m.Open(context.Background(), ws); err != nil {
			t.Fatalf("Open(%s) = %v", ws, err)
		}
	}
	outA, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch(ws-a) = %v", err)
	}
	outC, err := f.m.Watch(context.Background(), "ws-c")
	if err != nil {
		t.Fatalf("Watch(ws-c) = %v", err)
	}
	awaitText(t, outA, "READY")
	awaitText(t, outC, "READY")

	// Act.
	f.m.CloseAll(context.Background())

	// Assert.
	awaitClosed(t, outA)
	awaitClosed(t, outC)
}

func TestOpeningAFlowTellsTheObserverOnce(t *testing.T) {
	// Arrange: two workspaces on one account root.
	routes, defaultRoot, _ := twoWorkspaces(t)
	f := newFixture(t, routes)

	// Act: the second open joins the first flow.
	for _, ws := range []ids.WorkspaceID{"ws-a", "ws-b"} {
		if _, err := f.m.Open(context.Background(), ws); err != nil {
			t.Fatalf("Open(%s) = %v", ws, err)
		}
	}

	// Assert.
	if got := f.observer.recorded(); len(got) != 1 || got[0] != "opened "+defaultRoot {
		t.Fatalf("observed %v, want one opening of %s", got, defaultRoot)
	}
}

func TestTheChildExitingTellsTheObserverTheFlowEnded(t *testing.T) {
	// Arrange.
	routes, defaultRoot, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}
	out, err := f.m.Watch(context.Background(), "ws-a")
	if err != nil {
		t.Fatalf("Watch() = %v", err)
	}
	awaitText(t, out, "READY")

	// Act.
	if err := f.m.SendKeystrokes(context.Background(), "ws-a", []byte("QUIT\n")); err != nil {
		t.Fatalf("SendKeystrokes() = %v", err)
	}
	f.observer.await(t, "ended "+defaultRoot)

	// Assert.
	want := []string{"opened " + defaultRoot, "ended " + defaultRoot}
	if got := f.observer.recorded(); strings.Join(got, "|") != strings.Join(want, "|") {
		t.Fatalf("observed %v, want %v", got, want)
	}
}

func TestClosingAFlowTellsTheObserverItEnded(t *testing.T) {
	// Arrange.
	routes, defaultRoot, _ := twoWorkspaces(t)
	f := newFixture(t, routes)
	if _, err := f.m.Open(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Open() = %v", err)
	}

	// Act.
	if err := f.m.Close(context.Background(), "ws-a"); err != nil {
		t.Fatalf("Close() = %v", err)
	}

	// Assert.
	f.observer.await(t, "ended "+defaultRoot)
}
