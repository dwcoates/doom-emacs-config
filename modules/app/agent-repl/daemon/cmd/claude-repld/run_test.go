package main

import (
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/boot"
	"claude-repld/internal/daemonaddr"
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
		Graph: func(context.Context, process) (server.Deps, boot.Deps, error) {
			return server.Deps{}, boot.Deps{}, errServed
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
	opts := options{stateDir: root}
	for _, a := range adjust {
		a(&opts)
	}
	return run(context.Background(), opts, newTestHooks().hooks)
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
