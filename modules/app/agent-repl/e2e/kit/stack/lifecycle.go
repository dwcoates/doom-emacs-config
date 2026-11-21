package stack

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"agent-repld/integration/harness"
)

// LIFECYCLE FAMILIES (AGENTS.md "Integration tests share one canonical
// process, each case on its own slice"). A step whose subject is the DAEMON's
// own lifecycle (it deploys, relaunches, stops or cold-boots it) gets a fresh
// daemon of its own, with its own account roots and its own sidecar, but the
// STORE is not its subject: every step of a family runs on the family's one
// store, built once per test process on the first step that asks.
//
// WHY THE SIDECAR IS NOT SHARED. The sidecar reads the shim identity records
// under ONE daemon's state root (sidecarOpts.StateDir), and a fresh daemon per
// step is a fresh state root per step; a sidecar of another root would book a
// rotated transcript under a second id, which the store refuses.
//
// ONE PARENT, ONE LOCK DIR. Each step's state root sits under the family's
// parent, so harness.LockDirFor names the family's one lock directory for
// every step's daemon: the store reports its build there once, and each
// step's sidecar reports there as it starts, before its daemon is handed over.
//
// WHO MAY NOT JOIN ONE. A step that stops or restarts the store (its lifecycle
// is the subject) keeps NewWorld; a family's store is shared and refuses it.

// lifecycleServices is one family's shared store.
type lifecycleServices struct {
	parent  string
	lockDir string
	storeDB string
	store   *Store

	mu    sync.Mutex
	steps int
}

// lifecycleFamily is one family's store and how its build ended.
type lifecycleFamily struct {
	once  sync.Once
	svc   *lifecycleServices
	fatal *harness.SuiteFatal
	skip  *harness.SuiteSkip
}

// lifecycleFamilies holds every family by name.
var lifecycleFamilies = struct {
	mu       sync.Mutex
	families map[string]*lifecycleFamily
}{families: map[string]*lifecycleFamily{}}

// familyNamed answers the family NAME, created on the first ask.
func familyNamed(name string) *lifecycleFamily {
	lifecycleFamilies.mu.Lock()
	defer lifecycleFamilies.mu.Unlock()
	f, ok := lifecycleFamilies.families[name]
	if !ok {
		f = &lifecycleFamily{}
		lifecycleFamilies.families[name] = f
	}
	return f
}

// nextStateRoot answers a state root of the step's own under the family's
// parent, so its lock dir is the family's.
func (svc *lifecycleServices) nextStateRoot(t testing.TB) string {
	t.Helper()
	svc.mu.Lock()
	svc.steps++
	n := svc.steps
	svc.mu.Unlock()
	root := filepath.Join(svc.parent, fmt.Sprintf("s%d", n))
	if err := os.MkdirAll(root, 0o755); err != nil {
		t.Fatalf("e2e: mkdir the step's state root %s: %v", root, err)
	}
	if got := harness.LockDirFor(root); got != svc.lockDir {
		t.Fatalf("e2e: the step's state root %s locks in %s, not the family's %s; its deploy would judge a store that never reported there", root, got, svc.lockDir)
	}
	return root
}

// LifecycleWorld answers a World for one lifecycle step of the family NAME: a
// fresh daemon, account roots and sidecar built with OPTS, on the family's
// shared store. The step owns its daemon whole, to deploy, relaunch, stop or
// cold-boot; the store outlives it and refuses to be stopped.
func LifecycleWorld(t testing.TB, name string, opts WorldOpts) *World {
	t.Helper()
	if err := lifecycleOptsAllowed(opts); err != nil {
		t.Fatal(err)
	}
	f := familyNamed(name)
	f.once.Do(func() {
		f.fatal, f.skip = harness.RecoverSuiteBuild(func() { f.svc = newLifecycleServices(harness.Suite()) })
	})
	switch {
	case f.fatal != nil:
		t.Fatalf("e2e: the lifecycle family %q did not build: %s", name, f.fatal.Message)
	case f.skip != nil:
		t.Skipf("e2e: the lifecycle family %q did not build: %s", name, f.skip.Reason)
	}
	return newWorld(t, opts, f.svc)
}

// lifecycleOptsAllowed refuses options a lifecycle step may not name: its
// state root is its family's.
func lifecycleOptsAllowed(opts WorldOpts) error {
	if opts.DaemonOpts.StateDir != "" {
		return fmt.Errorf("e2e: a lifecycle step's state root is its family's; this step names %q", opts.DaemonOpts.StateDir)
	}
	return nil
}

// ScenarioLifecycleWorld is LifecycleWorld for the family of t's scenario
// (its top-level test), with daemon options OPTS: a scenario's steps run in
// order, so its family's store never serves two of its steps at once, and
// scenarios, which run side by side, never share a family.
func ScenarioLifecycleWorld(t testing.TB, opts harness.Opts) *World {
	t.Helper()
	return LifecycleWorld(t, scenarioOf(t.Name()), WorldOpts{DaemonOpts: opts})
}

// scenarioOf is the top-level test of the test NAME.
func scenarioOf(name string) string {
	scenario, _, _ := strings.Cut(name, "/")
	return scenario
}

// newLifecycleServices builds a family's store on the suite, and marks it
// shared until the suite's teardown, which may then stop it.
func newLifecycleServices(suite testing.TB) *lifecycleServices {
	suite.Helper()
	svc := &lifecycleServices{
		parent:  harness.ShortTempDir(suite),
		storeDB: filepath.Join(suite.TempDir(), "store.db"),
	}
	svc.lockDir = harness.LockDirFor(filepath.Join(svc.parent, "s0"))
	logsDir := filepath.Join(svc.parent, "logs")
	if err := os.MkdirAll(logsDir, 0o755); err != nil {
		suite.Fatalf("e2e: mkdir %s: %v", logsDir, err)
	}
	svc.store = startStore(suite, shortSocketPath(suite, "store"), svc.storeDB, filepath.Join(logsDir, "store.log"), svc.lockDir)
	svc.store.shared = true
	suite.Cleanup(func() { svc.store.shared = false })
	return svc
}
