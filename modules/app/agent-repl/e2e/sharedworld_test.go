package e2e

import (
	"sync"
	"testing"

	"agent-repld/integration/harness"
)

// THE SHARED DEFAULT WORLD (AGENTS.md "A case may share an expensive fixture,
// never a slice another case reads").
//
// A World costs about 2.7s to build and tear down; a fresh workspace inside a
// running one costs about 0.3s (docs/investigations/2026-10-14-test-shared-
// setup-lzb.md). So a test that needs only the DEFAULT world's options takes
// it from SharedWorld instead of building its own: the suite builds it once
// per test process, on the first test that asks, and ends it after the last.
//
// WHAT EACH TEST STILL OWNS. Its World is a view (harness.Daemon.View): the
// test's own failures, run budget and warning sweep, over the workspaces it
// registered, which it opens fresh. The store and sidecar are shared and may
// not be stopped or restarted by a test.
//
// WHO MAY NOT SHARE IT. A test that stops, restarts, deploys or relaunches any
// process; that needs other WorldOpts; that asserts a view spanning the
// daemon or the account (roster, sidebar, account usage, the run log's
// workspace-less records); or that declares a warning only a whole-daemon
// declaration can cover. Those keep NewWorld.

// sharedWorld is the suite's one default World and how its build ended.
type sharedWorld struct {
	once  sync.Once
	world *World
	fatal *harness.SuiteFatal
	skip  *harness.SuiteSkip
}

var theSharedWorld sharedWorld

// SharedWorld answers the suite's shared default World as t sees it. A build
// that failed fails every test that asks; a precondition the build lacked
// skips every test that asks, with the same reason.
func SharedWorld(t *testing.T) *World {
	t.Helper()
	return theSharedWorld.viewFor(t, func() *World { return newSharedWorld(harness.Suite()) })
}

// viewFor builds the world once, through build, and answers t's view of it.
func (s *sharedWorld) viewFor(t testing.TB, build func() *World) *World {
	t.Helper()
	s.once.Do(func() {
		s.fatal, s.skip = harness.RecoverSuiteBuild(func() { s.world = build() })
	})
	switch {
	case s.fatal != nil:
		t.Fatalf("e2e: the shared World did not build: %s", s.fatal.Message)
	case s.skip != nil:
		t.Skipf("e2e: the shared World did not build: %s", s.skip.Reason)
	}
	t.Cleanup(func() { requireServicesOutlivedTheTest(t, s.world.Store, s.world.Sidecar, s.world.Zettel) })
	return &World{Daemon: s.world.Daemon.View(t), Store: s.world.Store, Sidecar: s.world.Sidecar}
}

// newSharedWorld builds the default World on the suite and marks its store and
// sidecar shared until the suite's teardown, which may then stop them.
func newSharedWorld(suite testing.TB) *World {
	w := NewWorld(suite, WorldOpts{})
	w.Store.shared, w.Sidecar.shared = true, true
	// Registered last, so it runs first at teardown: the World's own
	// cleanups stop the processes it started.
	suite.Cleanup(func() { w.Store.shared, w.Sidecar.shared = false, false })
	return w
}
