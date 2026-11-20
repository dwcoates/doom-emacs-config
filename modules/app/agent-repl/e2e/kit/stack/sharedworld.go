package stack

import (
	"fmt"
	"reflect"
	"sync"
	"testing"

	"agent-repld/integration/harness"
)

// THE SHARED DEFAULT WORLD (AGENTS.md "Integration tests share one canonical
// process, each case on its own slice").
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
// WHO MAY NOT SHARE IT. A test that needs other WorldOpts takes the GROUP
// World built with them (GroupWorld), shared the same way by every test that
// needs those options. Only a test whose subject is a process's own lifecycle
// (it stops, restarts, deploys or relaunches the daemon, store or sidecar)
// keeps NewWorld. A test asserts only on its own workspaces, never on a view
// spanning the daemon; the rule is AGENTS.md "Integration tests share one
// canonical process, each case on its own slice".

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
	return theSharedWorld.viewFor(t, func() *World { return newSharedWorld(harness.Suite(), WorldOpts{}) })
}

// worldGroups holds each group's World, by group name, with the options it
// was built with.
type worldGroups struct {
	mu     sync.Mutex
	worlds map[string]*sharedWorld
	opts   map[string]WorldOpts
}

var theWorldGroups = &worldGroups{worlds: map[string]*sharedWorld{}, opts: map[string]WorldOpts{}}

// group answers the group NAME's one World, created on the first ask, and
// refuses an ask under options other than those the group was first asked
// with.
func (g *worldGroups) group(name string, opts WorldOpts) (*sharedWorld, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	if built, ok := g.opts[name]; ok {
		if !reflect.DeepEqual(built, opts) {
			return nil, fmt.Errorf("e2e: the group World %q was built with %+v, and this test asks for it with %+v; a group has one precondition", name, built, opts)
		}
		return g.worlds[name], nil
	}
	g.worlds[name], g.opts[name] = &sharedWorld{}, opts
	return g.worlds[name], nil
}

// GroupWorld answers t's view of the suite's one World for the group NAME,
// built with OPTS on the first test that asks and shared like SharedWorld by
// every test that asks for the group: one World per precondition (a
// keep-alive interval, a sidecar window, a real zettel), never one per test.
// The tests of a group read its options and never change them. Asking for a
// group under options other than those it was built with is a broken test
// and fails it.
func GroupWorld(t *testing.T, name string, opts WorldOpts) *World {
	t.Helper()
	group, err := theWorldGroups.group(name, opts)
	if err != nil {
		t.Fatal(err)
	}
	return group.viewFor(t, func() *World { return newSharedWorld(harness.Suite(), opts) })
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
	return &World{Daemon: s.world.Daemon.View(t), Store: s.world.Store, Sidecar: s.world.Sidecar, Zettel: s.world.Zettel}
}

// newSharedWorld builds a World with OPTS on the suite and marks its store and
// sidecar shared until the suite's teardown, which may then stop them.
func newSharedWorld(suite testing.TB, opts WorldOpts) *World {
	w := NewWorld(suite, opts)
	w.Store.shared, w.Sidecar.shared = true, true
	// Registered last, so it runs first at teardown: the World's own
	// cleanups stop the processes it started.
	suite.Cleanup(func() { w.Store.shared, w.Sidecar.shared = false, false })
	return w
}
