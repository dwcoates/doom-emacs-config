package ssm

import (
	"path/filepath"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// The two CONTEXT-CUT axes: clearing and compacting.
//
// Both are red, both outrank `thinking`, and both must be incapable of
// wedging — which is what most of this file is about. A clear whose
// ContextCleared never lands expires under a watchdog; a compaction its turn
// or a rotation outlives is closed by that bound.
// ---------------------------------------------------------------------------

// fakeTimer is a captured watchdog: it never fires on its own, so a test
// decides exactly when the expiry runs and never waits on wall-clock time.
type fakeTimer struct {
	mu      sync.Mutex
	fire    func()
	stopped bool
}

func (f *fakeTimer) Stop() bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.stopped = true
	return true
}

// armed reports whether the timer is still live (not stopped).
func (f *fakeTimer) armed() bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return !f.stopped
}

// timerFactory hands out fakeTimers and remembers every one it armed.
type timerFactory struct {
	mu     sync.Mutex
	timers []*fakeTimer
}

func (tf *timerFactory) afterFunc(_ time.Duration, f func()) Timer {
	tf.mu.Lock()
	defer tf.mu.Unlock()
	t := &fakeTimer{fire: f}
	tf.timers = append(tf.timers, t)
	return t
}

// last returns the most recently armed timer, failing when none was.
func (tf *timerFactory) last(t *testing.T) *fakeTimer {
	t.Helper()
	tf.mu.Lock()
	defer tf.mu.Unlock()
	if len(tf.timers) == 0 {
		t.Fatal("no clearing watchdog was armed")
	}
	return tf.timers[len(tf.timers)-1]
}

// openCutTest opens a Manager whose clearing watchdog is captured rather than
// scheduled, so an expiry is provoked by calling the callback.
func openCutTest(t *testing.T, resolver Resolver) (*Manager, *capLog, *timerFactory, string) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "state.db")
	cl := &capLog{}
	tf := &timerFactory{}
	m, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: resolver, AfterFunc: tf.afterFunc})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	wireAll(t, m, resolver)
	return m, cl, tf, path
}

// ---- The opening and closing edges ---------------------------------------

func TestContextCutAxesResolveTheirOwnState(t *testing.T) {
	tests := []struct {
		name string
		open func(m *Manager) error
		want frontendv1.RenderState
	}{
		{
			name: "a dispatched clear resolves CLEARING",
			open: func(m *Manager) error { return m.ApplyClearing("ws1", true, "clear_dispatched") },
			want: frontendv1.RenderState_RENDER_STATE_CLEARING,
		},
		{
			name: "a vendor compaction status resolves COMPACTING",
			open: func(m *Manager) error { return m.ApplyCompacting("ws1", true, "vendor_status:compacting") },
			want: frontendv1.RenderState_RENDER_STATE_COMPACTING,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a turn in flight, which is where a cut always happens.
			m, _, _, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
			if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
				t.Fatalf("turn started: %v", err)
			}

			// Act.
			if err := tc.open(m); err != nil {
				t.Fatalf("opening the axis: %v", err)
			}

			// Assert — the cut outranks the running turn's `thinking`.
			if got := mustCurrent(t, m, "ws1").State; got != tc.want {
				t.Fatalf("state = %s, want %s", renderName(got), renderName(tc.want))
			}
		})
	}
}

func TestContextCutReturnsToSessionStatusWhenClosed(t *testing.T) {
	tests := []struct {
		name  string
		open  func(m *Manager) error
		close func(m *Manager) error
	}{
		{
			name:  "the ContextCleared closes clearing",
			open:  func(m *Manager) error { return m.ApplyClearing("ws1", true, "clear_dispatched") },
			close: func(m *Manager) error { return m.ApplyClearing("ws1", false, "context_cleared") },
		},
		{
			name:  "the ContextCompacted closes compacting",
			open:  func(m *Manager) error { return m.ApplyCompacting("ws1", true, "vendor_status:compacting") },
			close: func(m *Manager) error { return m.ApplyCompacting("ws1", false, "context_compacted") },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, _, _, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
			if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
				t.Fatalf("turn started: %v", err)
			}
			if err := tc.open(m); err != nil {
				t.Fatalf("opening the axis: %v", err)
			}

			// Act.
			if err := tc.close(m); err != nil {
				t.Fatalf("closing the axis: %v", err)
			}

			// Assert — the turn is still running, so red is still right; only
			// the WORD went back to thinking.
			if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
				t.Fatalf("state = %s, want THINKING", renderName(got))
			}
		})
	}
}

func TestClearingOutranksCompactingWhenBothStand(t *testing.T) {
	// Arrange — both cuts somehow open at once. The user-initiated one is the
	// more useful report, and its rotation is about to invalidate the other.
	m, _, _, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyCompacting("ws1", true, "vendor_status:compacting"); err != nil {
		t.Fatalf("open compacting: %v", err)
	}

	// Act.
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("open clearing: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_CLEARING {
		t.Fatalf("state = %s, want CLEARING to outrank COMPACTING", renderName(got))
	}
}

func TestAContextCutOutranksThinkingButNotBlue(t *testing.T) {
	// Arrange — blue outranks everything, INCLUDING a cut: a cut running
	// behind a route the user cannot see is not something to advertise.
	m, _, _, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyCompacting("ws1", true, "vendor_status:compacting"); err != nil {
		t.Fatalf("open compacting: %v", err)
	}

	// Act.
	if err := m.ApplyConnectionDegraded("ws1", true, "no traffic"); err != nil {
		t.Fatalf("degrade: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DEGRADED {
		t.Fatalf("state = %s, want DEGRADED", renderName(got))
	}
}

func TestClosingAnAxisThatNeverOpenedAppendsNoRow(t *testing.T) {
	// Arrange — a compaction the file plane reports on a daemon that never saw
	// its status ticker. Not an error, and not a row either.
	m, cl, _, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	// Act.
	if err := m.ApplyCompacting("ws1", false, "context_compacted"); err != nil {
		t.Fatalf("close compacting: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING untouched", renderName(got))
	}
	if !cl.contains("compacting axis unchanged") {
		t.Fatal("the no-op close was not logged; a cut edge must never vanish silently")
	}
}

// ---- Failure edge: the clearing watchdog ---------------------------------

func TestAClearWhoseContextClearedNeverLandsExpires(t *testing.T) {
	// Arrange — a dispatched clear with nothing coming back for it.
	m, cl, tf, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("open clearing: %v", err)
	}

	// Act — the deadline fires.
	tf.last(t).fire()

	// Assert — the axis released, and the resolution fell back to the other
	// axes rather than staying wedged red on a word nothing can retire.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING after the watchdog released the axis", renderName(got))
	}
	if !cl.contains("CLEARING EXPIRED") {
		t.Fatal("the expiry was not loud-logged; an unaccounted-for clear must never expire quietly")
	}
}

func TestTheClearingWatchdogIsDisarmedByTheContextCleared(t *testing.T) {
	// Arrange — the normal path: the clear completes well inside the bound.
	m, _, tf, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("open clearing: %v", err)
	}
	watchdog := tf.last(t)

	// Act.
	if err := m.ApplyClearing("ws1", false, "context_cleared"); err != nil {
		t.Fatalf("close clearing: %v", err)
	}

	// Assert — a live timer would later expire an axis some LATER clear had
	// legitimately opened.
	if watchdog.armed() {
		t.Fatal("the watchdog is still armed after the clear completed")
	}
}

func TestAnExpiryAfterTheClearCompletedChangesNothing(t *testing.T) {
	// Arrange — the race: the deadline callback was already in flight when the
	// ContextCleared landed.
	m, cl, tf, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("open clearing: %v", err)
	}
	watchdog := tf.last(t)
	if err := m.ApplyClearing("ws1", false, "context_cleared"); err != nil {
		t.Fatalf("close clearing: %v", err)
	}

	// Act.
	watchdog.fire()

	// Assert — nothing to release, and no anomaly to report.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING", renderName(got))
	}
	if cl.contains("CLEARING EXPIRED") {
		t.Fatal("a completed clear was reported as expired")
	}
}

func TestReopeningAClearRenewsItsDeadline(t *testing.T) {
	// Arrange — a second `/clear` while the first is still open. Inheriting
	// the old deadline would expire the in-flight clear early.
	m, _, tf, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("first clear: %v", err)
	}
	first := tf.last(t)

	// Act.
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("second clear: %v", err)
	}

	// Assert.
	if first.armed() {
		t.Fatal("the first deadline is still armed after the clear was re-dispatched")
	}
	if second := tf.last(t); second == first || !second.armed() {
		t.Fatal("the re-dispatched clear did not arm a fresh deadline")
	}
}

func TestAnOpenClearingAxisIsRewatchedAcrossAReopen(t *testing.T) {
	// Arrange — the axis is a durable row and the watchdog is memory, so a
	// daemon restart mid-clear would otherwise leave it unexpirable.
	m, _, _, path := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("open clearing: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act — reopen over the same log.
	cl := &capLog{}
	tf := &timerFactory{}
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": "ws1"}, AfterFunc: tf.afterFunc})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })
	// The reopen marked every workspace hibernated (nothing is wired to a daemon
	// that has just started), which is orthogonal to the watchdog under test.
	wireAll(t, reopened, fakeResolver{"s1": "ws1"})

	// Assert — re-armed, and firing it still releases the axis.
	tf.last(t).fire()
	if !cl.contains("CLEARING EXPIRED") {
		t.Fatal("the re-armed watchdog did not release the axis the restart inherited")
	}
}

// ---- Failure edge: a compaction that outlives its window -----------------

func TestATurnEndClosesAnOpenCompaction(t *testing.T) {
	// Arrange — the vendor opened the window with a status ticker and never
	// closed it, and then the turn ended.
	m, cl, _, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyCompacting("ws1", true, "vendor_status:compacting"); err != nil {
		t.Fatalf("open compacting: %v", err)
	}

	// Act.
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE: a compaction cannot outlive its turn", renderName(got))
	}
	if !cl.contains("compacting window closed by turn_ended") {
		t.Fatal("the bounding close was not logged")
	}
}

func TestARotationClosesAnOpenCompaction(t *testing.T) {
	// Arrange — the ContextCompacted belongs to the retired identity and will
	// never arrive, exactly as the in-flight turn's end will not.
	m, cl, _, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyCompacting("ws1", true, "vendor_status:compacting"); err != nil {
		t.Fatalf("open compacting: %v", err)
	}

	// Act.
	if err := m.ApplySessionRotated("ws1", "uuid-old", "uuid-new"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("state = %s, want IDLE", renderName(got))
	}
	if !cl.contains("compacting window closed by session_rotated") {
		t.Fatal("the rotation did not close the compaction window")
	}
}

// ---- The rotation a /clear itself causes ---------------------------------

// These four cases replace a pair that pinned the OPPOSITE contract
// (TestClearingSurvivesTheRotationItsOwnClearCauses and
// TestTheClearingWatchdogSurvivesTheRotation). Those tests asserted the axis
// and its deadline were both held across the rotation, on the premise that "the
// ContextCleared belongs to the new identity and is still expected". Live
// evidence retired that premise: the ContextCleared has exactly one producer,
// the shim-sidecar tailing the vendor transcript, and a real `/clear` on two
// workspaces produced none at all under the new identity — both rode the full
// 60s bound into `CLEARING EXPIRED`. The rotation is the closing edge that
// exists on every clear the vendor actually performed, so it is the one
// asserted here.

func TestARotationClosesTheClearItsOwnDispatchCaused(t *testing.T) {
	// Arrange — the real sequence: `/clear` dispatched, then the vendor retires
	// the session uuid.
	m, cl, _, _ := openCutTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("open clearing: %v", err)
	}

	// Act.
	if err := m.ApplySessionRotated("ws1", "s1", "s2"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert — the axis released on the rotation rather than on a timeout, so
	// the workspace resolves on its other axes.
	if got := mustCurrent(t, m, "ws1").State; got == frontendv1.RenderState_RENDER_STATE_CLEARING {
		t.Fatal("state is still CLEARING: the rotation did not close the axis its own clear caused")
	}
	if !cl.contains("clearing axis closed by session_rotated") {
		t.Fatal("the rotation's close of the clearing axis was not logged")
	}
}

func TestARotationDisarmsTheClearingWatchdogItClosed(t *testing.T) {
	// Arrange — a deadline left live over a closed axis would later expire an
	// axis some LATER clear had legitimately opened.
	m, _, tf, _ := openCutTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("open clearing: %v", err)
	}
	watchdog := tf.last(t)

	// Act.
	if err := m.ApplySessionRotated("ws1", "s1", "s2"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert.
	if watchdog.armed() {
		t.Fatal("the clearing watchdog is still armed after the rotation closed its axis")
	}
}

func TestARotationWithNoClearInFlightLeavesTheClearingAxisAlone(t *testing.T) {
	// Arrange — a rotation the daemon did not dispatch a clear for (a resume, a
	// vendor-side re-key). The dispatch site is the axis's sole opener, so there
	// is nothing here to close.
	m, cl, _, _ := openCutTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	// Act.
	if err := m.ApplySessionRotated("ws1", "s1", "s2"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert.
	if cl.contains("clearing axis closed by session_rotated") {
		t.Fatal("a rotation with no clear in flight claimed to close the clearing axis")
	}
}

func TestAContextClearedAfterTheRotationClosedTheAxisAppendsNothing(t *testing.T) {
	// Arrange — the happy ordering still happens: the sidecar's ContextCleared
	// lands under the new identity moments after the rotation already closed the
	// axis. It must be a no-op, never a second edge.
	m, cl, _, _ := openCutTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.ApplyClearing("ws1", true, "clear_dispatched"); err != nil {
		t.Fatalf("open clearing: %v", err)
	}
	if err := m.ApplySessionRotated("ws1", "s1", "s2"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Act.
	if err := m.ApplyClearing("ws1", false, "context_cleared"); err != nil {
		t.Fatalf("close clearing: %v", err)
	}

	// Assert — reported as an unchanged axis rather than acted on.
	if !cl.contains("clearing axis unchanged") {
		t.Fatal("the late ContextCleared was not reported as a no-op over an already-closed axis")
	}
}

// ---- Argument validation --------------------------------------------------

func TestTheCutAxesRejectAnEmptyWorkspace(t *testing.T) {
	tests := []struct {
		name string
		call func(m *Manager) error
	}{
		{"ApplyClearing", func(m *Manager) error { return m.ApplyClearing("", true, "clear_dispatched") }},
		{"ApplyCompacting", func(m *Manager) error { return m.ApplyCompacting("", true, "vendor_status:compacting") }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, _, _, _ := openCutTest(t, fakeResolver{"s1": "ws1"})
			// Act.
			err := tc.call(m)
			// Assert.
			if err == nil {
				t.Fatalf("%s accepted an empty workspace", tc.name)
			}
		})
	}
}
