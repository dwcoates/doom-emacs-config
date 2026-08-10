package sessioncontroller

import (
	"strings"
	"sync"
	"testing"
	"time"
)

// restartepoch_test.go — the planned-bounce grace window.
//
// The epoch is driven off an INJECTED clock throughout: a window's length is
// the whole quantity under test, and reading a real clock would make every
// assertion here a race against the machine.

// clockedEpochRig is a Manager whose clock a test advances by hand, with a
// drain lease bound so both openers are reachable.
type clockedEpochRig struct {
	m     *Manager
	lease *fakeLease
	nowMs func(int64)
}

func newClockedEpochRig(t *testing.T) *clockedEpochRig {
	t.Helper()
	var mu sync.Mutex
	nowMs := int64(1000)
	m, _ := newClockedTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{}, func() int64 {
		mu.Lock()
		defer mu.Unlock()
		return nowMs
	})
	lease := &fakeLease{}
	if err := m.BindShutdownLease(lease); err != nil {
		t.Fatalf("BindShutdownLease: %v", err)
	}
	return &clockedEpochRig{m: m, lease: lease, nowMs: func(at int64) {
		mu.Lock()
		nowMs = at
		mu.Unlock()
	}}
}

// A DAEMON WITH NOTHING IN PROGRESS OWES NOTHING. The steady state must grant
// no grace at all, or every bound in the package is quietly loosened forever.
func TestRestartEpochIsClosedInTheSteadyState(t *testing.T) {
	// Arrange.
	r := newClockedEpochRig(t)

	// Act.
	state := r.m.restartEpochNow()

	// Assert.
	if state.open || state.graceMs != 0 {
		t.Fatalf("epoch = %+v in the steady state, want closed with no grace", state)
	}
}

// THE OUTGOING DAEMON'S OPENER. Taking the drain lease is the first instant a
// planned replacement is known to be happening.
func TestRestartEpochOpensOnTheDrainLease(t *testing.T) {
	// Arrange.
	r := newClockedEpochRig(t)

	// Act.
	r.lease.hold("sched-1")
	state := r.m.restartEpochNow()

	// Assert.
	if !state.open || !strings.Contains(state.reason, "sched-1") {
		t.Fatalf("epoch = %+v under a held drain lease, want it open and naming the schedule", state)
	}
}

// THE REPLACEMENT'S OPENER, and it is a SCOPE: the window is open for exactly
// as long as the boot reconciliation runs.
func TestRestartEpochOpensForTheBootWindow(t *testing.T) {
	// Arrange.
	r := newClockedEpochRig(t)
	var inside restartEpochState

	// Act.
	r.m.DuringBootWindow(func() { inside = r.m.restartEpochNow() })

	// Assert.
	if !inside.open || inside.reason != "boot_settling" {
		t.Fatalf("epoch inside the boot window = %+v, want it open for the boot", inside)
	}
}

// AND THE SCOPE CLOSES IT, however the reconciliation returns. An open window
// nobody closes extends every bound for the life of the process.
func TestRestartEpochBootWindowClosesOnPanic(t *testing.T) {
	// Arrange.
	r := newClockedEpochRig(t)

	// Act.
	func() {
		defer func() { _ = recover() }()
		r.m.DuringBootWindow(func() { panic("boot sweep exploded") })
	}()

	// Assert.
	if state := r.m.restartEpochNow(); state.open {
		t.Fatalf("epoch = %+v after a boot window that panicked, want it closed", state)
	}
}

// THE WINDOW'S ELAPSED IS BANKED AS GRACE. Suspending enforcement alone only
// moves the spurious failure to the instant the window closes.
func TestRestartEpochBanksTheWindowItSpanned(t *testing.T) {
	// Arrange.
	r := newClockedEpochRig(t)
	r.lease.hold("sched-1")
	r.m.restartEpochNow()

	// Act — the bounce takes 30 seconds and then ends.
	r.nowMs(31_000)
	r.lease.hold("")
	state := r.m.restartEpochNow()

	// Assert.
	if state.open || state.graceMs != 30_000 {
		t.Fatalf("epoch = %+v after a 30s bounce, want it closed with the 30s banked", state)
	}
}

// THE QUIET BETWEEN TWO BOUNCES IS NOT GRACE. A stamp left standing across a
// closed window would charge every later bound for time nothing was happening.
func TestRestartEpochExcludesTheQuietBetweenBounces(t *testing.T) {
	// Arrange — one 10s bounce, then five minutes of nothing.
	r := newClockedEpochRig(t)
	r.lease.hold("sched-1")
	r.m.restartEpochNow()
	r.nowMs(11_000)
	r.lease.hold("")
	r.m.restartEpochNow()
	r.nowMs(311_000)

	// Act — a second bounce begins.
	r.lease.hold("sched-2")
	state := r.m.restartEpochNow()

	// Assert.
	if state.graceMs != 10_000 {
		t.Fatalf("grace = %dms after one 10s bounce and five quiet minutes, want only the 10s", state.graceMs)
	}
}

// A BOUND CONSULTED MID-GAP ALREADY SEES THE GAP IT IS LIVING THROUGH, rather
// than having to wait for the window to close to learn of it.
func TestRestartEpochCountsTheOpenWindowAsItRuns(t *testing.T) {
	// Arrange.
	r := newClockedEpochRig(t)
	r.lease.hold("sched-1")
	r.m.restartEpochNow()

	// Act.
	r.nowMs(9_000)
	state := r.m.restartEpochNow()

	// Assert.
	if !state.open || state.graceMs != 8_000 || state.openElapsedMs != 8_000 {
		t.Fatalf("epoch = %+v eight seconds into an open window, want the elapsed already counted", state)
	}
}

// A DURATION BOUND STARTING INSIDE A WINDOW is extended by the window it is
// starting inside, and by nothing when there is no window.
func TestRestartExtendedBound(t *testing.T) {
	tests := []struct {
		name    string
		holdAt  int64
		readAt  int64
		holding bool
		want    time.Duration
	}{
		{name: "no window grants nothing", readAt: 1000, want: 10 * time.Second},
		{name: "an open window grants its own elapsed", holdAt: 1000, readAt: 6000, holding: true, want: 15 * time.Second},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := newClockedEpochRig(t)
			if tc.holding {
				r.nowMs(tc.holdAt)
				r.lease.hold("sched-1")
				r.m.restartEpochNow()
			}
			r.nowMs(tc.readAt)

			// Act.
			bound, _ := r.m.restartExtendedBound(10 * time.Second)

			// Assert.
			if bound != tc.want {
				t.Fatalf("extended bound = %s, want %s", bound, tc.want)
			}
		})
	}
}

// A SECOND BOOT WINDOW IS REFUSED rather than nesting: one of the two closes
// would be early and the other unreachable.
func TestRestartEpochRefusesASecondBootWindow(t *testing.T) {
	// Arrange.
	r := newClockedEpochRig(t)
	ran := false

	// Act — an inner window while the outer one is open.
	r.m.DuringBootWindow(func() {
		r.m.DuringBootWindow(func() { ran = true })
	})

	// Assert — the inner work still ran, and the outer close still landed.
	if !ran {
		t.Fatal("the refused inner boot window did not run its reconciliation; the refusal must not cost the work")
	}
	if state := r.m.restartEpochNow(); state.open {
		t.Fatalf("epoch = %+v after both windows returned, want it closed", state)
	}
}
