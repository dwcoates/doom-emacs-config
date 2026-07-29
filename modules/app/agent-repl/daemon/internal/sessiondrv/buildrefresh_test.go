package sessiondrv

import (
	"context"
	"errors"
	"runtime"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// ---------------------------------------------------------------------------
// THE STALE-SHIM REFRESH and the HARD RESTART (buildrefresh.go).
//
// A shim outlives its daemon, so a deploy does not reach it: the bundle on disk
// moves and the running process keeps executing the code it started with. These
// pin the comparator's three answers (match, mismatch, unknown), the loop guard,
// and the restart that carries them out.
// ---------------------------------------------------------------------------

// newRefreshRig builds a manager whose current bundle identity is `current`.
func newRefreshRig(t *testing.T, current string) (*Manager, *fakeSpawner, *fakeApplier) {
	t.Helper()
	spawner := &fakeSpawner{}
	m, last := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	_ = last
	m.cfg.ShimBuildSHA = func() string { return current }
	return m, spawner, m.cfg.SSM.(*fakeApplier)
}

// waitForStops blocks until the spawner has recorded at least n stops. The
// refresh bounce runs on its own goroutine (its caller is the shimclient read
// loop), so a test rendezvous with it rather than with a clock.
func waitForStops(spawner *fakeSpawner) {
	for {
		spawner.mu.Lock()
		n := len(spawner.stopped)
		spawner.mu.Unlock()
		if n > 0 {
			return
		}
		runtime.Gosched()
	}
}

// A MATCHING build is the steady state: the shim is already on current code and
// nothing happens to it.
func TestAMatchingShimBuildIsLeftAlone(t *testing.T) {
	// Arrange.
	m, spawner, applier := newRefreshRig(t, "sha-1")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)

	// Act.
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1"); bounced {
		t.Fatal("a shim already on the current bundle was bounced")
	}

	// Assert.
	spawner.mu.Lock()
	defer spawner.mu.Unlock()
	if len(spawner.stopped) != 0 {
		t.Fatalf("stopped %v; a matching build must be left alone", spawner.stopped)
	}
}

// A MISMATCH is the whole point: this shim survived a deploy and is running
// superseded code.
func TestAMismatchedShimBuildIsBounced(t *testing.T) {
	// Arrange.
	m, spawner, applier := newRefreshRig(t, "sha-2")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)

	// Act.
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1"); !bounced {
		t.Fatal("a shim running a superseded bundle was not bounced")
	}
	waitForStops(spawner)

	// Assert — stopped, then brought back up on the SAME session record, which
	// is what makes the conversation survive the bounce.
	spawner.mu.Lock()
	stopped := append([]string(nil), spawner.stopped...)
	spawner.mu.Unlock()
	if len(stopped) == 0 || stopped[0] != "s1" {
		t.Fatalf("stopped = %v, want the stale session s1", stopped)
	}
}

// AND EXACTLY ONCE. A shim that comes back still reporting a mismatch — a wrong
// stamp, an identity that cannot move — must be loud rather than restarted
// forever.
func TestAStaleShimIsBouncedOnlyOnce(t *testing.T) {
	// Arrange — already bounced.
	m, _, applier := newRefreshRig(t, "sha-2")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.refreshStaleShim("ws", "s1", "sha-1")

	// Act — the replacement reports the same stale identity.
	second := m.refreshStaleShim("ws", "s1", "sha-1")

	// Assert.
	if second {
		t.Fatal("a second bounce was started; that is the loop the latch exists to prevent")
	}
}

// AN UNKNOWN IDENTITY IS NOT A MISMATCH. A bundle built before the field
// existed reports nothing, and bouncing on that would restart every healthy
// shim a slightly older build produced.
func TestAShimWithNoBuildIdentityIsNeverBounced(t *testing.T) {
	// Arrange.
	m, spawner, applier := newRefreshRig(t, "sha-2")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)

	// Act.
	if bounced := m.refreshStaleShim("ws", "s1", ""); bounced {
		t.Fatal("a shim with no build identity was bounced; an unknown identity is not a difference")
	}

	// Assert.
	spawner.mu.Lock()
	defer spawner.mu.Unlock()
	if len(spawner.stopped) != 0 {
		t.Fatalf("stopped %v on an unknown shim identity", spawner.stopped)
	}
}

// The same, from the DAEMON's side: a checkout with no stamp must not bounce
// every shim it meets.
func TestAnAbsentBuildStampNeverBounces(t *testing.T) {
	// Arrange — no stamp at all.
	m, spawner, applier := newRefreshRig(t, "")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)

	// Act.
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1"); bounced {
		t.Fatal("a daemon with no build stamp bounced a shim it could not judge")
	}

	// Assert.
	spawner.mu.Lock()
	defer spawner.mu.Unlock()
	if len(spawner.stopped) != 0 {
		t.Fatalf("stopped %v with no stamp to compare against", spawner.stopped)
	}
}

// ---------------------------------------------------------------------------
// The hard restart
// ---------------------------------------------------------------------------

// A LIVE session is stopped and brought straight back up on the SAME record, so
// the respawn resumes the same conversation.
func TestRestartSessionStopsAndBringsItBack(t *testing.T) {
	// Arrange.
	m, spawner, applier := newRefreshRig(t, "sha-1")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)

	// Act.
	if err := m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession: %v", err)
	}

	// Assert — stopped once, and driving again afterwards.
	spawner.mu.Lock()
	stopped := append([]string(nil), spawner.stopped...)
	spawner.mu.Unlock()
	if len(stopped) == 0 || stopped[0] != "s1" {
		t.Fatalf("stopped = %v, want s1", stopped)
	}
	if !m.Live("ws") {
		t.Fatal("the workspace has no live driver after a restart")
	}
}

// A DORMANT workspace is not an error: "restart" and "start" are the same
// request when nothing is running.
func TestRestartSessionBringsUpADormantWorkspace(t *testing.T) {
	// Arrange — nothing brought up.
	m, _, _ := newRefreshRig(t, "sha-1")

	// Act.
	if err := m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession on a dormant workspace: %v", err)
	}

	// Assert.
	if !m.Live("ws") {
		t.Fatal("restarting a dormant workspace did not bring it up")
	}
}

// A workspace with NO SESSION RECORD is a loud error: there is nothing to
// restart, and reporting success would tell the user their session came back.
func TestRestartSessionFailsWithoutASession(t *testing.T) {
	// Arrange.
	m, _, _ := newRefreshRig(t, "sha-1")

	// Act.
	err := m.RestartSession(context.Background(), "unknown-ws")

	// Assert.
	if err == nil {
		t.Fatal("restarting a workspace with no session reported success")
	}
}

// A FAILED STOP fails the restart. The bring-up would refuse to spawn against
// the lock the surviving process still holds, so reporting ok would leave the
// user with exactly the shim they asked to replace.
func TestRestartSessionFailsWhenTheOrphanCannotBeStopped(t *testing.T) {
	// Arrange — no live driver, and a stop that cannot succeed.
	m, spawner, _ := newRefreshRig(t, "sha-1")
	spawner.stopErr = errors.New("operation not permitted")

	// Act.
	err := m.RestartSession(context.Background(), "ws")

	// Assert.
	if err == nil {
		t.Fatal("a restart whose shim could not be stopped reported success")
	}
}

// THE ANNOUNCED PID REACHES THE STOP. It is the only handle a daemon has on a
// shim it never spawned, so a restart that dropped it silently did nothing to
// exactly the shims that most need restarting.
func TestRestartSessionCarriesTheAnnouncedPidToTheStop(t *testing.T) {
	// Arrange — a session whose shim announced its pid on the handshake.
	m, spawner, _ := newRefreshRig(t, "sha-1")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	m.onHandshake("ws", "s1", &corev1.ShimHello{Pid: 4242})

	// Act.
	if err := m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession: %v", err)
	}

	// Assert.
	hints := spawner.stopHints()
	if len(hints) == 0 || hints[0] != 4242 {
		t.Fatalf("stop hints = %v, want the announced pid 4242 — without it a surviving shim cannot be stopped", hints)
	}
}

// A hello carrying NO pid records none, rather than recording pid 0 and asking
// the spawner to signal it.
func TestAHelloWithoutAPidRecordsNone(t *testing.T) {
	// Arrange.
	m, _, _ := newRefreshRig(t, "sha-1")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	m.onHandshake("ws", "s1", &corev1.ShimHello{Pid: 7})

	// Act — a later hello (a reconnect from a build that does not report one).
	m.onHandshake("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if got := m.shimPIDFor("s1"); got != 0 {
		t.Fatalf("recorded pid = %d, want 0 — a stale pid is a pid-reuse hazard, not a stop handle", got)
	}
}
