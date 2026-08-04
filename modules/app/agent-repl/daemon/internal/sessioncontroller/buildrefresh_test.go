package sessioncontroller

import (
	"context"
	"errors"
	"runtime"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/shim"
	"claude-repld/internal/ssm"
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
// The spawner ANNOUNCES its stops, so a test rendezvous with the bounce
// goroutine instead of spinning on its bookkeeping. The buffer keeps a stop
// from blocking a teardown no test is waiting on.
func newRefreshRig(t *testing.T, current string) (*Manager, *fakeSpawner, *fakeApplier) {
	t.Helper()
	spawner := &fakeSpawner{stops: make(chan shim.Stop, 4)}
	m, last := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, spawner)
	_ = last
	m.cfg.ShimBuildSHA = func() string { return current }
	return m, spawner, m.cfg.SSM.(*fakeApplier)
}

// waitForStop blocks until the spawner records a stop and returns the
// attribution it carried.
//
// The refresh bounce runs on its own goroutine (its caller is the shimclient
// read loop), and this is a RENDEZVOUS with that goroutine — a channel the
// spawner sends on — rather than a poll of its side effects. The bound is a
// failure bound: it exists so a bounce that never happens fails the test
// instead of hanging it.
func waitForStop(t *testing.T, spawner *fakeSpawner) shim.Stop {
	t.Helper()
	select {
	case by := <-spawner.stops:
		return by
	case <-time.After(refreshStopWait):
		t.Fatalf("no shim stop was announced within %s; the stale-build bounce never reached the spawner", refreshStopWait)
		return shim.Stop{}
	}
}

// refreshStopWait bounds the rendezvous above. It is generous because it is
// never waited out on a passing run: the receive completes the instant the
// bounce goroutine records its stop.
const refreshStopWait = 10 * time.Second

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
	m.mu.Lock()
	source := m.byWS["ws"]
	m.mu.Unlock()

	// Act.
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1"); !bounced {
		t.Fatal("a shim running a superseded bundle was not bounced")
	}
	by := waitForStop(t, spawner)

	// Assert — stopped, then brought back up on the SAME session record, which
	// is what makes the conversation survive the bounce. The stop is attributed
	// to the hard restart the refresh performs, not to a generic teardown.
	if stopped := spawner.stoppedSessions(); len(stopped) == 0 || stopped[0] != "s1" {
		t.Fatalf("stopped = %v, want the stale session s1", stopped)
	}
	if by.Initiator != "hard_restart" {
		t.Fatalf("stop initiator = %q, want hard_restart", by.Initiator)
	}
	select {
	case <-source.buildRefreshStarted:
	default:
		t.Fatal("the stale-build verdict did not publish its source-retirement edge")
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

// A SUCCESSFUL bounce is what the at-most-once latch is FOR: it records a
// refresh that actually happened, exactly once.
func TestASuccessfulStaleRefreshLatchesTheSessionOnce(t *testing.T) {
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
	waitForBuildLatch(m, "s1", true)

	// Assert — latched once, and a later mismatch is loud rather than a second
	// bounce.
	if second := m.refreshStaleShim("ws", "s1", "sha-1"); second {
		t.Fatal("a second bounce was started against an already-refreshed session")
	}
	if stops := len(spawner.stoppedSessions()); stops != 1 {
		t.Fatalf("stops = %d, want exactly 1 for a session refreshed once", stops)
	}
}

// A FAILED bounce must NOT latch, and must not be a log line either. The latch
// used to be set before the restart was known to have worked, so a stop→respawn
// that failed left the session marked "already refreshed": no second spawn, no
// state edge, forever.
func TestAFailedStaleRefreshLeavesTheLatchUnsetAndSurfacesTheFault(t *testing.T) {
	// Arrange — a live session controller whose shim cannot be stopped, so the
	// restart fails at its teardown.
	m, spawner, applier := newRefreshRig(t, "sha-2")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	spawner.mu.Lock()
	spawner.stopErr = errors.New("operation not permitted")
	spawner.mu.Unlock()

	// Act.
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1"); !bounced {
		t.Fatal("a shim running a superseded bundle was not bounced")
	}
	edge := waitForConnectivityCause(applier, staleShimRefreshFailedCause)

	// Assert — the failure reached the connectivity axis as `unavailable`, and
	// the session is NOT latched, so the refresh can be attempted again.
	if edge.state != ssm.SessionConnectivityUnavailable {
		t.Fatalf("connectivity edge state = %q, want %q", edge.state, ssm.SessionConnectivityUnavailable)
	}
	m.mu.Lock()
	latched := m.buildBounced["s1"]
	refresh := m.buildRefresh["s1"]
	inFlight := false
	if refresh != nil {
		select {
		case <-refresh.done:
		default:
			inFlight = true
		}
	}
	m.mu.Unlock()
	if latched {
		t.Fatal("a FAILED refresh latched the session as already-bounced; the workspace would never be bounced again")
	}
	if inFlight {
		t.Fatal("the in-flight marker survived a failed refresh")
	}
}

// waitForBuildLatch blocks until sessionID's success latch reaches want. It is
// a rendezvous with the bounce goroutine's own bookkeeping rather than a poll
// of a side effect it happens to produce first.
func waitForBuildLatch(m *Manager, sessionID string, want bool) {
	for {
		m.mu.Lock()
		got := m.buildBounced[sessionID]
		m.mu.Unlock()
		if got == want {
			return
		}
		runtime.Gosched()
	}
}

// waitForConnectivityCause blocks until a connectivity edge with causeKind has
// been applied, and returns it.
func waitForConnectivityCause(applier *fakeApplier, causeKind string) connectivityCall {
	for {
		for _, e := range applier.connectivityEdgesApplied() {
			if e.causeKind == causeKind {
				return e
			}
		}
		runtime.Gosched()
	}
}

// A health probe can be bound to the stale generation at the instant its
// ShimReady starts the intentional refresh. That one transport loss follows
// the recorded replacement generation; unrelated losses never do.
func TestHealthFollowsTheIntentionalStaleBuildReplacement(t *testing.T) {
	// Arrange.
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	m.mu.Lock()
	source := m.byWS["ws"]
	m.mu.Unlock()
	sourceReady := make(chan struct{})
	probeStarted := make(chan struct{}, 1)
	sourceClient := source.client.(*fakeClient)
	sourceClient.notReady = sourceReady
	sourceClient.awaitReadyStarted = probeStarted
	replacementClient := &fakeClient{}
	replacement := &sessionController{
		sessionID: "s1", workspace: "ws", generationID: "replacement",
		client: replacementClient,
	}
	refresh := &buildRefreshState{source: source, done: make(chan struct{})}
	m.mu.Lock()
	m.buildRefresh["s1"] = refresh
	m.mu.Unlock()

	// Act.
	type answer struct {
		status *corev1.HealthStatus
		err    error
	}
	done := make(chan answer, 1)
	go func() {
		status, err := m.Health(context.Background(), "ws", "s1", "health-refresh")
		done <- answer{status: status, err: err}
	}()
	<-probeStarted
	m.mu.Lock()
	m.byWS["ws"] = replacement
	close(source.buildRefreshStarted)
	close(refresh.done)
	m.mu.Unlock()

	// Assert.
	got := <-done
	if got.err != nil || !got.status.GetHealthy() || got.status.GetRequestId() != "health-refresh" {
		t.Fatalf("Health = (%+v, %v), want the replacement's correlated healthy response", got.status, got.err)
	}
	if ids := replacementClient.healthRequestIDs; len(ids) != 1 || ids[0] != "health-refresh" {
		t.Fatalf("replacement health request IDs = %v", ids)
	}
	if ids := sourceClient.healthRequestIDs; len(ids) != 0 {
		t.Fatalf("source health request IDs = %v, want none: its readiness is permanently withheld", ids)
	}
	select {
	case <-sourceReady:
		t.Fatal("the test accidentally released source readiness")
	default:
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
		t.Fatal("the workspace has no live session controller after a restart")
	}
}

// AN UNWIRED workspace is not an error: "restart" and "start" are the same
// request when nothing is running.
func TestRestartSessionBringsUpAnUnwiredWorkspace(t *testing.T) {
	// Arrange — nothing brought up.
	m, _, _ := newRefreshRig(t, "sha-1")

	// Act.
	if err := m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession on an unwired workspace: %v", err)
	}

	// Assert.
	if !m.Live("ws") {
		t.Fatal("restarting an unwired workspace did not bring it up")
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
	// Arrange — no live session controller, and a stop that cannot succeed.
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
	m.onHandshake("ws", "s1", &corev1.ShimHello{Pid: 4242, QueryInstanceId: "query-build-refresh"})

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
	m.onHandshake("ws", "s1", &corev1.ShimHello{Pid: 7, QueryInstanceId: "query-build-refresh"})

	// Act — a later hello (a reconnect from a build that does not report one).
	m.onHandshake("ws", "s1", &corev1.ShimHello{QueryInstanceId: "query-build-refresh"})

	// Assert.
	if got := m.shimPIDFor("s1"); got != 0 {
		t.Fatalf("recorded pid = %d, want 0 — a stale pid is a pid-reuse hazard, not a stop handle", got)
	}
}
