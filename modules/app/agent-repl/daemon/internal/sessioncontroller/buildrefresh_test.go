package sessioncontroller

import (
	"context"
	"errors"
	"runtime"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/registry"
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
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1", false, nil); bounced {
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
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1", false, nil); !bounced {
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
	m.refreshStaleShim("ws", "s1", "sha-1", false, nil)

	// Act — the replacement reports the same stale identity.
	second := m.refreshStaleShim("ws", "s1", "sha-1", false, nil)

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
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1", false, nil); !bounced {
		t.Fatal("a shim running a superseded bundle was not bounced")
	}
	waitForBuildLatch(m, "s1", true)

	// Assert — latched once, and a later mismatch is loud rather than a second
	// bounce.
	if second := m.refreshStaleShim("ws", "s1", "sha-1", false, nil); second {
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
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1", false, nil); !bounced {
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
	if bounced := m.refreshStaleShim("ws", "s1", "", false, nil); bounced {
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
	if bounced := m.refreshStaleShim("ws", "s1", "sha-1", false, nil); bounced {
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

// A HARD RESTART IS THE DELIBERATE WAY OUT OF A BRING-UP PARK. The park is a
// cooldown on AUTOMATIC respawns; refusing an explicit "replace this process"
// with "it is resting" would leave the one control that exists for a wedged
// session unable to reach the session most in need of it.
func TestRestartSessionLiftsABringUpPark(t *testing.T) {
	// Arrange — the session is parked at the give-up bound with a fresh
	// cooldown, so an ordinary open would be refused.
	m, _, _ := newRefreshRig(t, "sha-1")
	m.bringUpFailures["s1"] = &bringUpStreak{
		failures:      bringUpGiveUpAfter,
		cooldown:      bringUpParkCooldown,
		parkedUntilMs: m.now() + bringUpParkCooldown.Milliseconds(),
	}
	if _, _, err := m.bringUpTracked("ws"); !errors.Is(err, ErrBringUpGaveUp) {
		t.Fatalf("bring-up while parked = %v, want ErrBringUpGaveUp", err)
	}

	// Act.
	if err := m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession on a parked session: %v", err)
	}

	// Assert.
	if !m.Live("ws") {
		t.Fatal("the hard restart did not bring the parked session up")
	}
}

// THE DEFECT THIS COVERS. A hard restart brought the session up READY while its
// durable record still claimed a sleep, so the revival gate went on refusing
// every prompt the user sent — with no revival card standing to explain why, and
// the one control that exists for a wedged session having visibly "worked".
func TestRestartSessionClearsTheRevivalGate(t *testing.T) {
	// Arrange — a session whose record says it is asleep.
	m, _, _ := newRefreshRig(t, "sha-1")
	hib := newFakeHibernations()
	m.cfg.Hibernations = hib
	hib.setAsleep("s1", registry.HibernationDetail{Cause: "idle_ttl", SinceMs: 1})

	// Act.
	if err := m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession on a hibernated session: %v", err)
	}

	// Assert — the record no longer claims a sleep, so the gate is down.
	if detail, ok := hib.HibernationOf("s1"); ok && detail.Cause != "" {
		t.Fatalf("hibernation record after the restart = %+v, want the sleep cleared", detail)
	}
}

// The gate coming down is only worth anything if a prompt then gets through:
// this asserts the OUTCOME the user experiences, not just the record write.
func TestAPromptIsAdmittedAfterARestartClearsTheGate(t *testing.T) {
	// Arrange.
	m, _, _ := newRefreshRig(t, "sha-1")
	hib := newFakeHibernations()
	m.cfg.Hibernations = hib
	hib.setAsleep("s1", registry.HibernationDetail{Cause: "idle_ttl", SinceMs: 1})
	if err := m.guardHibernation("ws", "req-1", "host", submitterUser); err == nil {
		t.Fatal("the gate admitted a prompt while the session was asleep")
	}

	// Act.
	if err := m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession: %v", err)
	}

	// Assert.
	if err := m.guardHibernation("ws", "req-2", "host", submitterUser); err != nil {
		t.Fatalf("the gate still refuses host prompts after the restart: %v", err)
	}
}

// AN AWAKE SESSION IS NOT WRITTEN TO. clearHibernation stamps a fresh turn-end
// as the policy's new measuring point, so clearing unconditionally would let an
// ordinary restart shift the idle clock of a session that was never asleep.
func TestRestartSessionDoesNotWriteHibernationForAnAwakeSession(t *testing.T) {
	// Arrange.
	m, _, _ := newRefreshRig(t, "sha-1")
	hib := newFakeHibernations()
	m.cfg.Hibernations = hib

	// Act.
	if err := m.RestartSession(context.Background(), "ws"); err != nil {
		t.Fatalf("RestartSession: %v", err)
	}

	// Assert.
	if got := hib.writeCount(); got != 0 {
		t.Fatalf("hibernation writes during an awake restart = %d, want 0", got)
	}
}

// A CLEAR THAT DID NOT LAND MUST FAIL THE RESTART. Reporting success would hand
// the user a running session the gate still refuses — exactly the state this
// clear exists to prevent — so the failure is surfaced rather than logged past.
func TestRestartSessionFailsWhenTheGateCannotBeCleared(t *testing.T) {
	// Arrange.
	m, _, _ := newRefreshRig(t, "sha-1")
	hib := newFakeHibernations()
	m.cfg.Hibernations = hib
	hib.setAsleep("s1", registry.HibernationDetail{Cause: "idle_ttl", SinceMs: 1})
	hib.writeErr = errors.New("registry down")

	// Act.
	err := m.RestartSession(context.Background(), "ws")

	// Assert.
	if err == nil {
		t.Fatal("RestartSession reported success with the revival gate still standing")
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

// The bounce ledger's witness: a teardown must be able to name the pid it is
// handing each session over with, because a successor that can only count
// processes cannot tell a survivor from a replacement.
func TestLiveShimPIDsNamesEverySessionsShim(t *testing.T) {
	// Arrange.
	m, _, _ := newRefreshRig(t, "sha-1")
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	m.onHandshake("ws", "s1", &corev1.ShimHello{Pid: 27494, QueryInstanceId: "query-build-refresh"})

	// Act.
	pids := m.LiveShimPIDs()

	// Assert.
	if pids["s1"] != 27494 {
		t.Fatalf("LiveShimPIDs() = %v, want s1 named with pid 27494", pids)
	}
}

// A session whose shim never reported a pid is ABSENT rather than recorded as
// pid 0: an unknown identity must not be judged as a death later.
func TestLiveShimPIDsOmitsASessionWithNoReportedPid(t *testing.T) {
	// Arrange.
	m, _, _ := newRefreshRig(t, "sha-1")
	m.noteShimPID("s1", 0)

	// Act.
	pids := m.LiveShimPIDs()

	// Assert.
	if _, ok := pids["s1"]; ok {
		t.Fatalf("LiveShimPIDs() = %v, want s1 omitted rather than recorded as pid 0", pids)
	}
}

// ---------------------------------------------------------------------------
// WHETHER STOPPING A SURVIVING SHIM WOULD FIX ANYTHING.
//
// A shim outlives its daemon by design, so a planned bounce that stops one
// converts a free reattach — turn and all — into an interrupted turn the
// replacement has to re-drive. The only thing a stop can fix and a reattach
// cannot is a shim executing code the deploy replaced.
// ---------------------------------------------------------------------------

// A PROVEN MATCH IS THE ONLY "NO". The shim is already running what the daemon
// would spawn today, so stopping it buys an interrupted turn and nothing else.
func TestShimStopIsDeclinedForAProvenCurrentBundle(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	m.cfg.ShimBuildSHA = func() string { return "sha-current" }
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{SessionId: "s1", BuildSha: "sha-current"})

	// Act.
	stop, reported, want := m.ShimStopWouldFixTheBundle("s1")

	// Assert.
	if stop {
		t.Fatalf("stop = true for reported=%q current=%q, want the shim preserved", reported, want)
	}
}

// A SUPERSEDED BUNDLE IS THE ONE CASE A STOP FIXES.
func TestShimStopIsIssuedForASupersededBundle(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	m.cfg.ShimBuildSHA = func() string { return "sha-current" }
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{SessionId: "s1", BuildSha: "sha-old"})

	// Act.
	stop, reported, want := m.ShimStopWouldFixTheBundle("s1")

	// Assert.
	if !stop || reported != "sha-old" || want != "sha-current" {
		t.Fatalf("stop = (%v, %q, %q), want the superseded shim stopped", stop, reported, want)
	}
}

// AN UNREADABLE IDENTITY IS STOPPED, and that is the opposite reading from the
// automatic refresh's. Somebody explicitly asked for the bundle to be replaced,
// and preserving on an identity the daemon cannot read would silently defeat
// the deploy they asked for.
func TestShimStopIsIssuedWhenAnIdentityCannotBeRead(t *testing.T) {
	tests := []struct {
		name     string
		current  string
		reported string
	}{
		{name: "the shim announced no build", current: "sha-current"},
		{name: "the checkout has no stamp", reported: "sha-old"},
		{name: "neither side is known"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, applier, _ := newWiredRig(t)
			m.cfg.ShimBuildSHA = func() string { return tc.current }
			if err := m.Ensure("ws"); err != nil {
				t.Fatalf("Ensure: %v", err)
			}
			waitForWirings(applier, 1)
			m.onConnected("ws", "s1", &corev1.ShimHello{SessionId: "s1", BuildSha: tc.reported})

			// Act.
			stop, _, _ := m.ShimStopWouldFixTheBundle("s1")

			// Assert.
			if !stop {
				t.Fatal("an unreadable bundle identity preserved the shim; the deploy that asked for the stop would reach nothing")
			}
		})
	}
}

// A SESSION NOTHING EVER HANDSHAKED FOR has no announced bundle at all, and is
// answered on the same terms rather than by an absent map entry meaning "fine".
func TestShimStopIsIssuedForASessionThatNeverHandshaked(t *testing.T) {
	// Arrange.
	m, _, _ := newWiredRig(t)
	m.cfg.ShimBuildSHA = func() string { return "sha-current" }

	// Act.
	stop, reported, _ := m.ShimStopWouldFixTheBundle("never-seen")

	// Assert.
	if !stop || reported != "" {
		t.Fatalf("stop = (%v, %q) for a session with no handshake, want it stopped with no reported build", stop, reported)
	}
}
