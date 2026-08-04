package sessioncontroller

import (
	"errors"
	"runtime"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"

	"claude-repld/internal/ssm"
)

// ---------------------------------------------------------------------------
// The WIRED-AXIS PRODUCER: this package telling the SSM whether a workspace has
// a live backend session behind it.
//
// Every case drives the REAL bring-up / teardown path and asserts the edges
// that fall out of it, rather than calling noteWiring directly — the whole risk
// this axis carries is an edge that is never reached, which a direct call could
// never catch. Same shape as permstate_test.go.
// ---------------------------------------------------------------------------

// errApplyWired is the SSM refusing a wired-axis edge.
var errApplyWired = errors.New("the state log rejected the wired row")

// newWiredRig builds a manager over a fake shim and returns it with the applier
// its edges land in.
func newWiredRig(t *testing.T) (*Manager, *fakeApplier, func() *fakeClient) {
	t.Helper()
	m, last := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	return m, m.cfg.SSM.(*fakeApplier), last
}

// waitForWirings blocks until the applier has recorded at least n wired-axis
// edges, yielding the scheduler between checks.
//
// The session-controller-exit edge is written by the exit goroutine bringUp launches, so a
// test asserting it has to rendezvous with that goroutine rather than with the
// clock. Manager.Close joins the exit goroutines, which is the stronger
// rendezvous where it applies; this covers the cases that must observe an edge
// while the manager is still running.
func waitForWirings(applier *fakeApplier, n int) {
	for len(applier.wiringsApplied()) < n {
		runtime.Gosched()
	}
}

// wiringsFor returns the recorded edges for one workspace, as (wiring, reason).
func wiringsFor(applier *fakeApplier, workspace string) []wiringCall {
	var out []wiringCall
	for _, w := range applier.wiringsApplied() {
		if w.workspace == workspace {
			out = append(out, w)
		}
	}
	return out
}

// lastWiring returns the newest recorded wiring for a workspace.
func lastWiring(t *testing.T, applier *fakeApplier, workspace string) wiringCall {
	t.Helper()
	got := wiringsFor(applier, workspace)
	if len(got) == 0 {
		t.Fatalf("no wired-axis edge was applied for %q", workspace)
	}
	return got[len(got)-1]
}

// ---------------------------------------------------------------------------
// The opening edges
// ---------------------------------------------------------------------------

// A BRING-UP announces itself as `starting`, and that is the only thing it may
// claim: the shim has not answered yet.
func TestBringUpReportsStarting(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)

	// Act.
	if _, err := m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	waitForWirings(applier, 1)

	// Assert.
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringStarting {
		t.Fatalf("wiring = %s, want starting", got.wiring)
	}
}

// THE OPENING EDGE IS THE GATE'S OWN VERDICT. onConnected fires from the shim's
// ShimReady, so this is the moment — and the only moment — the axis may open.
func TestShimReadyWiresTheWorkspace(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	if _, err := m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	waitForWirings(applier, 1)

	// Act — the bring-up gate closes.
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringWired {
		t.Fatalf("wiring = %s, want wired", got.wiring)
	}
}

// A SECOND Ensure for a workspace already up writes nothing: bringUp returns
// the live session controller before it reaches the axis, so a re-entrant call cannot knock
// a wired workspace back to `starting`.
func TestASecondEnsureDoesNotReopenTheAxis(t *testing.T) {
	// Arrange — up and wired.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	before := len(wiringsFor(applier, "ws"))

	// Act.
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("second Ensure: %v", err)
	}

	// Assert.
	if after := len(wiringsFor(applier, "ws")); after != before {
		t.Fatalf("edges = %d, want %d — a re-entrant bring-up must not touch the axis", after, before)
	}
}

// ---------------------------------------------------------------------------
// The closing edges
// ---------------------------------------------------------------------------

// HIBERNATION closes it at the teardown itself, which is the earlier and
// already-known instant, rather than waiting for the session controller exit that follows.
//
// It reports HIBERNATED, not severed: this is a stop WE issued to reclaim ~500MB
// from a workspace nobody was using, so nothing broke. The earlier instant is
// exactly why the benign answer is available at all — by the time the session-controller-exit
// tail runs, the reason is gone.
func TestHibernateReportsHibernated(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert.
	got := lastWiring(t, applier, "ws")
	if got.wiring != ssm.WiringHibernated {
		t.Fatalf("wiring = %s, want hibernated", got.wiring)
	}
	if got.reason != "hibernated" {
		t.Fatalf("reason = %q, want the hibernation named", got.reason)
	}
}

// The SESSION-SCOPED stop is hibernation's twin and closes for the same reason,
// benign for the same reason, and therefore teal for the same reason.
func TestHibernateSessionReportsHibernated(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act.
	if err := m.HibernateSession("ws", "s1", StopCauseSessionDeleted()); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}

	// Assert.
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringHibernated {
		t.Fatalf("wiring = %s, want hibernated", got.wiring)
	}
}

// A session-scoped stop aimed at a record that is NOT driving the workspace
// leaves the axis alone. A replacement session may already own the workspace,
// and blueing it out would be a lie about a live one — the exact shape of the
// bug session-scoped hibernation exists to avoid.
func TestASessionScopedStopOfAnotherRecordLeavesTheAxisAlone(t *testing.T) {
	// Arrange — the workspace is controlled by s1.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	before := len(wiringsFor(applier, "ws"))

	// Act — reap some OTHER record for the same workspace.
	if err := m.HibernateSession("ws", "s_orphan", StopCauseSessionDeleted()); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}

	// Assert.
	if after := len(wiringsFor(applier, "ws")); after != before {
		t.Fatalf("edges = %d, want %d — stopping a non-driving record must not unwire the live one", after, before)
	}
}

// SESSION CONTROLLER EXIT reports SEVERED, and only for a NON-NIL runErr. `client.Run` loops
// forever across benign disconnects and returns non-nil only for a terminal
// protocol error, so a non-nil answer is genuine evidence the substrate failed —
// which is exactly what blue is for.
func TestSessionControllerExitOnATerminalErrorReportsSevered(t *testing.T) {
	// Arrange — a client whose Run can be made to return.
	runResult := make(chan error, 1)
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               &fakeApplier{},
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient: func(cfg shimclient.Config) sessionClient {
			return &fakeClient{cfg: cfg, runResult: runResult}
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	applier := m.cfg.SSM.(*fakeApplier)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act — Run ends with a terminal protocol error.
	runResult <- errors.New("protocol violation")
	waitForWirings(applier, 3)

	// Assert.
	got := lastWiring(t, applier, "ws")
	if got.wiring != ssm.WiringSevered {
		t.Fatalf("wiring = %s, want severed", got.wiring)
	}
	if got.reason != "session_controller_exit" {
		t.Fatalf("reason = %q, want session_controller_exit", got.reason)
	}
}

// A ROTATION is a BOUNCE: the shim re-handshakes, so the window between the
// announcement and the new ShimReady is a real gap in the wiring and is
// reported as one.
func TestARotationReportsTheBounce(t *testing.T) {
	// Arrange — up, wired, and a registrar that reports a rotation.
	m, applier, _ := newWiredRig(t)
	m.cfg.Registrar = &fakeRegistrar{adopted: map[string]string{"s1": "old-uuid"}}
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act.
	m.onHandshake("ws", "s1", &corev1.ShimHello{VendorSessionId: "new-uuid"})

	// Assert.
	got := lastWiring(t, applier, "ws")
	if got.wiring != ssm.WiringStarting {
		t.Fatalf("wiring = %s, want starting across the bounce", got.wiring)
	}
	if got.reason != "session_rotating" {
		t.Fatalf("reason = %q, want session_rotating", got.reason)
	}
}

// The re-handshake's own ShimReady closes the bounce window.
func TestTheReHandshakeRewiresAfterARotation(t *testing.T) {
	// Arrange — mid-bounce.
	m, applier, _ := newWiredRig(t)
	m.cfg.Registrar = &fakeRegistrar{adopted: map[string]string{"s1": "old-uuid"}}
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	m.onHandshake("ws", "s1", &corev1.ShimHello{VendorSessionId: "new-uuid"})

	// Act — the new gate closes.
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringWired {
		t.Fatalf("wiring = %s, want wired once the re-handshake completed", got.wiring)
	}
}

// A handshake that did NOT rotate leaves the axis alone: an ordinary reconnect
// is not a gap the user needs told about.
func TestAnUnrotatedHandshakeLeavesTheAxisAlone(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	m.cfg.Registrar = &fakeRegistrar{adopted: map[string]string{"s1": "same-uuid"}}
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	before := len(wiringsFor(applier, "ws"))

	// Act.
	m.onHandshake("ws", "s1", &corev1.ShimHello{VendorSessionId: "same-uuid"})

	// Assert.
	if after := len(wiringsFor(applier, "ws")); after != before {
		t.Fatalf("edges = %d, want %d — an unrotated handshake is not a wiring change", after, before)
	}
}

// A LINK LOSS WITHOUT A SESSION CONTROLLER EXIT is the fifth closing edge. The controller lives
// on across a reconnect, so without this the workspace kept claiming to be fully
// wired for the whole reconnect.
func TestALinkLossReportsStarting(t *testing.T) {
	// Arrange — up and wired.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act — the shim connection drops while the session controller lives on.
	m.onLinkLost("ws", "s1", errors.New("shim connection closed: EOF"))

	// Assert.
	got := lastWiring(t, applier, "ws")
	if got.wiring != ssm.WiringStarting {
		t.Fatalf("wiring = %s, want starting while the reconnect re-runs the gate", got.wiring)
	}
	if got.reason != "link_lost" {
		t.Fatalf("reason = %q, want link_lost", got.reason)
	}
}

// The RE-HANDSHAKE closes the gate again, so the axis returns to wired without
// anything else having to notice the reconnect finished.
func TestTheReHandshakeRewiresAfterALinkLoss(t *testing.T) {
	// Arrange — mid-reconnect.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	m.onLinkLost("ws", "s1", errors.New("shim connection closed: EOF"))

	// Act — the reconnect's own ShimReady lands.
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringWired {
		t.Fatalf("wiring = %s, want wired once the reconnect re-handshaked", got.wiring)
	}
}

// A link loss on a SUPERSEDED session leaves the axis alone: a replacement now
// drives the workspace, and re-spinning it would be a lie about a live session.
func TestALinkLossOnASupersededSessionLeavesTheAxisAlone(t *testing.T) {
	// Arrange — the workspace is controlled by s1.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	before := len(wiringsFor(applier, "ws"))

	// Act — some OTHER record's link dies.
	m.onLinkLost("ws", "s_superseded", errors.New("shim connection closed: EOF"))

	// Assert.
	if after := len(wiringsFor(applier, "ws")); after != before {
		t.Fatalf("edges = %d, want %d — a superseded link loss must not touch the live session controller's axis", after, before)
	}
}

// The edge is only reachable if the session controller actually BINDS it on the client it
// builds, which is the half a direct onLinkLost call can never prove.
func TestBringUpBindsTheLinkLossCallback(t *testing.T) {
	// Arrange.
	m, _, lastClient := newWiredRig(t)

	// Act.
	if _, err := m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}

	// Assert.
	if lastClient().cfg.OnLinkLost == nil {
		t.Fatal("the shimclient was built with no OnLinkLost; the fifth wired edge can never fire")
	}
}

// ---------------------------------------------------------------------------
// Failure surfacing
// ---------------------------------------------------------------------------

// A rejected edge is LOUD and does not abort the bring-up it rides: the color
// stops tracking the session, which is bad, and a bring-up that never completes
// because of a logging failure is worse.
func TestARejectedWiringEdgeIsLoggedNotFatal(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	applier.connectivityErr = errApplyWired
	var lines []string
	m.logf = func(format string, args ...any) { lines = append(lines, format) }

	// Act.
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure must not fail because the SSM refused a wiring row: %v", err)
	}
	waitForWirings(applier, 1)

	// Assert.
	var found bool
	for _, l := range lines {
		if len(l) > 0 && l[0] == 's' && contains(l, "connectivity edge FAILED") {
			found = true
		}
	}
	if !found {
		t.Fatalf("a refused wiring edge was not surfaced; logged: %v", lines)
	}
}

// contains is strings.Contains without the import, kept local so this file's
// intent stays obvious at a glance.
func contains(haystack, needle string) bool {
	for i := 0; i+len(needle) <= len(haystack); i++ {
		if haystack[i:i+len(needle)] == needle {
			return true
		}
	}
	return false
}

// THE TRAP THIS WHOLE DESIGN TURNS ON: a hibernation's own cancel ends
// client.Run, so the session-controller-exit tail fires on the SAME workspace milliseconds
// after the teal row lands. A tail that wrote `severed` unconditionally
// therefore repainted every single hibernation blue immediately after it went
// teal — the entire split undone by one write.
//
// The discriminator is runErr. A hibernation's cancel ends Run with nil, and nil
// is positive evidence that nothing broke, so the tail writes nothing and the
// benign answer recorded at the earlier instant stands.
func TestAHibernationSurvivesItsOwnSessionControllerExit(t *testing.T) {
	// Arrange — a live, wired controller.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act — hibernate, then join the exit goroutine the cancel released.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}
	m.Close()

	// Assert — the axis still reads the hibernation, not a session-controller-exit severance.
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringHibernated {
		t.Fatalf("wiring after the exit tail = %s/%q, want hibernated — the tail repainted a hibernation blue",
			got.wiring, got.reason)
	}
}

// ---------------------------------------------------------------------------
// The settled guard: hibernating a working workspace is mechanically impossible
// ---------------------------------------------------------------------------

// hibernateGuardRig brings a workspace up and wires it, returning the manager and
// the applier whose resolved state the guard reads.
func hibernateGuardRig(t *testing.T) (*Manager, *fakeApplier) {
	t.Helper()
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	return m, applier
}

// A LIVE TURN IS REFUSED. The guard is inside the shared teardown rather than in
// the idle sweeper, which is what makes this hold for the frontend command and
// every future caller too and not only for the sweeper that happens to gate
// itself.
func TestHibernateRefusesALiveTurn(t *testing.T) {
	// Arrange.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_THINKING,
		TurnActive: true,
	})

	// Act.
	err := m.Hibernate("ws", StopCauseHibernateIdleSweep())

	// Assert.
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("Hibernate over a live turn = %v, want ErrNotSettled", err)
	}
}

// AND IT REFUSES BEFORE TOUCHING ANYTHING. A refusal that had already evicted the
// controller or SIGTERMed the shim would be a hibernation with an error attached
// rather than a hibernation that did not happen.
func TestARefusedHibernationLeavesTheSessionRunning(t *testing.T) {
	// Arrange.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_THINKING,
		TurnActive: true,
	})
	before := len(wiringsFor(applier, "ws"))

	// Act.
	if err := m.Hibernate("ws", StopCauseHibernateIdleSweep()); !errors.Is(err, ErrNotSettled) {
		t.Fatalf("Hibernate = %v, want ErrNotSettled", err)
	}

	// Assert — the axis never moved, so nothing was torn down.
	if after := len(wiringsFor(applier, "ws")); after != before {
		t.Fatalf("edges = %d, want %d — a refused hibernation must not move the axis", after, before)
	}
}

// A CONTEXT CUT IS REFUSED for the same reason a plain turn is: both are red,
// both mean a turn is in flight, and only the word distinguishes what the agent
// is busy with.
func TestHibernateRefusesAContextCut(t *testing.T) {
	// Arrange.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_CLEARING})

	// Act.
	err := m.Hibernate("ws", StopCauseHibernateIdleSweep())

	// Assert.
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("Hibernate over a clearing workspace = %v, want ErrNotSettled", err)
	}
}

// A VENDOR BLOCK IS REFUSED, which is the less obvious half. Nothing is running
// under purple — it is a turn OUTCOME — but it is the one state whose whole
// purpose is to tell the user something needs their attention, and reaping the
// session replaces that with a teal tab claiming everything is fine and asleep.
func TestHibernateRefusesAVendorBlockedWorkspace(t *testing.T) {
	// Arrange.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED})

	// Act.
	err := m.Hibernate("ws", StopCauseHibernateIdleSweep())

	// Assert.
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("Hibernate over a vendor-blocked workspace = %v, want ErrNotSettled", err)
	}
}

// A SETTLED WORKSPACE IS HIBERNATED, which is the whole point of the knob: a
// guard that refused everything would just disable hibernation.
func TestHibernateAllowsASettledWorkspace(t *testing.T) {
	// Arrange.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_READY})

	// Act.
	err := m.Hibernate("ws", StopCauseHibernateIdleSweep())

	// Assert.
	if err != nil {
		t.Fatalf("Hibernate over a settled workspace = %v, want it allowed", err)
	}
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringHibernated {
		t.Fatalf("wiring = %s, want hibernated", got.wiring)
	}
}

// A STATE READ FAILURE ALLOWS THE TEARDOWN, and this is the one place in the
// hibernation path where an unknown answers YES. This guard only VETOES a
// teardown somebody already decided on, so turning a read outage into a refusal
// would silently disable hibernation daemon-wide — and memory exhaustion is a
// worse failure than one shim reaped a moment early. The failure is logged
// loudly either way.
func TestHibernateProceedsWhenTheStateReadFails(t *testing.T) {
	// Arrange.
	m, applier := hibernateGuardRig(t)
	applier.reconcMutex.Lock()
	applier.currentErr = errors.New("the state log is unreadable")
	applier.reconcMutex.Unlock()

	// Act.
	err := m.Hibernate("ws", StopCauseHibernateIdleSweep())

	// Assert.
	if err != nil {
		t.Fatalf("Hibernate with an unreadable state = %v, want it allowed", err)
	}
}

// THE SESSION-SCOPED STOP IS NOT GUARDED, and the asymmetry is deliberate.
// Hibernate is a memory optimization nobody asked for, so it must never cost a
// live turn; HibernateSession serves DeleteSession and the supersede sweep, where
// the caller has decided this exact record must go and a mid-turn refusal would
// leave an orphan shim running forever.
func TestHibernateSessionIsNotGatedOnSettledness(t *testing.T) {
	// Arrange — a workspace mid-turn.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_THINKING,
		TurnActive: true,
	})

	// Act.
	err := m.HibernateSession("ws", "s1", StopCauseSessionDeleted())

	// Assert.
	if err != nil {
		t.Fatalf("HibernateSession over a live turn = %v, want it delivered", err)
	}
}
