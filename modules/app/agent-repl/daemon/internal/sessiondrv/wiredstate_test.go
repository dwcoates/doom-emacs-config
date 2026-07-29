package sessiondrv

import (
	"errors"
	"runtime"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

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
// The driver-exit edge is written by the exit goroutine bringUp launches, so a
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
// the live driver before it reaches the axis, so a re-entrant call cannot knock
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
// already-known instant, rather than waiting for the driver exit that follows.
func TestHibernateReportsDormant(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act.
	if err := m.Hibernate("ws"); err != nil {
		t.Fatalf("Hibernate: %v", err)
	}

	// Assert.
	got := lastWiring(t, applier, "ws")
	if got.wiring != ssm.WiringDormant {
		t.Fatalf("wiring = %s, want dormant", got.wiring)
	}
	if got.reason != "hibernated" {
		t.Fatalf("reason = %q, want the hibernation named", got.reason)
	}
}

// The SESSION-SCOPED stop is hibernation's twin and closes for the same reason.
func TestHibernateSessionReportsDormant(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act.
	if err := m.HibernateSession("ws", "s1"); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}

	// Assert.
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringDormant {
		t.Fatalf("wiring = %s, want dormant", got.wiring)
	}
}

// A session-scoped stop aimed at a record that is NOT driving the workspace
// leaves the axis alone. A replacement session may already own the workspace,
// and blueing it out would be a lie about a live one — the exact shape of the
// bug session-scoped hibernation exists to avoid.
func TestASessionScopedStopOfAnotherRecordLeavesTheAxisAlone(t *testing.T) {
	// Arrange — the workspace is driven by s1.
	m, applier, _ := newWiredRig(t)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	before := len(wiringsFor(applier, "ws"))

	// Act — reap some OTHER record for the same workspace.
	if err := m.HibernateSession("ws", "s_orphan"); err != nil {
		t.Fatalf("HibernateSession: %v", err)
	}

	// Assert.
	if after := len(wiringsFor(applier, "ws")); after != before {
		t.Fatalf("edges = %d, want %d — stopping a non-driving record must not unwire the live one", after, before)
	}
}

// DRIVER EXIT is the catch-all: whatever ended Run — a terminal protocol error
// here — takes the wiring with it.
func TestDriverExitReportsDormant(t *testing.T) {
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
	if got.wiring != ssm.WiringDormant {
		t.Fatalf("wiring = %s, want dormant", got.wiring)
	}
	if got.reason != "driver_exit" {
		t.Fatalf("reason = %q, want driver_exit", got.reason)
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

// ---------------------------------------------------------------------------
// Failure surfacing
// ---------------------------------------------------------------------------

// A rejected edge is LOUD and does not abort the bring-up it rides: the color
// stops tracking the session, which is bad, and a bring-up that never completes
// because of a logging failure is worse.
func TestARejectedWiringEdgeIsLoggedNotFatal(t *testing.T) {
	// Arrange.
	m, applier, _ := newWiredRig(t)
	applier.wiredErr = errApplyWired
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
		if len(l) > 0 && l[0] == 's' && contains(l, "wired axis to the SSM FAILED") {
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
