package ssm

import (
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func turnClaimEvent(start bool, seq uint64, id string) *corev1.Event {
	ev := &corev1.Event{
		SessionId: "vendor-session",
		Seq:       seq,
		Plane:     corev1.Plane_PLANE_STREAM,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId: id,
	}
	if start {
		ev.Payload = &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: id}}
	} else {
		ev.Payload = &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: id}}
	}
	return ev
}

func turnClaimBridgeEvent(seq uint64, id, previous, current string) *corev1.Event {
	return &corev1.Event{
		SessionId: current,
		Seq:       seq,
		Plane:     corev1.Plane_PLANE_STREAM,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId: id,
		Payload: &corev1.Event_TurnClaimBridge{TurnClaimBridge: &corev1.TurnClaimBridge{
			TurnId: id, PreviousSessionId: previous,
		}},
	}
}

func openTurnClaimManager(t *testing.T, path string) *Manager {
	t.Helper()
	m, err := Open(Options{
		DBPath: path,
		Logf:   func(string, ...any) {},
		Resolver: fakeResolver{
			"vendor-session": "ws",
		},
	})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	return m
}

func resolveTurnClaim(t *testing.T, m *Manager, ev *corev1.Event) (before, after []string, replayed bool) {
	t.Helper()
	before, after, replayed, err := m.ResolveTurnLifecycle("ws", "daemon-session", ev)
	if err != nil {
		t.Fatalf("ResolveTurnLifecycle(seq=%d id=%q): %v", ev.GetSeq(), turnCorrelation(ev), err)
	}
	return before, after, replayed
}

func TestTurnClaimLedgerSurvivesRestartAndReceiptsMakeCrashWindowsIdempotent(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state.db")
	first := openTurnClaimManager(t, path)

	start := turnClaimEvent(true, 12885, "turn-live")
	before, after, replayed := resolveTurnClaim(t, first, start)
	if len(before) != 0 || !reflect.DeepEqual(after, []string{"turn-live"}) || replayed {
		t.Fatalf("first start = before:%v after:%v replayed:%v", before, after, replayed)
	}
	before, after, replayed = resolveTurnClaim(t, first, start)
	if !reflect.DeepEqual(before, []string{"turn-live"}) ||
		!reflect.DeepEqual(after, []string{"turn-live"}) || !replayed {
		t.Fatalf("replayed start = before:%v after:%v replayed:%v", before, after, replayed)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("close first manager: %v", err)
	}

	second := openTurnClaimManager(t, path)
	end := turnClaimEvent(false, 12905, "turn-live")
	before, after, replayed = resolveTurnClaim(t, second, end)
	if !reflect.DeepEqual(before, []string{"turn-live"}) || len(after) != 0 || replayed {
		t.Fatalf("end after restart = before:%v after:%v replayed:%v", before, after, replayed)
	}
	before, after, replayed = resolveTurnClaim(t, second, end)
	if len(before) != 0 || len(after) != 0 || !replayed {
		t.Fatalf("replayed end = before:%v after:%v replayed:%v", before, after, replayed)
	}
	if _, _, _, err := second.ResolveTurnLifecycle(
		"ws", "daemon-session", turnClaimEvent(false, 12906, "turn-live"),
	); err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("same identity at a different end seq err = %v, want no active claim", err)
	}
	if err := second.Close(); err != nil {
		t.Fatalf("close second manager: %v", err)
	}
}

func TestTurnClaimLedgerRejectsUnprovedAndMismatchedEndsWithoutMutation(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	if _, _, _, err := m.ResolveTurnLifecycle(
		"ws", "daemon-session", turnClaimEvent(false, 1, "turn-unseen"),
	); err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("unproved end err = %v", err)
	}
	resolveTurnClaim(t, m, turnClaimEvent(true, 2, "turn-current"))
	before, after, _, err := m.ResolveTurnLifecycle(
		"ws", "daemon-session", turnClaimEvent(false, 3, "turn-other"),
	)
	if err == nil {
		t.Fatal("mismatched end succeeded")
	}
	if !reflect.DeepEqual(before, []string{"turn-current"}) ||
		!reflect.DeepEqual(after, []string{"turn-current"}) {
		t.Fatalf("mismatched end mutated ledger: before=%v after=%v", before, after)
	}
}

func TestTurnClaimLedgerRejectsEnvelopeAndHandshakeIdentityAmbiguity(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	envelopeMismatch := turnClaimEvent(true, 1, "turn-payload")
	envelopeMismatch.RequestId = "turn-envelope"
	if _, _, _, err := m.ResolveTurnLifecycle(
		"ws", "daemon-session", envelopeMismatch,
	); err == nil || !strings.Contains(err.Error(), "envelope mismatch") {
		t.Fatalf("envelope mismatch err = %v", err)
	}
	if _, _, err := m.ReconcileTurnHandshake(
		"ws", "daemon-session", []string{"turn-1", "turn-1"}, true,
	); err == nil || !strings.Contains(err.Error(), "duplicate identity") {
		t.Fatalf("duplicate handshake identity err = %v", err)
	}
	if before, after, err := m.ReconcileTurnHandshake(
		"ws", "daemon-session", nil, false,
	); err != nil {
		t.Fatalf("confirm empty ledger: %v", err)
	} else if len(before) != 0 || len(after) != 0 {
		t.Fatalf("rejected ambiguity mutated ledger: before=%v after=%v", before, after)
	}
}

func TestTurnClaimLedgerTracksQueuedIdentitiesIndependently(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	resolveTurnClaim(t, m, turnClaimEvent(true, 1, "turn-1"))
	resolveTurnClaim(t, m, turnClaimEvent(true, 2, "turn-2"))

	before, after, replayed := resolveTurnClaim(t, m, turnClaimEvent(false, 3, "turn-2"))
	if !reflect.DeepEqual(before, []string{"turn-1", "turn-2"}) ||
		!reflect.DeepEqual(after, []string{"turn-1"}) || replayed {
		t.Fatalf("out-of-order queued end = before:%v after:%v replayed:%v", before, after, replayed)
	}
	resolveTurnClaim(t, m, turnClaimEvent(false, 4, "turn-1"))
}

func TestTurnClaimLedgerCarriesStableTurnAcrossVendorSessionRotation(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state.db")
	m := openTurnClaimManager(t, path)
	oldStart := turnClaimEvent(true, 77, "turn-stable")
	oldStart.SessionId = "vendor-old"
	resolveTurnClaim(t, m, oldStart)

	bridge := turnClaimBridgeEvent(1, "turn-stable", "vendor-old", "vendor-new")
	replayed, err := m.ResolveTurnClaimBridge("ws", "daemon-session", bridge)
	if err != nil || replayed {
		t.Fatalf("first turn bridge = replayed:%v err:%v", replayed, err)
	}
	replayed, err = m.ResolveTurnClaimBridge("ws", "daemon-session", bridge)
	if err != nil || !replayed {
		t.Fatalf("same-process bridge replay = replayed:%v err:%v", replayed, err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("close before restart: %v", err)
	}

	m = openTurnClaimManager(t, path)
	replayed, err = m.ResolveTurnClaimBridge("ws", "daemon-session", bridge)
	if err != nil || !replayed {
		t.Fatalf("post-crash bridge replay = replayed:%v err:%v", replayed, err)
	}

	rotatedEnd := turnClaimEvent(false, 4, "turn-stable")
	rotatedEnd.SessionId = "vendor-new"
	before, after, endReplayed := resolveTurnClaim(t, m, rotatedEnd)
	if !reflect.DeepEqual(before, []string{"turn-stable"}) || len(after) != 0 || endReplayed {
		t.Fatalf("rotated end = before:%v after:%v replayed:%v", before, after, endReplayed)
	}
}

func TestTurnClaimBridgeProvesMissedRetiredStartWithoutSynthesizingOne(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state.db")
	first := openTurnClaimManager(t, path)
	bridge := turnClaimBridgeEvent(1, "turn-missed", "vendor-old", "vendor-new")
	if replayed, err := first.ResolveTurnClaimBridge(
		"ws", "daemon-session", bridge,
	); err != nil || replayed {
		t.Fatalf("bridge missing retired start = replayed:%v err:%v", replayed, err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("close first manager: %v", err)
	}

	second := openTurnClaimManager(t, path)
	end := turnClaimEvent(false, 2, "turn-missed")
	end.SessionId = "vendor-old"
	if _, _, _, err := second.ResolveTurnLifecycle(
		"ws", "daemon-session", end,
	); err == nil || !strings.Contains(err.Error(), `expects "vendor-new"`) {
		t.Fatalf("retired-session end err = %v, want rotated-session rejection", err)
	}
	end.SessionId = "vendor-new"
	before, after, replayed := resolveTurnClaim(t, second, end)
	if !reflect.DeepEqual(before, []string{"turn-missed"}) || len(after) != 0 || replayed {
		t.Fatalf("bridged end after restart = before:%v after:%v replayed:%v", before, after, replayed)
	}
}

func TestTurnStartedNeverActsAsRotationProof(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-old": "ws", "vendor-new": "ws"})
	start := turnClaimEvent(true, 7, "turn-real")
	start.SessionId = "vendor-old"
	resolveTurnClaim(t, m, start)

	duplicate := turnClaimEvent(true, 1, "turn-real")
	duplicate.SessionId = "vendor-new"
	if _, _, _, err := m.ResolveTurnLifecycle(
		"ws", "daemon-session", duplicate,
	); err == nil || !strings.Contains(err.Error(), "duplicate turn start identity") {
		t.Fatalf("rotated duplicate TurnStarted err = %v, want hard rejection", err)
	}
}

func TestTurnClaimHandshakeAdoptsSnapshotAndRejectsContradictionBeforeMutation(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state.db")
	first := openTurnClaimManager(t, path)
	before, after, err := first.ReconcileTurnHandshake(
		"ws", "daemon-session", []string{"turn-live"}, true,
	)
	if err != nil {
		t.Fatalf("adopt handshake: %v", err)
	}
	if len(before) != 0 || !reflect.DeepEqual(after, []string{"turn-live"}) {
		t.Fatalf("adopt handshake = before:%v after:%v", before, after)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("close first manager: %v", err)
	}

	second := openTurnClaimManager(t, path)
	before, after, err = second.ReconcileTurnHandshake(
		"ws", "daemon-session", []string{"turn-other"}, true,
	)
	if err == nil || !strings.Contains(err.Error(), "disagree") {
		t.Fatalf("contradictory handshake err = %v", err)
	}
	if !reflect.DeepEqual(before, []string{"turn-live"}) ||
		!reflect.DeepEqual(after, []string{"turn-live"}) {
		t.Fatalf("contradictory handshake mutated ledger: before=%v after=%v", before, after)
	}

	// The first streamed start binds the adopted seq=0 claim rather than
	// creating a second active identity; its eventual end remains correlated.
	before, after, replayed := resolveTurnClaim(t, second, turnClaimEvent(true, 10, "turn-live"))
	if !reflect.DeepEqual(before, []string{"turn-live"}) ||
		!reflect.DeepEqual(after, []string{"turn-live"}) || replayed {
		t.Fatalf("bind adopted start = before:%v after:%v replayed:%v", before, after, replayed)
	}
	resolveTurnClaim(t, second, turnClaimEvent(false, 11, "turn-live"))
	if err := second.Close(); err != nil {
		t.Fatalf("close second manager: %v", err)
	}
}

func TestLegacyHandshakeClaimBindsToFirstOrderedStreamStart(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	if _, after, err := m.ReconcileTurnHandshake("ws", "daemon-session", nil, true); err != nil {
		t.Fatalf("legacy handshake: %v", err)
	} else if !reflect.DeepEqual(after, []string{""}) {
		t.Fatalf("legacy active claims = %v, want one anonymous claim", after)
	}
	resolveTurnClaim(t, m, turnClaimEvent(true, 20, ""))
	resolveTurnClaim(t, m, turnClaimEvent(false, 21, ""))
}

// ---------------------------------------------------------------------------
// ReconcileTurnHandshake and the session-status lifecycle.
//
// The shim's pre-subscription snapshot is already authoritative for
// `turn_lifecycle_claim` and for the session controller's process-local latch. These tests
// pin the edge that was missing: it is authoritative for the axis too, which is
// what lets a shim coming back cure a ledger some earlier crash poisoned.
// ---------------------------------------------------------------------------

// A shim reporting no turns at all over a workspace still claiming one closes
// the claim. This is the cure for a `thinking` no restart could clear.
func TestHandshakeWithNoTurnsClosesAStaleThinking(t *testing.T) {
	// Arrange — a latched `thinking` survives into a fresh handshake.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = THINKING, want the stale claim closed by the handshake")
	}
	if !cl.contains("ssm: stale turn CLOSED ws=ws1 session=s1 reason=\"shim_handshake_no_turns\"") {
		t.Fatalf("missing the canonical handshake close record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A shim NAMING live turns is asserting the opposite, so the axis stands.
func TestHandshakeNamingLiveTurnsLeavesTheAxisAlone(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if _, _, err := m.ReconcileTurnHandshake("ws1", "s1", []string{"turn-a"}, false); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING preserved — the shim says a turn is running", renderName(got))
	}
	if cl.contains("stale turn CLOSED") {
		t.Fatalf("the axis was closed under a shim reporting a live turn; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A LEGACY shim's empty id list is not a statement that nothing is running: its
// `turn_in_flight` flag is, and it says the opposite.
func TestHandshakeWithLegacyActiveLeavesTheAxisAlone(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, true); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING preserved — a legacy shim claims a turn it cannot name", renderName(got))
	}
	if cl.contains("stale turn CLOSED") {
		t.Fatalf("the axis was closed under a legacy turn-in-flight claim; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A settled axis has nothing to close, and the handshake appends nothing to it.
func TestHandshakeOverASettledAxisAppendsNothing(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Act.
	if _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE preserved", renderName(got))
	}
	if !cl.contains("reason=\"shim_handshake_no_turns\" sole_session_controller=true — the session-status lifecycle holds no `thinking`") {
		t.Fatalf("missing the canonical no-op record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A failing close is LOGGED and never fails the handshake: DaemonHello is gated
// on this call's error, so refusing a good session over a row it merely could
// not tidy would trade a stale color for a dead workspace.
func TestHandshakeCloseFailureIsLoggedWithoutFailingTheHandshake(t *testing.T) {
	// Arrange — the session-status lifecycle table is gone, so the close cannot read it.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if _, err := m.db.Exec(`DROP TABLE workspace_state`); err != nil {
		t.Fatalf("drop workspace_state: %v", err)
	}
	// Act.
	before, after, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false)
	// Assert.
	if err != nil {
		t.Fatalf("ReconcileTurnHandshake = %v, want nil — the ledger reconciliation itself succeeded", err)
	}
	if len(before) != 0 || len(after) != 0 {
		t.Fatalf("before = %v after = %v, want both empty", before, after)
	}
	if !cl.contains("ssm: closing the stale turn on the shim handshake FAILED workspace=ws1 claimant_session=s1") {
		t.Fatalf("missing the canonical close-failure record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// THE TRAP, at the handshake. The closing row must not be swallowed by the same
// readiness guard that reads the stale row it is closing.
func TestHandshakeCloseUnblocksTheSuppressedReadiness(t *testing.T) {
	// Arrange — a latched `thinking` is suppressing readiness.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if !cl.contains("ssm: readiness suppressed (turn in flight) ws=ws1") {
		t.Fatalf("arrangement did not reproduce the suppression; log:\n%s", strings.Join(cl.lines, "\n"))
	}
	// Act — the shim comes back reporting nothing, then announces readiness.
	if _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if err := m.Apply(evSessionStarted("s1", 3)); err != nil {
		t.Fatalf("session started after the handshake: %v", err)
	}
	// Assert.
	if cl.count("ssm: readiness suppressed (turn in flight) ws=ws1") != 1 {
		t.Fatalf("readiness suppressed again; log:\n%s", strings.Join(cl.lines, "\n"))
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY", renderName(got))
	}
}
