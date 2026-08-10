package ssm

import (
	"errors"
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
		Resolver: vendorAliasResolver{
			workspace: "ws",
			daemonID:  "daemon-session",
			vendorIDs: []string{"vendor-session", "vendor-old", "vendor-new", "vendor-other"},
		},
	})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	return m
}

func resolveTurnClaim(t *testing.T, m *Manager, ev *corev1.Event) (before, after []string, replayed bool) {
	t.Helper()
	before, after, replayed, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", ev)
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
		"ws", "daemon-session", "", turnClaimEvent(false, 12906, "turn-live"),
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
		"ws", "daemon-session", "", turnClaimEvent(false, 1, "turn-unseen"),
	); err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("unproved end err = %v", err)
	}
	resolveTurnClaim(t, m, turnClaimEvent(true, 2, "turn-current"))
	before, after, _, err := m.ResolveTurnLifecycle(
		"ws", "daemon-session", "", turnClaimEvent(false, 3, "turn-other"),
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
		"ws", "daemon-session", "", envelopeMismatch,
	); err == nil || !strings.Contains(err.Error(), "envelope mismatch") {
		t.Fatalf("envelope mismatch err = %v", err)
	}
	if _, _, _, err := m.ReconcileTurnHandshake(
		"ws", "daemon-session", []string{"turn-1", "turn-1"}, true, nil,
	); err == nil || !strings.Contains(err.Error(), "duplicate identity") {
		t.Fatalf("duplicate handshake identity err = %v", err)
	}
	if before, after, _, err := m.ReconcileTurnHandshake(
		"ws", "daemon-session", nil, false, nil,
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
		"ws", "daemon-session", "", end,
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
		"ws", "daemon-session", "", duplicate,
	); err == nil || !strings.Contains(err.Error(), "duplicate turn start identity") {
		t.Fatalf("rotated duplicate TurnStarted err = %v, want hard rejection", err)
	}
}

func TestTurnClaimHandshakeAdoptsSnapshotAndRejectsContradictionBeforeMutation(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state.db")
	first := openTurnClaimManager(t, path)
	before, after, _, err := first.ReconcileTurnHandshake(
		"ws", "daemon-session", []string{"turn-live"}, true, nil,
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
	before, after, _, err = second.ReconcileTurnHandshake(
		"ws", "daemon-session", []string{"turn-other"}, true, nil,
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
	if _, after, _, err := m.ReconcileTurnHandshake("ws", "daemon-session", nil, true, nil); err != nil {
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
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if _, _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, nil); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = THINKING, want the stale claim closed by the handshake")
	}
	if !cl.contains("ssm: stale turn CLOSED ws=ws1 session=s1 reason=\"" + TurnCloseRestartInterrupted + "\"") {
		t.Fatalf("missing the canonical handshake close record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A shim NAMING live turns is asserting the opposite, so the axis stands.
func TestHandshakeNamingLiveTurnsLeavesTheAxisAlone(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "turn-a", 1)
	// Act.
	if _, _, _, err := m.ReconcileTurnHandshake("ws1", "s1", []string{"turn-a"}, false, nil); err != nil {
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
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if _, _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, true, nil); err != nil {
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
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := applyTest(m, evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Act.
	if _, _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, nil); err != nil {
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
	before, after, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, nil)
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
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := applyTest(m, evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if !cl.contains("ssm: readiness suppressed (turn in flight) ws=ws1") {
		t.Fatalf("arrangement did not reproduce the suppression; log:\n%s", strings.Join(cl.lines, "\n"))
	}
	// Act — the shim comes back reporting nothing, then announces readiness.
	if _, _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, nil); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if err := applyTest(m, evSessionStarted("s1", 3)); err != nil {
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

// ---------------------------------------------------------------------------
// The PHANTOM turn: a durable claim contradicted by the live shim.
//
// The claim and the shim are two authorities on "is a turn in flight", and only
// the shim can see the process the turn would be running in. These tests pin
// the reconciling edge, and pin just as hard that it fires ONLY where the
// contradiction is unambiguous.
// ---------------------------------------------------------------------------

// claimTurn seeds one durable turn claim held by claimant for ws.
func claimTurn(t *testing.T, m *Manager, ws, claimant, vendorSession, id string, seq uint64) {
	t.Helper()
	ev := turnClaimEvent(true, seq, id)
	ev.SessionId = vendorSession
	if _, _, _, err := m.ResolveTurnLifecycle(ws, claimant, "", ev); err != nil {
		t.Fatalf("seed durable turn claim %q: %v", id, err)
	}
}

// TestActiveTurnIDsNamesTheSessionsOpenClaim covers the ledger READ the drain
// hold names an accepted-but-not-yet-observed turn from: process memory cannot
// answer that question and the ledger can.
func TestActiveTurnIDsNamesTheSessionsOpenClaim(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "daemon-prompt-1", 1)

	// Act.
	ids, err := m.ActiveTurnIDs("ws1", "s1")

	// Assert.
	if err != nil {
		t.Fatalf("ActiveTurnIDs: %v", err)
	}
	if !reflect.DeepEqual(ids, []string{"daemon-prompt-1"}) {
		t.Fatalf("ActiveTurnIDs = %v, want the open claim", ids)
	}
}

// TestActiveTurnIDsReportsNothingForASessionHoldingNoClaim pins that holding
// nothing is an ANSWER rather than a failure: a hold with no name is decided by
// the caller, and an error here would make an empty ledger indistinguishable
// from an unreadable one.
func TestActiveTurnIDsReportsNothingForASessionHoldingNoClaim(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	ids, err := m.ActiveTurnIDs("ws1", "s1")

	// Assert.
	if err != nil {
		t.Fatalf("ActiveTurnIDs: %v", err)
	}
	if len(ids) != 0 {
		t.Fatalf("ActiveTurnIDs = %v, want none", ids)
	}
}

// TestActiveTurnIDsRefusesAnUnidentifiedSession keeps the read from answering
// for a caller that named neither the workspace nor the session.
func TestActiveTurnIDsRefusesAnUnidentifiedSession(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	_, err := m.ActiveTurnIDs("ws1", "")

	// Assert.
	if err == nil {
		t.Fatal("ActiveTurnIDs with no claimant session id = nil error, want a refusal")
	}
}

func TestHandshakeReportingNoTurnClosesThePhantomClaim(t *testing.T) {
	// Arrange — a claim survives a restart the shim knows nothing about.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "daemon-prompt-1", 1)

	// Act — the returning shim reports no turn in flight and no turn ids.
	_, after, closed, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, nil)

	// Assert.
	if err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if !reflect.DeepEqual(closed, []string{"daemon-prompt-1"}) {
		t.Fatalf("closed = %v, want the phantom claim", closed)
	}
	if len(after) != 0 {
		t.Fatalf("durable active turns = %v, want none — the shim says nothing is running", after)
	}
}

func TestHandshakePhantomCloseRecordsTheRestartInterruptCause(t *testing.T) {
	// Arrange — the user must be able to see the turn was CUT, not merely lost.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	claimTurn(t, m, "ws1", "s1", "s1", "daemon-prompt-1", 2)

	// Act.
	if _, _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, nil); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetCauseKind(); got != causeShimStopped+":"+TurnCloseRestartInterrupted {
		t.Fatalf("cause_kind = %q, want the restart interrupt named on the workspace state", got)
	}
	if !cl.contains("ssm: turn claims INTERRUPTED BY RESTART workspace=ws1 claimant_session=s1 closed=[<legacy>,daemon-prompt-1]") {
		t.Fatalf("missing the loud restart-interrupt record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestHandshakeSparesTheClaimTheStoreProvesCompleted(t *testing.T) {
	// Arrange — a turn that FINISHED during the daemon gap. The hello is silent
	// about it exactly as it is silent about a cut one, and only the durable
	// terminal record tells the two apart.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "daemon-prompt-1", 1)

	// Act.
	_, after, closed, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, []string{"daemon-prompt-1"})

	// Assert.
	if err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if len(closed) != 0 {
		t.Fatalf("closed = %v, want none — the store's record outranks the hello's silence", closed)
	}
	if !reflect.DeepEqual(after, []string{"daemon-prompt-1"}) {
		t.Fatalf("durable active turns = %v, want the claim held open for its own replayed TurnEnded", after)
	}
}

func TestHandshakeCutsOnlyTheClaimTheStoreCannotProve(t *testing.T) {
	// Arrange — one completed turn beside one genuinely cut turn.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "turn-finished", 1)
	claimTurn(t, m, "ws1", "s1", "s1", "turn-cut", 2)

	// Act.
	_, after, closed, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, []string{"turn-finished"})

	// Assert.
	if err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if !reflect.DeepEqual(closed, []string{"turn-cut"}) {
		t.Fatalf("closed = %v, want only the claim with no durable terminal evidence", closed)
	}
	if !reflect.DeepEqual(after, []string{"turn-finished"}) {
		t.Fatalf("durable active turns = %v, want the durably-ended claim still open", after)
	}
}

func TestHandshakeSparingAClaimLeavesTheStatusAxisToItsOwnBoundary(t *testing.T) {
	// Arrange — the stale-turn tidy retires `thinking` on a handshake that
	// finds nothing running. A claim deliberately held open is still running as
	// far as every reader is concerned until its own end replays.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	claimTurn(t, m, "ws1", "s1", "s1", "daemon-prompt-1", 2)

	// Act.
	if _, _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, []string{"daemon-prompt-1"}); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}

	// Assert.
	if !mustCurrent(t, m, "ws1").GetTurnActive() {
		t.Fatal("turn_active = false, want the axis left standing for the spared claim's own replayed boundary")
	}
}

func TestHandshakeSparingAnAbsentClaimStillCutsTheStandingOne(t *testing.T) {
	// Arrange — evidence naming a turn this claimant does not hold must not
	// spare a different claim by accident.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "turn-standing", 1)

	// Act.
	_, after, closed, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, []string{"turn-elsewhere"})

	// Assert.
	if err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if !reflect.DeepEqual(closed, []string{"turn-standing"}) {
		t.Fatalf("closed = %v, want the standing claim cut — nothing proved anything about it", closed)
	}
	if len(after) != 0 {
		t.Fatalf("durable active turns = %v, want none", after)
	}
}

func TestHandshakeConfirmingItsOwnClaimClosesNothing(t *testing.T) {
	// Arrange — the shim names the very turn the ledger holds.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "turn-live", 1)

	// Act.
	_, after, closed, err := m.ReconcileTurnHandshake("ws1", "s1", []string{"turn-live"}, true, nil)

	// Assert.
	if err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if len(closed) != 0 {
		t.Fatalf("closed = %v, want none — the shim CONFIRMS the claim", closed)
	}
	if !reflect.DeepEqual(after, []string{"turn-live"}) {
		t.Fatalf("durable active turns = %v, want the confirmed claim intact", after)
	}
}

func TestLegacyHandshakeClaimingATurnClosesNothing(t *testing.T) {
	// Arrange — an empty id list under turn_in_flight=true says nothing at all
	// about which turn is running, only that one IS.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "turn-live", 1)

	// Act.
	_, after, closed, err := m.ReconcileTurnHandshake("ws1", "s1", nil, true, nil)

	// Assert.
	if err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if len(closed) != 0 || !reflect.DeepEqual(after, []string{"turn-live"}) {
		t.Fatalf("closed = %v after = %v, want the legacy claim believed", closed, after)
	}
}

func TestSynthesizeTurnCloseEndsTheHeldClaim(t *testing.T) {
	// Arrange — the ALREADY_COMPLETE Ack's ledger half.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "turn-held", 1)

	// Act.
	closed, err := m.SynthesizeTurnClose("ws1", "s1", TurnCloseAlreadyComplete)

	// Assert.
	if err != nil {
		t.Fatalf("SynthesizeTurnClose: %v", err)
	}
	if !reflect.DeepEqual(closed, []string{"turn-held"}) {
		t.Fatalf("closed = %v, want the held claim", closed)
	}
}

func TestSynthesizeTurnCloseWithNoClaimIsABenignNoOp(t *testing.T) {
	// Arrange — the turn's own end got there first, which is the ordinary case.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	closed, err := m.SynthesizeTurnClose("ws1", "s1", TurnCloseAlreadyComplete)

	// Assert.
	if err != nil {
		t.Fatalf("SynthesizeTurnClose = %v, want nil on an empty ledger", err)
	}
	if len(closed) != 0 {
		t.Fatalf("closed = %v, want none", closed)
	}
}

func TestSynthesizeTurnCloseRejectsAnUnnamedCause(t *testing.T) {
	// Arrange — a synthesized end must name the observation that authorized it.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "turn-held", 1)

	// Act.
	closed, err := m.SynthesizeTurnClose("ws1", "s1", "because")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "must name one of the observations") {
		t.Fatalf("SynthesizeTurnClose err = %v, want a loud refusal", err)
	}
	if len(closed) != 0 {
		t.Fatalf("closed = %v, want none on a refused cause", closed)
	}
}

func TestSynthesizeTurnCloseRefusesAnEmptyClaimant(t *testing.T) {
	// Arrange — a claim is only ever ended on behalf of the session holding it.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	_, err := m.SynthesizeTurnClose("ws1", "", TurnCloseAlreadyComplete)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "empty claimant session id") {
		t.Fatalf("SynthesizeTurnClose err = %v, want a loud refusal", err)
	}
}

func TestGenuineTurnEndAfterASynthesizedCloseIsAdmittedAsAReplay(t *testing.T) {
	// Arrange — the pre-restart turn's own end, replayed off the store after the
	// subscription reopened. It reports what the synthesized close already
	// recorded, so it is accounted for rather than read as a contradiction.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	claimTurn(t, m, "ws1", "s1", "s1", "turn-cut", 1)
	if _, _, _, err := m.ReconcileTurnHandshake("ws1", "s1", nil, false, nil); err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	end := turnClaimEvent(false, 2, "turn-cut")
	end.SessionId = "s1"

	// Act.
	_, after, replayed, err := m.ResolveTurnLifecycle("ws1", "s1", "", end)

	// Assert.
	if err != nil {
		t.Fatalf("ResolveTurnLifecycle = %v, want the already-accounted boundary admitted", err)
	}
	if !replayed {
		t.Fatal("late genuine end reported as a first delivery, want an already-accounted replay")
	}
	if len(after) != 0 {
		t.Fatalf("durable active turns = %v, want none", after)
	}
}

func TestUnknownTurnEndWithoutAnyClaimIsStillRejected(t *testing.T) {
	// Arrange — the synthesized-close tolerance must not become a blanket
	// amnesty for ends nothing ever claimed.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	end := turnClaimEvent(false, 2, "turn-unknown")
	end.SessionId = "s1"

	// Act.
	_, _, _, err := m.ResolveTurnLifecycle("ws1", "s1", "", end)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("ResolveTurnLifecycle err = %v, want the unclaimed end rejected", err)
	}
}

// ---------------------------------------------------------------------------
// The DEAD-CLAIM bridge: rotation proof that arrives for a turn already closed.
//
// The live incident: a dispatched /clear held turn daemon-prompt-2-…, restart
// reconciliation closed that claim (end_seq=0), and the /clear's own vendor-session
// rotation then produced a bridge for it. The bridge was refused — correctly —
// but the refusal was escalated as a protocol violation, which severed the shim
// link, ended the session controller, and left last_seen_seq parked BEFORE the
// bridge, so the shim replayed it into the next session and killed that one too.
//
// These tests pin the split: a refusal against a CLOSED claim is recoverable,
// a refusal against a LIVE one stays a protocol violation.
// ---------------------------------------------------------------------------

func TestTurnClaimBridgeClassifiesRefusalByWhetherTheClaimIsStillLive(t *testing.T) {
	tests := []struct {
		name     string
		seed     func(t *testing.T, m *Manager)
		bridge   *corev1.Event
		wantDead bool
		wantMsg  string
	}{
		{
			name: "closed claim is a dead-epoch refusal the session can survive",
			seed: func(t *testing.T, m *Manager) {
				t.Helper()
				claimTurn(t, m, "ws", "daemon-session", "vendor-old", "daemon-prompt-2", 5)
				// The restart reconciliation that closed the claim with end_seq=0.
				if _, _, closed, err := m.ReconcileTurnHandshake("ws", "daemon-session", nil, false, nil); err != nil {
					t.Fatalf("close the phantom claim: %v", err)
				} else if !reflect.DeepEqual(closed, []string{"daemon-prompt-2"}) {
					t.Fatalf("closed = %v, want the seeded claim", closed)
				}
			},
			bridge:   turnClaimBridgeEvent(6, "daemon-prompt-2", "vendor-old", "vendor-new"),
			wantDead: true,
			wantMsg:  "conflicts with completed claim end_seq=0",
		},
		{
			name: "live claim already bridged elsewhere stays a protocol violation",
			seed: func(t *testing.T, m *Manager) {
				t.Helper()
				claimTurn(t, m, "ws", "daemon-session", "vendor-old", "daemon-prompt-2", 5)
				first := turnClaimBridgeEvent(6, "daemon-prompt-2", "vendor-old", "vendor-new")
				if replayed, err := m.ResolveTurnClaimBridge("ws", "daemon-session", first); err != nil || replayed {
					t.Fatalf("seed accepted bridge = replayed:%v err:%v", replayed, err)
				}
			},
			bridge:   turnClaimBridgeEvent(7, "daemon-prompt-2", "vendor-old", "vendor-other"),
			wantDead: false,
			wantMsg:  "conflicts with durable event_session",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
			tc.seed(t, m)

			// Act.
			replayed, err := m.ResolveTurnClaimBridge("ws", "daemon-session", tc.bridge)

			// Assert.
			if err == nil || replayed {
				t.Fatalf("ResolveTurnClaimBridge = replayed:%v err:%v, want a refusal", replayed, err)
			}
			if !strings.Contains(err.Error(), tc.wantMsg) {
				t.Fatalf("refusal = %v, want it to name %q", err, tc.wantMsg)
			}
			if got := errors.Is(err, ErrTurnBridgeDeadClaim); got != tc.wantDead {
				t.Fatalf("errors.Is(ErrTurnBridgeDeadClaim) = %v, want %v (err=%v)", got, tc.wantDead, err)
			}
		})
	}
}

func TestDeadClaimBridgeRefusalLeavesTheClosedClaimUntouched(t *testing.T) {
	// Arrange — a closed claim, then the bridge that arrives too late for it.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
	claimTurn(t, m, "ws", "daemon-session", "vendor-old", "daemon-prompt-2", 5)
	if _, _, _, err := m.ReconcileTurnHandshake("ws", "daemon-session", nil, false, nil); err != nil {
		t.Fatalf("close the phantom claim: %v", err)
	}

	// Act.
	if _, err := m.ResolveTurnClaimBridge(
		"ws", "daemon-session", turnClaimBridgeEvent(6, "daemon-prompt-2", "vendor-old", "vendor-new"),
	); !errors.Is(err, ErrTurnBridgeDeadClaim) {
		t.Fatalf("bridge err = %v, want the dead-claim refusal", err)
	}

	// Assert — the refusal recorded nothing: the claim stays closed, not active.
	before, after, closed, err := m.ReconcileTurnHandshake("ws", "daemon-session", nil, false, nil)
	if err != nil {
		t.Fatalf("ReconcileTurnHandshake: %v", err)
	}
	if len(before) != 0 || len(after) != 0 || len(closed) != 0 {
		t.Fatalf("ledger after refusal = before:%v after:%v closed:%v, want no active claim — a refused bridge must not revive a closed one",
			before, after, closed)
	}
}

// --- turn ends and the query that produced them -------------------------------
//
// The four cases every provenance consumer owes, at the durable claim ledger.

// stampedEnd is a TurnEnded for ID produced by ENVELOPEQUERY.
func stampedEnd(seq uint64, id, envelopeQuery string) *corev1.Event {
	ev := turnClaimEvent(false, seq, id)
	ev.QueryInstanceId = envelopeQuery
	return ev
}

// (a) A claimless end PRODUCED BY the live query is a genuine contradiction and
// stays fatal, exactly as it was before provenance existed.
func TestClaimlessEndFromTheLiveQueryStaysFatal(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})

	// Act
	_, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "live-query",
		stampedEnd(1, "turn-unclaimed", "live-query"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("claimless live end = %v, want the hard rejection", err)
	}
}

// (b) A claimless end produced by a RETIRED query is history: the invocation
// that owned the turn is gone, so there is no live claim it could belong to.
func TestClaimlessEndFromARetiredQueryIsHistory(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})

	// Act
	_, _, replayed, err := m.ResolveTurnLifecycle("ws", "daemon-session", "live-query",
		stampedEnd(1, "turn-unclaimed", "retired-query"))

	// Assert
	if err != nil {
		t.Fatalf("claimless retired end = %v, want it accepted as history", err)
	}
	if !replayed {
		t.Fatal("a historical end was not reported as a replay")
	}
}

// (b) And it writes NOTHING: the ledger is left exactly as it was found.
func TestClaimlessEndFromARetiredQueryMutatesNoLedgerRow(t *testing.T) {
	// Arrange -- one live turn open, so there is state that could be corrupted.
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	resolveTurnClaim(t, m, turnClaimEvent(true, 1, "turn-live"))

	// Act
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "live-query",
		stampedEnd(2, "turn-unclaimed", "retired-query")); err != nil {
		t.Fatalf("claimless retired end: %v", err)
	}

	// Assert
	after, err := m.ActiveTurnIDs("ws", "daemon-session")
	if err != nil {
		t.Fatalf("ActiveTurnIDs: %v", err)
	}
	if len(after) != 1 || after[0] != "turn-live" {
		t.Fatalf("active turns = %v, want the live claim untouched by a historical end", after)
	}
}

// (c) EMPTY FAILS CLOSED. A producer that predates query_instance_id keeps
// precisely the strict behavior it had before the field existed.
func TestUnstampedClaimlessEndStaysFatal(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})

	// Act
	_, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "live-query",
		stampedEnd(1, "turn-unclaimed", ""))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("unstamped claimless end = %v, want it judged exactly as a live row is", err)
	}
}

// (d) THE STARTUP CASE. A row the session wrote before its subscription could
// exist arrives during catch-up at a low seq, but it names the query running
// right now -- so it is LIVE on ONE comparison and still gets the full check.
func TestCatchUpDeliveredEndFromTheLiveQueryIsStillChecked(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})

	// Act -- seq 1, beneath everything, exactly how catch-up delivers it.
	_, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "live-query",
		stampedEnd(1, "turn-unclaimed", "live-query"))

	// Assert -- delivery order bought it no exemption.
	if err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("catch-up-delivered live end = %v, want the hard rejection", err)
	}
}

// A caller with no bound query has nothing to compare against, so the check
// stays strict.
func TestClaimlessEndWithNoBoundQueryStaysFatal(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})

	// Act
	_, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "",
		stampedEnd(1, "turn-unclaimed", "retired-query"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("claimless end with no bound query = %v, want the hard rejection", err)
	}
}

// ---------------------------------------------------------------------------
// THE CLAIMANT OUTLIVES ITS CLAIM. A workspace's daemon session id (claimant)
// is re-minted on every CreateSession — hibernate, revive, and reopen all
// mint a fresh one for the SAME underlying vendor conversation — but
// turn_lifecycle_claim rows stay keyed to whichever claimant opened them and
// are never rebound. The live incident: workspace slack-ceac-tech-xfq's turn
// "daemon-prompt-1-8550144961a9" opened under claimant s_cdc521e7a5647643,
// its real TurnEnded (seq=171) arrived and was never durably closed, the
// workspace hibernated and later revived under a brand-new claimant, and
// every subsequent bring-up replayed that same TurnEnded under the NEW
// claimant — finding zero rows under (workspace, new-claimant, turn_id) and
// rejecting "has no durable active claim" forever, permanently denying the
// workspace.
// ---------------------------------------------------------------------------

// TestTurnEndForAPriorGenerationClaimantIsAdmitted is the GUARANTEE: a
// TurnEnded whose OWN claimant generation holds nothing for this turn id is
// resolved against the workspace's claim under a RETIRED claimant, once the
// event's own vendor session proves it is the same durable boundary.
func TestTurnEndForAPriorGenerationClaimantIsAdmitted(t *testing.T) {
	// Arrange — the claim opens under a RETIRED claimant generation.
	m, cl, _ := openTest(t, fakeResolver{"vend1": "ws"})
	claimTurn(t, m, "ws", "s-old", "vend1", "turn-x", 1)

	end := turnClaimEvent(false, 5, "turn-x")
	end.SessionId = "vend1"

	// Act — a NEW claimant generation (revival after hibernation) replays the
	// turn's own end.
	_, _, replayed, err := m.ResolveTurnLifecycle("ws", "s-new", "", end)

	// Assert.
	if err != nil {
		t.Fatalf("ResolveTurnLifecycle = %v, want the prior generation's claim resolved", err)
	}
	if replayed {
		t.Fatal("first delivery of the real end reported as replayed")
	}
	oldActive, err := m.ActiveTurnIDs("ws", "s-old")
	if err != nil {
		t.Fatalf("ActiveTurnIDs(s-old): %v", err)
	}
	if len(oldActive) != 0 {
		t.Fatalf("retired claimant's active turns = %v, want the claim closed", oldActive)
	}
	if !cl.contains("CROSS-GENERATION CLAIM MATCH") {
		t.Fatal("cross-generation admission was not logged loudly")
	}
}

// TestTurnEndForAPriorGenerationClaimantStaysFatalOnVendorSessionConflict is
// the VIOLATION: widening the search past the caller's own claimant is not a
// blanket amnesty. A turn id that resolves to a DIFFERENT vendor session than
// the one this end claims is a genuine contradiction and stays rejected.
func TestTurnEndForAPriorGenerationClaimantStaysFatalOnVendorSessionConflict(t *testing.T) {
	// Arrange — claimed under vend1, by a retired claimant.
	m, _, _ := openTest(t, fakeResolver{"vend1": "ws", "vend2": "ws"})
	claimTurn(t, m, "ws", "s-old", "vend1", "turn-x", 1)

	end := turnClaimEvent(false, 5, "turn-x")
	end.SessionId = "vend2"

	// Act — a different vendor session, under a new claimant, contradicts it.
	_, _, _, err := m.ResolveTurnLifecycle("ws", "s-new", "", end)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "belongs to event_session") {
		t.Fatalf("ResolveTurnLifecycle err = %v, want the vendor session conflict rejected", err)
	}
}

// TestTurnEndForAnUnknownTurnStaysFatalAcrossEveryClaimant is the VIOLATION's
// other edge: a turn id no claimant in the workspace ever held is rejected
// even after the cross-generation widening, exactly as it was before.
func TestTurnEndForAnUnknownTurnStaysFatalAcrossEveryClaimant(t *testing.T) {
	// Arrange — nothing has ever claimed this turn id, under any claimant.
	m, _, _ := openTest(t, fakeResolver{"vend1": "ws"})

	end := turnClaimEvent(false, 5, "turn-never-claimed")
	end.SessionId = "vend1"

	// Act.
	_, _, _, err := m.ResolveTurnLifecycle("ws", "s-new", "", end)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no durable active claim") {
		t.Fatalf("ResolveTurnLifecycle err = %v, want the unknown turn rejected", err)
	}
}

// A redelivered `TurnStarted` for an identity the ledger has already SETTLED is
// the defect class this table exists for: a shim control-request timeout
// declares a submit unknown-fate while the shim actually took it, the queue
// redelivers the same turn identity at a new store coordinate, and the ledger
// used to refuse that second start fatally.
func TestSettledTurnStartRedeliveryVerdict(t *testing.T) {
	tests := []struct {
		name         string
		duplicateSeq uint64
	}{
		{name: "the redelivery lands at a new store coordinate", duplicateSeq: 30},
		{name: "the redelivery repeats the original store coordinate", duplicateSeq: 10},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: one turn lived its whole life in the ledger.
			m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
			resolveTurnClaim(t, m, turnClaimEvent(true, 10, "turn-settled"))
			resolveTurnClaim(t, m, turnClaimEvent(false, 20, "turn-settled"))

			// Act: the same identity is delivered a second time.
			before, after, replayed, err := m.ResolveTurnLifecycle(
				"ws", "daemon-session", "", turnClaimEvent(true, tc.duplicateSeq, "turn-settled"),
			)

			// Assert: idempotent, and no claim reopened.
			if err != nil {
				t.Fatalf("settled turn start redelivery err = %v, want nil", err)
			}
			if !replayed {
				t.Fatalf("settled turn start redelivery replayed = false, want true")
			}
			if len(before) != 0 || len(after) != 0 {
				t.Fatalf("settled turn start redelivery = before:%v after:%v, want no active claim either side", before, after)
			}
		})
	}
}

// TurnClaimExists is the evidence an unknown-fate submit is reconciled
// against, so it must answer for a turn that has already FINISHED as readily
// as for one still running.
func TestTurnClaimExistsAnswersForEveryClaimState(t *testing.T) {
	tests := []struct {
		name   string
		turnID string
		end    bool
		want   bool
	}{
		{name: "the claim is still open", turnID: "turn-probe", end: false, want: true},
		{name: "the claim is already closed", turnID: "turn-probe", end: true, want: true},
		{name: "no claim was ever opened", turnID: "turn-absent", end: false, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
			resolveTurnClaim(t, m, turnClaimEvent(true, 10, "turn-probe"))
			if tc.end {
				resolveTurnClaim(t, m, turnClaimEvent(false, 20, "turn-probe"))
			}

			// Act.
			got, err := m.TurnClaimExists("ws", tc.turnID)

			// Assert.
			if err != nil {
				t.Fatalf("TurnClaimExists err = %v, want nil", err)
			}
			if got != tc.want {
				t.Fatalf("TurnClaimExists(%q) = %v, want %v", tc.turnID, got, tc.want)
			}
		})
	}
}

func TestTurnClaimExistsRefusesAnIncompleteIdentity(t *testing.T) {
	// Arrange.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))

	// Act.
	_, err := m.TurnClaimExists("ws", "")

	// Assert: an empty identity matches every legacy claim, so answering it
	// would report a landed submit for a prompt nothing was keyed by.
	if err == nil || !strings.Contains(err.Error(), "requires workspace and turn id") {
		t.Fatalf("TurnClaimExists err = %v, want the incomplete identity refused", err)
	}
}

func TestConflictingTurnStartCarriesTheTurnScopedSentinel(t *testing.T) {
	// Arrange: a turn whose claim is still OPEN.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
	resolveTurnClaim(t, m, turnClaimEvent(true, 10, "turn-open"))

	// Act: a second start claims the same identity at another coordinate.
	_, _, _, err := m.ResolveTurnLifecycle(
		"ws", "daemon-session", "", turnClaimEvent(true, 11, "turn-open"),
	)

	// Assert: still refused, and named so the refusal can be scoped to the turn.
	if !errors.Is(err, ErrTurnStartConflict) {
		t.Fatalf("conflicting turn start err = %v, want ErrTurnStartConflict", err)
	}
}

func TestTheLedgerKeepsWorkingAfterATurnStartConflict(t *testing.T) {
	// Arrange: a conflicting duplicate was refused.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
	resolveTurnClaim(t, m, turnClaimEvent(true, 10, "turn-open"))
	if _, _, _, err := m.ResolveTurnLifecycle(
		"ws", "daemon-session", "", turnClaimEvent(true, 11, "turn-open"),
	); err == nil {
		t.Fatalf("conflicting turn start err = nil, want a refusal")
	}

	// Act: the conversation carries on with the next turn.
	before, after, replayed := resolveTurnClaim(t, m, turnClaimEvent(true, 12, "turn-next"))

	// Assert: the ledger admitted it beside the still-open first claim.
	if replayed || !reflect.DeepEqual(before, []string{"turn-open"}) ||
		!reflect.DeepEqual(after, []string{"turn-open", "turn-next"}) {
		t.Fatalf("turn after conflict = before:%v after:%v replayed:%v", before, after, replayed)
	}
}

func TestASettledDuplicateReplaysCleanlyOnEveryResume(t *testing.T) {
	// Arrange: a durable stream that CONTAINS the duplicate start, exactly as a
	// resume re-reads it from the store.
	path := filepath.Join(t.TempDir(), "state.db")
	stream := []*corev1.Event{
		turnClaimEvent(true, 10, "turn-settled"),
		turnClaimEvent(false, 20, "turn-settled"),
		turnClaimEvent(true, 30, "turn-settled"),
	}
	first := openTurnClaimManager(t, path)
	for _, ev := range stream {
		resolveTurnClaim(t, first, ev)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("close first manager: %v", err)
	}

	// Act: a resume replays the whole stream, duplicate included.
	second := openTurnClaimManager(t, path)
	var replayErr error
	for _, ev := range stream {
		if _, _, _, err := second.ResolveTurnLifecycle("ws", "daemon-session", "", ev); err != nil {
			replayErr = err
		}
	}

	// Assert: the resume is clean, so the session never becomes unresumable.
	if replayErr != nil {
		t.Fatalf("resume replay err = %v, want nil", replayErr)
	}
}
