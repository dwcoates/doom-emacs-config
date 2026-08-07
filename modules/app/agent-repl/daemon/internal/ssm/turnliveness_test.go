package ssm

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// This file covers the ONE turn-liveness derivation and the durable end that
// keeps a killed turn dead across a store replay.
//
// The sequence the first test reproduces is the observed wedge, from
// ~/.claude-emacs/claude-repld.log for the `uds-ack-track-before-send-uko`
// workspace: a workspace-create prompt went out, a second create_session
// superseded the session mid-turn, the replacement replayed the same
// `turn_started` seq out of the store, and the two folds disagreed — the ledger
// took the replay as fresh work under the new claimant, the axis dropped it as a
// duplicate store coordinate. The sidebar stayed green and every prompt after it
// queued behind a turn that had been killed minutes earlier.

// supersedeRig drives the wedge's exact shape: one workspace, an original
// session that takes a prompt and is torn down mid-turn, and a replacement
// session that subscribes to the SAME store stream and replays it.
type supersedeRig struct {
	t         *testing.T
	m         *Manager
	log       *capLog
	workspace string
	// vendor is the store's identity for the conversation. It is what the
	// replayed event carries and what BOTH daemon sessions see events under,
	// which is precisely why a claimant-keyed answer and a store-keyed answer
	// could disagree about it.
	vendor string
}

// supersedingResolver binds the store's vendor identity to whichever daemon
// session currently drives the workspace, exactly as the registry does across a
// supersede.
type supersedingResolver struct {
	workspace string
	vendor    string
	current   *string
}

func (r supersedingResolver) Session(sessionID string) (Binding, bool) {
	if sessionID == r.vendor || sessionID == *r.current {
		return Binding{Workspace: r.workspace, SessionID: *r.current}, true
	}
	return Binding{}, false
}

func newSupersedeRig(t *testing.T) (*supersedeRig, *string) {
	t.Helper()
	current := new(string)
	*current = "s_original"
	resolver := supersedingResolver{workspace: "/ws/uds", vendor: "vendor-uds", current: current}
	m, cl, _ := openUnwiredTest(t, resolver)
	if err := m.ApplyWired("/ws/uds", WiringWired, "test arrangement"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	return &supersedeRig{t: t, m: m, log: cl, workspace: "/ws/uds", vendor: "vendor-uds"}, current
}

// turnStarted is the store's `turn_started` at seq, for the named turn.
func (r *supersedeRig) turnStarted(seq uint64, turnID string) *corev1.Event {
	return &corev1.Event{
		SessionId: r.vendor,
		Seq:       seq,
		Plane:     corev1.Plane_PLANE_STREAM,
		RequestId: turnID,
		Payload:   &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: turnID}},
	}
}

// TestASupersededTurnsReplayLeavesTheColourAndTheQueueWithOneAnswer is the
// wedge, reproduced end to end.
func TestASupersededTurnsReplayLeavesTheColourAndTheQueueWithOneAnswer(t *testing.T) {
	// Arrange — the original session takes the workspace-create prompt and its
	// turn starts, exactly as at 13:47:51.
	rig, current := newSupersedeRig(t)
	const turnID = "workspace-create:workspace_commands:0"
	if err := rig.m.MarkPromptAccepted(rig.workspace, "s_original", "req-create",
		func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}
	if _, err := rig.m.ApplyTurnBoundary(rig.workspace, "s_original", "", rig.turnStarted(3, turnID)); err != nil {
		t.Fatalf("original turn started: %v", err)
	}

	// Act — the supersede tears the original session down MID-TURN (13:47:52),
	// and the replacement session comes up and replays the SAME store event
	// (13:47:55).
	if _, err := rig.m.CloseStaleTurn(rig.workspace, "s_original", "session_superseded", true); err != nil {
		t.Fatalf("CloseStaleTurn: %v", err)
	}
	*current = "s_replacement"
	boundary, err := rig.m.ApplyTurnBoundary(rig.workspace, "s_replacement", "", rig.turnStarted(3, turnID))
	if err != nil {
		t.Fatalf("replacement replay of the same turn_started: %v", err)
	}

	// Assert — ONE answer. The queue's test and the colour are the same value,
	// not two values that happen to agree.
	if boundary.Liveness.Active() {
		t.Fatalf("liveness after the replay = %s, want no turn in flight: the turn it names was killed by the supersede", boundary.Liveness)
	}
	state := mustCurrent(t, rig.m, rig.workspace)
	if state.GetTurnActive() != boundary.Liveness.Active() {
		t.Fatalf("colour's turn_active = %v, queue's liveness = %v — these must be one value",
			state.GetTurnActive(), boundary.Liveness.Active())
	}
	if state.GetState() == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = THINKING over a killed turn; want a settled colour")
	}

	// And the prompt the user sends next is DELIVERED rather than queued: the
	// one liveness answer says nothing is in flight to queue behind.
	live, err := rig.m.TurnLiveness(rig.workspace)
	if err != nil {
		t.Fatalf("TurnLiveness: %v", err)
	}
	if live.Active() {
		t.Fatalf("turn liveness = %s, want inactive so the next prompt is delivered rather than queued", live)
	}
}

// TestTheDurableEndOfAKilledTurnSurvivesIntoTheReplacementsReplay is PART B on
// its own: the record that makes the recurrence impossible is keyed by the
// killed start's STORE coordinate, so a claimant that never ran the turn still
// finds it.
func TestTheDurableEndOfAKilledTurnSurvivesIntoTheReplacementsReplay(t *testing.T) {
	// Arrange — a live turn, killed by a supersede.
	rig, current := newSupersedeRig(t)
	const turnID = "turn-killed"
	if _, err := rig.m.ApplyTurnBoundary(rig.workspace, "s_original", "", rig.turnStarted(3, turnID)); err != nil {
		t.Fatalf("original turn started: %v", err)
	}
	if _, err := rig.m.CloseStaleTurn(rig.workspace, "s_original", "session_superseded", true); err != nil {
		t.Fatalf("CloseStaleTurn: %v", err)
	}

	// The durable end names the STORE coordinate, not the claimant that held
	// the claim — that claimant is exactly what the supersede retires.
	var cause string
	if err := rig.m.db.QueryRow(
		`SELECT cause FROM turn_interruption WHERE workspace=? AND start_event_session_id=? AND turn_id=?`,
		rig.workspace, rig.vendor, turnID,
	).Scan(&cause); err != nil {
		t.Fatalf("read the durable end recorded against the killed start: %v", err)
	}
	if cause != TurnCloseShimStopped {
		t.Fatalf("durable end cause = %q, want %q", cause, TurnCloseShimStopped)
	}

	// Act — the replacement replays the start under a claimant that has never
	// seen this turn.
	*current = "s_replacement"
	if _, err := rig.m.ApplyTurnBoundary(rig.workspace, "s_replacement", "", rig.turnStarted(3, turnID)); err != nil {
		t.Fatalf("replacement replay: %v", err)
	}

	// Assert — a MATCHED start/end pair, not a start with no end.
	var endSeq any
	var endCause string
	if err := rig.m.db.QueryRow(
		`SELECT end_seq, end_cause FROM turn_lifecycle_claim
		 WHERE workspace=? AND claimant_session_id=? AND turn_id=?`,
		rig.workspace, "s_replacement", turnID,
	).Scan(&endSeq, &endCause); err != nil {
		t.Fatalf("read the replacement's reconstructed claim: %v", err)
	}
	if endSeq == nil {
		t.Fatal("the replayed start reopened the killed turn: the claim has no end")
	}
	if endCause != TurnCloseShimStopped {
		t.Fatalf("reconstructed end cause = %q, want the killed turn's own %q", endCause, TurnCloseShimStopped)
	}
	if !rig.log.contains("turn start ADMITTED ALREADY ENDED") {
		t.Fatalf("the reconstructed pair was not reported; log:\n%s", strings.Join(rig.log.lines, "\n"))
	}
}

// TestASecondFoldOfTheEventStreamFailsHardAndWritesNothing drives the
// enforcement invariant into violation: a consumer folding a turn boundary
// through the general lifecycle apply would reach a second turn-liveness answer,
// so it is refused rather than tolerated.
func TestASecondFoldOfTheEventStreamFailsHardAndWritesNothing(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	ev := &corev1.Event{
		SessionId: "s1",
		Seq:       9,
		Plane:     corev1.Plane_PLANE_STREAM,
		RequestId: "turn-9",
		Payload:   &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "turn-9"}},
	}

	// Act.
	err := m.Apply(ev)

	// Assert — it fails hard.
	if err == nil {
		t.Fatal("Apply accepted a turn boundary; the second fold must be unreachable")
	}
	if !strings.Contains(err.Error(), "must use ApplyTurnBoundary") {
		t.Fatalf("error = %q, want it to name the one door", err)
	}
	// ...through the module's canonical logging helper, with the structured
	// context needed to find the caller.
	if !cl.contains("ssm: INVARIANT VIOLATION operation=apply") ||
		!cl.contains("seq=9") || !cl.contains(`turn_id="turn-9"`) {
		t.Fatalf("no canonical invariant record with structured context; log:\n%s", strings.Join(cl.lines, "\n"))
	}
	// ...and NOTHING was written, on either the axis or the ledger.
	var rows int
	if err := m.db.QueryRow(
		`SELECT COUNT(*) FROM workspace_state WHERE cause_seq = 9`,
	).Scan(&rows); err != nil {
		t.Fatalf("count state rows: %v", err)
	}
	if rows != 0 {
		t.Fatalf("session-status rows written by the refused fold = %d, want 0", rows)
	}
	var claims int
	if err := m.db.QueryRow(`SELECT COUNT(*) FROM turn_lifecycle_claim`).Scan(&claims); err != nil {
		t.Fatalf("count turn claims: %v", err)
	}
	if claims != 0 {
		t.Fatalf("turn claims written by the refused fold = %d, want 0", claims)
	}
}

// TestAnUnderivedTurnLivenessIsNotAnAnswer covers the other half of the
// invariant: Go's zero value must never read as "no turn in flight".
func TestAnUnderivedTurnLivenessIsNotAnAnswer(t *testing.T) {
	var zero TurnLiveness
	if zero.Derived() {
		t.Fatal("a zero TurnLiveness reported itself as derived")
	}
	if got := zero.String(); !strings.Contains(got, "UNDERIVED") {
		t.Fatalf("zero value renders as %q, want it to name itself underived", got)
	}
}
