package sessiondrv

// Stage-3 fault-injection matrix: the five-color semantics driven through the
// WIRED stack rather than through the resolver alone.
//
// The unit suites (ssm/fivecolor_test.go, errclass/errclass_test.go) already
// pin each rule in isolation. What they cannot show is that a REAL fault,
// entering at the seam a real shim enters at, moves both planes together — the
// workspace color AND the account of why. Every test here injects at that seam:
// a core.v1 event fed to the session consumer, or a transport callback fired
// the way shimclient fires it, with a real ssm.Manager on a real database
// underneath and the real errclass classifier in the path.
//
// The two planes asserted throughout:
//
//   - COLOR: ssm.Manager.Current(workspace).State, the resolved RenderState.
//   - ACCOUNT: the SystemFailureItem cards the consumer pushed, which is what
//     a user reads to find out why the color moved.
//
// Precedence under test is blue > purple > red > yellow > green.

import (
	"path/filepath"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
)

// staticResolver binds session ids to workspaces for the SSM, standing in for
// the daemon's session registry.
type staticResolver map[string]string

func (r staticResolver) Workspace(sessionID string) (string, bool) {
	ws, ok := r[sessionID]
	return ws, ok
}

// faultRig wires a real ssm.Manager to a real consumer with a recording
// pusher. It is the injection point for the whole matrix: events go in at
// Apply/Consume, colors come out of the SSM, and cards come out of the pusher.
type faultRig struct {
	t     *testing.T
	mgr   *ssm.Manager
	push  *fakePusher
	cons  *consumer
	ws    string
	sid   string
	seq   uint64
	death []string
}

const (
	faultWorkspace = "/ws/fault"
	faultSessionID = "s_fault"
)

func newFaultRig(t *testing.T) *faultRig {
	t.Helper()
	mgr, err := ssm.Open(ssm.Options{
		DBPath:   filepath.Join(t.TempDir(), "ssm.db"),
		Resolver: staticResolver{faultSessionID: faultWorkspace},
		Logf:     t.Logf,
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = mgr.Close() })

	rig := &faultRig{t: t, mgr: mgr, push: &fakePusher{}, ws: faultWorkspace, sid: faultSessionID}
	rig.cons = newConsumer(faultWorkspace, faultSessionID, rig.push, mgr, nil, t.Logf,
		nil, nil, nil,
		func() { rig.death = append(rig.death, errclass.DeathReasonShimDied) })
	return rig
}

// apply feeds a lifecycle event through the consumer's Apply sink, the way
// shimclient's demux does, stamping the next store seq.
func (r *faultRig) apply(payload any) {
	r.t.Helper()
	r.seq++
	ev := &corev1.Event{SessionId: r.sid, Seq: r.seq}
	switch p := payload.(type) {
	case *corev1.SessionStarted:
		ev.Payload = &corev1.Event_SessionStarted{SessionStarted: p}
	case *corev1.SessionEnded:
		ev.Payload = &corev1.Event_SessionEnded{SessionEnded: p}
	case *corev1.TurnStarted:
		ev.Payload = &corev1.Event_TurnStarted{TurnStarted: p}
	case *corev1.TurnEnded:
		ev.Payload = &corev1.Event_TurnEnded{TurnEnded: p}
	case *corev1.TaskStarted:
		ev.Payload = &corev1.Event_TaskStarted{TaskStarted: p}
	case *corev1.TaskEnded:
		ev.Payload = &corev1.Event_TaskEnded{TaskEnded: p}
	default:
		r.t.Fatalf("faultRig.apply: unsupported payload %T", payload)
	}
	r.cons.Apply(ev)
}

// state returns the workspace's currently resolved render state.
func (r *faultRig) state() frontendv1.RenderState {
	r.t.Helper()
	ws, found, err := r.mgr.Current(r.ws)
	if err != nil {
		r.t.Fatalf("resolve current state: %v", err)
	}
	if !found {
		return frontendv1.RenderState_RENDER_STATE_UNSPECIFIED
	}
	return ws.GetState()
}

// wantState fails unless the workspace resolves to want.
func (r *faultRig) wantState(want frontendv1.RenderState, context string) {
	r.t.Helper()
	if got := r.state(); got != want {
		r.t.Fatalf("%s: state = %s, want %s", context, got, want)
	}
}

// cards returns every system-failure item the consumer pushed, in push order.
func (r *faultRig) cards() []*frontendv1.SystemFailureItem {
	r.push.mu.Lock()
	defer r.push.mu.Unlock()
	var out []*frontendv1.SystemFailureItem
	for _, d := range r.push.convo {
		for _, it := range d.GetItems() {
			if f := it.GetSystemFailure(); f != nil {
				out = append(out, f)
			}
		}
	}
	return out
}

// cardUUIDs returns the ConversationItem uuid of every pushed failure card, in
// push order, so a resolve-in-place can be told from an accumulating pair.
func (r *faultRig) cardUUIDs() []string {
	r.push.mu.Lock()
	defer r.push.mu.Unlock()
	var out []string
	for _, d := range r.push.convo {
		for _, it := range d.GetItems() {
			if it.GetSystemFailure() != nil {
				out = append(out, it.GetUuid())
			}
		}
	}
	return out
}

// retainedCards returns the consumer's retained failure items — what a resync
// would replay — as distinct from the full push history.
func (r *faultRig) retainedCards() []*frontendv1.ConversationItem {
	return r.cons.snapshotFailItems()
}

// settleGreen brings the workspace to green the way a real bring-up does: the
// shim asserts readiness, the transcript backfill settles, and a frontend
// attests it painted the (empty) history.
func (r *faultRig) settleGreen() {
	r.t.Helper()
	r.apply(&corev1.SessionStarted{Model: "test-model", Cwd: r.ws})
	if err := r.mgr.ApplyBackfillState(r.ws, BackfillDone); err != nil {
		r.t.Fatalf("apply backfill done: %v", err)
	}
	if err := r.mgr.ApplyPaintAck(r.ws, 0); err != nil {
		r.t.Fatalf("apply paint ack: %v", err)
	}
	r.wantState(frontendv1.RenderState_RENDER_STATE_READY, "after bring-up")
}

// ---------------------------------------------------------------------------
// Scenario 1 — the shim dies mid-turn.
// ---------------------------------------------------------------------------

// A shim that dies mid-turn must not leave the workspace advertising the turn
// as running. SessionEnded outranks the live turn (dead is rank 10, thinking
// is 30), so the color reports the broken route rather than the work behind it.
func TestShimDeathMidTurnResolvesBlueOverTheLiveTurn(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_THINKING, "turn in flight")

	// Act
	rig.apply(&corev1.SessionEnded{})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DEAD, "after shim death")
}

// The color and its account may not disagree: the same SessionEnded that turns
// the workspace blue must also record WHY, which is what SessionView.death
// carries.
func TestShimDeathMidTurnRecordsTheDeathReason(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})

	// Act
	rig.apply(&corev1.SessionEnded{})

	// Assert
	if len(rig.death) != 1 {
		t.Fatalf("death callback fired %d time(s), want exactly 1", len(rig.death))
	}
	if rig.death[0] != errclass.DeathReasonShimDied {
		t.Fatalf("death reason = %q, want %q", rig.death[0], errclass.DeathReasonShimDied)
	}
}

// The recorded death reason must classify as INTERNAL: agent-repl's own
// machinery failed, nothing about the account is implicated. This is the item
// that rides SessionView.death.
func TestShimDeathClassifiesAsInternal(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.SessionEnded{})

	// Act
	item := errclass.Death(t.Logf, rig.death[0])

	// Assert
	if item.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_INTERNAL {
		t.Fatalf("death class = %s, want ERROR_CLASS_INTERNAL", item.GetErrorClass())
	}
}

// The transport half of a shim dying: the missed-heartbeat window elapses and
// the daemon files an INTERNAL card in the conversation feed, so the user can
// find out from the conversation why the workspace changed color.
func TestShimConnectionLossMidTurnFilesAnInternalCard(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})

	// Act
	rig.cons.ConnectionDegraded(rig.sid, "no shim traffic for 30s")

	// Assert
	cards := rig.cards()
	if len(cards) != 1 {
		t.Fatalf("pushed %d failure card(s), want exactly 1", len(cards))
	}
	if cards[0].GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_INTERNAL {
		t.Fatalf("card class = %s, want ERROR_CLASS_INTERNAL", cards[0].GetErrorClass())
	}
}

// Blue outranks the live turn on the transport axis too: a degraded connection
// during a turn reports the compromised route, not the work behind it.
func TestShimConnectionLossMidTurnResolvesBlue(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_THINKING, "turn in flight")

	// Act
	rig.cons.ConnectionDegraded(rig.sid, "no shim traffic for 30s")

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DEGRADED, "connection lost mid-turn")
}

// ---------------------------------------------------------------------------
// Scenario 2 — the store connection is lost and later restored.
// ---------------------------------------------------------------------------

// A store outage the shim reports must reach the user as a card, classified
// INTERNAL: the store is agent-repl's own machinery.
func TestStoreOutageFilesAnInternalCard(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()

	// Act
	rig.cons.Degraded(rig.sid, &corev1.DegradedState{
		Component:    "store-client",
		Reason:       "store socket closed",
		DroppedCount: 7,
	})

	// Assert
	cards := rig.cards()
	if len(cards) != 1 {
		t.Fatalf("pushed %d failure card(s), want exactly 1", len(cards))
	}
	if cards[0].GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_INTERNAL {
		t.Fatalf("card class = %s, want ERROR_CLASS_INTERNAL", cards[0].GetErrorClass())
	}
}

// The opening edge of a window-shaped failure carries no resolution: the
// outage is still open, and stamping it closed would settle an alarm about
// something that has not ended.
func TestStoreOutageOpensUnresolved(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()

	// Act
	rig.cons.Degraded(rig.sid, &corev1.DegradedState{Component: "store-client", Reason: "store socket closed"})

	// Assert
	if got := rig.cards()[0].GetResolvedAtMs(); got != 0 {
		t.Fatalf("resolved_at_ms = %d on the opening edge, want 0", got)
	}
}

// The closing edge must reconcile the card IN PLACE rather than accumulate a
// second one: both edges derive the same uuid, so the retained item a resync
// replays is the SETTLED card, not a re-opened alarm.
func TestStoreRecoveryResolvesTheSameCardInPlace(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	const recoveredAt int64 = 1_700_000_042_000
	rig.cons.now = func() int64 { return recoveredAt }
	rig.cons.Degraded(rig.sid, &corev1.DegradedState{Component: "store-client", Reason: "store socket closed"})

	// Act
	rig.cons.Degraded(rig.sid, &corev1.DegradedState{Component: "store-client", Recovered: true})

	// Assert
	uuids := rig.cardUUIDs()
	if len(uuids) != 2 || uuids[0] != uuids[1] {
		t.Fatalf("card uuids = %v, want two pushes under one uuid", uuids)
	}
	retained := rig.retainedCards()
	if len(retained) != 1 {
		t.Fatalf("retained %d card(s), want exactly 1 (the settled one)", len(retained))
	}
	if got := retained[0].GetSystemFailure().GetResolvedAtMs(); got != recoveredAt {
		t.Fatalf("retained resolved_at_ms = %d, want %d", got, recoveredAt)
	}
}

// A transport outage moves the SSM's degraded axis to blue, so the color and
// the card agree that the route is compromised.
func TestConnectionOutageResolvesDegraded(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()

	// Act
	rig.cons.ConnectionDegraded(rig.sid, "missed heartbeat window")

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DEGRADED, "connection degraded")
}

// Recovery must return the workspace to green with no manual intervention: the
// degraded axis clears itself when traffic resumes.
func TestConnectionRecoveryReturnsToGreenUnaided(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.cons.ConnectionDegraded(rig.sid, "missed heartbeat window")
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DEGRADED, "connection degraded")

	// Act
	rig.cons.ConnectionRecovered(rig.sid)

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_READY, "after recovery")
}

// The transport card resolves in place on the same uuid the opening edge used,
// the same contract the store card honors.
func TestConnectionRecoveryResolvesTheSameCardInPlace(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	const recoveredAt int64 = 1_700_000_099_000
	rig.cons.now = func() int64 { return recoveredAt }
	rig.cons.ConnectionDegraded(rig.sid, "missed heartbeat window")

	// Act
	rig.cons.ConnectionRecovered(rig.sid)

	// Assert
	retained := rig.retainedCards()
	if len(retained) != 1 {
		t.Fatalf("retained %d card(s), want exactly 1 (the settled one)", len(retained))
	}
	if got := retained[0].GetSystemFailure().GetResolvedAtMs(); got != recoveredAt {
		t.Fatalf("retained resolved_at_ms = %d, want %d", got, recoveredAt)
	}
}

// ---------------------------------------------------------------------------
// Scenario 3 — an auth failure from the vendor.
// ---------------------------------------------------------------------------

// An auth-class turn conclusion is vendor-inherent: only a human or the vendor
// releases it, so the workspace resolves purple rather than settling green.
func TestAuthFailureResolvesVendorBlocked(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})

	// Act
	rig.apply(&corev1.TurnEnded{StopReason: "authentication_failed", IsError: true})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED, "after an auth failure")
}

// The account of a vendor block is classified API, never INTERNAL: nothing of
// agent-repl's own machinery failed.
func TestAuthFailureClassifiesAsAPI(t *testing.T) {
	// Arrange
	te := &corev1.TurnEnded{StopReason: "authentication_failed", IsError: true}

	// Act
	item := errclass.TurnEnd(te)

	// Assert
	if item.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_API {
		t.Fatalf("auth failure class = %s, want ERROR_CLASS_API", item.GetErrorClass())
	}
}

// An auth-class ApiErrorLine reaching the feed carries the same class, so the
// two routes to the same fact cannot disagree.
func TestAuthApiErrorLineClassifiesAsAPI(t *testing.T) {
	// Arrange
	line := &datav1.ApiErrorLine{
		Level: "error",
		Error: &datav1.ApiErrorDetail{Status: 401, Message: "invalid api key"},
	}

	// Act
	item := errclass.APIError(line, "item-1")

	// Assert
	if item.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_API {
		t.Fatalf("401 class = %s, want ERROR_CLASS_API", item.GetErrorClass())
	}
}

// A vendor block is released only by evidence the vendor released it — a clean
// turn conclusion — never by a retry. The green underneath is then revealed
// rather than the workspace being left with no state.
func TestCleanTurnAfterAuthFailureReleasesTheBlock(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "authentication_failed", IsError: true})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED, "blocked")

	// Act
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DONE, "after a clean turn")
}

// ---------------------------------------------------------------------------
// Scenario 4 — usage and rate-limit rejection.
// ---------------------------------------------------------------------------

// A 429 is an API-class failure: the vendor refused, and releasing it needs
// the vendor or a human rather than a retry.
func TestRateLimitApiErrorClassifiesAsAPI(t *testing.T) {
	// Arrange
	line := &datav1.ApiErrorLine{
		Level: "error",
		Error: &datav1.ApiErrorDetail{Status: 429, Message: "rate limited"},
	}

	// Act
	item := errclass.APIError(line, "item-2")

	// Assert
	if item.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_API {
		t.Fatalf("429 class = %s, want ERROR_CLASS_API", item.GetErrorClass())
	}
}

// A budget stop is a limit the USER set, so releasing it takes a human
// decision — which is exactly what API class means.
func TestBudgetStopResolvesVendorBlocked(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})

	// Act
	rig.apply(&corev1.TurnEnded{StopReason: "error_max_budget", IsError: true})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED, "after a budget stop")
}

// A `rejected` rate-limit status blocks: the request was refused.
func TestRejectedRateLimitStatusBlocks(t *testing.T) {
	// Act
	got := ssm.VendorBlockingRateLimit("rejected")

	// Assert
	if !got {
		t.Fatal("VendorBlockingRateLimit(\"rejected\") = false, want true")
	}
}

// An `allowed_warning` rides a request the API ALLOWED. Opening a blocked
// window on it paints a working session as stopped, which is the misreport the
// status split exists to prevent.
func TestAllowedWarningRateLimitStatusDoesNotBlock(t *testing.T) {
	// Act
	got := ssm.VendorBlockingRateLimit("allowed_warning")

	// Assert
	if got {
		t.Fatal("VendorBlockingRateLimit(\"allowed_warning\") = true, want false")
	}
}

// An allowed_warning must not move the workspace off green through any wired
// path: the request it rode succeeded, so the turn concluded cleanly.
func TestAllowedWarningLeavesTheWorkspaceGreen(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})

	// Act — the turn carrying the warning ANSWERED, so it ends cleanly.
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DONE, "after a turn carrying an allowed_warning")
}

// ---------------------------------------------------------------------------
// Scenario 5 — the interrupt race.
// ---------------------------------------------------------------------------

// Only FAILED is a failure. The enum's whole purpose is that a stop landing on
// an already-finished turn is distinguishable from one that could not be
// delivered, so the second is never painted as the first.
func TestInterruptOutcomesAreThreeValued(t *testing.T) {
	// Arrange
	tests := []struct {
		name    string
		outcome corev1.InterruptOutcome
		wantErr bool
	}{
		{"interrupted is success", corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED, false},
		{"already complete is success", corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE, false},
		{"failed is the only failure", corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := errclass.InterruptError(tc.outcome)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("InterruptError(%s) err = %v, wantErr %v", tc.outcome, err, tc.wantErr)
			}
		})
	}
}

// An already-complete stop must mint no failure card: the user asked for the
// turn to be over and it already is, which is success.
func TestAlreadyCompleteInterruptMintsNoCard(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})

	// Act — the stop lands after the turn already concluded.
	err := errclass.InterruptError(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)

	// Assert
	if err != nil {
		t.Fatalf("already-complete produced err %v, want nil", err)
	}
	if cards := rig.cards(); len(cards) != 0 {
		t.Fatalf("already-complete minted %d card(s), want 0", len(cards))
	}
}

// An undeliverable stop is a route failure, so it classifies INTERNAL rather
// than implicating the vendor.
func TestUndeliverableInterruptClassifiesAsInternal(t *testing.T) {
	// Arrange
	err := errclass.InterruptError(corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED)

	// Act
	item := errclass.Command(t.Logf, err)

	// Assert
	if item.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_INTERNAL {
		t.Fatalf("undeliverable interrupt class = %s, want ERROR_CLASS_INTERNAL", item.GetErrorClass())
	}
}

// The workspace must never stick red after an interrupt lands: an aborted turn
// is a normal conclusion the user asked for, so it settles green.
func TestInterruptedTurnDoesNotStickRed(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_THINKING, "turn in flight")

	// Act — the interrupt lands and the turn concludes as aborted.
	rig.apply(&corev1.TurnEnded{StopReason: "aborted", IsError: true})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DONE, "after an interrupt")
}

// A stop racing a turn that concluded on its own must leave the workspace on
// the clean conclusion, not on a block invented by the race.
func TestInterruptRacingACleanEndLeavesGreen(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})

	// Act — the turn finishes first; the stop finds nothing to interrupt.
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})
	if err := errclass.InterruptError(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE); err != nil {
		t.Fatalf("already-complete produced err %v, want nil", err)
	}

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DONE, "after a raced interrupt")
}

// ---------------------------------------------------------------------------
// Scenario 6 — the turn lifecycle.
// ---------------------------------------------------------------------------

// Submit-accept is the moment the workspace goes red: TurnStarted is what the
// SSM resolves to thinking.
func TestTurnStartFlipsToRed(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()

	// Act
	rig.apply(&corev1.TurnStarted{})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_THINKING, "at submit-accept")
}

// A failed backfill compromises the route: an incomplete history cannot be the
// basis of a ready claim, so the workspace stays blue however green the agent
// axis reads underneath.
func TestFailedBackfillHoldsTheWorkspaceBlue(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.apply(&corev1.SessionStarted{Model: "test-model", Cwd: rig.ws})
	if err := rig.mgr.ApplyPaintAck(rig.ws, 0); err != nil {
		t.Fatalf("apply paint ack: %v", err)
	}

	// Act
	if err := rig.mgr.ApplyBackfillState(rig.ws, BackfillFailed); err != nil {
		t.Fatalf("apply backfill failed: %v", err)
	}

	// Assert — blue's token for a compromised route.
	rig.wantState(frontendv1.RenderState_RENDER_STATE_INIT, "with a failed backfill")
}

// Settling the backfill releases the axis, letting the workspace reach green.
func TestBackfillSettlingReleasesTowardGreen(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.apply(&corev1.SessionStarted{Model: "test-model", Cwd: rig.ws})
	if err := rig.mgr.ApplyBackfillState(rig.ws, BackfillFailed); err != nil {
		t.Fatalf("apply backfill failed: %v", err)
	}
	rig.wantState(frontendv1.RenderState_RENDER_STATE_INIT, "with a failed backfill")

	// Act
	if err := rig.mgr.ApplyBackfillState(rig.ws, BackfillDone); err != nil {
		t.Fatalf("apply backfill done: %v", err)
	}

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_READY, "after the backfill settled")
}

// A withdrawn paint attestation resolves blue: a frontend that has not
// attested painting is indistinguishable from one that cannot paint, so the
// honest answer is the compromised-route color.
func TestWithdrawnPaintAttestationHoldsBlue(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()

	// Act
	if err := rig.mgr.ApplyPaintLost(rig.ws, "frontend disconnected"); err != nil {
		t.Fatalf("apply paint lost: %v", err)
	}

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_INIT, "with the attestation withdrawn")
}

// A fresh paint ack re-attests the route and returns the workspace to green.
func TestFreshPaintAckReturnsToGreen(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	if err := rig.mgr.ApplyPaintLost(rig.ws, "frontend disconnected"); err != nil {
		t.Fatalf("apply paint lost: %v", err)
	}
	rig.wantState(frontendv1.RenderState_RENDER_STATE_INIT, "with the attestation withdrawn")

	// Act
	if err := rig.mgr.ApplyPaintAck(rig.ws, 1); err != nil {
		t.Fatalf("apply paint ack: %v", err)
	}

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_READY, "after re-attestation")
}

// ---------------------------------------------------------------------------
// Scenario 7 — async-only work after the turn ends.
// ---------------------------------------------------------------------------

// Background work outliving the turn is yellow, not green: "something is still
// running" is a weaker claim than a live turn and a stronger one than nothing.
func TestBackgroundWorkAfterTurnEndResolvesYellow(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TaskStarted{TaskId: "task-1"})

	// Act
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC, "with a task still live")
}

// Quiescence returns the workspace to green once the detached work ends.
func TestQuiescenceAfterBackgroundWorkReturnsGreen(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TaskStarted{TaskId: "task-1"})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC, "with a task still live")

	// Act
	rig.apply(&corev1.TaskEnded{TaskId: "task-1"})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DONE, "after quiescence")
}

// A live turn is never demoted to yellow: red is the stronger claim, so
// background work alongside a running turn stays red.
func TestBackgroundWorkDuringATurnStaysRed(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})

	// Act
	rig.apply(&corev1.TaskStarted{TaskId: "task-1"})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_THINKING, "with a turn in flight")
}

// ---------------------------------------------------------------------------
// Scenario 8 — precedence between composed conditions.
// ---------------------------------------------------------------------------

// Blue outranks purple: a session whose shim is gone is unreachable, which is
// a stronger claim than the vendor having stopped it.
func TestShimDeathOutranksAnOpenVendorBlock(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "authentication_failed", IsError: true})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED, "vendor blocked")

	// Act
	rig.apply(&corev1.SessionEnded{})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DEAD, "shim dead while vendor blocked")
}

// Blue outranks purple on the transport axis too: a degraded route beats an
// open vendor block.
func TestConnectionOutageOutranksAnOpenVendorBlock(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "authentication_failed", IsError: true})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED, "vendor blocked")

	// Act
	rig.cons.ConnectionDegraded(rig.sid, "missed heartbeat window")

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DEGRADED, "connection lost while vendor blocked")
}

// Purple outranks red: a session the vendor has stopped is not going to finish
// whatever it still looks busy doing.
func TestVendorBlockOutranksALiveTurn(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "authentication_failed", IsError: true})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED, "vendor blocked")

	// Act — a new turn starts while the block still stands.
	rig.apply(&corev1.TurnStarted{})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED, "turn started while vendor blocked")
}

// Yellow outranks green: detached work is a stronger claim than nothing
// running, so it wins over the settled turn underneath.
func TestBackgroundWorkOutranksTheSettledTurn(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DONE, "settled turn")

	// Act
	rig.apply(&corev1.TaskStarted{TaskId: "task-late"})

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC, "detached work after the turn settled")
}

// Blue outranks yellow: a compromised route is not something to advertise as
// working, however much detached work is still live behind it.
func TestConnectionOutageOutranksBackgroundWork(t *testing.T) {
	// Arrange
	rig := newFaultRig(t)
	rig.settleGreen()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TaskStarted{TaskId: "task-1"})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})
	rig.wantState(frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC, "detached work live")

	// Act
	rig.cons.ConnectionDegraded(rig.sid, "missed heartbeat window")

	// Assert
	rig.wantState(frontendv1.RenderState_RENDER_STATE_DEGRADED, "connection lost with detached work live")
}
