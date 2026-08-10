package sessioncontroller

import (
	"errors"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// THE INVARIANT: no historical wedge survives a bring-up.
//
// Session s_f223cd698d687299 was reconciled at the wire — the returning shim
// reported turn_in_flight=false and the ledger agreed — and the workspace still
// rendered `thinking`, because nothing re-derived the render state from the
// reconciled ledger and nothing handed the re-derivation to the frontends
// already holding the stale one.
//
// The heal is keyed on the STATE the reconciliation left behind rather than on
// which branch produced it, so a latch heals whatever put it there.
// ---------------------------------------------------------------------------

// wedgedBringUpRig is a consumer at the shim handshake whose workspace carries
// a `thinking` the reconciled ledger does not support.
func wedgedBringUpRig(t *testing.T) (*consumer, *fakeApplier, *fakePusher, *levelSplitLogs) {
	t.Helper()
	applier := &fakeApplier{}
	// The SYNTHETIC WEDGE: the axis still holds a live turn this session owns,
	// and the reconciliation below will find no claim behind it.
	applier.alreadyCompleteDid = true
	push := &fakePusher{}
	logs := &levelSplitLogs{}
	c := newConsumer("ws", "s1", push, applier, &fakeProgress{}, newFakeClearCompactStore(),
		newSettlingTurnAccountingStore(), logs.logf, nil, nil, nil, nil, nil)
	c.warnf = logs.warnf
	return c, applier, push, logs
}

// THE REPORTED DEFECT. A bring-up whose shim reports no turn in flight must
// retire the stale `thinking`, not leave it for an edge that is not coming.
func TestABringUpOverAWedgedThinkingStateHealsIt(t *testing.T) {
	// Arrange.
	c, applier, _, _ := wedgedBringUpRig(t)

	// Act.
	if _, _, err := c.reconcileTurnHandshake(&corev1.ShimHello{TurnInFlight: false}); err != nil {
		t.Fatalf("reconcileTurnHandshake: %v", err)
	}

	// Assert.
	applier.reconcMutex.Lock()
	defer applier.reconcMutex.Unlock()
	if len(applier.alreadyCompletes) != 1 {
		t.Fatalf("render-state reconciliations = %d, want the stale `thinking` re-derived from the reconciled ledger", len(applier.alreadyCompletes))
	}
}

// THE HALF THE INCIDENT TURNED ON. Retiring the row heals the daemon; only the
// publish heals the frontend that is already drawing the stale one.
func TestABringUpSelfHealPublishesTheReDerivedState(t *testing.T) {
	// Arrange.
	c, _, push, _ := wedgedBringUpRig(t)

	// Act.
	if _, _, err := c.reconcileTurnHandshake(&corev1.ShimHello{TurnInFlight: false}); err != nil {
		t.Fatalf("reconcileTurnHandshake: %v", err)
	}

	// Assert.
	push.mu.Lock()
	states := append([]*frontendv1.WorkspaceState(nil), push.state...)
	push.mu.Unlock()
	if len(states) != 1 || states[0].GetState() != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("published states = %v, want the re-derived state handed to the frontends", states)
	}
}

// The heal is RECORDED, because a colour that changes at bring-up with no
// account of why is indistinguishable from one that changed by accident.
func TestABringUpSelfHealRecordsWhatItRetired(t *testing.T) {
	// Arrange.
	c, _, _, logs := wedgedBringUpRig(t)

	// Act.
	if _, _, err := c.reconcileTurnHandshake(&corev1.ShimHello{TurnInFlight: false}); err != nil {
		t.Fatalf("reconcileTurnHandshake: %v", err)
	}

	// Assert.
	if !strings.Contains(strings.Join(logs.info, "\n"), "bring-up render-state SELF-HEALED") {
		t.Fatalf("log = %v, want the heal recorded", logs.info)
	}
}

// A SHIM THAT IS RUNNING A TURN IS BELIEVED. The heal's predicate is the
// reconciled ledger plus the hello's own statement, and a hello reporting a
// turn in flight is not a wedge to cure.
func TestABringUpOverALiveTurnHealsNothing(t *testing.T) {
	// Arrange.
	c, applier, _, _ := wedgedBringUpRig(t)

	// Act.
	if _, _, err := c.reconcileTurnHandshake(&corev1.ShimHello{TurnInFlight: true}); err != nil {
		t.Fatalf("reconcileTurnHandshake: %v", err)
	}

	// Assert.
	applier.reconcMutex.Lock()
	defer applier.reconcMutex.Unlock()
	if len(applier.alreadyCompletes) != 0 {
		t.Fatalf("render-state reconciliations = %d, want none over a turn the shim says is running", len(applier.alreadyCompletes))
	}
}

// A CLAIM THE RECONCILIATION SPARED KEEPS THE AXIS. Its own replayed boundary
// paints the workspace idle moments from now, and retiring `thinking` ahead of
// it would flash a settled workspace for a turn that has not settled.
func TestABringUpThatSparedAClaimHealsNothing(t *testing.T) {
	// Arrange — the hello names a turn, so the ledger keeps its claim.
	c, applier, _, _ := wedgedBringUpRig(t)

	// Act.
	if _, _, err := c.reconcileTurnHandshake(&corev1.ShimHello{ActiveTurnIds: []string{"t-spared"}}); err != nil {
		t.Fatalf("reconcileTurnHandshake: %v", err)
	}

	// Assert.
	applier.reconcMutex.Lock()
	defer applier.reconcMutex.Unlock()
	if len(applier.alreadyCompletes) != 0 {
		t.Fatalf("render-state reconciliations = %d, want none while a claim still stands", len(applier.alreadyCompletes))
	}
}

// A HEAL THAT FAILS MUST NOT FAIL THE BRING-UP. The session is perfectly
// driveable with a stale colour, and refusing to establish it over a row this
// could not tidy would be strictly worse than the row.
func TestAFailedSelfHealStillEstablishesTheSession(t *testing.T) {
	// Arrange.
	c, applier, _, logs := wedgedBringUpRig(t)
	applier.alreadyCompleteErr = errors.New("state db unwritable")

	// Act.
	_, _, err := c.reconcileTurnHandshake(&corev1.ShimHello{TurnInFlight: false})

	// Assert.
	if err != nil {
		t.Fatalf("reconcileTurnHandshake error = %v, want the bring-up to survive a heal it could not write", err)
	}
	if !strings.Contains(strings.Join(logs.warn, "\n"), "bring-up render-state self-heal FAILED") {
		t.Fatalf("warn = %v, want the failed heal surfaced", logs.warn)
	}
}

// A workspace with nothing latched heals nothing and says nothing: the heal
// must not manufacture a record for a bring-up that found the axis settled.
func TestABringUpOverASettledAxisRecordsNoHeal(t *testing.T) {
	// Arrange — the reconciliation finds no stale row to retire.
	c, applier, _, logs := wedgedBringUpRig(t)
	applier.alreadyCompleteDid = false

	// Act.
	if _, _, err := c.reconcileTurnHandshake(&corev1.ShimHello{TurnInFlight: false}); err != nil {
		t.Fatalf("reconcileTurnHandshake: %v", err)
	}

	// Assert.
	if strings.Contains(strings.Join(logs.info, "\n"), "SELF-HEALED") {
		t.Fatalf("log = %v, want no heal reported over an axis that was already settled", logs.info)
	}
}
