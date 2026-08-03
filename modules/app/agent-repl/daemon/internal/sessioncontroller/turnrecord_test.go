package sessioncontroller

import (
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// This file covers the session controller's ONE turn record (turnrecord.go):
// every legal transition, the transition it REFUSES, the ordering that makes the
// name available before anyone can observe the turn, and the two drain holds
// whose ids come from somewhere other than the record itself.

// --- the transitions --------------------------------------------------------

// TestTurnRecordTransitions covers each legal transition, one per row. Every row
// is a single edge: the record it starts from, the one edge applied to it, and
// the record that edge must produce.
func TestTurnRecordTransitions(t *testing.T) {
	named := func(id string) turnRecord { return turnRecord{phase: turnPhaseNamed, turnID: id} }
	accepted := func(req string) turnRecord { return turnRecord{phase: turnPhaseAccepted, requestID: req} }
	adopted := turnRecord{phase: turnPhaseAdopted}

	tests := []struct {
		name  string
		start turnRecord
		edge  func(d *sessionController)
		want  turnRecord
	}{
		{
			name:  "the accept edge claims an idle session",
			start: turnRecord{},
			edge:  func(d *sessionController) { d.noteTurnAcceptedLocked("r-1") },
			want:  accepted("r-1"),
		},
		{
			name:  "the accept edge leaves a named turn's name alone",
			start: named("t_42"),
			edge:  func(d *sessionController) { d.noteTurnAcceptedLocked("r-1") },
			want:  named("t_42"),
		},
		{
			name:  "the ledger's claim names an accepted turn",
			start: accepted("r-1"),
			edge:  func(d *sessionController) { d.noteTurnClaimsLocked([]string{"t_42"}) },
			want:  named("t_42"),
		},
		{
			name:  "the ledger's claim names a turn nothing accepted",
			start: turnRecord{},
			edge:  func(d *sessionController) { d.noteTurnClaimsLocked([]string{"t_42"}) },
			want:  named("t_42"),
		},
		{
			name:  "an overlapping boundary renames the record onto the surviving claim",
			start: named("t_42"),
			edge:  func(d *sessionController) { d.noteTurnClaimsLocked([]string{"t_99"}) },
			want:  named("t_99"),
		},
		{
			name:  "an emptied claim set releases the record",
			start: named("t_42"),
			edge:  func(d *sessionController) { d.noteTurnClaimsLocked(nil) },
			want:  turnRecord{},
		},
		{
			name:  "the handshake adopts a turn this process never saw begin",
			start: turnRecord{},
			edge:  func(d *sessionController) { d.noteTurnAdoptedLocked(true) },
			want:  adopted,
		},
		{
			name:  "the handshake leaves a named turn's name alone",
			start: named("t_42"),
			edge:  func(d *sessionController) { d.noteTurnAdoptedLocked(true) },
			want:  named("t_42"),
		},
		{
			name:  "a handshake reporting no turn releases the record",
			start: adopted,
			edge:  func(d *sessionController) { d.noteTurnAdoptedLocked(false) },
			want:  turnRecord{},
		},
		{
			// The turn end, the ALREADY_COMPLETE reconciliation and the vendor
			// session rotation are all this one edge: the phase and the id go
			// together, so no writer can leave a name standing under a later latch.
			name:  "the release edge drops the phase and the id in one assignment",
			start: named("t_42"),
			edge:  func(d *sessionController) { d.noteTurnIdleLocked() },
			want:  turnRecord{},
		},
		{
			name:  "a refused submit restores exactly what the accept edge displaced",
			start: accepted("r-1"),
			edge:  func(d *sessionController) { d.noteTurnRestoreLocked(named("t_42")) },
			want:  named("t_42"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			d := &sessionController{workspace: "ws", sessionID: "s1", turn: tt.start}

			// Act.
			tt.edge(d)

			// Assert.
			if d.turn != tt.want {
				t.Fatalf("turn record = %#v, want %#v", d.turn, tt.want)
			}
		})
	}
}

// TestANamedTransitionRefusesAnEmptyTurnID is the REFUSED transition. A legacy
// start carries no turn id at all, so the ledger can hold a claim that names
// nothing; the record must never take that as `named("")`, which would be an
// active record whose provenance is a string nobody remembered to check.
func TestANamedTransitionRefusesAnEmptyTurnID(t *testing.T) {
	// Arrange.
	d := &sessionController{workspace: "ws", sessionID: "s1"}

	// Act.
	p := d.noteTurnClaimsLocked([]string{""})

	// Assert.
	if !p.unnamed {
		t.Error("the projection did not report the claim set as unnameable, so nothing would log the refusal")
	}
	if d.turn.phase != turnPhaseAdopted {
		t.Errorf("turn record = %#v, want the adopted phase: the turn is real and holds the drain, and nothing can name it", d.turn)
	}
	if id, named := d.turn.name(); named || id != "" {
		t.Errorf("turn record names (%q, %v), want the record to name NOTHING rather than an empty id", id, named)
	}
}

// TestARefusedNamingIsReportedThroughTheDaemonLog covers the OTHER half of the
// refusal: it is stated once, loudly, with the workspace and session on it.
func TestARefusedNamingIsReportedThroughTheDaemonLog(t *testing.T) {
	// Arrange.
	var mu sync.Mutex
	var lines []string
	h := newQueueHarnessWithHolds(t, nil, nil, func(format string, args ...any) {
		mu.Lock()
		defer mu.Unlock()
		lines = append(lines, fmt.Sprintf(format, args...))
	})

	// Act.
	h.m.noteTurnClaims(h.controller(), []string{""})

	// Assert.
	mu.Lock()
	defer mu.Unlock()
	found := false
	for _, line := range lines {
		if strings.Contains(line, "turn claim set NAMES NOTHING") && strings.Contains(line, `session=s1`) {
			found = true
		}
	}
	if !found {
		t.Fatalf("no loud line reported the unnameable claim set; log=%v", lines)
	}
}

// --- the ordering the flake lived in ----------------------------------------

// TestTheTurnRecordIsNamedBeforeTheSSMSeesTheBoundary pins the ordering the
// torn pair came from. The SSM apply is what publishes the WorkspaceState a
// frontend (and the e2e that reproduced this) reads as "a turn is in flight",
// and the name used to bind after it returned — so a scheduled shutdown taken on
// the strength of that publication derived a hold it could not name.
func TestTheTurnRecordIsNamedBeforeTheSSMSeesTheBoundary(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	d := h.controller()
	var mu sync.Mutex
	var atApply turnRecord
	h.applier.onApply = func(*corev1.Event) {
		h.m.mu.Lock()
		defer h.m.mu.Unlock()
		mu.Lock()
		defer mu.Unlock()
		atApply = d.turn
	}

	// Act.
	if err := d.consumer.Apply(turnStartEvent(corev1.Plane_PLANE_STREAM, 1, "t_42")); err != nil {
		t.Fatalf("Apply(TurnStarted): %v", err)
	}

	// Assert.
	mu.Lock()
	defer mu.Unlock()
	if id, named := atApply.name(); !named || id != "t_42" {
		t.Fatalf("turn record at the SSM apply = %s, want named(t_42): every frontend-visible consequence of this boundary follows the apply, so the hold must be nameable by then", atApply)
	}
}

// TestANamingEdgeTellsTheDrainEngine covers the notification the naming edge
// never used to make. The engine never trusts a delta — it re-reads DrainHolds —
// so a rename that told it nothing left it broadcasting the previous answer.
func TestANamingEdgeTellsTheDrainEngine(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	before := h.lease.activityCount()

	// Act.
	h.m.noteTurnClaims(h.controller(), []string{"t_42"})

	// Assert.
	if got := h.lease.activityCount(); got <= before {
		t.Fatalf("drain activity notifications = %d, want more than the %d before the naming edge", got, before)
	}
}

// --- the holds whose id does not come from the record ------------------------

// TestAnAcceptedHoldIsNamedFromTheDurableTurnLedger covers the window the
// record itself cannot answer for: the daemon committed to a submit and the
// turn's start has not been observed, so the hold's id comes from the durable
// turn claims instead of from process memory.
func TestAnAcceptedHoldIsNamedFromTheDurableTurnLedger(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	d := h.controller()
	h.m.mu.Lock()
	d.noteTurnAcceptedLocked("r-1")
	h.m.mu.Unlock()
	h.applier.setDurableTurns("t_42")

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{}})

	// Assert.
	if len(holds) != 1 || holds[0].TurnID != "t_42" {
		t.Fatalf("DrainHolds = %+v, want one hold naming t_42 from the durable turn claims", holds)
	}
}

// TestAnAcceptedHoldStandsWhenTheLedgerCannotBeRead covers the failure
// direction: the id is unresolvable, and the HOLD is what must not be dropped —
// a dropped hold is a bounce cutting live work.
func TestAnAcceptedHoldStandsWhenTheLedgerCannotBeRead(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	d := h.controller()
	h.m.mu.Lock()
	d.noteTurnAcceptedLocked("r-1")
	h.m.mu.Unlock()
	h.applier.setActiveTurnIDsErr(errors.New("state store is gone"))

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{}})

	// Assert.
	if len(holds) != 1 || !holds[0].TurnActive || holds[0].TurnID != "" {
		t.Fatalf("DrainHolds = %+v, want the hold to STAND with no turn id", holds)
	}
}

// TestAnAdoptedHoldBroadcastsTheDeliberateEmptyID covers the other end. The
// empty id is keyed on PROVENANCE — this process never saw the turn begin — and
// is not a lookup that failed, so the ledger is not consulted for it.
func TestAnAdoptedHoldBroadcastsTheDeliberateEmptyID(t *testing.T) {
	// Arrange.
	h := newLeaseHarness(t)
	d := h.controller()
	h.m.mu.Lock()
	d.noteTurnAdoptedLocked(true)
	h.m.mu.Unlock()
	h.applier.setDurableTurns("t_42")

	// Act.
	holds := h.m.DrainHolds(fakeTaskCounter{counts: map[string]int64{}})

	// Assert.
	if len(holds) != 1 || !holds[0].TurnActive || holds[0].TurnID != "" {
		t.Fatalf("DrainHolds = %+v, want an adopted hold carrying no turn id", holds)
	}
}
