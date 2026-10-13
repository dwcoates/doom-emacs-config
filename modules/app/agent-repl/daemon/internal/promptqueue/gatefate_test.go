package promptqueue

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// THE 2026-10-13 INCIDENT, deterministically: the owner answered a cold gate
// with clear and the prompts held at the gate were sent into the cleared
// session. A gate's answer now settles the held prompts' fate before the
// session comes back (gatefate.go).

// answered runs the gate's answer with FATE on the workspace, failing the test
// on an error.
func answered(t *testing.T, h *harness, fate GateFate) {
	t.Helper()
	if err := h.q.ColdGateAnswered(context.Background(), theWorkspace, fate); err != nil {
		t.Fatalf("ColdGateAnswered(%s) = %v, want nil", fate, err)
	}
}

// keptAtAnEarlierGate holds t1 at a gate the user answered with KEEP, and
// brings the session up as that answer does.
func keptAtAnEarlierGate(t *testing.T) *harness {
	t.Helper()
	h := revivedIntoTheGate(t)
	answered(t, h, GateKeep)
	answerGate(h)
	h.q.ReleaseReconnectHolds(theWorkspace)
	return h
}

// lastGateCount answers the last count of prompts held at a gate the queue
// reported.
func (h *harness) lastGateCount(t *testing.T) int {
	t.Helper()
	h.gateMu.Lock()
	defer h.gateMu.Unlock()
	if len(h.gateCounts) == 0 {
		t.Fatal("the queue reported no count of prompts held at a cold gate")
	}
	return h.gateCounts[len(h.gateCounts)-1]
}

func TestAnsweringTheGateSettlesTheHeldPromptsFate(t *testing.T) {
	tests := []struct {
		name string
		fate GateFate
		// wantHold is the hold t1 stands on afterwards; nil wants it retired.
		wantHold *wsm.HoldKind
	}{
		{name: "clear discards it", fate: GateDiscard, wantHold: nil},
		{name: "pay or compact keeps it for the user", fate: GateKeep, wantHold: kindPtr(wsm.HoldColdGateAnswered)},
		{name: "compact and resume leaves it to be sent", fate: GateSend, wantHold: kindPtr(wsm.HoldColdGate)},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := revivedIntoTheGate(t)

			// Act
			answered(t, h, tc.fate)

			// Assert
			got := h.db.hold("t1")
			if tc.wantHold == nil {
				if got.Tombstone == nil || got.Tombstone.Kind != tombstoneCleared {
					t.Fatalf("hold = %+v, want t1 retired as cleared at the cold gate", got)
				}
				return
			}
			if !heldUnder(h, "t1", *tc.wantHold) {
				t.Fatalf("hold = %+v, want t1 standing on %s", got, *tc.wantHold)
			}
		})
	}
}

func TestOnlyTheSendFateDeliversOnceTheSessionIsUp(t *testing.T) {
	tests := []struct {
		name        string
		fate        GateFate
		wantStarted int
	}{
		{name: "clear sends nothing", fate: GateDiscard, wantStarted: 0},
		{name: "pay or compact sends nothing", fate: GateKeep, wantStarted: 0},
		{name: "compact and resume sends the held prompt", fate: GateSend, wantStarted: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := revivedIntoTheGate(t)
			answered(t, h, tc.fate)
			answerGate(h)

			// Act: the answer's session-up edge.
			h.q.ReleaseReconnectHolds(theWorkspace)

			// Assert
			if got := h.sender.started(); len(got) != tc.wantStarted {
				t.Fatalf("started = %v, want %d turn(s)", got, tc.wantStarted)
			}
		})
	}
}

func TestAKeptPromptIsNotDeliveredByATurnEnd(t *testing.T) {
	// Arrange
	h := keptAtAnEarlierGate(t)
	running(t, h, "later-turn", "something else")
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "later-turn", wsm.CloseCompleted)

	// Assert
	if got := h.sender.started(); len(got) != 0 || !heldUnder(h, "t1", wsm.HoldColdGateAnswered) {
		t.Fatalf("started = %v, t1 = %+v; want nothing sent and the kept prompt still kept", got, h.db.hold("t1"))
	}
}

func TestASubmissionBesideAKeptPromptIsDeliveredAndLeavesItKept(t *testing.T) {
	// Arrange
	h := keptAtAnEarlierGate(t)

	// Act
	disposition, err := h.q.Submit(context.Background(), submission("t2", "a fresh prompt"))

	// Assert
	if err != nil || !disposition.Delivered || !heldUnder(h, "t1", wsm.HoldColdGateAnswered) {
		t.Fatalf("disposition = %+v, err = %v, t1 = %+v; want t2 delivered and t1 still kept", disposition, err, h.db.hold("t1"))
	}
}

func TestAPromptQueuedAfterAKeptOneIsNotJudgedAgainstIt(t *testing.T) {
	// Arrange
	h := keptAtAnEarlierGate(t)
	strandedRow(t, h, "t2")

	// Act
	kind, _, err := h.q.itemAhead(context.Background(), theWorkspace, "t2")

	// Assert
	if err != nil || kind != aheadRunning {
		t.Fatalf("itemAhead = %v, %v; want the running turn ahead, the kept prompt standing outside the line", kind, err)
	}
}

func TestReleasingAKeptPromptDeliversIt(t *testing.T) {
	// Arrange
	h := keptAtAnEarlierGate(t)

	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")

	// Assert
	if got := h.sender.started(); err != nil || len(got) != 1 || got[0] != "t1" {
		t.Fatalf("Release = %v, started = %v; want t1 delivered", err, got)
	}
}

func TestReleasingAKeptPromptAtAStandingGateIsRefused(t *testing.T) {
	// Arrange: the kept prompt's session parked at a gate again.
	h := keptAtAnEarlierGate(t)
	parkAtGate(h)

	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")

	// Assert
	if !errors.Is(err, ErrReleaseRefused) || !heldUnder(h, "t1", wsm.HoldColdGateAnswered) {
		t.Fatalf("Release = %v, t1 = %+v; want refused and t1 still kept", err, h.db.hold("t1"))
	}
}

func TestALaterGatesClearDiscardsAKeptPrompt(t *testing.T) {
	// Arrange
	h := keptAtAnEarlierGate(t)
	parkAtGate(h)

	// Act
	answered(t, h, GateDiscard)

	// Assert
	if got := h.db.hold("t1").Tombstone; got == nil || got.Kind != tombstoneCleared {
		t.Fatalf("tombstone = %+v, want the kept prompt discarded", got)
	}
}

func TestALaterGatesCompactAndResumeSendsAKeptPrompt(t *testing.T) {
	// Arrange
	h := keptAtAnEarlierGate(t)
	parkAtGate(h)
	answered(t, h, GateSend)
	answerGate(h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want the kept prompt sent", got)
	}
}

func TestTheClearLeavesAPromptOfAnotherHoldStanding(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	strandedRow(t, h, "t2")
	refresh := wsm.HoldBuildRefresh
	if err := h.db.UpdateHeldPromptHold(context.Background(), "t2", &refresh, ""); err != nil {
		t.Fatalf("UpdateHeldPromptHold: %v", err)
	}

	// Act
	answered(t, h, GateDiscard)

	// Assert
	if !heldUnder(h, "t2", wsm.HoldBuildRefresh) {
		t.Fatalf("t2 = %+v, want the build-refresh hold untouched", h.db.hold("t2"))
	}
}

func TestAnAnswerWithNothingHeldSettlesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	parkAtGate(h)

	// Act
	err := h.q.ColdGateAnswered(context.Background(), theWorkspace, GateDiscard)

	// Assert
	if err != nil || !logged(h.log.Records(), "info", opColdGate, "no prompt is held at the cold gate; the answer has nothing to settle") {
		t.Fatalf("ColdGateAnswered = %v, records = %+v; want nil and the INFO record", err, h.log.Records())
	}
}

func TestAClearWhoseDiscardFailsIsRefusedAndDiscardsNothing(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	h.db.tombstoneErr = errors.New("disk full")

	// Act
	err := h.q.ColdGateAnswered(context.Background(), theWorkspace, GateDiscard)

	// Assert
	if err == nil || !heldUnder(h, "t1", wsm.HoldColdGate) {
		t.Fatalf("ColdGateAnswered = %v, t1 = %+v; want an error and t1 still held at the gate", err, h.db.hold("t1"))
	}
}

func TestAClearWhoseDiscardFailsIsRecordedAtError(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	h.db.tombstoneErr = errors.New("disk full")

	// Act
	_ = h.q.ColdGateAnswered(context.Background(), theWorkspace, GateDiscard)

	// Assert
	r, ok := recordFor(h.log.Records(), "error", opColdGate, "the prompts held at the cold gate could not be discarded; the clear is refused")
	if !ok || r.Context["cause"] != "disk full" || r.Context["held"] != 1 || r.Context["fate"] != "discard" {
		t.Fatalf("records = %+v, want the ERROR record with its cause, count and fate", h.log.Records())
	}
}

func TestAKeepWhoseRestampFailsIsRefused(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	h.db.holdErr = errors.New("disk full")

	// Act
	err := h.q.ColdGateAnswered(context.Background(), theWorkspace, GateKeep)

	// Assert
	if err == nil || !heldUnder(h, "t1", wsm.HoldColdGate) {
		t.Fatalf("ColdGateAnswered = %v, t1 = %+v; want an error and t1 unmoved", err, h.db.hold("t1"))
	}
}

func TestAKeepWhoseRestampFailsIsRecordedAtError(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	h.db.holdErr = errors.New("disk full")

	// Act
	_ = h.q.ColdGateAnswered(context.Background(), theWorkspace, GateKeep)

	// Assert
	r, ok := recordFor(h.log.Records(), "error", opColdGate, "a prompt held at the cold gate could not be moved to its answer's hold; the answer is refused")
	if !ok || r.Context["turn"] != "t1" || r.Context["cause"] != "disk full" || r.Context["hold"] != "cold_gate_answered" {
		t.Fatalf("records = %+v, want the ERROR record with the turn, hold and cause", h.log.Records())
	}
}

func TestAnAnswerWhoseHoldsCannotBeReadIsRefused(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	h.db.heldErr = errors.New("disk full")

	// Act
	err := h.q.ColdGateAnswered(context.Background(), theWorkspace, GateKeep)

	// Assert
	if err == nil || !logged(h.log.Records(), "error", opColdGate, "the prompts held at the cold gate could not be read; the answer is refused") {
		t.Fatalf("ColdGateAnswered = %v, want an error and the ERROR record", err)
	}
}

func TestAnUnknownFateIsRefusedLoudly(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)

	// Act
	err := h.q.ColdGateAnswered(context.Background(), theWorkspace, GateFate(99))

	// Assert
	r, ok := recordFor(h.log.Records(), "error", opColdGate, "the cold gate's answer named no fate for its held prompts")
	if err == nil || !ok || r.Context["invariant_violation"] == nil || !heldUnder(h, "t1", wsm.HoldColdGate) {
		t.Fatalf("ColdGateAnswered = %v, records = %+v; want an error, the invariant record, and t1 unmoved", err, h.log.Records())
	}
}

func TestTheTrayReportsHowManyPromptsAreHeldAtTheGate(t *testing.T) {
	tests := []struct {
		name string
		fate GateFate
		want int
	}{
		{name: "a kept prompt is still held at the gate", fate: GateKeep, want: 1},
		{name: "a discarded prompt is not", fate: GateDiscard, want: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := revivedIntoTheGate(t)

			// Act
			answered(t, h, tc.fate)

			// Assert
			if got := h.lastGateCount(t); got != tc.want {
				t.Fatalf("held at the gate = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestTheTrayCountsAPromptWaitingAtTheGate(t *testing.T) {
	// Arrange / Act
	h := revivedIntoTheGate(t)

	// Assert
	if got := h.lastGateCount(t); got != 1 {
		t.Fatalf("held at the gate = %d, want 1", got)
	}
}

// kindPtr is a hold kind's address.
func kindPtr(k wsm.HoldKind) *wsm.HoldKind { return &k }

// recordFor answers the first record at LEVEL under OP carrying MESSAGE.
func recordFor(records []dlog.Record, level, op, message string) (dlog.Record, bool) {
	for _, r := range records {
		if r.Level == level && r.Operation == op && r.Message == message {
			return r, true
		}
	}
	return dlog.Record{}, false
}
