package promptqueue

import (
	"context"
	"errors"
	"testing"
	"time"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE DOOR ITSELF: the row closes and the feed is told together, and a failed
// durable write is recorded without hiding the turn's end.

func TestTheDoorTellsTheFeedTheCloseItRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.q.OnTurnEnded(theWorkspace, "turn-1", wsm.CloseAgentDied)

	// Assert
	if got := h.db.closedTurns["turn-1"]; got != wsm.CloseAgentDied {
		t.Fatalf("durable close = %s, want agent_died", closeName(got))
	}
	if got := h.feed.closedTells(); len(got) != 1 || got[0] != (closedTell{turn: "turn-1", how: wsm.CloseAgentDied}) {
		t.Fatalf("feed tells = %+v, want the one agent_died close", got)
	}
}

func TestTheDoorDrawsTheEndingWhenTheDurableWriteFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.closeTurnErrs = map[ids.TurnID]error{"turn-1": errors.New("the store is down")}

	// Act
	h.q.OnTurnEnded(theWorkspace, "turn-1", wsm.CloseFailed)

	// Assert
	if got := h.feed.closedTells(); len(got) != 1 {
		t.Fatalf("feed tells = %+v, want the ending drawn despite the failed write", got)
	}
	logged := false
	for _, record := range h.log.Records() {
		logged = logged || (record.Level == "error" && record.Operation == opTurnEnded)
	}
	if !logged {
		t.Fatalf("records = %+v, want the failed write at ERROR", h.log.Records())
	}
}

func TestATurnOfAnUnresolvableWorkspaceStillCloses(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.workspaceErr = errors.New("the workspace read failed")

	// Act
	h.q.OnTurnEnded(theWorkspace, "turn-1", wsm.CloseCompleted)

	// Assert
	if got := h.db.closedTurns["turn-1"]; got != wsm.CloseCompleted {
		t.Fatalf("durable close = %s, want completed", closeName(got))
	}
	if got := h.feed.closedTells(); len(got) != 1 {
		t.Fatalf("feed tells = %+v, want the ending drawn", got)
	}
}

func TestCloseOrphansTellsTheFeedEveryTurnItClosed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	for _, turn := range []ids.TurnID{"turn-1", "turn-2"} {
		h.db.turns[turn] = &wsm.Turn{ID: turn, Workspace: theWorkspace}
	}

	// Act
	report, err := h.q.CloseOrphans(context.Background(), theWorkspace, time.UnixMilli(1))

	// Assert
	if err != nil || len(report.Turns) != 2 {
		t.Fatalf("CloseOrphans = (%+v, %v), want both turns closed", report, err)
	}
	if got := h.feed.closedTells(); len(got) != 2 || got[0].how != wsm.CloseOrphaned || got[1].how != wsm.CloseOrphaned {
		t.Fatalf("feed tells = %+v, want both turns told orphaned", got)
	}
}

func TestCloseOrphansThatFailsTellsTheFeedNothing(t *testing.T) {
	// Arrange: the transaction is whole or nothing, so nothing closed.
	h := newHarness(t)
	h.db.turns["turn-1"] = &wsm.Turn{ID: "turn-1", Workspace: theWorkspace}
	h.db.orphansErr = errors.New("the store is down")

	// Act
	_, err := h.q.CloseOrphans(context.Background(), theWorkspace, time.UnixMilli(1))

	// Assert
	if err == nil {
		t.Fatalf("CloseOrphans succeeded over a failed transaction")
	}
	if got := h.feed.closedTells(); len(got) != 0 {
		t.Fatalf("feed tells = %+v, want none for a close that did not happen", got)
	}
}

func TestClaimingAnOpenDisplacedTurnDrawsItsEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.turns["turn-1"] = &wsm.Turn{ID: "turn-1", Workspace: theWorkspace, Displaced: true}

	// Act
	claimed, err := h.q.ClaimDisplacedTurn(context.Background(), theWorkspace, "turn-1")

	// Assert
	if err != nil || !claimed {
		t.Fatalf("ClaimDisplacedTurn = (%v, %v), want (true, nil)", claimed, err)
	}
	if got := h.feed.closedTells(); len(got) != 1 || got[0].how != wsm.CloseOrphaned {
		t.Fatalf("feed tells = %+v, want the orphaned ending", got)
	}
}

func TestClaimingAnAlreadyClosedDisplacedTurnDrawsNothing(t *testing.T) {
	// Arrange: the capture's kill ended the turn through the door already.
	h := newHarness(t)
	how := wsm.CloseKilled
	h.db.turns["turn-1"] = &wsm.Turn{ID: "turn-1", Workspace: theWorkspace, Displaced: true, Close: &how}

	// Act
	claimed, err := h.q.ClaimDisplacedTurn(context.Background(), theWorkspace, "turn-1")

	// Assert
	if err != nil || !claimed {
		t.Fatalf("ClaimDisplacedTurn = (%v, %v), want (true, nil)", claimed, err)
	}
	if got := h.feed.closedTells(); len(got) != 0 {
		t.Fatalf("feed tells = %+v, want none: the claim closed nothing", got)
	}
}

func TestAFailedDisplacedClaimIsHandedBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.claimErr = errors.New("the store is down")

	// Act
	_, err := h.q.ClaimDisplacedTurn(context.Background(), theWorkspace, "turn-1")

	// Assert
	if err == nil {
		t.Fatalf("ClaimDisplacedTurn succeeded over a failed claim")
	}
}
