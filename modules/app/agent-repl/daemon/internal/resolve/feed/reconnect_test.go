package feed

import (
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/ids"
)

// THE NETWORK COMING BACK ANSWERS A CONNECTION FAILURE (reconnect.go).

// connectionFailedTurn draws TURN's prompt and its connection-failed card.
func (h *harness) connectionFailedTurn(turn string) {
	h.t.Helper()
	h.deliverPrompt(turn, "question "+turn)
	h.outageTurn(turn, unreachableFailure())
}

func TestTheConnectionComingBackRetiresEveryConnectionFailedCard(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{}
	h := newDurableHarness(t, durableStores{reconnected: store})
	h.resolver.NetworkFaultOpened(testWorkspace, "f-1")
	h.connectionFailedTurn("t1")
	h.connectionFailedTurn("t2")

	// Act
	h.resolver.NetworkFaultClosed(testWorkspace, "f-1")

	// Assert
	if h.endingDrawn("t1") || h.endingDrawn("t2") {
		t.Fatalf("t1 drawn = %v, t2 drawn = %v, want both cards retired", h.endingDrawn("t1"), h.endingDrawn("t2"))
	}
	if h.rowByID(rootFeed(), promptRowID("t1").GetValue()) == nil {
		t.Fatal("the prompt the connection failed was retired too")
	}
	if !slices.Equal(store.turns, []ids.TurnID{"t1", "t2"}) {
		t.Fatalf("recorded %v, want t1 and t2", store.turns)
	}
	if !h.hasRecordWith("info", opReconnected, "retired", 2) {
		t.Fatalf("records = %+v, want the reconnection recorded at INFO", h.records())
	}
}

func TestAConnectionFailedCardStandsWhileANetworkFaultStands(t *testing.T) {
	cases := []struct {
		name  string
		close func(h *harness)
	}{
		{name: "no fault closed", close: func(*harness) {}},
		{name: "another fault still stands", close: func(h *harness) { h.resolver.NetworkFaultClosed(testWorkspace, "f-1") }},
		{name: "a fault the feed never held closed", close: func(h *harness) { h.resolver.NetworkFaultClosed(testWorkspace, "f-9") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.resolver.NetworkFaultOpened(testWorkspace, "f-1")
			h.resolver.NetworkFaultOpened(testWorkspace, "f-2")
			h.connectionFailedTurn("t1")

			// Act
			tc.close(h)

			// Assert
			if !h.endingDrawn("t1") {
				t.Fatal("the connection-failed card was retired while the network was still down")
			}
		})
	}
}

func TestTheConnectionComingBackRetiresOnlyConnectionFailedCards(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.NetworkFaultOpened(testWorkspace, "f-1")
	h.deliverPrompt("t1", "question")
	h.outageTurn("t1", overloaded())

	// Act
	h.resolver.NetworkFaultClosed(testWorkspace, "f-1")

	// Assert
	if !h.endingDrawn("t1") {
		t.Fatal("the network coming back retired an overload's card")
	}
	if h.hasRecord("info", opReconnected) {
		t.Fatalf("records = %+v, want no reconnection recorded", h.records())
	}
}

func TestAConnectionFailedCardOfAnAnsweredTurnArrivingLaterIsNotDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.resolver.NetworkFaultOpened(testWorkspace, "f-1")
	h.connectionFailedTurn("t1")
	h.resolver.NetworkFaultClosed(testWorkspace, "f-1")

	// Act: the other store plane re-delivers the ending.
	h.outageTurn("t1", unreachableFailure())

	// Assert
	if h.endingDrawn("t1") {
		t.Fatal("the answered turn's connection-failed card was drawn again")
	}
}

func TestAnAnsweredConnectionFailureStaysRetiredAfterARestart(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{}
	before := newDurableHarness(t, durableStores{reconnected: store})
	before.resolver.NetworkFaultOpened(testWorkspace, "f-1")
	before.connectionFailedTurn("t1")
	before.resolver.NetworkFaultClosed(testWorkspace, "f-1")

	// Act: the successor replays the conversation.
	after := newDurableHarness(t, durableStores{reconnected: store})
	after.connectionFailedTurn("t1")

	// Assert
	if after.endingDrawn("t1") {
		t.Fatal("the answered connection-failed card was drawn again after a restart")
	}
	if !after.hasRecord("info", opReconnectedRestore) {
		t.Fatalf("records = %+v, want the load recorded at INFO", after.records())
	}
}

func TestAFailedReconnectionRecordStillRetiresTheCardsAndIsRaised(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{putErr: errors.New("disk full")}
	h := newDurableHarness(t, durableStores{reconnected: store})
	h.resolver.NetworkFaultOpened(testWorkspace, "f-1")
	h.connectionFailedTurn("t1")

	// Act
	h.resolver.NetworkFaultClosed(testWorkspace, "f-1")

	// Assert
	if h.endingDrawn("t1") {
		t.Fatal("the card stood because recording it failed")
	}
	assertRecordedFault(t, h, opReconnected, "disk full")
}

func TestAFailedReconnectionLoadIsLoggedAndRaised(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{loadErr: errors.New("corrupt page")}

	// Act
	h := newDurableHarness(t, durableStores{reconnected: store})

	// Assert
	assertRecordedFault(t, h, opReconnectedRestore, "corrupt page")
}

func TestTheConnectionComingBackWithNoCardRecordsNothing(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{}
	h := newDurableHarness(t, durableStores{reconnected: store})
	h.resolver.NetworkFaultOpened(testWorkspace, "f-1")

	// Act
	h.resolver.NetworkFaultClosed(testWorkspace, "f-1")

	// Assert
	if len(store.turns) != 0 {
		t.Fatalf("recorded %v, want nothing", store.turns)
	}
}
