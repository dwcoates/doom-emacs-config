package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// turnPtr is a turn by address.
func turnPtr(turn ids.TurnID) *ids.TurnID { return &turn }

func TestTurnOfNamesTheWireTurn(t *testing.T) {
	// Arrange / Act
	got := turnOf(&conversationv1.TurnId{Value: "turn-1"})

	// Assert
	if got == nil || *got != "turn-1" {
		t.Fatalf("turnOf = %v, want turn-1", got)
	}
}

func TestTurnOfAnEmptyWireTurnIsNone(t *testing.T) {
	// Arrange / Act
	got := turnOf(&conversationv1.TurnId{})

	// Assert
	if got != nil {
		t.Fatalf("turnOf = %v, want none", *got)
	}
}

func TestTurnOfANilWireTurnIsNone(t *testing.T) {
	// Arrange / Act
	got := turnOf(nil)

	// Assert
	if got != nil {
		t.Fatalf("turnOf = %v, want none", *got)
	}
}

// A ROW IS DRAWN WHERE ITS OWN TURN DRAWS (turnaddress.go): a merge's own
// turn in its tab, every other turn on the root feed, and nothing copied
// between them.

// addressedMergeTurn stands a merge tab's address and hands TURN over at it,
// as the prompt queue does for a turn the merge starts; it answers the tab.
func addressedMergeTurn(h *harness, lease ids.LeaseID, turn ids.TurnID) feedid.Ref {
	addr := recordedTabAddress(lease)
	h.resolver.SetOutputAddress(testWorkspace, addr)
	h.resolver.AddressTurn(testWorkspace, turn, addr)
	return *addr.Parent
}

// answer delivers a settled main-agent response of TURN.
func (h *harness) answer(turn, unit, markdown string) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseSuccessActivity(unit, markdown),
		&conversationv1.TurnId{Value: turn}, nil, noAddress())
}

func TestARepairTurnsRowsLandInTheMergeTab(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	parent := addressedMergeTurn(h, lease, "repair-1")

	// Act
	h.deliverPrompt("repair-1", "resolve the conflict")
	h.answer("repair-1", "unit-1", "resolved")

	// Assert
	rows := h.rows(feedid.Feed{Merge: &lease})
	if len(rows) != 2 {
		t.Fatalf("tab rows = %v, want the repair's prompt and its answer", rowIDs(rows))
	}
	for _, row := range rows {
		if row.GetParent().GetRow().GetValue() != testEncode(parent).GetValue() {
			t.Fatalf("row %s parent = %v, want the addressed tab", row.GetId().GetValue(), row.GetParent())
		}
	}
}

func TestARepairTurnDrawsNothingOnTheRootFeed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	addressedMergeTurn(h, lease, "repair-1")

	// Act
	h.deliverPrompt("repair-1", "resolve the conflict")
	h.answer("repair-1", "unit-1", "resolved")
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), turnPtr("repair-1"), completed(""), nil, nil, noAddress())

	// Assert
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("root rows = %v, want no copy of the repair turn's rows", rowIDs(rows))
	}
}

func TestAUserPromptSentMidMergeLandsOnTheRootFeed(t *testing.T) {
	// Arrange: a merge's address stands; the user's turn was not handed it.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.resolver.SetOutputAddress(testWorkspace, &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}})

	// Act
	h.deliverPrompt("turn-1", "what is the merge doing?")
	h.answer("turn-1", "unit-1", "rebasing")

	// Assert
	if rows := h.rows(rootFeed()); len(rows) != 2 {
		t.Fatalf("root rows = %v, want the user's prompt and its answer", rowIDs(rows))
	}
	if rows := h.rows(feedid.Feed{Merge: &lease}); len(rows) != 0 {
		t.Fatalf("tab rows = %v, want none of the user's turn", rowIDs(rows))
	}
}

func TestInterleavedTurnsEachDrawWhereTheirOwnTurnDraws(t *testing.T) {
	// Arrange: a repair turn and a user turn of one session, interleaved.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	addressedMergeTurn(h, lease, "repair-1")
	h.deliverPrompt("repair-1", "resolve the conflict")
	h.deliverPrompt("user-1", "and meanwhile?")

	// Act
	h.answer("repair-1", "unit-r", "resolved")
	h.answer("user-1", "unit-u", "meanwhile, this")

	// Assert
	wantTab := []string{
		testEncode(feedid.Ref{WS: testWorkspace, Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: "repair-1"}}).GetValue(),
	}
	tab := rowIDs(h.rows(feedid.Feed{Merge: &lease}))
	if len(tab) != 2 || tab[0] != wantTab[0] {
		t.Fatalf("tab rows = %v, want the repair's prompt then its answer", tab)
	}
	root := rowIDs(h.rows(rootFeed()))
	if len(root) != 2 || root[0] != h.promptRowID("user-1") {
		t.Fatalf("root rows = %v, want the user's prompt then its answer", root)
	}
}

func TestAMergeTabRowNeverReachesTheRootFeedWhileARepairTurnRuns(t *testing.T) {
	// Arrange: a repair turn is running in the tab.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	tab := addressedMergeTurn(h, lease, "repair-1")
	h.deliverPrompt("repair-1", "resolve the conflict")

	// Act: the orchestrator upserts the tab row on the merge feed.
	h.resolver.UpsertSynthesized(testWorkspace, feedid.Feed{Merge: &lease}, &frontendv1.FeedRow{
		Id:  testEncode(tab),
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{}},
	})

	// Assert
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("root rows = %v, want the tab row on the merge feed alone", rowIDs(rows))
	}
}

func TestAnAddressedTurnsLateFrameIsDrawnInItsTabAfterTheAddressIsWithdrawn(t *testing.T) {
	// Arrange: the merge ended while its repair turn still ran.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	addressedMergeTurn(h, lease, "repair-1")
	h.deliverPrompt("repair-1", "resolve the conflict")
	h.resolver.SetOutputAddress(testWorkspace, nil)

	// Act
	h.answer("repair-1", "unit-1", "resolved late")

	// Assert
	if rows := h.rows(feedid.Feed{Merge: &lease}); len(rows) != 2 {
		t.Fatalf("tab rows = %v, want the late answer in the turn's tab", rowIDs(rows))
	}
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("root rows = %v, want none", rowIDs(rows))
	}
}

func TestAnAddressedTurnsLateFrameIsSaidOnceAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	addressedMergeTurn(h, lease, "repair-1")
	h.deliverPrompt("repair-1", "resolve the conflict")
	h.resolver.SetOutputAddress(testWorkspace, nil)

	// Act
	h.answer("repair-1", "unit-1", "late")
	h.answer("repair-1", "unit-2", "later")

	// Assert
	said := 0
	for _, record := range h.records() {
		if record.Operation == "daemon.feed.turn_address_outlived" && record.Level == "info" {
			said++
		}
	}
	if said != 1 {
		t.Fatalf("outlived records = %d, want exactly one at info", said)
	}
}

func TestAddressTurnWithNoAddressDrawsTheTurnOnTheRoot(t *testing.T) {
	// Arrange: the turn was handed an address, then handed none.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	addressedMergeTurn(h, lease, "turn-1")

	// Act
	h.resolver.AddressTurn(testWorkspace, "turn-1", nil)
	h.deliverPrompt("turn-1", "on the root")

	// Assert
	h.only(rootFeed())
}

func TestAddressTurnKeepsItsOwnCopyOfTheAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	addr := &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}}
	h.resolver.AddressTurn(testWorkspace, "turn-1", addr)

	// Act: the caller changes its value after handing it over.
	addr.Feed = rootFeed()
	h.deliverPrompt("turn-1", "in the tab")

	// Assert
	h.only(feedid.Feed{Merge: &lease})
}
