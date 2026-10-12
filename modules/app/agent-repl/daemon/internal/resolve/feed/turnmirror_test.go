package feed

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// requesterWorkspace is the workspace a mirrored turn is also drawn in.
const requesterWorkspace = ids.WorkspaceID("ws-requester")

// pickMirror is a mirror into the requester's bubble of LEASE, under its pick
// tab.
func pickMirror(lease ids.LeaseID) *wsm.MirrorAddress {
	return &wsm.MirrorAddress{
		Workspace: requesterWorkspace,
		Feed:      feedid.Feed{Merge: &lease},
		Parent:    feedid.Ref{WS: requesterWorkspace, Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "pick", Sub: "1"}},
	}
}

// mirroredTurn stands MIRROR and hands TURN over at it, as the prompt queue
// does for a turn an async merge starts.
func mirroredTurn(h *harness, turn ids.TurnID, mirror *wsm.MirrorAddress) {
	h.resolver.SetOutputMirror(testWorkspace, mirror)
	h.resolver.MirrorTurn(testWorkspace, turn, mirror)
}

// rowsOf answers one feed's rows of workspace WS in order.
func rowsOf(r *resolver, ws ids.WorkspaceID, feed feedid.Feed) []*frontendv1.FeedRow {
	r.mu.Lock()
	defer r.mu.Unlock()
	f := r.feed(r.state(ws), feed)
	out := make([]*frontendv1.FeedRow, 0, len(f.order))
	for _, id := range f.order {
		out = append(out, f.rows[id])
	}
	return out
}

func TestAMirroredTurnsRowsAreDrawnInItsOwnRootFeed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	mirroredTurn(h, "pick-1", pickMirror("lease-7"))

	// Act
	h.deliverPrompt("pick-1", "pick the fix")
	h.answer("pick-1", "unit-1", "picked")

	// Assert
	if rows := h.rows(rootFeed()); len(rows) != 2 {
		t.Fatalf("root rows = %v, want the pick's prompt and its answer", rowIDs(rows))
	}
}

func TestAMirroredTurnsRowsAreDrawnAgainUnderTheMirrorsParent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mirror := pickMirror(lease)
	mirroredTurn(h, "pick-1", mirror)

	// Act
	h.deliverPrompt("pick-1", "pick the fix")
	h.answer("pick-1", "unit-1", "picked")

	// Assert: each root row stands again in the requester's bubble, under the
	// pick tab, re-identified into that workspace and feed, with no turn.
	own := h.rows(rootFeed())
	mirrored := rowsOf(h.resolver, requesterWorkspace, feedid.Feed{Merge: &lease})
	if len(mirrored) != len(own) {
		t.Fatalf("mirrored rows = %v, want one per root row %v", rowIDs(mirrored), rowIDs(own))
	}
	for i, row := range mirrored {
		ref, err := testDecode(row.GetId())
		if err != nil {
			t.Fatalf("mirrored id %q: %v", row.GetId().GetValue(), err)
		}
		source, _ := testDecode(own[i].GetId())
		if ref.WS != requesterWorkspace || ref.Feed.Merge == nil || *ref.Feed.Merge != lease || ref.Row != source.Row {
			t.Fatalf("mirrored id = %+v, want the source's row key in the requester's bubble", ref)
		}
		if row.GetParent().GetRow().GetValue() != testEncode(mirror.Parent).GetValue() {
			t.Fatalf("mirrored parent = %v, want the pick tab", row.GetParent())
		}
		if row.GetTurn() != nil {
			t.Fatalf("mirrored row carries turn %v, want none", row.GetTurn())
		}
	}
}

func TestATurnWithNoMirrorIsDrawnInItsOwnWorkspaceOnly(t *testing.T) {
	// Arrange: a mirror stands, but the user's turn was not handed it.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.resolver.SetOutputMirror(testWorkspace, pickMirror(lease))

	// Act
	h.deliverPrompt("turn-1", "an ordinary turn")

	// Assert
	if rows := rowsOf(h.resolver, requesterWorkspace, feedid.Feed{Merge: &lease}); len(rows) != 0 {
		t.Fatalf("mirrored rows = %v, want none for a turn with no mirror", rowIDs(rows))
	}
}

func TestANestedRowOfAMirroredTurnKeepsItsParentReidentified(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mirror := pickMirror(lease)
	mirroredTurn(h, "pick-1", mirror)
	parent := feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-0"}}
	row := &frontendv1.FeedRow{
		Id:     testEncode(feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-1"}}),
		Parent: &frontendv1.FeedRowParent{Row: testEncode(parent)},
		Turn:   &conversationv1.TurnId{Value: "pick-1"},
	}

	// Act
	h.resolver.mu.Lock()
	h.resolver.upsert(h.resolver.state(testWorkspace), placement{feed: rootFeed()}, row, true)
	h.resolver.mu.Unlock()

	// Assert
	mirrored := rowsOf(h.resolver, requesterWorkspace, feedid.Feed{Merge: &lease})
	want := testEncode(feedid.Ref{WS: requesterWorkspace, Feed: feedid.Feed{Merge: &lease}, Row: parent.Row}).GetValue()
	if len(mirrored) != 1 || mirrored[0].GetParent().GetRow().GetValue() != want {
		t.Fatalf("mirrored = %v, want the row under its re-identified parent %s", mirrored, want)
	}
}

func TestAMirroredRowIsRecordedDurablyInTheMirrorsWorkspace(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	lease := ids.LeaseID("lease-7")
	mirror := pickMirror(lease)
	r.SetOutputMirror(testWorkspace, mirror)
	r.MirrorTurn(testWorkspace, "pick-1", mirror)
	row := &frontendv1.FeedRow{
		Id:   testEncode(feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-1"}}),
		Turn: &conversationv1.TurnId{Value: "pick-1"},
	}

	// Act
	r.mu.Lock()
	r.upsert(r.state(testWorkspace), placement{feed: rootFeed()}, row, false)
	r.mu.Unlock()

	// Assert
	id := testEncode(feedid.Ref{WS: requesterWorkspace, Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-1"}}).GetValue()
	if _, ok := store.recorded(requesterWorkspace, id); !ok {
		t.Fatal("the mirrored row was not recorded durably in the requester's workspace")
	}
}

func TestARowWhoseIdentityWillNotDecodeIsNotMirroredAndIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	mirroredTurn(h, "pick-1", pickMirror(lease))
	row := &frontendv1.FeedRow{Id: &frontendv1.FeedId{Value: "not-a-row-id"}, Turn: &conversationv1.TurnId{Value: "pick-1"}}

	// Act
	h.resolver.mu.Lock()
	h.resolver.upsert(h.resolver.state(testWorkspace), placement{feed: rootFeed()}, row, true)
	h.resolver.mu.Unlock()

	// Assert
	if rows := rowsOf(h.resolver, requesterWorkspace, feedid.Feed{Merge: &lease}); len(rows) != 0 {
		t.Fatalf("mirrored rows = %v, want none", rowIDs(rows))
	}
	if !h.hasRecord("error", "daemon.feed.mirror_undecodable") {
		t.Fatalf("the undecodable row was not recorded at ERROR: %+v", h.records())
	}
}

func TestOutputMirrorAnswersTheStandingMirrorAndNoneOnceWithdrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	mirror := pickMirror("lease-7")

	// Act
	h.resolver.SetOutputMirror(testWorkspace, mirror)
	standing := h.resolver.OutputMirror(testWorkspace)
	h.resolver.SetOutputMirror(testWorkspace, nil)
	withdrawn := h.resolver.OutputMirror(testWorkspace)

	// Assert
	if standing == nil || standing.Workspace != requesterWorkspace {
		t.Fatalf("standing mirror = %+v, want the one installed", standing)
	}
	if withdrawn != nil {
		t.Fatalf("mirror after withdrawal = %+v, want none", withdrawn)
	}
}

func TestOutputMirrorOfAnUnknownWorkspaceIsNone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act / Assert
	if m := h.resolver.OutputMirror("ws-unknown"); m != nil {
		t.Fatalf("OutputMirror = %+v, want none", m)
	}
}

func TestAReplayedTurnRecordedWithAMirrorIsMirroredAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.mirrors = map[ids.TurnID]*wsm.MirrorAddress{"pick-1": pickMirror(lease)}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("pick-1", "pick the fix")))

	// Assert
	h.only(rootFeed())
	if rows := rowsOf(h.resolver, requesterWorkspace, feedid.Feed{Merge: &lease}); len(rows) != 1 {
		t.Fatalf("mirrored rows = %v, want the replayed prompt", rowIDs(rows))
	}
}

func TestAnUnreadableRecordedMirrorIsAnErrorAndThePageStillDraws(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.mirrors = map[ids.TurnID]*wsm.MirrorAddress{"pick-1": pickMirror("lease-7")}
	h.mirrorsErr = errors.New("database is locked")

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("pick-1", "pick the fix")))

	// Assert
	if !h.hasRecord("error", "daemon.feed.turn_mirrors_unreadable") {
		t.Fatalf("the failed read was not recorded at ERROR: %+v", h.records())
	}
	h.only(rootFeed())
}
