package merge

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TestTabRoundIsPartOfTheRowKey covers the append-only rule: a second round is a
// SECOND tab, so two rounds must not upsert one another.
func TestTabRoundIsPartOfTheRowKey(t *testing.T) {
	// Arrange: one tab kind at two rounds.
	first := tabRef(theWorkspace, "lease-1", TabTests, 1)
	second := tabRef(theWorkspace, "lease-1", TabTests, 2)

	// Act.
	same := first.Row == second.Row

	// Assert.
	if same {
		t.Fatalf("rounds 1 and 2 share the row key %+v; the second round would replace the first", first.Row)
	}
}

// TestTabLabelCarriesTheRoundAsANumber covers the drawing contract: the client
// decorates rounds beyond the first, so the round travels as a number rather
// than baked into the text.
func TestTabLabelCarriesTheRoundAsANumber(t *testing.T) {
	// Arrange: a second-round tests tab.
	label := tabLabel(TabTests, 2)

	// Act.
	text, round := label.GetText(), label.GetRound()

	// Assert.
	if text != "tests" || round != 2 {
		t.Fatalf("the label is %q round %d, want the bare word and the round", text, round)
	}
}

// TestMergeSubFeedIsKeyedByTheLease covers the bubble's address: its sub-feed
// belongs to the merge lease that owns it.
func TestMergeSubFeedIsKeyedByTheLease(t *testing.T) {
	// Arrange: one lease.
	lease := ids.LeaseID("lease-9")

	// Act.
	feed := mergeFeed(lease)

	// Assert.
	if feed.Merge == nil || *feed.Merge != lease {
		t.Fatalf("the merge feed is %+v, want it keyed by the lease", feed)
	}
	if feed.Root || feed.Agent != nil {
		t.Fatalf("the merge feed is %+v, want exactly the merge arm set", feed)
	}
}

// TestHeadLivesOnTheRootFeed covers where the collapsed head is drawn: the
// bubble is an activity unit of the turn the user is watching.
func TestHeadLivesOnTheRootFeed(t *testing.T) {
	// Arrange: one merge's head address.
	ref := headRef(theWorkspace, "lease-1")

	// Act.
	feed := ref.Feed

	// Assert.
	if !feed.Root || feed.Merge != nil {
		t.Fatalf("the head sits on %+v, want the root feed", feed)
	}
}

// TestQueueSnapshotPlacesYouStructurally covers the "you are here" contract: it
// is a structure rather than something a client derives by comparing ids.
func TestQueueSnapshotPlacesYouStructurally(t *testing.T) {
	// Arrange: a three-deep queue seen from the middle entry.
	entries := []wsm.MergeQueueEntry{
		{Workspace: "ws-1", Position: 1},
		{Workspace: "ws-2", Position: 2},
		{Workspace: "ws-3", Position: 3},
	}
	names := map[ids.WorkspaceID]string{"ws-1": "one", "ws-2": "two", "ws-3": "three"}
	dirs := map[ids.WorkspaceID]string{}

	// Act.
	snap := queueSnapshot(entries, "ws-2", names, dirs, tabLabel(TabMerge, 1))

	// Assert.
	if len(snap.GetAhead()) != 1 || snap.GetAhead()[0].GetLabel().GetText() != "one" {
		t.Fatalf("ahead is %v, want the front alone", snap.GetAhead())
	}
	if snap.GetCurrent().GetLabel().GetText() != "two" {
		t.Fatalf("current is %v, want this workspace", snap.GetCurrent())
	}
	if len(snap.GetBehind()) != 1 || snap.GetBehind()[0].GetLabel().GetText() != "three" {
		t.Fatalf("behind is %v, want the one entry after it", snap.GetBehind())
	}
}

// TestQueueSnapshotFrontCarriesItsActiveTab covers what a waiting user learns:
// the front's progress, in the same message the front's own bubble draws.
func TestQueueSnapshotFrontCarriesItsActiveTab(t *testing.T) {
	// Arrange: a queue whose front is running its tests.
	entries := []wsm.MergeQueueEntry{{Workspace: "ws-1", Position: 1}, {Workspace: "ws-2", Position: 2}}

	// Act.
	snap := queueSnapshot(entries, "ws-2", map[ids.WorkspaceID]string{}, map[ids.WorkspaceID]string{}, tabLabel(TabTests, 2))

	// Assert.
	merging := snap.GetAhead()[0].GetMerging()
	if merging == nil {
		t.Fatalf("the front's status is %T, want merging", snap.GetAhead()[0].GetStatus())
	}
	if merging.GetActiveTab().GetText() != "tests" || merging.GetActiveTab().GetRound() != 2 {
		t.Fatalf("the front's active tab is %v, want the second tests round", merging.GetActiveTab())
	}
}

// TestQueueSnapshotMarksTheRestWaiting covers the other arm of an entry's
// standing.
func TestQueueSnapshotMarksTheRestWaiting(t *testing.T) {
	// Arrange: a two-deep queue seen from the front.
	entries := []wsm.MergeQueueEntry{{Workspace: "ws-1", Position: 1}, {Workspace: "ws-2", Position: 2}}

	// Act.
	snap := queueSnapshot(entries, "ws-1", map[ids.WorkspaceID]string{}, map[ids.WorkspaceID]string{}, tabLabel(TabMerge, 1))

	// Assert.
	if snap.GetBehind()[0].GetWaiting() == nil {
		t.Fatalf("the entry behind is %T, want waiting", snap.GetBehind()[0].GetStatus())
	}
}

// TestParkedBadgeCarriesTheComposedLine covers the one account a parked merge
// has: the daemon composes the sentence and both surfaces draw it verbatim.
func TestParkedBadgeCarriesTheComposedLine(t *testing.T) {
	// Arrange: a composed standing line.
	line := "parked for your input — 2 conflicts remain in daemon/server.go"

	// Act.
	badge := parkedBadge(line)

	// Assert.
	if badge.GetLine().GetText() != line {
		t.Fatalf("the badge reads %q, want the composed line verbatim", badge.GetLine().GetText())
	}
}

// TestQueueTabSettlesAtTheFront covers the queue tab's whole job: the wait, which
// completes when this workspace reaches the front.
func TestQueueTabSettlesAtTheFront(t *testing.T) {
	tests := []struct {
		name    string
		front   bool
		settled bool
	}{
		{name: "waiting behind another merge", front: false, settled: false},
		{name: "at the front", front: true, settled: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a queue tab in one of the two places.
			snap := &frontendv1.FeedMergeQueue{}

			// Act.
			tab := queueTab(snap, tc.front, 1000)

			// Assert.
			_, settled := tab.GetQueue().GetState().(*frontendv1.FeedMergeTabQueue_Settled)
			if settled != tc.settled {
				t.Fatalf("the queue tab settled = %v, want %v", settled, tc.settled)
			}
		})
	}
}

// TestDequeueOfferNamesTheWorkspace covers the composed sentence: the card draws
// it and never assembles one from the offer's kind.
func TestDequeueOfferNamesTheWorkspace(t *testing.T) {
	// Arrange: one workspace's name.
	offer := dequeueOffer("fix-flaky")

	// Act.
	headline := offer.GetMergeDequeue().GetHeadline().GetText()

	// Assert.
	if headline == "" {
		t.Fatal("the offer carries no composed sentence")
	}
	if want := "fix-flaky"; !strings.Contains(headline, want) {
		t.Fatalf("the offer reads %q, want it to name %q", headline, want)
	}
}

// TestBranchLabelReadsAsTheMerge covers the head's branch line.
func TestBranchLabelReadsAsTheMerge(t *testing.T) {
	// Arrange: a source and a target.
	got := branchLabel("DWC/fix-flaky", "master")

	// Act, Assert.
	if got != "DWC/fix-flaky → master" {
		t.Fatalf("the label is %q, want the source, an arrow and the target", got)
	}
}
