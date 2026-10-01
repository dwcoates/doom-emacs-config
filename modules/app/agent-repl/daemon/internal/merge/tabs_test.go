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

// TestHeadOpensFolded covers the bubble's first draw: the footer already
// carries the merge's live state, so every result arm ships the bubble folded.
func TestHeadOpensFolded(t *testing.T) {
	tests := []struct {
		name   string
		result any
	}{
		{"update", nil},
		{"success", &frontendv1.FeedMergeSuccess{}},
		{"error", &frontendv1.FeedMergeError{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange and act.
			row := headRow(theWorkspace, "lease-1", "branch", 1, tt.result)

			// Assert.
			fold := row.GetActivity().GetMerge().GetHead().GetFold()
			if fold == nil || !fold.GetFolded() {
				t.Fatalf("the head ships fold %+v, want folded", fold)
			}
		})
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

func TestTheNewTabsDrawTheirWords(t *testing.T) {
	tests := []struct{ kind, want string }{
		{TabRebasing, "rebasing"}, {TabCommitting, "committing"}, {TabUpdatingMain, "updating main"},
	}
	for _, tt := range tests {
		t.Run(tt.kind, func(t *testing.T) {
			// Act / Assert.
			if got := tabWord(tt.kind); got != tt.want {
				t.Fatalf("tabWord(%s) = %q, want %q", tt.kind, got, tt.want)
			}
		})
	}
}

func TestTheRebasingTabCarriesItsProgressAndLines(t *testing.T) {
	// Act.
	tab := rebasingTab(live(), 2, 5, []string{"replayed 2/5 · x"}, 0, "")

	// Assert.
	r := tab.GetRebasing()
	if r.GetProgress().GetReplayed() != 2 || r.GetProgress().GetTotal() != 5 || len(r.GetLines()) != 1 || r.GetLive() == nil {
		t.Fatalf("rebasing tab = %+v", r)
	}
}

func TestTheUpdatingMainTabMovesFromFetchingToFastForwarding(t *testing.T) {
	// Act.
	fetching := updatingMainTab(live(), "", 0, "")
	forwarding := updatingMainTab(nil, "abc", 9, "")

	// Assert.
	if fetching.GetUpdatingMain().GetStep().GetFetching() == nil {
		t.Fatalf("first tab = %+v, want fetching", fetching)
	}
	if forwarding.GetUpdatingMain().GetStep().GetFastForwarding().GetCommit() != "abc" || forwarding.GetUpdatingMain().GetSettled() == nil {
		t.Fatalf("second tab = %+v, want settled fast-forwarding to abc", forwarding)
	}
}

func TestTheCommittingTabCarriesTheMergeCommitsSubject(t *testing.T) {
	// Act.
	tab := committingTab(nil, "merge(master): x", 5, "it failed")

	// Assert.
	c := tab.GetCommitting()
	if c.GetSubject().GetText() != "merge(master): x" || c.GetSettled().GetFailed().GetSummary() != "it failed" {
		t.Fatalf("committing tab = %+v", c)
	}
}
