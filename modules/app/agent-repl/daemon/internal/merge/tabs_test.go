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

// TestHeadFoldFollowsTheMergesOutcome covers the bubble's fold: it stays
// folded while the merge runs, when it lands and when it is abandoned, and
// ships OPEN only once the merge has failed, so the failure is in front of
// the reader.
func TestHeadFoldFollowsTheMergesOutcome(t *testing.T) {
	tests := []struct {
		name   string
		result any
		folded bool
	}{
		{"update", nil, true},
		{"success", &frontendv1.FeedMergeSuccess{}, true},
		{"failed", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{}}}, false},
		{"abandoned", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{}}}, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange and act.
			row := headRow(theWorkspace, "lease-1", "branch", 1, tt.result)

			// Assert.
			fold := row.GetActivity().GetMerge().GetHead().GetFold()
			if fold == nil || fold.GetFolded() != tt.folded {
				t.Fatalf("the head ships fold %+v, want folded=%v", fold, tt.folded)
			}
		})
	}
}

// TestQueueSnapshotPlacesEachEntrysStanding covers the snapshot's assembly:
// each entry carries the standing queueStandings resolved for its place in
// line, so the front's progress reaches a waiting user unchanged.
func TestQueueSnapshotPlacesEachEntrysStanding(t *testing.T) {
	// Arrange: a queue whose front is in its second tests round.
	entries := []wsm.MergeQueueEntry{{Workspace: "ws-1", Position: 1}, {Workspace: "ws-2", Position: 2}}
	front := &frontendv1.FeedMergeQueueMerging{ActiveTab: tabLabel(TabTests, 2), StageEnteredAtMs: 7000}
	standings := []*frontendv1.FeedMergeQueueEntry{
		{Status: &frontendv1.FeedMergeQueueEntry_Merging{Merging: front}},
		{Status: &frontendv1.FeedMergeQueueEntry_Waiting{Waiting: &frontendv1.FeedMergeQueueWaiting{StageEnteredAtMs: 3000}}},
	}

	// Act.
	snap := queueSnapshot(entries, "ws-2", map[ids.WorkspaceID]string{}, map[ids.WorkspaceID]string{}, standings)

	// Assert.
	if got := snap.GetAhead()[0].GetMerging(); got != front {
		t.Fatalf("the front's standing is %v, want the resolved merging standing", got)
	}
	if got := snap.GetCurrent().GetWaiting().GetStageEnteredAtMs(); got != 3000 {
		t.Fatalf("this workspace's wait began at %d, want 3000", got)
	}
}

// TestQueueTabSettlesAtTheFront covers the queue tab's whole job: the wait, which
// completes when this workspace reaches the front.
func TestQueueTabSettlesAtTheFront(t *testing.T) {
	tests := []struct {
		name    string
		state   tabState
		settled bool
	}{
		{name: "waiting behind another merge", state: tabState{startedMS: 1000}, settled: false},
		{name: "at the front", state: tabState{startedMS: 1000, settled: true, endedMS: 2000}, settled: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a queue tab in one of the two places.
			snap := &frontendv1.FeedMergeQueue{}

			// Act.
			tab := queueTab(snap, tc.state)

			// Assert.
			_, settled := tab.GetQueue().GetState().(*frontendv1.FeedMergeTabQueue_Settled)
			if settled != tc.settled {
				t.Fatalf("the queue tab settled = %v, want %v", settled, tc.settled)
			}
		})
	}
}

// TestTabStateBadgeCarriesTheStart covers the one badge every tab kind selects
// from: live or settled, it ships when the round began, and a settled one its
// end and outcome.
func TestTabStateBadgeCarriesTheStart(t *testing.T) {
	tests := []struct {
		name        string
		state       tabState
		wantLive    bool
		wantEnded   int64
		wantFailure string
	}{
		{name: "live", state: tabState{startedMS: 1000}, wantLive: true},
		{name: "settled succeeded", state: tabState{startedMS: 1000, settled: true, endedMS: 4000}, wantEnded: 4000},
		{name: "settled failed", state: tabState{startedMS: 1000, settled: true, endedMS: 4000, failure: "it broke"}, wantEnded: 4000, wantFailure: "it broke"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			live, settled := tc.state.badge()

			// Assert.
			if (live != nil) != tc.wantLive || (settled != nil) == tc.wantLive {
				t.Fatalf("badge = live %v settled %v, want exactly the live arm = %v", live, settled, tc.wantLive)
			}
			if live != nil && live.GetStartedAtMs() != 1000 {
				t.Fatalf("the live badge began at %d, want 1000", live.GetStartedAtMs())
			}
			if settled == nil {
				return
			}
			if settled.GetStartedAtMs() != 1000 || settled.GetEndedAtMs() != tc.wantEnded {
				t.Fatalf("the settled badge spans %d..%d, want 1000..%d", settled.GetStartedAtMs(), settled.GetEndedAtMs(), tc.wantEnded)
			}
			if got := settled.GetFailed().GetSummary(); got != tc.wantFailure {
				t.Fatalf("the settled badge's failure is %q, want %q", got, tc.wantFailure)
			}
			if tc.wantFailure == "" && settled.GetSucceeded() == nil {
				t.Fatalf("the settled badge's outcome is %T, want succeeded", settled.GetOutcome())
			}
		})
	}
}

// tabBadgeOf reads any tab kind's state arms through the getters every kind
// shares.
func tabBadgeOf(t *testing.T, tab *frontendv1.FeedMergeTab) (*frontendv1.FeedMergeTabLive, *frontendv1.FeedMergeTabSettled) {
	t.Helper()
	m := tab.ProtoReflect()
	field := m.WhichOneof(m.Descriptor().Oneofs().ByName("kind"))
	if field == nil {
		t.Fatal("the tab carries no kind")
	}
	inner, ok := m.Get(field).Message().Interface().(interface {
		GetLive() *frontendv1.FeedMergeTabLive
		GetSettled() *frontendv1.FeedMergeTabSettled
	})
	if !ok {
		t.Fatalf("the %s kind has no live/settled state", field.Name())
	}
	return inner.GetLive(), inner.GetSettled()
}

// TestEveryTabBuilderShipsTheBadgeItWasGiven covers the call sites sharing the
// one badge: every kind's builder places the tabState's start, live or
// settled, so no kind can drop the instant its round began.
func TestEveryTabBuilderShipsTheBadgeItWasGiven(t *testing.T) {
	builders := map[string]func(tabState) *frontendv1.FeedMergeTab{
		TabQueue:        func(s tabState) *frontendv1.FeedMergeTab { return queueTab(&frontendv1.FeedMergeQueue{}, s) },
		TabPrePrompt:    func(s tabState) *frontendv1.FeedMergeTab { return promptTab(TabPrePrompt, s) },
		TabRebasing:     func(s tabState) *frontendv1.FeedMergeTab { return rebasingTab(s, 0, 0, nil) },
		TabConflicts:    conflictsTab,
		TabTests:        func(s tabState) *frontendv1.FeedMergeTab { return testsTab(s, nil, nil) },
		TabFixes:        func(s tabState) *frontendv1.FeedMergeTab { return fixesTab(s, 1) },
		TabCommitting:   func(s tabState) *frontendv1.FeedMergeTab { return committingTab(s, "x") },
		TabUpdatingMain: func(s tabState) *frontendv1.FeedMergeTab { return updatingMainTab(s, "") },
		TabPostPrompt:   func(s tabState) *frontendv1.FeedMergeTab { return promptTab(TabPostPrompt, s) },
	}
	for kind, build := range builders {
		t.Run(kind, func(t *testing.T) {
			// Act.
			live, _ := tabBadgeOf(t, build(tabState{startedMS: 1000}))
			_, settled := tabBadgeOf(t, build(tabState{startedMS: 1000, settled: true, endedMS: 4000}))

			// Assert.
			if live.GetStartedAtMs() != 1000 {
				t.Fatalf("the live %s tab began at %d, want 1000", kind, live.GetStartedAtMs())
			}
			if settled.GetStartedAtMs() != 1000 || settled.GetEndedAtMs() != 4000 {
				t.Fatalf("the settled %s tab spans %d..%d, want 1000..4000", kind, settled.GetStartedAtMs(), settled.GetEndedAtMs())
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
	got := branchLabel("ABC/fix-flaky", "master")

	// Act, Assert.
	if got != "ABC/fix-flaky → master" {
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
	tab := rebasingTab(tabState{startedMS: 1}, 2, 5, []string{"replayed 2/5 · x"})

	// Assert.
	r := tab.GetRebasing()
	if r.GetProgress().GetReplayed() != 2 || r.GetProgress().GetTotal() != 5 || len(r.GetLines()) != 1 || r.GetLive() == nil {
		t.Fatalf("rebasing tab = %+v", r)
	}
}

func TestTheUpdatingMainTabMovesFromFetchingToFastForwarding(t *testing.T) {
	// Act.
	fetching := updatingMainTab(tabState{startedMS: 1}, "")
	forwarding := updatingMainTab(tabState{startedMS: 1, settled: true, endedMS: 9}, "abc")

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
	tab := committingTab(tabState{startedMS: 1, settled: true, endedMS: 5, failure: "it failed"}, "merge(master): x")

	// Assert.
	c := tab.GetCommitting()
	if c.GetSubject().GetText() != "merge(master): x" || c.GetSettled().GetFailed().GetSummary() != "it failed" {
		t.Fatalf("committing tab = %+v", c)
	}
}
