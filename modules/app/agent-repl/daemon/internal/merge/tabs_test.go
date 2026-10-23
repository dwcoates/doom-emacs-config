package merge

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// TestTabRoundIsPartOfTheRowKey covers the append-only rule: a second round is a
// SECOND tab, so two rounds must not upsert one another.
func TestTabRoundIsPartOfTheRowKey(t *testing.T) {
	// Arrange: one tab kind at two rounds.
	first := tabRef(theWorkspace, "lease-1", roundTabKey(TabCommitting, 1))
	second := tabRef(theWorkspace, "lease-1", roundTabKey(TabCommitting, 2))

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

// TestHeadShipsNoFold covers the producer's half of the bubble's fold: the
// orchestrator composes every head with NO fold, in every state, because the
// feed resolver alone decides it (resolve/feed/mergefold.go).
func TestHeadShipsNoFold(t *testing.T) {
	tests := []struct {
		name   string
		result any
	}{
		{"merging", headMerging},
		{"waiting", headWaiting},
		{"success", &frontendv1.FeedMergeSuccess{}},
		{"failed", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{}}}},
		{"abandoned", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange and act.
			row := headRow(theWorkspace, "lease-1", "branch", 1, tt.result)

			// Assert.
			if fold := row.GetActivity().GetMerge().GetHead().GetFold(); fold != nil {
				t.Fatalf("the head ships fold %+v, want none", fold)
			}
		})
	}
}

// TestALiveHeadDrawsItsStage covers the live head's badge arm
// (frontend.v1.FeedMergeUpdate.stage): each stage draws its own arm.
func TestALiveHeadDrawsItsStage(t *testing.T) {
	tests := []struct {
		name        string
		stage       liveHead
		wantWaiting bool
	}{
		{name: "merging", stage: headMerging, wantWaiting: false},
		{name: "waiting", stage: headWaiting, wantWaiting: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange and act.
			update := headRow(theWorkspace, "lease-1", "branch", 1, tt.stage).GetActivity().GetMerge().GetUpdate()

			// Assert.
			if update == nil {
				t.Fatalf("the head draws no live arm")
			}
			if (update.GetWaiting() != nil) != tt.wantWaiting || (update.GetMerging() != nil) == tt.wantWaiting {
				t.Fatalf("the head draws stage %T, want waiting %v", update.GetStage(), tt.wantWaiting)
			}
		})
	}
}

// TestHeadRowRefusesAResultThatIsNoHeadArm covers the invariant: a result
// that is neither a live stage nor an outcome panics rather than drawing a
// guessed arm, including the untyped nil and a stage outside the enum.
func TestHeadRowRefusesAResultThatIsNoHeadArm(t *testing.T) {
	tests := []struct {
		name   string
		result any
	}{
		{name: "nil", result: nil},
		{name: "a stage outside the enum", result: liveHead{stage: 99}},
		{name: "a foreign type", result: "merging"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			defer func() {
				// Assert.
				if recover() == nil {
					t.Fatalf("headRow drew a head for result %#v, want a panic", tt.result)
				}
			}()

			// Act.
			headRow(theWorkspace, "lease-1", "branch", 1, tt.result)
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
	snap := queueSnapshot(entries, "ws-2", snapshotView(false, standings))

	// Assert.
	if got := snap.GetAhead()[0].GetMerging(); got != front {
		t.Fatalf("the front's standing is %v, want the resolved merging standing", got)
	}
	if got := snap.GetCurrent().GetWaiting().GetStageEnteredAtMs(); got != 3000 {
		t.Fatalf("this workspace's wait began at %d, want 3000", got)
	}
}

// snapshotView is a queue view of the two entries ws-1 and ws-2, both landing
// in the main worktree at /src/doom, on a queue PAUSED or not.
func snapshotView(paused bool, standings []*frontendv1.FeedMergeQueueEntry) queueView {
	return queueView{
		paused:    paused,
		names:     map[ids.WorkspaceID]string{},
		dirs:      map[ids.WorkspaceID]string{},
		targets:   map[ids.WorkspaceID]string{"ws-1": "/src/doom", "ws-2": "/src/doom"},
		bubbles:   map[ids.WorkspaceID]*frontendv1.FeedMergeQueueBubble{},
		standings: standings,
	}
}

// TestQueueSnapshotCarriesThePauseAndEachTarget covers the snapshot's queue
// facts: the pause rides the snapshot exactly while the queue is paused, and
// every entry names the worktree it lands in by its directory name and path.
func TestQueueSnapshotCarriesThePauseAndEachTarget(t *testing.T) {
	tests := []struct {
		name   string
		paused bool
	}{
		{name: "a queue that runs", paused: false},
		{name: "a paused queue", paused: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			entries := []wsm.MergeQueueEntry{{Workspace: "ws-1", Position: 1}, {Workspace: "ws-2", Position: 2}}
			standings := []*frontendv1.FeedMergeQueueEntry{
				{Status: &frontendv1.FeedMergeQueueEntry_Merging{Merging: &frontendv1.FeedMergeQueueMerging{ActiveTab: tabLabel(TabQueue, 1)}}},
				{Status: &frontendv1.FeedMergeQueueEntry_Waiting{Waiting: &frontendv1.FeedMergeQueueWaiting{StageEnteredAtMs: 3000}}},
			}

			// Act.
			snap := queueSnapshot(entries, "ws-2", snapshotView(tt.paused, standings))

			// Assert.
			if got := snap.GetPaused() != nil; got != tt.paused {
				t.Fatalf("the snapshot says paused %v, want %v", got, tt.paused)
			}
			for _, entry := range []*frontendv1.FeedMergeQueueEntry{snap.GetAhead()[0], snap.GetCurrent()} {
				if got := entry.GetTarget(); got.GetText() != "doom" || got.GetDir() != "/src/doom" {
					t.Fatalf("an entry's target is %+v, want doom at /src/doom", got)
				}
			}
		})
	}
}

// TestQueueTargetRefusesAnUnresolvedWorktree covers the invariant: an entry
// whose landing worktree was never resolved panics rather than drawing an
// empty target.
func TestQueueTargetRefusesAnUnresolvedWorktree(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatalf("queueTarget drew a target for no worktree, want a panic")
		}
	}()

	// Act.
	queueTarget("")
}

// TestALiveHeadDrawsItsQueuesPause covers frontend.v1.FeedMergeUpdate.paused:
// a live head carries the pause exactly while its queue is paused, beside its
// stage rather than in place of it.
func TestALiveHeadDrawsItsQueuesPause(t *testing.T) {
	tests := []struct {
		name        string
		head        liveHead
		wantPaused  bool
		wantWaiting bool
	}{
		{name: "waiting on a queue that runs", head: headWaiting.onPaused(false), wantPaused: false, wantWaiting: true},
		{name: "waiting on a paused queue", head: headWaiting.onPaused(true), wantPaused: true, wantWaiting: true},
		{name: "merging on a paused queue", head: headMerging.onPaused(true), wantPaused: true, wantWaiting: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			update := headRow(theWorkspace, "lease-1", "branch", 1, tt.head).GetActivity().GetMerge().GetUpdate()

			// Assert.
			if got := update.GetPaused() != nil; got != tt.wantPaused {
				t.Fatalf("the head says paused %v, want %v", got, tt.wantPaused)
			}
			if got := update.GetWaiting() != nil; got != tt.wantWaiting {
				t.Fatalf("the head says waiting %v, want %v", got, tt.wantWaiting)
			}
		})
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
		TabQueue:     func(s tabState) *frontendv1.FeedMergeTab { return queueTab(&frontendv1.FeedMergeQueue{}, s) },
		TabPreDeploy: func(s tabState) *frontendv1.FeedMergeTab { return hookPromptTab(s) },
		TabRebasing: func(s tabState) *frontendv1.FeedMergeTab {
			v := rebaseView{Span: spanOf(s)}
			return v.rebasingTab(false)
		},
		rowRebaseCommit: func(s tabState) *frontendv1.FeedMergeTab {
			v := rebaseView{Replay: 1, Commits: []commitView{{SHA: "a1b2c3d4e5", Subject: "x", Span: spanOf(s)}}}
			return v.commitTab(0, false)
		},
		TabTests: func(s tabState) *frontendv1.FeedMergeTab {
			v := testsView{Span: spanOf(s)}
			return v.testsTab(false)
		},
		rowTestsRun:       func(s tabState) *frontendv1.FeedMergeTab { return runTab(1, s, nil, nil) },
		rowTestsRunFixing: func(s tabState) *frontendv1.FeedMergeTab { return fixingTab(s, 1) },
		TabCommitting:     func(s tabState) *frontendv1.FeedMergeTab { return committingTab(s, "x") },
		TabDeploy:         func(s tabState) *frontendv1.FeedMergeTab { return hookScriptTab(s, scriptView{path: "bin/deploy.sh"}) },
		TabPostDeploy:     func(s tabState) *frontendv1.FeedMergeTab { return hookPromptTab(s) },
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

// TestQueueOfferCarriesItsSentence covers the composed sentence: the overlay
// draws it verbatim and never assembles one of its own.
func TestQueueOfferCarriesItsSentence(t *testing.T) {
	// Arrange, Act.
	headline := queueOffer().GetHeadline().GetText()

	// Assert.
	if headline == "" {
		t.Fatal("the question carries no composed sentence")
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
		{TabRebasing, "rebasing"}, {TabCommitting, "committing"},
		{TabPreDeploy, "pre-deploy"}, {TabDeploy, "deploy"}, {TabPostDeploy, "post-deploy"},
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

// spanOf is the view span a badge state was drawn from.
func spanOf(s tabState) span {
	if s.settled {
		return span{StartedMS: s.startedMS, EndedMS: s.endedMS, Failure: s.failure}
	}
	return span{StartedMS: s.startedMS}
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

// TestPublishHeadPinsALiveHeadAndSettlesAResolvedOne covers the head's place:
// pinned to the feed's bottom while the merge is in flight, and settled where
// it stands by the first publication carrying how the merge resolved.
func TestPublishHeadPinsALiveHeadAndSettlesAResolvedOne(t *testing.T) {
	tests := []struct {
		name       string
		result     any
		wantPinned bool
	}{
		{name: "in flight", result: headMerging, wantPinned: true},
		{name: "landed", result: &frontendv1.FeedMergeSuccess{Commit: "abc123"}, wantPinned: false},
		{name: "failed", result: &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: "tests failed"}}}, wantPinned: false},
		{name: "abandoned", result: &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{Summary: "closed"}}}, wantPinned: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, tt.result, headRun)

			// Assert.
			heads := h.feed.headPublications()
			if len(heads) != 1 || heads[0].Pinned != tt.wantPinned || !heads[0].Feed.Root {
				t.Fatalf("heads = %+v, want one root head published pinned=%v", heads, tt.wantPinned)
			}
		})
	}
}

// failedHead is a head result: the merge failed.
func failedHead() *frontendv1.FeedMergeError {
	return &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: "conflicts"}}}
}

// TestAQueueRedrawReadBeforeTheMergeResolvedNeverRepinsItsHead reproduces the
// race behind daemon.feed.pin_refused: the queue's redraw reads the stored
// queue while the merge is still in it, the run resolves and publishes its
// failed head, and the redraw then draws the merge's head live from what it
// read. The resolved head must stand.
func TestAQueueRedrawReadBeforeTheMergeResolvedNeverRepinsItsHead(t *testing.T) {
	// Arrange: one merge in line, and the run resolving between the redraw's
	// read of the queue and its drawing of the heads.
	h := newHarness(t)
	h.emacsRepo()
	enqueue(t, h)
	ledger, ok := h.o.leaseOf(theWorkspace)
	if !ok {
		t.Fatal("the queued merge has no ledger identity")
	}
	resolved := false
	h.db.onQueueRead = func() {
		if resolved {
			return
		}
		resolved = true
		h.o.publishHead(theWorkspace, ledger, "feature → master", 1, failedHead(), headRun)
	}
	before := len(h.feed.headPublications())

	// Act.
	if err := h.o.republishQueue(context.Background(), h.repoKey()); err != nil {
		t.Fatalf("republishQueue: %v", err)
	}

	// Assert: the resolved head is the last head drawn, and it is unpinned.
	heads := h.feed.headPublications()[before:]
	if len(heads) != 1 || heads[0].Pinned || heads[0].Row.GetActivity().GetMerge().GetError() == nil {
		t.Fatalf("heads after the redraw = %+v, want only the resolved head, unpinned", heads)
	}
}

// TestAStaleLiveHeadIsRecordedWithTheCauseThatDrewIt covers the record of a
// live publication the head book does not draw: it names the caller, so a
// reader of the log knows which redraw was stale.
func TestAStaleLiveHeadIsRecordedWithTheCauseThatDrewIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, failedHead(), headRun)

	// Act.
	h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, headMerging, headQueue)

	// Assert.
	rec, found := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{
		Level: "debug", Operation: "daemon.merge.head",
		Message: "a live head publication came after its ledger resolved; the resolved head stands and the live one is not drawn"})
	if !found || rec.Context["cause"] != "queue" || rec.Context["ledger"] != "lease-1" || rec.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("record = %+v (found %v), want the stale publication recorded with its cause, ledger and workspace", rec, found)
	}
}

// TestEveryDrawnHeadIsRecordedWithTheCauseThatDrewIt covers the record of a
// drawn publication: who drew it, for which ledger, and whether it pinned.
func TestEveryDrawnHeadIsRecordedWithTheCauseThatDrewIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, headMerging, headRecover)

	// Assert.
	rec, found := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{
		Level: "debug", Operation: "daemon.merge.head", Message: "published the merge head"})
	if !found || rec.Context["cause"] != "recover" || rec.Context["ledger"] != "lease-1" || rec.Context["pinned"] != true || rec.Context["resolved"] != false {
		t.Fatalf("record = %+v (found %v), want the publication recorded with its cause, ledger and pin", rec, found)
	}
}

// TestTheHeadBookOrdersEveryPublication covers the head book's decision: a
// live publication of a resolved ledger is not drawn, and everything else is.
func TestTheHeadBookOrdersEveryPublication(t *testing.T) {
	tests := []struct {
		name       string
		first      any
		lease      ids.LeaseID
		then       any
		wantDrawn  bool
		wantPinned bool
	}{
		{name: "live after its ledger resolved is not drawn", first: failedHead(), lease: "lease-1", then: headMerging, wantDrawn: false},
		{name: "live after another ledger resolved is drawn pinned", first: failedHead(), lease: "lease-2", then: headMerging, wantDrawn: true, wantPinned: true},
		{name: "an outcome after an outcome is drawn", first: failedHead(), lease: "lease-1", then: &frontendv1.FeedMergeSuccess{Commit: "abc123"}, wantDrawn: true, wantPinned: false},
		{name: "live after live is drawn pinned", first: headMerging, lease: "lease-1", then: headMerging, wantDrawn: true, wantPinned: true},
		{name: "an outcome after live is drawn settled", first: headMerging, lease: "lease-1", then: failedHead(), wantDrawn: true, wantPinned: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, tt.first, headRun)

			// Act.
			h.o.publishHead(theWorkspace, tt.lease, "feature → master", 1, tt.then, headQueue)

			// Assert.
			heads := h.feed.headPublications()
			if drawn := len(heads) == 2; drawn != tt.wantDrawn {
				t.Fatalf("heads = %+v, want the second drawn=%v", heads, tt.wantDrawn)
			}
			if tt.wantDrawn && heads[1].Pinned != tt.wantPinned {
				t.Fatalf("second head pinned=%v, want %v", heads[1].Pinned, tt.wantPinned)
			}
		})
	}
}

// TestRestateHeadDrawsTheHeadAsItLastStood covers the restate: a resolved
// head is restated resolved, never live, and a head never drawn is not drawn.
func TestRestateHeadDrawsTheHeadAsItLastStood(t *testing.T) {
	tests := []struct {
		name       string
		drawn      bool
		result     any
		wantHeads  int
		wantPinned bool
	}{
		{name: "never drawn", drawn: false, wantHeads: 0},
		{name: "live", drawn: true, result: headMerging, wantHeads: 2, wantPinned: true},
		{name: "resolved", drawn: true, result: failedHead(), wantHeads: 2, wantPinned: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tt.drawn {
				h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, tt.result, headRun)
			}

			// Act.
			h.o.restateHead(theWorkspace, headReveal)

			// Assert.
			heads := h.feed.headPublications()
			if len(heads) != tt.wantHeads {
				t.Fatalf("heads = %+v, want %d", heads, tt.wantHeads)
			}
			if tt.wantHeads > 0 && heads[len(heads)-1].Pinned != tt.wantPinned {
				t.Fatalf("restated head pinned=%v, want %v", heads[len(heads)-1].Pinned, tt.wantPinned)
			}
		})
	}
}

// TestRestateLiveHeadRestatesOnlyALiveHead covers the live restate: MARK and
// the restate run only while the head is drawn and has no result.
func TestRestateLiveHeadRestatesOnlyALiveHead(t *testing.T) {
	tests := []struct {
		name      string
		drawn     bool
		result    any
		wantDrawn bool
		wantLive  bool
	}{
		{name: "never drawn", drawn: false, wantDrawn: false, wantLive: false},
		{name: "resolved", drawn: true, result: failedHead(), wantDrawn: true, wantLive: false},
		{name: "live", drawn: true, result: headMerging, wantDrawn: true, wantLive: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tt.drawn {
				h.o.publishHead(theWorkspace, "lease-1", "feature → master", 1, tt.result, headRun)
			}
			before := len(h.feed.headPublications())
			marked := false

			// Act.
			drawn, live := h.o.restateLiveHead(theWorkspace, headOffer, func() { marked = true })

			// Assert.
			restated := len(h.feed.headPublications()) - before
			if drawn != tt.wantDrawn || live != tt.wantLive || marked != tt.wantLive || (restated == 1) != tt.wantLive {
				t.Fatalf("drawn=%v live=%v marked=%v restated=%d, want drawn=%v live=%v", drawn, live, marked, restated, tt.wantDrawn, tt.wantLive)
			}
		})
	}
}
