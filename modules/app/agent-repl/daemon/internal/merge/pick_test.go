package merge

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/wsm"
)

// pickHarness is a harness whose pick brief is loadable and whose pick leaves
// a clean worktree one commit ahead of the target, unless a test says
// otherwise.
func pickHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.briefs[BriefPick] = []string{"pick_instruction", "fork_head", "target_branch", "worktree_dir"}
	h.git.mu.Lock()
	h.git.clean = true
	h.git.ahead = 1
	h.git.mu.Unlock()
	return h
}

// requestPick asks for the harness workspace's async pick of "the reconnect
// fix", as the user, which forks the merge workspace and begins the pick.
func requestPick(t *testing.T, h *harness) {
	t.Helper()
	err := h.o.Enqueue(context.Background(), Request{
		Workspace: theWorkspace, Source: asyncSource, By: RequestedByUser,
		Detached: &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix", Pick: "the reconnect fix"},
	})
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	// THE PICK BEGINS AT THE HEAD: the hand-off put the merge workspace in
	// line, and its admission starts the pick.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}
}

// runThePick runs the merge workspace's begun pick to its judgement.
func runThePick(t *testing.T, h *harness, ctx context.Context, resumed bool) {
	t.Helper()
	h.o.mu.Lock()
	p, ok := h.o.picks[theMergeWorkspace]
	h.o.mu.Unlock()
	if !ok {
		t.Fatal("no pick stands for the merge workspace")
	}
	h.o.runPick(ctx, p, resumed)
}

// pickTabs answers every pick tab drawn in the requester's feed, in order.
func pickTabs(h *harness) []*frontendv1.FeedMergeTabResolving {
	var out []*frontendv1.FeedMergeTabResolving
	for _, tab := range h.feed.tabsOn(theWorkspace) {
		if pick := tab.GetResolving(); pick != nil {
			out = append(out, pick)
		}
	}
	return out
}

func TestAnAsyncInstructionForksOntoTheRequestsBranch(t *testing.T) {
	// Arrange
	h := pickHarness(t)

	// Act
	requestPick(t, h)

	// Assert
	if forks := h.detached.forks; len(forks) != 1 || forks[0] != string(theWorkspace)+":reconnect-fix:"+asyncBranch {
		t.Fatalf("forks = %v, want one named reconnect-fix onto the request's branch", forks)
	}
}

func TestTheResolvingBriefNamesTheCommitTheMergeWorkspaceWasCutAt(t *testing.T) {
	// Arrange
	h := pickHarness(t)

	// Act
	requestPick(t, h)
	values := h.o.pickBriefValues(context.Background(), h.o.picks[theMergeWorkspace])

	// Assert
	if values["fork_head"] != "f0rkhead" || values["pick_instruction"] != "the reconnect fix" {
		t.Fatalf("brief values = %v, want the fork head and the instruction", values)
	}
}

func TestAPickDrawsAPlacedHeadAndALivePickTabInTheRequestersFeed(t *testing.T) {
	// Arrange
	h := pickHarness(t)

	// Act
	requestPick(t, h)

	// Assert
	// The bubble is drawn as the merge waits in line, then again as its pick
	// begins at the head: one bubble, its last head live.
	heads := h.feed.headsOn(theWorkspace)
	if len(heads) == 0 || heads[len(heads)-1].GetUpdate() == nil {
		t.Fatalf("heads = %+v, want the bubble's last head live", heads)
	}
	for _, row := range h.feed.rows {
		if row.Pinned {
			t.Fatalf("row %v was pinned, want an async bubble placed", row.Row.GetId())
		}
	}
	tabs := pickTabs(h)
	if len(tabs) != 1 || tabs[0].GetLive() == nil {
		t.Fatalf("pick tabs = %+v, want one live", tabs)
	}
}

func TestAPickRecordsItsTurnAndLedgerBeforeItIsSubmitted(t *testing.T) {
	// Arrange
	h := pickHarness(t)

	// Act
	requestPick(t, h)

	// Assert: nothing is submitted until the pick runs, and the row is ready.
	entry, ok := h.entryOf(theMergeWorkspace)
	if !ok || entry.Source.PickStartedAt == nil || entry.Source.PickTurn == "" || entry.Ledger == "" {
		t.Fatalf("entry = %+v, %v; want the pick's start, turn and ledger recorded", entry, ok)
	}
	if len(h.queue.submissions) != 0 {
		t.Fatalf("submissions = %d before the pick ran, want none", len(h.queue.submissions))
	}
}

func TestAPickStandsTheMirrorIntoItsTab(t *testing.T) {
	// Arrange
	h := pickHarness(t)

	// Act
	requestPick(t, h)

	// Assert
	calls := h.feed.mirrorsStood()
	if len(calls) != 1 || calls[0].ws != theMergeWorkspace || calls[0].mirror == nil {
		t.Fatalf("mirror calls = %+v, want one stood on the merge workspace", calls)
	}
	m := calls[0].mirror
	if m.Workspace != theWorkspace || m.Parent.WS != theWorkspace || m.Parent.Row != (feedid.RowKey{Kind: feedid.KindMergeTab, ID: TabPick, Sub: "1"}) {
		t.Fatalf("mirror = %+v, want the requester's pick tab", m)
	}
}

func TestAPickSubmitsItsBriefToTheMergeWorkspace(t *testing.T) {
	// Arrange
	h := pickHarness(t)
	requestPick(t, h)
	// The pick's turn is read before it runs: a pick that succeeds at the head
	// clears itself off the row.
	entry, _ := h.entryOf(theMergeWorkspace)

	// Act
	runThePick(t, h, context.Background(), false)

	// Assert
	if len(h.queue.submissions) != 1 {
		t.Fatalf("submissions = %d, want the pick's brief", len(h.queue.submissions))
	}
	sub := h.queue.submissions[0]
	text := sub.Said.GetContent().GetBlocks()[0].GetText().GetText()
	if sub.WS != theMergeWorkspace || sub.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK || sub.Turn != entry.Source.PickTurn || !strings.Contains(text, "the reconnect fix") {
		t.Fatalf("submission = %+v (%q), want the pick brief to the merge workspace as its recorded turn", sub, text)
	}
}

func TestAPickThatLeavesItsChangesCommittedJoinsTheQueue(t *testing.T) {
	// Arrange
	h := pickHarness(t)
	requestPick(t, h)

	// Act
	runThePick(t, h, context.Background(), false)

	// Assert
	tabs := pickTabs(h)
	if last := tabs[len(tabs)-1]; last.GetSettled().GetSucceeded() == nil {
		t.Fatalf("last pick tab = %+v, want settled succeeded", last)
	}
	calls := h.feed.mirrorsStood()
	if calls[len(calls)-1].mirror != nil {
		t.Fatal("the pick's mirror still stands after the pick")
	}
	if entry, ok := h.entryOf(theMergeWorkspace); !ok || entry.State != wsm.MergeQueued {
		t.Fatalf("entry = %+v, %v; want the merge in line", entry, ok)
	}
}

func TestAPickThatDoesNotLeaveItsChangesPickedFailsTheMerge(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		why     string
	}{
		{name: "its turn failed", arrange: func(h *harness) { h.turnCloses = []wsm.TurnClose{wsm.CloseFailed} }, why: "its turn ended"},
		{name: "a dirty worktree", arrange: func(h *harness) { h.git.clean = false }, why: "uncommitted changes"},
		{name: "nothing picked", arrange: func(h *harness) { h.git.ahead = 0 }, why: "no commit stands ahead"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := pickHarness(t)
			tt.arrange(h)
			requestPick(t, h)

			// Act
			runThePick(t, h, context.Background(), false)

			// Assert: the tab and the head fail, the request leaves the
			// queue, and the requester is told while the workspace is
			// revealed.
			tabs := pickTabs(h)
			if failed := tabs[len(tabs)-1].GetSettled().GetFailed(); failed == nil || !strings.Contains(failed.GetSummary(), tt.why) {
				t.Fatalf("last pick tab = %+v, want failed for %q", tabs[len(tabs)-1], tt.why)
			}
			heads := h.feed.headsOn(theWorkspace)
			if heads[len(heads)-1].GetError().GetFailed() == nil {
				t.Fatalf("last head = %+v, want the merge failed", heads[len(heads)-1])
			}
			if _, ok := h.entryOf(theMergeWorkspace); ok {
				t.Fatal("the merge whose pick failed still stands in the queue")
			}
			sent := h.detached.sent()
			if len(sent) != 1 || sent[0].outcome.Kind != DetachedFailed {
				t.Fatalf("handbacks = %+v, want the failure told to the requester", sent)
			}
			if revealed := h.detached.revealed(); len(revealed) != 1 || revealed[0] != theMergeWorkspace {
				t.Fatalf("reveals = %v, want the merge workspace revealed", revealed)
			}
		})
	}
}

func TestAPickThatCannotBeJudgedFailsTheMergeAndIsAnError(t *testing.T) {
	// Arrange
	h := pickHarness(t)
	h.git.cleanErr = errors.New("git status failed")
	requestPick(t, h)

	// Act
	runThePick(t, h, context.Background(), false)

	// Assert
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.pick"); !ok {
		t.Fatal("the unjudgeable pick was not recorded at ERROR")
	}
	if _, ok := h.entryOf(theMergeWorkspace); ok {
		t.Fatal("the merge whose pick could not be judged still stands in the queue")
	}
}

func TestAPickWhoseBriefCannotBeComposedFailsTheMergeAndIsAnError(t *testing.T) {
	// Arrange
	h := pickHarness(t)
	requestPick(t, h)
	delete(h.briefs, BriefPick)

	// Act
	runThePick(t, h, context.Background(), false)

	// Assert
	if len(h.queue.submissions) != 0 {
		t.Fatal("a pick with no brief submitted something")
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.pick"); !ok {
		t.Fatal("the missing brief was not recorded at ERROR")
	}
	if sent := h.detached.sent(); len(sent) != 1 || sent[0].outcome.Kind != DetachedFailed {
		t.Fatalf("handbacks = %+v, want the failure told", sent)
	}
}

func TestAWithdrawnPickSettlesItsTabWithoutFailingTheMergeItself(t *testing.T) {
	// Arrange: the pick's turn is still running when its wait is withdrawn.
	h := pickHarness(t)
	requestPick(t, h)
	h.mu.Lock()
	h.awaitGate = make(chan struct{})
	h.mu.Unlock()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	runThePick(t, h, ctx, false)

	// Assert
	tabs := pickTabs(h)
	if failed := tabs[len(tabs)-1].GetSettled().GetFailed(); failed == nil || !strings.Contains(failed.GetSummary(), "left the queue") {
		t.Fatalf("last pick tab = %+v, want settled for the merge leaving the queue", tabs[len(tabs)-1])
	}
	if sent := h.detached.sent(); len(sent) != 0 {
		t.Fatalf("handbacks = %+v, want the withdrawal's own path to tell the requester", sent)
	}
}

func TestAResumedPickReattachesToItsRecordedTurn(t *testing.T) {
	// Arrange: a pick a restart cut short, its turn never delivered.
	h := pickHarness(t)
	h.registerMergeWorkspace(theWorkspace, "request-1", "master")
	started := time.UnixMilli(1_700_000_000_000)
	h.db.mu.Lock()
	h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{{
		Repo: h.repoKey(), Workspace: theMergeWorkspace, State: wsm.MergeRequested, Ledger: "ledger-1", EnqueuedAt: started,
		Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Branch: "feature-merge", Pick: "the reconnect fix", PickStartedAt: &started, PickTurn: "turn-pick"},
	}}
	h.db.mu.Unlock()

	// Act
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover: %v", err)
	}
	runThePick(t, h, context.Background(), true)

	// Assert
	if len(h.queue.submissions) != 1 || h.queue.submissions[0].Turn != "turn-pick" {
		t.Fatalf("submissions = %+v, want the recorded turn submitted under its own id", h.queue.submissions)
	}
	if entry, ok := h.entryOf(theMergeWorkspace); !ok || entry.State != wsm.MergeQueued || entry.Ledger != "ledger-1" {
		t.Fatalf("entry = %+v, %v; want the merge in line in the same bubble", entry, ok)
	}
}

func TestAnAskDuringAPickRedrawsItsTabWaitingOnTheUser(t *testing.T) {
	// Arrange
	h := pickHarness(t)
	requestPick(t, h)

	// Act
	h.asks.OnPermission(theMergeWorkspace, nil, permissionEdge("perm-1", true))

	// Assert
	tabs := pickTabs(h)
	if tabs[len(tabs)-1].GetWaitingOnUser() == nil {
		t.Fatalf("last pick tab = %+v, want waiting on the user", tabs[len(tabs)-1])
	}
}

func TestAFailedPickEndsTheMergeEvenThoughEndingItWithdrawsItsOwnWait(t *testing.T) {
	// Arrange: the pick runs under its own wait's context, which ending the
	// merge withdraws.
	h := pickHarness(t)
	h.git.ahead = 0
	requestPick(t, h)
	h.o.mu.Lock()
	ctx := h.o.requested[theMergeWorkspace].ctx
	h.o.mu.Unlock()

	// Act
	runThePick(t, h, ctx, false)

	// Assert
	if sent := h.detached.sent(); len(sent) != 1 || sent[0].outcome.Kind != DetachedFailed {
		t.Fatalf("handbacks = %+v, want the failure told", sent)
	}
	if revealed := h.detached.revealed(); len(revealed) != 1 {
		t.Fatalf("reveals = %v, want the merge workspace revealed", revealed)
	}
}

func TestAPickIsTimedByHowItEnded(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{name: "a pick that leaves its changes committed", arrange: func(*harness) {}, want: "pick:ok"},
		{name: "a pick whose turn failed", arrange: func(h *harness) { h.turnCloses = []wsm.TurnClose{wsm.CloseFailed} }, want: "pick:failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := pickHarness(t)
			tt.arrange(h)
			requestPick(t, h)

			// Act
			runThePick(t, h, context.Background(), false)

			// Assert
			got := h.timings.all()
			if len(got) == 0 || got[0] != tt.want {
				t.Fatalf("timings = %v, want %s first", got, tt.want)
			}
		})
	}
}

func TestAnAsyncMergeWithAnInstructionWaitsInLineUntilItHeadsTheQueue(t *testing.T) {
	// Arrange
	h := pickHarness(t)

	// Act: the hand-off, with no admission yet.
	err := h.o.Enqueue(context.Background(), Request{
		Workspace: theWorkspace, Source: asyncSource, By: RequestedByUser,
		Detached: &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix", Pick: "the reconnect fix"},
	})

	// Assert: the merge workspace is in line, its pick pending and not begun.
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	entry, ok := h.entryOf(theMergeWorkspace)
	if !ok || entry.State != wsm.MergeQueued || entry.Source.Pick == "" || entry.Source.PickStartedAt != nil {
		t.Fatalf("entry = %+v, %v; want it queued with its pick pending", entry, ok)
	}
	if _, picking := h.o.picks[theMergeWorkspace]; picking || len(h.queue.submissions) != 0 {
		t.Fatalf("a pick began before the merge headed its queue (submissions %d)", len(h.queue.submissions))
	}
}

func TestAPickAtTheHeadHoldsTheSlotUntilItSucceedsAndThenTheMergeIsAdmitted(t *testing.T) {
	// Arrange
	h := pickHarness(t)
	requestPick(t, h)

	// Act: the pump runs again while the pick stands, and after it.
	whilePicking, err := h.o.pumpOnce(context.Background(), h.repoKey())
	if err != nil {
		t.Fatalf("pump while picking: %v", err)
	}
	held, _ := h.entryOf(theMergeWorkspace)
	runThePick(t, h, context.Background(), false)
	cleared, _ := h.entryOf(theMergeWorkspace)
	afterPick, err := h.o.pumpOnce(context.Background(), h.repoKey())
	if err != nil {
		t.Fatalf("pump after the pick: %v", err)
	}

	// Assert
	if whilePicking || held.State != wsm.MergeQueued {
		t.Fatalf("admitted while picking = %v, entry %+v; want nothing admitted and the merge holding the head", whilePicking, held)
	}
	if cleared.Source.Pick != "" || cleared.Source.PickStartedAt != nil {
		t.Fatalf("entry after the pick = %+v, want its pick cleared", cleared)
	}
	if !afterPick {
		t.Fatal("the merge was not admitted once its pick succeeded")
	}
}

func TestAMergeWorkspaceWhosePickIsPendingNeverJoinsARace(t *testing.T) {
	tests := []struct {
		name  string
		entry wsm.MergeQueueEntry
		want  bool
	}{
		{name: "an ordinary merge in line", entry: wsm.MergeQueueEntry{State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}}, want: true},
		{name: "a merge workspace whose pick is pending", entry: wsm.MergeQueueEntry{State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Pick: "rebase it"}}},
		{name: "a branch merged upstream", entry: wsm.MergeQueueEntry{State: wsm.MergeQueued, Source: wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}}},
		{name: "a merge only requested", entry: wsm.MergeQueueEntry{State: wsm.MergeRequested, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := racable(tt.entry)

			// Assert
			if got != tt.want {
				t.Fatalf("racable = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestARestartTakesBackAPickItCutShortAtTheHead(t *testing.T) {
	// Arrange: the pick began at the head, then the daemon went away.
	h := pickHarness(t)
	requestPick(t, h)
	entry, _ := h.entryOf(theMergeWorkspace)
	h.o.mu.Lock()
	delete(h.o.picks, theMergeWorkspace)
	delete(h.o.requested, theMergeWorkspace)
	h.o.mu.Unlock()

	// Act
	err := h.o.recoverEntry(context.Background(), h.repoKey(), entry)

	// Assert
	if err != nil {
		t.Fatalf("recoverEntry: %v", err)
	}
	h.o.mu.Lock()
	p, picking := h.o.picks[theMergeWorkspace]
	h.o.mu.Unlock()
	if !picking || p.turn != entry.Source.PickTurn {
		t.Fatalf("pick = %+v, %v; want the begun pick taken back on its own turn %s", p, picking, entry.Source.PickTurn)
	}
}
