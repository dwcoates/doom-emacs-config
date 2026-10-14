package sidebar_test

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

func TestRowCarriesTheWorkspaceJoinKey(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")

	// Act.
	r.SetRegistry(registry(ws))

	// Assert: names collide across repos; dirs do not, and the id is identity.
	got := onlyRow(t, r).GetWorkspace().GetWorkspace()
	if got.GetId() != "w1" || got.GetDir() != ws.Dir {
		t.Fatalf("row workspace = %v, want the id and dir", got)
	}
}

func TestRowCarriesTheDisplayName(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "the name")))

	// Assert.
	if got := onlyRow(t, r).GetName().GetText(); got != "the name" {
		t.Fatalf("name = %q, want the display name", got)
	}
}

func TestRosterStatesTheCurrentWorkspace(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Act.
	r.SetSelected(theWS)

	// Assert.
	got := latest(t, r).GetCurrent().GetWorkspace()
	if got.GetId() != "w1" {
		t.Fatalf("current = %v, want the selected workspace by identity", got)
	}
}

func TestRosterStatesNoCurrentWorkspaceBeforeAnySelection(t *testing.T) {
	// Arrange, Act.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert: UNSET when there is none.
	if got := latest(t, r).GetCurrent(); got != nil {
		t.Fatalf("current = %v, want unset", got)
	}
}

func TestRowRestatesWhetherItIsCurrent(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))

	// Act.
	r.SetSelected(theWS)

	// Assert: redundant with the roster's own current, and deliberately so.
	rows := repoRows(t, latest(t, r))
	if !rowFor(rows, "w1").GetCurrent().GetCurrent() {
		t.Fatal("the selected row was not marked current")
	}
	if rowFor(rows, "w2").GetCurrent().GetCurrent() {
		t.Fatal("an unselected row was marked current")
	}
}

func TestSelectionIsStampedBeforeTheRegistryEchoesIt(t *testing.T) {
	// Arrange: the registry still says nothing is selected.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Act.
	r.SetSelected(theWS)

	// Assert: the daemon stamps it the moment SelectWorkspace arrives.
	if got := latest(t, r).GetCurrent().GetWorkspace().GetId(); got != "w1" {
		t.Fatalf("current = %q, want the stamped selection", got)
	}
}

func TestARegistryWithNoSelectionDoesNotClearAStampedOne(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))
	r.SetSelected(theWS)

	// Act: WSM has not caught up yet.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if got := latest(t, r).GetCurrent(); got == nil {
		t.Fatal("a registry that carries no selection cleared the stamped one")
	}
}

func TestARegistrysSelectionIsTaken(t *testing.T) {
	// Arrange: the durable selection, as a boot restore states it.
	r, _ := newResolver(t)
	reg := registry(workspace("w1", "one"))
	current := theWS
	reg.Current = &current

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if got := latest(t, r).GetCurrent().GetWorkspace().GetId(); got != "w1" {
		t.Fatalf("current = %q, want the registry's selection", got)
	}
}

func TestRowDrawsTheAttentionMarkerOnANotification(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	noticed := workspace("w1", "one")
	noticed.Attention = true

	// Act.
	r.SetRegistry(registry(noticed))

	// Assert: presence is the fact.
	if onlyRow(t, r).GetAttention() == nil {
		t.Fatal("a workspace with an unseen notification drew no attention marker")
	}
}

func TestRowDrawsNoAttentionMarkerWithoutANotification(t *testing.T) {
	// Arrange, Act.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if got := onlyRow(t, r).GetAttention(); got != nil {
		t.Fatalf("attention = %v, want no marker", got)
	}
}

func TestSelectingAWorkspaceClearsItsAttentionMarker(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	noticed := workspace("w1", "one")
	noticed.Attention = true
	r.SetRegistry(registry(noticed))

	// Act.
	r.SetSelected(theWS)

	// Assert.
	if onlyRow(t, r).GetAttention() != nil {
		t.Fatal("selecting the workspace did not clear its attention marker")
	}
}

func TestSelectingAWorkspaceLeavesAnotherRowsAttentionMarker(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	noticed := workspace("w2", "two")
	noticed.Attention = true
	r.SetRegistry(registry(workspace("w1", "one"), noticed))

	// Act.
	r.SetSelected(theWS)

	// Assert.
	if rowFor(repoRows(t, latest(t, r)), "w2").GetAttention() == nil {
		t.Fatal("selecting one workspace cleared another's attention marker")
	}
}

func TestTheWhenColumnCountsTheRunningTurnFromTheFootersClock(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	started := epoch.Add(time.Hour)

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: started, Act: footer.ActPrompt})

	// Assert: the row counts from the very instant the strip's clock does.
	running := onlyRow(t, r).GetWhen().GetTurnRunning()
	clock := r.Resolver.(*fanned).footer.TurnClock(theWS)
	if running == nil || running.GetStartedAtMs() != started.UnixMilli() || running.GetStartedAtMs() != clock.GetTurnStartedAtMs() {
		t.Fatalf("when = %v, footer clock = %v, want turn_running at the clock's start %d", onlyRow(t, r).GetWhen(), clock, started.UnixMilli())
	}
}

func TestTheWhenColumnReadsNoTurnRunning(t *testing.T) {
	tests := []struct {
		name    string
		record  func(*wsm.Workspace)
		arrange func(t *testing.T, r sidebarResolver)
	}{
		{name: "a workspace that has never run a turn", record: func(*wsm.Workspace) {}, arrange: func(*testing.T, sidebarResolver) {}},
		{name: "a workspace with no creation time", record: func(ws *wsm.Workspace) { ws.CreatedAt = time.Time{} }, arrange: func(*testing.T, sidebarResolver) {}},
		{name: "a finished turn", record: func(*wsm.Workspace) {}, arrange: func(t *testing.T, r sidebarResolver) { finished(t, r) }},
		{
			// A turn the shim link has not been seen for has no clock yet,
			// exactly as the strip draws none.
			name:   "an accepted turn before its link is seen",
			record: func(*wsm.Workspace) {},
			arrange: func(_ *testing.T, r sidebarResolver) {
				r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			},
		},
		{
			// REGRESSION LOCK: the column no longer counts from a turn end,
			// an activity or a selection.
			name: "a record with a turn end, an activity and a selection",
			record: func(ws *wsm.Workspace) {
				ws.LastTurnEndedAt = at(time.Hour)
				ws.LastActivityAt = at(2 * time.Hour)
				ws.LastSelectedAt = at(3 * time.Hour)
			},
			arrange: func(*testing.T, sidebarResolver) {},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ws := workspace(string(theWS), "one")
			tc.record(&ws)
			r := arrange(t, ws)

			// Act.
			tc.arrange(t, r)

			// Assert.
			if when := onlyRow(t, r).GetWhen(); when.GetNoTurnRunning() == nil {
				t.Fatalf("when = %v, want no_turn_running", when)
			}
		})
	}
}

func TestTheWhenColumnStartsCountingOnceTheTurnsLinkIsSeen(t *testing.T) {
	// Arrange: the turn is accepted before the shim link is seen, so the
	// strip's clock has not started.
	r := arrange(t)
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	live(t, r)

	// Assert: the clock's start reaches the roster when the strip's does.
	if got := onlyRow(t, r).GetWhen().GetTurnRunning().GetStartedAtMs(); got != epoch.UnixMilli() {
		t.Fatalf("when = %v, want turn_running from %d", onlyRow(t, r).GetWhen(), epoch.UnixMilli())
	}
}

func TestTheWhenColumnStopsCountingWhenTheTurnEnds(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert.
	if when := onlyRow(t, r).GetWhen(); when.GetNoTurnRunning() == nil {
		t.Fatalf("when = %v, want no_turn_running once the turn ended", when)
	}
}

func TestTheWhenColumnCountsASyncMergeFromItsQueueInstant(t *testing.T) {
	queued := epoch.Add(time.Hour)
	tests := []struct {
		name  string
		facts footer.MergeFacts
	}{
		{name: "a merge waiting in line", facts: footer.MergeFacts{State: "queued", QueuedAt: queued, Step: footer.StepEnqueued, QueuePlace: 1, QueueWaiting: 1}},
		{name: "a merge waiting for the workspace to fall free", facts: footer.MergeFacts{State: "merging", QueuedAt: queued, Step: footer.StepAwaitingFree}},
		{name: "a merge being worked on", facts: footer.MergeFacts{State: "merging", QueuedAt: queued, Step: footer.StepTesting}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))

			// Act.
			r.SetMerge(theWS, tc.facts)

			// Assert: the row counts from the instant the footer reads.
			merging := onlyRow(t, r).GetWhen().GetSyncMerging()
			started := r.Resolver.(*fanned).footer.MergeStartedAt(theWS)
			if merging == nil || started == nil || merging.GetStartedAtMs() != queued.UnixMilli() || *started != queued.UnixMilli() {
				t.Fatalf("when = %v, footer merge start = %v, want sync_merging from %d", onlyRow(t, r).GetWhen(), started, queued.UnixMilli())
			}
		})
	}
}

func TestTheWhenColumnCountsTheMergeRatherThanTheTurnItRuns(t *testing.T) {
	// Arrange: a merge's own turn (its fixing prompt) runs while it stands.
	r := live(t, arrange(t))
	queued := epoch.Add(time.Hour)
	r.SetMerge(theWS, footer.MergeFacts{State: "merging", QueuedAt: queued, Step: footer.StepFixing, Attempt: 1, MaxAttempts: 3})

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch.Add(2 * time.Hour), Act: footer.ActPrompt})

	// Assert.
	if got := onlyRow(t, r).GetWhen().GetSyncMerging().GetStartedAtMs(); got != queued.UnixMilli() {
		t.Fatalf("when = %v, want sync_merging from %d", onlyRow(t, r).GetWhen(), queued.UnixMilli())
	}
}

func TestTheWhenColumnStopsCountingTheMergeOnceItLeaves(t *testing.T) {
	tests := []struct {
		name  string
		facts footer.MergeFacts
	}{
		{name: "a failed merge", facts: footer.MergeFacts{State: "failed", FailedArea: footer.FailedTests}},
		{name: "a merge taken off the queue", facts: footer.MergeFacts{State: "none"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))
			r.SetMerge(theWS, footer.MergeFacts{State: "merging", QueuedAt: epoch, Step: footer.StepTesting})

			// Act.
			r.SetMerge(theWS, tc.facts)

			// Assert.
			if when := onlyRow(t, r).GetWhen(); when.GetNoTurnRunning() == nil {
				t.Fatalf("when = %v, want no_turn_running once the merge left", when)
			}
		})
	}
}

func TestTheRowCarriesTheDurableLastSelectedInstant(t *testing.T) {
	cases := []struct {
		name     string
		selected *time.Time
		closed   bool
		want     *int64
	}{
		{name: "a selected workspace carries its instant", selected: at(2 * time.Hour), want: millis(at(2 * time.Hour))},
		{name: "a never-selected workspace carries none", selected: nil, want: nil},
		{name: "a closed workspace still carries its instant", selected: at(time.Hour), closed: true, want: millis(at(time.Hour))},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			ws := workspace("w1", "one")
			ws.LastSelectedAt = tc.selected
			ws.Closed = tc.closed

			// Act.
			r.SetRegistry(registry(ws))

			// Assert.
			got := onlyRow(t, r).GetLastSelected()
			switch {
			case tc.want == nil && got != nil:
				t.Fatalf("last_selected = %v, want unset", got)
			case tc.want != nil && got == nil:
				t.Fatalf("last_selected unset, want %d", *tc.want)
			case tc.want != nil && got.GetAtMs() != *tc.want:
				t.Fatalf("last_selected = %d, want %d", got.GetAtMs(), *tc.want)
			}
		})
	}
}

// TestANewRegistryMovesTheLastSelectedInstant pins that the field follows the
// durable record push by push: a selection re-publishes the registry, and the
// row carries the new instant.
func TestANewRegistryMovesTheLastSelectedInstant(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.LastSelectedAt = at(time.Hour)
	r.SetRegistry(registry(ws))
	ws.LastSelectedAt = at(3 * time.Hour)

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	if got := onlyRow(t, r).GetLastSelected().GetAtMs(); got != epoch.Add(3*time.Hour).UnixMilli() {
		t.Fatalf("last_selected = %d, want the newer selection instant", got)
	}
}

func millis(t *time.Time) *int64 {
	ms := t.UnixMilli()
	return &ms
}

func TestMergedBeatsARunningTurnInTheWhenColumn(t *testing.T) {
	// Arrange.
	ws := workspace(string(theWS), "one")
	ws.MergedAt = at(time.Hour)
	r := live(t, arrange(t, ws))

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert: that the workspace is done is the more interesting fact.
	row := rowFor(rosterwalk.MergedRows(latest(t, r)), string(theWS))
	if got := row.GetWhen().GetMerged().GetAtMs(); got != epoch.Add(time.Hour).UnixMilli() {
		t.Fatalf("when = %v, want the merge instant", row.GetWhen())
	}
}

func TestHiddenBeatsARunningTurnInTheWhenColumn(t *testing.T) {
	// Arrange.
	ws := workspace(string(theWS), "one")
	ws.Hidden = true
	ws.HiddenAt = at(time.Hour)
	r := live(t, arrange(t, ws))

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert: a hidden row counts from its hide.
	row := latest(t, r).GetHidden().GetRows().GetRows()[0]
	if got := row.GetWhen().GetHidden().GetAtMs(); got != epoch.Add(time.Hour).UnixMilli() {
		t.Fatalf("when = %v, want the hide instant", row.GetWhen())
	}
}

func TestRowDetailCarriesTheBranchAndItsBase(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	detail := onlyRow(t, r).GetDetail()
	if detail.GetBranch().GetName() != ws.Branch {
		t.Fatalf("branch = %q, want %q", detail.GetBranch().GetName(), ws.Branch)
	}
	if detail.GetParentBranch().GetName() != ws.ParentBranch {
		t.Fatalf("parent branch = %q, want %q", detail.GetParentBranch().GetName(), ws.ParentBranch)
	}
}

func TestRowDetailOmitsTheBranchWhenThereIsNone(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.Branch = ""

	// Act.
	r.SetRegistry(registry(ws))

	// Assert: an absent line is omitted, not rendered blank.
	if got := onlyRow(t, r).GetDetail().GetBranch(); got != nil {
		t.Fatalf("branch = %v, want the line omitted", got)
	}
}

func TestRowDetailOmitsTheParentBranchWhenThereIsNone(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.ParentBranch = ""

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	if got := onlyRow(t, r).GetDetail().GetParentBranch(); got != nil {
		t.Fatalf("parent branch = %v, want the line omitted", got)
	}
}

func TestRowDetailCarriesTheCommitCount(t *testing.T) {
	merged := time.UnixMilli(1_700_000_000_000)
	tests := []struct {
		name   string
		count  uint32
		merged bool
	}{
		{name: "a live workspace's count", count: 6},
		{name: "a zero count", count: 0},
		{name: "a merged workspace's landed count", count: 3, merged: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			ws := workspace("w1", "one")
			count := tc.count
			ws.CommitCount = &count
			if tc.merged {
				ws.MergedAt, ws.Closed = &merged, true
			}

			// Act.
			r.SetRegistry(registry(ws))

			// Assert.
			var got []*frontendv1.RosterRow
			if tc.merged {
				got = mergedRows(latest(t, r))
			} else {
				got = []*frontendv1.RosterRow{onlyRow(t, r)}
			}
			if len(got) != 1 || got[0].GetDetail().GetCommits() == nil || got[0].GetDetail().GetCommits().GetCount() != tc.count {
				t.Fatalf("rows = %v, want one row whose detail carries %d commits", got, tc.count)
			}
		})
	}
}

func TestRowDetailOmitsTheCommitCountWhenNeverCounted(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if got := onlyRow(t, r).GetDetail().GetCommits(); got != nil {
		t.Fatalf("commits = %v, want the line omitted", got)
	}
}

func TestRowDetailCarriesTheSummary(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Act.
	r.SetSummary(theWS, "rework the roster resolver")

	// Assert.
	got := onlyRow(t, r).GetDetail().GetSummary().GetText()
	if got != "rework the roster resolver" {
		t.Fatalf("summary = %q, want the last prompt's line", got)
	}
}

func TestRowSummaryIsThePromptsFirstLineOnly(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Act: a prompt is often a paragraph and the row has one line to give it.
	r.SetSummary(theWS, "rework the roster\nand then the tray\nand the footer")

	// Assert.
	if got := onlyRow(t, r).GetDetail().GetSummary().GetText(); got != "rework the roster" {
		t.Fatalf("summary = %q, want the first line only", got)
	}
}

func TestRowDetailOmitsAnEmptySummary(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Act.
	r.SetSummary(theWS, "")

	// Assert.
	if got := onlyRow(t, r).GetDetail().GetSummary(); got != nil {
		t.Fatalf("summary = %v, want the line omitted", got)
	}
}

func TestRowRecedesWhenTheWorkspaceIsClosed(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	closed := workspace("w1", "one")
	closed.Closed = true

	// Act.
	r.SetRegistry(registry(closed))

	// Assert.
	if !onlyRow(t, r).GetClosed().GetClosed() {
		t.Fatal("a closed workspace's row did not recede")
	}
}

func TestRowRecedesWhenTheWorkspaceMerged(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	merged := workspace("w1", "one")
	merged.MergedAt = at(0)

	// Act.
	r.SetRegistry(registry(merged))

	// Assert.
	row := rowFor(rosterwalk.MergedRows(latest(t, r)), "w1")
	if !row.GetClosed().GetClosed() {
		t.Fatal("a merged workspace's row did not recede")
	}
}

func TestRowRecedesWhenTheSessionWasKilled(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(workspace("w1", "one"))
	reg.Sessions = []wsm.Session{{
		Workspace: theWS,
		Terminal:  &wsm.SessionTerminal{Kind: "killed", At: epoch},
	}}

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if !onlyRow(t, r).GetClosed().GetClosed() {
		t.Fatal("a killed session's row did not recede")
	}
}

func TestRowDoesNotRecedeOnAnOrdinarySessionDeath(t *testing.T) {
	// Arrange: a shim that died is not a workspace anyone put away.
	r, _ := newResolver(t)
	reg := registry(workspace("w1", "one"))
	reg.Sessions = []wsm.Session{{
		Workspace: theWS,
		Terminal:  &wsm.SessionTerminal{Kind: "shim_died", At: epoch},
	}}

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if onlyRow(t, r).GetClosed().GetClosed() {
		t.Fatal("a shim death receded the row")
	}
}

func TestRowDoesNotRecedeOnceAKilledSessionsTerminalIsRetired(t *testing.T) {
	// Arrange: the row-level consequence of the live-shim invariant — a
	// workspace whose shim is live carries no terminal record, so the row it
	// resolves to is one Emacs gives a tab. The record is present and its
	// terminal retired, which is what a re-opened workspace's record looks
	// like; a killed one beside it recedes (TestRowRecedesWhenTheSessionWasKilled).
	r, _ := newResolver(t)
	reg := registry(workspace("w1", "one"))
	reg.Sessions = []wsm.Session{{Workspace: theWS}}

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if onlyRow(t, r).GetClosed().GetClosed() {
		t.Fatal("a session whose terminal was retired receded the row")
	}
}

func TestRowDoesNotRecedeWhileTheWorkspaceIsOpen(t *testing.T) {
	// Arrange, Act.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if onlyRow(t, r).GetClosed().GetClosed() {
		t.Fatal("an open workspace's row receded")
	}
}

// ---- The VIEWED marker: the row's display mode ----------------------------
//
// PRESENT is PARTIAL and ABSENT is FULL, the marker only ever stands on a
// turn-end row (done, interrupted or turn_failed), and it is derived from the read-result
// fact, which only the next turn resets. These lock all three halves, because
// a marker that never clears, a marker that clears on every push and a marker
// drawn on live or exceptional work are the three ways this feature fails.

// finished brings the workspace to a DONE row: a live session whose turn
// completed.
func finished(t *testing.T, r sidebarResolver) sidebarResolver {
	t.Helper()
	live(t, r)
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
	if got := statusName(onlyRow(t, r)); got != "done" {
		t.Fatalf("status = %q, want done — the arrangement did not finish the turn", got)
	}
	return r
}

func TestRowStatesItsLiveDetachedWork(t *testing.T) {
	cases := []struct {
		name       string
		arrange    func(t *testing.T, r sidebarResolver)
		wantStatus string
		wantLive   bool
	}{
		{name: "an unread turn end with detached work states it", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
			r.SetTurnEnded(theWS, wsm.CloseCompleted)
		}, wantStatus: "idle_async", wantLive: true},
		{name: "idle_async states it", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
			r.SetTurnEnded(theWS, wsm.CloseCompleted)
			r.SetViewed(theWS)
		}, wantStatus: "idle_async", wantLive: true},
		{name: "a done with no detached work does not", arrange: func(t *testing.T, r sidebarResolver) {
			finished(t, r)
		}, wantStatus: "done", wantLive: false},
		{name: "an emptied live-work set clears it", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
			r.SetTurnEnded(theWS, wsm.CloseCompleted)
			r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{})
		}, wantStatus: "done", wantLive: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := arrange(t)

			// Act.
			tc.arrange(t, r)

			// Assert: presence is the fact, on whatever arm the row stands.
			row := onlyRow(t, r)
			if got := statusName(row); got != tc.wantStatus {
				t.Fatalf("status = %q, want %q", got, tc.wantStatus)
			}
			if got := row.GetDetachedLive() != nil; got != tc.wantLive {
				t.Fatalf("detached_live = %v, want %v", got, tc.wantLive)
			}
		})
	}
}

func TestAContextCutsEndDrawsTheRowPartialAtOnce(t *testing.T) {
	cases := []struct {
		name       string
		act        footer.SessionAct
		how        sidebar.TurnClose
		wantStatus string
		wantViewed bool
	}{
		{name: "a completed clear is read at once", act: footer.ActClear, how: wsm.CloseCompleted, wantStatus: "done", wantViewed: true},
		{name: "a completed compaction is read at once", act: footer.ActCompact, how: wsm.CloseCompleted, wantStatus: "done", wantViewed: true},
		{name: "an interrupted clear is read at once, as every interrupt is", act: footer.ActClear, how: wsm.CloseKilled, wantStatus: "interrupted", wantViewed: true},
		{name: "a failed compaction stands as its turn fault", act: footer.ActCompact, how: wsm.CloseFailed, wantStatus: "vendor_blocked", wantViewed: false},
		{name: "a completed prompt stays unread", act: footer.ActPrompt, how: wsm.CloseCompleted, wantStatus: "done", wantViewed: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: tc.act})

			// Act.
			r.SetTurnEnded(theWS, tc.how)

			// Assert: the marker rides the very push that ends the turn.
			row := onlyRow(t, r)
			if got := statusName(row); got != tc.wantStatus {
				t.Fatalf("status = %q, want %q", got, tc.wantStatus)
			}
			if got := row.GetViewed() != nil; got != tc.wantViewed {
				t.Fatalf("viewed = %v, want %v", got, tc.wantViewed)
			}
		})
	}
}

func TestAContextCutReadAtOnceIsRecorded(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActClear})

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert.
	for _, rec := range r.surfaces.Records() {
		if rec.Operation == "daemon.sidebar.result_read_on_cut" && rec.Context["act"] == "clear" {
			return
		}
	}
	t.Fatal("no daemon.sidebar.result_read_on_cut record naming the clear")
}

func TestAnInterruptsEndRecordsItsResultReadAtOnce(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseKilled)

	// Assert.
	for _, rec := range r.surfaces.Records() {
		if rec.Operation == "daemon.sidebar.result_read_on_interrupt" && rec.Context["status"] == "interrupted" {
			return
		}
	}
	t.Fatal("no daemon.sidebar.result_read_on_interrupt record naming the interrupt")
}

func TestAContextCutReadAtOnceStaysReadAfterItsDetachedWorkEnds(t *testing.T) {
	// Arrange: a clear completed while detached work ran, so the read row
	// yielded to idle_async.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActClear})
	r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
	if got := statusName(onlyRow(t, r)); got != "idle_async" {
		t.Fatalf("status = %q, want idle_async — the read cut yields to live detached work", got)
	}

	// Act: the detached work ends.
	r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{})

	// Assert.
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the cut's done still PARTIAL once the detached work ends")
	}
}

func TestRowCarriesNoViewedMarkerUntilTheEditorReportsOne(t *testing.T) {
	// Arrange, Act.
	r := finished(t, arrange(t))

	// Assert: ABSENT is FULL, which is what a row that has not been seen is.
	if got := onlyRow(t, r).GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want unset for a row nobody has dwelt on", got)
	}
}

func TestViewedReportDrawsADoneRowPartial(t *testing.T) {
	// Arrange.
	r := finished(t, arrange(t))

	// Act.
	r.SetViewed(theWS)

	// Assert: presence is the mode.
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the marker the editor's report raises on a done row")
	}
}

func TestAViewedReportOnANonDoneRowDoesNotSurviveIntoDone(t *testing.T) {
	// Arrange: a report on a thinking row, which is refused.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnActivity(theWS, agent("a1"), &conversationv1.AgentActivity{})
	r.SetViewed(theWS)

	// Act: the turn finishes.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert: the finished response is new; the user has not seen it yet.
	row := onlyRow(t, r)
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done — the arrangement did not finish the turn", got)
	}
	if got := row.GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want unset: a report made while thinking is not a report on the done row", got)
	}
}

func TestAViewedReportOnANonDoneRowIsRecordedAsRefused(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.SetViewed(theWS)

	// Assert: the dropped report is visible in the log, not silent.
	for _, rec := range r.surfaces.Records() {
		if rec.Operation == "daemon.sidebar.row_viewed_refused" {
			return
		}
	}
	t.Fatal("no daemon.sidebar.row_viewed_refused record for a report on a ready row")
}

func TestViewedReportIsIdempotent(t *testing.T) {
	// Arrange.
	r := finished(t, arrange(t))
	r.SetViewed(theWS)

	// Act: marking an already-viewed workspace changes nothing.
	r.SetViewed(theWS)

	// Assert.
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the marker to still stand after a second report")
	}
}

func TestAStatusChangeClearsTheViewedMarker(t *testing.T) {
	// Arrange: a viewed, done row.
	r := finished(t, arrange(t))
	r.SetViewed(theWS)

	// Act: new activity, from an origin the editor never told the roster about.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert: new activity is not something the user has already seen.
	row := onlyRow(t, r)
	if got := statusName(row); got != "submitting" {
		t.Fatalf("status = %q, want submitting — the arrangement did not change the arm", got)
	}
	if got := row.GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want the marker cleared by the status change", got)
	}
}

func TestAViewedMarkerClearedByAStatusChangeStaysClearedBackOnDone(t *testing.T) {
	// Arrange: a viewed, done row that took another turn.
	r := finished(t, arrange(t))
	r.SetViewed(theWS)
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Act: that turn finishes too.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert: the new response is unseen until the editor reports it again.
	row := onlyRow(t, r)
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done — the arrangement did not finish the turn", got)
	}
	if got := row.GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want unset until a fresh report on the new done", got)
	}
}

func TestAPushThatRestatesTheSameStatusKeepsTheViewedMarker(t *testing.T) {
	// Arrange.
	r := finished(t, arrange(t))
	r.SetViewed(theWS)

	// Act: a re-push that leaves the arm exactly where it was.
	r.SetSummary(theWS, "a summary line")

	// Assert: without this guard nothing could ever stay PARTIAL.
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the marker to survive a push that changed no status")
	}
}

func TestAnotherWorkspacesStatusChangeLeavesThisMarkerStanding(t *testing.T) {
	// Arrange: two workspaces, one of them done and viewed.
	r := arrange(t, workspace(string(theWS), "one"), workspace("w2", "two"))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
	r.SetViewed(theWS)

	// Act: the OTHER workspace takes a turn.
	r.SetTurn(ids.WorkspaceID("w2"), &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert: the reset is per row, never roster-wide.
	row := rowFor(repoRows(t, latest(t, r)), string(theWS))
	if row == nil {
		t.Fatal("the viewed workspace lost its row")
	}
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done — the arrangement did not finish the turn", got)
	}
	if got := row.GetViewed(); got == nil {
		t.Fatal("viewed = unset, want a neighbour's activity to leave this row PARTIAL")
	}
}

// ---- The REVIVING marker: a parked session coming back up ------------------
//
// PRESENT while the workspace verbs hold a revival in flight, ABSENT
// otherwise. It is a marker BESIDE the status: it neither moves the arm nor
// clears the viewed marker.

func TestRevivingMarkerFollowsTheRevivalEdges(t *testing.T) {
	cases := []struct {
		name  string
		edges []bool
		want  bool
	}{
		{name: "no revival was ever decided", edges: nil, want: false},
		{name: "a revival is in flight", edges: []bool{true}, want: true},
		{name: "the revival ended", edges: []bool{true, false}, want: false},
		{name: "a lowered marker with no revival stays absent", edges: []bool{false}, want: false},
		{name: "a second revival after the first ended", edges: []bool{true, false, true}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t))

			// Act.
			for _, reviving := range tc.edges {
				r.SetReviving(theWS, reviving)
			}

			// Assert: presence is the fact.
			if got := onlyRow(t, r).GetReviving() != nil; got != tc.want {
				t.Fatalf("reviving present = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRevivingMarkerLeavesTheStatusArmAlone(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	before := statusName(onlyRow(t, r))

	// Act.
	r.SetReviving(theWS, true)

	// Assert: the marker is not an arm and moves none.
	if got := statusName(onlyRow(t, r)); got != before {
		t.Fatalf("status = %q, want %q unchanged by the reviving marker", got, before)
	}
}

func TestRevivingMarkerLeavesTheViewedMarkerStanding(t *testing.T) {
	// Arrange.
	r := finished(t, arrange(t))
	r.SetViewed(theWS)

	// Act.
	r.SetReviving(theWS, true)

	// Assert: only a STATUS change clears viewed, and this is not one.
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the reviving marker to leave the display mode alone")
	}
}

func TestRevivingMarkerIsPerRow(t *testing.T) {
	// Arrange: two workspaces.
	r := arrange(t, workspace(string(theWS), "one"), workspace("w2", "two"))

	// Act: only the other one revives.
	r.SetReviving(ids.WorkspaceID("w2"), true)

	// Assert.
	row := rowFor(repoRows(t, latest(t, r)), string(theWS))
	if row == nil {
		t.Fatal("the workspace lost its row")
	}
	if got := row.GetReviving(); got != nil {
		t.Fatalf("reviving = %v, want a neighbour's revival to leave this row unmarked", got)
	}
}

func TestAReadResultStaysReadAcrossALinkBlip(t *testing.T) {
	// Arrange: a done row the user has read.
	r := finished(t, arrange(t))
	r.SetViewed(theWS)
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Act: the route comes back; no new result arrived in between.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert: the result is still read, so the row is PARTIAL again.
	row := onlyRow(t, r)
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done — the arrangement did not restore the link", got)
	}
	if got := row.GetViewed(); got == nil {
		t.Fatal("viewed = unset, want a read result to stay read across an arm change that is not a new result")
	}
}

// A FAILED TURN'S FAULT STANDS OVER ITS END until the next turn (owner
// ruling, 2026-10-06), so its read state is observed where it lives: the
// durable result the resolver reports.
func TestAReadFailedResultStaysReadAcrossALinkBlip(t *testing.T) {
	// Arrange: a failed turn the user has read.
	r, reports := resultResolver(t, nil)
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.SetTurnEnded(theWS, wsm.CloseFailed)
	r.SetViewed(theWS)
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Act: the route comes back; no new result arrived in between.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert: the fault still stands over the turn's end, and its result is
	// still the READ failed turn.
	if got := statusName(onlyRow(t, r)); got != "vendor_blocked" {
		t.Fatalf("status = %q, want the vendor turn fault — the arrangement did not restore the link", got)
	}
	last := (*reports)[len(*reports)-1].result
	if last == nil || last.End != wsm.TurnResultFailed || !last.Read {
		t.Fatalf("last reported result = %+v, want a read failed turn", last)
	}
}

func TestViewedReportNeverDrawsANonTurnEndRowPartialWithNothingParked(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, r sidebarResolver)
		want    string
	}{
		{name: "none", arrange: func(*testing.T, sidebarResolver) {}, want: "none"},
		{name: "init", arrange: func(_ *testing.T, r sidebarResolver) {
			r.OnLink(theWS, shimclient.LinkDialing)
		}, want: "init"},
		{name: "ready", arrange: func(t *testing.T, r sidebarResolver) { live(t, r) }, want: "ready"},
		{name: "submitting", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
		}, want: "submitting"},
		{name: "thinking", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			r.OnActivity(theWS, agent("a1"), &conversationv1.AgentActivity{})
		}, want: "thinking"},
		{name: "clearing", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActClear})
		}, want: "clearing"},
		{name: "compacting", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActCompact})
		}, want: "compacting"},
		{name: "permission", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.OnPermission(theWS, agent("a1"), permissionAsk("p1"))
		}, want: "permission"},
		{name: "idle_async", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
			r.SetTurnEnded(theWS, wsm.CloseCompleted)
			r.SetViewed(theWS) // read, so the unread done yields to idle_async
		}, want: "idle_async"},
		{name: "vendor_blocked", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.OnSessionUpdate(theWS, rejectedRateLimit())
		}, want: "vendor_blocked"},
		{name: "severed", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.OnLink(theWS, shimclient.LinkRedialing)
		}, want: "severed"},
		{name: "start_failed", arrange: func(_ *testing.T, r sidebarResolver) {
			r.OnLink(theWS, shimclient.LinkDialing)
			r.OnLink(theWS, shimclient.LinkDead)
		}, want: "start_failed"},
		{name: "degraded", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.OnSessionUpdate(theWS, degradedUpdate())
		}, want: "degraded"},
		{name: "dead", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.OnLink(theWS, shimclient.LinkDead)
		}, want: "dead"},
		{name: "merging", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetMerge(theWS, footer.MergeFacts{State: "merging", Step: footer.StepTesting})
		}, want: "merging"},
		{name: "merge_queued", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetMerge(theWS, footer.MergeFacts{State: "queued", Step: footer.StepEnqueued, QueuePlace: 1, QueueWaiting: 1})
		}, want: "merge_queued"},
		{name: "merge_failed", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetMerge(theWS, footer.MergeFacts{State: "failed", FailedArea: footer.FailedTests})
		}, want: "merge_failed"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := arrange(t)
			tc.arrange(t, r)
			if got := statusName(onlyRow(t, r)); got != tc.want {
				t.Fatalf("status = %q, want %q — the arrangement missed the arm", got, tc.want)
			}

			// Act: the editor reports a dwell, however long it has been.
			r.SetViewed(theWS)

			// Assert: live work and exceptional states are never deprioritized.
			if got := onlyRow(t, r).GetViewed(); got != nil {
				t.Fatalf("viewed = %v on a %s row, want unset: only a turn-end row goes PARTIAL", got, tc.want)
			}
		})
	}
}

// ---- AVAILABILITY: whether an editor may open the workspace yet -------------
//
// Resolved from the shim link alone: available once a link has connected under
// this daemon (and never again pending), unavailable when the link died before
// it ever connected, pending otherwise.

// availabilityName names the row's set availability arm, "" when unset.
func availabilityName(row *frontendv1.RosterRow) string {
	switch row.GetAvailability().GetAvailability().(type) {
	case *frontendv1.RosterRowAvailability_Pending:
		return "pending"
	case *frontendv1.RosterRowAvailability_Available:
		return "available"
	case *frontendv1.RosterRowAvailability_Unavailable:
		return "unavailable"
	default:
		return ""
	}
}

func TestAvailabilityFollowsTheBringUpAndTheShimLink(t *testing.T) {
	cases := []struct {
		name       string
		bringingUp bool
		links      []shimclient.LinkState
		want       string
	}{
		{name: "nothing under way and no link: nothing to wait for", want: "available"},
		{name: "a bring-up under way with no link yet", bringingUp: true, want: "pending"},
		{name: "a bring-up under way still dialing", bringingUp: true, links: []shimclient.LinkState{shimclient.LinkDialing}, want: "pending"},
		{name: "a bring-up whose link connected", bringingUp: true, links: []shimclient.LinkState{shimclient.LinkConnected}, want: "available"},
		{name: "a link that died before it ever connected", links: []shimclient.LinkState{shimclient.LinkDialing, shimclient.LinkDead}, want: "unavailable"},
		{name: "a link that connected and then died", links: []shimclient.LinkState{shimclient.LinkConnected, shimclient.LinkDead}, want: "available"},
		{name: "a failed start that a later start brought up", links: []shimclient.LinkState{shimclient.LinkDead, shimclient.LinkConnected}, want: "available"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := arrange(t)
			r.SetBringingUp(theWS, tc.bringingUp)

			// Act.
			for _, link := range tc.links {
				r.OnLink(theWS, link)
			}

			// Assert.
			if got := availabilityName(onlyRow(t, r)); got != tc.want {
				t.Fatalf("availability = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAvailabilityLeavesPendingWhenTheBringUpEnds(t *testing.T) {
	// Arrange: a bring-up that ends with no link at all (a cold gate, say).
	r := arrange(t)
	r.SetBringingUp(theWS, true)

	// Act.
	r.SetBringingUp(theWS, false)

	// Assert.
	if got := availabilityName(onlyRow(t, r)); got != "available" {
		t.Fatalf("availability = %q, want available", got)
	}
}

// armChanges answers every roster status-arm change the resolver recorded.
func armChanges(surfaces *dlog.TestSurfaces) []dlog.Record {
	var out []dlog.Record
	for _, rec := range surfaces.Records() {
		if rec.Operation == "daemon.sidebar.status_arm_changed" {
			out = append(out, rec)
		}
	}
	return out
}

func TestTheRosterRecordsEachPublishedStatusArmChange(t *testing.T) {
	cases := []struct {
		name string
		act  func(r sidebarResolver)
		want []dlog.Context
	}{
		{
			name: "the first arm a row publishes replaces nothing and is not a change",
			act:  func(sidebarResolver) {},
			want: nil,
		},
		{
			name: "an arm change is recorded with the arm, the arm it replaced, and the fact that moved it",
			act: func(r sidebarResolver) {
				r.OnLink(theWS, shimclient.LinkConnected)
			},
			want: []dlog.Context{
				{"arm": "ready", "previous_arm": "none", "cause": "daemon.sidebar.on_link", "workspace_id": string(theWS)},
			},
		},
		{
			name: "a fact that leaves the arm where it stands records nothing",
			act: func(r sidebarResolver) {
				r.OnLink(theWS, shimclient.LinkConnected)
				r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
				r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
			},
			want: []dlog.Context{
				{"arm": "ready", "previous_arm": "none", "cause": "daemon.sidebar.on_link", "workspace_id": string(theWS)},
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := arrange(t)

			// Act.
			tc.act(r)

			// Assert.
			got := armChanges(r.surfaces)
			if len(got) != len(tc.want) {
				t.Fatalf("arm changes = %+v, want %d", got, len(tc.want))
			}
			for i, want := range tc.want {
				if got[i].Level != "info" {
					t.Fatalf("change %d level = %q, want info", i, got[i].Level)
				}
				for key, value := range want {
					if got[i].Context[key] != value {
						t.Fatalf("change %d %s = %v, want %v (record %+v)", i, key, got[i].Context[key], value, got[i].Context)
					}
				}
			}
		})
	}
}

func TestAMergeWorkspaceNestsUnderTheWorkspaceItWasForkedFrom(t *testing.T) {
	// Arrange: a revealed merge workspace.
	r, _ := newResolver(t)
	requester := workspace("w1", "one")
	merging := workspace("w2", "one-merge")
	parent := requester.ID
	merging.Parent = &parent
	merging.MergeOf = &wsm.MergeOrigin{Requester: requester.ID, Request: "request-1"}
	merging.MergeRevealed = true

	// Act
	r.SetRegistry(registry(requester, merging))

	// Assert
	rows := repoRows(t, latest(t, r))
	if len(rows) != 1 || len(rows[0].GetChildren()) != 1 || rows[0].GetChildren()[0].GetName().GetText() != "one-merge" {
		t.Fatalf("rows = %v, want the merge workspace nested under its requester", rowNames(rows))
	}
}

// hiddenMergeWorkspace is an unrevealed merge workspace forked from REQUESTER,
// nested under it.
func hiddenMergeWorkspace(requester ids.WorkspaceID) wsm.Workspace {
	merging := workspace("w2", "one-merge")
	merging.Parent = &requester
	merging.ParentBranch = "master"
	merging.MergeOf = &wsm.MergeOrigin{Requester: requester, Request: "request-1"}
	return merging
}

func TestAHiddenMergeWorkspaceIsInNoSection(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	requester := workspace("w1", "one")

	// Act
	r.SetRegistry(registry(requester, hiddenMergeWorkspace(requester.ID)))

	// Assert
	for _, row := range rosterwalk.AllRows(latest(t, r)) {
		if row.GetWorkspace().GetWorkspace().GetId() == "w2" {
			t.Fatalf("the hidden merge workspace has a row: %v", row)
		}
	}
	rows := repoRows(t, latest(t, r))
	if len(rows) != 1 || len(rows[0].GetChildren()) != 0 {
		t.Fatalf("rows = %v, want the requester alone, with no child", rowNames(rows))
	}
}

func TestAHiddenMergeWorkspaceHasNoTab(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	requester := workspace("w1", "one")

	// Act
	r.SetRegistry(registry(requester, hiddenMergeWorkspace(requester.ID)))

	// Assert
	for _, tab := range latest(t, r).GetTabBar().GetTabs() {
		if tab.GetWorkspace().GetId() == "w2" {
			t.Fatalf("the hidden merge workspace has a tab: %v", tab)
		}
	}
}

func TestALandedHiddenMergeWorkspaceIsInRecentlyMerged(t *testing.T) {
	// Arrange: it never needed the user, and its branch landed.
	r, _ := newResolver(t)
	requester := workspace("w1", "one")
	merged := hiddenMergeWorkspace(requester.ID)
	merged.MergedAt = at(0)
	merged.Closed = true

	// Act
	r.SetRegistry(registry(requester, merged))

	// Assert
	if row := rowFor(rosterwalk.MergedRows(latest(t, r)), "w2"); row == nil {
		t.Fatal("the landed merge workspace is not under Recently Merged")
	}
}

func TestARevealedMergeWorkspaceHasATab(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	requester := workspace("w1", "one")
	revealed := hiddenMergeWorkspace(requester.ID)
	revealed.MergeRevealed = true

	// Act
	r.SetRegistry(registry(requester, revealed))

	// Assert
	for _, tab := range latest(t, r).GetTabBar().GetTabs() {
		if tab.GetWorkspace().GetId() == "w2" {
			return
		}
	}
	t.Fatal("the revealed merge workspace has no tab")
}

func TestTheRowCarriesTheDurableLastTurnEndedInstant(t *testing.T) {
	cases := []struct {
		name   string
		ended  *time.Time
		closed bool
		want   *int64
	}{
		{name: "a workspace whose turn closed carries the close instant", ended: at(2 * time.Hour), want: millis(at(2 * time.Hour))},
		{name: "a workspace with no closed turn carries none", ended: nil, want: nil},
		{name: "a closed workspace still carries its instant", ended: at(time.Hour), closed: true, want: millis(at(time.Hour))},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			ws := workspace("w1", "one")
			ws.LastTurnEndedAt = tc.ended
			ws.Closed = tc.closed

			// Act.
			r.SetRegistry(registry(ws))

			// Assert.
			got := onlyRow(t, r).GetLastTurnEnded()
			switch {
			case tc.want == nil && got != nil:
				t.Fatalf("last_turn_ended = %v, want unset", got)
			case tc.want != nil && got == nil:
				t.Fatalf("last_turn_ended unset, want %d", *tc.want)
			case tc.want != nil && got.GetAtMs() != *tc.want:
				t.Fatalf("last_turn_ended = %d, want %d", got.GetAtMs(), *tc.want)
			}
		})
	}
}

// TestTheLastTurnEndedInstantIsNotTheSelectionInstant pins that the two
// durable instants are read from their own record fields: selecting a
// workspace moves last_selected and leaves last_turn_ended where the close put
// it.
func TestTheLastTurnEndedInstantIsNotTheSelectionInstant(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.LastTurnEndedAt = at(time.Hour)
	ws.LastSelectedAt = at(3 * time.Hour)

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	if got := onlyRow(t, r).GetLastTurnEnded().GetAtMs(); got != epoch.Add(time.Hour).UnixMilli() {
		t.Fatalf("last_turn_ended = %d, want the close instant, not the selection", got)
	}
}

func TestTheRowCarriesTheDurableCloseRecord(t *testing.T) {
	cases := []struct {
		name    string
		closure *wsm.Closure
		wantArm string // the set `by` arm, "" for an unset field
	}{
		{name: "an open workspace carries none", closure: nil, wantArm: ""},
		{name: "the user's close", closure: &wsm.Closure{At: *at(time.Hour), By: wsm.CloseByUser}, wantArm: "user"},
		{name: "an agent's close", closure: &wsm.Closure{At: *at(time.Hour), By: wsm.CloseByAgent}, wantArm: "agent"},
		{name: "a merge's close", closure: &wsm.Closure{At: *at(time.Hour), By: wsm.CloseByMerge}, wantArm: "merge"},
		{name: "the daemon's close", closure: &wsm.Closure{At: *at(time.Hour), By: wsm.CloseByDaemon}, wantArm: "daemon"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			ws := workspace("w1", "one")
			ws.Closure = tc.closure
			ws.Closed = tc.closure != nil

			// Act.
			r.SetRegistry(registry(ws))

			// Assert.
			got := onlyRow(t, r).GetClosure()
			if tc.wantArm == "" {
				if got != nil {
					t.Fatalf("closure = %v, want unset", got)
				}
				return
			}
			if got == nil {
				t.Fatalf("closure unset, want the %s arm", tc.wantArm)
			}
			if got.GetAtMs() != *millis(at(time.Hour)) {
				t.Fatalf("closure at = %d, want %d", got.GetAtMs(), *millis(at(time.Hour)))
			}
			arm := ""
			switch {
			case got.GetUser() != nil:
				arm = "user"
			case got.GetAgent() != nil:
				arm = "agent"
			case got.GetMerge() != nil:
				arm = "merge"
			case got.GetDaemon() != nil:
				arm = "daemon"
			}
			if arm != tc.wantArm {
				t.Fatalf("closure by = %q, want %q", arm, tc.wantArm)
			}
		})
	}
}

func TestAnUndeclaredCloseCauseFailsHard(t *testing.T) {
	// Arrange.
	r, log := newResolver(t)
	ws := workspace("w1", "one")
	ws.Closed = true
	ws.Closure = &wsm.Closure{At: *at(time.Hour), By: wsm.CloseCause("someone")}

	// Act.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("an undeclared close cause did not fail hard")
		}
		for _, rec := range log.Records() {
			if rec.Operation == "daemon.sidebar.closure" && rec.Level == "error" {
				return
			}
		}
		t.Fatalf("the undeclared cause was not logged at error: %v", log.Records())
	}()
	r.SetRegistry(registry(ws))
}
