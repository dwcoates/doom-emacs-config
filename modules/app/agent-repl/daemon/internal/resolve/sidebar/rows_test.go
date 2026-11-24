package sidebar_test

import (
	"agentrepl/protohelpers/rosterwalk"
	"sync"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/nameprefix"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

func TestRowNamesItsRepository(t *testing.T) {
	tests := []struct {
		name string
		reg  sidebar.Registry
		rows func(*frontendv1.WorkspaceRoster) []*frontendv1.RosterRow
		want []string
	}{
		{
			name: "an open row names its section's repository",
			reg:  registry(workspace("w1", "one")),
			rows: func(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
				return innerRows(rosterwalk.UnsectionedRows(roster.GetRepository().GetSections()[0]))
			},
			want: []string{"alpha"},
		},
		{
			name: "an open child names its own repository, not its section's",
			reg:  crossRepositoryFamily(),
			rows: func(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
				return innerRows(rosterwalk.OpenFamily(rosterwalk.UnsectionedRows(roster.GetRepository().GetSections()[0])[0]))
			},
			// The parent's own-repository child first, then beta's block.
			want: []string{"alpha", "beta", "beta"},
		},
		{
			name: "a hidden row, in no repository section, names its repository",
			reg:  registry(hiddenWorkspace("w-hidden", "hidden")),
			rows: hiddenRows,
			want: []string{"alpha"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistry(tc.reg)

			// Assert.
			var got []string
			for _, row := range tc.rows(latest(t, r)) {
				got = append(got, row.GetRepository().GetName())
			}
			if !equal(got, tc.want) {
				t.Fatalf("repositories = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRowOfAnUnregisteredRepositoryNamesNoneAndIsRecorded(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	stray := hiddenWorkspace("w-stray", "stray")
	stray.Repo = ids.RepoID("repo-missing")

	// Act.
	r.SetRegistry(registry(stray))

	// Assert: the row is still drawn, names no repository, and the violation
	// is recorded with the repository it names.
	rows := hiddenRows(latest(t, r))
	if len(rows) != 1 || rows[0].Repository != nil {
		t.Fatalf("hidden rows = %v, want the stray drawn with no repository", rows)
	}
	rec, ok := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.sidebar.row_repository"})
	if !ok || rec.Context["repo_id"] != "repo-missing" || rec.Context["workspace_id"] != "w-stray" {
		t.Fatalf("record = %v (found %v), want one naming repo-missing and w-stray", rec, ok)
	}
}

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

func TestRowNameWearsTheSharedPrefix(t *testing.T) {
	tests := []struct {
		name  string
		level wsm.Priority
	}{
		{name: "p05", level: wsm.PriorityP05},
		{name: "p1", level: wsm.PriorityP1},
		{name: "p2", level: wsm.PriorityP2},
		{name: "p3", level: wsm.PriorityP3},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			ws := prioritized(workspace("w", "n"), tc.level)

			// Act.
			r.SetRegistry(registry(ws))

			// Assert: the row's prefix IS the one decision, so the sidebar and
			// the topbar cannot draw different marks.
			got := onlyRow(t, r).GetName().GetPrefix()
			if want := nameprefix.Of(ws); want == nil || !proto.Equal(got, want) {
				t.Fatalf("prefix = %v, want %v", got, want)
			}
		})
	}
}

func TestRowNameOfAnUnprioritizedWorkspaceWearsNoPrefix(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w", "n")))

	// Assert: unset is no prefix, never an empty one.
	if got := onlyRow(t, r).GetName().GetPrefix(); got != nil {
		t.Fatalf("an unprioritized workspace's name wore a prefix: %v", got)
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

func TestTheIdleWhenColumnCarriesTheDurableLastTurnEnd(t *testing.T) {
	tests := []struct {
		name  string
		ended *time.Time
		want  *int64
	}{
		{name: "no turn has ended", ended: nil, want: nil},
		{name: "a turn ended long before this daemon started", ended: at(-72 * time.Hour), want: ptr(epoch.Add(-72 * time.Hour).UnixMilli())},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the record alone, no live footer clock.
			ws := workspace(string(theWS), "one")
			ws.LastTurnEndedAt = tc.ended
			r := arrange(t, ws)

			// Act.
			idle := onlyRow(t, r).GetWhen().GetNoTurnRunning()

			// Assert.
			if idle == nil {
				t.Fatalf("when = %v, want no_turn_running", onlyRow(t, r).GetWhen())
			}
			if (idle.LastTurnEndedAtMs == nil) != (tc.want == nil) || (tc.want != nil && *idle.LastTurnEndedAtMs != *tc.want) {
				t.Fatalf("last_turn_ended_at_ms = %v, want %v", idle.LastTurnEndedAtMs, tc.want)
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
	ws.Closed = true
	r := live(t, arrange(t, ws))

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert: that the workspace is done is the more interesting fact.
	row := rowFor(mergedRows(t, r), string(theWS))
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
	row := hiddenRows(latest(t, r))[0]
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
				got = mergedRows(t, r)
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

func TestRowDetailListsTheWorkspacesChildren(t *testing.T) {
	later := epoch.Add(time.Minute)
	parent := workspace("w-parent", "parent")
	child := func(id, name string, place int64, mutate func(*wsm.Workspace)) wsm.Workspace {
		ws := placed(workspace(id, name), place)
		ws.Parent = &parent.ID
		if mutate != nil {
			mutate(&ws)
		}
		return ws
	}
	type listed struct {
		name    string
		receded bool
	}
	tests := []struct {
		name     string
		children []wsm.Workspace
		sessions []wsm.Session
		want     []listed
	}{
		{name: "a childless workspace lists none"},
		{
			name:     "an open child is listed, not receded",
			children: []wsm.Workspace{child("w-a", "a", 1, nil)},
			want:     []listed{{name: "a"}},
		},
		{
			name:     "children are listed in roster order",
			children: []wsm.Workspace{child("w-b", "b", 2, nil), child("w-a", "a", 1, nil)},
			want:     []listed{{name: "a"}, {name: "b"}},
		},
		{
			name:     "a hidden child is listed",
			children: []wsm.Workspace{child("w-a", "a", 1, func(ws *wsm.Workspace) { ws.Hidden, ws.HiddenAt = true, &later })},
			want:     []listed{{name: "a"}},
		},
		{
			name:     "a closed child is listed receded",
			children: []wsm.Workspace{child("w-a", "a", 1, func(ws *wsm.Workspace) { ws.Closed = true })},
			want:     []listed{{name: "a", receded: true}},
		},
		{
			name: "a merged child is listed receded",
			children: []wsm.Workspace{child("w-a", "a", 1, func(ws *wsm.Workspace) {
				ws.Closed, ws.MergedAt = true, &later
			})},
			want: []listed{{name: "a", receded: true}},
		},
		{
			name:     "a child whose session was killed is listed receded",
			children: []wsm.Workspace{child("w-a", "a", 1, nil)},
			sessions: []wsm.Session{{Workspace: "w-a", Terminal: &wsm.SessionTerminal{Kind: "killed", At: epoch}}},
			want:     []listed{{name: "a", receded: true}},
		},
		{
			name: "a hidden merge workspace is not listed",
			children: []wsm.Workspace{child("w-a", "a", 1, func(ws *wsm.Workspace) {
				ws.MergeOf = &wsm.MergeOrigin{}
			})},
		},
		{
			name: "a grandchild is its parent's child, not the grandparent's",
			children: []wsm.Workspace{
				child("w-a", "a", 1, nil),
				child("w-g", "g", 2, func(ws *wsm.Workspace) { id := ids.WorkspaceID("w-a"); ws.Parent = &id }),
			},
			want: []listed{{name: "a"}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(append([]wsm.Workspace{placed(parent, 0)}, tc.children...)...)
			reg.Sessions = tc.sessions

			// Act.
			r.SetRegistry(reg)

			// Assert.
			row := rowFor(rosterwalk.AllRows(latestPush(t, r)), string(parent.ID))
			if row == nil {
				t.Fatal("the roster drew no row for the parent")
			}
			var got []listed
			for _, c := range row.GetDetail().GetChildren() {
				got = append(got, listed{name: c.GetName(), receded: c.GetReceded() != nil})
				if want := "/repos/one/w-" + c.GetName(); c.GetWorkspace().GetDir() != want || c.GetWorkspace().GetId() != "w-"+c.GetName() {
					t.Fatalf("child %q workspace = %v, want its own ref", c.GetName(), c.GetWorkspace())
				}
			}
			if len(got) != len(tc.want) {
				t.Fatalf("children = %v, want %v", got, tc.want)
			}
			for i := range got {
				if got[i] != tc.want[i] {
					t.Fatalf("children = %v, want %v", got, tc.want)
				}
			}
		})
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

func TestAReopenedMergedRowDoesNotRecede(t *testing.T) {
	// Arrange: OPEN BEATS MERGED.
	r, _ := newResolver(t)
	reopened := workspace("w1", "one")
	reopened.MergedAt = at(0)

	// Act.
	r.SetRegistry(registry(reopened))

	// Assert.
	if onlyRow(t, r).GetClosed().GetClosed() {
		t.Fatal("a reopened merged workspace's row receded")
	}
}

func TestRowRecedesWhenTheWorkspaceMerged(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	merged := workspace("w1", "one")
	merged.MergedAt = at(0)
	merged.Closed = true

	// Act.
	r.SetRegistry(registry(merged))

	// Assert.
	row := rowFor(mergedRows(t, r), "w1")
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
// PRESENT is PARTIAL and ABSENT is FULL. Only the user's interaction raises
// the marker, on any arm, and every status change lowers it. These lock both
// halves, because a marker that never clears, a marker that clears on every
// push and a marker the row raises by itself are the three ways this feature
// fails.

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
			r.SetInteracted(theWS, epoch)
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

func TestATurnsOwnEndViewsNothing(t *testing.T) {
	cases := []struct {
		name       string
		act        footer.SessionAct
		how        sidebar.TurnClose
		wantStatus string
	}{
		{name: "a completed clear", act: footer.ActClear, how: wsm.CloseCompleted, wantStatus: "done"},
		{name: "a completed compaction", act: footer.ActCompact, how: wsm.CloseCompleted, wantStatus: "done"},
		{name: "an interrupted prompt", act: footer.ActPrompt, how: wsm.CloseKilled, wantStatus: "interrupted"},
		{name: "a completed prompt", act: footer.ActPrompt, how: wsm.CloseCompleted, wantStatus: "done"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the user interacted while the turn ran.
			r := live(t, arrange(t))
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: tc.act})
			r.SetInteracted(theWS, epoch)

			// Act.
			r.SetTurnEnded(theWS, tc.how)

			// Assert: the end is a status change, so the row is FULL until the
			// user interacts with what it now shows.
			row := onlyRow(t, r)
			if got := statusName(row); got != tc.wantStatus {
				t.Fatalf("status = %q, want %q", got, tc.wantStatus)
			}
			if got := row.GetViewed(); got != nil {
				t.Fatalf("viewed = %v, want unset: a turn's end raises no viewed marker", got)
			}
		})
	}
}

func TestATurnsEndIsRecordedNamingItsContextCut(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActClear})

	// Act.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert.
	for _, rec := range r.surfaces.Records() {
		if rec.Operation == "daemon.sidebar.result_ended" && rec.Context["context_cut"] == "clear" && rec.Context["status"] == "done" {
			return
		}
	}
	t.Fatal("no daemon.sidebar.result_ended record naming the clear")
}

func TestTheBackgroundEndingLowersTheViewedMarker(t *testing.T) {
	// Arrange: the user interacted with the idle_async row.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnDetachedWork(theWS, agent("a1"), detachedWork("work-1"))
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
	r.SetInteracted(theWS, epoch)
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the idle_async row viewed — the arrangement did not interact")
	}

	// Act: the detached work ends, and the row returns to its done.
	r.OnLiveWorkChanged(theWS, sidebar.LiveWorkSet{})

	// Assert.
	row := onlyRow(t, r)
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done", got)
	}
	if got := row.GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want unset: idle_async to done is a status change", got)
	}
}

func TestRowCarriesNoViewedMarkerUntilTheUserInteracts(t *testing.T) {
	// Arrange, Act.
	r := finished(t, arrange(t))

	// Assert: ABSENT is FULL, which is what a row nobody interacted with is.
	if got := onlyRow(t, r).GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want unset for a row nobody has interacted with", got)
	}
}

func TestViewedReportDrawsADoneRowPartial(t *testing.T) {
	// Arrange.
	r := finished(t, arrange(t))

	// Act.
	r.SetInteracted(theWS, epoch)

	// Assert: presence is the mode.
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the marker the editor's report raises on a done row")
	}
}

func TestAnInteractionWhileThinkingDoesNotSurviveIntoDone(t *testing.T) {
	// Arrange: an interaction on a thinking row, which views it.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.OnActivity(theWS, agent("a1"), &conversationv1.AgentActivity{})
	r.SetInteracted(theWS, epoch)
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the thinking row viewed — every arm takes an interaction")
	}

	// Act: the turn finishes.
	r.SetTurnEnded(theWS, wsm.CloseCompleted)

	// Assert: the finished response is new; the user has not seen it yet.
	row := onlyRow(t, r)
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done — the arrangement did not finish the turn", got)
	}
	if got := row.GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want unset: an interaction while thinking is not one with the done row", got)
	}
}

func TestAnInteractionThatViewsTheRowIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.SetInteracted(theWS, epoch)

	// Assert.
	for _, rec := range r.surfaces.Records() {
		if rec.Operation == "daemon.sidebar.row_viewed" && rec.Level == dlog.LevelInfo && rec.Context["status"] == "ready" {
			return
		}
	}
	t.Fatal("no daemon.sidebar.row_viewed record at info naming the ready row")
}

func TestViewedReportIsIdempotent(t *testing.T) {
	// Arrange.
	r := finished(t, arrange(t))
	r.SetInteracted(theWS, epoch)

	// Act: marking an already-viewed workspace changes nothing.
	r.SetInteracted(theWS, epoch)

	// Assert.
	if got := onlyRow(t, r).GetViewed(); got == nil {
		t.Fatal("viewed = unset, want the marker to still stand after a second report")
	}
}

func TestAStatusChangeClearsTheViewedMarker(t *testing.T) {
	// Arrange: a viewed, done row.
	r := finished(t, arrange(t))
	r.SetInteracted(theWS, epoch)

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
	r.SetInteracted(theWS, epoch)
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
	r.SetInteracted(theWS, epoch)

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
	r.SetInteracted(theWS, epoch)

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
	r.SetInteracted(theWS, epoch)

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

func TestALinkBlipLowersTheViewedMarker(t *testing.T) {
	// Arrange: a done row the user has interacted with.
	r := finished(t, arrange(t))
	r.SetInteracted(theWS, epoch)
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Act: the route comes back.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert: done to severed and back are two status changes, so the row is
	// FULL until the user interacts again.
	row := onlyRow(t, r)
	if got := statusName(row); got != "done" {
		t.Fatalf("status = %q, want done — the arrangement did not restore the link", got)
	}
	if got := row.GetViewed(); got != nil {
		t.Fatalf("viewed = %v, want unset after the blip's status changes", got)
	}
}

func TestAnInteractionDrawsEveryStatusPartial(t *testing.T) {
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
		}, want: "idle_async"},
		{name: "done", arrange: func(t *testing.T, r sidebarResolver) { finished(t, r) }, want: "done"},
		{name: "interrupted", arrange: func(t *testing.T, r sidebarResolver) {
			live(t, r)
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			r.SetTurnEnded(theWS, wsm.CloseKilled)
		}, want: "interrupted"},
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

			// Act.
			r.SetInteracted(theWS, epoch)

			// Assert: every arm takes the user's interaction.
			row := onlyRow(t, r)
			if got := statusName(row); got != tc.want {
				t.Fatalf("status = %q after the interaction, want it unchanged at %q", got, tc.want)
			}
			if got := row.GetViewed(); got == nil {
				t.Fatalf("viewed = unset on a %s row, want PARTIAL after the user's interaction", tc.want)
			}
		})
	}
}

func TestRowStatesTheLaterInteractionInstant(t *testing.T) {
	later := epoch.Add(time.Minute)
	cases := []struct {
		name    string
		durable *time.Time
		live    *time.Time
		want    *frontendv1.RosterRowLastInteracted
	}{
		{name: "none recorded states none"},
		{name: "the durable instant alone", durable: &epoch, want: &frontendv1.RosterRowLastInteracted{AtMs: epoch.UnixMilli()}},
		{name: "a live instant newer than the durable one", durable: &epoch, live: &later, want: &frontendv1.RosterRowLastInteracted{AtMs: later.UnixMilli()}},
		{name: "a durable instant newer than the live one", durable: &later, live: &epoch, want: &frontendv1.RosterRowLastInteracted{AtMs: later.UnixMilli()}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			rec := workspace(string(theWS), "one")
			rec.InteractedAt = tc.durable
			r := live(t, arrange(t, rec))

			// Act.
			if tc.live != nil {
				r.SetInteracted(theWS, *tc.live)
			}

			// Assert.
			got := onlyRow(t, r).GetLastInteracted()
			if !proto.Equal(got, tc.want) {
				t.Fatalf("last_interacted = %v, want %v", got, tc.want)
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
	rows := repoOpen(t, latest(t, r))
	if len(rows) != 1 || len(rows[0].GetChildren()) != 1 || rows[0].GetChildren()[0].GetRow().GetName().GetText() != "one-merge" {
		t.Fatalf("rows = %v, want the merge workspace nested under its requester", rowNames(innerRows(rows)))
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
	for _, row := range rosterwalk.AllRows(latestPush(t, r)) {
		if row.GetWorkspace().GetWorkspace().GetId() == "w2" {
			t.Fatalf("the hidden merge workspace has a row: %v", row)
		}
	}
	rows := repoOpen(t, latest(t, r))
	if len(rows) != 1 || len(rows[0].GetChildren()) != 0 {
		t.Fatalf("rows = %v, want the requester alone, with no child", rowNames(innerRows(rows)))
	}
}

func TestAHiddenMergeWorkspaceHasNoTab(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	requester := workspace("w1", "one")

	// Act
	r.SetRegistry(registry(requester, hiddenMergeWorkspace(requester.ID)))

	// Assert
	for _, open := range rosterwalk.OpenRows(latest(t, r)) {
		if open.GetRow().GetWorkspace().GetWorkspace().GetId() == "w2" {
			t.Fatalf("the hidden merge workspace has a tab: %v", open.GetTab())
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
	merged.Closed = true

	// Act
	r.SetRegistry(registry(requester, merged))

	// Assert
	if row := rowFor(mergedRows(t, r), "w2"); row == nil {
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
	for _, open := range rosterwalk.OpenRows(latest(t, r)) {
		if open.GetRow().GetWorkspace().GetWorkspace().GetId() == "w2" && open.GetTab().GetNumbered() != nil {
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
	// The footer is bound first and the roster settled, so the footer's
	// posted status edges are applied before the install: the install's
	// render then runs on this goroutine, whose panic the test recovers,
	// rather than on the goroutine leading a posted batch.
	fan := r.(*fanned)
	fan.bindRegistry(registry(ws))
	r.Settle()

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
	fan.Resolver.SetRegistry(registry(ws))
}

// THE TURN BOUNDARY is the running turn's start, else the durable last end,
// except under a sync merge, which counts from the start of the turn that
// asked for it.
func TestTheRowCarriesTheNewestTurnBoundary(t *testing.T) {
	queued := epoch.Add(3 * time.Hour)
	tests := []struct {
		name       string
		ended      *time.Time
		mergeTurn  *time.Time
		runningAt  *time.Time
		syncMerges bool
		want       *int64
	}{
		{name: "no turn has started", want: nil},
		{name: "an idle workspace counts from its last end", ended: at(time.Hour), want: millis(at(time.Hour))},
		{name: "a running turn counts from its start", ended: at(time.Hour), runningAt: at(2 * time.Hour), want: millis(at(2 * time.Hour))},
		{name: "a sync merge counts from the turn that asked for it", ended: at(2 * time.Hour), mergeTurn: at(time.Hour), syncMerges: true, want: millis(at(time.Hour))},
		{name: "a sync merge counts from that turn while the merge runs its own", ended: at(2 * time.Hour), mergeTurn: at(time.Hour), runningAt: at(4 * time.Hour), syncMerges: true, want: millis(at(time.Hour))},
		{name: "a sync merge with no turn before it has none", ended: at(2 * time.Hour), syncMerges: true, want: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ws := workspace(string(theWS), "one")
			ws.LastTurnEndedAt = tc.ended
			ws.MergeTurnStartedAt = tc.mergeTurn
			r := live(t, arrange(t, ws))
			if tc.syncMerges {
				r.SetMerge(theWS, footer.MergeFacts{State: "merging", QueuedAt: queued, Step: footer.StepTesting})
			}

			// Act.
			if tc.runningAt != nil {
				r.SetTurn(theWS, &footer.TurnStarted{At: *tc.runningAt, Act: footer.ActPrompt})
			}

			// Assert.
			got := onlyRow(t, r).GetLastTurnBoundary()
			switch {
			case tc.want == nil && got != nil:
				t.Fatalf("last_turn_boundary = %v, want unset", got)
			case tc.want != nil && got == nil:
				t.Fatalf("last_turn_boundary unset, want %d", *tc.want)
			case tc.want != nil && got.GetAtMs() != *tc.want:
				t.Fatalf("last_turn_boundary = %d, want %d", got.GetAtMs(), *tc.want)
			}
		})
	}
}

// THE TURN START is the running turn's start while one runs, else the durable
// newest start: it never reads as an end once the turn closes.
func TestTheRowCarriesTheNewestTurnStart(t *testing.T) {
	tests := []struct {
		name      string
		started   *time.Time
		ended     *time.Time
		runningAt *time.Time
		want      *int64
	}{
		{name: "no turn has started", want: nil},
		{name: "an idle workspace counts from its last start, not its end", started: at(time.Hour), ended: at(2 * time.Hour), want: millis(at(time.Hour))},
		{name: "a running turn counts from its start", started: at(time.Hour), ended: at(90 * time.Minute), runningAt: at(2 * time.Hour), want: millis(at(2 * time.Hour))},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ws := workspace(string(theWS), "one")
			ws.LastTurnStartedAt = tc.started
			ws.LastTurnEndedAt = tc.ended
			r := live(t, arrange(t, ws))
			if tc.runningAt != nil {
				r.SetTurn(theWS, &footer.TurnStarted{At: *tc.runningAt, Act: footer.ActPrompt})
			}

			// Act.
			got := onlyRow(t, r).GetLastTurnStarted()

			// Assert.
			switch {
			case tc.want == nil && got != nil:
				t.Fatalf("last_turn_started = %v, want unset", got)
			case tc.want != nil && got == nil:
				t.Fatalf("last_turn_started unset, want %d", *tc.want)
			case tc.want != nil && got.GetAtMs() != *tc.want:
				t.Fatalf("last_turn_started = %d, want %d", got.GetAtMs(), *tc.want)
			}
		})
	}
}

func TestRunningTurnStartReadsOnlyARunningClock(t *testing.T) {
	started, ended := int64(10), int64(20)
	tests := []struct {
		name  string
		clock *frontendv1.FooterClock
		want  *int64
	}{
		{name: "no turn's clock stands", clock: &frontendv1.FooterClock{}, want: nil},
		{name: "a turn runs", clock: &frontendv1.FooterClock{TurnStartedAtMs: &started}, want: &started},
		{name: "the last turn's frozen span", clock: &frontendv1.FooterClock{TurnStartedAtMs: &started, TurnEndedAtMs: &ended}, want: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := sidebar.RunningTurnStart(tc.clock)

			// Assert.
			if (got == nil) != (tc.want == nil) || (got != nil && *got != *tc.want) {
				t.Fatalf("running turn start = %v, want %v", got, tc.want)
			}
		})
	}
}

// The when-column and the turn boundary read a running turn through the one
// helper, so the two fields name the same start while it runs and neither
// reads one once it has ended.
func TestTheWhenColumnAndTheTurnBoundaryAgreeOnTheRunningTurn(t *testing.T) {
	// Arrange.
	ws := workspace(string(theWS), "one")
	ws.LastTurnEndedAt = at(time.Hour)
	r := live(t, arrange(t, ws))
	started := epoch.Add(2 * time.Hour)

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: started, Act: footer.ActPrompt})
	running := onlyRow(t, r)
	r.SetTurnEnded(theWS, wsm.CloseCompleted)
	ended := onlyRow(t, r)

	// Assert.
	if running.GetWhen().GetTurnRunning().GetStartedAtMs() != started.UnixMilli() || running.GetLastTurnBoundary().GetAtMs() != started.UnixMilli() {
		t.Fatalf("running: when = %v, boundary = %v, want both at %d", running.GetWhen(), running.GetLastTurnBoundary(), started.UnixMilli())
	}
	if ended.GetWhen().GetTurnRunning() != nil || ended.GetLastTurnBoundary().GetAtMs() == started.UnixMilli() {
		t.Fatalf("ended: when = %v, boundary = %v, want neither reading the ended turn as running", ended.GetWhen(), ended.GetLastTurnBoundary())
	}
}

// mirroredResolver builds a live resolver whose one workspace stands ready on
// its own footer, recording every durable status change it reports.
func mirroredResolver(t *testing.T) (sidebar.Resolver, *dlog.TestSurfaces, func() int) {
	t.Helper()
	var mu sync.Mutex
	reports := 0
	r, surfaces := newResolver(t, sidebar.WithStatusChangeSink(func(ids.WorkspaceID, time.Time) {
		mu.Lock()
		defer mu.Unlock()
		reports++
	}))
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("arranged status = %q, want ready", got)
	}
	mu.Lock()
	reports = 0
	mu.Unlock()
	return r, surfaces, func() int {
		mu.Lock()
		defer mu.Unlock()
		return reports
	}
}

func TestARowNotYetAdoptedDrawsTheIncumbentsArm(t *testing.T) {
	tests := []struct {
		name string
		// mirrors are the successive incumbent arm maps installed.
		mirrors []map[ids.WorkspaceID]string
		// settle declares the row's facts in after the mirrors.
		settle     bool
		wantStatus string
		// wantDurable is how many status changes reach the durable sink.
		wantDurable int
	}{
		{
			name:       "a mirrored row draws the incumbent's arm over its own footer",
			mirrors:    []map[ids.WorkspaceID]string{{theWS: "thinking"}},
			wantStatus: "thinking",
		},
		{
			name:       "a row the mirror does not name projects its own footer",
			mirrors:    []map[ids.WorkspaceID]string{{"another-workspace": "thinking"}},
			wantStatus: "ready",
		},
		{
			// THE ROW IS ITS OWN AGAIN ONCE ITS FACTS ARE SETTLED (carry.go):
			// until then it keeps the incumbent's arm.
			name:        "a row adopted after the mirror is its own again once its facts are settled",
			mirrors:     []map[ids.WorkspaceID]string{{theWS: "thinking"}, nil},
			settle:      true,
			wantStatus:  "ready",
			wantDurable: 1,
		},
		{
			name:       "a row adopted after the mirror keeps the incumbent's arm until its facts are settled",
			mirrors:    []map[ids.WorkspaceID]string{{theWS: "thinking"}, nil},
			wantStatus: "thinking",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, surfaces, durable := mirroredResolver(t)

			// Act
			for _, arms := range tc.mirrors {
				r.SetIncumbentArms(arms)
			}
			if tc.settle {
				r.SettleFacts(theWS)
			}

			// Assert
			if got := statusName(onlyRow(t, r)); got != tc.wantStatus {
				t.Fatalf("status = %q, want %q", got, tc.wantStatus)
			}
			if got := durable(); got != tc.wantDurable {
				t.Fatalf("durable status changes = %d, want %d: a mirrored arm's move is the incumbent's to keep", got, tc.wantDurable)
			}
			if errs := dlog.FilterRecords(surfaces.Records(), dlog.RecordQuery{Level: "error"}); len(errs) != 0 {
				t.Fatalf("errors = %v, want none: the mirrored arm is the incumbent's own checked claim", errs)
			}
		})
	}
}

func TestAMirroredArmsMoveIsLoggedAsTheRowsArmChange(t *testing.T) {
	// Arrange
	r, surfaces, _ := mirroredResolver(t)

	// Act
	r.SetIncumbentArms(map[ids.WorkspaceID]string{theWS: "thinking"})

	// Assert
	var found bool
	for _, rec := range dlog.FilterRecords(surfaces.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.sidebar.status_arm_changed"}) {
		if rec.Context["arm"] == "thinking" && rec.Context["previous_arm"] == "ready" &&
			rec.Context["cause"] == "daemon.sidebar.set_incumbent_arms" {
			found = true
		}
	}
	if !found {
		t.Fatalf("no status_arm_changed record for the mirrored move: %v", surfaces.Records())
	}
}

// THE EMOJI-ACK GLYPH IS THE DAEMON'S (frontend.v1.WorkspaceNamePrefix.emoji_ack):
// an emoji-ack workspace's row wears the thumbs up in its name's prefix, every
// other row none, and the name itself is the registry's slug either way.
func TestARowsNameWearsTheGlyphExactlyForAnEmojiAckWorkspace(t *testing.T) {
	cases := []struct {
		name      string
		emojiAck  bool
		wantGlyph *frontendv1.WorkspaceNameEmojiAck
	}{
		{name: "an emoji-ack workspace wears the thumbs up", emojiAck: true, wantGlyph: &frontendv1.WorkspaceNameEmojiAck{Text: "👍"}},
		{name: "any other workspace wears none", emojiAck: false, wantGlyph: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ws := workspace(string(theWS), "acked")
			ws.EmojiAck = tc.emojiAck

			// Act.
			r := live(t, arrange(t, ws))

			// Assert.
			name := onlyRow(t, r).GetName()
			if name.GetText() != "acked" {
				t.Fatalf("name text = %q, want the registry name alone", name.GetText())
			}
			if !proto.Equal(name.GetPrefix().GetEmojiAck(), tc.wantGlyph) {
				t.Fatalf("name's emoji-ack glyph = %v, want %v", name.GetPrefix().GetEmojiAck(), tc.wantGlyph)
			}
		})
	}
}

// THE OPEN-OWNED-PULL-REQUEST MARKER IS PRESENCE ALONE
// (frontend.v1.RosterRowOpenOwnedPullRequest): it stands exactly while the
// verbs last said the workspace owns a pull request still open.
func TestOpenOwnedPullRequestMarkerFollowsTheLastFact(t *testing.T) {
	cases := []struct {
		name  string
		facts []bool
		want  bool
	}{
		{name: "a workspace never told carries no marker", facts: nil, want: false},
		{name: "a workspace told it owns an open pull request carries the marker", facts: []bool{true}, want: true},
		{name: "a workspace whose pull request ended loses the marker", facts: []bool{true, false}, want: false},
		{name: "a workspace told twice keeps the marker", facts: []bool{true, true}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t, workspace(string(theWS), "one")))

			// Act.
			for _, open := range tc.facts {
				r.SetOpenOwnedPullRequest(theWS, open)
			}

			// Assert.
			if got := onlyRow(t, r).GetOpenOwnedPullRequest() != nil; got != tc.want {
				t.Fatalf("open_owned_pull_request present = %v, want %v", got, tc.want)
			}
		})
	}
}

// AN UNCHANGED FACT RENDERS NOTHING: telling the roster what it already holds
// publishes no push, so the next push is the next real change.
func TestOpenOwnedPullRequestUnchangedPublishesNothing(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	<-ch

	// Act: the unchanged fact, then a change that must still arrive.
	r.SetOpenOwnedPullRequest(theWS, false)
	r.SetSummary(theWS, "a change")

	// Assert.
	row := rowFor(repoRows(t, (<-ch).GetWorkspaces()), string(theWS))
	if got := row.GetDetail().GetSummary().GetText(); got != "a change" {
		t.Fatalf("next delivery carried %q, want the change: the unchanged fact published a push", got)
	}
}

func TestARowsRecordsGoToItsOwnWorkspacesSink(t *testing.T) {
	// Arrange
	r, surfaces := newResolver(t)
	a, b := workspace("w-a", "a"), workspace("w-b", "b")

	// Act: one render draws both rows.
	r.SetRegistry(registry(a, b))

	// Assert: each row's status record is in its own workspace's sink.
	dirs := map[string]string{"w-a": a.Dir, "w-b": b.Dir}
	seen := map[string]bool{}
	for _, rec := range surfaces.Records() {
		if rec.Operation != "daemon.sidebar.status_decision" {
			continue
		}
		ws, _ := rec.Context["workspace_id"].(string)
		want, ok := dirs[ws]
		if !ok {
			continue
		}
		seen[ws] = true
		if rec.Context["workspace_dir"] != want {
			t.Fatalf("%s's status record = %+v, want it in %s's own sink (workspace_dir %s)", ws, rec, ws, want)
		}
	}
	if !seen["w-a"] || !seen["w-b"] {
		t.Fatalf("status records seen for %v, want both rows'", seen)
	}
}

// THE CONTEXT-HEAT MARKER (frontend.v1.RosterRowContextHeat) stands exactly
// while the context holds sidebar.ContextHeatTokens or more, carrying the window fill.
func TestContextHeatMarkerStandsFromTheThreshold(t *testing.T) {
	cases := []struct {
		name     string
		tokens   int64
		fill     float64
		wantHeat bool
	}{
		{name: "just below the threshold carries no marker", tokens: sidebar.ContextHeatTokens - 1, fill: 0.399, wantHeat: false},
		{name: "exactly the threshold carries the marker", tokens: sidebar.ContextHeatTokens, fill: 0.4, wantHeat: true},
		{name: "far above the threshold carries the marker with its fill", tokens: 900_000, fill: 0.9, wantHeat: true},
		{name: "a cleared context carries no marker", tokens: 0, fill: 0, wantHeat: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := live(t, arrange(t, workspace(string(theWS), "one")))

			// Act.
			r.SetContextFill(theWS, tc.tokens, tc.fill)

			// Assert.
			heat := onlyRow(t, r).GetContextHeat()
			if (heat != nil) != tc.wantHeat {
				t.Fatalf("context_heat present = %v, want %v", heat != nil, tc.wantHeat)
			}
			if heat != nil && heat.GetWindowFill() != tc.fill {
				t.Fatalf("window_fill = %v, want %v", heat.GetWindowFill(), tc.fill)
			}
		})
	}
}

func TestContextHeatMarkerLeavesWhenTheContextShrinks(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t, workspace(string(theWS), "one")))
	r.SetContextFill(theWS, 500_000, 0.5)

	// Act.
	r.SetContextFill(theWS, 20_000, 0.02)

	// Assert.
	if onlyRow(t, r).GetContextHeat() != nil {
		t.Fatal("context_heat still present after the context shrank below the threshold")
	}
}

// A figure that leaves the marker as it stood renders nothing.
func TestContextHeatUnchangedPublishesNothing(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	<-ch

	// Act: two readings below the threshold, then a change that must still arrive.
	r.SetContextFill(theWS, 10_000, 0.01)
	r.SetContextFill(theWS, 20_000, 0.02)
	r.SetSummary(theWS, "a change")

	// Assert.
	row := rowFor(repoRows(t, (<-ch).GetWorkspaces()), string(theWS))
	if got := row.GetDetail().GetSummary().GetText(); got != "a change" {
		t.Fatalf("next delivery carried %q, want the change: an unchanged marker published a push", got)
	}
}

// ---- The DORMANT marker: the session runs no work ------------------------
//
// PRESENT exactly while the session is dormant (wsm.Session.Dormant), which
// the editor reads to hold no page of a dormant workspace off screen.

func TestDormantMarkerFollowsTheSession(t *testing.T) {
	restored := epoch.Add(time.Minute)
	before, after := epoch, epoch.Add(2*time.Minute)
	cases := []struct {
		name        string
		session     wsm.Session
		turnStarted *time.Time
		want        bool
	}{
		{name: "a live session", session: wsm.Session{Workspace: theWS}, want: false},
		{name: "a parked session", session: wsm.Session{Workspace: theWS, Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalParked}}, want: true},
		{name: "a killed session", session: wsm.Session{Workspace: theWS, Terminal: &wsm.SessionTerminal{Kind: "killed", At: epoch}}, want: false},
		{name: "a keep-alive's restore with no turn since", session: wsm.Session{Workspace: theWS, KeepaliveRestoredAt: &restored}, turnStarted: &before, want: true},
		{name: "a keep-alive's restore a turn has started since", session: wsm.Session{Workspace: theWS, KeepaliveRestoredAt: &restored}, turnStarted: &after, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := arrange(t)
			rec := workspace(string(theWS), "one")
			rec.LastTurnStartedAt = tc.turnStarted

			// Act.
			r.SetRegistry(sidebar.Registry{
				View:         wsm.DefaultSidebarView,
				Workspaces:   []wsm.Workspace{rec},
				Repositories: []wsm.Repository{repo},
				Sessions:     []wsm.Session{tc.session},
			})

			// Assert: presence is the fact.
			if got := onlyRow(t, r).GetDormant() != nil; got != tc.want {
				t.Fatalf("dormant present = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAWorkspaceWithNoSessionIsNotMarkedDormant(t *testing.T) {
	// Arrange.
	r := arrange(t)

	// Act.
	row := onlyRow(t, r)

	// Assert.
	if got := row.GetDormant(); got != nil {
		t.Fatalf("dormant = %v, want none for a workspace with no session", got)
	}
}

func TestARepeatedInteractionPublishesOnlyWhenItReordersTheRecency(t *testing.T) {
	cases := []struct {
		name        string
		otherAt     *time.Time
		wantPublish bool
	}{
		{name: "already viewed and the newest publishes nothing", wantPublish: false},
		{name: "outranked by a newer workspace publishes the reorder", otherAt: at(time.Hour), wantPublish: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the row is viewed on its ready.
			other := workspace("w2", "two")
			other.InteractedAt = tc.otherAt
			r := live(t, arrange(t, workspace(string(theWS), "one"), other))
			r.SetInteracted(theWS, epoch)
			before := r.Revision()

			// Act
			r.SetInteracted(theWS, epoch.Add(time.Second))

			// Assert
			if got := r.Revision() != before; got != tc.wantPublish {
				t.Fatalf("published = %v (revision %d -> %d), want %v", got, before, r.Revision(), tc.wantPublish)
			}
		})
	}
}
