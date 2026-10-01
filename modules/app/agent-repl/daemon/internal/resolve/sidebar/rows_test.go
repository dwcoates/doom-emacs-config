package sidebar_test

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
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

func TestRowShowsWhenItWasLastActive(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.LastActivityAt = at(time.Hour)

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	got := onlyRow(t, r).GetWhen().GetActive().GetAtMs()
	if got != epoch.Add(time.Hour).UnixMilli() {
		t.Fatalf("when = %d, want the last-activity instant in millis", got)
	}
}

// TestTheWhenColumnIgnoresLastSelected is the REGRESSION LOCK: the column once
// showed LastSelectedAt, which reset on every SelectWorkspace and made the age
// jump on mere navigation. A row that was selected AFTER its last activity must
// still show the activity instant, never the (later) selection instant.
func TestTheWhenColumnIgnoresLastSelected(t *testing.T) {
	// Arrange — activity an hour in, then a LATER selection.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.LastActivityAt = at(time.Hour)
	ws.LastSelectedAt = at(2 * time.Hour)

	// Act.
	r.SetRegistry(registry(ws))

	// Assert — the active arm, at the activity instant, not the selection one.
	row := onlyRow(t, r)
	if row.GetWhen().GetLastSelected() != nil {
		t.Fatalf("when = %v, want no last-selected arm", row.GetWhen())
	}
	if got := row.GetWhen().GetActive().GetAtMs(); got != epoch.Add(time.Hour).UnixMilli() {
		t.Fatalf("when = %d, want the last-activity instant, never the later selection", got)
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

func TestMergedBeatsLastActiveInTheWhenColumn(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.LastActivityAt = at(0)
	ws.MergedAt = at(time.Hour)

	// Act.
	r.SetRegistry(registry(ws))

	// Assert: that the workspace is done is the more interesting fact.
	row := rowFor(latest(t, r).GetRecentlyMerged().GetRows().GetRows(), "w1")
	if got := row.GetWhen().GetMerged().GetAtMs(); got != epoch.Add(time.Hour).UnixMilli() {
		t.Fatalf("when = %v, want the merge instant", row.GetWhen())
	}
}

func TestTheWhenColumnFallsBackToCreatedForANeverActiveWorkspace(t *testing.T) {
	// Arrange — never merged, never active, but a registered workspace always
	// has a creation time.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	got := onlyRow(t, r).GetWhen().GetCreated().GetAtMs()
	if got != epoch.UnixMilli() {
		t.Fatalf("when = %d, want the creation instant", got)
	}
}

func TestTheWhenColumnIsEmptyWithGenuinelyNoTimeToShow(t *testing.T) {
	// Arrange — a record with no merge, no activity and no creation time: the
	// one case an empty column is correct.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.CreatedAt = time.Time{}

	// Act.
	r.SetRegistry(registry(ws))

	// Assert: an unset oneof is an empty column, never "0ms ago".
	if got := onlyRow(t, r).GetWhen().GetShown(); got != nil {
		t.Fatalf("when = %v, want an unset oneof", got)
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
	row := rowFor(latest(t, r).GetRecentlyMerged().GetRows().GetRows(), "w1")
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

func TestAReadFailedResultStaysReadAcrossALinkBlip(t *testing.T) {
	// Arrange: a turn_failed row the user has read.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.SetTurnEnded(theWS, wsm.CloseFailed)
	r.SetViewed(theWS)
	r.OnLink(theWS, shimclient.LinkRedialing)

	// Act: the route comes back; no new result arrived in between.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert: the result is still read, so the row is PARTIAL again.
	row := onlyRow(t, r)
	if got := statusName(row); got != "turn_failed" {
		t.Fatalf("status = %q, want turn_failed — the arrangement did not restore the link", got)
	}
	if got := row.GetViewed(); got == nil {
		t.Fatal("viewed = unset, want a read failed result to stay read across an arm change that is not a new result")
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

func TestAvailabilityFollowsTheShimLink(t *testing.T) {
	cases := []struct {
		name  string
		links []shimclient.LinkState
		want  string
	}{
		{name: "no link was ever seen", links: nil, want: "pending"},
		{name: "the link is still dialing", links: []shimclient.LinkState{shimclient.LinkDialing}, want: "pending"},
		{name: "the link connected", links: []shimclient.LinkState{shimclient.LinkConnected}, want: "available"},
		{name: "the link died before it ever connected", links: []shimclient.LinkState{shimclient.LinkDialing, shimclient.LinkDead}, want: "unavailable"},
		{name: "a link that connected and then died", links: []shimclient.LinkState{shimclient.LinkConnected, shimclient.LinkDead}, want: "available"},
		{name: "a failed start that a later start brought up", links: []shimclient.LinkState{shimclient.LinkDead, shimclient.LinkConnected}, want: "available"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r := arrange(t)

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
