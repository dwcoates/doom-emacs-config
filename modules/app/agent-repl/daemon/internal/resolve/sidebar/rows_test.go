package sidebar_test

import (
	"testing"
	"time"

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

func TestRowShowsWhenItWasLastSelected(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.LastSelectedAt = at(time.Hour)

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	got := onlyRow(t, r).GetWhen().GetLastSelected().GetAtMs()
	if got != epoch.Add(time.Hour).UnixMilli() {
		t.Fatalf("when = %d, want the selection instant in millis", got)
	}
}

func TestMergedBeatsLastSelectedInTheWhenColumn(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ws := workspace("w1", "one")
	ws.LastSelectedAt = at(0)
	ws.MergedAt = at(time.Hour)

	// Act.
	r.SetRegistry(registry(ws))

	// Assert: that the workspace is done is the more interesting fact.
	row := rowFor(latest(t, r).GetRecentlyMerged().GetRows().GetRows(), "w1")
	if got := row.GetWhen().GetMerged().GetAtMs(); got != epoch.Add(time.Hour).UnixMilli() {
		t.Fatalf("when = %v, want the merge instant", row.GetWhen())
	}
}

func TestTheWhenColumnIsEmptyWithNothingToShow(t *testing.T) {
	// Arrange, Act.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

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

func TestRowDoesNotRecedeWhileTheWorkspaceIsOpen(t *testing.T) {
	// Arrange, Act.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if onlyRow(t, r).GetClosed().GetClosed() {
		t.Fatal("an open workspace's row receded")
	}
}
