package sidebar_test

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
)

func TestNewRefusesWithoutLogSurfaces(t *testing.T) {
	// Arrange, Act.
	_, err := sidebar.New(testColors(), nil)

	// Assert.
	if err == nil {
		t.Fatal("sidebar.New accepted nil log surfaces")
	}
}

func TestNewRefusesAnEmptyRosterStatusTable(t *testing.T) {
	// Arrange, Act.
	_, err := sidebar.New(vocab.RenderColors{}, dlog.NewTestSurfaces())

	// Assert.
	if err == nil {
		t.Fatal("sidebar.New accepted render colors with no roster_status table")
	}
}

func TestNewRefusesARosterStatusTableMissingAnArm(t *testing.T) {
	// Arrange.
	colors := testColors()
	delete(colors.RosterStatus, "vendor_blocked")

	// Act.
	_, err := sidebar.New(colors, dlog.NewTestSurfaces())

	// Assert: an unpainted dot fails here, never on the wire.
	if err == nil {
		t.Fatal("sidebar.New accepted a table with no color for an arm it emits")
	}
}

func TestNewRefusesARosterStatusTableWithASurplusRow(t *testing.T) {
	// Arrange.
	colors := testColors()
	colors.RosterStatus["invented"] = "grey"

	// Act.
	_, err := sidebar.New(colors, dlog.NewTestSurfaces())

	// Assert: a colored state nothing can reach means the table has drifted.
	if err == nil {
		t.Fatal("sidebar.New accepted a table coloring a state that is no arm")
	}
}

func TestNewRefusesAMergeGlyphsTableMissingAnArm(t *testing.T) {
	// Arrange.
	colors := testColors()
	delete(colors.MergeGlyphs, "merging")

	// Act.
	_, err := sidebar.New(colors, dlog.NewTestSurfaces())

	// Assert.
	if err == nil {
		t.Fatal("sidebar.New accepted a table with no glyph for a merge arm")
	}
}

func TestRosterPublishesNothingBeforeAnyRegistry(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act: a live frame with no registry behind it.
	r.OnLink(theWS, shimclient.LinkConnected)

	// Assert: a roster built from live frames alone would name no workspaces.
	if _, ok := r.Topic().Latest(); ok {
		t.Fatal("the roster published before any registry")
	}
}

func TestRosterPublishesOnTheFirstRegistry(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if _, ok := r.Topic().Latest(); !ok {
		t.Fatal("the roster published nothing on its first registry")
	}
}

func TestRosterPublishesTheWholeRosterOnEveryPush(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert: always whole, never a delta.
	roster := latest(t, r)
	switch {
	case roster.GetRepository() == nil:
		t.Fatal("the push carried no repository grouping")
	case roster.GetTask() == nil:
		t.Fatal("the push carried no task grouping")
	case roster.GetRecentlyMerged() == nil:
		t.Fatal("the push carried no recently-merged section")
	}
}

func TestRosterRepublishesWhenAFactChanges(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	r.SetRegistry(registry(workspace("w1", "one")))
	<-ch

	// Act.
	r.SetSummary(theWS, "a new prompt")

	// Assert.
	got := rowFor(repoRows(t, <-ch), "w1").GetDetail().GetSummary().GetText()
	if got != "a new prompt" {
		t.Fatalf("republished summary = %q, want the change", got)
	}
}

func TestRosterDoesNotRepublishAnIdenticalRender(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	reg := registry(workspace("w1", "one"))
	r.SetRegistry(reg)
	<-ch

	// Act: the same registry again, then a change that must still arrive.
	r.SetRegistry(reg)
	r.SetSummary(theWS, "a change")

	// Assert: the duplicate never reached the wire.
	got := rowFor(repoRows(t, <-ch), "w1").GetDetail().GetSummary().GetText()
	if got != "a change" {
		t.Fatalf("next delivery carried %q, want the change — a duplicate reached the wire", got)
	}
}

func TestOneStreamServesEveryWebview(t *testing.T) {
	// Arrange: the roster carries no workspace, so there is one topic to have.
	r, _ := newResolver(t)
	first := subscribe(t, r)
	second := subscribe(t, r)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if len(repoRows(t, <-first)) != 1 || len(repoRows(t, <-second)) != 1 {
		t.Fatal("the two subscribers did not receive the same roster")
	}
}

func TestANukedWorkspaceLeavesTheRoster(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))

	// Act: the registry no longer carries it.
	r.SetRegistry(registry(workspace("w2", "two")))

	// Assert.
	if rowFor(repoRows(t, latest(t, r)), "w1") != nil {
		t.Fatal("a nuked workspace kept its row")
	}
}

func TestANukedWorkspaceLosesItsSelection(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))
	r.SetSelected(theWS)

	// Act.
	r.SetRegistry(registry(workspace("w2", "two")))

	// Assert.
	if got := latest(t, r).GetCurrent(); got != nil {
		t.Fatalf("current = %v, want no selection once the workspace is gone", got)
	}
}

func TestANukedWorkspacesSessionFactsAreForgotten(t *testing.T) {
	// Arrange: a workspace whose session was live, then nuked and re-registered.
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Act.
	r.SetRegistry(registry(workspace("w2", "two")))
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert: a lingering accumulation would resurrect the old row's dot.
	if got := statusName(rowFor(repoRows(t, latest(t, r)), "w1")); got != "none" {
		t.Fatalf("status = %q, want none — the live half outlived the nuke", got)
	}
}

func TestRosterRecordsAFrameForAWorkspaceTheRegistryDoesNotCarry(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))

	// Act.
	r.OnLink(ids.WorkspaceID("w-stranger"), shimclient.LinkConnected)

	// Assert.
	var found bool
	for _, rec := range surfaces.Records() {
		if rec.Context["invariant_violation"] == "roster fact for a workspace the registry does not carry" {
			found = true
		}
	}
	if !found {
		t.Fatal("a frame for an unregistered workspace was not recorded as an invariant violation")
	}
}

func TestASessionUpdateWithNoArmChangesNothing(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnSessionUpdate(theWS, &conversationv1.SessionUpdate{})

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want the row unchanged", got)
	}
}

func TestANilSessionUpdateChangesNothing(t *testing.T) {
	// Arrange.
	r := live(t, arrange(t))

	// Act.
	r.OnSessionUpdate(theWS, nil)

	// Assert.
	if got := statusName(onlyRow(t, r)); got != "ready" {
		t.Fatalf("status = %q, want the row unchanged", got)
	}
}
