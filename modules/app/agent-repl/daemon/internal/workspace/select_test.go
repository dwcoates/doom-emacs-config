package workspace

import (
	"agent-repld/internal/sourcescan"
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

func TestSelectRecordsTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if f.db.current == nil || *f.db.current != "w1" || !f.db.currentAt.Equal(fixedNow) {
		t.Fatalf("current = (%v, %v), want w1 at %v", f.db.current, f.db.currentAt, fixedNow)
	}
}

func TestSelectClearsTheAttentionMarker(t *testing.T) {
	// Arrange: the user has now looked at whatever raised it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.attention["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("Select() left the attention marker set")
	}
}

func TestSelectTellsTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.selected) != 1 || f.sidebar.selected[0] != "w1" {
		t.Fatalf("roster selections = %v, want w1 once", f.sidebar.selected)
	}
}

func TestSelectRaisesTheCacheKeepersVisibilityEdges(t *testing.T) {
	tests := []struct {
		name     string
		previous string
		want     []ids.WorkspaceID
	}{
		{name: "the first selection reconciles the selected workspace", want: []ids.WorkspaceID{"w2"}},
		{name: "a switch reconciles the selected and the deselected workspace", previous: "w1", want: []ids.WorkspaceID{"w2", "w1"}},
		{name: "a re-selection changes no visibility", previous: "w2", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.workspace("w2", t.TempDir())
			if tt.previous != "" {
				previous := ids.WorkspaceID(tt.previous)
				f.db.current = &previous
			}

			// Act.
			if err := f.verbs.Select(context.Background(), "w2", RecordSelection); err != nil {
				t.Fatalf("Select: %v", err)
			}

			// Assert.
			if !slices.Equal(f.visibilityEdges, tt.want) {
				t.Fatalf("visibility edges = %v, want %v", f.visibilityEdges, tt.want)
			}
		})
	}
}

func TestSelectIsIdempotent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", *f.db.current)
	}
}

func TestSelectRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.Select(context.Background(), "nope", RecordSelection)

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestSetPriorityRecordsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	priority := wsm.PriorityP05

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", &priority); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	got := f.db.priorities["w1"]
	if got == nil || *got != wsm.PriorityP05 {
		t.Fatalf("recorded priority = %v, want P05", got)
	}
}

func TestSetPriorityClearsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", nil); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	got, recorded := f.db.priorities["w1"]
	if !recorded || got != nil {
		t.Fatalf("recorded priority = (%v, %v), want a recorded clear", got, recorded)
	}
}

func TestSetPriorityRepublishesTheRoster(t *testing.T) {
	// Arrange: the row draws the priority badge, so a priority change is a
	// roster push.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	priority := wsm.PriorityP2

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", &priority); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

// TestSelectTellsTheRosterAsOnePush pins that a select reaches the roster as
// ONE mutation carrying both the refreshed registry and the selection, so
// every client receives one roster per switch rather than two.
func TestSelectTellsTheRosterAsOnePush(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if want := []string{"registry+selected"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
}

// TestSelectWithAnUnreadableRegistryStillTellsTheRosterTheSelection pins the
// failure arm: a registry read that fails is recorded at ERROR, and the
// selection still reaches the roster on its own push.
func TestSelectWithAnUnreadableRegistryStillTellsTheRosterTheSelection(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.listWorkspacesErr = errFake

	// Act.
	_ = f.verbs.Select(context.Background(), "w1", RecordSelection)

	// Assert.
	if want := []string{"selected"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
	if got := recordLevel(f, "could not read the workspaces, repositories and subsections for the roster"); got != "error" {
		t.Fatalf("the failed registry read was recorded at %q, want error", got)
	}
}

// TestSelectPublishesARegistryCarryingTheSelection pins that the refreshed
// registry is the one WSM holds AFTER the write, not before it.
func TestSelectPublishesARegistryCarryingTheSelection(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	got := f.sidebar.registries[0].Current
	if got == nil || *got != "w1" {
		t.Fatalf("the published registry's current = %v, want w1", got)
	}
}

// TestSelectPublishesARegistryCarryingTheSelectionInstant pins that the roster
// push a selection makes carries the new durable last-selected instant, which
// is what clients order most-recently-selected by.
func TestSelectPublishesARegistryCarryingTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	for _, ws := range f.sidebar.registries[0].Workspaces {
		if ws.ID != "w1" {
			continue
		}
		if ws.LastSelectedAt == nil || !ws.LastSelectedAt.Equal(fixedNow) {
			t.Fatalf("the published last-selected instant = %v, want %v", ws.LastSelectedAt, fixedNow)
		}
		return
	}
	t.Fatalf("the published registry carries no w1")
}

// TestReselectingDoesNotRestampTheSelectionInstant pins the idempotence the
// contract states: re-selecting the workspace already being looked at is a
// success that CHANGES NO VIEW. The instant answers "when did the user last
// switch to this", and the user did not switch.
func TestReselectingDoesNotRestampTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	f.db.currentAt = fixedNow.Add(-time.Hour)

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if !f.db.currentAt.Equal(fixedNow.Add(-time.Hour)) {
		t.Fatalf("the selection instant = %v, want the re-selection to leave it alone", f.db.currentAt)
	}
}

// TestReselectingStillClearsTheAttentionMarker pins that idempotence does not
// swallow the clear: a notification can raise the marker while the workspace
// IS current, and looking at it again is what clears it.
func TestReselectingStillClearsTheAttentionMarker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	f.db.attention["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("re-selecting left the attention marker set")
	}
}

// recordLevel returns the level of the first captured record whose message
// equals want, or "" when none matches.
func recordLevel(f *fixture, want string) string {
	r, _ := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Message: want})
	return r.Level
}

func TestSelectLogsTheSwitchReceiptAtInfo(t *testing.T) {
	// Arrange: a switch to a workspace that is not already current.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: a switch must leave an info-level trace in production logs.
	if got := recordLevel(f, "selected the workspace"); got != "info" {
		t.Fatalf("the select receipt is %q, want info", got)
	}
}

func TestSelectLogsTheReselectionReceiptAtInfo(t *testing.T) {
	// Arrange: the workspace is already current, so the next select re-selects it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("first Select: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if got := recordLevel(f, "the workspace was already current; the selection instant stands"); got != "info" {
		t.Fatalf("the reselection receipt is %q, want info", got)
	}
}

// ---- FollowStoredSelection -------------------------------------------------

// followFixture arranges a roster naming w1 while WSM stores STORED, the state
// a peer daemon's select leaves this daemon in.
func followFixture(t *testing.T, stored *ids.WorkspaceID) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.sidebar.selected = []ids.WorkspaceID{"w1"}
	f.db.current = stored
	return f
}

func storedSelection(ws ids.WorkspaceID) *ids.WorkspaceID { return &ws }

func TestFollowStoredSelectionPushesTheStoredSelection(t *testing.T) {
	// Arrange: the peer daemon stamped w2.
	f := followFixture(t, storedSelection("w2"))

	// Act.
	if err := f.verbs.FollowStoredSelection(context.Background()); err != nil {
		t.Fatalf("FollowStoredSelection: %v", err)
	}

	// Assert: one push, carrying the registry and the stored selection.
	if want := []string{"registry+selected"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
	if got, _ := f.sidebar.Selected(); got != "w2" {
		t.Fatalf("roster selection = %q, want w2", got)
	}
}

func TestFollowStoredSelectionPushesNothingWhenTheRosterAgrees(t *testing.T) {
	// Arrange: the roster already names what WSM stores.
	f := followFixture(t, storedSelection("w1"))

	// Act.
	if err := f.verbs.FollowStoredSelection(context.Background()); err != nil {
		t.Fatalf("FollowStoredSelection: %v", err)
	}

	// Assert.
	if len(f.sidebar.calls) != 0 {
		t.Fatalf("roster calls = %v, want none", f.sidebar.calls)
	}
}

func TestFollowStoredSelectionPushesNothingWhenWSMStoresNone(t *testing.T) {
	// Arrange.
	f := followFixture(t, nil)

	// Act.
	if err := f.verbs.FollowStoredSelection(context.Background()); err != nil {
		t.Fatalf("FollowStoredSelection: %v", err)
	}

	// Assert.
	if len(f.sidebar.calls) != 0 {
		t.Fatalf("roster calls = %v, want none", f.sidebar.calls)
	}
}

func TestFollowStoredSelectionFailsWhenTheStoredSelectionCannotBeRead(t *testing.T) {
	// Arrange.
	f := followFixture(t, storedSelection("w2"))
	f.db.currentErr = errFake

	// Act.
	err := f.verbs.FollowStoredSelection(context.Background())

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("FollowStoredSelection error = %v, want it to wrap %v", err, errFake)
	}
	if got := recordLevel(f, "could not read the stored selection"); got != "error" {
		t.Fatalf("the failed read was recorded at %q, want error", got)
	}
}

func TestFollowStoredSelectionFailsWithoutAPushWhenTheRegistryCannotBeRead(t *testing.T) {
	// Arrange.
	f := followFixture(t, storedSelection("w2"))
	f.db.listWorkspacesErr = errFake

	// Act.
	err := f.verbs.FollowStoredSelection(context.Background())

	// Assert: the failure is answered and nothing half-built reaches the roster.
	if err == nil {
		t.Fatal("FollowStoredSelection answered nil for an unreadable registry")
	}
	if len(f.sidebar.calls) != 0 {
		t.Fatalf("roster calls = %v, want none", f.sidebar.calls)
	}
}

func TestFollowStoredSelectionLogsTheFollowAtInfo(t *testing.T) {
	// Arrange.
	f := followFixture(t, storedSelection("w2"))

	// Act.
	if err := f.verbs.FollowStoredSelection(context.Background()); err != nil {
		t.Fatalf("FollowStoredSelection: %v", err)
	}

	// Assert: a roster that moved on another daemon's word leaves a trace.
	if got := recordLevel(f, "the roster took the selection another daemon stored"); got != "info" {
		t.Fatalf("the follow receipt is %q, want info", got)
	}
}

// ---- MarkViewed ------------------------------------------------------------

func TestMarkViewedTellsTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.MarkViewed(context.Background(), "w1"); err != nil {
		t.Fatalf("MarkViewed: %v", err)
	}

	// Assert.
	if len(f.sidebar.viewed) != 1 || f.sidebar.viewed[0] != "w1" {
		t.Fatalf("roster viewed reports = %v, want w1 once", f.sidebar.viewed)
	}
}

func TestMarkViewedWritesNoDurableRecord(t *testing.T) {
	// Arrange: the display mode is a VIEW fact and must not outlive a restart.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.MarkViewed(context.Background(), "w1"); err != nil {
		t.Fatalf("MarkViewed: %v", err)
	}

	// Assert: looking at a workspace is not selecting it either.
	if f.db.current != nil {
		t.Fatalf("current = %v, want MarkViewed to have recorded no selection", f.db.current)
	}
}

func TestMarkViewedIsIdempotent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.MarkViewed(context.Background(), "w1"); err != nil {
		t.Fatalf("first MarkViewed: %v", err)
	}
	if err := f.verbs.MarkViewed(context.Background(), "w1"); err != nil {
		t.Fatalf("second MarkViewed: %v", err)
	}

	// Assert: a second report is a success that changes nothing downstream.
	if len(f.sidebar.viewed) != 2 {
		t.Fatalf("roster viewed reports = %v, want both reports forwarded", f.sidebar.viewed)
	}
}

func TestMarkViewedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.MarkViewed(context.Background(), "nope")

	// Assert: an unregistered workspace has no row to mark.
	if err == nil {
		t.Fatal("MarkViewed() accepted a workspace the registry does not hold")
	}
	if len(f.sidebar.viewed) != 0 {
		t.Fatalf("roster viewed reports = %v, want none for a refused mark", f.sidebar.viewed)
	}
}

func TestSelectReturnsTheFeedToItsTailOnASwitch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select w1: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w2", RecordSelection); err != nil {
		t.Fatalf("Select w2: %v", err)
	}

	// Assert.
	if !slices.Equal(f.host.tailReturns, []ids.WorkspaceID{"w1", "w2"}) {
		t.Fatalf("tail returns = %v, want [w1 w2]", f.host.tailReturns)
	}
}

func TestReselectingLeavesTheFeedWhereItIs(t *testing.T) {
	// Arrange: a re-selection is Emacs re-asserting, never a switch.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("first Select: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if !slices.Equal(f.host.tailReturns, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("tail returns = %v, want [w1] (the first select only)", f.host.tailReturns)
	}
}

func TestSelectOnADaemonStandingDownLeavesTheSelectionStanding(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.standingDown = true

	// Act.
	_ = f.verbs.Select(context.Background(), "w1", RecordSelection)

	// Assert: the next daemon serves this selection.
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", f.db.current)
	}
}

func TestSetHiddenRecordsTheFlagAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name   string
		hidden bool
	}{{"hide", true}, {"show", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			prior := f.db.workspaces["w1"]
			prior.Hidden = !tt.hidden
			f.db.workspaces["w1"] = prior

			// Act
			err := f.verbs.SetHidden(context.Background(), "w1", tt.hidden)

			// Assert
			if err != nil {
				t.Fatalf("SetHidden: %v", err)
			}
			if got := f.db.workspaces["w1"].Hidden; got != tt.hidden {
				t.Fatalf("recorded hidden = %v, want %v", got, tt.hidden)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
		})
	}
}

func TestSetHiddenRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.SetHidden(context.Background(), "nope", true)

	// Assert
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refusal", len(f.sidebar.registries))
	}
}

func TestSetHiddenSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setHiddenErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.SetHidden(context.Background(), "w1", true)

	// Assert
	if err == nil {
		t.Fatal("a failed hidden write was answered as success")
	}
	if !loggedAtError(f, opSetHidden) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed write", len(f.sidebar.registries))
	}
	if len(f.visibilityEdges) != 0 {
		t.Fatalf("visibility edges = %v, want none for a failed write", f.visibilityEdges)
	}
}

func TestRecordHiddenWritesRepublishesAndRaisesTheEdge(t *testing.T) {
	tests := []struct {
		name   string
		hidden bool
	}{
		{name: "hiding", hidden: true},
		{name: "showing", hidden: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			prior := f.db.workspaces["w1"]
			prior.Hidden = !tt.hidden
			f.db.workspaces["w1"] = prior

			// Act
			err := f.verbs.(*verbs).recordHidden(context.Background(), f.log.Global(), "w1", tt.hidden)

			// Assert
			if err != nil {
				t.Fatalf("recordHidden: %v", err)
			}
			if got := f.db.workspaces["w1"].Hidden; got != tt.hidden {
				t.Fatalf("recorded hidden = %v, want %v", got, tt.hidden)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
			if !slices.Equal(f.visibilityEdges, []ids.WorkspaceID{"w1"}) {
				t.Fatalf("visibility edges = %v, want the workspace", f.visibilityEdges)
			}
		})
	}
}

func TestRecordHiddenSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setHiddenErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.(*verbs).recordHidden(context.Background(), f.log.Global(), "w1", false)

	// Assert
	if err == nil {
		t.Fatal("a failed hidden write was answered as success")
	}
	if !loggedAtError(f, opSetHidden) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 || len(f.visibilityEdges) != 0 {
		t.Fatalf("republications %d, edges %v; want none for a failed write", len(f.sidebar.registries), f.visibilityEdges)
	}
}

// TestTheHiddenFlagHasOneWrite fails when a verb writes the hidden flag by
// hand instead of through recordHidden, which would skip the roster push or
// the cache keeper's edge.
func TestTheHiddenFlagHasOneWrite(t *testing.T) {
	// Arrange
	files := sourcescan.Production(t)

	// Act
	var sites []string
	for _, file := range files {
		for range strings.Count(string(file.Source), "v.deps.DB.SetHidden(") {
			sites = append(sites, file.Name)
		}
	}

	// Assert
	if len(sites) != 1 || sites[0] != "select.go" {
		t.Fatalf("the hidden flag is written at %v, want only recordHidden in select.go", sites)
	}
}

func TestSwapSwapsTheWorkspacesAndRepublishes(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())

	// Act.
	if err := f.verbs.Swap(context.Background(), "w1", "w2"); err != nil {
		t.Fatalf("Swap: %v", err)
	}

	// Assert.
	if want := [][2]ids.WorkspaceID{{"w1", "w2"}}; !slices.Equal(f.db.swaps, want) {
		t.Fatalf("swaps = %v, want %v", f.db.swaps, want)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	if !hasRecord(f, dlog.LevelInfo, opSwap) {
		t.Fatal("the swap was not recorded at info")
	}
}

func TestSwapRefusesAWorkspaceThisDaemonDoesNotServe(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act.
	err := f.verbs.Swap(context.Background(), "w1", "w2")

	// Assert.
	asRefusal(t, err, ArmTransferringAway)
	if len(f.db.swaps) != 0 {
		t.Fatalf("a refused swap wrote %v", f.db.swaps)
	}
}

func TestSwapRefusesTwoWorkspacesThatAreNotSiblings(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.db.swapErr = wsm.ErrNotSiblings

	// Act.
	err := f.verbs.Swap(context.Background(), "w1", "w2")

	// Assert.
	asRefusal(t, err, ArmNotSiblings)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a refused swap republished %d times", len(f.sidebar.registries))
	}
	if loggedAtError(f, opSwap) {
		t.Fatal("a refused swap was logged at error")
	}
}

func TestSwapRefusesTwoRepositories(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.db.swapErr = wsm.ErrOtherRepository

	// Act.
	err := f.verbs.Swap(context.Background(), "w1", "w2")

	// Assert.
	asRefusal(t, err, ArmOtherRepository)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a refused swap republished %d times", len(f.sidebar.registries))
	}
	if loggedAtError(f, opSwap) {
		t.Fatal("a refused swap was logged at error")
	}
}

func TestSwapSurfacesAStoreFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.db.swapErr = errors.New("disk on fire")

	// Act.
	err := f.verbs.Swap(context.Background(), "w1", "w2")

	// Assert.
	if err == nil || !errors.Is(err, f.db.swapErr) {
		t.Fatalf("error = %v, want the store's failure", err)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a failed swap republished %d times", len(f.sidebar.registries))
	}
	if !loggedAtError(f, opSwap) {
		t.Fatal("the store failure was not logged at error")
	}
}

func TestOwnedPairNamesTheTargetOnTheWorkspacesLogger(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())

	// Act.
	log, err := f.verbs.(*verbs).ownedPair(context.Background(), "MoveWorkspace", "w1", "w2")
	log.Info(opMove, "probe", nil)

	// Assert.
	if err != nil {
		t.Fatalf("ownedPair: %v", err)
	}
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Operation: opMove})
	if !ok || rec.Context["target"] != "w2" {
		t.Fatalf("probe record = %+v (found %v), want it to name target w2", rec, ok)
	}
}

func TestOwnedPairRefusesATargetThisDaemonDoesNotKnow(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	_, err := f.verbs.(*verbs).ownedPair(context.Background(), "MoveWorkspace", "w1", "absent")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestMoveMovesTheWorkspaceAndRepublishes(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())

	// Act.
	if err := f.verbs.Move(context.Background(), "w1", "w2"); err != nil {
		t.Fatalf("Move: %v", err)
	}

	// Assert.
	if want := [][2]ids.WorkspaceID{{"w1", "w2"}}; !slices.Equal(f.db.moves, want) {
		t.Fatalf("moves = %v, want %v", f.db.moves, want)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	if !hasRecord(f, dlog.LevelInfo, opMove) {
		t.Fatal("the move was not recorded at info")
	}
}

func TestMoveRefusesAWorkspaceThisDaemonDoesNotServe(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act.
	err := f.verbs.Move(context.Background(), "w1", "w2")

	// Assert.
	asRefusal(t, err, ArmTransferringAway)
	if len(f.db.moves) != 0 {
		t.Fatalf("a refused move wrote %v", f.db.moves)
	}
}

func TestMoveTurnsTheStoresRefusalsIntoArms(t *testing.T) {
	tests := []struct {
		name  string
		store error
		arm   string
	}{
		{name: "two repositories", store: wsm.ErrOtherRepository, arm: ArmOtherRepository},
		{name: "not siblings", store: wsm.ErrNotSiblings, arm: ArmNotSiblings},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.workspace("w2", t.TempDir())
			f.db.moveErr = tt.store

			// Act.
			err := f.verbs.Move(context.Background(), "w1", "w2")

			// Assert.
			asRefusal(t, err, tt.arm)
			if len(f.sidebar.registries) != 0 {
				t.Fatalf("a refused move republished %d times", len(f.sidebar.registries))
			}
			if loggedAtError(f, opMove) {
				t.Fatal("a refused move was logged at error")
			}
		})
	}
}

func TestMoveSurfacesAStoreFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.db.moveErr = errors.New("disk on fire")

	// Act.
	err := f.verbs.Move(context.Background(), "w1", "w2")

	// Assert.
	if err == nil || !errors.Is(err, f.db.moveErr) {
		t.Fatalf("error = %v, want the store's failure", err)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a failed move republished %d times", len(f.sidebar.registries))
	}
	if !loggedAtError(f, opMove) {
		t.Fatal("the store failure was not logged at error")
	}
}

func TestSetHiddenTellsTheCacheKeeper(t *testing.T) {
	tests := []struct {
		name   string
		hidden bool
	}{
		{name: "a hide", hidden: true},
		{name: "a show", hidden: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act
			if err := f.verbs.SetHidden(context.Background(), "w1", tt.hidden); err != nil {
				t.Fatalf("SetHidden: %v", err)
			}

			// Assert
			if !slices.Equal(f.visibilityEdges, []ids.WorkspaceID{"w1"}) {
				t.Fatalf("visibility edges = %v, want the workspace", f.visibilityEdges)
			}
		})
	}
}

// TestHidingStandsNothingDownAtOnce pins the owner's request of 2026-10-13: a
// hidden workspace's cache is left to lapse, and the cache keeper stands its
// session down only when it does.
func TestHidingStandsNothingDownAtOnce(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	if err := f.verbs.SetHidden(context.Background(), "w1", true); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Assert
	if len(f.standDowns) != 0 {
		t.Fatalf("stand-downs = %v, want none at the hide", f.standDowns)
	}
}

func TestShowingAWorkspaceStartsNothingAndStandsNothingDown(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	if err := f.verbs.SetHidden(context.Background(), "w1", false); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Assert
	if len(f.standDowns) != 0 {
		t.Fatalf("stand-downs = %v, want none for a show", f.standDowns)
	}
}

func TestHideLandingTarget(t *testing.T) {
	at := func(minute int) *time.Time {
		v := time.Date(2026, 10, 20, 12, minute, 0, 0, time.UTC)
		return &v
	}
	tab := func(id ids.WorkspaceID, withheld wsm.Withholding, selected *time.Time) sidebar.Tab {
		return sidebar.Tab{Workspace: wsm.Workspace{ID: id, LastSelectedAt: selected}, Withheld: withheld}
	}
	drawn := func(id ids.WorkspaceID) sidebar.Tab { return tab(id, wsm.Drawn, nil) }
	selected := func(id ids.WorkspaceID, minute int) sidebar.Tab { return tab(id, wsm.Drawn, at(minute)) }
	tests := []struct {
		name      string
		tabs      []sidebar.Tab
		want      ids.WorkspaceID
		wantBy    hideLanding
		wantFound bool
	}{
		{
			name:      "the most recently selected tab wins over the first and the neighbours",
			tabs:      []sidebar.Tab{selected("first", 1), selected("left", 2), selected("w1", 9), selected("right", 3), selected("recent", 5)},
			want:      "recent",
			wantBy:    landedByRecency,
			wantFound: true,
		},
		{
			name:      "a selected tab wins over never-selected neighbours",
			tabs:      []sidebar.Tab{drawn("left"), drawn("w1"), drawn("right"), selected("far", 1)},
			want:      "far",
			wantBy:    landedByRecency,
			wantFound: true,
		},
		{
			name:      "a withheld tab is passed over however recently it was selected",
			tabs:      []sidebar.Tab{tab("hidden", wsm.WithheldHidden, at(8)), tab("collapsed", wsm.WithheldCollapsed, at(7)), selected("older", 1), drawn("w1")},
			want:      "older",
			wantBy:    landedByRecency,
			wantFound: true,
		},
		{
			name:      "with no tab ever selected the tab immediately left wins",
			tabs:      []sidebar.Tab{drawn("first"), drawn("left"), drawn("w1"), drawn("right")},
			want:      "left",
			wantBy:    landedByNeighbour,
			wantFound: true,
		},
		{
			name:      "with no tab ever selected withheld tabs to the left are passed over",
			tabs:      []sidebar.Tab{drawn("far-left"), tab("hidden", wsm.WithheldHidden, nil), tab("collapsed", wsm.WithheldCollapsed, nil), drawn("w1")},
			want:      "far-left",
			wantBy:    landedByNeighbour,
			wantFound: true,
		},
		{
			name:      "with no tab ever selected the leftmost tab lands on its nearest drawn right neighbour",
			tabs:      []sidebar.Tab{drawn("w1"), tab("hidden", wsm.WithheldHidden, nil), drawn("right"), drawn("further")},
			want:      "right",
			wantBy:    landedByNeighbour,
			wantFound: true,
		},
		{
			name:      "no other drawn tab finds none",
			tabs:      []sidebar.Tab{tab("hidden", wsm.WithheldHidden, at(1)), drawn("w1"), tab("collapsed", wsm.WithheldCollapsed, at(2))},
			wantFound: false,
		},
		{
			name:      "a workspace with no tab finds none",
			tabs:      []sidebar.Tab{selected("w2", 1), drawn("w3")},
			wantFound: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, by, found := hideLandingTarget(tt.tabs, "w1")

			// Assert
			if found != tt.wantFound || found && (got.ID != tt.want || by != tt.wantBy) {
				t.Fatalf("landing = (%q, %q, %v), want (%q, %q, %v)", got.ID, by, found, tt.want, tt.wantBy, tt.wantFound)
			}
		})
	}
}

// hideFixture is a fixture with w1, w2 and w3 in that registry order, and
// CURRENT selected.
func hideFixture(t *testing.T, current ids.WorkspaceID) *fixture {
	t.Helper()
	f := newFixture(t)
	for i, id := range []ids.WorkspaceID{"w1", "w2", "w3"} {
		f.workspace(id, filepath.Join(t.TempDir(), string(id)))
		ws := f.db.workspaces[id]
		ws.Place = int64(i + 1)
		f.db.workspaces[id] = ws
	}
	f.db.current = &current
	return f
}

func TestHidingTheCurrentWorkspaceLandsOnItsLeftNeighbour(t *testing.T) {
	// Arrange
	f := hideFixture(t, "w3")

	// Act
	if err := f.verbs.SetHidden(context.Background(), "w3", true); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w2" {
		t.Fatalf("current = %v, want w2, the tab left of w3", f.db.current)
	}
	landed := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opSetHidden && r.Level == dlog.LevelInfo && r.Context["workspace"] == "w2" && r.Context["hidden"] == "w3" && r.Context["place"] != nil {
			landed = true
		}
	}
	if !landed {
		t.Fatalf("no info record names the landing; records = %+v", f.log.logger.Records())
	}
}

func TestHidingTheLeftmostCurrentWorkspaceLandsOnItsRightNeighbour(t *testing.T) {
	// Arrange
	f := hideFixture(t, "w1")

	// Act
	if err := f.verbs.SetHidden(context.Background(), "w1", true); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w2" {
		t.Fatalf("current = %v, want w2, the tab right of w1", f.db.current)
	}
}

func TestHidingTheCurrentWorkspaceLandsOnTheOneSelectedBeforeIt(t *testing.T) {
	// Arrange: the user selected w1, then w3; w2 sits between them in the bar.
	f := newFixture(t)
	for i, id := range []ids.WorkspaceID{"w1", "w2", "w3", "w4"} {
		ws := f.workspace(id, t.TempDir())
		ws.Place = int64(i + 1)
		f.db.with(ws)
	}
	for minute, id := range map[int]ids.WorkspaceID{1: "w1", 2: "w4", 3: "w3"} {
		ws := f.db.workspaces[id]
		selected := time.Date(2026, 10, 20, 12, minute, 0, 0, time.UTC)
		ws.LastSelectedAt = &selected
		f.db.workspaces[id] = ws
	}
	f.db.current = &[]ids.WorkspaceID{"w3"}[0]

	// Act
	if err := f.verbs.SetHidden(context.Background(), "w3", true); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w4" {
		t.Fatalf("current = %v, want w4, the workspace selected before w3", f.db.current)
	}
	landed := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opSetHidden && r.Level == dlog.LevelInfo && r.Context["workspace"] == "w4" && r.Context["hidden"] == "w3" && r.Context["by"] == string(landedByRecency) {
			landed = true
		}
	}
	if !landed {
		t.Fatalf("no info record names the recency landing; records = %+v", f.log.logger.Records())
	}
}

func TestHidingAnotherWorkspaceLeavesTheSelectionWhereItIs(t *testing.T) {
	// Arrange: the user stands on w1 and hides w2.
	f := hideFixture(t, "w1")

	// Act
	if err := f.verbs.SetHidden(context.Background(), "w2", true); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1 unmoved", f.db.current)
	}
}

func TestHidingWithNoOtherDrawnTabLeavesTheSelectionAndSaysSo(t *testing.T) {
	// Arrange: w2 and w3 are already hidden.
	f := hideFixture(t, "w1")
	for _, id := range []ids.WorkspaceID{"w2", "w3"} {
		ws := f.db.workspaces[id]
		ws.Hidden = true
		f.db.workspaces[id] = ws
	}

	// Act
	if err := f.verbs.SetHidden(context.Background(), "w1", true); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Assert
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1 unmoved", f.db.current)
	}
	said := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opSetHidden && r.Level == dlog.LevelInfo && r.Message == "the tab bar draws no other workspace beside the hidden one; the selection stays" {
			said = true
		}
	}
	if !said {
		t.Fatalf("no info record says the selection stays; records = %+v", f.log.logger.Records())
	}
}

func TestAConditionalSelectStandsWhenTheUserMovedOff(t *testing.T) {
	// Arrange: the select is conditional on w1, but the user is on w2.
	f := hideFixture(t, "w2")
	from := ids.WorkspaceID("w1")
	v := f.verbs.(*verbs)

	// Act
	applied, err := v.selectCurrent(context.Background(), f.log.logger, "w3", &from, RecordSelection)

	// Assert
	if err != nil || applied {
		t.Fatalf("selectCurrent = (%v, %v), want (false, nil)", applied, err)
	}
	if *f.db.current != "w2" {
		t.Fatalf("current = %v, want w2, the user's own switch", *f.db.current)
	}
	stood := false
	for _, r := range f.log.logger.Records() {
		if r.Operation == opSelect && r.Level == dlog.LevelInfo && r.Context["from"] == "w1" && r.Context["current"] == "w2" {
			stood = true
		}
	}
	if !stood {
		t.Fatalf("no info record says the user's switch stands; records = %+v", f.log.logger.Records())
	}
}

func TestAFailedCurrentReadFailsTheLandingLoudly(t *testing.T) {
	// Arrange
	f := hideFixture(t, "w1")
	f.db.currentErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.SetHidden(context.Background(), "w1", true)

	// Assert
	if err == nil {
		t.Fatal("a landing whose current read failed was answered as success")
	}
	if !loggedAtError(f, opSetHidden) {
		t.Fatalf("the failed read was not recorded at error: err=%v records=%+v", err, f.log.logger.Records())
	}
}
func TestHidingLandsOnTheNeighbourTheUserSawBeforeTheHide(t *testing.T) {
	// Arrange: w2 is current between w1 and w3; once hidden, its tab follows
	// the drawn ones, but the landing is read off the bar as it stood.
	f := newFixture(t)
	for i, id := range []ids.WorkspaceID{"w1", "w2", "w3"} {
		ws := f.workspace(id, t.TempDir())
		ws.Place = int64(i + 1)
		f.db.with(ws)
	}
	f.db.current = &[]ids.WorkspaceID{"w2"}[0]

	// Act.
	if err := f.verbs.SetHidden(context.Background(), "w2", true); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Assert.
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1, the left neighbour the user saw", f.db.current)
	}
}

func TestSetHiddenAnswersAFamilyRefusal(t *testing.T) {
	tests := []struct {
		name   string
		hidden bool
		err    error
	}{
		{name: "a hide of a parent with open children", hidden: true, err: errOpenChildren},
		{name: "a show under a parent that is not open", hidden: false, err: errParentNotOpen},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.setHiddenErr = tt.err

			// Act
			err := f.verbs.SetHidden(context.Background(), "w1", tt.hidden)

			// Assert
			requireFamilyAnswer(t, f, err, tt.err, opSetHidden)
			if len(f.visibilityEdges) != 0 {
				t.Fatalf("visibility edges = %v, want none for a refused write", f.visibilityEdges)
			}
		})
	}
}

func TestPlaceRefusal(t *testing.T) {
	tests := []struct {
		name string
		err  error
		arm  string
	}{
		{name: "two repositories", err: fmt.Errorf("swap: %w", wsm.ErrOtherRepository), arm: ArmOtherRepository},
		{name: "not siblings", err: fmt.Errorf("swap: %w", wsm.ErrNotSiblings), arm: ArmNotSiblings},
		{name: "any other error", err: errors.New("disk I/O error")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := placeRefusal(dlog.NewTestLogger(), "MoveWorkspace", tt.err)

			// Assert.
			if tt.arm == "" {
				if got != nil {
					t.Fatalf("placeRefusal = %v, want nil", got)
				}
				return
			}
			if got == nil || got.Arm != tt.arm || got.Rpc != "MoveWorkspace" {
				t.Fatalf("placeRefusal = %+v, want MoveWorkspace's %s", got, tt.arm)
			}
		})
	}
}

func TestSwapAndMoveShareThePlaceRefusal(t *testing.T) {
	// Act.
	shared := sourcescan.Count(t, "placeRefusal(log, ")
	byHand := sourcescan.Count(t, "ArmNotSiblings, err.Error()")

	// Assert.
	if shared != 2 || byHand != 1 {
		t.Fatalf("placeRefusal calls = %d and hand-spelled not_siblings arms = %d, want Swap and Move sharing the one helper", shared, byHand)
	}
}

func TestSelectAsARecencyWalkStepLeavesTheSelectionInstant(t *testing.T) {
	// Arrange: w1 was last chosen an hour ago; the walk steps onto it now.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	earlier := fixedNow.Add(-time.Hour)
	ws := f.db.workspaces["w1"]
	ws.LastSelectedAt = &earlier
	f.db.workspaces["w1"] = ws

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", KeepSelectionHistory); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: current moved, the history did not.
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", f.db.current)
	}
	if !slices.Equal(f.db.keptHistorySelects, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("kept-history selects = %v, want w1 once", f.db.keptHistorySelects)
	}
	if got := f.db.workspaces["w1"].LastSelectedAt; got == nil || !got.Equal(earlier) {
		t.Fatalf("last selected = %v, want the standing %v", got, earlier)
	}
}

func TestSelectAsARecencyWalkStepStillTellsTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", KeepSelectionHistory); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.selected) != 1 || f.sidebar.selected[0] != "w1" {
		t.Fatalf("roster selections = %v, want w1 once", f.sidebar.selected)
	}
}

func TestSelectAsARecencyWalkStepLogsItsReceiptAtInfo(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", KeepSelectionHistory); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{
		Message: "selected the workspace as a step of a recency walk; the selection instant stands"})
	if !ok || r.Level != "info" || r.Context["history"] != "keep" {
		t.Fatalf("walk receipt = %+v (found %v), want info with history=keep", r, ok)
	}
}

func TestSelectAsARecencyWalkStepThatCannotRecordFailsLoudly(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.keepSelectedErr = errors.New("disk gone")

	// Act.
	err := f.verbs.Select(context.Background(), "w1", KeepSelectionHistory)

	// Assert: the error reaches the caller, is logged once with its cause,
	// and the roster was told nothing.
	if err == nil || !strings.Contains(err.Error(), "disk gone") {
		t.Fatalf("Select = %v, want the store's failure", err)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Message: "could not record the walk's selection"})
	if !ok || r.Level != "error" || r.Context["cause"] != "disk gone" || r.Context["history"] != "keep" {
		t.Fatalf("failure record = %+v (found %v), want error with cause and history", r, ok)
	}
	if len(f.sidebar.selected) != 0 {
		t.Fatalf("roster selections = %v, want none after a failed record", f.sidebar.selected)
	}
}

func TestSelectionHistoryNamesEachValue(t *testing.T) {
	tests := []struct {
		history SelectionHistory
		want    string
	}{
		{RecordSelection, "record"},
		{KeepSelectionHistory, "keep"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act / Assert.
			if got := tt.history.String(); got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestSelectionHistoryPanicsOnAnUnknownValue(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("String() on an unknown value did not panic")
		}
	}()

	// Act.
	_ = SelectionHistory(99).String()
}

// TestSelectStartsNoShim pins the owner ruling of 2026-10-28: switching to a
// workspace starts no shim, whatever its session's state; the next prompt
// does.
func TestSelectStartsNoShim(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
	}{
		{name: "a hibernated workspace", arrange: func(f *fixture) { f.hibernate("w1") }},
		{name: "a workspace with no session", arrange: func(*fixture) {}},
		{name: "a hibernated workspace on a daemon standing down", arrange: func(f *fixture) {
			f.hibernate("w1")
			f.fleet.standingDown = true
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			tc.arrange(f)

			// Act.
			err := f.verbs.Select(context.Background(), "w1", RecordSelection)

			// Assert.
			if err != nil || len(f.fleet.startCalls) != 0 || len(f.fleet.started) != 0 {
				t.Fatalf("Select = %v after starts %v; want the selection and no start", err, f.fleet.startCalls)
			}
		})
	}
}

func TestSelectLeavesAParkedWorkspacesParkDrawn(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")

	// Act.
	if err := f.verbs.Select(context.Background(), "w1", RecordSelection); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.topbarParked) != 0 {
		t.Fatalf("topbar park edges = %v, want the park left as it is", f.topbarParked)
	}
}
