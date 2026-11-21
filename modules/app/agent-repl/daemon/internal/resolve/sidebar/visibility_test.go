package sidebar_test

import (
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// visibilityName reads a row's visibility arm by name.
func visibilityName(v *frontendv1.RosterRowVisibility) string {
	switch v.GetVisibility().(type) {
	case *frontendv1.RosterRowVisibility_NotVisible:
		return "not_visible"
	case *frontendv1.RosterRowVisibility_UnselectedVisible:
		return "unselected_visible"
	case *frontendv1.RosterRowVisibility_Selected:
		return "selected"
	default:
		return "unset"
	}
}

// visibilities reads the visibility of every row the resolver last published,
// of every merged row it pages, and of every thread row, by id.
func visibilities(t *testing.T, r sidebar.Resolver) map[string]string {
	t.Helper()
	push := latestPush(t, r)
	out := map[string]string{}
	for _, row := range append(rosterwalk.AllRows(push), mergedRows(t, r)...) {
		out[row.GetWorkspace().GetWorkspace().GetId()] = visibilityName(row.GetVisibility())
	}
	for _, row := range push.GetWorkspaces().GetThreads().GetRows() {
		out[row.GetThread().GetId()] = visibilityName(row.GetVisibility())
	}
	return out
}

// foldedOne is theWS alone, in a collapsed repository section.
func foldedOne() sidebar.Registry {
	reg := registry(workspace(string(theWS), "one"))
	reg.Repositories[0].Folded = true
	return reg
}

// working brings WS to a running turn: its session is live and a prompt's
// turn runs.
func working(r sidebarResolver, ws ids.WorkspaceID) {
	r.OnLink(ws, shimclient.LinkConnected)
	r.OnSessionStarted(ws, &conversationv1.SessionStarted{VendorSessionId: "vendor-" + string(ws)})
	r.SetTurn(ws, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.AckTurn(ws)
}

// finished runs one turn of WS to its end, its result unread.
func turnFinished(r sidebarResolver, ws ids.WorkspaceID) {
	working(r, ws)
	r.SetTurnEnded(ws, wsm.CloseCompleted)
}

func TestAFoldedIdleWorkspaceIsNotVisible(t *testing.T) {
	// Arrange
	r := arrange(t, foldedOne().Workspaces...)
	r.SetRegistry(foldedOne())

	// Act
	got := visibilities(t, r)

	// Assert
	if got[string(theWS)] != "not_visible" {
		t.Fatalf("visibility = %s, want not_visible: a fold covers an idle, read workspace", got[string(theWS)])
	}
}

func TestAFoldedWorkingWorkspaceIsVisible(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())

	// Act
	working(r, theWS)

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "unselected_visible" {
		t.Fatalf("visibility = %s, want unselected_visible: a working workspace is visible under a fold", got)
	}
}

func TestAFoldedUnviewedWorkspaceIsVisible(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())

	// Act
	turnFinished(r, theWS)

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "unselected_visible" {
		t.Fatalf("visibility = %s, want unselected_visible: an unread result keeps a folded workspace visible", got)
	}
}

func TestAFoldedWorkspaceWhoseResultIsReadIsNotVisible(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())
	turnFinished(r, theWS)

	// Act
	r.SetViewed(theWS)

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "not_visible" {
		t.Fatalf("visibility = %s, want not_visible once the result is read", got)
	}
}

func TestAVisibleRowUnderAFoldHasANumberedSurfacedTab(t *testing.T) {
	// Arrange
	r := arrange(t)
	r.SetRegistry(foldedOne())

	// Act
	working(r, theWS)

	// Assert
	row := rosterwalk.FlattenOpen(rosterwalk.UnsectionedRows(latest(t, r).GetRepository().GetSections()[0]))[0]
	if _, numbered := row.GetTab().GetTab().(*frontendv1.RosterOpenRowTab_Numbered); !numbered || !row.GetSurfaced() {
		t.Fatalf("tab = %v surfaced = %v, want a numbered tab drawn inside the fold", row.GetTab(), row.GetSurfaced())
	}
}

func TestTheParentOfAVisibleWorkspaceUnderAFoldIsVisible(t *testing.T) {
	// Arrange: the family sits in a collapsed repository; one child works.
	reg := blockFamily()
	reg.Repositories[0].Folded = true
	r := arrange(t)
	r.SetRegistry(reg)

	// Act
	working(r, "w-a1")

	// Assert
	got := visibilities(t, r)
	if got["w-p"] != "unselected_visible" || got["w-a1"] != "unselected_visible" {
		t.Fatalf("parent %s, child %s; want both visible: the parent of a visible workspace is visible", got["w-p"], got["w-a1"])
	}
}

func TestASiblingOfAVisibleWorkspaceUnderAFoldStaysNotVisible(t *testing.T) {
	// Arrange
	reg := blockFamily()
	reg.Repositories[0].Folded = true
	r := arrange(t)
	r.SetRegistry(reg)

	// Act
	working(r, "w-a1")

	// Assert
	if got := visibilities(t, r)["w-g1"]; got != "not_visible" {
		t.Fatalf("sibling visibility = %s, want not_visible", got)
	}
}

func TestTheSelectionIsSelectedUnderAFold(t *testing.T) {
	// Arrange
	r := arrange(t)

	// Act
	r.SetRegistrySelected(foldedOne(), theWS, sidebar.SelectStamp{})

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "selected" {
		t.Fatalf("visibility = %s, want selected", got)
	}
}

func TestAHiddenSelectionIsSelected(t *testing.T) {
	// Arrange
	r := arrange(t)

	// Act
	r.SetRegistrySelected(registry(hiddenWorkspace(string(theWS), "one")), theWS, sidebar.SelectStamp{})

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "selected" {
		t.Fatalf("visibility = %s, want selected: the selection is selected whatever excludes it", got)
	}
}

func TestAHiddenWorkspaceNobodyIsOnIsNotVisible(t *testing.T) {
	// Arrange
	r := arrange(t)

	// Act
	r.SetRegistry(registry(hiddenWorkspace(string(theWS), "one")))

	// Assert
	if got := visibilities(t, r)[string(theWS)]; got != "not_visible" {
		t.Fatalf("visibility = %s, want not_visible", got)
	}
}

func TestAThreadIsSelectedOnlyWhileItIsTheSelection(t *testing.T) {
	cases := []struct {
		name     string
		selected ids.WorkspaceID
		want     string
	}{
		{name: "the selected thread", selected: "t-1", want: "selected"},
		{name: "a thread the user left", selected: "w-a", want: "not_visible"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)

			// Act
			r.SetRegistrySelected(withThreads(thread("t-1", "why", nil)), tc.selected, sidebar.SelectStamp{})

			// Assert
			if got := visibilities(t, r)["t-1"]; got != tc.want {
				t.Fatalf("thread visibility = %s, want %s", got, tc.want)
			}
		})
	}
}

// visibilitySink records every change the resolver hands its visibility sink.
type visibilitySink struct {
	mu      sync.Mutex
	changes []map[ids.WorkspaceID]wsm.Visibility
}

func (s *visibilitySink) record(changes map[ids.WorkspaceID]wsm.Visibility) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.changes = append(s.changes, changes)
}

func (s *visibilitySink) all() []map[ids.WorkspaceID]wsm.Visibility {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]map[ids.WorkspaceID]wsm.Visibility(nil), s.changes...)
}

func TestEveryVisibilityChangeIsHandedToTheSink(t *testing.T) {
	// Arrange
	sink := &visibilitySink{}
	r, _ := newResolver(t, sidebar.WithVisibilitySink(sink.record))

	// Act: drawn first, then folded away.
	r.SetRegistry(registry(workspace(string(theWS), "one")))
	r.SetRegistry(foldedOne())

	// Assert
	got := sink.all()
	if len(got) != 2 || got[0][theWS] != wsm.UnselectedVisible || got[1][theWS] != wsm.NotVisible {
		t.Fatalf("changes = %v, want visible then not visible", got)
	}
}

func TestAnUnchangedVisibilityIsNotHandedToTheSinkAgain(t *testing.T) {
	// Arrange
	sink := &visibilitySink{}
	r, _ := newResolver(t, sidebar.WithVisibilitySink(sink.record))
	r.SetRegistry(registry(workspace(string(theWS), "one")))

	// Act
	r.SetRegistry(registry(workspace(string(theWS), "one")))

	// Assert
	if got := sink.all(); len(got) != 1 {
		t.Fatalf("changes = %v, want the one change only", got)
	}
}

func TestAVisibilityTheStoreAlreadyHoldsIsNotWrittenAgain(t *testing.T) {
	// Arrange
	sink := &visibilitySink{}
	r, _ := newResolver(t, sidebar.WithVisibilitySink(sink.record))
	ws := workspace(string(theWS), "one")
	ws.Visibility = wsm.UnselectedVisible

	// Act
	r.SetRegistry(registry(ws))

	// Assert
	if got := sink.all(); len(got) != 0 {
		t.Fatalf("changes = %v, want none: the store already holds the derived value", got)
	}
}

func TestVisibilityAnswersTheLastRendersValue(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistrySelected(registry(workspace(string(theWS), "one"), workspace("w2", "two")), theWS, sidebar.SelectStamp{})

	// Assert
	if r.Visibility(theWS) != wsm.Selected || r.Visibility("w2") != wsm.UnselectedVisible || r.Visibility("w-gone") != wsm.NotVisible {
		t.Fatalf("visibility = %v, %v, %v; want selected, visible, and not visible for a workspace never derived",
			r.Visibility(theWS), r.Visibility("w2"), r.Visibility("w-gone"))
	}
}
