package footer

import (
	"slices"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"
)

// openChild is an open child named NAME, waiting on the user when WAITING.
func openChild(name string, waiting bool) OpenChild {
	return OpenChild{Ref: &workspacev1.WorkspaceRef{Id: "ws-" + name}, Name: name, Status: "thinking", Tone: "red",
		ShortSummary: "Summary of " + name, WaitingOnUser: waiting}
}

func TestTheChildrenChipCountsTheOpenChildrenAndThoseWaitingOnTheUser(t *testing.T) {
	tests := []struct {
		name        string
		children    []OpenChild
		wantCount   uint32
		wantWaiting uint32
	}{
		{name: "no open child raises no chip"},
		{name: "open children none waiting", children: []OpenChild{openChild("a", false), openChild("b", false)}, wantCount: 2},
		{name: "open children one waiting", children: []OpenChild{openChild("a", true), openChild("b", false)}, wantCount: 2, wantWaiting: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.SetOpenChildren(testWS, tt.children)

			// Assert
			chip := h.view(t).GetStrip().GetLiveWork().GetChildren()
			if chip.GetCount() != tt.wantCount || chip.GetWaitingOnUser().GetCount() != tt.wantWaiting {
				t.Fatalf("children chip = %v, want count %d waiting %d", chip, tt.wantCount, tt.wantWaiting)
			}
			if (chip.GetWaitingOnUser() != nil) != (tt.wantWaiting > 0) {
				t.Fatalf("waiting glyph = %v, want one iff a child waits", chip.GetWaitingOnUser())
			}
		})
	}
}

func TestTheChildrenPanelListsEachOpenChildInTheTrackersOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bare := openChild("c", false)
	bare.ShortSummary = ""

	// Act
	h.r.SetOpenChildren(testWS, []OpenChild{openChild("b", false), openChild("a", true), bare})

	// Assert
	var got []string
	for _, row := range h.view(t).GetExpanded().GetChildren().GetRows() {
		got = append(got, row.GetName().GetText()+"|"+row.GetStatus().GetText()+"|"+row.GetStatus().GetTone()+"|"+row.GetShortSummary().GetText()+"|"+row.GetSelect().GetTarget().GetId())
	}
	want := []string{"b|thinking|red|Summary of b|ws-b", "a|thinking|red|Summary of a|ws-a", "c|thinking|red||ws-c"}
	if !slices.Equal(got, want) {
		t.Fatalf("rows = %v, want %v", got, want)
	}
}

func TestEmptyingTheOpenChildrenRetiresTheChipAndEmptiesThePanel(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetOpenChildren(testWS, []OpenChild{openChild("a", false)})

	// Act
	h.r.SetOpenChildren(testWS, nil)

	// Assert
	view := h.view(t)
	if chip := view.GetStrip().GetLiveWork().GetChildren(); chip != nil {
		t.Fatalf("children chip = %v, want none", chip)
	}
	if rows := view.GetExpanded().GetChildren().GetRows(); len(rows) != 0 {
		t.Fatalf("panel rows = %v, want none", rows)
	}
}

func TestTheOpenChildrenAreCopiedNotShared(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	children := []OpenChild{openChild("a", false)}
	h.r.SetOpenChildren(testWS, children)

	// Act: the caller reuses its slice.
	children[0].Name = "changed"
	h.r.SetOpenChildren(testWS, []OpenChild{openChild("a", false), openChild("b", false)})
	h.r.SetOpenChildren(testWS, children[:1])

	// Assert: the installed list is the one handed at the last call.
	rows := h.view(t).GetExpanded().GetChildren().GetRows()
	if len(rows) != 1 || rows[0].GetName().GetText() != "changed" {
		t.Fatalf("rows = %v, want the one child as last installed", rows)
	}
}
