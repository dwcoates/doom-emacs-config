package sidebar

import (
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestLandingTarget(t *testing.T) {
	at := func(minute int) *time.Time {
		v := time.Date(2026, 10, 20, 12, minute, 0, 0, time.UTC)
		return &v
	}
	tab := func(id ids.WorkspaceID, withheld wsm.Withholding, selected *time.Time) Tab {
		return Tab{Workspace: wsm.Workspace{ID: id, LastSelectedAt: selected}, Withheld: withheld}
	}
	drawn := func(id ids.WorkspaceID) Tab { return tab(id, wsm.Drawn, nil) }
	selected := func(id ids.WorkspaceID, minute int) Tab { return tab(id, wsm.Drawn, at(minute)) }
	tests := []struct {
		name      string
		tabs      []Tab
		want      ids.WorkspaceID
		wantBy    Landing
		wantFound bool
	}{
		{
			name:      "the most recently selected tab wins over the first and the neighbours",
			tabs:      []Tab{selected("first", 1), selected("left", 2), selected("w1", 9), selected("right", 3), selected("recent", 5)},
			want:      "recent",
			wantBy:    LandedByRecency,
			wantFound: true,
		},
		{
			name:      "a selected tab wins over never-selected neighbours",
			tabs:      []Tab{drawn("left"), drawn("w1"), drawn("right"), selected("far", 1)},
			want:      "far",
			wantBy:    LandedByRecency,
			wantFound: true,
		},
		{
			name:      "a withheld tab is passed over however recently it was selected",
			tabs:      []Tab{tab("hidden", wsm.WithheldHidden, at(8)), tab("collapsed", wsm.WithheldCollapsed, at(7)), selected("older", 1), drawn("w1")},
			want:      "older",
			wantBy:    LandedByRecency,
			wantFound: true,
		},
		{
			name:      "with no tab ever selected the tab immediately left wins",
			tabs:      []Tab{drawn("first"), drawn("left"), drawn("w1"), drawn("right")},
			want:      "left",
			wantBy:    LandedByNeighbour,
			wantFound: true,
		},
		{
			name:      "with no tab ever selected withheld tabs to the left are passed over",
			tabs:      []Tab{drawn("far-left"), tab("hidden", wsm.WithheldHidden, nil), tab("collapsed", wsm.WithheldCollapsed, nil), drawn("w1")},
			want:      "far-left",
			wantBy:    LandedByNeighbour,
			wantFound: true,
		},
		{
			name:      "with no tab ever selected the leftmost tab lands on its nearest drawn right neighbour",
			tabs:      []Tab{drawn("w1"), tab("hidden", wsm.WithheldHidden, nil), drawn("right"), drawn("further")},
			want:      "right",
			wantBy:    LandedByNeighbour,
			wantFound: true,
		},
		{
			name:      "no other drawn tab finds none",
			tabs:      []Tab{tab("hidden", wsm.WithheldHidden, at(1)), drawn("w1"), tab("collapsed", wsm.WithheldCollapsed, at(2))},
			wantFound: false,
		},
		{
			name:      "a workspace with no tab finds none",
			tabs:      []Tab{selected("w2", 1), drawn("w3")},
			wantFound: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, by, found := LandingTarget(tt.tabs, "w1", nil)

			// Assert
			if found != tt.wantFound || found && (got.ID != tt.want || by != tt.wantBy) {
				t.Fatalf("landing = (%q, %q, %v), want (%q, %q, %v)", got.ID, by, found, tt.want, tt.wantBy, tt.wantFound)
			}
		})
	}
}

func TestLandingTargetWithinAFoldPassesOverTheFoldsMembers(t *testing.T) {
	at := func(minute int) *time.Time {
		v := time.Date(2026, 11, 9, 12, minute, 0, 0, time.UTC)
		return &v
	}
	sub := wsm.SubsectionID("s1")
	inSub := func(id ids.WorkspaceID, minute int) Tab {
		return Tab{Workspace: wsm.Workspace{ID: id, Subsection: &sub, LastSelectedAt: at(minute)}, Withheld: wsm.Drawn}
	}
	outside := func(id ids.WorkspaceID, minute int) Tab {
		return Tab{Workspace: wsm.Workspace{ID: id, LastSelectedAt: at(minute)}, Withheld: wsm.Drawn}
	}
	neverSelected := func(id ids.WorkspaceID) Tab {
		return Tab{Workspace: wsm.Workspace{ID: id}, Withheld: wsm.Drawn}
	}
	tests := []struct {
		name      string
		tabs      []Tab
		want      ids.WorkspaceID
		wantBy    Landing
		wantFound bool
	}{
		{
			name:      "the most recent tab outside the fold wins over a more recent member",
			tabs:      []Tab{outside("older", 1), inSub("w1", 9), inSub("member", 8), outside("newer", 5)},
			want:      "newer",
			wantBy:    LandedByRecency,
			wantFound: true,
		},
		{
			name:      "with no tab outside ever selected a member neighbour is passed over",
			tabs:      []Tab{neverSelected("far-left"), inSub("member", 4), inSub("w1", 9)},
			want:      "far-left",
			wantBy:    LandedByNeighbour,
			wantFound: true,
		},
		{
			name:      "a fold holding every drawn tab leaves nowhere to land",
			tabs:      []Tab{inSub("member", 4), inSub("w1", 9)},
			wantFound: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, by, found := LandingTarget(tt.tabs, "w1", wsm.SubsectionMembers(sub))

			// Assert
			if found != tt.wantFound || found && (got.ID != tt.want || by != tt.wantBy) {
				t.Fatalf("landing = (%q, %q, %v), want (%q, %q, %v)", got.ID, by, found, tt.want, tt.wantBy, tt.wantFound)
			}
		})
	}
}
