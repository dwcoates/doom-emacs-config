package runtimemetrics

import (
	"testing"

	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

func TestVisibilityCountsAreServedByVisibilityAndReason(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	counts := map[wsm.VisibilityReason]int{}
	for _, r := range wsm.VisibilityReasonsAll {
		counts[r] = 0
	}
	counts[wsm.ReasonSelected] = 1
	counts[wsm.ReasonUncovered] = 42
	counts[wsm.ReasonCoveredWorking] = 3
	counts[wsm.ReasonCovered] = 1500

	// Act
	s.SetVisibilityCounts(counts)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_workspaces_visible{reason="selected",visibility="selected"} 1`,
		`agentrepl_workspaces_visible{reason="uncovered",visibility="unselected_visible"} 42`,
		`agentrepl_workspaces_visible{reason="covered_working",visibility="unselected_visible"} 3`,
		`agentrepl_workspaces_visible{reason="covered",visibility="not_visible"} 1500`,
		`agentrepl_workspaces_visible{reason="parent_of_visible",visibility="unselected_visible"} 0`,
	)
}

func TestEveryVisibilityReasonIsServedFromZero(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	body := scrape(t, s)

	// Assert
	for _, r := range wsm.VisibilityReasonsAll {
		mustContain(t, body, `agentrepl_workspaces_visible{reason="`+r.String()+`",visibility="`+r.Visibility().String()+`"} 0`)
	}
}

func TestAVisibilityChangeIsCountedByFromToAndReason(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.CountVisibilityChange(wsm.NotVisible, wsm.UnselectedVisible, wsm.ReasonCoveredUnviewed)
	s.CountVisibilityChange(wsm.NotVisible, wsm.UnselectedVisible, wsm.ReasonCoveredUnviewed)
	s.CountVisibilityChange(wsm.UnselectedVisible, wsm.NotVisible, wsm.ReasonCovered)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_visibility_changes_total{from="not_visible",reason="covered_unviewed",to="unselected_visible"} 2`,
		`agentrepl_visibility_changes_total{from="unselected_visible",reason="covered",to="not_visible"} 1`,
	)
}

func TestAResultReadIsCountedByWhatReadIt(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.CountResultRead(sidebar.ReadByEditorView)
	s.CountResultRead(sidebar.ReadByInterrupt)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_workspace_results_read_total{by="editor_view"} 1`,
		`agentrepl_workspace_results_read_total{by="interrupted"} 1`,
		`agentrepl_workspace_results_read_total{by="context_cut"} 0`,
	)
}

func TestUndeclaredVisibilityLabelsPanic(t *testing.T) {
	tests := []struct {
		name string
		act  func(s *Surface)
	}{
		{name: "a reason outside the vocabulary", act: func(s *Surface) {
			s.SetVisibilityCounts(map[wsm.VisibilityReason]int{wsm.VisibilityReason(99): 1})
		}},
		{name: "a reader outside the vocabulary", act: func(s *Surface) { s.CountResultRead("someone") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := openSurface(t)
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatal("an undeclared label value did not panic")
				}
			}()

			// Act
			tt.act(s)
		})
	}
}

func TestTheOffSurfaceRecordsNoVisibility(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and Assert: none of these panics on the off surface.
	s.SetVisibilityCounts(map[wsm.VisibilityReason]int{wsm.ReasonSelected: 1})
	s.CountVisibilityChange(wsm.NotVisible, wsm.Selected, wsm.ReasonSelected)
	s.CountResultRead(sidebar.ReadByEditorView)
}
