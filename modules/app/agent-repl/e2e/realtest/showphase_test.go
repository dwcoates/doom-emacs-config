//go:build realtest

package realtest

import (
	"strings"
	"testing"
	"time"
)

func TestShowPhaseNoteReportsTheHealthySingleEdge(t *testing.T) {
	// Arrange / Act
	note := showPhaseNote(1, true, 420*time.Millisecond, 2)

	// Assert
	if strings.Contains(note, "FINDING") {
		t.Errorf("a paint that happened on the first focus edge is the healthy shape and must not be reported "+
			"as a finding; it said %q", note)
	}
}

func TestShowPhaseNoteCallsExtraEdgesAProductFinding(t *testing.T) {
	// Arrange / Act
	note := showPhaseNote(2, true, 21*time.Second, 2)

	// Assert
	if !strings.Contains(note, "PRODUCT FINDING") {
		t.Errorf("a paint that needed a second focus edge is a product finding about the drain re-parking; "+
			"it said %q", note)
	}
}

func TestShowPhaseNoteNamesTheRepark(t *testing.T) {
	// Arrange / Act
	note := showPhaseNote(3, true, 41*time.Second, 3)

	// Assert
	if !strings.Contains(note, "re-parks") {
		t.Errorf("the finding must name the mechanism the lead has to act on; it said %q", note)
	}
}

func TestShowPhaseNoteSaysALongerCeilingWouldNotHelp(t *testing.T) {
	// Arrange / Act
	note := showPhaseNote(3, false, 2*time.Minute, 2)

	// Assert
	if !strings.Contains(note, "not waiting on a longer ceiling") {
		t.Errorf("an unpainted verdict must rule out the ceiling, which is what the 2m timeout looked like; "+
			"it said %q", note)
	}
}

func TestShowPhaseNoteCountsTheWorkspaces(t *testing.T) {
	// Arrange / Act
	note := showPhaseNote(1, true, time.Second, 4)

	// Assert
	if !strings.Contains(note, "4 open workspace(s)") {
		t.Errorf("the note must say how many workspaces the verdict covers; it said %q", note)
	}
}

func TestShowPhaseEdgeCeilingIsTheSliceForAnEarlyEdge(t *testing.T) {
	// Arrange / Act
	got := showPhaseEdgeCeiling(1, 0)

	// Assert
	if got != showEdgeCeiling {
		t.Errorf("an early edge gets its own slice %s; it got %s", showEdgeCeiling, got)
	}
}

func TestShowPhaseEdgeCeilingGivesTheLastEdgeWhatIsLeft(t *testing.T) {
	// Arrange
	spent := 40 * time.Second

	// Act
	got := showPhaseEdgeCeiling(showMaxFocusEdges, spent)

	// Assert
	if want := showCeiling - spent; got != want {
		t.Errorf("the last edge must be given the rest of the show ceiling (%s); it got %s", want, got)
	}
}

func TestShowPhaseEdgeCeilingNeverGoesBelowOneSlice(t *testing.T) {
	// Arrange: a phase that has already overrun the whole show ceiling.
	spent := showCeiling + time.Minute

	// Act
	got := showPhaseEdgeCeiling(showMaxFocusEdges, spent)

	// Assert
	if got != showEdgeCeiling {
		t.Errorf("an overrun phase must still give its last edge a real chance rather than a negative bound; "+
			"it got %s", got)
	}
}

func TestShowPhaseTotalBoundMatchesTheShowCeiling(t *testing.T) {
	// Arrange / Act / Assert
	//
	// The edges are slices of one bound, not additions to it: a phase whose
	// slices summed past showCeiling would reintroduce the multi-minute stall
	// the shared helper exists to remove.
	if showEdgeCeiling*time.Duration(showMaxFocusEdges) > showCeiling {
		t.Errorf("%d edges of %s each exceed the show ceiling %s",
			showMaxFocusEdges, showEdgeCeiling, showCeiling)
	}
}

func TestShowPhaseNoteDoesNotClaimTheEdgeWasReal(t *testing.T) {
	// Arrange / Act
	note := showPhaseNote(3, false, 2*time.Minute, 2)

	// Assert
	if !strings.Contains(note, "REQUESTED activation") {
		t.Errorf("the harness can only say it ASKED for activation — whether the editor took focus is a "+
			"separate reading, and the 2026-09-12 evening sweeps claimed an edge that never happened; "+
			"it said %q", note)
	}
}
