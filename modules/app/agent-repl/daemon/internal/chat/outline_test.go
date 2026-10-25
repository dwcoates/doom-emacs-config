package chat

import (
	"context"
	"errors"
	"fmt"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE OUTLINE: runs, the window around the shown run, the breadcrumb above it,
// and the Show gesture that moves the path and reveals a node.

// outline is the outline last handed to the roster.
func (f *fixture) outline() *frontendv1.RosterThreadOutline {
	f.t.Helper()
	if len(f.outlines) == 0 {
		f.t.Fatal("no outline was handed to the roster")
	}
	return f.outlines[len(f.outlines)-1]
}

// rowShape is a row as "node@depth×turns:place", plus "+N" when N runs fold
// below it.
func rowShape(r *frontendv1.RosterThreadOutlineRow) string {
	place := "off"
	switch r.GetPlace().GetPlace().(type) {
	case *frontendv1.RosterThreadOutlinePlace_OnPath:
		place = "path"
	case *frontendv1.RosterThreadOutlinePlace_Shown:
		place = "shown"
	}
	out := fmt.Sprintf("%s@%d×%d:%s", r.GetNode().GetId(), r.GetDepth(), r.GetTurns(), place)
	if r.Folded != nil {
		out += fmt.Sprintf("+%d", r.GetFolded().GetRuns())
	}
	return out
}

func shapes(o *frontendv1.RosterThreadOutline) []string {
	var out []string
	for _, r := range o.GetRows() {
		out = append(out, rowShape(r))
	}
	return out
}

// branchy seeds a thread whose shown path passes LEVELS branch points, each
// with an older side branch and a newer main branch (shown: a tie goes to the
// newest), and answers the branch points' ids, root first.
func branchy(f *fixture, levels int) []string {
	var points []string
	at := f.zettel.seed("", 0)
	for i := 0; i < levels; i++ {
		points = append(points, at)
		f.zettel.seed(at, 0) // the side branch
		at = f.zettel.seed(at, 0)
	}
	return points
}

func TestALinearThreadIsOneRun(t *testing.T) {
	// Arrange
	f := newFixture(t)
	root := f.zettel.seed("", 0)
	f.zettel.seed(f.zettel.seed(f.zettel.seed(root, 0), 0), 0)

	// Act
	f.open()

	// Assert
	o := f.outline()
	if got := shapes(o); !equal(got, []string{root + "@0×4:shown"}) || o.Breadcrumb != nil {
		t.Fatalf("outline = %v (breadcrumb %v), want one shown run of 4", got, o.GetBreadcrumb())
	}
	if label := o.GetRows()[0].GetLabel().GetText(); label != "q "+root {
		t.Fatalf("label = %q, want the run's first question", label)
	}
}

func TestABranchPointStartsARunPerChild(t *testing.T) {
	// Arrange: root → a, which has an older child b and a newer child c.
	f := newFixture(t)
	root := f.zettel.seed("", 0)
	a := f.zettel.seed(root, 0)
	b := f.zettel.seed(a, 0)
	c := f.zettel.seed(a, 0)

	// Act
	f.open()

	// Assert
	want := []string{root + "@0×2:path", b + "@1×1:off", c + "@1×1:shown"}
	if got := shapes(f.outline()); !equal(got, want) {
		t.Fatalf("outline = %v, want %v", got, want)
	}
}

func TestTheRowsStartAFewRunsAboveTheShownRun(t *testing.T) {
	cases := []struct {
		name       string
		levels     int
		wantCrumbs string
	}{
		{name: "near the top, nothing folds", levels: 3, wantCrumbs: ""},
		{name: "a little deeper, the folded runs are named whole", levels: 6, wantCrumbs: "whole:3"},
		{name: "deep, the folded runs are elided", levels: 9, wantCrumbs: "elided:1+4+1"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			branchy(f, tc.levels)

			// Act
			f.open()

			// Assert: no row deeper than the window, whatever the depth.
			o := f.outline()
			for _, r := range o.GetRows() {
				if r.GetDepth() > OutlineWindow {
					t.Fatalf("row %s at depth %d, past the window %d", rowShape(r), r.GetDepth(), OutlineWindow)
				}
			}
			got := ""
			switch b := o.GetBreadcrumb().GetExtent().(type) {
			case *frontendv1.RosterThreadOutlineBreadcrumb_Whole:
				got = fmt.Sprintf("whole:%d", len(b.Whole.GetCrumbs()))
			case *frontendv1.RosterThreadOutlineBreadcrumb_Elided:
				got = fmt.Sprintf("elided:1+%d+1", b.Elided.GetOmitted())
			}
			if got != tc.wantCrumbs {
				t.Fatalf("breadcrumb = %q, want %q", got, tc.wantCrumbs)
			}
		})
	}
}

func TestARowAtTheWindowsFloorCountsTheRunsFoldedBelowIt(t *testing.T) {
	// Arrange: the shown path ends three branch points down, and the
	// first-level side branch holds branch points of its own, four deep.
	f := newFixture(t)
	points := branchy(f, 3)
	side := "n2" // the first level's side branch, seeded right after the root
	at := side
	for i := 0; i < 4; i++ {
		f.zettel.seed(at, 0)
		at = f.zettel.seed(at, 0)
	}

	// Act
	f.open()

	// Assert
	var floor []string
	for _, r := range f.outline().GetRows() {
		if r.GetDepth() == OutlineWindow && r.Folded != nil {
			floor = append(floor, rowShape(r))
		}
	}
	if len(floor) == 0 {
		t.Fatalf("outline = %v (points %v), want a row at the floor folding runs below it", shapes(f.outline()), points)
	}
}

func TestTheOutlineIsHandedOnlyWhenItChanges(t *testing.T) {
	// Arrange
	f := newFixture(t)
	root := f.zettel.seed("", 0)
	f.open()
	handed := len(f.outlines)

	// Act: a highlight republishes the view, the outline unchanged.
	if err := f.svc.Select(context.Background(), thr, &Highlight{Node: NodeID(root)}); err != nil {
		t.Fatal(err)
	}

	// Assert
	if len(f.outlines) != handed {
		t.Fatalf("outlines handed = %d, want still %d", len(f.outlines), handed)
	}
}

func TestADeletedThreadsOutlineIsTakenBack(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.open()

	// Act
	if err := f.svc.Delete(context.Background(), thr); err != nil {
		t.Fatal(err)
	}

	// Assert
	if f.outline() != nil {
		t.Fatalf("last outline = %v, want nil once the thread is deleted", f.outline())
	}
}

func TestShowMovesThePathThroughTheNodeAndRevealsIt(t *testing.T) {
	// Arrange: root → a with children b (older) and c (newer, shown).
	f := newFixture(t)
	root := f.zettel.seed("", 0)
	a := f.zettel.seed(root, 0)
	b := f.zettel.seed(a, 0)
	f.zettel.seed(a, 0)
	f.open()

	// Act
	err := f.svc.Show(context.Background(), thr, NodeID(b))

	// Assert
	if err != nil || !equal(f.path(), []string{root, a, b}) {
		t.Fatalf("Show = %v, path = %v; want the path through %s", err, f.path(), b)
	}
	reveal := f.view().GetConversation().GetReveal()
	if reveal.GetNode().GetId() != b || reveal.GetSequence() != 1 {
		t.Fatalf("reveal = %v, want %s at sequence 1", reveal, b)
	}
}

func TestEachShowRaisesTheRevealsSequence(t *testing.T) {
	// Arrange
	f := newFixture(t)
	root := f.zettel.seed("", 0)
	f.open()

	// Act
	for i := 0; i < 2; i++ {
		if err := f.svc.Show(context.Background(), thr, NodeID(root)); err != nil {
			t.Fatal(err)
		}
	}

	// Assert
	if got := f.view().GetConversation().GetReveal().GetSequence(); got != 2 {
		t.Fatalf("sequence = %d, want 2", got)
	}
}

func TestShowingAnUnknownNodeIsRefusedAndRevealsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.open()

	// Act
	err := f.svc.Show(context.Background(), thr, "nope")

	// Assert
	if !errors.Is(err, ErrUnknownNode) || f.view().GetConversation().GetReveal() != nil {
		t.Fatalf("Show = %v, reveal = %v; want unknown_node and no reveal", err, f.view().GetConversation().GetReveal())
	}
}

func TestAThreadWithNoQuestionHasNoOutline(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	f.open()

	// Assert
	if len(f.outlines) != 0 {
		t.Fatalf("outlines = %v, want none handed for an empty thread", f.outlines)
	}
}
