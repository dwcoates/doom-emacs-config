package feed

import (
	"slices"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
)

// forkPointRow is the fork-point divider's row id for the generation whose
// book is BOOK.
func forkPointRow(book string) string {
	return testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindSeparation, ID: "fork_point:" + book},
	}).GetValue()
}

func TestAForksWalkDrawsAGreyDividerBetweenInheritedAndOwnRows(t *testing.T) {
	// Arrange
	h, _ := forkHarness(t, 10, bookTurn(mainAgent(), "own-1", "own-answer"),
		[]LineageSegment{segment("ws-parent", parentBook, 10_000)},
		map[string][]bookEntry{parentBook.GetValue(): bookTurn(parentBook, "parent-1", "parent-answer")})

	// Act
	rows := h.walkRows("reader-1")

	// Assert: below the inherited answer, above the fork's own.
	got := rowIDs(rows)
	divider := slices.Index(got, forkPointRow(parentBook.GetValue()))
	if divider < 0 {
		t.Fatalf("rows = %v, want the fork-point divider", got)
	}
	if divider < slices.Index(got, root("parent-answer")) || divider > slices.Index(got, root("own-answer")) {
		t.Fatalf("rows = %v, want the divider between the inherited answer and the fork's own", got)
	}
	separation := rows[divider].GetSeparation()
	if _, ok := separation.GetKind().(*frontendv1.FeedSessionSeparation_Forked); !ok {
		t.Fatalf("divider kind = %T, want forked", separation.GetKind())
	}
	if separation.GetLabel().GetText() != forkPointLabel || separation.GetTokens() != nil {
		t.Fatalf("divider = %v, want the fork-point label and no token figure", separation)
	}
}

func TestAForkPointDividerCutsNoContext(t *testing.T) {
	// Arrange
	row := &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Separation{Separation: &frontendv1.FeedSessionSeparation{
		Kind: &frontendv1.FeedSessionSeparation_Forked{Forked: &frontendv1.FeedContextForked{}},
	}}}

	// Act and Assert: it bounds neither the feed's opening nor its history.
	if cutsContext(row) || endsHistory(row) {
		t.Fatal("the fork-point divider must not cut context or end history")
	}
}

func TestAForksWalkDrawsOneDividerPerGeneration(t *testing.T) {
	// Arrange
	h, _ := forkHarness(t, 2, bookTurn(mainAgent(), "own-1", "own"),
		[]LineageSegment{
			segment("ws-parent", parentBook, 9_900),
			segment("ws-grandparent", grandparentBook, 8_900),
		},
		map[string][]bookEntry{
			parentBook.GetValue():      bookTurn(parentBook, "parent-1", "from-parent"),
			grandparentBook.GetValue(): bookTurn(grandparentBook, "grand-1", "from-grandparent"),
		})

	// Act
	got := rowIDs(h.walkRows("reader-1"))

	// Assert: grandparent rows, its divider, parent rows, its divider, own.
	want := []string{
		root("from-grandparent"), forkPointRow(grandparentBook.GetValue()),
		root("from-parent"), forkPointRow(parentBook.GetValue()),
		root("own"),
	}
	var order []string
	for _, id := range got {
		if slices.Contains(want, id) {
			order = append(order, id)
		}
	}
	if !slices.Equal(order, want) {
		t.Fatalf("order = %v, want %v (rows %v)", order, want, got)
	}
}

func TestAWorkspaceThatIsNoForkDrawsNoForkPointDivider(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.mainBook(10, bookTurn(mainAgent(), "turn-1", "unit-1"))

	// Act
	rows := h.walkRows("reader-1")

	// Assert
	for _, row := range rows {
		if _, ok := row.GetSeparation().GetKind().(*frontendv1.FeedSessionSeparation_Forked); ok {
			t.Fatalf("rows = %v, want no fork-point divider", rowIDs(rows))
		}
	}
}
