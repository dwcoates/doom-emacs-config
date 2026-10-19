package sidebar_test

import (
	"fmt"
	"testing"

	"agentrepl/protohelpers/rosterwalk"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// openRowsAndNumbers answers the Open section's rows top to bottom, as the
// sidebar draws them and the tab bar walks them (a folded repository's left
// out), and each one's tab number.
func openRowsAndNumbers(roster *frontendv1.WorkspaceRoster) (rows, numbers []string) {
	rows, numbers = []string{}, []string{}
	for _, open := range rosterwalk.OpenRows(roster) {
		if open.GetTab().GetFolded() != nil {
			continue
		}
		rows = append(rows, open.GetRow().GetWorkspace().GetWorkspace().GetId())
		numbers = append(numbers, fmt.Sprint(open.GetTab().GetNumbered().GetNumber()))
	}
	return rows, numbers
}

// numberingRegistry is two repositories whose workspaces interleave in the
// registry, a subsection, a family placed apart, a hidden workspace, a closed
// one, a merged and reopened one, and a folded repository's workspace.
func numberingRegistry() sidebar.Registry {
	later := subsection("sub-later", "later", 1)
	b1 := placed(workspace("b1", "b1"), 2)
	b1.Repo = otherRepo.ID
	parent := placed(workspace("p", "parent"), 6)
	kid := placed(workspace("k", "kid"), 1)
	kid.Parent = &parent.ID
	reopened := placed(workspace("m", "reopened"), 9)
	reopened.MergedAt = at(0)
	closed := placed(workspace("x", "closed"), 4)
	closed.Closed = true
	folded := wsm.Repository{ID: "repo-3", Dir: "/repos/three", Name: "gamma", DefaultBranch: "main", Folded: true, Place: 3}
	c1 := placed(workspace("c1", "c1"), 8)
	c1.Repo = folded.ID
	reg := registry(placed(workspace("a1", "a1"), 3), b1, in(placed(workspace("a2", "a2"), 5), later), parent, kid,
		placed(hiddenWorkspace("h", "h"), 7), c1, reopened, closed)
	reg.Repositories = append(reg.Repositories, otherRepo, folded)
	reg.Subsections = []wsm.Subsection{later}
	return reg
}

func TestNumbersMatchTheOpenRowsDrawnOrder(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(numberingRegistry())

	// Assert: read down the Open section, the rows are numbered 1, 2, 3, ...
	// with no gap, so tab N is the row drawn immediately before tab N+1.
	rows, numbers := openRowsAndNumbers(latest(t, r))
	if want := []string{"a1", "p", "k", "m", "a2", "b1"}; !equal(rows, want) {
		t.Fatalf("open rows = %v, want %v", rows, want)
	}
	if want := []string{"1", "2", "3", "4", "5", "6"}; !equal(numbers, want) {
		t.Fatalf("numbers = %v, want %v", numbers, want)
	}
}

func TestAFoldedRepositorysOpenRowsHaveAFoldedTab(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(numberingRegistry())

	// Assert.
	for _, open := range rosterwalk.OpenRows(latest(t, r)) {
		if open.GetRow().GetWorkspace().GetWorkspace().GetId() == "c1" && open.GetTab().GetFolded() == nil {
			t.Fatalf("c1's tab = %v, want folded", open.GetTab())
		}
	}
}

func TestEveryWorkspaceIsInOneList(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(numberingRegistry())

	// Assert: the hidden one only under Hidden, the reopened merge under Open
	// (OPEN BEATS MERGED), the closed one only under Closed.
	roster := latest(t, r)
	seen := map[string]int{}
	for _, row := range rosterwalk.AllRows(roster) {
		seen[row.GetWorkspace().GetWorkspace().GetId()]++
	}
	for id, n := range seen {
		if n != 1 {
			t.Fatalf("%s is listed %d times, want once", id, n)
		}
	}
	if rowFor(hiddenRows(roster), "h") == nil || rowFor(rosterwalk.MergedRows(roster), "m") != nil || rowFor(rosterwalk.ClosedRows(roster), "x") == nil {
		t.Fatalf("lists = %v, want h hidden, m open, x closed", seen)
	}
}

func TestAnOpenChildOfAClosedParentTakesItsParentsPlace(t *testing.T) {
	// Arrange: a family that predates the rule against closing a parent with
	// open children.
	r, _ := newResolver(t)
	first := placed(workspace("first", "first"), 1)
	parent := placed(workspace("p", "parent"), 2)
	parent.Closed = true
	kid := placed(workspace("k", "kid"), 9)
	kid.Parent = &parent.ID
	last := placed(workspace("last", "last"), 3)

	// Act.
	r.SetRegistry(registry(first, parent, kid, last))

	// Assert.
	if rows, _ := openRowsAndNumbers(latest(t, r)); !equal(rows, []string{"first", "k", "last"}) {
		t.Fatalf("open rows = %v, want the kid where its closed parent stood", rows)
	}
}
