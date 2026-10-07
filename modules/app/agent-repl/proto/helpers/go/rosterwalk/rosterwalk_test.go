package rosterwalk

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// row is a roster row for id with children.
func row(id string, children ...*frontendv1.RosterRow) *frontendv1.RosterRow {
	return &frontendv1.RosterRow{
		Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id}},
		Children:  children,
	}
}

// ids names the workspace ids of rows, in order.
func ids(rows []*frontendv1.RosterRow) []string {
	var out []string
	for _, r := range rows {
		out = append(out, r.GetWorkspace().GetWorkspace().GetId())
	}
	return out
}

// rowsOf wraps rows in a rows region.
func rowsOf(rows ...*frontendv1.RosterRow) *frontendv1.RosterRows {
	return &frontendv1.RosterRows{Rows: rows}
}

func TestFlattenRows(t *testing.T) {
	cases := []struct {
		name string
		rows []*frontendv1.RosterRow
		want []string
	}{
		{name: "an empty region lists nothing", rows: nil, want: nil},
		{name: "flat rows keep their order", rows: []*frontendv1.RosterRow{row("a"), row("b")}, want: []string{"a", "b"}},
		{name: "a child follows its parent, before the parent's next sibling",
			rows: []*frontendv1.RosterRow{row("a", row("a1")), row("b")}, want: []string{"a", "a1", "b"}},
		{name: "a grandchild is listed at its depth",
			rows: []*frontendv1.RosterRow{row("a", row("a1", row("a1x")), row("a2"))}, want: []string{"a", "a1", "a1x", "a2"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := ids(FlattenRows(tc.rows))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("FlattenRows = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAllRows(t *testing.T) {
	cases := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		want   []string
	}{
		{name: "a nil roster lists nothing", roster: nil, want: nil},
		{name: "repository sections, then task sections, then recently merged, in that order",
			roster: &frontendv1.WorkspaceRoster{
				RecentlyMerged: &frontendv1.RosterMergedSection{Rows: rowsOf(row("m"))},
				Task: &frontendv1.RosterTaskView{Sections: []*frontendv1.RosterTaskSection{
					{Rows: rowsOf(row("t"))}}},
				Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
					{Rows: rowsOf(row("r1"))}, {Rows: rowsOf(row("r2"))}}},
			},
			want: []string{"r1", "r2", "t", "m"}},
		{name: "nested rows are flattened in every grouping",
			roster: &frontendv1.WorkspaceRoster{
				Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
					{Rows: rowsOf(row("r", row("r1")))}}},
				Task: &frontendv1.RosterTaskView{Sections: []*frontendv1.RosterTaskSection{
					{Rows: rowsOf(row("t", row("t1")))}}},
				RecentlyMerged: &frontendv1.RosterMergedSection{Rows: rowsOf(row("m", row("m1")))},
			},
			want: []string{"r", "r1", "t", "t1", "m", "m1"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := ids(AllRows(tc.roster))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("AllRows = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestRosterwalkIsTheOnlyChildWalk pins that no Go reader anywhere in
// agent-repl walks RosterRow.children by hand: a hand-rolled walk is exactly
// how one reader came to miss child workspaces while the others saw them.
func TestRosterwalkIsTheOnlyChildWalk(t *testing.T) {
	// Arrange: the agent-repl root, four directories up.
	root, err := filepath.Abs(filepath.Join("..", "..", "..", ".."))
	if err != nil {
		t.Fatalf("resolve the agent-repl root: %v", err)
	}
	if _, err := os.Stat(filepath.Join(root, "daemon", "go.mod")); err != nil {
		t.Fatalf("%s is not the agent-repl root: %v", root, err)
	}
	generated := filepath.Join(root, "proto", "gen")
	self, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("resolve this package: %v", err)
	}
	var offenders []string

	// Act.
	err = filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if path == generated || path == self || d.Name() == "node_modules" || d.Name() == ".git" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), "GetChildren()") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk agent-repl: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these files walk RosterRow.children by hand; flatten through rosterwalk instead: %v", offenders)
	}
}
