package sidebar

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// flatIDs names the workspace ids of a flattened region, in order.
func flatIDs(rows []*frontendv1.RosterRow) []string {
	var ids []string
	for _, row := range FlattenRows(rows) {
		ids = append(ids, row.GetWorkspace().GetWorkspace().GetId())
	}
	return ids
}

func TestFlattenRows(t *testing.T) {
	cases := []struct {
		name string
		rows []*frontendv1.RosterRow
		want []string
	}{
		{name: "an empty region lists nothing", rows: nil, want: nil},
		{name: "flat rows keep their order", rows: []*frontendv1.RosterRow{tabRow("a", false), tabRow("b", false)}, want: []string{"a", "b"}},
		{name: "a child follows its parent, before the parent's next sibling",
			rows: []*frontendv1.RosterRow{tabRow("a", false, tabRow("a1", false)), tabRow("b", false)},
			want: []string{"a", "a1", "b"}},
		{name: "a grandchild is listed at its depth",
			rows: []*frontendv1.RosterRow{tabRow("a", false, tabRow("a1", false, tabRow("a1x", false)), tabRow("a2", false))},
			want: []string{"a", "a1", "a1x", "a2"}},
		{name: "a closed row and its children are still listed",
			rows: []*frontendv1.RosterRow{tabRow("a", true, tabRow("a1", false))},
			want: []string{"a", "a1"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := flatIDs(tc.rows)

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("FlattenRows = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestFlattenRowsIsTheOnlyChildWalk pins that no reader in this module walks
// RosterRow.children by hand: a hand-rolled walk is exactly how a reader came
// to miss nested (child) workspaces while the others saw them.
func TestFlattenRowsIsTheOnlyChildWalk(t *testing.T) {
	// Arrange: the daemon module root, three directories up.
	root, err := filepath.Abs(filepath.Join("..", "..", ".."))
	if err != nil {
		t.Fatalf("resolve the module root: %v", err)
	}
	var offenders []string

	// Act.
	err = filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if filepath.Base(path) == "flatten.go" && filepath.Base(filepath.Dir(path)) == "sidebar" {
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
		t.Fatalf("walk the module: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these files walk RosterRow.children by hand; flatten through sidebar.FlattenRows instead: %v", offenders)
	}
}
