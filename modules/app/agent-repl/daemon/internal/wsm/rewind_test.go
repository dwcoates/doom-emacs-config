package wsm

import (
	"context"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// layoutUndos strips one layout's additions from a migrated file, leaving its
// stamp alone, keyed by the layout that added them. Every layout from the
// oldest listed one up to LayoutVersion has its entry, so a test that rewinds a
// file past a new layout fails until the layout's undo is listed here.
var layoutUndos = map[int]func(*testing.T, string){
	39: undoLayout39,
	40: undoLayout40,
	41: undoLayout41,
	42: undoLayout42,
	43: undoLayout43,
	44: undoLayout44,
}

// undoLayoutsAbove strips the additions of every layout above BASE from a
// migrated file, newest first, leaving its stamp alone: the one way a test
// rewinds a file to an older layout's shape.
func undoLayoutsAbove(t *testing.T, path string, base int) {
	t.Helper()
	for layout := LayoutVersion; layout > base; layout-- {
		undo, ok := layoutUndos[layout]
		if !ok {
			t.Fatalf("layout %d has no undo in layoutUndos; a test cannot rewind a file to layout %d", layout, base)
		}
		undo(t, path)
	}
}

// migratedFile answers the path of a file migrated to LayoutVersion.
func migratedFile(t *testing.T) string {
	t.Helper()
	path := layout3Fixture(t)
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	handle.Close()
	return path
}

func TestEveryLayoutFromTheOldestUndoneHasAnUndo(t *testing.T) {
	// Arrange
	oldest := LayoutVersion
	for layout := range layoutUndos {
		oldest = min(oldest, layout)
	}

	// Act / Assert
	for layout := oldest; layout <= LayoutVersion; layout++ {
		if _, ok := layoutUndos[layout]; !ok {
			t.Errorf("layout %d has no undo, want every layout from %d to %d listed", layout, oldest, LayoutVersion)
		}
	}
	if _, ok := layoutUndos[LayoutVersion+1]; ok {
		t.Errorf("layoutUndos lists layout %d, past this build's layout", LayoutVersion+1)
	}
}

func TestUndoLayoutsAboveStripsOnlyTheLayoutsAboveItsBase(t *testing.T) {
	tests := []struct {
		name     string
		base     int
		wantGone []string
		wantKept []string
	}{
		{name: "the newest layout alone", base: LayoutVersion - 1,
			wantGone: []string{"merge_compact_after"}, wantKept: []string{"merge_fork_head", "merge_revealed"}},
		{name: "down to layout 40", base: 40,
			wantGone: []string{"merge_compact_after", "merge_fork_head"}, wantKept: []string{"merge_revealed"}},
		{name: "down to layout 38", base: 38,
			wantGone: []string{"merge_compact_after", "merge_fork_head", "merge_revealed"}},
		{name: "nothing above the current layout", base: LayoutVersion,
			wantKept: []string{"merge_compact_after", "merge_fork_head", "merge_revealed"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := migratedFile(t)

			// Act
			undoLayoutsAbove(t, path, tt.base)

			// Assert
			for _, column := range tt.wantGone {
				if workspacesHasColumn(t, path, column) {
					t.Errorf("workspaces.%s survived a rewind to layout %d", column, tt.base)
				}
			}
			for _, column := range tt.wantKept {
				if !workspacesHasColumn(t, path, column) {
					t.Errorf("workspaces.%s was stripped by a rewind to layout %d", column, tt.base)
				}
			}
		})
	}
}

// workspacesHasColumn reports whether the file's workspaces table has COLUMN.
func workspacesHasColumn(t *testing.T, path, column string) bool {
	t.Helper()
	return rawScalar[int](t, path, `SELECT count(*) FROM pragma_table_info('workspaces') WHERE name = ?`, column) == 1
}

func TestEveryRewindGoesThroughUndoLayoutsAbove(t *testing.T) {
	// Arrange: every test file of this package but the one holding the undos'
	// table.
	names, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	direct := regexp.MustCompile(`(?m)^\s*undoLayout\d+\(t, `)

	// Act / Assert: no test strips a layout by calling its undo itself, so no
	// rewind can skip a layout's additions.
	for _, name := range names {
		if name == "rewind_test.go" {
			continue
		}
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		for _, line := range direct.FindAllString(string(source), -1) {
			t.Errorf("%s rewinds a layout by hand (%s), want undoLayoutsAbove", name, strings.TrimSpace(line))
		}
	}
}
