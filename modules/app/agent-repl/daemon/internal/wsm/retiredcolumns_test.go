package wsm

import (
	"context"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// columnSet answers TABLE's columns in the file at PATH, sorted: which
// columns a route leaves, apart from the order an ALTER appends them in,
// which the two routes to a layout legitimately differ in.
func columnSet(t *testing.T, path, table string) []string {
	t.Helper()
	columns := strings.Split(tableColumns(t, path, table), ",")
	slices.Sort(columns)
	return columns
}

// openMigrated opens the file at PATH, migrating it to this build's layout,
// and closes it.
func openMigrated(t *testing.T, path string) {
	t.Helper()
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := handle.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
}

// TestTheRetiredColumnsStepDropsEveryRetiredColumn pins the layout-140 step:
// every column no statement reads or writes is gone from a migrated file.
func TestTheRetiredColumnsStepDropsEveryRetiredColumn(t *testing.T) {
	tests := []struct {
		table  string
		column string
	}{
		{table: "workspaces", column: "principal_link"},
		{table: "sidebar_view", column: "open_shows_merge_queues"},
		{table: "sidebar_view", column: "backlog_folded"},
		{table: "sidebar_view", column: "in_progress_folded"},
		{table: "sidebar_view", column: "merging_folded"},
		{table: "sidebar_view", column: "viewed_folded"},
		{table: "sidebar_view", column: "grouping"},
	}
	for _, tt := range tests {
		t.Run(tt.table+"."+tt.column, func(t *testing.T) {
			// Arrange
			path := fixtureAt(t, stepNamed(t, "retired_columns_dropped")-1)

			// Act
			openMigrated(t, path)

			// Assert
			columns := strings.Split(tableColumns(t, path, tt.table), ",")
			for _, c := range columns {
				if c == tt.column {
					t.Fatalf("%s columns after the migration = %v, want no %s", tt.table, columns, tt.column)
				}
			}
		})
	}
}

// TestTheRetiredColumnsStepKeepsAViewSomeoneChanged pins that dropping the
// retired columns leaves every fold the view still holds as it stood.
func TestTheRetiredColumnsStepKeepsAViewSomeoneChanged(t *testing.T) {
	// Arrange
	path := fixtureAt(t, stepNamed(t, "retired_columns_dropped")-1)
	execRaw(t, path, `INSERT INTO sidebar_view (id, merged_folded, grouping, hidden_folded, open_folded, merge_queue_folded, open_shows_merge_queues)
VALUES (1, 1, 'repository', 1, 0, 1, 1)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()
	view, err := handle.(*store).SidebarView(context.Background())

	// Assert
	want := SidebarView{MergedFolded: true, HiddenFolded: true, MergeQueueFolded: true}
	if err != nil || view != want {
		t.Fatalf("SidebarView after the migration = %+v, %v; want %+v", view, err, want)
	}
}

// TestAMigratedFileMatchesAFreshOneOnceTheRetiredColumnsAreDropped pins that
// the two routes to this layout leave the tables the step reshapes in one
// shape.
func TestAMigratedFileMatchesAFreshOneOnceTheRetiredColumnsAreDropped(t *testing.T) {
	for _, table := range []string{"workspaces", "sidebar_view"} {
		t.Run(table, func(t *testing.T) {
			// Arrange
			migrated := fixtureAt(t, stepNamed(t, "retired_columns_dropped")-1)
			openMigrated(t, migrated)
			fresh := filepath.Join(t.TempDir(), "fresh.db")
			openMigrated(t, fresh)

			// Act
			got, want := columnSet(t, migrated, table), columnSet(t, fresh, table)

			// Assert
			if !slices.Equal(got, want) {
				t.Fatalf("migrated %s columns = %q, a fresh file's = %q; the two routes drifted", table, got, want)
			}
		})
	}
}

// TestTheRetiredColumnsStepIsBreaking pins the step's kind: the build before
// it inserts sidebar_view's grouping, so the deploy that crosses it restarts.
func TestTheRetiredColumnsStepIsBreaking(t *testing.T) {
	// Arrange
	to := stepNamed(t, "retired_columns_dropped")

	// Act
	var kind MigrationKind
	for _, m := range migrations {
		if m.To == to {
			kind = m.Kind
		}
	}

	// Assert
	if kind != MigrationBreaking {
		t.Fatalf("the retired-columns step is %v, want breaking", kind)
	}
}

// TestAFreshViewWriteMintsTheRowWithoutTheDroppedGrouping pins the one write
// that named a dropped column: minting the sidebar_view row.
func TestAFreshViewWriteMintsTheRowWithoutTheDroppedGrouping(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetHiddenSectionFolded(context.Background(), true)

	// Assert
	if err != nil {
		t.Fatalf("SetHiddenSectionFolded: %v", err)
	}
	view, err := s.SidebarView(context.Background())
	if want := (SidebarView{HiddenFolded: true}); err != nil || view != want {
		t.Fatalf("SidebarView = %+v, %v; want %+v", view, err, want)
	}
}
