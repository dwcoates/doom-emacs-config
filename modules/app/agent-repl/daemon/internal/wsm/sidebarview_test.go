package wsm

import (
	"context"
	"path/filepath"
	"testing"
)

func TestSidebarViewDefaultsWhenUnset(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.SidebarView(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("SidebarView: %v", err)
	}
	if got != DefaultSidebarView {
		t.Fatalf("view = %+v, want the default %+v", got, DefaultSidebarView)
	}
}

func TestSidebarViewWritesAreReadBack(t *testing.T) {
	tests := []struct {
		name  string
		write func(*store) error
		want  SidebarView
	}{
		{
			name:  "folding the band keeps the other folds",
			write: func(s *store) error { return s.SetMergedSectionFolded(context.Background(), true) },
			want:  SidebarView{MergedFolded: true},
		},
		{
			name: "a second write keeps the first",
			write: func(s *store) error {
				if err := s.SetHiddenSectionFolded(context.Background(), true); err != nil {
					return err
				}
				return s.SetMergedSectionFolded(context.Background(), true)
			},
			want: SidebarView{MergedFolded: true, HiddenFolded: true},
		},
		{
			name:  "folding the merge queue section keeps the band",
			write: func(s *store) error { return s.SetMergeQueueSectionFolded(context.Background(), true) },
			want:  SidebarView{MergeQueueFolded: true},
		},
		{
			name: "unfolding the merge queue section again replaces its fold",
			write: func(s *store) error {
				if err := s.SetMergeQueueSectionFolded(context.Background(), true); err != nil {
					return err
				}
				return s.SetMergeQueueSectionFolded(context.Background(), false)
			},
			want: SidebarView{},
		},
		{
			name:  "folding the hidden section keeps the band",
			write: func(s *store) error { return s.SetHiddenSectionFolded(context.Background(), true) },
			want:  SidebarView{HiddenFolded: true},
		},
		{
			name:  "folding the open section keeps the band and the hidden section",
			write: func(s *store) error { return s.SetOpenSectionFolded(context.Background(), true) },
			want:  SidebarView{OpenFolded: true},
		},
		{
			name: "unfolding the open section again replaces its fold",
			write: func(s *store) error {
				if err := s.SetOpenSectionFolded(context.Background(), true); err != nil {
					return err
				}
				return s.SetOpenSectionFolded(context.Background(), false)
			},
			want: SidebarView{},
		},
		{
			name: "unfolding the hidden section again replaces its fold",
			write: func(s *store) error {
				if err := s.SetHiddenSectionFolded(context.Background(), true); err != nil {
					return err
				}
				return s.SetHiddenSectionFolded(context.Background(), false)
			},
			want: SidebarView{},
		},
		{
			name: "sizing two bands at once keeps the folds",
			write: func(s *store) error {
				if err := s.SetHiddenSectionFolded(context.Background(), true); err != nil {
					return err
				}
				return s.SetSectionHeights(context.Background(), []SectionHeightChange{
					{Section: BandMergeQueue, Height: SizedHeight(4)},
					{Section: BandHidden, Height: SizedHeight(0)},
				})
			},
			want: SidebarView{HiddenFolded: true, MergeQueueHeight: SizedHeight(4), HiddenHeight: SizedHeight(0)},
		},
		{
			name: "sizing the recently merged section before anything else mints the view",
			write: func(s *store) error {
				return s.SetSectionHeights(context.Background(), []SectionHeightChange{{Section: BandMerged, Height: SizedHeight(12)}})
			},
			want: SidebarView{MergedHeight: SizedHeight(12)},
		},
		{
			name: "fitting a sized section again replaces its height and keeps the others",
			write: func(s *store) error {
				if err := s.SetSectionHeights(context.Background(), []SectionHeightChange{
					{Section: BandMerged, Height: SizedHeight(2)},
					{Section: BandHidden, Height: SizedHeight(3)},
				}); err != nil {
					return err
				}
				return s.SetSectionHeights(context.Background(), []SectionHeightChange{{Section: BandMerged, Height: FittedHeight()}})
			},
			want: SidebarView{HiddenHeight: SizedHeight(3)},
		},
		{
			name: "unfolding the band again replaces the fold",
			write: func(s *store) error {
				if err := s.SetMergedSectionFolded(context.Background(), true); err != nil {
					return err
				}
				return s.SetMergedSectionFolded(context.Background(), false)
			},
			want: SidebarView{MergedFolded: false},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			err := tt.write(s)

			// Assert
			if err != nil {
				t.Fatalf("write: %v", err)
			}
			got, err := s.SidebarView(context.Background())
			if err != nil || got != tt.want {
				t.Fatalf("SidebarView = %+v, %v; want %+v", got, err, tt.want)
			}
			if rows := scalar[int](t, s, `SELECT count(*) FROM sidebar_view`); rows != 1 {
				t.Fatalf("sidebar_view rows = %d, want the one singleton", rows)
			}
		})
	}
}

func TestSidebarViewSurvivesAReopen(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	first, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := first.SetHiddenSectionFolded(context.Background(), true); err != nil {
		t.Fatalf("SetHiddenSectionFolded: %v", err)
	}
	if err := first.SetOpenSectionFolded(context.Background(), true); err != nil {
		t.Fatalf("SetOpenSectionFolded: %v", err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	second, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close()
	view, err := second.SidebarView(context.Background())

	// Assert
	want := SidebarView{HiddenFolded: true, OpenFolded: true}
	if err != nil || view != want {
		t.Fatalf("view after reopen = %+v, %v; want %+v", view, err, want)
	}
}

// TestTheMigrationAddsTheSidebarView pins the layout-23 step.
func TestTheMigrationAddsTheSidebarView(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 22)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-22 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('tasks') WHERE name = 'folded'`); got != 1 {
		t.Fatalf("tasks.folded columns after the migration = %d, want 1", got)
	}
	if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'sidebar_view'`); got != 1 {
		t.Fatalf("sidebar_view tables after the migration = %d, want 1", got)
	}
}

func TestTheDefaultViewExpandsTheHiddenSection(t *testing.T) {
	// Arrange, Act, Assert
	if DefaultSidebarView.HiddenFolded {
		t.Fatalf("DefaultSidebarView = %+v, want the hidden section expanded", DefaultSidebarView)
	}
}

// TestTheMigrationAddsTheHiddenWorkspaces pins the layout-32 step.
func TestTheMigrationAddsTheHiddenWorkspaces(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 31)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-31 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	for _, column := range []struct{ table, name string }{{"workspaces", "hidden"}, {"sidebar_view", "hidden_folded"}} {
		if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info(?) WHERE name = ?`, column.table, column.name); got != 1 {
			t.Fatalf("%s.%s columns after the migration = %d, want 1", column.table, column.name, got)
		}
	}
}

func TestTheDefaultViewExpandsTheOpenSection(t *testing.T) {
	// Arrange, Act, Assert
	if DefaultSidebarView.OpenFolded {
		t.Fatalf("DefaultSidebarView = %+v, want the open section expanded", DefaultSidebarView)
	}
}

// TestTheMigrationAddsTheOpenSectionFold pins the layout-46 step.
func TestTheMigrationAddsTheOpenSectionFold(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 45)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-45 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('sidebar_view') WHERE name = 'open_folded'`); got != 1 {
		t.Fatalf("sidebar_view.open_folded columns after the migration = %d, want 1", got)
	}
	view, err := s.SidebarView(context.Background())
	if err != nil {
		t.Fatalf("SidebarView after the migration: %v", err)
	}
	if view.OpenFolded {
		t.Fatalf("SidebarView after the migration = %+v, want the open section expanded", view)
	}
}

// TestAMigratedViewKeepsItsFoldsWhenTheShowsColumnLands pins that the
// layout-76 step leaves a view someone had already changed as it stood.
func TestAMigratedViewKeepsItsFoldsWhenTheShowsColumnLands(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 75)
	execRaw(t, path, `INSERT INTO sidebar_view (id, merged_folded, grouping, hidden_folded, open_folded) VALUES (1, 1, 'repository', 1, 1)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-75 database: %v", err)
	}
	defer handle.Close()
	view, err := handle.(*store).SidebarView(context.Background())

	// Assert
	want := SidebarView{MergedFolded: true, HiddenFolded: true, OpenFolded: true}
	if err != nil || view != want {
		t.Fatalf("SidebarView after the migration = %+v, %v; want %+v", view, err, want)
	}
}

// TestTheMigrationAddsTheMergeQueueSectionFold pins the layout-97 step: a
// view someone had already changed keeps its folds, and the Merge Queue
// section lands expanded.
func TestTheMigrationAddsTheMergeQueueSectionFold(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 96)
	execRaw(t, path, `INSERT INTO sidebar_view (id, merged_folded, grouping, hidden_folded, open_folded) VALUES (1, 1, 'repository', 1, 0)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-96 database: %v", err)
	}
	defer handle.Close()
	view, err := handle.(*store).SidebarView(context.Background())

	// Assert
	want := SidebarView{MergedFolded: true, HiddenFolded: true}
	if err != nil || view != want {
		t.Fatalf("SidebarView after the migration = %+v, %v; want %+v", view, err, want)
	}
}

func TestTheDefaultViewExpandsTheMergeQueueSection(t *testing.T) {
	if DefaultSidebarView.MergeQueueFolded {
		t.Fatal("the default view folds the Merge Queue section; a merge in flight must be in sight")
	}
}

func TestSidebarViewRefusesAStoredHeightOutOfRange(t *testing.T) {
	tests := []struct {
		name  string
		lines int64
	}{{"negative", -1}, {"beyond uint32", 1 << 33}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			if err := s.SetSectionHeights(context.Background(), []SectionHeightChange{{Section: BandHidden, Height: SizedHeight(1)}}); err != nil {
				t.Fatalf("SetSectionHeights: %v", err)
			}
			if _, err := s.db().Exec(`UPDATE sidebar_view SET hidden_lines = ? WHERE id = 1`, tt.lines); err != nil {
				t.Fatalf("corrupt the height: %v", err)
			}

			// Act
			_, err := s.SidebarView(context.Background())

			// Assert
			if err == nil {
				t.Fatalf("SidebarView read a height of %d lines without error", tt.lines)
			}
		})
	}
}

func TestSetSectionHeightsRefusesNoChange(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("SetSectionHeights handed no change did not panic")
		}
	}()

	// Act
	_ = s.SetSectionHeights(context.Background(), nil)
}

func TestSetSectionHeightsRefusesAnUnknownSection(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("SetSectionHeights handed an unknown section did not panic")
		}
	}()

	// Act
	_ = s.SetSectionHeights(context.Background(), []SectionHeightChange{{Section: "open_lines", Height: SizedHeight(1)}})
}
