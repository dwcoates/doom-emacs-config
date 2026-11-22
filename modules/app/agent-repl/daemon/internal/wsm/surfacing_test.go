package wsm

import (
	"context"
	"testing"
)

func surfacedOf(t *testing.T, s *store, id WorkspaceID) bool {
	t.Helper()
	return scalar[bool](t, s, `SELECT surfaced FROM workspaces WHERE id = ?`, id)
}

func TestSurfaceHeldWorkMarksAWorkspaceAFoldHides(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(*testing.T, *store, selectionFoldWorld)
		pick    func(selectionFoldWorld) Workspace
		want    bool
	}{
		{name: "a root under a folded repository and subsection", pick: func(w selectionFoldWorld) Workspace { return w.root }, want: true},
		{name: "a child under its family's folded subsection", pick: func(w selectionFoldWorld) Workspace { return w.child }, want: true},
		{name: "a workspace under a folded repository alone", pick: func(w selectionFoldWorld) Workspace { return w.other }, want: true},
		{
			name: "a root whose repository is open but whose subsection is folded",
			arrange: func(t *testing.T, s *store, w selectionFoldWorld) {
				if err := s.SetRepositoryFolded(context.Background(), w.root.Repo, false); err != nil {
					t.Fatalf("SetRepositoryFolded: %v", err)
				}
			},
			pick: func(w selectionFoldWorld) Workspace { return w.root },
			want: true,
		},
		{
			name: "a workspace no fold hides",
			arrange: func(t *testing.T, s *store, w selectionFoldWorld) {
				for _, repo := range []RepoID{w.root.Repo, w.other.Repo} {
					if err := s.SetRepositoryFolded(context.Background(), repo, false); err != nil {
						t.Fatalf("SetRepositoryFolded: %v", err)
					}
				}
				if err := s.SetSubsectionFolded(context.Background(), w.sub.ID, false); err != nil {
					t.Fatalf("SetSubsectionFolded: %v", err)
				}
			},
			pick: func(w selectionFoldWorld) Workspace { return w.root },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newSelectionFoldWorld(t, s)
			if tt.arrange != nil {
				tt.arrange(t, s, w)
			}
			ws := tt.pick(w)

			// Act
			got, err := s.SurfaceHeldWork(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("SurfaceHeldWork: %v", err)
			}
			if got != tt.want || surfacedOf(t, s, ws.ID) != tt.want {
				t.Errorf("surfaced = %v (stored %v), want %v", got, surfacedOf(t, s, ws.ID), tt.want)
			}
		})
	}
}

func TestSurfaceHeldWorkNeverUnfoldsTheFoldThatHidesIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newSelectionFoldWorld(t, s)

	// Act
	if _, err := s.SurfaceHeldWork(context.Background(), w.root.ID); err != nil {
		t.Fatalf("SurfaceHeldWork: %v", err)
	}

	// Assert
	if !repositoryFolded(t, s, w.root.Repo) || !subsectionNow(t, s, w.sub.ID).Folded {
		t.Error("a turn starting under a fold unfolded it")
	}
}

func TestSurfaceHeldWorkUnderAChildrenFold(t *testing.T) {
	tests := []struct {
		name string
		pick func(childrenFoldWorld) Workspace
		want bool
	}{
		{name: "a child is hidden by its parent's fold", pick: func(w childrenFoldWorld) Workspace { return w.child }, want: true},
		{name: "a grandchild is hidden by its ancestor's fold", pick: func(w childrenFoldWorld) Workspace { return w.grandchild }, want: true},
		{name: "the workspace whose children are folded is not hidden by it", pick: func(w childrenFoldWorld) Workspace { return w.root }},
		{name: "an unrelated workspace is not hidden", pick: func(w childrenFoldWorld) Workspace { return w.sibling }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newChildrenFoldWorld(t, s)
			if err := s.SetChildrenFolded(context.Background(), w.root.ID, true); err != nil {
				t.Fatalf("SetChildrenFolded: %v", err)
			}

			// Act
			got, err := s.SurfaceHeldWork(context.Background(), tt.pick(w).ID)

			// Assert
			if err != nil {
				t.Fatalf("SurfaceHeldWork: %v", err)
			}
			if got != tt.want {
				t.Errorf("surfaced = %v, want %v", got, tt.want)
			}
			if childrenFoldedOf(t, s, w.root.ID) != true {
				t.Error("a turn starting under a children fold unfolded it")
			}
		})
	}
}

func TestSurfaceHeldWorkOfAMarkedWorkspaceAnswersNothingNew(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newSelectionFoldWorld(t, s)
	if _, err := s.SurfaceHeldWork(context.Background(), w.root.ID); err != nil {
		t.Fatalf("SurfaceHeldWork: %v", err)
	}

	// Act
	got, err := s.SurfaceHeldWork(context.Background(), w.root.ID)

	// Assert
	if err != nil || got {
		t.Fatalf("SurfaceHeldWork = %v, %v; want false, nil", got, err)
	}
}

func TestSurfaceHeldWorkOfAnUnknownWorkspaceTouchesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.SurfaceHeldWork(context.Background(), "absent")

	// Assert
	if err != nil || got {
		t.Fatalf("SurfaceHeldWork = %v, %v; want false, nil", got, err)
	}
}

func TestEndSurfacingTakesTheMarkOff(t *testing.T) {
	tests := []struct {
		name   string
		marked bool
		want   bool
	}{
		{name: "a marked workspace", marked: true, want: true},
		{name: "a workspace that was never marked"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newSelectionFoldWorld(t, s)
			if tt.marked {
				if _, err := s.SurfaceHeldWork(context.Background(), w.root.ID); err != nil {
					t.Fatalf("SurfaceHeldWork: %v", err)
				}
			}

			// Act
			got, err := s.EndSurfacing(context.Background(), w.root.ID)

			// Assert
			if err != nil {
				t.Fatalf("EndSurfacing: %v", err)
			}
			if got != tt.want || surfacedOf(t, s, w.root.ID) {
				t.Errorf("ended = %v (still marked %v), want %v and unmarked", got, surfacedOf(t, s, w.root.ID), tt.want)
			}
		})
	}
}

func TestAListedWorkspaceCarriesItsSurfacedMark(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newSelectionFoldWorld(t, s)
	if _, err := s.SurfaceHeldWork(context.Background(), w.root.ID); err != nil {
		t.Fatalf("SurfaceHeldWork: %v", err)
	}

	// Act
	listed, err := s.ListWorkspaces(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ListWorkspaces: %v", err)
	}
	for _, ws := range listed {
		if want := ws.ID == w.root.ID; ws.Surfaced != want {
			t.Errorf("workspace %s Surfaced = %v, want %v", ws.ID, ws.Surfaced, want)
		}
	}
}

func TestSurfacingWritesSurfaceAStoreFailure(t *testing.T) {
	tests := []struct {
		name string
		call func(*store, WorkspaceID) error
	}{
		{name: "SurfaceHeldWork", call: func(s *store, id WorkspaceID) error {
			_, err := s.SurfaceHeldWork(context.Background(), id)
			return err
		}},
		{name: "EndSurfacing", call: func(s *store, id WorkspaceID) error { _, err := s.EndSurfacing(context.Background(), id); return err }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newSelectionFoldWorld(t, s)
			if err := s.Close(); err != nil {
				t.Fatalf("Close: %v", err)
			}

			// Act
			err := tt.call(s, w.root.ID)

			// Assert
			if err == nil {
				t.Fatalf("%s on a closed store answered no error", tt.name)
			}
		})
	}
}

func TestSelectingIntoCollapsedFoldsLeavesThemCollapsed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newSelectionFoldWorld(t, s)

	// Act
	err := s.SetCurrent(context.Background(), w.child.ID, instant)

	// Assert
	if err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}
	if !repositoryFolded(t, s, w.root.Repo) || !subsectionNow(t, s, w.sub.ID).Folded {
		t.Error("the selection opened a fold holding the workspace")
	}
	if cur, err := s.Current(context.Background()); err != nil || cur == nil || *cur != w.child.ID {
		t.Errorf("Current = %v, %v; want %s", cur, err, w.child.ID)
	}
}

func TestADrawnInsideFoldWorkspaceIsOneSurfacedOrCurrent(t *testing.T) {
	tests := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "neither surfaced nor current"},
		{name: "surfaced", ws: Workspace{Surfaced: true}, want: true},
		{name: "current", ws: Workspace{Current: true}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.ws.DrawnInsideFold()

			// Assert
			if got != tt.want {
				t.Errorf("DrawnInsideFold = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestTheMigrationAddsTheSurfacedColumn(t *testing.T) {
	// Arrange: a file the build at layout 152 left.
	path := fixtureAt(t, 152)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-152 database: %v", err)
	}
	defer handle.Close()

	// Assert
	got := scalar[int](t, handle.(*store), `SELECT count(*) FROM pragma_table_info('workspaces') WHERE name = 'surfaced'`)
	if got != 1 {
		t.Fatalf("workspaces carries the surfaced column %d times after the migration, want 1", got)
	}
}
