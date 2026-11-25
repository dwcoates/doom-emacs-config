package wsm

import (
	"context"
	"slices"
	"testing"
	"time"
)

// backloggedWith reads which family hide hid ID: empty when none did.
func backloggedWith(t *testing.T, s *store, id WorkspaceID) string {
	t.Helper()
	var with *string
	if err := s.db().QueryRow(`SELECT backlogged_with FROM workspaces WHERE id = ?`, id).Scan(&with); err != nil {
		t.Fatalf("read backlogged_with of %s: %v", id, err)
	}
	if with == nil {
		return ""
	}
	return *with
}

// threeGenerations is a parent with a child and a grandchild, all open.
func threeGenerations(t *testing.T) (f family, grandchild Workspace) {
	t.Helper()
	f = newFamily(t)
	return f, registerChild(t, f.s, f.repoDir, "grandchild", f.child.ID)
}

func TestHideFamilyHidesTheParentWithEveryOpenDescendant(t *testing.T) {
	// Arrange
	f, grandchild := threeGenerations(t)

	// Act
	members, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0))

	// Assert
	want := []WorkspaceID{f.child.ID, grandchild.ID}
	if err != nil || !slices.Equal(slices.Sorted(slices.Values(members)), slices.Sorted(slices.Values(want))) {
		t.Fatalf("HideFamily = %v, %v; want the child and the grandchild", members, err)
	}
	for _, ws := range []Workspace{f.parent, f.child, grandchild} {
		if !workspaceNow(t, f.s, ws.ID).Backlogged {
			t.Fatalf("%s is shown, want the whole family hidden", ws.Name)
		}
	}
	if backloggedWith(t, f.s, f.child.ID) != string(f.parent.ID) || backloggedWith(t, f.s, grandchild.ID) != string(f.parent.ID) || backloggedWith(t, f.s, f.parent.ID) != "" {
		t.Fatal("want the child and the grandchild marked as hidden with the parent, and the parent unmarked")
	}
}

func TestHideFamilyLeavesAMemberBackloggedBeforeUnmarked(t *testing.T) {
	// Arrange
	f := newFamily(t)
	mustHide(t, f.s, f.child.ID)

	// Act
	members, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0))

	// Assert
	if err != nil || len(members) != 0 || backloggedWith(t, f.s, f.child.ID) != "" {
		t.Fatalf("HideFamily = %v, %v, child marked %q; want the child left as the user hid it", members, err, backloggedWith(t, f.s, f.child.ID))
	}
}

func TestHideFamilyLeavesAClosedChildClosed(t *testing.T) {
	// Arrange
	f := newFamily(t)
	mustHide(t, f.s, f.child.ID)
	mustClose(t, f.s, f.child.ID, CloseByUser)

	// Act
	members, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0))

	// Assert
	if err != nil || len(members) != 0 || !workspaceNow(t, f.s, f.parent.ID).Backlogged {
		t.Fatalf("HideFamily = %v, %v; want the parent hidden alone", members, err)
	}
}

func TestShowFamilyShowsTheParentAndWhatWasBackloggedWithIt(t *testing.T) {
	// Arrange
	f, grandchild := threeGenerations(t)
	if _, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0)); err != nil {
		t.Fatal(err)
	}

	// Act
	members, err := f.s.ShowFamily(context.Background(), f.parent.ID)

	// Assert
	if err != nil || !slices.Equal(members, []WorkspaceID{f.child.ID, grandchild.ID}) {
		t.Fatalf("ShowFamily = %v, %v; want the child, then the grandchild", members, err)
	}
	for _, ws := range []Workspace{f.parent, f.child, grandchild} {
		if workspaceNow(t, f.s, ws.ID).Backlogged || backloggedWith(t, f.s, ws.ID) != "" {
			t.Fatalf("%s is still hidden or marked, want the family shown and unmarked", ws.Name)
		}
	}
}

func TestShowFamilyShowsAChildBeforeTheGrandchildWhateverTheirPlaces(t *testing.T) {
	// Arrange: the grandchild sits ahead of its own parent.
	f, grandchild := threeGenerations(t)
	if _, err := f.s.db().Exec(`UPDATE workspaces SET place = (SELECT MIN(place) - 1 FROM workspaces) WHERE id = ?`, grandchild.ID); err != nil {
		t.Fatal(err)
	}
	if _, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0)); err != nil {
		t.Fatal(err)
	}

	// Act
	members, err := f.s.ShowFamily(context.Background(), f.parent.ID)

	// Assert
	if err != nil || !slices.Equal(members, []WorkspaceID{f.child.ID, grandchild.ID}) {
		t.Fatalf("ShowFamily = %v, %v; want the child shown before the grandchild", members, err)
	}
}

func TestShowFamilyLeavesWhatTheUserHidOrClosedAsItIs(t *testing.T) {
	tests := []struct {
		name           string
		arrange        func(t *testing.T, f family)
		wantBacklogged bool
	}{
		{name: "a child the user hid before stays hidden", wantBacklogged: true, arrange: func(t *testing.T, f family) {
			mustHide(t, f.s, f.child.ID)
			if _, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0)); err != nil {
				t.Fatal(err)
			}
		}},
		{name: "a child closed while hidden with it stays closed", wantBacklogged: true, arrange: func(t *testing.T, f family) {
			if _, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0)); err != nil {
				t.Fatal(err)
			}
			mustClose(t, f.s, f.child.ID, CloseByDaemon)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFamily(t)
			tt.arrange(t, f)

			// Act
			members, err := f.s.ShowFamily(context.Background(), f.parent.ID)

			// Assert
			if err != nil || len(members) != 0 || workspaceNow(t, f.s, f.child.ID).Backlogged != tt.wantBacklogged {
				t.Fatalf("ShowFamily = %v, %v; want the child left as it was", members, err)
			}
		})
	}
}

func TestShowFamilyRefusesARootWhoseParentIsNotOpen(t *testing.T) {
	// Arrange: the root is a child of a hidden workspace.
	f, grandchild := threeGenerations(t)
	if _, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0)); err != nil {
		t.Fatal(err)
	}

	// Act
	members, err := f.s.ShowFamily(context.Background(), f.child.ID)

	// Assert
	requireParentNotOpen(t, err, f.parent, ParentBacklogged)
	if len(members) != 0 || !workspaceNow(t, f.s, f.child.ID).Backlogged || !workspaceNow(t, f.s, grandchild.ID).Backlogged {
		t.Fatalf("ShowFamily = %v; want nothing shown", members)
	}
}

func TestAnOrdinaryShowForgetsTheFamilyHide(t *testing.T) {
	// Arrange
	f := newFamily(t)
	if _, err := f.s.HideFamily(context.Background(), f.parent.ID, time.Unix(10, 0)); err != nil {
		t.Fatal(err)
	}
	if err := f.s.SetBacklogged(context.Background(), f.parent.ID, false, time.Unix(20, 0)); err != nil {
		t.Fatal(err)
	}

	// Act
	err := f.s.SetBacklogged(context.Background(), f.child.ID, false, time.Unix(20, 0))

	// Assert
	if err != nil || backloggedWith(t, f.s, f.child.ID) != "" {
		t.Fatalf("SetBacklogged = %v, marked %q; want the child shown and unmarked", err, backloggedWith(t, f.s, f.child.ID))
	}
}
