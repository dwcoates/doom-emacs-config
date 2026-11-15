package wsm

import (
	"maps"
	"testing"
)

// child is a workspace of REPO recorded under PARENT.
func child(id, repo, parent WorkspaceID) Workspace {
	p := parent
	return Workspace{ID: id, Repo: RepoID(repo), Parent: &p}
}

func TestParentsIn(t *testing.T) {
	tests := []struct {
		name string
		in   []Workspace
		want map[WorkspaceID]WorkspaceID
	}{
		{"a parent among the set is answered",
			[]Workspace{{ID: "p", Repo: "r"}, child("c", "r", "p")},
			map[WorkspaceID]WorkspaceID{"c": "p"}},
		{"a parent outside the set is not",
			[]Workspace{child("c", "r", "gone")},
			map[WorkspaceID]WorkspaceID{}},
		{"a workspace recorded as its own parent is not",
			[]Workspace{child("c", "r", "c")},
			map[WorkspaceID]WorkspaceID{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ParentsIn(tt.in)

			// Assert
			if !maps.Equal(got, tt.want) {
				t.Fatalf("ParentsIn = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestParentChainCycles(t *testing.T) {
	tests := []struct {
		name   string
		parent map[WorkspaceID]WorkspaceID
		want   bool
	}{
		{"a chain ending at a root does not cycle", map[WorkspaceID]WorkspaceID{"c": "p"}, false},
		{"a chain returning to the start cycles", map[WorkspaceID]WorkspaceID{"c": "p", "p": "c"}, true},
		{"a chain entering a loop above the start cycles", map[WorkspaceID]WorkspaceID{"c": "p", "p": "q", "q": "p"}, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert
			if got := ParentChainCycles("c", tt.parent); got != tt.want {
				t.Fatalf("ParentChainCycles = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestFamilyRoots(t *testing.T) {
	tests := []struct {
		name string
		in   []Workspace
		want map[WorkspaceID]WorkspaceID
	}{
		{"a top-level workspace is its own root",
			[]Workspace{{ID: "p", Repo: "r"}},
			map[WorkspaceID]WorkspaceID{"p": "p"}},
		{"a grandchild's root is the top of its chain",
			[]Workspace{{ID: "p", Repo: "r"}, child("c", "r", "p"), child("g", "r", "c")},
			map[WorkspaceID]WorkspaceID{"p": "p", "c": "p", "g": "p"}},
		{"a cycling chain leaves each member its own root",
			[]Workspace{child("a", "r", "b"), child("b", "r", "a")},
			map[WorkspaceID]WorkspaceID{"a": "a", "b": "b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := FamilyRoots(tt.in)

			// Assert
			if !maps.Equal(got, tt.want) {
				t.Fatalf("FamilyRoots = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSectionRepos(t *testing.T) {
	tests := []struct {
		name string
		in   []Workspace
		want map[WorkspaceID]RepoID
	}{
		{"a top-level workspace is in its own repository's section",
			[]Workspace{{ID: "p", Repo: "r1"}},
			map[WorkspaceID]RepoID{"p": "r1"}},
		{"a cross-repository child is in its root's repository's section",
			[]Workspace{{ID: "p", Repo: "r1"}, child("c", "r2", "p")},
			map[WorkspaceID]RepoID{"p": "r1", "c": "r1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := SectionRepos(tt.in)

			// Assert
			if !maps.Equal(got, tt.want) {
				t.Fatalf("SectionRepos = %v, want %v", got, tt.want)
			}
		})
	}
}
