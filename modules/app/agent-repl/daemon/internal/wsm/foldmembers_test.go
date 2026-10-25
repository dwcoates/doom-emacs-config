package wsm

import "testing"

func TestFoldMembers(t *testing.T) {
	sub := SubsectionID("sub-1")
	other := SubsectionID("sub-2")
	tests := []struct {
		name    string
		members FoldMembers
		ws      Workspace
		member  bool
		stands  bool
	}{
		{"a repository holds its workspace in no subsection", RepositoryMembers("repo-1"), Workspace{Repo: "repo-1"}, true, true},
		{"a repository holds its workspace in a subsection", RepositoryMembers("repo-1"), Workspace{Repo: "repo-1", Subsection: &sub}, true, true},
		{"a repository does not hold another repository's workspace", RepositoryMembers("repo-1"), Workspace{Repo: "repo-2"}, false, false},
		{"a subsection holds the workspace sitting in it", SubsectionMembers(sub), Workspace{Repo: "repo-1", Subsection: &sub}, true, true},
		{"a subsection does not hold a workspace in another subsection", SubsectionMembers(sub), Workspace{Repo: "repo-1", Subsection: &other}, false, false},
		{"a subsection does not hold a workspace in no subsection", SubsectionMembers(sub), Workspace{Repo: "repo-1"}, false, false},
		{"a closed member is not stood down", RepositoryMembers("repo-1"), Workspace{Repo: "repo-1", Closed: true}, true, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			member, stands := tt.members(tt.ws), tt.members.StandsDown(tt.ws)
			// Assert
			if member != tt.member || stands != tt.stands {
				t.Fatalf("member, stands down = %v, %v; want %v, %v", member, stands, tt.member, tt.stands)
			}
		})
	}
}

func TestFoldMembersHoldsCurrent(t *testing.T) {
	tests := []struct {
		name    string
		current *Workspace
		want    bool
	}{
		{"no current workspace is held by no section", nil, false},
		{"an open current member is held", &Workspace{Repo: "repo-1"}, true},
		{"a closed current member is not held", &Workspace{Repo: "repo-1", Closed: true}, false},
		{"a current workspace of another repository is not held", &Workspace{Repo: "repo-2"}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert
			if got := RepositoryMembers("repo-1").HoldsCurrent(tt.current); got != tt.want {
				t.Fatalf("HoldsCurrent = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestWorkspaceFoldSpare(t *testing.T) {
	tests := []struct {
		name   string
		ws     Workspace
		want   FoldSpare
		spared bool
	}{
		{"an ordinary open workspace is spared nothing", Workspace{}, "", false},
		{"a one-shot is spared", Workspace{OneShot: true}, FoldSpareOneShot, true},
		{"a workspace with a merge pending is spared", Workspace{MergePending: true}, FoldSpareMergePending, true},
		{"a one-shot with a merge pending is spared as a one-shot", Workspace{OneShot: true, MergePending: true}, FoldSpareOneShot, true},
		{"a hidden one-shot is spared nothing", Workspace{OneShot: true, Hidden: true}, "", false},
		{"a closed one-shot is spared nothing", Workspace{OneShot: true, Closed: true}, "", false},
		{"a hidden workspace with a merge pending is spared nothing", Workspace{MergePending: true, Hidden: true}, "", false},
		{"a closed workspace with a merge pending is spared nothing", Workspace{MergePending: true, Closed: true}, "", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, spared := tt.ws.FoldSpare()
			// Assert
			if got != tt.want || spared != tt.spared {
				t.Fatalf("FoldSpare = %q, %v; want %q, %v", got, spared, tt.want, tt.spared)
			}
		})
	}
}
