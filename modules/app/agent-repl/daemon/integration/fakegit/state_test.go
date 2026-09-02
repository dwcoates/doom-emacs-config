package fakegit

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadOfAMissingFileIsAnEmptyWorld(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "absent.json")

	// Act.
	s, err := Load(path)

	// Assert.
	if err != nil {
		t.Fatalf("Load(missing) = error %v, want an empty world", err)
	}
	if len(s.Repos) != 0 {
		t.Fatalf("Load(missing) carried %d repos, want none", len(s.Repos))
	}
}

func TestLoadOfAMalformedFileIsALoudFailure(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "state.json")
	if err := os.WriteFile(path, []byte("{not json"), 0o644); err != nil {
		t.Fatalf("writing the fixture: %v", err)
	}

	// Act.
	_, err := Load(path)

	// Assert.
	if err == nil {
		t.Fatal("Load(malformed) = nil error, want a refusal")
	}
}

func TestSaveThenLoadRoundTripsTheWorld(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "state.json")
	s := NewState()
	s.Repos = append(s.Repos, &Repo{Dir: "/w", CommonDir: "/w/.git", DefaultBranch: "main"})

	// Act.
	if err := Save(path, s); err != nil {
		t.Fatalf("Save = error %v, want a written fixture", err)
	}
	back, err := Load(path)
	if err != nil {
		t.Fatalf("Load = error %v, want the saved world", err)
	}

	// Assert.
	if len(back.Repos) != 1 || back.Repos[0].Dir != "/w" {
		t.Fatalf("Load after Save = %+v, want the one repository back", back.Repos)
	}
}

func TestWithLockPersistsTheMutation(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "state.json")

	// Act.
	if err := WithLock(path, func(s *State) error {
		s.Repos = append(s.Repos, &Repo{Dir: "/a"})
		return nil
	}); err != nil {
		t.Fatalf("WithLock = error %v, want the mutation saved", err)
	}
	back, err := Load(path)

	// Assert.
	if err != nil || len(back.Repos) != 1 {
		t.Fatalf("Load after WithLock = %+v (%v), want one repository", back, err)
	}
}

func TestFindWorktreePrefersTheDeepestTree(t *testing.T) {
	// Arrange: a worktree nested inside another repository's tree.
	root := t.TempDir()
	outer := filepath.Join(root, "outer")
	inner := filepath.Join(outer, "inner")
	s := NewState()
	s.Repos = append(s.Repos,
		&Repo{Dir: outer, Worktrees: []*Worktree{{Dir: outer}}},
		&Repo{Dir: inner, Worktrees: []*Worktree{{Dir: inner}}})

	// Act.
	repo, _ := s.FindWorktree(inner)

	// Assert.
	if repo == nil || repo.Dir != inner {
		t.Fatalf("FindWorktree(%s) = %v, want the inner repository", inner, repo)
	}
}

func TestFindWorktreeOfAnUnregisteredDirectoryIsNothing(t *testing.T) {
	// Arrange.
	s := NewState()

	// Act.
	repo, wt := s.FindWorktree(t.TempDir())

	// Assert.
	if repo != nil || wt != nil {
		t.Fatalf("FindWorktree(unregistered) = %v/%v, want nothing", repo, wt)
	}
}

func TestAddCommitMovesTheBranchAndItsWorktree(t *testing.T) {
	// Arrange.
	repo := &Repo{Dir: "/w", Branches: []string{"main"}, BranchHeads: map[string]string{"main": "old"}}
	repo.Worktrees = []*Worktree{{Dir: "/w", Branch: "main", Head: "old"}}
	s := NewState()

	// Act.
	c := s.AddCommit(repo, "main", "a subject", []string{"old"}, []string{"f.txt"})

	// Assert.
	if repo.BranchHeads["main"] != c.SHA || repo.Worktrees[0].Head != c.SHA {
		t.Fatalf("after AddCommit branch=%q worktree=%q, want both at %q",
			repo.BranchHeads["main"], repo.Worktrees[0].Head, c.SHA)
	}
}

func TestRemoveBranchDropsTheNameAndItsHead(t *testing.T) {
	// Arrange.
	repo := &Repo{}
	repo.AddBranch("feature", "sha")

	// Act.
	repo.RemoveBranch("feature")

	// Assert.
	if repo.HasBranch("feature") || repo.BranchHeads["feature"] != "" {
		t.Fatalf("after RemoveBranch the branch is still %v/%q", repo.Branches, repo.BranchHeads["feature"])
	}
}

func TestRemoveWorktreeDropsOnlyThatTree(t *testing.T) {
	// Arrange.
	repo := &Repo{Worktrees: []*Worktree{{Dir: "/a"}, {Dir: "/b"}}}

	// Act.
	repo.RemoveWorktree("/a")

	// Assert.
	if len(repo.Worktrees) != 1 || repo.Worktrees[0].Dir != "/b" {
		t.Fatalf("after RemoveWorktree = %v, want only /b", repo.Worktrees)
	}
}

// TestLoadLockedReadsThroughTheSameLockWritesTake covers the reader's half of
// the fixture file's exclusion: Save truncates and rewrites in place, so an
// unlocked read can see an empty file and report a world with no repositories.
func TestLoadLockedReadsThroughTheSameLockWritesTake(t *testing.T) {
	// Arrange: a world with one repository, written under the lock.
	path := filepath.Join(t.TempDir(), "fakegit.json")
	if err := WithLock(path, func(s *State) error {
		s.Repos = append(s.Repos, &Repo{Dir: "/repo", DefaultBranch: "main"})
		return nil
	}); err != nil {
		t.Fatalf("WithLock: %v", err)
	}

	// Act.
	got, err := LoadLocked(path)

	// Assert.
	if err != nil {
		t.Fatalf("LoadLocked = %v, want the written world", err)
	}
	if len(got.Repos) != 1 {
		t.Fatalf("LoadLocked carried %d repos, want the one that was written", len(got.Repos))
	}
}
