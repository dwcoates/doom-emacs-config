package fakegit

import (
	"io"
	"os"
	"path/filepath"
	"strings"
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

func TestSaveReplacesTheFixtureRatherThanRewritingItInPlace(t *testing.T) {
	// Arrange: a reader holds the fixture as the first world wrote it.
	path := filepath.Join(t.TempDir(), "state.json")
	first := NewState()
	first.Repos = append(first.Repos, &Repo{Dir: "/first"})
	if err := Save(path, first); err != nil {
		t.Fatalf("Save(first) = %v", err)
	}
	held, err := os.Open(path)
	if err != nil {
		t.Fatalf("open the fixture: %v", err)
	}
	defer held.Close()

	// Act
	if err := Save(path, NewState()); err != nil {
		t.Fatalf("Save(second) = %v", err)
	}

	// Assert: the held file still reads the whole first world, which only a
	// rename over it (never a truncation of it) leaves true.
	body, err := io.ReadAll(held)
	if err != nil || !strings.Contains(string(body), `"/first"`) {
		t.Fatalf("the held fixture read %q (%v), want the first world intact", body, err)
	}
}

func TestSaveLeavesNoTemporaryFileBehind(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	path := filepath.Join(dir, "state.json")

	// Act
	if err := Save(path, NewState()); err != nil {
		t.Fatalf("Save = %v", err)
	}

	// Assert
	entries, err := os.ReadDir(dir)
	if err != nil || len(entries) != 1 || entries[0].Name() != "state.json" {
		t.Fatalf("the fixture directory holds %v (%v), want only state.json", entries, err)
	}
}

func TestSaveThatCannotReplaceTheFixtureFailsAndCleansUp(t *testing.T) {
	// Arrange: the fixture's path is a non-empty directory, which no rename
	// can replace.
	dir := t.TempDir()
	path := filepath.Join(dir, "state.json")
	if err := os.MkdirAll(filepath.Join(path, "occupied"), 0o755); err != nil {
		t.Fatalf("arrange: %v", err)
	}

	// Act
	err := Save(path, NewState())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "rename") {
		t.Fatalf("Save = %v, want a refused rename", err)
	}
	entries, _ := os.ReadDir(dir)
	if len(entries) != 1 {
		t.Fatalf("the fixture directory holds %v, want the temporary file removed", entries)
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

func TestMintSHAScramblesTheSequenceIntoTheLeadingDigits(t *testing.T) {
	// Arrange.
	s := NewState()

	// Act.
	first, second := s.MintSHA(), s.MintSHA()

	// Assert.
	if first[:7] == second[:7] {
		t.Fatalf("minted %q and %q, want shas that differ inside real git's seven-character abbreviation", first, second)
	}
}

func TestALoadNeverSeesASaveHalfWritten(t *testing.T) {
	// Arrange: a world with a repository, saved over and over while another
	// reader loads it without the lock -- as a successor's git reads the file
	// a SIGKILLed predecessor's git may have been writing.
	path := filepath.Join(t.TempDir(), "fakegit.json")
	world := NewState()
	world.Repos = []*Repo{{Dir: "/repo", Worktrees: []*Worktree{{Dir: "/repo"}}}}
	if err := Save(path, world); err != nil {
		t.Fatalf("Save: %v", err)
	}
	done := make(chan struct{})
	go func() {
		defer close(done)
		for i := 0; i < 2000; i++ {
			if err := Save(path, world); err != nil {
				t.Errorf("Save: %v", err)
				return
			}
		}
	}()

	// Act, Assert: every load reads a whole world.
	for {
		select {
		case <-done:
			return
		default:
		}
		s, err := Load(path)
		if err != nil {
			t.Fatalf("Load during a save: %v", err)
		}
		if len(s.Repos) != 1 {
			t.Fatalf("Load during a save read %d repositories, want the 1 saved: a reader saw the file emptied", len(s.Repos))
		}
	}
}
