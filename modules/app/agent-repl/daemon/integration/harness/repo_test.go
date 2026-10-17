package harness

import (
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

func TestReflogEntriesWritesOneGitShapedEntryPerInstant(t *testing.T) {
	// Arrange.
	first, second := time.Unix(1_700_000_000, 0), time.Unix(1_700_000_100, 0)

	// Act.
	got := reflogEntries(first, second)

	// Assert.
	lines := strings.Split(strings.TrimSuffix(got, "\n"), "\n")
	if len(lines) != 2 {
		t.Fatalf("reflogEntries = %q, want two lines", got)
	}
	for i, want := range []string{"> 1700000000 +0000\t", "> 1700000100 +0000\t"} {
		if !strings.Contains(lines[i], want) {
			t.Fatalf("line %d = %q, want %q in git's reflog shape", i, lines[i], want)
		}
	}
}

func TestMintRepoDirSkipsANameAlreadyTaken(t *testing.T) {
	// Arrange
	w := World(t)
	if err := os.Mkdir(filepath.Join(w.root, "repo-a"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act
	got := mintRepoDir(t, w)

	// Assert
	if filepath.Base(got) != "repo-b" {
		t.Fatalf("mintRepoDir = %s, want repo-b", got)
	}
}

func TestConcurrentMintsNeverShareARepositoryName(t *testing.T) {
	// Arrange
	w := World(t)
	const n = 16
	names := make(chan string, n)
	var wg sync.WaitGroup

	// Act
	for i := 0; i < n; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			names <- mintRepoDir(t, w)
		}()
	}
	wg.Wait()
	close(names)

	// Assert
	seen := map[string]bool{}
	for name := range names {
		if seen[name] {
			t.Fatalf("two mints answered %s", name)
		}
		seen[name] = true
	}
	if len(seen) != n {
		t.Fatalf("minted %d names, want %d", len(seen), n)
	}
}

func TestAddBranchAtTipCutsABranchAtTheDefaultHeadCheckedOutNowhere(t *testing.T) {
	// Arrange
	repo := NewRepo(t)

	// Act
	repo.AddBranchAtTip("at-tip")

	// Assert
	if got, want := repo.BranchHead("at-tip"), repo.BranchHead(DefaultBranch); got != want {
		t.Fatalf("at-tip head = %s, want the default branch's %s", got, want)
	}
	if !repo.HasBranch("at-tip") {
		t.Fatalf("Branches() = %v, want at-tip among them", repo.Branches())
	}
	if got := len(repo.Worktrees()); got != 1 {
		t.Fatalf("Worktrees() = %d, want the main worktree alone: the branch is checked out nowhere", got)
	}
}
