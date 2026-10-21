package harness

import (
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/integration/fakegit"
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

func TestAddFakeRepoStatesOneCommitOnTheDefaultBranch(t *testing.T) {
	// Arrange
	s := fakegit.NewState()

	// Act
	repo := addFakeRepo(s, "/fake/repo", "first", []string{"a.txt"})

	// Assert
	if s.Repo("/fake/repo") != repo {
		t.Fatal("the fixture does not answer the recorded repository for its dir")
	}
	head := repo.Worktree("/fake/repo").Head
	if head == "" || repo.BranchHeads[DefaultBranch] != head {
		t.Fatalf("head = %q, branch %s = %q, want the one commit on both", head, DefaultBranch, repo.BranchHeads[DefaultBranch])
	}
}

func TestNewRepoAtAndThePinnedCheckoutShareOneRepositoryShape(t *testing.T) {
	// Arrange
	w := World(t)
	made := NewRepoAt(t, filepath.Join(w.root, "made"))
	pinned := filepath.Join(w.root, "pinned")

	// Act
	w.tagPinnedCheckout(pinned)

	// Assert: both are addFakeRepo's shape, differing only in their commits and tags.
	s := w.read()
	for _, dir := range []string{made.Dir, pinned} {
		repo := s.Repo(dir)
		if repo == nil || repo.DefaultBranch != DefaultBranch || len(repo.Worktrees) != 1 ||
			repo.CommonDir != filepath.Join(fakegit.Canon(dir), ".git") {
			t.Fatalf("repository at %s = %+v, want addFakeRepo's shape", dir, repo)
		}
	}
}

func TestThePinnedCheckoutsHeadCarriesThePinnedVersion(t *testing.T) {
	// Arrange
	w := World(t)
	pinned := filepath.Join(w.root, "pinned")

	// Act
	w.tagPinnedCheckout(pinned)

	// Assert
	repo := w.read().Repo(pinned)
	tag := repo.Tags[PinnedVersion]
	if tag == nil || tag.Commit != repo.Worktree(pinned).Head {
		t.Fatalf("tags = %+v, want %s on the head", repo.Tags, PinnedVersion)
	}
}

func TestThePinnedCheckoutIsRecordedOncePerWorld(t *testing.T) {
	// Arrange
	w := World(t)
	pinned := filepath.Join(w.root, "pinned")
	w.tagPinnedCheckout(pinned)
	head := w.read().Repo(pinned).Worktree(pinned).Head

	// Act: a successor daemon of the same test records it again.
	w.tagPinnedCheckout(pinned)

	// Assert
	s := w.read()
	var count int
	for _, repo := range s.Repos {
		if repo.Dir == pinned {
			count++
		}
	}
	if count != 1 || s.Repo(pinned).Worktree(pinned).Head != head {
		t.Fatalf("pinned checkout recorded %d times, head moved: %v", count, s.Repo(pinned).Worktree(pinned).Head != head)
	}
}

func TestThePinnedCheckoutLeavesItsDirectoryUntouched(t *testing.T) {
	// Arrange
	w := World(t)
	pinned := filepath.Join(w.root, "pinned")

	// Act
	w.tagPinnedCheckout(pinned)

	// Assert
	if _, err := os.Stat(pinned); !os.IsNotExist(err) {
		t.Fatalf("stat %s = %v, want the shared directory never written", pinned, err)
	}
}
