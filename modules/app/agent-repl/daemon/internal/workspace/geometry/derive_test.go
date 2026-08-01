package geometry

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// runGit runs one fixture git command through the SAME env-stripped builder the
// deriver uses. Fixtures live entirely inside t.TempDir(), and the stripping is
// what keeps an inherited GIT_DIR (a daemon or a test run launched from a git
// hook) from pointing these writes at a real repository.
func runGit(t *testing.T, dir string, args ...string) string {
	t.Helper()
	identity := []string{"-c", "user.name=geometry test", "-c", "user.email=geometry@test.invalid", "-c", "commit.gpgsign=false"}
	out, err := gitCapture(context.Background(), dir, append(identity, args...)...)
	if err != nil {
		t.Fatalf("git %v in %s: %v", args, dir, err)
	}
	return out
}

// newRepo creates a repository with one commit on a known branch and returns
// its main worktree directory.
func newRepo(t *testing.T) string {
	t.Helper()
	root := t.TempDir()
	main := filepath.Join(root, "repo")
	if err := os.MkdirAll(main, 0o755); err != nil {
		t.Fatal(err)
	}
	runGit(t, main, "init", "--quiet")
	runGit(t, main, "symbolic-ref", "HEAD", "refs/heads/master")
	if err := os.WriteFile(filepath.Join(main, "seed.txt"), []byte("seed\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	runGit(t, main, "add", "seed.txt")
	runGit(t, main, "commit", "--quiet", "-m", "seed")
	return main
}

func TestDeriveResolvesTheBranchAndTheRepositorysMainWorktree(t *testing.T) {
	// Arrange — a linked worktree, exactly the shape a pre-cutover workspace has.
	main := newRepo(t)
	linked := filepath.Join(filepath.Dir(main), "feature-one")
	runGit(t, main, "worktree", "add", "-b", "DWC/feature-one", linked)
	deriver, err := NewDeriver(func(string, ...any) {})
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	got, err := deriver.Derive(context.Background(), linked)

	// Assert.
	if err != nil {
		t.Fatalf("Derive: %v", err)
	}
	// The two directories are recorded in their canonical spelling: macOS hands
	// out /var/... while git answers /private/var/..., and one directory must
	// not become two coordinates.
	wantSource, err := canonical(linked)
	if err != nil {
		t.Fatal(err)
	}
	wantTarget, err := canonical(main)
	if err != nil {
		t.Fatal(err)
	}
	want := Record{Workspace: Key(linked), SourceBranch: "DWC/feature-one", SourceDir: wantSource, TargetDir: wantTarget, Origin: OriginBackfilled}
	if got != want {
		t.Fatalf("Derive = %#v, want %#v", got, want)
	}
}

func TestDeriveRefusesAWorktreeOnADetachedHead(t *testing.T) {
	// Arrange — a detached worktree has no branch, so there is no commit range
	// to cherry-pick and nothing may be guessed in its place.
	main := newRepo(t)
	linked := filepath.Join(filepath.Dir(main), "detached")
	runGit(t, main, "worktree", "add", "--detach", linked)
	var logs []string
	deriver, err := NewDeriver(func(format string, args ...any) { logs = append(logs, format) })
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	got, err := deriver.Derive(context.Background(), linked)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "resolve checked-out branch") {
		t.Fatalf("Derive error = %v, want a branch-resolution refusal", err)
	}
	if got != (Record{}) {
		t.Fatalf("Derive returned a partial record %#v", got)
	}
	if !containsFormat(logs, "stage=branch") {
		t.Fatalf("the branch failure was not logged: %#v", logs)
	}
}

func TestDeriveRefusesTheRepositorysOwnMainWorktree(t *testing.T) {
	// Arrange — the main worktree's target would be itself.
	main := newRepo(t)
	deriver, err := NewDeriver(func(string, ...any) {})
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err = deriver.Derive(context.Background(), main)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "cannot merge into itself") {
		t.Fatalf("Derive error = %v, want a self-merge refusal", err)
	}
}

func TestDeriveRefusesADirectoryThatIsNotAWorktree(t *testing.T) {
	// Arrange.
	plain := t.TempDir()
	deriver, err := NewDeriver(func(string, ...any) {})
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err = deriver.Derive(context.Background(), plain)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "resolve checked-out branch") {
		t.Fatalf("Derive error = %v, want a git refusal", err)
	}
}

func TestDeriveRefusesAWorkspaceDirectoryThatIsGone(t *testing.T) {
	// Arrange — the worktree was deleted out from under the registry record.
	missing := filepath.Join(t.TempDir(), "deleted-worktree")
	deriver, err := NewDeriver(func(string, ...any) {})
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err = deriver.Derive(context.Background(), missing)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no such file or directory") {
		t.Fatalf("Derive error = %v, want a stat refusal", err)
	}
}

func TestDeriveRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	deriver, err := NewDeriver(func(string, ...any) {})
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err = deriver.Derive(context.Background(), "")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "workspace directory") {
		t.Fatalf("Derive error = %v, want an empty-workspace refusal", err)
	}
}

func TestNewDeriverRefusesWithoutALogger(t *testing.T) {
	// Arrange / Act.
	_, err := NewDeriver(nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "Logf") {
		t.Fatalf("NewDeriver error = %v, want a Logf refusal", err)
	}
}

func TestGitCmdStripsInheritedRepositoryBindings(t *testing.T) {
	// Arrange — a daemon launched from a git hook inherits these; an inherited
	// binding would make every workspace derive the HOOK's repository.
	t.Setenv("GIT_DIR", "/somewhere/else/.git")
	t.Setenv("GIT_WORK_TREE", "/somewhere/else")
	t.Setenv("GIT_INDEX_FILE", "/somewhere/else/.git/index")

	// Act.
	cmd := gitCmd(context.Background(), t.TempDir(), "status")

	// Assert.
	sawPath := false
	for _, entry := range cmd.Env {
		for _, banned := range []string{"GIT_DIR=", "GIT_WORK_TREE=", "GIT_INDEX_FILE=", "GIT_OBJECT_DIRECTORY=", "GIT_COMMON_DIR=", "GIT_PREFIX="} {
			if strings.HasPrefix(entry, banned) {
				t.Fatalf("gitCmd env kept %q; derivation would target another repository", entry)
			}
		}
		if strings.HasPrefix(entry, "PATH=") {
			sawPath = true
		}
	}
	if !sawPath {
		t.Fatal("gitCmd env dropped PATH; only repository bindings may be stripped")
	}
}

func TestMainWorktreeReadsTheFirstPorcelainEntry(t *testing.T) {
	tests := []struct {
		name    string
		listing string
		want    string
	}{
		{"main then linked", "worktree /repo\nHEAD abc\nbranch refs/heads/master\n\nworktree /repo-worktrees/f\nHEAD def\nbranch refs/heads/f\n", "/repo"},
		{"trailing slash is cleaned", "worktree /repo/\n", "/repo"},
		{"no entries", "\n\n", ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := mainWorktree(tc.listing)

			// Assert.
			if got != tc.want {
				t.Fatalf("mainWorktree = %q, want %q", got, tc.want)
			}
		})
	}
}
