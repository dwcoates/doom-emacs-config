package gitclient

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
)

func TestScrubEnvStripsEveryRepositorySelectingVar(t *testing.T) {
	tests := []struct {
		name    string
		binding string
	}{
		{name: "git dir", binding: "GIT_DIR=/nowhere/.git"},
		{name: "work tree", binding: "GIT_WORK_TREE=/nowhere"},
		{name: "index file", binding: "GIT_INDEX_FILE=/nowhere/index"},
		{name: "common dir", binding: "GIT_COMMON_DIR=/nowhere/.git"},
		{name: "prefix", binding: "GIT_PREFIX=sub/"},
		{name: "object directory", binding: "GIT_OBJECT_DIRECTORY=/nowhere/objects"},
		{name: "alternate object directories", binding: "GIT_ALTERNATE_OBJECT_DIRECTORIES=/nowhere/alt"},
		{name: "namespace", binding: "GIT_NAMESPACE=other"},
		{name: "ceiling directories", binding: "GIT_CEILING_DIRECTORIES=/nowhere"},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			env := []string{"PATH=/usr/bin", test.binding}

			// Act.
			scrubbed := scrubEnv(env)

			// Assert.
			for _, entry := range scrubbed {
				if entry == test.binding {
					t.Fatalf("scrubEnv kept %q; `-C dir` must be the only repository selector", test.binding)
				}
			}
		})
	}
}

func TestScrubEnvKeepsUnrelatedBindings(t *testing.T) {
	// Arrange.
	env := []string{"PATH=/usr/bin", "HOME=/home/someone", "GIT_AUTHOR_NAME=Someone"}

	// Act.
	scrubbed := scrubEnv(env)

	// Assert.
	for _, want := range env {
		if !containsEntry(scrubbed, want) {
			t.Fatalf("scrubEnv dropped %q; only repository-selecting bindings may be removed", want)
		}
	}
}

func TestScrubEnvPinsTerminalPromptOverInheritedValue(t *testing.T) {
	// Arrange: an environment that would let git block on a credential prompt.
	env := []string{"GIT_TERMINAL_PROMPT=1"}

	// Act.
	scrubbed := scrubEnv(env)

	// Assert: the pinned binding is the only one present, not merely the last.
	if got := entriesFor(scrubbed, "GIT_TERMINAL_PROMPT"); len(got) != 1 || got[0] != "GIT_TERMINAL_PROMPT=0" {
		t.Fatalf("GIT_TERMINAL_PROMPT bindings = %v, want exactly [GIT_TERMINAL_PROMPT=0]", got)
	}
}

func TestScrubEnvPinsLocaleOverInheritedValue(t *testing.T) {
	// Arrange: a locale that would translate git's messages.
	env := []string{"LC_ALL=fr_FR.UTF-8"}

	// Act.
	scrubbed := scrubEnv(env)

	// Assert.
	if got := entriesFor(scrubbed, "LC_ALL"); len(got) != 1 || got[0] != "LC_ALL=C" {
		t.Fatalf("LC_ALL bindings = %v, want exactly [LC_ALL=C]", got)
	}
}

// TestInheritedGitDirDoesNotSelectTheRepository is the regression this leaf
// exists for: git honors GIT_DIR ahead of `-C dir`, so a leaked one from a hook
// would silently point every command at another repository.
func TestInheritedGitDirDoesNotSelectTheRepository(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	t.Setenv("GIT_DIR", filepath.Join(t.TempDir(), "bogus", ".git"))

	// Act.
	branch, err := git.CurrentBranch(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("CurrentBranch under a bogus inherited GIT_DIR: %v", err)
	}
	if branch != "main" {
		t.Fatalf("CurrentBranch = %q, want %q", branch, "main")
	}
}

// TestInheritedGitWorkTreeDoesNotSelectTheWorkTree covers the second half of
// the same hazard: GIT_WORK_TREE alone is enough to produce bogus work-tree
// errors against a directory the caller never named.
func TestInheritedGitWorkTreeDoesNotSelectTheWorkTree(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	t.Setenv("GIT_WORK_TREE", t.TempDir())

	// Act.
	clean, err := git.IsClean(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("IsClean under a bogus inherited GIT_WORK_TREE: %v", err)
	}
	if !clean {
		t.Fatalf("IsClean = false, want true for a freshly seeded repository")
	}
}

func TestFailureCarriesGitExitCodeAndStderr(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	_, err := git.ResolveRef(context.Background(), dir, "refs/heads/does-not-exist")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("ResolveRef error = %v (%T), want a *gitclient.Error", err, err)
	}
	if failure.ExitCode == 0 {
		t.Fatalf("Error.ExitCode = 0, want git's nonzero status")
	}
	if failure.Dir != dir {
		t.Fatalf("Error.Dir = %q, want %q", failure.Dir, dir)
	}
	if strings.TrimSpace(failure.Stderr) == "" {
		t.Fatalf("Error.Stderr is empty; git's own words are the evidence")
	}
}

func TestFailureCarriesTheArgumentVector(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	_, err := git.ResolveRef(context.Background(), dir, "refs/heads/does-not-exist")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("ResolveRef error = %v (%T), want a *gitclient.Error", err, err)
	}
	if len(failure.Args) == 0 || failure.Args[0] != "rev-parse" {
		t.Fatalf("Error.Args = %v, want the rev-parse vector", failure.Args)
	}
}

func TestFailureIsLoggedAtErrorOnce(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	_, _ = git.ResolveRef(context.Background(), dir, "refs/heads/does-not-exist")

	// Assert.
	var errorRecords int
	for _, record := range surfaces.records() {
		if record.Level == "error" && record.Operation == "daemon.gitclient.resolve_ref" {
			errorRecords++
		}
	}
	if errorRecords != 1 {
		t.Fatalf("error records for resolve_ref = %d, want exactly 1", errorRecords)
	}
}

func TestOrdinaryPathIsLoggedAtDebug(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	if _, err := git.CurrentBranch(context.Background(), dir); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if _, ok := recordFor(surfaces.records(), "debug", "daemon.gitclient.current_branch"); !ok {
		t.Fatalf("no debug record for daemon.gitclient.current_branch; every branch logs")
	}
}

func TestInvokeReportsAnUnrunnableGit(t *testing.T) {
	// Arrange: a PATH with no git on it at all.
	git, surfaces := newTestClient(t)
	t.Setenv("PATH", t.TempDir())

	// Act.
	_, err := git.CurrentBranch(context.Background(), t.TempDir())

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("CurrentBranch error = %v (%T), want a *gitclient.Error", err, err)
	}
	if failure.ExitCode != -1 {
		t.Fatalf("Error.ExitCode = %d, want -1 for a git that never ran", failure.ExitCode)
	}
	if _, ok := recordFor(surfaces.records(), "error", "daemon.gitclient.current_branch"); !ok {
		t.Fatalf("a git that could not be run must be logged at ERROR")
	}
}

// containsEntry reports whether env carries that exact binding.
func containsEntry(env []string, binding string) bool {
	for _, entry := range env {
		if entry == binding {
			return true
		}
	}
	return false
}

// entriesFor returns every binding of that variable name in env.
func entriesFor(env []string, name string) []string {
	var found []string
	for _, entry := range env {
		if strings.HasPrefix(entry, name+"=") {
			found = append(found, entry)
		}
	}
	return found
}
