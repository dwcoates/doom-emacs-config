package gitclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// --- the environment contract, as a pure function -----------------------

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
			if containsEntry(scrubbed, test.binding) {
				t.Fatalf("scrubEnv kept %q; `-C dir` must be the only repository selector", test.binding)
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

	// Assert: the pinned binding is the ONLY one present, not merely the last.
	if got := envValues(scrubbed, "GIT_TERMINAL_PROMPT"); len(got) != 1 || got[0] != "GIT_TERMINAL_PROMPT=0" {
		t.Fatalf("GIT_TERMINAL_PROMPT bindings = %v, want exactly [GIT_TERMINAL_PROMPT=0]", got)
	}
}

func TestScrubEnvPinsLocaleOverInheritedValue(t *testing.T) {
	// Arrange: a locale that would translate git's messages.
	env := []string{"LC_ALL=fr_FR.UTF-8"}

	// Act.
	scrubbed := scrubEnv(env)

	// Assert.
	if got := envValues(scrubbed, "LC_ALL"); len(got) != 1 || got[0] != "LC_ALL=C" {
		t.Fatalf("LC_ALL bindings = %v, want exactly [LC_ALL=C]", got)
	}
}

// --- the environment contract, as the child actually sees it ------------

// TestInheritedGitDirNeverReachesTheChild is the regression this leaf exists
// for: git honors GIT_DIR ahead of `-C dir`, so a hook-leaked one would
// silently point every command at another repository.
func TestInheritedGitDirNeverReachesTheChild(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("main\n"))
	t.Setenv("GIT_DIR", "/nowhere/else/.git")

	// Act.
	if _, err := git.CurrentBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, "GIT_DIR"); len(got) != 0 {
		t.Fatalf("the child's environment carries %v; GIT_DIR must be scrubbed", got)
	}
}

func TestInheritedGitWorkTreeNeverReachesTheChild(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))
	t.Setenv("GIT_WORK_TREE", "/nowhere/else")

	// Act.
	if _, err := git.IsClean(context.Background(), "/repo"); err != nil {
		t.Fatalf("IsClean: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, "GIT_WORK_TREE"); len(got) != 0 {
		t.Fatalf("the child's environment carries %v; GIT_WORK_TREE must be scrubbed", got)
	}
}

func TestInheritedGitIndexFileNeverReachesTheChild(t *testing.T) {
	// Arrange: an index binding would make git stage into another repository's
	// index even with `-C dir` correct.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))
	t.Setenv("GIT_INDEX_FILE", "/nowhere/else/index")

	// Act.
	if _, err := git.ConflictedFiles(context.Background(), "/repo"); err != nil {
		t.Fatalf("ConflictedFiles: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, "GIT_INDEX_FILE"); len(got) != 0 {
		t.Fatalf("the child's environment carries %v; GIT_INDEX_FILE must be scrubbed", got)
	}
}

func TestPinnedTerminalPromptReachesTheChild(t *testing.T) {
	// Arrange: a daemon has no terminal, so git must fail rather than block.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("main\n"))
	t.Setenv("GIT_TERMINAL_PROMPT", "1")

	// Act.
	if _, err := git.CurrentBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, "GIT_TERMINAL_PROMPT"); len(got) != 1 || got[0] != "GIT_TERMINAL_PROMPT=0" {
		t.Fatalf("the child sees GIT_TERMINAL_PROMPT %v, want exactly [GIT_TERMINAL_PROMPT=0]", got)
	}
}

func TestPinnedLocaleReachesTheChild(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("main\n"))
	t.Setenv("LC_ALL", "fr_FR.UTF-8")

	// Act.
	if _, err := git.CurrentBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, "LC_ALL"); len(got) != 1 || got[0] != "LC_ALL=C" {
		t.Fatalf("the child sees LC_ALL %v, want exactly [LC_ALL=C]", got)
	}
}

func TestUnrelatedInheritedBindingReachesTheChild(t *testing.T) {
	// Arrange: the scrub is narrow. The git identity, PATH and HOME must
	// survive, or the daemon's git could not commit at all.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("main\n"))
	t.Setenv("GIT_AUTHOR_NAME", "Someone")

	// Act.
	if _, err := git.CurrentBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, "GIT_AUTHOR_NAME"); len(got) != 1 || got[0] != "GIT_AUTHOR_NAME=Someone" {
		t.Fatalf("the child sees GIT_AUTHOR_NAME %v, want it preserved", got)
	}
}

// --- `-C dir` is the only selector --------------------------------------

func TestTheDirectoryIsPassedAsDashC(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("main\n"))

	// Act.
	if _, err := git.CurrentBranch(context.Background(), "/some/repo"); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if got := fake.only().dashCDir(); got != "/some/repo" {
		t.Fatalf("the call selected %q, want `-C /some/repo`", got)
	}
}

func TestTheChildIsNotChdiredIntoTheRepository(t *testing.T) {
	// Arrange: selecting the repository by spawning git INSIDE it would make
	// the daemon's own working directory part of the contract.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("main\n"))
	here, err := os.Getwd()
	if err != nil {
		t.Fatalf("reading the working directory: %v", err)
	}

	// Act.
	if _, err := git.CurrentBranch(context.Background(), "/some/repo"); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if got := fake.only().Cwd; got != here {
		t.Fatalf("git was spawned in %q, want the daemon's own %q", got, here)
	}
}

// --- failure evidence ----------------------------------------------------

func TestFailureCarriesGitExitCode(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: Needed a single revision\n"))

	// Act.
	_, err := git.ResolveRef(context.Background(), "/repo", "nope")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("ResolveRef error = %v (%T), want a *gitclient.Error", err, err)
	}
	if failure.ExitCode != 128 {
		t.Fatalf("Error.ExitCode = %d, want 128", failure.ExitCode)
	}
}

func TestFailureCarriesGitStderrVerbatim(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	const stderr = "fatal: Needed a single revision\n"
	newFakeGit(t, fails(128, stderr))

	// Act.
	_, err := git.ResolveRef(context.Background(), "/repo", "nope")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("ResolveRef error = %v (%T), want a *gitclient.Error", err, err)
	}
	if failure.Stderr != stderr {
		t.Fatalf("Error.Stderr = %q, want %q verbatim", failure.Stderr, stderr)
	}
}

func TestFailureCarriesGitStdoutVerbatim(t *testing.T) {
	// Arrange: a git that printed something before failing. Dropping it would
	// throw away half the evidence.
	git, _ := newTestClient(t)
	newFakeGit(t, gitFixture{Stdout: "partial output\n", Stderr: "fatal: boom\n", Exit: 1})

	// Act.
	_, err := git.ResolveRef(context.Background(), "/repo", "nope")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("ResolveRef error = %v (%T), want a *gitclient.Error", err, err)
	}
	if failure.Stdout != "partial output\n" {
		t.Fatalf("Error.Stdout = %q, want it preserved", failure.Stdout)
	}
}

func TestFailureCarriesTheDirectory(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(1, "boom\n"))

	// Act.
	_, err := git.ResolveRef(context.Background(), "/some/repo", "nope")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("ResolveRef error = %v (%T), want a *gitclient.Error", err, err)
	}
	if failure.Dir != "/some/repo" {
		t.Fatalf("Error.Dir = %q, want %q", failure.Dir, "/some/repo")
	}
}

func TestFailureCarriesTheSubcommandVectorWithoutTheDashC(t *testing.T) {
	// Arrange: Dir already records the `-C` directory, so repeating it in Args
	// would make the rendered message say it twice.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(1, "boom\n"))

	// Act.
	_, err := git.ResolveRef(context.Background(), "/repo", "nope")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("ResolveRef error = %v (%T), want a *gitclient.Error", err, err)
	}
	if len(failure.Args) == 0 || failure.Args[0] != "rev-parse" {
		t.Fatalf("Error.Args = %v, want the rev-parse vector with no `-C`", failure.Args)
	}
	if containsEntry(failure.Args, "-C") {
		t.Fatalf("Error.Args = %v, want no `-C`", failure.Args)
	}
}

func TestErrorMessageQuotesGitStderr(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: not a valid object name\n"))

	// Act.
	_, err := git.ResolveRef(context.Background(), "/repo", "nope")

	// Assert.
	if !strings.Contains(err.Error(), "fatal: not a valid object name") {
		t.Fatalf("Error() = %q, want it to quote git's own words", err.Error())
	}
}

// --- logging -------------------------------------------------------------

func TestFailureIsLoggedAtErrorExactlyOnce(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(1, "boom\n"))

	// Act.
	_, _ = git.ResolveRef(context.Background(), "/repo", "nope")

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

func TestFailureRecordCarriesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(1, "boom\n"))

	// Act.
	_, _ = git.ResolveRef(context.Background(), "/repo", "nope")

	// Assert.
	record, ok := recordFor(surfaces.records(), "error", "daemon.gitclient.resolve_ref")
	if !ok {
		t.Fatalf("no error record for resolve_ref")
	}
	if record.Context["stderr"] != "boom\n" {
		t.Fatalf("the record's stderr = %v, want git's own words", record.Context["stderr"])
	}
}

func TestOrdinaryPathIsLoggedAtDebug(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("main\n"))

	// Act.
	if _, err := git.CurrentBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if _, ok := recordFor(surfaces.records(), "debug", "daemon.gitclient.current_branch"); !ok {
		t.Fatalf("no debug record for daemon.gitclient.current_branch; every branch logs")
	}
}

// --- a git that never ran ------------------------------------------------

func TestUnrunnableGitReportsNoExitStatus(t *testing.T) {
	// Arrange: a PATH with no git on it at all.
	git, _ := newTestClient(t)
	t.Setenv("PATH", filepath.Join(t.TempDir(), "empty"))

	// Act.
	_, err := git.CurrentBranch(context.Background(), "/repo")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("CurrentBranch error = %v (%T), want a *gitclient.Error", err, err)
	}
	if failure.ExitCode != -1 {
		t.Fatalf("Error.ExitCode = %d, want -1 for a git that never ran", failure.ExitCode)
	}
}

func TestUnrunnableGitCarriesTheSpawnFailureAsEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	t.Setenv("PATH", filepath.Join(t.TempDir(), "empty"))

	// Act.
	_, err := git.CurrentBranch(context.Background(), "/repo")

	// Assert: git wrote no stderr, so the reason it could not be spawned takes
	// its place rather than leaving the evidence empty.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("CurrentBranch error = %v (%T), want a *gitclient.Error", err, err)
	}
	if strings.TrimSpace(failure.Stderr) == "" {
		t.Fatalf("Error.Stderr is empty; the spawn failure is the only evidence there is")
	}
}

func TestUnrunnableGitIsLoggedAtError(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	t.Setenv("PATH", filepath.Join(t.TempDir(), "empty"))

	// Act.
	_, _ = git.CurrentBranch(context.Background(), "/repo")

	// Assert.
	if _, ok := recordFor(surfaces.records(), "error", "daemon.gitclient.current_branch"); !ok {
		t.Fatalf("a git that could not be run must be logged at ERROR")
	}
}

// containsEntry reports whether values carries that exact string.
func containsEntry(values []string, want string) bool {
	for _, value := range values {
		if value == want {
			return true
		}
	}
	return false
}
