package gitclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"
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

func TestScrubEnvStripsInheritedHookMarkers(t *testing.T) {
	tests := []struct {
		name    string
		binding string
	}{
		{name: "merge queue marker", binding: MergeQueueMarker + "=1"},
		{name: "owner override", binding: OwnerOverride + "=1"},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: a daemon started from a shell that exported the binding.
			env := []string{"PATH=/usr/bin", test.binding}

			// Act.
			scrubbed := scrubEnv(env)

			// Assert.
			if containsEntry(scrubbed, test.binding) {
				t.Fatalf("scrubEnv kept %q; a hook marker reaches git only where a method sets it", test.binding)
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

func TestAnInheritedMergeQueueMarkerNeverReachesAnotherGit(t *testing.T) {
	// Arrange: only FastForward may vouch for a move of master.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("main\n"))
	t.Setenv(MergeQueueMarker, "1")

	// Act.
	if _, err := git.CurrentBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, MergeQueueMarker); len(got) != 0 {
		t.Fatalf("the child's environment carries %v; only the queue's fast-forward sets the marker", got)
	}
}

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

// --- a git somebody else killed -------------------------------------------
//
// A git ended by a signal with the caller's context ALIVE is neither an exit
// status nor our own cancellation: ExitCode() answers -1 and the child wrote
// no stderr, so a record that reports it as "git exited nonzero, exit -1" with
// an empty stderr names no cause at all. Observed as exactly that under an
// overloaded integration run.

func TestAGitKilledByASignalIsAFailureNotAJudgeableExitCode(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, killedBy(syscall.SIGKILL))

	// Act.
	_, err := git.IsClean(context.Background(), "/repo")

	// Assert: a failure, so no caller can read the -1 as "dirty".
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("IsClean error = %v (%T), want a *gitclient.Error", err, err)
	}
}

func TestAGitKilledByASignalCarriesTheSignalAsEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, killedBy(syscall.SIGKILL))

	// Act.
	_, err := git.CurrentBranch(context.Background(), "/repo")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("CurrentBranch error = %v (%T), want a *gitclient.Error", err, err)
	}
	if failure.Signal != syscall.SIGKILL.String() {
		t.Fatalf("Error.Signal = %q, want %q", failure.Signal, syscall.SIGKILL.String())
	}
}

func TestAGitKilledByASignalIsNotReadAsACancellation(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, killedBy(syscall.SIGKILL))

	// Act: the context is never cancelled, so the daemon asked for nothing.
	_, err := git.CurrentBranch(context.Background(), "/repo")

	// Assert.
	if IsCancelled(err) {
		t.Fatalf("a signalled git reported %v, want a failure rather than a cancellation", err)
	}
}

func TestAGitKilledByASignalIsRecordedWithTheSignal(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, killedBy(syscall.SIGKILL))

	// Act.
	_, _ = git.CurrentBranch(context.Background(), "/repo")

	// Assert.
	record, ok := recordFor(surfaces.records(), "error", "daemon.gitclient.current_branch")
	if !ok {
		t.Fatalf("no error record for a killed git in %+v", surfaces.records())
	}
	if got := record.Context["signal"]; got != syscall.SIGKILL.String() {
		t.Fatalf("the record's signal = %v, want %q", got, syscall.SIGKILL.String())
	}
}

func TestAGitKilledByASignalIsRecordedWithItsPid(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	fake := newFakeGit(t, killedBy(syscall.SIGKILL))

	// Act.
	_, _ = git.CurrentBranch(context.Background(), "/repo")

	// Assert: the pid is the one the killed child really had.
	record, ok := recordFor(surfaces.records(), "error", "daemon.gitclient.current_branch")
	if !ok {
		t.Fatalf("no error record for a killed git in %+v", surfaces.records())
	}
	if got, want := record.Context["pid"], fake.only().Pid; got != want {
		t.Fatalf("the record's pid = %v, want the killed child's %d", got, want)
	}
}

func TestAGitThatExitedNonzeroIsRecordedWithItsPid(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	fake := newFakeGit(t, fails(1, "boom\n"))

	// Act.
	_, _ = git.ResolveRef(context.Background(), "/repo", "nope")

	// Assert.
	record, ok := recordFor(surfaces.records(), "error", "daemon.gitclient.resolve_ref")
	if !ok {
		t.Fatalf("no error record for resolve_ref in %+v", surfaces.records())
	}
	if got, want := record.Context["pid"], fake.only().Pid; got != want {
		t.Fatalf("the record's pid = %v, want the child's %d", got, want)
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

func TestUnrunnableGitIsRecordedWithoutAPid(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	t.Setenv("PATH", filepath.Join(t.TempDir(), "empty"))

	// Act.
	_, _ = git.CurrentBranch(context.Background(), "/repo")

	// Assert: no process ever existed, so none is named.
	record, ok := recordFor(surfaces.records(), "error", "daemon.gitclient.current_branch")
	if !ok {
		t.Fatalf("no error record for an unrunnable git in %+v", surfaces.records())
	}
	if pid, named := record.Context["pid"]; named {
		t.Fatalf("the record names pid %v for a git that never started", pid)
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

// --- cancellation is not a failure --------------------------------------

// TestInvokeClassifiesEveryOutcome is the classification table. A git THE
// DAEMON stopped, a git that decided against us, and a git that never ran are
// three different facts, and the log record must say which.
func TestInvokeClassifiesEveryOutcome(t *testing.T) {
	tests := []struct {
		name string
		// arrange builds the context and installs the fake, returning the
		// context the call runs under.
		arrange func(t *testing.T) context.Context
		// wantCancelled is whether the error must be a *Cancelled.
		wantCancelled bool
		// wantExitCode is the *Error's exit code, when a failure is wanted.
		wantExitCode int
		// wantLevel is the level the one record about the outcome must carry.
		wantLevel string
	}{
		{
			name: "a cancelled context is a cancellation",
			arrange: func(t *testing.T) context.Context {
				newFakeGit(t, ok("main\n"))
				ctx, cancel := context.WithCancel(context.Background())
				cancel()
				return ctx
			},
			wantCancelled: true,
			wantLevel:     "info",
		},
		{
			name: "an exceeded deadline is a cancellation",
			arrange: func(t *testing.T) context.Context {
				newFakeGit(t, ok("main\n"))
				// A deadline already in the past: the context is done before
				// the spawn, with no wait on the clock at all.
				ctx, cancel := context.WithDeadline(context.Background(), time.Now().Add(-time.Second))
				t.Cleanup(cancel)
				return ctx
			},
			wantCancelled: true,
			wantLevel:     "info",
		},
		{
			name: "a nonzero exit is a failure carrying its code",
			arrange: func(t *testing.T) context.Context {
				newFakeGit(t, fails(3, "fatal: not a git repository\n"))
				return context.Background()
			},
			wantExitCode: 3,
			wantLevel:    "error",
		},
		{
			name: "a git that never ran is a failure",
			arrange: func(t *testing.T) context.Context {
				newFakeGit(t, ok("main\n"))
				// No git on PATH at all: the spawn itself fails, so there is
				// no exit status and -1 marks its absence.
				t.Setenv("PATH", t.TempDir())
				return context.Background()
			},
			wantExitCode: -1,
			wantLevel:    "error",
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			git, surfaces := newTestClient(t)
			ctx := test.arrange(t)

			// Act.
			_, err := git.CurrentBranch(ctx, "/repo")

			// Assert.
			if err == nil {
				t.Fatal("CurrentBranch succeeded; every case here must report an error")
			}
			if got := IsCancelled(err); got != test.wantCancelled {
				t.Fatalf("IsCancelled(%v) = %t, want %t", err, got, test.wantCancelled)
			}
			if !test.wantCancelled {
				var failure *Error
				if !errors.As(err, &failure) {
					t.Fatalf("error %v is not a *gitclient.Error", err)
				}
				if failure.ExitCode != test.wantExitCode {
					t.Fatalf("ExitCode = %d, want %d", failure.ExitCode, test.wantExitCode)
				}
			}
			if _, found := recordFor(surfaces.records(), test.wantLevel, "daemon.gitclient.current_branch"); !found {
				t.Fatalf("no %s record about the outcome; records = %v", test.wantLevel, surfaces.records())
			}
		})
	}
}

// TestACancelledContextLogsNoError is the defect itself: at shutdown every
// git still in flight was recorded as "git exited nonzero" with exit -1, which
// blamed git for a decision the daemon made.
func TestACancelledContextLogsNoError(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("main\n"))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	if _, err := git.CurrentBranch(ctx, "/repo"); err == nil {
		t.Fatal("CurrentBranch succeeded under a cancelled context")
	}

	// Assert.
	for _, record := range surfaces.records() {
		if record.Level == "error" {
			t.Fatalf("a cancelled git was recorded at ERROR: %+v", record)
		}
	}
}

// TestACancellationNamesItsSubcommand keeps the record diagnosable: the log
// must say WHICH git was stopped.
func TestACancellationNamesItsSubcommand(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("main\n"))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	if _, err := git.CurrentBranch(ctx, "/repo"); err == nil {
		t.Fatal("CurrentBranch succeeded under a cancelled context")
	}

	// Assert.
	record, found := recordFor(surfaces.records(), "info", "daemon.gitclient.current_branch")
	if !found {
		t.Fatalf("no info record; records = %v", surfaces.records())
	}
	if got := record.Context["subcommand"]; got != "rev-parse" {
		t.Fatalf("the record names subcommand %v, want rev-parse", got)
	}
}

// TestACancellationUnwrapsToTheContextError is what lets every caller that
// already asks errors.Is(err, context.Canceled) classify a stopped git without
// knowing this package's types.
func TestACancellationUnwrapsToTheContextError(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("main\n"))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := git.CurrentBranch(ctx, "/repo")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("error %v does not unwrap to context.Canceled", err)
	}
}

// TestAGitKilledMidFlightIsACancellation covers the case the pre-ended
// contexts cannot: a git that really was RUNNING and really was killed, whose
// wait reports a signalled death with an ExitCode of -1.
func TestAGitKilledMidFlightIsACancellation(t *testing.T) {
	// Arrange: a fake git that hangs on the pipe until it is killed.
	git, surfaces := newTestClient(t)
	fifo := newFifo(t)
	newFakeGit(t, blocks(fifo))
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the call runs while the test meets the child at the pipe, then
	// cancels it. The rendezvous is the kernel's, not the clock's.
	done := make(chan error, 1)
	go func() {
		_, err := git.CurrentBranch(ctx, "/repo")
		done <- err
	}()
	awaitOpen(t, fifo)
	cancel()
	err := <-done

	// Assert.
	if !IsCancelled(err) {
		t.Fatalf("a killed git reported %v, want a cancellation", err)
	}
	for _, record := range surfaces.records() {
		if record.Level == "error" {
			t.Fatalf("a killed git was recorded at ERROR: %+v", record)
		}
	}
}

func TestAGitCancelledMidFlightIsRecordedWithItsPid(t *testing.T) {
	// Arrange: a git that is live, proven by the pipe rendezvous, when the
	// context ends.
	git, surfaces := newTestClient(t)
	fifo := newFifo(t)
	fake := newFakeGit(t, blocks(fifo))
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() {
		_, err := git.CurrentBranch(ctx, "/repo")
		done <- err
	}()
	awaitOpen(t, fifo)

	// Act.
	cancel()
	<-done

	// Assert.
	record, ok := recordFor(surfaces.records(), "info", "daemon.gitclient.current_branch")
	if !ok {
		t.Fatalf("no info record for a cancelled git in %+v", surfaces.records())
	}
	if got, want := record.Context["pid"], fake.only().Pid; got != want {
		t.Fatalf("the cancellation record's pid = %v, want the stopped child's %d", got, want)
	}
}

func TestAGitCancelledBeforeItsSpawnIsRecordedWithoutAPid(t *testing.T) {
	// Arrange: the context ends before the client ever spawns.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("main\n"))
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, _ = git.CurrentBranch(ctx, "/repo")

	// Assert.
	record, ok := recordFor(surfaces.records(), "info", "daemon.gitclient.current_branch")
	if !ok {
		t.Fatalf("no info record for a cancelled git in %+v", surfaces.records())
	}
	if pid, named := record.Context["pid"]; named {
		t.Fatalf("the cancellation record names pid %v for a git that never started", pid)
	}
}

// TestTheHookSpellsTheMarkers holds the two spellings of the hook markers
// together: the daemon sets them by the constants, and the repository's
// reference-transaction hook reads them by name.
func TestTheHookSpellsTheMarkers(t *testing.T) {
	// Arrange.
	hook, err := os.ReadFile(filepath.Join("..", "..", "..", "..", "..", "..", ".githooks", "reference-transaction"))
	if err != nil {
		t.Fatalf("reading the reference-transaction hook: %v", err)
	}

	for _, marker := range []string{MergeQueueMarker, OwnerOverride} {
		t.Run(marker, func(t *testing.T) {
			// Act.
			want := `="` + marker + `"`

			// Assert.
			if !strings.Contains(string(hook), want) {
				t.Fatalf("the hook does not bind %s; the daemon and the hook must spell it the same", marker)
			}
		})
	}
}
