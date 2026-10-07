package gitclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

// commitLine renders one line of commitFormat output.
func commitLine(sha, subject, author, at string) string {
	return strings.Join([]string{sha, subject, author, at}, fieldSep)
}

func TestNewRefusesAbsentLogSurfaces(t *testing.T) {
	// Arrange, Act.
	git, err := New(nil)

	// Assert.
	if err == nil {
		t.Fatalf("New(nil) = %v, want a refusal: a client that cannot log is not usable", git)
	}
}

// --- DefaultBranch ------------------------------------------------------

func TestDefaultBranchPrefersOriginHead(t *testing.T) {
	// Arrange: origin/HEAD STATES the answer, so it outranks every guess.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("origin/trunk\n", "symbolic-ref"))

	// Act.
	branch, err := git.DefaultBranch(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}
	if branch != "trunk" {
		t.Fatalf("DefaultBranch = %q, want %q with the remote stripped", branch, "trunk")
	}
}

func TestDefaultBranchConsultsNothingElseWhenOriginHeadAnswers(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("origin/trunk\n", "symbolic-ref"))

	// Act.
	if _, err := git.DefaultBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}

	// Assert.
	fake.assertNever("config")
	fake.assertNever("show-ref")
}

func TestDefaultBranchReadsOriginHeadForTheRightRef(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("origin/trunk\n", "symbolic-ref"))

	// Act.
	if _, err := git.DefaultBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "symbolic-ref", "--short", "refs/remotes/origin/HEAD")
}

func TestDefaultBranchUsesConfiguredInitDefaultBranch(t *testing.T) {
	// Arrange: no origin/HEAD; the configured name exists locally, and so does
	// `main` — the configured name must win.
	git, _ := newTestClient(t)
	newFakeGit(t,
		fails(1, "fatal: ref refs/remotes/origin/HEAD is not a symbolic ref\n", "symbolic-ref"),
		ok("development\n", "config", "--get", "init.defaultBranch"),
		ok("", "show-ref", "--verify", "--quiet", "refs/heads/development"),
		ok("", "show-ref", "--verify", "--quiet", "refs/heads/main"),
	)

	// Act.
	branch, err := git.DefaultBranch(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}
	if branch != "development" {
		t.Fatalf("DefaultBranch = %q, want %q", branch, "development")
	}
}

func TestDefaultBranchIgnoresConfiguredBranchThatDoesNotExist(t *testing.T) {
	// Arrange: init.defaultBranch names a branch this repository never made.
	// Naming it would hand the caller a ref every later command fails on.
	git, _ := newTestClient(t)
	newFakeGit(t,
		fails(1, "no origin/HEAD\n", "symbolic-ref"),
		ok("never-created\n", "config", "--get", "init.defaultBranch"),
		fails(1, "", "show-ref", "--verify", "--quiet", "refs/heads/never-created"),
		ok("", "show-ref", "--verify", "--quiet", "refs/heads/main"),
	)

	// Act.
	branch, err := git.DefaultBranch(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}
	if branch != "main" {
		t.Fatalf("DefaultBranch = %q, want %q", branch, "main")
	}
}

func TestDefaultBranchFallsBackToMain(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t,
		fails(1, "no origin/HEAD\n", "symbolic-ref"),
		fails(1, "", "config", "--get", "init.defaultBranch"),
		ok("", "show-ref", "--verify", "--quiet", "refs/heads/main"),
	)

	// Act.
	branch, err := git.DefaultBranch(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}
	if branch != "main" {
		t.Fatalf("DefaultBranch = %q, want %q", branch, "main")
	}
}

func TestDefaultBranchFallsBackToMaster(t *testing.T) {
	// Arrange: only `master` exists, so the last candidate is the answer.
	git, _ := newTestClient(t)
	newFakeGit(t,
		fails(1, "no origin/HEAD\n", "symbolic-ref"),
		fails(1, "", "config", "--get", "init.defaultBranch"),
		fails(1, "", "show-ref", "--verify", "--quiet", "refs/heads/main"),
		ok("", "show-ref", "--verify", "--quiet", "refs/heads/master"),
	)

	// Act.
	branch, err := git.DefaultBranch(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}
	if branch != "master" {
		t.Fatalf("DefaultBranch = %q, want %q", branch, "master")
	}
}

func TestDefaultBranchFailsWhenNothingMatches(t *testing.T) {
	// Arrange: no origin/HEAD, no configured name, no main, no master. The
	// daemon must never invent one.
	git, _ := newTestClient(t)
	newFakeGit(t,
		fails(1, "no origin/HEAD\n", "symbolic-ref"),
		fails(1, "", "config"),
		fails(1, "", "show-ref"),
	)

	// Act.
	branch, err := git.DefaultBranch(context.Background(), "/repo")

	// Assert.
	if err == nil {
		t.Fatalf("DefaultBranch = %q, want a loud failure", branch)
	}
}

func TestDefaultBranchFailureIsLoggedAtError(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t,
		fails(1, "no origin/HEAD\n", "symbolic-ref"),
		fails(1, "", "config"),
		fails(1, "", "show-ref"),
	)

	// Act.
	_, _ = git.DefaultBranch(context.Background(), "/repo")

	// Assert.
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.default_branch"); !found {
		t.Fatalf("an undeterminable default branch must be logged at ERROR")
	}
}

func TestDefaultBranchVerifiesCandidatesUnderRefsHeadsOnly(t *testing.T) {
	// Arrange: `refs/heads/<name>` with --verify is exact, so a TAG named
	// `main` can never be mistaken for the branch.
	git, _ := newTestClient(t)
	fake := newFakeGit(t,
		fails(1, "no origin/HEAD\n", "symbolic-ref"),
		fails(1, "", "config"),
		ok("", "show-ref"),
	)

	// Act.
	if _, err := git.DefaultBranch(context.Background(), "/repo"); err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}

	// Assert.
	fake.assertSubject(2, "show-ref", "--verify", "--quiet", "refs/heads/main")
}

// --- BranchExists -------------------------------------------------------

func TestBranchExistsAnswersTrueForALocalBranch(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("", "show-ref"))

	// Act.
	got, err := git.BranchExists(context.Background(), "/repo", "flaky-login-test")

	// Assert.
	if err != nil {
		t.Fatalf("BranchExists: %v", err)
	}
	if !got {
		t.Fatal("BranchExists = false, want true for a branch show-ref verified")
	}
}

// TestBranchExistsAnswersFalseWithoutAnErrorRecord pins WHY this method exists
// beside ResolveRef: the naming call's collision probe asks about branches
// that are supposed not to exist, and an absent branch is the ANSWER it wants
// — never an error record an operator has to explain away.
func TestBranchExistsAnswersFalseWithoutAnErrorRecord(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(1, "", "show-ref"))

	// Act.
	got, err := git.BranchExists(context.Background(), "/repo", "not-a-branch")

	// Assert.
	if err != nil {
		t.Fatalf("BranchExists: %v", err)
	}
	if got {
		t.Fatal("BranchExists = true, want false for a branch show-ref did not verify")
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.branch_exists"); found {
		t.Fatal("an absent branch was recorded at ERROR; it is an ordinary answer")
	}
}

func TestBranchExistsVerifiesUnderRefsHeadsOnly(t *testing.T) {
	// Arrange: a TAG named the same must never be mistaken for the branch.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "show-ref"))

	// Act.
	if _, err := git.BranchExists(context.Background(), "/repo", "v1"); err != nil {
		t.Fatalf("BranchExists: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "show-ref", "--verify", "--quiet", "refs/heads/v1")
}

// --- RepositoryOf -------------------------------------------------------

func TestRepositoryOfAnswersTheMainWorktree(t *testing.T) {
	// Arrange: `worktree list --porcelain` lists the main worktree FIRST.
	git, _ := newTestClient(t)
	main := existingDir(t, "main")
	linked := existingDir(t, "linked")
	newFakeGit(t, ok("worktree "+main+"\nHEAD abc\n\nworktree "+linked+"\n", "worktree", "list"))

	// Act.
	got, inRepository, err := git.RepositoryOf(context.Background(), linked)

	// Assert.
	if err != nil {
		t.Fatalf("RepositoryOf: %v", err)
	}
	if !inRepository {
		t.Fatal("RepositoryOf = not in a repository, want the main worktree git listed first")
	}
	// The answer is CANONICALIZED, which on macOS resolves /var to /private/var.
	canonical, err := filepath.EvalSymlinks(main)
	if err != nil {
		t.Fatalf("EvalSymlinks(%s): %v", main, err)
	}
	if got != canonical {
		t.Fatalf("RepositoryOf = %q, want the first listed worktree %q", got, canonical)
	}
}

// TestRepositoryOfAnswersFalseWithoutAnErrorRecord pins WHY this method exists
// beside MainWorktree: a person picks the path, so "that is not in a
// repository" is the answer the question was asked to get -- never a fault an
// operator has to explain away.
func TestRepositoryOfAnswersFalseWithoutAnErrorRecord(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: not a git repository\n", "worktree", "list"))

	// Act.
	got, inRepository, err := git.RepositoryOf(context.Background(), "/elsewhere")

	// Assert.
	if err != nil {
		t.Fatalf("RepositoryOf: %v", err)
	}
	if inRepository || got != "" {
		t.Fatalf("RepositoryOf = (%q, %v), want no repository", got, inRepository)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.repository_of"); found {
		t.Fatal("a path outside every repository was recorded at ERROR; it is an ordinary answer")
	}
}

// TestRepositoryOfAnswersFalseForABareRepository is the second shape of the
// same answer: git ran fine and named no worktree, so there is no directory to
// record as the repository's.
func TestRepositoryOfAnswersFalseForABareRepository(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("", "worktree", "list"))

	// Act.
	_, inRepository, err := git.RepositoryOf(context.Background(), "/bare")

	// Assert.
	if err != nil {
		t.Fatalf("RepositoryOf: %v", err)
	}
	if inRepository {
		t.Fatal("RepositoryOf = in a repository, want false for a listing with no worktree")
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.repository_of"); found {
		t.Fatal("a bare repository was recorded at ERROR; it is an ordinary answer")
	}
}

func TestRepositoryOfAsksGitWithTheWorktreeListing(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := existingDir(t, "repo")
	fake := newFakeGit(t, ok("worktree "+dir+"\n", "worktree", "list"))

	// Act.
	if _, _, err := git.RepositoryOf(context.Background(), dir); err != nil {
		t.Fatalf("RepositoryOf: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "list", "--porcelain")
}

// --- ResolveRef ---------------------------------------------------------

func TestResolveRefAnswersTheFullSha(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	const sha = "9f8e7d6c5b4a39281706f5e4d3c2b1a098765432"
	newFakeGit(t, ok(sha+"\n"))

	// Act.
	got, err := git.ResolveRef(context.Background(), "/repo", "main")

	// Assert.
	if err != nil {
		t.Fatalf("ResolveRef: %v", err)
	}
	if got != sha {
		t.Fatalf("ResolveRef = %q, want %q", got, sha)
	}
}

func TestResolveRefPeelsTheRefToACommit(t *testing.T) {
	// Arrange: without `^{commit}` an annotated tag resolves to the tag object
	// rather than to the commit the caller wants.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("deadbeef\n"))

	// Act.
	if _, err := git.ResolveRef(context.Background(), "/repo", "v1"); err != nil {
		t.Fatalf("ResolveRef: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "rev-parse", "--verify", "--end-of-options", "v1^{commit}")
}

// --- Worktrees ----------------------------------------------------------

func TestCreateWorktreeMakesTheBranchAndTheTreeInOneCommand(t *testing.T) {
	// Arrange: one command means there is no window in which the branch exists
	// without its tree.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.CreateWorktree(context.Background(), "/repo", "feature/one", "main", "/wt"); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "add", "-b", "feature/one", "/wt", "main")
}

func TestCreateWorktreeFailurePropagatesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: a branch named 'feature/one' already exists\n"))

	// Act.
	err := git.CreateWorktree(context.Background(), "/repo", "feature/one", "main", "/wt")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("CreateWorktree error = %v (%T), want a *gitclient.Error", err, err)
	}
	if !strings.Contains(failure.Stderr, "already exists") {
		t.Fatalf("Error.Stderr = %q, want git's own refusal", failure.Stderr)
	}
}

func TestCreateWorktreeReattachesTheDirectoryToItsLogSinks(t *testing.T) {
	// Arrange: a path an earlier removal detached belongs to the new workspace.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok(""))

	// Act.
	if err := git.CreateWorktree(context.Background(), "/repo", "feature/one", "main", "/wt"); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}

	// Assert.
	if got := strings.Join(surfaces.dirEvents, ","); got != "attach /wt" {
		t.Fatalf("log sink directory events = %q, want the new worktree attached", got)
	}
}

func TestCreateWorktreeThatFailsAttachesNothing(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: a branch named 'feature/one' already exists\n"))

	// Act.
	_ = git.CreateWorktree(context.Background(), "/repo", "feature/one", "main", "/wt")

	// Assert.
	if len(surfaces.dirEvents) != 0 {
		t.Fatalf("log sink directory events = %v, want none for a worktree that was never created", surfaces.dirEvents)
	}
}

func TestCreateWorktreeAttachFailureIsReturnedAndLogged(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	surfaces.dirFailure = errors.New("attach refused")
	newFakeGit(t, ok(""))

	// Act.
	err := git.CreateWorktree(context.Background(), "/repo", "feature/one", "main", "/wt")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "attach refused") {
		t.Fatalf("CreateWorktree = %v, want the attach failure", err)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.create_worktree"); !found {
		t.Fatal("the attach failure was not recorded at ERROR")
	}
}

func TestRestoreWorktreeChecksTheExistingBranchOutWithoutCreatingOne(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.RestoreWorktree(context.Background(), "/repo", "/wt", "feature/one"); err != nil {
		t.Fatalf("RestoreWorktree: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "add", "/wt", "feature/one")
}

func TestRestoreWorktreeFailurePropagatesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: invalid reference: feature/one\n"))

	// Act.
	err := git.RestoreWorktree(context.Background(), "/repo", "/wt", "feature/one")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("RestoreWorktree error = %v (%T), want a *gitclient.Error", err, err)
	}
	if !strings.Contains(failure.Stderr, "invalid reference") {
		t.Fatalf("Error.Stderr = %q, want git's own refusal", failure.Stderr)
	}
}

func TestRestoreWorktreeReattachesTheDirectoryToItsLogSinks(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok(""))

	// Act.
	if err := git.RestoreWorktree(context.Background(), "/repo", "/wt", "feature/one"); err != nil {
		t.Fatalf("RestoreWorktree: %v", err)
	}

	// Assert.
	if got := strings.Join(surfaces.dirEvents, ","); got != "attach /wt" {
		t.Fatalf("log sink directory events = %q, want the restored worktree attached", got)
	}
}

func TestRestoreWorktreeThatFailsAttachesNothing(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: invalid reference: feature/one\n"))

	// Act.
	_ = git.RestoreWorktree(context.Background(), "/repo", "/wt", "feature/one")

	// Assert.
	if len(surfaces.dirEvents) != 0 {
		t.Fatalf("log sink directory events = %v, want none for a worktree that was never restored", surfaces.dirEvents)
	}
}

func TestRestoreWorktreeAttachFailureIsReturnedAndLogged(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	surfaces.dirFailure = errors.New("attach refused")
	newFakeGit(t, ok(""))

	// Act.
	err := git.RestoreWorktree(context.Background(), "/repo", "/wt", "feature/one")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "attach refused") {
		t.Fatalf("RestoreWorktree = %v, want the attach failure", err)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.restore_worktree"); !found {
		t.Fatal("the attach failure was not recorded at ERROR")
	}
}

func TestUnregisterMissingWorktreeRemovesOnlyThatRegistration(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.UnregisterMissingWorktree(context.Background(), "/repo", "/wt"); err != nil {
		t.Fatalf("UnregisterMissingWorktree: %v", err)
	}

	// Assert: no --force (git must still refuse a locked one), and no prune.
	fake.assertSubject(0, "worktree", "remove", "/wt")
}

func TestUnregisterMissingWorktreeFailurePropagatesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: cannot remove a locked working tree;\nuse 'remove -f -f' to override or unlock first\n"))

	// Act.
	err := git.UnregisterMissingWorktree(context.Background(), "/repo", "/wt")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "locked working tree") {
		t.Fatalf("UnregisterMissingWorktree error = %v, want git's own refusal as a *gitclient.Error", err)
	}
}

func TestAddDetachedWorktreeChecksTheCommitOutWithNoBranch(t *testing.T) {
	// Arrange: the merge queue's scratch tree names no branch.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.AddDetachedWorktree(context.Background(), "/repo", "/queue-tree", "abc123"); err != nil {
		t.Fatalf("AddDetachedWorktree: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "add", "--detach", "/queue-tree", "abc123")
}

func TestAddDetachedWorktreeFailurePropagatesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: '/queue-tree' already exists\n"))

	// Act.
	err := git.AddDetachedWorktree(context.Background(), "/repo", "/queue-tree", "abc123")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "already exists") {
		t.Fatalf("AddDetachedWorktree error = %v (%T), want git's own refusal as a *gitclient.Error", err, err)
	}
}

func TestAddDetachedWorktreeAttachesNoLogSink(t *testing.T) {
	// Arrange: the scratch tree is no workspace, so no workspace sink is in it.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok(""))

	// Act.
	if err := git.AddDetachedWorktree(context.Background(), "/repo", "/queue-tree", "abc123"); err != nil {
		t.Fatalf("AddDetachedWorktree: %v", err)
	}

	// Assert.
	if len(surfaces.dirEvents) != 0 {
		t.Fatalf("log sink directory events = %v, want none for the queue's scratch tree", surfaces.dirEvents)
	}
}

func TestFastForwardRefusesAnythingButAFastForward(t *testing.T) {
	// Arrange: --ff-only is what keeps an untested tree off the branch.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.FastForward(context.Background(), "/target", "abc123"); err != nil {
		t.Fatalf("FastForward: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "merge", "--ff-only", "abc123")
}

func TestFastForwardCarriesTheMergeQueueMarker(t *testing.T) {
	// Arrange: the marker is how the repository's hook knows the queue moved
	// master.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.FastForward(context.Background(), "/target", "abc123"); err != nil {
		t.Fatalf("FastForward: %v", err)
	}

	// Assert: exactly one binding, the queue's own.
	if got := envValues(fake.only().Env, MergeQueueMarker); len(got) != 1 || got[0] != MergeQueueMarker+"=1" {
		t.Fatalf("%s bindings = %v, want exactly [%s=1]", MergeQueueMarker, got, MergeQueueMarker)
	}
}

func TestFastForwardReplacesAnInheritedMergeQueueMarker(t *testing.T) {
	// Arrange: an inherited value is scrubbed, so the method's is the only one.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))
	t.Setenv(MergeQueueMarker, "stale")

	// Act.
	if err := git.FastForward(context.Background(), "/target", "abc123"); err != nil {
		t.Fatalf("FastForward: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, MergeQueueMarker); len(got) != 1 || got[0] != MergeQueueMarker+"=1" {
		t.Fatalf("%s bindings = %v, want exactly [%s=1]", MergeQueueMarker, got, MergeQueueMarker)
	}
}

func TestFastForwardNeverCarriesTheOwnerOverride(t *testing.T) {
	// Arrange: the daemon never acts as the owner.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))
	t.Setenv(OwnerOverride, "1")

	// Act.
	if err := git.FastForward(context.Background(), "/target", "abc123"); err != nil {
		t.Fatalf("FastForward: %v", err)
	}

	// Assert.
	if got := envValues(fake.only().Env, OwnerOverride); len(got) != 0 {
		t.Fatalf("the fast-forward carries %v; the owner override is never the daemon's", got)
	}
}

func TestFastForwardRunsInTheTargetCheckout(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.FastForward(context.Background(), "/target", "abc123"); err != nil {
		t.Fatalf("FastForward: %v", err)
	}

	// Assert.
	if got := fake.only().dashCDir(); got != "/target" {
		t.Fatalf("FastForward ran in %q, want the target checkout", got)
	}
}

func TestFastForwardRefusalPropagatesTheGitEvidence(t *testing.T) {
	// Arrange: a branch that moved is git's refusal, never a merge.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: Not possible to fast-forward, aborting.\n"))

	// Act.
	err := git.FastForward(context.Background(), "/target", "abc123")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "Not possible to fast-forward") {
		t.Fatalf("FastForward error = %v (%T), want git's own refusal as a *gitclient.Error", err, err)
	}
}

func TestIsAncestorAsksMergeBase(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if _, err := git.IsAncestor(context.Background(), "/repo", "feature", "abc123"); err != nil {
		t.Fatalf("IsAncestor: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "merge-base", "--is-ancestor", "feature", "abc123")
}

func TestIsAncestorAnswers(t *testing.T) {
	tests := []struct {
		name    string
		fixture gitFixture
		want    bool
	}{
		{name: "exit 0 is an ancestor", fixture: ok(""), want: true},
		{name: "exit 1 is not an ancestor", fixture: fails(1, ""), want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			git, _ := newTestClient(t)
			newFakeGit(t, tc.fixture)

			// Act.
			got, err := git.IsAncestor(context.Background(), "/repo", "feature", "abc123")

			// Assert.
			if err != nil || got != tc.want {
				t.Fatalf("IsAncestor = %v, %v; want %v with no error", got, err, tc.want)
			}
		})
	}
}

func TestIsAncestorThatCannotTellIsAFailure(t *testing.T) {
	// Arrange: an unknown ref is neither answer.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: Not a valid object name feature\n"))

	// Act.
	_, err := git.IsAncestor(context.Background(), "/repo", "feature", "abc123")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || failure.ExitCode != 128 {
		t.Fatalf("IsAncestor error = %v (%T), want git's own exit-128 failure", err, err)
	}
}

func TestRemoveWorktreeDetachesTheDirectoryBeforeGitRuns(t *testing.T) {
	// Arrange: a sink opened mid-removal must not re-create the directory.
	git, surfaces := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t, gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir}, ok("", "worktree", "prune"))

	// Act.
	if err := git.RemoveWorktree(context.Background(), "/repo", worktreeDir); err != nil {
		t.Fatalf("RemoveWorktree: %v", err)
	}

	// Assert.
	if got := strings.Join(surfaces.dirEvents, ","); got != "detach "+worktreeDir {
		t.Fatalf("log sink directory events = %q, want the worktree detached", got)
	}
}

func TestRemoveWorktreeDetachFailureRemovesNothing(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	surfaces.dirFailure = errors.New("detach refused")
	worktreeDir := existingDir(t, "wt")
	fake := newFakeGit(t, ok("", "worktree", "prune"))

	// Act.
	err := git.RemoveWorktree(context.Background(), "/repo", worktreeDir)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "detach refused") {
		t.Fatalf("RemoveWorktree = %v, want the detach failure", err)
	}
	fake.assertNever("worktree", "remove")
	fake.assertNever("worktree", "prune")
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.remove_worktree"); !found {
		t.Fatal("the detach failure was not recorded at ERROR")
	}
}

func TestRemoveWorktreeForcesTheRemovalThenPrunes(t *testing.T) {
	// Arrange: --force is what a tree parked mid-merge needs; the prune keeps
	// git's administrative record from outliving the directory.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	fake := newFakeGit(t, gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir}, ok("", "worktree", "prune"))

	// Act.
	if err := git.RemoveWorktree(context.Background(), "/repo", worktreeDir); err != nil {
		t.Fatalf("RemoveWorktree: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "remove", "--force", worktreeDir)
	fake.assertSubject(1, "worktree", "prune")
}

func TestRemoveWorktreeIsANoOpForADirectoryAlreadyGone(t *testing.T) {
	// Arrange: the exact double teardown the old merge pipeline logged loud
	// failures for. git exits 128 for a tree that is not there, and a second
	// teardown is an ordinary event rather than a fault.
	git, _ := newTestClient(t)
	gone := filepath.Join(t.TempDir(), "already-gone")
	fake := newFakeGit(t, ok("", "worktree", "prune"))

	// Act.
	err := git.RemoveWorktree(context.Background(), "/repo", gone)

	// Assert.
	if err != nil {
		t.Fatalf("RemoveWorktree = %v, want a silent no-op", err)
	}
	fake.assertNever("worktree", "remove")
}

func TestRemoveWorktreePrunesEvenWhenTheDirectoryWasAlreadyGone(t *testing.T) {
	// Arrange: a stale registration is precisely what the prune exists to
	// retire, so skipping the removal must not skip the prune.
	git, _ := newTestClient(t)
	gone := filepath.Join(t.TempDir(), "already-gone")
	fake := newFakeGit(t, ok("", "worktree", "prune"))

	// Act.
	if err := git.RemoveWorktree(context.Background(), "/repo", gone); err != nil {
		t.Fatalf("RemoveWorktree: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "prune")
}

func TestRemoveWorktreeLogsNoErrorForADirectoryAlreadyGone(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	gone := filepath.Join(t.TempDir(), "already-gone")
	newFakeGit(t, ok("", "worktree", "prune"))

	// Act.
	if err := git.RemoveWorktree(context.Background(), "/repo", gone); err != nil {
		t.Fatalf("RemoveWorktree: %v", err)
	}

	// Assert.
	if record, found := recordFor(surfaces.records(), "error", "daemon.gitclient.remove_worktree"); found {
		t.Fatalf("removing an already-removed worktree logged an ERROR: %+v", record)
	}
}

func TestRemoveWorktreeAcceptsARefusalWhoseTreeIsGoneAnyway(t *testing.T) {
	// Arrange: git took the directory away and then reported a failure. THE
	// POSTCONDITION decides success, not the exit status of any one step.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t,
		gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir, Stderr: "fatal: is not a working tree\n", Exit: 128},
		ok("", "worktree", "prune"),
	)

	// Act.
	err := git.RemoveWorktree(context.Background(), "/repo", worktreeDir)

	// Assert.
	if err != nil {
		t.Fatalf("RemoveWorktree = %v, want success: the directory is gone, which is what was asked", err)
	}
}

func TestRemoveWorktreeFailsWhenTheTreeSurvives(t *testing.T) {
	// Arrange: git refused and the directory is still there. That is a real
	// failure and stays loud.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t,
		fails(128, "fatal: contains modified or untracked files\n", "worktree", "remove"),
		ok("", "worktree", "prune"),
	)

	// Act.
	err := git.RemoveWorktree(context.Background(), "/repo", worktreeDir)

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("RemoveWorktree error = %v (%T), want git's own refusal as a *gitclient.Error", err, err)
	}
}

func TestRemoveWorktreePruneFailurePropagates(t *testing.T) {
	// Arrange: a prune that fails leaves git's record inconsistent, which the
	// caller must hear about.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t,
		gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir},
		fails(1, "fatal: could not prune\n", "worktree", "prune"),
	)

	// Act.
	err := git.RemoveWorktree(context.Background(), "/repo", worktreeDir)

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("RemoveWorktree error = %v (%T), want a *gitclient.Error", err, err)
	}
}

func TestNukeRemovesTheTreeBeforeDeletingTheBranch(t *testing.T) {
	// Arrange: git refuses to delete a branch a registered worktree has
	// checked out, so the order is forced.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	fake := newFakeGit(t,
		gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir},
		ok("", "worktree", "prune"),
		ok("", "show-ref"),
		ok("", "branch", "-D"),
	)

	// Act.
	if err := git.Nuke(context.Background(), "/repo", worktreeDir, "feature/one"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "remove", "--force", worktreeDir)
	fake.assertSubject(3, "branch", "-D", "feature/one")
}

func TestNukeForcesTheBranchDelete(t *testing.T) {
	// Arrange: an unmerged branch is exactly what a nuke is for, and a plain
	// `branch -d` would refuse it.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	fake := newFakeGit(t,
		gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir},
		ok("", "worktree", "prune"),
		ok("", "show-ref"),
		ok("", "branch"),
	)

	// Act.
	if err := git.Nuke(context.Background(), "/repo", worktreeDir, "feature/one"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	call, found := fake.find("branch")
	if !found {
		t.Fatalf("no branch deletion was issued")
	}
	if !containsEntry(call.subject(), "-D") {
		t.Fatalf("the branch deletion was %v, want the forced -D", call.subject())
	}
}

func TestNukeIsANoOpForABranchAlreadyGone(t *testing.T) {
	// Arrange: the postcondition already holds, so there is nothing to do and
	// nothing to complain about.
	git, _ := newTestClient(t)
	gone := filepath.Join(t.TempDir(), "already-gone")
	fake := newFakeGit(t,
		ok("", "worktree", "prune"),
		fails(1, "", "show-ref"),
	)

	// Act.
	err := git.Nuke(context.Background(), "/repo", gone, "feature/one")

	// Assert.
	if err != nil {
		t.Fatalf("Nuke = %v, want a silent no-op", err)
	}
	fake.assertNever("branch")
}

func TestNukeStopsWhenTheWorktreeRemovalFails(t *testing.T) {
	// Arrange: deleting the branch while its tree is still registered would
	// leave the repository in a state neither the caller nor git expects.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	fake := newFakeGit(t,
		fails(128, "fatal: contains modified or untracked files\n", "worktree", "remove"),
		ok("", "worktree", "prune"),
	)

	// Act.
	err := git.Nuke(context.Background(), "/repo", worktreeDir, "feature/one")

	// Assert.
	if err == nil {
		t.Fatalf("Nuke = nil, want the worktree removal's failure")
	}
	fake.assertNever("branch")
}

// --- Repository identity ------------------------------------------------

func TestCommonDirAbsolutizesARelativeAnswer(t *testing.T) {
	// Arrange: `rev-parse --git-common-dir` answers `.git` relatively for a
	// plain checkout, which is useless as an identity.
	git, _ := newTestClient(t)
	repoDir := existingDir(t, "repo")
	gitDir := filepath.Join(repoDir, ".git")
	if err := os.MkdirAll(gitDir, 0o755); err != nil {
		t.Fatalf("making %s: %v", gitDir, err)
	}
	newFakeGit(t, ok(".git\n"))

	// Act.
	common, err := git.CommonDir(context.Background(), repoDir)

	// Assert.
	if err != nil {
		t.Fatalf("CommonDir: %v", err)
	}
	if !filepath.IsAbs(common) {
		t.Fatalf("CommonDir = %q, want an absolute path", common)
	}
}

func TestCommonDirCanonicalizesSymlinks(t *testing.T) {
	// Arrange: the same repository reached through a symlink. On macOS the
	// /tmp -> /private/tmp link makes this the ordinary case, not an exotic
	// one, and an uncanonicalized answer would make one repository compare as
	// two.
	git, _ := newTestClient(t)
	target := existingDir(t, "real.git")
	link := filepath.Join(t.TempDir(), "linked.git")
	if err := os.Symlink(target, link); err != nil {
		t.Fatalf("symlink %s -> %s: %v", link, target, err)
	}
	newFakeGit(t, ok(link+"\n"))

	// Act.
	common, err := git.CommonDir(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("CommonDir: %v", err)
	}
	want, err := filepath.EvalSymlinks(target)
	if err != nil {
		t.Fatalf("EvalSymlinks(%q): %v", target, err)
	}
	if common != want {
		t.Fatalf("CommonDir = %q, want the canonicalized %q", common, want)
	}
}

func TestCommonDirFailsOnAPathThatCannotBeCanonicalized(t *testing.T) {
	// Arrange: an identity that cannot be resolved is not an identity, and
	// answering with the raw path would let two repositories compare equal by
	// accident.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(filepath.Join(t.TempDir(), "no-such-dir")+"\n"))

	// Act.
	_, err := git.CommonDir(context.Background(), "/repo")

	// Assert.
	if err == nil {
		t.Fatalf("CommonDir = nil error, want a failure for an unresolvable path")
	}
}

func TestCommonDirFailureIsLoggedAtError(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok(filepath.Join(t.TempDir(), "no-such-dir")+"\n"))

	// Act.
	_, _ = git.CommonDir(context.Background(), "/repo")

	// Assert.
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.common_dir"); !found {
		t.Fatalf("an uncanonicalizable common dir must be logged at ERROR")
	}
}

func TestSameRepoHoldsForOneRepositoryReachedTwoWays(t *testing.T) {
	// Arrange: a worktree and its parent checkout report the same common dir,
	// which is the answer the merge-method split needs.
	git, _ := newTestClient(t)
	common := existingDir(t, "repo.git")
	newFakeGit(t, ok(common+"\n"))

	// Act.
	same, err := git.SameRepo(context.Background(), "/repo", "/repo/worktrees/one")

	// Assert.
	if err != nil {
		t.Fatalf("SameRepo: %v", err)
	}
	if !same {
		t.Fatalf("SameRepo(one repository reached two ways) = false, want true")
	}
}

func TestSameRepoIsFalseForDistinctCommonDirs(t *testing.T) {
	// Arrange: two directories that report different common dirs are two
	// repositories, and the merge orchestrator must take the non-Emacs method
	// for them.
	git, _ := newTestClient(t)
	first := existingDir(t, "first.git")
	second := existingDir(t, "second.git")
	newFakeGit(t,
		gitFixture{Dir: "/first", Stdout: first + "\n"},
		gitFixture{Dir: "/second", Stdout: second + "\n"},
	)

	// Act.
	same, err := git.SameRepo(context.Background(), "/first", "/second")

	// Assert.
	if err != nil {
		t.Fatalf("SameRepo: %v", err)
	}
	if same {
		t.Fatalf("SameRepo(two separate repositories) = true, want false")
	}
}

func TestSameRepoPropagatesAFailureReadingEitherIdentity(t *testing.T) {
	// Arrange: an unanswerable identity is not a "not the same" answer.
	git, _ := newTestClient(t)
	first := existingDir(t, "first.git")
	newFakeGit(t,
		gitFixture{Dir: "/first", Stdout: first + "\n"},
		gitFixture{Dir: "/second", Stderr: "fatal: not a git repository\n", Exit: 128},
	)

	// Act.
	_, err := git.SameRepo(context.Background(), "/first", "/second")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("SameRepo error = %v (%T), want a *gitclient.Error", err, err)
	}
}

// --- MergeNoFF ----------------------------------------------------------

func TestMergeNoFFNeverFastForwards(t *testing.T) {
	// Arrange: --no-ff is the whole point. A fast-forward would leave neither
	// one commit to revert nor a second-parent range to read.
	git, _ := newTestClient(t)
	fake := newFakeGit(t,
		ok("", "merge"),
		ok(commitLine("aaa", "merge feature", "Test Author", "2026-08-29T10:00:00Z")+"\n", "show"),
	)

	// Act.
	if _, err := git.MergeNoFF(context.Background(), "/repo", "feature", "merge feature"); err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "merge", "--no-ff", "--no-edit", "-m", "merge feature", "feature")
}

func TestMergeNoFFLandsTheMergeCommit(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t,
		ok("", "merge"),
		ok(commitLine("aaaabbbb", "merge feature", "Test Author", "2026-08-29T10:00:00Z")+"\n", "show"),
	)

	// Act.
	outcome, err := git.MergeNoFF(context.Background(), "/repo", "feature", "merge feature")

	// Assert.
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}
	if outcome.Landed == nil || outcome.Landed.SHA != "aaaabbbb" {
		t.Fatalf("MergeOutcome = %+v, want the merge commit aaaabbbb", outcome)
	}
}

func TestMergeNoFFLandedCommitCarriesTheSubjectAndAuthor(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t,
		ok("", "merge"),
		ok(commitLine("aaaabbbb", "merge feature", "Test Author", "2026-08-29T10:00:00Z")+"\n", "show"),
	)

	// Act.
	outcome, err := git.MergeNoFF(context.Background(), "/repo", "feature", "merge feature")

	// Assert.
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}
	if outcome.Landed.Subject != "merge feature" || outcome.Landed.Author != "Test Author" {
		t.Fatalf("Landed = %+v, want the scripted subject and author", *outcome.Landed)
	}
	if outcome.Landed.At.IsZero() {
		t.Fatalf("Landed.At is the zero instant; the author time is a fact git always has")
	}
}

func TestMergeNoFFConflictAnswersTheConflictedFiles(t *testing.T) {
	// Arrange: a conflict is an ANSWER, not a failure.
	git, _ := newTestClient(t)
	newFakeGit(t,
		fails(1, "CONFLICT (content): Merge conflict in shared.txt\n", "merge"),
		ok("shared.txt\x00", "diff"),
	)

	// Act.
	outcome, err := git.MergeNoFF(context.Background(), "/repo", "feature", "merge feature")

	// Assert.
	if err != nil {
		t.Fatalf("MergeNoFF = %v, want a Conflicted answer rather than an error", err)
	}
	if len(outcome.Conflicted) != 1 || outcome.Conflicted[0] != "shared.txt" {
		t.Fatalf("MergeOutcome.Conflicted = %v, want [shared.txt]", outcome.Conflicted)
	}
}

func TestMergeNoFFConflictLandsNothing(t *testing.T) {
	// Arrange: exactly one of Landed and Conflicted is set.
	git, _ := newTestClient(t)
	newFakeGit(t,
		fails(1, "CONFLICT\n", "merge"),
		ok("shared.txt\x00", "diff"),
	)

	// Act.
	outcome, err := git.MergeNoFF(context.Background(), "/repo", "feature", "merge feature")

	// Assert.
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}
	if outcome.Landed != nil {
		t.Fatalf("MergeOutcome.Landed = %+v on a conflict, want nil", *outcome.Landed)
	}
}

func TestMergeNoFFNeverAbortsAConflictedMerge(t *testing.T) {
	// Arrange: the conflicted worktree IS the resolution flow's workbench. An
	// abort here would destroy the state the conflict agent is dispatched into.
	git, _ := newTestClient(t)
	fake := newFakeGit(t,
		fails(1, "CONFLICT\n", "merge"),
		ok("shared.txt\x00", "diff"),
	)

	// Act.
	if _, err := git.MergeNoFF(context.Background(), "/repo", "feature", "merge feature"); err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Assert.
	fake.assertNever("merge", "--abort")
	fake.assertNever("reset")
	fake.assertNever("checkout")
}

func TestMergeNoFFConflictIsLoggedAtWarning(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t,
		fails(1, "CONFLICT\n", "merge"),
		ok("shared.txt\x00", "diff"),
	)

	// Act.
	if _, err := git.MergeNoFF(context.Background(), "/repo", "feature", "merge feature"); err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Assert.
	if _, found := recordFor(surfaces.records(), "warn", "daemon.gitclient.merge_no_ff"); !found {
		t.Fatalf("a conflicted merge was not logged at WARN: %+v", surfaces.records())
	}
}

func TestMergeNoFFFailsLoudlyWhenNoConflictsRemain(t *testing.T) {
	// Arrange: a nonzero exit that left no conflicted paths is a real failure
	// (an unknown branch, a dirty tree, unrelated histories), not a conflict.
	git, _ := newTestClient(t)
	newFakeGit(t,
		fails(128, "merge: no-such-branch - not something we can merge\n", "merge"),
		ok("", "diff"),
	)

	// Act.
	_, err := git.MergeNoFF(context.Background(), "/repo", "no-such-branch", "merge nothing")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("MergeNoFF error = %v (%T), want a *gitclient.Error", err, err)
	}
	if !strings.Contains(failure.Stderr, "not something we can merge") {
		t.Fatalf("Error.Stderr = %q, want git's own words", failure.Stderr)
	}
}

func TestMergeNoFFPropagatesAFailureReadingTheLandedCommit(t *testing.T) {
	// Arrange: the merge landed but its commit could not be read. Answering
	// Landed{nil} would hand the caller a merge with no commit to revert.
	git, _ := newTestClient(t)
	newFakeGit(t,
		ok("", "merge"),
		fails(128, "fatal: bad object HEAD\n", "show"),
	)

	// Act.
	_, err := git.MergeNoFF(context.Background(), "/repo", "feature", "merge feature")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("MergeNoFF error = %v (%T), want a *gitclient.Error", err, err)
	}
}

// --- ConflictedFiles / AbortMerge ---------------------------------------

func TestConflictedFilesAsksForUnmergedPathsOnly(t *testing.T) {
	// Arrange: `-z` keeps a path with unusual bytes raw instead of quoted, so
	// a resolution agent is handed a filename rather than an escaped string.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if _, err := git.ConflictedFiles(context.Background(), "/repo"); err != nil {
		t.Fatalf("ConflictedFiles: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "diff", "--name-only", "--diff-filter=U", "-z")
}

func TestConflictedFilesIsEmptyWhenGitPrintsNothing(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(""))

	// Act.
	conflicted, err := git.ConflictedFiles(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("ConflictedFiles: %v", err)
	}
	if len(conflicted) != 0 {
		t.Fatalf("ConflictedFiles = %v, want none", conflicted)
	}
}

func TestConflictedFilesSplitsOnTheNULSeparator(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("one.txt\x00two.txt\x00"))

	// Act.
	conflicted, err := git.ConflictedFiles(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("ConflictedFiles: %v", err)
	}
	if strings.Join(conflicted, ",") != "one.txt,two.txt" {
		t.Fatalf("ConflictedFiles = %v, want [one.txt two.txt]", conflicted)
	}
}

func TestAbortMergeAsksGitToAbort(t *testing.T) {
	// Arrange: the caller that decides to give up gets its own verb; the merge
	// never decides that for it.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.AbortMerge(context.Background(), "/repo"); err != nil {
		t.Fatalf("AbortMerge: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "merge", "--abort")
}

func TestAbortMergeFailurePropagates(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: There is no merge to abort\n"))

	// Act.
	err := git.AbortMerge(context.Background(), "/repo")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("AbortMerge error = %v (%T), want a *gitclient.Error", err, err)
	}
}

// --- Commit -------------------------------------------------------------

func TestCommitRecordsTheStagedIndexWithoutAnEditor(t *testing.T) {
	// Arrange: --no-edit keeps git from opening an editor on a merge's own
	// prepared message, and -m supplies the caller's.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("aaaabbbbccccdddd", "rev-parse"), ok(""))

	// Act.
	if _, err := git.Commit(context.Background(), "/repo", "resolve the conflict"); err != nil {
		t.Fatalf("Commit: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "commit", "--no-edit", "-m", "resolve the conflict")
}

func TestCommitReadsTheShaBackWithRevParse(t *testing.T) {
	// Arrange: commit's own chatter is porcelain, so the sha is read back.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("aaaabbbbccccdddd", "rev-parse"), ok(""))

	// Act.
	sha, err := git.Commit(context.Background(), "/repo", "resolve the conflict")
	if err != nil {
		t.Fatalf("Commit: %v", err)
	}

	// Assert.
	fake.assertSubject(1, "rev-parse", "HEAD")
	if sha != "aaaabbbbccccdddd" {
		t.Fatalf("Commit sha = %q, want the rev-parse answer", sha)
	}
}

func TestCommitSelectsTheDirectoryWithDashC(t *testing.T) {
	// Arrange: `-C dir` is the ONLY repository selector this client uses.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("aaaabbbbccccdddd", "rev-parse"), ok(""))

	// Act.
	if _, err := git.Commit(context.Background(), "/repo", "resolve the conflict"); err != nil {
		t.Fatalf("Commit: %v", err)
	}

	// Assert.
	if got := fake.call(0).dashCDir(); got != "/repo" {
		t.Fatalf("-C dir = %q, want /repo", got)
	}
}

func TestCommitScrubsTheInheritedRepositorySelectors(t *testing.T) {
	// Arrange: a leaked GIT_DIR is a real, previously-observed source of
	// bogus work-tree errors.
	t.Setenv("GIT_DIR", "/elsewhere/.git")
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("aaaabbbbccccdddd", "rev-parse"), ok(""))

	// Act.
	if _, err := git.Commit(context.Background(), "/repo", "resolve the conflict"); err != nil {
		t.Fatalf("Commit: %v", err)
	}

	// Assert.
	if got := envValues(fake.call(0).Env, "GIT_DIR"); len(got) != 0 {
		t.Fatalf("GIT_DIR reached the child as %v, want it scrubbed", got)
	}
}

func TestCommitFailureCarriesGitsEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(1, "error: Committing is not possible because you have unmerged files.\n"))

	// Act.
	_, err := git.Commit(context.Background(), "/repo", "resolve the conflict")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("Commit error = %v (%T), want a *gitclient.Error", err, err)
	}
	if !strings.Contains(failure.Stderr, "unmerged files") {
		t.Fatalf("Stderr = %q, want git's own words as evidence", failure.Stderr)
	}
}

func TestCommitFailsWhenTheShaCannotBeRead(t *testing.T) {
	// Arrange: the commit landed but the read-back did not; the caller is told
	// rather than handed an empty sha.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: bad revision 'HEAD'\n", "rev-parse"), ok(""))

	// Act.
	sha, err := git.Commit(context.Background(), "/repo", "resolve the conflict")

	// Assert.
	if err == nil || sha != "" {
		t.Fatalf("Commit = %q, %v; want a failure and no sha", sha, err)
	}
}

// --- RevertMerge --------------------------------------------------------

func TestRevertMergeUsesTheFirstParentAsMainline(t *testing.T) {
	// Arrange: a merge commit cannot be reverted without -m, and mainline 1 is
	// what "undo what the merge brought in, keep the target's own history"
	// means.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if err := git.RevertMerge(context.Background(), "/repo", "aaaabbbb"); err != nil {
		t.Fatalf("RevertMerge: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "revert", "-m", "1", "--no-edit", "aaaabbbb")
}

func TestRevertMergeFailurePropagates(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(1, "error: commit is a merge but no -m option was given\n"))

	// Act.
	err := git.RevertMerge(context.Background(), "/repo", "aaaabbbb")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("RevertMerge error = %v (%T), want a *gitclient.Error", err, err)
	}
}

// --- LandedRange --------------------------------------------------------

func TestLandedRangeWalksTheSecondParentHistory(t *testing.T) {
	// Arrange: reading the range off the COMMIT means the answer survives the
	// source branch being deleted, moved or merged again later.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if _, err := git.LandedRange(context.Background(), "/repo", "aaaabbbb"); err != nil {
		t.Fatalf("LandedRange: %v", err)
	}

	// Assert.
	subject := fake.call(0).subject()
	if subject[len(subject)-1] != "aaaabbbb^1..aaaabbbb^2" {
		t.Fatalf("LandedRange asked for %v, want the second-parent range", subject)
	}
}

func TestLandedRangeIsOldestFirst(t *testing.T) {
	// Arrange: the caller renders it as the story of what landed.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(
		commitLine("111", "feature one", "Test Author", "2026-08-29T10:00:00Z")+"\n"+
			commitLine("222", "feature two", "Test Author", "2026-08-29T10:05:00Z")+"\n"))

	// Act.
	landed, err := git.LandedRange(context.Background(), "/repo", "aaaabbbb")

	// Assert.
	if err != nil {
		t.Fatalf("LandedRange: %v", err)
	}
	if !containsEntry(fake.call(0).subject(), "--reverse") {
		t.Fatalf("LandedRange did not ask for --reverse: %v", fake.call(0).subject())
	}
	if len(landed) != 2 || landed[0].SHA != "111" || landed[1].SHA != "222" {
		t.Fatalf("LandedRange = %+v, want 111 then 222", landed)
	}
}

func TestLandedRangeCarriesEachCommitsSubjectAndAuthor(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(commitLine("111", "feature one", "Test Author", "2026-08-29T10:00:00Z")+"\n"))

	// Act.
	landed, err := git.LandedRange(context.Background(), "/repo", "aaaabbbb")

	// Assert.
	if err != nil {
		t.Fatalf("LandedRange: %v", err)
	}
	if len(landed) != 1 || landed[0].Subject != "feature one" || landed[0].Author != "Test Author" {
		t.Fatalf("LandedRange = %+v, want the scripted subject and author", landed)
	}
}

func TestLandedRangeKeepsASubjectContainingPunctuation(t *testing.T) {
	// Arrange: the field separator is ASCII unit separator precisely so a
	// subject full of colons, pipes and commas cannot fool the split.
	git, _ := newTestClient(t)
	const subject = "fix: a | b, c — everything"
	newFakeGit(t, ok(commitLine("111", subject, "Test Author", "2026-08-29T10:00:00Z")+"\n"))

	// Act.
	landed, err := git.LandedRange(context.Background(), "/repo", "aaaabbbb")

	// Assert.
	if err != nil {
		t.Fatalf("LandedRange: %v", err)
	}
	if len(landed) != 1 || landed[0].Subject != subject {
		t.Fatalf("LandedRange subject = %+v, want %q intact", landed, subject)
	}
}

func TestLandedRangeIsEmptyForAnEmptyRange(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(""))

	// Act.
	landed, err := git.LandedRange(context.Background(), "/repo", "aaaabbbb")

	// Assert.
	if err != nil {
		t.Fatalf("LandedRange: %v", err)
	}
	if len(landed) != 0 {
		t.Fatalf("LandedRange = %+v, want none", landed)
	}
}

func TestLandedRangeFailsOnACommitLineMissingAField(t *testing.T) {
	// Arrange: dropping the line would hand the caller a landed range that is
	// quietly short.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("111"+fieldSep+"feature one\n"))

	// Act.
	_, err := git.LandedRange(context.Background(), "/repo", "aaaabbbb")

	// Assert.
	if err == nil {
		t.Fatalf("LandedRange = nil error for an unreadable commit line, want a loud failure")
	}
}

func TestLandedRangeFailsOnAnUnreadableAuthorDate(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(commitLine("111", "feature one", "Test Author", "not-a-date")+"\n"))

	// Act.
	_, err := git.LandedRange(context.Background(), "/repo", "aaaabbbb")

	// Assert.
	if err == nil {
		t.Fatalf("LandedRange = nil error for an unreadable author date, want a loud failure")
	}
}

func TestLandedRangeParseFailureIsLoggedAtError(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("111"+fieldSep+"feature one\n"))

	// Act.
	_, _ = git.LandedRange(context.Background(), "/repo", "aaaabbbb")

	// Assert.
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.landed_range"); !found {
		t.Fatalf("an unparseable commit line must be logged at ERROR")
	}
}

// --- ChangedPaths -------------------------------------------------------

func TestChangedPathsPassesTheRangeSpecThrough(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if _, err := git.ChangedPaths(context.Background(), "/repo", "base..HEAD"); err != nil {
		t.Fatalf("ChangedPaths: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "diff", "--name-only", "-z", "base..HEAD")
}

func TestChangedPathsListsWhatTheRangeTouched(t *testing.T) {
	// Arrange: the rollout controller classifies subsystems from these paths.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("daemon/one.go\x00webapp/two.ts\x00"))

	// Act.
	paths, err := git.ChangedPaths(context.Background(), "/repo", "base..HEAD")

	// Assert.
	if err != nil {
		t.Fatalf("ChangedPaths: %v", err)
	}
	if strings.Join(paths, ",") != "daemon/one.go,webapp/two.ts" {
		t.Fatalf("ChangedPaths = %v, want [daemon/one.go webapp/two.ts]", paths)
	}
}

func TestChangedPathsIsEmptyForAnUnchangedRange(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(""))

	// Act.
	paths, err := git.ChangedPaths(context.Background(), "/repo", "head..head")

	// Assert.
	if err != nil {
		t.Fatalf("ChangedPaths: %v", err)
	}
	if len(paths) != 0 {
		t.Fatalf("ChangedPaths = %v, want none", paths)
	}
}

func TestChangedPathsKeepsAPathWithUnusualBytesUnquoted(t *testing.T) {
	// Arrange: without `-z` git hands back a quoted, escaped path that no
	// caller could open.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("naïve name.txt\x00"))

	// Act.
	paths, err := git.ChangedPaths(context.Background(), "/repo", "base..HEAD")

	// Assert.
	if err != nil {
		t.Fatalf("ChangedPaths: %v", err)
	}
	if len(paths) != 1 || paths[0] != "naïve name.txt" {
		t.Fatalf("ChangedPaths = %q, want the raw path %q", paths, "naïve name.txt")
	}
}

// --- IsClean / CurrentBranch -------------------------------------------

func TestIsCleanAsksForThePorcelainStatusWithoutOptionalLocks(t *testing.T) {
	// Arrange: a probe must not refresh and write the index, which would take
	// index.lock against the tree's user and bump the index's mtime.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if _, err := git.IsClean(context.Background(), "/repo"); err != nil {
		t.Fatalf("IsClean: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "--no-optional-locks", "status", "--porcelain")
}

func TestIsCleanIsTrueForAnEmptyStatus(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(""))

	// Act.
	clean, err := git.IsClean(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("IsClean: %v", err)
	}
	if !clean {
		t.Fatalf("IsClean = false for an empty status, want true")
	}
}

func TestIsCleanIsFalseForAModifiedFile(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(" M README.md\n"))

	// Act.
	clean, err := git.IsClean(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("IsClean: %v", err)
	}
	if clean {
		t.Fatalf("IsClean = true for a modified tracked file, want false")
	}
}

func TestIsCleanIsFalseForAnUntrackedFile(t *testing.T) {
	// Arrange: untracked content is content a merge or a teardown would either
	// lose or sweep in, so it counts as unclean.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("?? scratch.txt\n"))

	// Act.
	clean, err := git.IsClean(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("IsClean: %v", err)
	}
	if clean {
		t.Fatalf("IsClean = true with an untracked file present, want false")
	}
}

func TestCurrentBranchAnswersTheCheckedOutBranch(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("trunk\n"))

	// Act.
	branch, err := git.CurrentBranch(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}
	if branch != "trunk" {
		t.Fatalf("CurrentBranch = %q, want %q", branch, "trunk")
	}
}

func TestCurrentBranchIsEmptyForADetachedHead(t *testing.T) {
	// Arrange: a detached HEAD is a STATE, not a failure. The caller decides
	// whether it can proceed without a branch.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("HEAD\n"))

	// Act.
	branch, err := git.CurrentBranch(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("CurrentBranch on a detached HEAD = %v, want no error", err)
	}
	if branch != "" {
		t.Fatalf("CurrentBranch = %q, want the empty string for a detached HEAD", branch)
	}
}

func TestCurrentBranchFailurePropagates(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: not a git repository\n"))

	// Act.
	_, err := git.CurrentBranch(context.Background(), "/repo")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("CurrentBranch error = %v (%T), want a *gitclient.Error", err, err)
	}
}

// --- RemoveCleanWorktree / PruneWorktrees --------------------------------

func TestRemoveCleanWorktreeNeverForces(t *testing.T) {
	// Arrange: git's own refusal of a dirty tree is the safety this removal
	// rests on, so --force must never reach it.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	fake := newFakeGit(t, gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir}, ok("", "worktree", "prune"))

	// Act.
	if err := git.RemoveCleanWorktree(context.Background(), "/repo", worktreeDir); err != nil {
		t.Fatalf("RemoveCleanWorktree: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "remove", worktreeDir)
	fake.assertSubject(1, "worktree", "prune")
}

func TestRemoveCleanWorktreeDetachesTheDirectoryBeforeGitRuns(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t, gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir}, ok("", "worktree", "prune"))

	// Act.
	if err := git.RemoveCleanWorktree(context.Background(), "/repo", worktreeDir); err != nil {
		t.Fatalf("RemoveCleanWorktree: %v", err)
	}

	// Assert.
	if got := strings.Join(surfaces.dirEvents, ","); got != "detach "+worktreeDir {
		t.Fatalf("log sink directory events = %q, want only the detach", got)
	}
}

func TestRemoveCleanWorktreeRefusalReturnsGitsEvidence(t *testing.T) {
	// Arrange: the tree became dirty after the caller looked.
	git, _ := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t,
		fails(128, "fatal: contains modified or untracked files, use --force to delete it\n", "worktree", "remove"),
		ok("", "worktree", "prune"),
	)

	// Act.
	err := git.RemoveCleanWorktree(context.Background(), "/repo", worktreeDir)

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "modified or untracked") {
		t.Fatalf("RemoveCleanWorktree = %v, want git's refusal as a *gitclient.Error", err)
	}
}

func TestRemoveCleanWorktreeRefusalReattachesTheSurvivingTree(t *testing.T) {
	// Arrange: a tree git kept may still be somebody's workspace, whose sinks
	// must keep their canonical links.
	git, surfaces := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t,
		fails(128, "fatal: contains modified or untracked files\n", "worktree", "remove"),
		ok("", "worktree", "prune"),
	)

	// Act.
	_ = git.RemoveCleanWorktree(context.Background(), "/repo", worktreeDir)

	// Assert.
	if got := strings.Join(surfaces.dirEvents, ","); got != "detach "+worktreeDir+",attach "+worktreeDir {
		t.Fatalf("log sink directory events = %q, want the refused tree detached then re-attached", got)
	}
}

func TestRemoveCleanWorktreeReattachFailureIsReturnedAndLogged(t *testing.T) {
	// Arrange: the removal is refused and the tree's re-attach then fails.
	git, surfaces := newTestClient(t)
	surfaces.attachFailure = errors.New("attach refused")
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t,
		fails(128, "fatal: contains modified or untracked files\n", "worktree", "remove"),
		ok("", "worktree", "prune"),
	)

	// Act.
	err := git.RemoveCleanWorktree(context.Background(), "/repo", worktreeDir)

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(err.Error(), "attach refused") {
		t.Fatalf("RemoveCleanWorktree = %v, want both the refusal and the attach failure", err)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.remove_clean_worktree"); !found {
		t.Fatal("the attach failure was not recorded at ERROR")
	}
}

func TestRemoveCleanWorktreeSuccessReattachesNothing(t *testing.T) {
	// Arrange: a removed tree stays detached, so a late record cannot bring
	// it back.
	git, surfaces := newTestClient(t)
	worktreeDir := existingDir(t, "wt")
	newFakeGit(t, gitFixture{Match: []string{"worktree", "remove"}, RemovePath: worktreeDir}, ok("", "worktree", "prune"))

	// Act.
	if err := git.RemoveCleanWorktree(context.Background(), "/repo", worktreeDir); err != nil {
		t.Fatalf("RemoveCleanWorktree: %v", err)
	}

	// Assert.
	for _, event := range surfaces.dirEvents {
		if strings.HasPrefix(event, "attach ") {
			t.Fatalf("log sink directory events = %v, want no re-attach after a removal", surfaces.dirEvents)
		}
	}
}

func TestPruneWorktreesRunsThePrune(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "worktree", "prune"))

	// Act.
	if err := git.PruneWorktrees(context.Background(), "/repo"); err != nil {
		t.Fatalf("PruneWorktrees: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "prune")
}

func TestPruneWorktreesFailureIsReturnedAndLogged(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(1, "fatal: could not prune\n", "worktree", "prune"))

	// Act.
	err := git.PruneWorktrees(context.Background(), "/repo")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("PruneWorktrees = %v, want a *gitclient.Error", err)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.prune_worktrees"); !found {
		t.Fatal("the prune failure was not recorded at ERROR")
	}
}

// --- ListWorktrees -------------------------------------------------------

func TestListWorktreesAsksForTheNULSeparatedPorcelain(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("worktree /repo\x00HEAD aaaa\x00branch refs/heads/main\x00\x00", "worktree", "list"))

	// Act.
	if _, err := git.ListWorktrees(context.Background(), "/repo"); err != nil {
		t.Fatalf("ListWorktrees: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "worktree", "list", "--porcelain", "-z")
}

func TestListWorktreesReadsEveryAttribute(t *testing.T) {
	cases := []struct {
		name   string
		record string
		want   Worktree
	}{
		{"branch", "worktree /wt\x00HEAD aaaa\x00branch refs/heads/feat/x\x00\x00",
			Worktree{Dir: "/wt", Head: "aaaa", Branch: "feat/x"}},
		{"detached", "worktree /wt\x00HEAD aaaa\x00detached\x00\x00",
			Worktree{Dir: "/wt", Head: "aaaa", Detached: true}},
		{"bare", "worktree /repo.git\x00bare\x00\x00",
			Worktree{Dir: "/repo.git", Bare: true}},
		{"locked with a reason", "worktree /wt\x00HEAD aaaa\x00branch refs/heads/x\x00locked on a usb stick\x00\x00",
			Worktree{Dir: "/wt", Head: "aaaa", Branch: "x", Locked: true, LockedReason: "on a usb stick"}},
		{"locked without a reason", "worktree /wt\x00HEAD aaaa\x00branch refs/heads/x\x00locked\x00\x00",
			Worktree{Dir: "/wt", Head: "aaaa", Branch: "x", Locked: true}},
		{"prunable", "worktree /wt\x00HEAD aaaa\x00branch refs/heads/x\x00prunable gitdir file points to non-existent location\x00\x00",
			Worktree{Dir: "/wt", Head: "aaaa", Branch: "x", Prunable: true, PrunableReason: "gitdir file points to non-existent location"}},
		{"a path with spaces", "worktree /a dir/wt\x00HEAD aaaa\x00branch refs/heads/x\x00\x00",
			Worktree{Dir: "/a dir/wt", Head: "aaaa", Branch: "x"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			git, _ := newTestClient(t)
			newFakeGit(t, ok(tc.record, "worktree", "list"))

			// Act.
			got, err := git.ListWorktrees(context.Background(), "/repo")

			// Assert.
			if err != nil {
				t.Fatalf("ListWorktrees: %v", err)
			}
			if len(got) != 1 || !reflect.DeepEqual(got[0], tc.want) {
				t.Fatalf("ListWorktrees = %+v, want [%+v]", got, tc.want)
			}
		})
	}
}

func TestListWorktreesKeepsGitsOrderMainFirst(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("worktree /repo\x00HEAD aaaa\x00branch refs/heads/main\x00\x00"+
		"worktree /wt\x00HEAD bbbb\x00branch refs/heads/x\x00\x00", "worktree", "list"))

	// Act.
	got, err := git.ListWorktrees(context.Background(), "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("ListWorktrees: %v", err)
	}
	if len(got) != 2 || got[0].Dir != "/repo" || got[1].Dir != "/wt" {
		t.Fatalf("ListWorktrees = %+v, want /repo then /wt", got)
	}
}

func TestListWorktreesRefusesAnUnknownAttribute(t *testing.T) {
	// Arrange: a newer git stating something new about a tree must not be
	// read as a tree with nothing to say.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("worktree /wt\x00HEAD aaaa\x00frozen\x00\x00", "worktree", "list"))

	// Act.
	_, err := git.ListWorktrees(context.Background(), "/repo")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "frozen") {
		t.Fatalf("ListWorktrees = %v, want a refusal naming the unknown attribute", err)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.list_worktrees"); !found {
		t.Fatal("the unreadable listing was not recorded at ERROR")
	}
}

func TestListWorktreesRefusesAnUnterminatedRecord(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("worktree /wt\x00HEAD aaaa", "worktree", "list"))

	// Act.
	_, err := git.ListWorktrees(context.Background(), "/repo")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not terminated") {
		t.Fatalf("ListWorktrees = %v, want a refusal of the truncated record", err)
	}
}

func TestListWorktreesFailurePropagates(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: not a git repository\n", "worktree", "list"))

	// Act.
	_, err := git.ListWorktrees(context.Background(), "/repo")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("ListWorktrees = %v, want a *gitclient.Error", err)
	}
}

// --- AdminDir / CommitterTime / TreeOf -----------------------------------

func TestAdminDirAsksTheWorktreeForItsAbsoluteGitDir(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("/repo/.git/worktrees/wt\n", "rev-parse"))

	// Act.
	got, err := git.AdminDir(context.Background(), "/wt")

	// Assert.
	if err != nil || got != "/repo/.git/worktrees/wt" {
		t.Fatalf("AdminDir = (%q, %v), want the per-worktree git dir", got, err)
	}
	if call := fake.only(); call.dashCDir() != "/wt" || !reflect.DeepEqual(call.subject(), []string{"rev-parse", "--absolute-git-dir"}) {
		t.Fatalf("AdminDir ran %v in %s, want rev-parse --absolute-git-dir in the worktree", call.subject(), call.dashCDir())
	}
}

func TestCommitterTimeReadsTheCommitterDate(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("2026-09-01T10:00:00+02:00\n", "show"))

	// Act.
	got, err := git.CommitterTime(context.Background(), "/repo", "abc")

	// Assert.
	if err != nil || !got.Equal(time.Date(2026, 9, 1, 8, 0, 0, 0, time.UTC)) {
		t.Fatalf("CommitterTime = (%v, %v), want 2026-09-01T08:00Z", got, err)
	}
	fake.assertSubject(0, "show", "--no-patch", "--format=%cI", "abc")
}

func TestCommitterTimeRefusesAnUnreadableDate(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("yesterday\n", "show"))

	// Act.
	_, err := git.CommitterTime(context.Background(), "/repo", "abc")

	// Assert.
	if err == nil {
		t.Fatal("CommitterTime = nil error for an unreadable date, want a refusal")
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.committer_time"); !found {
		t.Fatal("the unreadable date was not recorded at ERROR")
	}
}

func TestTreeOfPeelsTheRefToItsTree(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("tttt\n", "rev-parse"))

	// Act.
	got, err := git.TreeOf(context.Background(), "/repo", "abc")

	// Assert.
	if err != nil || got != "tttt" {
		t.Fatalf("TreeOf = (%q, %v), want tttt", got, err)
	}
	fake.assertSubject(0, "rev-parse", "--verify", "--end-of-options", "abc^{tree}")
}

// --- MergeTree -----------------------------------------------------------

func TestMergeTreeWritesTheTreeWithoutMessages(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("tttt\n", "merge-tree"))

	// Act.
	if _, err := git.MergeTree(context.Background(), "/repo", "base", "other"); err != nil {
		t.Fatalf("MergeTree: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "merge-tree", "--write-tree", "--no-messages", "base", "other")
}

func TestMergeTreeAnswersACleanMergesTree(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("tttt\n", "merge-tree"))

	// Act.
	got, err := git.MergeTree(context.Background(), "/repo", "base", "other")

	// Assert.
	if err != nil || got != (MergeTreeOutcome{Tree: "tttt"}) {
		t.Fatalf("MergeTree = (%+v, %v), want the clean tree", got, err)
	}
}

func TestMergeTreeAnswersAConflictAsAnOutcome(t *testing.T) {
	// Arrange: exit 1 is git's "the merge would conflict", an answer.
	git, surfaces := newTestClient(t)
	newFakeGit(t, gitFixture{Match: []string{"merge-tree"}, Stdout: "cccc\n100644 aaaa 1\tf.txt\n", Exit: 1})

	// Act.
	got, err := git.MergeTree(context.Background(), "/repo", "base", "other")

	// Assert.
	if err != nil || got != (MergeTreeOutcome{Tree: "cccc", Conflicted: true}) {
		t.Fatalf("MergeTree = (%+v, %v), want the conflicted outcome", got, err)
	}
	if record, found := recordFor(surfaces.records(), "error", "daemon.gitclient.merge_tree"); found {
		t.Fatalf("a conflict was recorded at ERROR: %+v", record)
	}
}

func TestMergeTreeFailsOnAnyOtherExit(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: unknown option `write-tree'\n", "merge-tree"))

	// Act.
	_, err := git.MergeTree(context.Background(), "/repo", "base", "other")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("MergeTree = %v, want a *gitclient.Error", err)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.merge_tree"); !found {
		t.Fatal("the failure was not recorded at ERROR")
	}
}

func TestMergeTreeFailsWhenNoTreeIsPrinted(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("", "merge-tree"))

	// Act.
	_, err := git.MergeTree(context.Background(), "/repo", "base", "other")

	// Assert.
	if err == nil {
		t.Fatal("MergeTree = nil error with no tree printed, want a refusal")
	}
}

// --- DeleteBranchAt ------------------------------------------------------

func TestDeleteBranchAtComparesAndDeletes(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "update-ref"))

	// Act.
	if err := git.DeleteBranchAt(context.Background(), "/repo", "feat/x", "aaaa"); err != nil {
		t.Fatalf("DeleteBranchAt: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "update-ref", "-d", "refs/heads/feat/x", "aaaa")
}

func TestDeleteBranchAtMovedBranchIsARefusal(t *testing.T) {
	// Arrange: a commit landed on the branch after the caller judged it.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: cannot lock ref 'refs/heads/feat/x': is at bbbb but expected aaaa\n", "update-ref"))

	// Act.
	err := git.DeleteBranchAt(context.Background(), "/repo", "feat/x", "aaaa")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "expected aaaa") {
		t.Fatalf("DeleteBranchAt = %v, want git's refusal", err)
	}
}
func TestRecordCommitRunsTheCommitThenReadsHeadBack(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("aaaabbbbccccdddd", "rev-parse"), ok(""))

	// Act.
	sha, err := git.(*client).recordCommit(context.Background(), "op", "/repo", "commit", "-m", "x")

	// Assert.
	if err != nil {
		t.Fatalf("recordCommit: %v", err)
	}
	fake.assertSubject(0, "commit", "-m", "x")
	fake.assertSubject(1, "rev-parse", "HEAD")
	if sha != "aaaabbbbccccdddd" {
		t.Fatalf("recordCommit sha = %q, want the rev-parse answer", sha)
	}
}

func TestRecordCommitNeverReadsHeadAfterAFailedCommit(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, fails(1, "nothing to commit\n"))

	// Act.
	_, err := git.(*client).recordCommit(context.Background(), "op", "/repo", "commit", "-m", "x")

	// Assert.
	if err == nil {
		t.Fatalf("recordCommit = nil error, want the commit's failure")
	}
	fake.assertNever("rev-parse")
}

func TestStatusEmptyWithNoPathspecProbesTheWholeTree(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if _, err := git.(*client).statusEmpty(context.Background(), "op", "/repo"); err != nil {
		t.Fatalf("statusEmpty: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "--no-optional-locks", "status", "--porcelain")
}

func TestStatusEmptyIsFalseForAnyListedEntry(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(" M a.go\n"))

	// Act.
	clean, err := git.(*client).statusEmpty(context.Background(), "op", "/repo")

	// Assert.
	if err != nil {
		t.Fatalf("statusEmpty: %v", err)
	}
	if clean {
		t.Fatalf("statusEmpty = true for a listed entry, want false")
	}
}

func TestCommitSharesTheRecordCommitShape(t *testing.T) {
	// Arrange: Commit's argv is recordCommit's — the commit, then the
	// read-back — so a hand-rolled divergent commit path fails here.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("aaaabbbbccccdddd", "rev-parse"), ok(""))

	// Act.
	if _, err := git.Commit(context.Background(), "/repo", "m"); err != nil {
		t.Fatalf("Commit: %v", err)
	}

	// Assert.
	if n := len(fake.calls()); n != 2 {
		t.Fatalf("Commit issued %d gits, want the commit and its read-back", n)
	}
	fake.assertSubject(1, "rev-parse", "HEAD")
}

func TestIsCleanSharesTheStatusEmptyShape(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if _, err := git.IsClean(context.Background(), "/repo"); err != nil {
		t.Fatalf("IsClean: %v", err)
	}

	// Assert: the whole-tree probe, with no pathspec separator.
	fake.assertSubject(0, "--no-optional-locks", "status", "--porcelain")
}

func TestPathCleanProbesOnlyThePath(t *testing.T) {
	// Arrange: the probe is narrowed by a pathspec after `--`, so a path that
	// reads like a flag can never be taken for one.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act.
	if _, err := git.PathClean(context.Background(), "/repo", "/repo/prompts/a.md"); err != nil {
		t.Fatalf("PathClean: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "--no-optional-locks", "status", "--porcelain", "--", "/repo/prompts/a.md")
}

func TestPathCleanIsTrueForAnEmptyStatus(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(""))

	// Act.
	clean, err := git.PathClean(context.Background(), "/repo", "/repo/a.md")

	// Assert.
	if err != nil || !clean {
		t.Fatalf("PathClean = (%v, %v), want (true, nil)", clean, err)
	}
}

func TestPathCleanIsFalseForAModifiedPath(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(" M a.md\n"))

	// Act.
	clean, err := git.PathClean(context.Background(), "/repo", "/repo/a.md")

	// Assert.
	if err != nil || clean {
		t.Fatalf("PathClean = (%v, %v), want (false, nil)", clean, err)
	}
}

func TestPathCleanIsFalseForAnUntrackedPath(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok("?? a.md\n"))

	// Act.
	clean, err := git.PathClean(context.Background(), "/repo", "/repo/a.md")

	// Assert.
	if err != nil || clean {
		t.Fatalf("PathClean = (%v, %v), want (false, nil)", clean, err)
	}
}

func TestPathCleanFailureCarriesGitsEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: not a git repository\n"))

	// Act.
	_, err := git.PathClean(context.Background(), "/repo", "/repo/a.md")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "not a git repository") {
		t.Fatalf("PathClean error = %v, want a *gitclient.Error with git's words", err)
	}
}

func TestCommitPathCommitsOnlyThePath(t *testing.T) {
	// Arrange: `--only` commits the path through a temporary index, so
	// nothing else staged rides along.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("aaaabbbbccccdddd", "rev-parse"), ok(""))

	// Act.
	if _, err := git.CommitPath(context.Background(), "/repo", "/repo/a.md", "update a"); err != nil {
		t.Fatalf("CommitPath: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "commit", "--only", "-m", "update a", "--", "/repo/a.md")
}

func TestCommitPathAnswersTheReadBackSha(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("aaaabbbbccccdddd", "rev-parse"), ok(""))

	// Act.
	sha, err := git.CommitPath(context.Background(), "/repo", "/repo/a.md", "update a")

	// Assert.
	if err != nil {
		t.Fatalf("CommitPath: %v", err)
	}
	fake.assertSubject(1, "rev-parse", "HEAD")
	if sha != "aaaabbbbccccdddd" {
		t.Fatalf("CommitPath sha = %q, want the rev-parse answer", sha)
	}
}

func TestCommitPathFailureCarriesGitsEvidence(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(1, "[merge-queue] REFUSED: master moves only through the agent-repl merge queue.\n"))

	// Act.
	_, err := git.CommitPath(context.Background(), "/repo", "/repo/a.md", "update a")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "REFUSED") {
		t.Fatalf("CommitPath error = %v, want a *gitclient.Error with git's words", err)
	}
}
