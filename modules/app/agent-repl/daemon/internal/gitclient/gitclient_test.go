package gitclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

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
	// Arrange: a repository whose local origin/HEAD records `trunk`, while a
	// `main` branch also exists to prove origin/HEAD outranks the guesses.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "trunk")
	gitAt(t, dir, "branch", "main")
	sha := gitAt(t, dir, "rev-parse", "HEAD")
	gitAt(t, dir, "update-ref", "refs/remotes/origin/trunk", sha)
	gitAt(t, dir, "symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/trunk")

	// Act.
	branch, err := git.DefaultBranch(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}
	if branch != "trunk" {
		t.Fatalf("DefaultBranch = %q, want %q", branch, "trunk")
	}
}

func TestDefaultBranchUsesConfiguredInitDefaultBranch(t *testing.T) {
	// Arrange: no origin/HEAD, an init.defaultBranch that exists locally, and
	// a `main` that also exists — the configured name must win over `main`.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "development")
	gitAt(t, dir, "branch", "main")
	gitAt(t, dir, "config", "init.defaultBranch", "development")

	// Act.
	branch, err := git.DefaultBranch(context.Background(), dir)

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
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "config", "init.defaultBranch", "never-created")

	// Act.
	branch, err := git.DefaultBranch(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}
	if branch != "main" {
		t.Fatalf("DefaultBranch = %q, want %q: a configured name that does not exist is not an answer", branch, "main")
	}
}

func TestDefaultBranchFallsBackToMain(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	branch, err := git.DefaultBranch(context.Background(), dir)

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
	dir := seedRepo(t, "master")

	// Act.
	branch, err := git.DefaultBranch(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("DefaultBranch: %v", err)
	}
	if branch != "master" {
		t.Fatalf("DefaultBranch = %q, want %q", branch, "master")
	}
}

func TestDefaultBranchFailsWhenNothingMatches(t *testing.T) {
	// Arrange: neither origin/HEAD, nor a configured name, nor main, nor
	// master. The daemon must never invent one.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "some-other-name")

	// Act.
	branch, err := git.DefaultBranch(context.Background(), dir)

	// Assert.
	if err == nil {
		t.Fatalf("DefaultBranch = %q, want a loud failure", branch)
	}
}

func TestDefaultBranchIgnoresATagNamedLikeABranch(t *testing.T) {
	// Arrange: `main` exists only as a TAG, so it is not a default branch.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "some-other-name")
	gitAt(t, dir, "tag", "main")

	// Act.
	_, err := git.DefaultBranch(context.Background(), dir)

	// Assert.
	if err == nil {
		t.Fatalf("DefaultBranch accepted a tag named main; only local branches count")
	}
}

// --- ResolveRef ---------------------------------------------------------

func TestResolveRefAnswersTheFullSha(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	want := gitAt(t, dir, "rev-parse", "HEAD")

	// Act.
	got, err := git.ResolveRef(context.Background(), dir, "main")

	// Assert.
	if err != nil {
		t.Fatalf("ResolveRef: %v", err)
	}
	if got != want {
		t.Fatalf("ResolveRef = %q, want %q", got, want)
	}
}

func TestResolveRefPeelsAnAnnotatedTagToItsCommit(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "tag", "-a", "v1", "-m", "release one")
	want := gitAt(t, dir, "rev-parse", "HEAD")

	// Act.
	got, err := git.ResolveRef(context.Background(), dir, "v1")

	// Assert.
	if err != nil {
		t.Fatalf("ResolveRef: %v", err)
	}
	if got != want {
		t.Fatalf("ResolveRef = %q, want the commit %q rather than the tag object", got, want)
	}
}

// --- Worktrees ----------------------------------------------------------

func TestCreateWorktreeChecksOutANewBranch(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")

	// Act.
	err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir)

	// Assert.
	if err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}
	if got := gitAt(t, worktreeDir, "rev-parse", "--abbrev-ref", "HEAD"); got != "feature/one" {
		t.Fatalf("the worktree is on %q, want %q", got, "feature/one")
	}
}

func TestCreateWorktreeFailsOnAnExistingBranch(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	err := git.CreateWorktree(context.Background(), dir, "main", "main", filepath.Join(t.TempDir(), "wt"))

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("CreateWorktree error = %v (%T), want a *gitclient.Error", err, err)
	}
}

func TestRemoveWorktreeLeavesTheBranch(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")
	if err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}

	// Act.
	err := git.RemoveWorktree(context.Background(), dir, worktreeDir)

	// Assert.
	if err != nil {
		t.Fatalf("RemoveWorktree: %v", err)
	}
	if gitExitAt(t, dir, "show-ref", "--verify", "--quiet", "refs/heads/feature/one") != 0 {
		t.Fatalf("RemoveWorktree deleted the branch; it must leave it")
	}
}

func TestRemoveWorktreeTakesTheDirectoryOffDisk(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")
	if err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}

	// Act.
	if err := git.RemoveWorktree(context.Background(), dir, worktreeDir); err != nil {
		t.Fatalf("RemoveWorktree: %v", err)
	}

	// Assert.
	if _, err := os.Lstat(worktreeDir); !os.IsNotExist(err) {
		t.Fatalf("the worktree directory %s survived removal (stat err = %v)", worktreeDir, err)
	}
}

func TestRemoveWorktreeIsANoOpForADirectoryAlreadyGone(t *testing.T) {
	// Arrange: the exact double-teardown the old merge pipeline logged loud
	// failures for.
	git, surfaces := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")
	if err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}
	if err := git.RemoveWorktree(context.Background(), dir, worktreeDir); err != nil {
		t.Fatalf("the first RemoveWorktree: %v", err)
	}

	// Act.
	err := git.RemoveWorktree(context.Background(), dir, worktreeDir)

	// Assert.
	if err != nil {
		t.Fatalf("the second RemoveWorktree = %v, want a silent no-op", err)
	}
	if record, ok := recordFor(surfaces.records(), "error", "daemon.gitclient.remove_worktree"); ok {
		t.Fatalf("removing an already-removed worktree logged an ERROR: %+v", record)
	}
}

func TestRemoveWorktreePrunesTheRegistration(t *testing.T) {
	// Arrange: a worktree whose directory is deleted behind git's back, so the
	// stale registration is all that is left.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")
	if err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}
	if err := os.RemoveAll(worktreeDir); err != nil {
		t.Fatalf("removing the worktree directory: %v", err)
	}

	// Act.
	if err := git.RemoveWorktree(context.Background(), dir, worktreeDir); err != nil {
		t.Fatalf("RemoveWorktree: %v", err)
	}

	// Assert.
	if listed := gitAt(t, dir, "worktree", "list"); strings.Contains(listed, worktreeDir) {
		t.Fatalf("`worktree list` still registers %s:\n%s", worktreeDir, listed)
	}
}

func TestNukeRemovesTheWorktreeAndTheBranch(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")
	if err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}

	// Act.
	err := git.Nuke(context.Background(), dir, worktreeDir, "feature/one")

	// Assert.
	if err != nil {
		t.Fatalf("Nuke: %v", err)
	}
	if gitExitAt(t, dir, "show-ref", "--verify", "--quiet", "refs/heads/feature/one") == 0 {
		t.Fatalf("Nuke left the branch behind")
	}
}

func TestNukeDeletesAnUnmergedBranchForcibly(t *testing.T) {
	// Arrange: a branch carrying a commit that is on no other branch, which a
	// non-forced delete would refuse.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")
	if err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}
	gitAt(t, worktreeDir, "config", "user.name", "Test Author")
	gitAt(t, worktreeDir, "config", "user.email", "test@example.invalid")
	writeCommit(t, worktreeDir, "only-here.txt", "unmerged\n", "unmerged work")

	// Act.
	err := git.Nuke(context.Background(), dir, worktreeDir, "feature/one")

	// Assert.
	if err != nil {
		t.Fatalf("Nuke: %v", err)
	}
	if gitExitAt(t, dir, "show-ref", "--verify", "--quiet", "refs/heads/feature/one") == 0 {
		t.Fatalf("Nuke left the unmerged branch behind; the delete must be forced")
	}
}

func TestNukeIsANoOpForABranchAlreadyGone(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")
	if err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}
	if err := git.Nuke(context.Background(), dir, worktreeDir, "feature/one"); err != nil {
		t.Fatalf("the first Nuke: %v", err)
	}

	// Act.
	err := git.Nuke(context.Background(), dir, worktreeDir, "feature/one")

	// Assert.
	if err != nil {
		t.Fatalf("the second Nuke = %v, want a silent no-op: the postcondition already holds", err)
	}
}

// --- Repository identity ------------------------------------------------

func TestCommonDirIsAbsolute(t *testing.T) {
	// Arrange: `rev-parse --git-common-dir` answers `.git` relatively for a
	// plain checkout, which is useless as an identity.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	common, err := git.CommonDir(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("CommonDir: %v", err)
	}
	if !filepath.IsAbs(common) {
		t.Fatalf("CommonDir = %q, want an absolute path", common)
	}
}

func TestCommonDirIsSymlinkCanonical(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	common, err := git.CommonDir(context.Background(), dir)

	// Assert: canonical means passing it through EvalSymlinks changes nothing.
	if err != nil {
		t.Fatalf("CommonDir: %v", err)
	}
	resolved, err := filepath.EvalSymlinks(common)
	if err != nil {
		t.Fatalf("EvalSymlinks(%q): %v", common, err)
	}
	if resolved != common {
		t.Fatalf("CommonDir = %q, want the canonicalized %q", common, resolved)
	}
}

func TestSameRepoHoldsAcrossAWorktree(t *testing.T) {
	// Arrange: a worktree is the same repository as the checkout it came from,
	// which is the answer the merge-method split needs.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	worktreeDir := filepath.Join(t.TempDir(), "wt")
	if err := git.CreateWorktree(context.Background(), dir, "feature/one", "main", worktreeDir); err != nil {
		t.Fatalf("CreateWorktree: %v", err)
	}

	// Act.
	same, err := git.SameRepo(context.Background(), dir, worktreeDir)

	// Assert.
	if err != nil {
		t.Fatalf("SameRepo: %v", err)
	}
	if !same {
		t.Fatalf("SameRepo(checkout, its worktree) = false, want true")
	}
}

func TestSameRepoHoldsThroughASymlinkedPath(t *testing.T) {
	// Arrange: the same repository reached through a symlink. On macOS the
	// /tmp -> /private/tmp symlink makes this the ordinary case rather than an
	// exotic one.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	link := filepath.Join(t.TempDir(), "linked-repo")
	if err := os.Symlink(dir, link); err != nil {
		t.Fatalf("symlink %s -> %s: %v", link, dir, err)
	}

	// Act.
	same, err := git.SameRepo(context.Background(), dir, link)

	// Assert.
	if err != nil {
		t.Fatalf("SameRepo: %v", err)
	}
	if !same {
		t.Fatalf("SameRepo(dir, a symlink to dir) = false, want true")
	}
}

func TestSameRepoIsFalseForDistinctRepositories(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	first := seedRepo(t, "main")
	second := seedRepo(t, "main")

	// Act.
	same, err := git.SameRepo(context.Background(), first, second)

	// Assert.
	if err != nil {
		t.Fatalf("SameRepo: %v", err)
	}
	if same {
		t.Fatalf("SameRepo(two separate repositories) = true, want false")
	}
}

// --- MergeNoFF ----------------------------------------------------------

func TestMergeNoFFLandsAMergeCommitWithTwoParents(t *testing.T) {
	// Arrange: a source branch that could fast-forward, so only --no-ff can
	// produce a merge commit.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "checkout", "--quiet", "-b", "feature")
	writeCommit(t, dir, "feature.txt", "one\n", "feature one")
	gitAt(t, dir, "checkout", "--quiet", "main")

	// Act.
	outcome, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature")

	// Assert.
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}
	if outcome.Landed == nil {
		t.Fatalf("MergeOutcome = %+v, want a Landed merge commit", outcome)
	}
	parents := strings.Fields(gitAt(t, dir, "rev-list", "--parents", "-n", "1", outcome.Landed.SHA))
	if len(parents) != 3 {
		t.Fatalf("the merge commit has %d parents, want 2 (fields = %v)", len(parents)-1, parents)
	}
}

func TestMergeNoFFLandedCommitCarriesTheSubjectAndAuthor(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "checkout", "--quiet", "-b", "feature")
	writeCommit(t, dir, "feature.txt", "one\n", "feature one")
	gitAt(t, dir, "checkout", "--quiet", "main")

	// Act.
	outcome, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature")

	// Assert.
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}
	if outcome.Landed.Subject != "merge feature" {
		t.Fatalf("Landed.Subject = %q, want %q", outcome.Landed.Subject, "merge feature")
	}
	if outcome.Landed.Author != "Test Author" {
		t.Fatalf("Landed.Author = %q, want %q", outcome.Landed.Author, "Test Author")
	}
	if outcome.Landed.At.IsZero() {
		t.Fatalf("Landed.At is the zero instant; the commit's author time is a fact git always has")
	}
}

func TestMergeNoFFLandedRangeIsTheSourceBranchesCommits(t *testing.T) {
	// Arrange: two commits on the source branch and one on the target, so the
	// second-parent range is a genuinely narrower answer than "everything new".
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "checkout", "--quiet", "-b", "feature")
	first := writeCommit(t, dir, "feature.txt", "one\n", "feature one")
	second := writeCommit(t, dir, "feature.txt", "two\n", "feature two")
	gitAt(t, dir, "checkout", "--quiet", "main")
	writeCommit(t, dir, "target.txt", "target\n", "target work")

	outcome, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature")
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Act.
	landed, err := git.LandedRange(context.Background(), dir, outcome.Landed.SHA)

	// Assert: oldest first, exactly the source branch's own commits.
	if err != nil {
		t.Fatalf("LandedRange: %v", err)
	}
	got := make([]string, 0, len(landed))
	for _, commit := range landed {
		got = append(got, commit.SHA)
	}
	want := []string{first, second}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("LandedRange = %v, want %v (oldest first)", got, want)
	}
}

func TestLandedRangeCarriesEachCommitsSubject(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "checkout", "--quiet", "-b", "feature")
	writeCommit(t, dir, "feature.txt", "one\n", "feature one")
	gitAt(t, dir, "checkout", "--quiet", "main")
	outcome, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature")
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Act.
	landed, err := git.LandedRange(context.Background(), dir, outcome.Landed.SHA)

	// Assert.
	if err != nil {
		t.Fatalf("LandedRange: %v", err)
	}
	if len(landed) != 1 || landed[0].Subject != "feature one" {
		t.Fatalf("LandedRange = %+v, want one commit subjected %q", landed, "feature one")
	}
}

func TestMergeNoFFConflictAnswersTheConflictedFiles(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir, conflicted := conflictRepo(t, "main")

	// Act.
	outcome, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature")

	// Assert: a conflict is an ANSWER, not an error.
	if err != nil {
		t.Fatalf("MergeNoFF = %v, want a Conflicted answer rather than an error", err)
	}
	if len(outcome.Conflicted) != 1 || outcome.Conflicted[0] != conflicted {
		t.Fatalf("MergeOutcome.Conflicted = %v, want [%s]", outcome.Conflicted, conflicted)
	}
}

func TestMergeNoFFConflictLeavesTheIndexStaged(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir, _ := conflictRepo(t, "main")

	// Act.
	if _, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature"); err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Assert: the unmerged index entries and MERGE_HEAD are the resolution
	// flow's workbench and must survive untouched.
	if unmerged := gitAt(t, dir, "ls-files", "--unmerged"); unmerged == "" {
		t.Fatalf("the index carries no unmerged entries; the conflicted merge must be left staged")
	}
	if gitExitAt(t, dir, "rev-parse", "--verify", "--quiet", "MERGE_HEAD") != 0 {
		t.Fatalf("MERGE_HEAD is gone; MergeNoFF must never abort a conflicted merge")
	}
}

func TestMergeNoFFConflictIsLoggedAtWarning(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	dir, _ := conflictRepo(t, "main")

	// Act.
	if _, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature"); err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Assert.
	if _, ok := recordFor(surfaces.records(), "warn", "daemon.gitclient.merge_no_ff"); !ok {
		t.Fatalf("a conflicted merge was not logged at WARN: %+v", surfaces.records())
	}
}

func TestMergeNoFFFailsLoudlyForAnUnknownBranch(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	_, err := git.MergeNoFF(context.Background(), dir, "no-such-branch", "merge nothing")

	// Assert: a nonzero exit that left no conflicts is a real failure.
	var failure *Error
	if !errors.As(err, &failure) {
		t.Fatalf("MergeNoFF error = %v (%T), want a *gitclient.Error", err, err)
	}
	if strings.TrimSpace(failure.Stderr) == "" {
		t.Fatalf("Error.Stderr is empty; git's own words are the evidence")
	}
}

// --- ConflictedFiles / AbortMerge ---------------------------------------

func TestConflictedFilesIsEmptyOutsideAMerge(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	conflicted, err := git.ConflictedFiles(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("ConflictedFiles: %v", err)
	}
	if len(conflicted) != 0 {
		t.Fatalf("ConflictedFiles = %v, want none", conflicted)
	}
}

func TestAbortMergeRestoresACleanTree(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir, _ := conflictRepo(t, "main")
	if _, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature"); err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Act.
	err := git.AbortMerge(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("AbortMerge: %v", err)
	}
	clean, err := git.IsClean(context.Background(), dir)
	if err != nil {
		t.Fatalf("IsClean: %v", err)
	}
	if !clean {
		t.Fatalf("IsClean = false after AbortMerge, want a restored clean tree")
	}
}

// --- RevertMerge --------------------------------------------------------

func TestRevertMergeUndoesTheMergesContent(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "checkout", "--quiet", "-b", "feature")
	writeCommit(t, dir, "feature.txt", "one\n", "feature one")
	gitAt(t, dir, "checkout", "--quiet", "main")
	outcome, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature")
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}

	// Act.
	err = git.RevertMerge(context.Background(), dir, outcome.Landed.SHA)

	// Assert.
	if err != nil {
		t.Fatalf("RevertMerge: %v", err)
	}
	if _, statErr := os.Lstat(filepath.Join(dir, "feature.txt")); !os.IsNotExist(statErr) {
		t.Fatalf("feature.txt survived the revert (stat err = %v)", statErr)
	}
}

func TestRevertMergeAddsExactlyOneCommit(t *testing.T) {
	// Arrange: the rollback is ONE commit, because the landing was one.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "checkout", "--quiet", "-b", "feature")
	writeCommit(t, dir, "feature.txt", "one\n", "feature one")
	writeCommit(t, dir, "feature.txt", "two\n", "feature two")
	gitAt(t, dir, "checkout", "--quiet", "main")
	outcome, err := git.MergeNoFF(context.Background(), dir, "feature", "merge feature")
	if err != nil {
		t.Fatalf("MergeNoFF: %v", err)
	}
	before := gitAt(t, dir, "rev-list", "--count", "HEAD")

	// Act.
	if err := git.RevertMerge(context.Background(), dir, outcome.Landed.SHA); err != nil {
		t.Fatalf("RevertMerge: %v", err)
	}

	// Assert.
	after := gitAt(t, dir, "rev-list", "--count", "HEAD")
	if before == after {
		t.Fatalf("the commit count did not change (%s); the revert must add one commit", after)
	}
	if gitAt(t, dir, "rev-list", "--count", outcome.Landed.SHA+"..HEAD") != "1" {
		t.Fatalf("the revert added more than one commit on top of the merge")
	}
}

// --- ChangedPaths -------------------------------------------------------

func TestChangedPathsListsWhatARangeTouched(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	base := gitAt(t, dir, "rev-parse", "HEAD")
	writeCommit(t, dir, "daemon/one.go", "package one\n", "daemon change")
	writeCommit(t, dir, "webapp/two.ts", "export {}\n", "webapp change")

	// Act.
	paths, err := git.ChangedPaths(context.Background(), dir, base+"..HEAD")

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
	dir := seedRepo(t, "main")
	head := gitAt(t, dir, "rev-parse", "HEAD")

	// Act.
	paths, err := git.ChangedPaths(context.Background(), dir, head+".."+head)

	// Assert.
	if err != nil {
		t.Fatalf("ChangedPaths: %v", err)
	}
	if len(paths) != 0 {
		t.Fatalf("ChangedPaths = %v, want none", paths)
	}
}

func TestChangedPathsKeepsAPathWithUnusualBytesUnquoted(t *testing.T) {
	// Arrange: without `-z` git would hand back a quoted, escaped path that no
	// caller could open.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	base := gitAt(t, dir, "rev-parse", "HEAD")
	writeCommit(t, dir, "naïve name.txt", "content\n", "unusual path")

	// Act.
	paths, err := git.ChangedPaths(context.Background(), dir, base+"..HEAD")

	// Assert.
	if err != nil {
		t.Fatalf("ChangedPaths: %v", err)
	}
	if len(paths) != 1 || paths[0] != "naïve name.txt" {
		t.Fatalf("ChangedPaths = %q, want the raw path %q", paths, "naïve name.txt")
	}
}

// --- IsClean / CurrentBranch -------------------------------------------

func TestIsCleanIsTrueForAnUntouchedCheckout(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")

	// Act.
	clean, err := git.IsClean(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("IsClean: %v", err)
	}
	if !clean {
		t.Fatalf("IsClean = false, want true")
	}
}

func TestIsCleanIsFalseForAModifiedFile(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	if err := os.WriteFile(filepath.Join(dir, "README.md"), []byte("edited\n"), 0o644); err != nil {
		t.Fatalf("writing README.md: %v", err)
	}

	// Act.
	clean, err := git.IsClean(context.Background(), dir)

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
	dir := seedRepo(t, "main")
	if err := os.WriteFile(filepath.Join(dir, "scratch.txt"), []byte("scratch\n"), 0o644); err != nil {
		t.Fatalf("writing scratch.txt: %v", err)
	}

	// Act.
	clean, err := git.IsClean(context.Background(), dir)

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
	dir := seedRepo(t, "trunk")

	// Act.
	branch, err := git.CurrentBranch(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("CurrentBranch: %v", err)
	}
	if branch != "trunk" {
		t.Fatalf("CurrentBranch = %q, want %q", branch, "trunk")
	}
}

func TestCurrentBranchIsEmptyForADetachedHead(t *testing.T) {
	// Arrange: a detached HEAD is a STATE, not a failure.
	git, _ := newTestClient(t)
	dir := seedRepo(t, "main")
	gitAt(t, dir, "checkout", "--quiet", "--detach")

	// Act.
	branch, err := git.CurrentBranch(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("CurrentBranch on a detached HEAD = %v, want no error", err)
	}
	if branch != "" {
		t.Fatalf("CurrentBranch = %q, want the empty string for a detached HEAD", branch)
	}
}
