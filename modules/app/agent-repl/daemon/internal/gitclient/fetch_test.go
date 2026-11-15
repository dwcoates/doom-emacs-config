package gitclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/recordlock"
)

func TestPrivateRemoteRefNamesTheDaemonsOwnNamespace(t *testing.T) {
	// Act.
	got := PrivateRemoteRef("origin", "master")

	// Assert.
	if got != "refs/agent-repl/origin/master" {
		t.Fatalf("PrivateRemoteRef = %q, want refs/agent-repl/origin/master", got)
	}
}

// fetchRepo is a repository whose common dir is a real directory, because the
// fetch lock lives in it.
func fetchRepo(t *testing.T) (repoDir, commonDir string) {
	t.Helper()
	repoDir = existingDir(t, "repo")
	commonDir, err := filepath.EvalSymlinks(existingDir(t, "common"))
	if err != nil {
		t.Fatalf("canonicalizing the common dir: %v", err)
	}
	return repoDir, commonDir
}

func TestFetchBranchFetchesOnlyThatBranchIntoItsPrivateRef(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	repoDir, commonDir := fetchRepo(t)
	fake := newFakeGit(t, ok(commonDir+"\n", "rev-parse"), ok("", "fetch"))

	// Act.
	ref, err := git.FetchBranch(context.Background(), repoDir, "origin", "master")

	// Assert: one refspec, forced, into refs/agent-repl/*; no tag, no
	// FETCH_HEAD, no submodule, no maintenance; no whole-remote fetch.
	if err != nil {
		t.Fatalf("FetchBranch: %v", err)
	}
	if ref != "refs/agent-repl/origin/master" {
		t.Fatalf("FetchBranch answered %q, want the private ref", ref)
	}
	fake.assertSubject(1, "fetch", "--no-tags", "--no-write-fetch-head", "--recurse-submodules=no",
		"--no-auto-maintenance", "origin", "+refs/heads/master:refs/agent-repl/origin/master")
}

func TestFetchBranchResolvesTheRepositoryItLocks(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	repoDir, commonDir := fetchRepo(t)
	fake := newFakeGit(t, ok(commonDir+"\n", "rev-parse"), ok("", "fetch"))

	// Act.
	if _, err := git.FetchBranch(context.Background(), repoDir, "origin", "master"); err != nil {
		t.Fatalf("FetchBranch: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "rev-parse", "--git-common-dir")
}

func TestFetchBranchReleasesTheRepositorysFetchLock(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	repoDir, commonDir := fetchRepo(t)
	newFakeGit(t, ok(commonDir+"\n", "rev-parse"), ok("", "fetch"))

	// Act.
	if _, err := git.FetchBranch(context.Background(), repoDir, "origin", "master"); err != nil {
		t.Fatalf("FetchBranch: %v", err)
	}

	// Assert: the lock is free again.
	rec, _, err := recordlock.TryExclusive(filepath.Join(commonDir, fetchLockName))
	if err != nil {
		t.Fatalf("the fetch lock is still held after the fetch: %v", err)
	}
	rec.Release()
}

func TestFetchBranchFailurePropagatesTheGitEvidence(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	repoDir, commonDir := fetchRepo(t)
	newFakeGit(t, ok(commonDir+"\n", "rev-parse"), fails(128, "fatal: could not read from remote\n", "fetch"))

	// Act.
	ref, err := git.FetchBranch(context.Background(), repoDir, "origin", "master")

	// Assert: git's words reach the caller, and the ERROR record names the
	// directory, the refspec and git's stderr.
	var failure *Error
	if ref != "" || !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "could not read") {
		t.Fatalf("FetchBranch = %q, %v; want no ref and git's own evidence", ref, err)
	}
	record, found := recordFor(surfaces.records(), "error", "daemon.gitclient.fetch_branch")
	if !found {
		t.Fatalf("the failure was not logged at ERROR: %+v", surfaces.records())
	}
	args, _ := record.Context["args"].([]string)
	if record.Context["dir"] != repoDir || !strings.Contains(record.Context["stderr"].(string), "could not read") ||
		!containsArg(args, "+refs/heads/master:refs/agent-repl/origin/master") {
		t.Fatalf("the ERROR record = %+v, want the dir, the refspec and git's stderr", record.Context)
	}
}

func TestFetchBranchWhoseRepositoryCannotBeResolvedFetchesNothing(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, fails(128, "fatal: not a git repository\n", "rev-parse"))

	// Act.
	_, err := git.FetchBranch(context.Background(), existingDir(t, "repo"), "origin", "master")

	// Assert.
	if err == nil {
		t.Fatal("FetchBranch succeeded in a directory that is no repository")
	}
	fake.assertNever("fetch")
}

func TestFetchBranchWhoseLockCannotBeTakenFetchesNothingAndSaysWhy(t *testing.T) {
	// Arrange: a common dir the lock file cannot be created in.
	git, surfaces := newTestClient(t)
	repoDir, commonDir := fetchRepo(t)
	if err := os.Chmod(commonDir, 0o555); err != nil {
		t.Fatalf("making %s read-only: %v", commonDir, err)
	}
	t.Cleanup(func() { os.Chmod(commonDir, 0o755) })
	fake := newFakeGit(t, ok(commonDir+"\n", "rev-parse"), ok("", "fetch"))

	// Act.
	ref, err := git.FetchBranch(context.Background(), repoDir, "origin", "master")

	// Assert: the helper's own words name the lock it could not take.
	if ref != "" || err == nil {
		t.Fatalf("FetchBranch = %q, %v; want a failure", ref, err)
	}
	fake.assertNever("fetch")
	record, found := recordFor(surfaces.records(), "error", "daemon.gitclient.fetch_branch")
	stderr, _ := record.Context["stderr"].(string)
	if !found || record.Context["dir"] != repoDir || !strings.Contains(stderr, "agent-repl fetch lock: taking "+filepath.Join(commonDir, fetchLockName)) {
		t.Fatalf("the ERROR record = %+v (found %v), want the dir and the lock the helper could not take", record.Context, found)
	}
}

func TestFetchBranchCalledOffWhileAnotherHoldsTheLockFetchesNothing(t *testing.T) {
	// Arrange: another holder has the repository's fetch lock.
	git, _ := newTestClient(t)
	repoDir, commonDir := fetchRepo(t)
	fake := newFakeGit(t, ok(commonDir+"\n", "rev-parse"), ok("", "fetch"))
	held, _, err := recordlock.TryExclusive(filepath.Join(commonDir, fetchLockName))
	if err != nil {
		t.Fatalf("taking the lock: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() {
		_, err := git.FetchBranch(ctx, repoDir, "origin", "master")
		done <- err
	}()

	// Act: the fetch is called off while the lock is still held.
	cancel()
	err = <-done
	held.Release()

	// Assert: it answers the cancellation and no fetch ran.
	var cancelled *Cancelled
	if !errors.As(err, &cancelled) {
		t.Fatalf("FetchBranch = %v, want the cancellation", err)
	}
	fake.assertNever("fetch")
}

func TestFetchEnvNamesTheLockGitAndTheRefLockWait(t *testing.T) {
	// Act.
	got := fetchEnv("/repo/.git/agent-repl-fetch.lock")

	// Assert.
	want := []string{
		fetchLockEnv + "=/repo/.git/agent-repl-fetch.lock",
		fetchGitEnv + "=" + gitBinary,
		"GIT_CONFIG_COUNT=1",
		"GIT_CONFIG_KEY_0=core.filesRefLockTimeout",
		"GIT_CONFIG_VALUE_0=" + fetchRefLockWait,
	}
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Fatalf("fetchEnv = %v, want %v", got, want)
	}
}

// containsArg reports whether ARGS holds WANT.
func containsArg(args []string, want string) bool {
	for _, a := range args {
		if a == want {
			return true
		}
	}
	return false
}
