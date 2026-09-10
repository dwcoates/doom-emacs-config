package merge

import (
	"path/filepath"
	"testing"
)

// TestRepoLockNameIsStableAndPathSafe covers the lock file's naming: a
// repository key is a path, and a path is neither short enough nor safe enough
// to be a file name.
func TestRepoLockNameIsStableAndPathSafe(t *testing.T) {
	tests := []struct {
		name string
		repo string
		same string
	}{
		{name: "an unclean spelling names the same lock", repo: "/repos/a/.git", same: "/repos/a/b/../.git"},
		{name: "a trailing slash names the same lock", repo: "/repos/a/.git", same: "/repos/a/.git/"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: two spellings of one repository.
			first := repoLockName(tc.repo)

			// Act.
			second := repoLockName(tc.same)

			// Assert.
			if first != second {
				t.Fatalf("%q and %q named %q and %q; two spellings must not grow two queues", tc.repo, tc.same, first, second)
			}
			if filepath.Base(first) != first {
				t.Fatalf("lock name %q is not a bare file name", first)
			}
		})
	}
}

// TestRepoLockNamesDifferPerRepository covers the keying: two repositories must
// not share one queue lock.
func TestRepoLockNamesDifferPerRepository(t *testing.T) {
	// Arrange: two distinct repositories.
	a, b := "/repos/a/.git", "/repos/b/.git"

	// Act.
	nameA, nameB := repoLockName(a), repoLockName(b)

	// Assert.
	if nameA == nameB {
		t.Fatalf("distinct repositories share the lock name %q", nameA)
	}
}

// TestAcquireRepoLockExcludesASecondHolder covers the exclusivity the kernel
// arbitrates: one repository's queue runs in one place.
func TestAcquireRepoLockExcludesASecondHolder(t *testing.T) {
	// Arrange: a held lock on a repository.
	dir := t.TempDir()
	held, ok, err := acquireRepoLock(dir, "/repos/a/.git")
	if err != nil || !ok {
		t.Fatalf("first acquisition failed: ok=%v err=%v", ok, err)
	}
	t.Cleanup(func() { held.Release() })

	// Act: the same repository, a second acquisition.
	second, ok, err := acquireRepoLock(dir, "/repos/a/.git")

	// Assert.
	if err != nil {
		t.Fatalf("a contended lock reported an error rather than an answer: %v", err)
	}
	if ok {
		second.Release()
		t.Fatal("a second holder took a lock that was already held")
	}
}

// TestAcquireRepoLockAdmitsAnotherRepository covers the scope: the lock excludes
// per repository, not globally.
func TestAcquireRepoLockAdmitsAnotherRepository(t *testing.T) {
	// Arrange: one repository's lock held.
	dir := t.TempDir()
	held, _, err := acquireRepoLock(dir, "/repos/a/.git")
	if err != nil {
		t.Fatalf("first acquisition failed: %v", err)
	}
	t.Cleanup(func() { held.Release() })

	// Act: a different repository.
	other, ok, err := acquireRepoLock(dir, "/repos/b/.git")

	// Assert.
	if err != nil || !ok {
		t.Fatalf("a different repository was excluded: ok=%v err=%v", ok, err)
	}
	other.Release()
}

// TestRepoLockReleaseFreesIt covers the handoff: a released lock is takeable
// again, which is what lets the next merge in a repository start.
func TestRepoLockReleaseFreesIt(t *testing.T) {
	// Arrange: a lock taken and released.
	dir := t.TempDir()
	held, _, err := acquireRepoLock(dir, "/repos/a/.git")
	if err != nil {
		t.Fatalf("acquisition failed: %v", err)
	}
	if err := held.Release(); err != nil {
		t.Fatalf("release failed: %v", err)
	}

	// Act.
	again, ok, err := acquireRepoLock(dir, "/repos/a/.git")

	// Assert.
	if err != nil || !ok {
		t.Fatalf("a released lock was not retakeable: ok=%v err=%v", ok, err)
	}
	again.Release()
}

// TestRepoLockReleaseIsIdempotent covers the teardown path, which runs on both
// the ordinary and the failing end of a merge.
func TestRepoLockReleaseIsIdempotent(t *testing.T) {
	// Arrange: an already-released lock.
	dir := t.TempDir()
	held, _, err := acquireRepoLock(dir, "/repos/a/.git")
	if err != nil {
		t.Fatalf("acquisition failed: %v", err)
	}
	if err := held.Release(); err != nil {
		t.Fatalf("first release failed: %v", err)
	}

	// Act.
	err = held.Release()

	// Assert.
	if err != nil {
		t.Fatalf("a second release errored: %v", err)
	}
}
