package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"
)

// repoFile writes a FILE inside a directory and answers its path. It is the
// gesture the command is built around: the user picks a file, never the
// repository root.
func repoFile(t *testing.T, dir string) string {
	t.Helper()
	path := filepath.Join(dir, "README.md")
	if err := os.WriteFile(path, []byte("fixture\n"), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	return path
}

func TestRegisterRepositoryMintsARepositoryFromAFileInsideIt(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir

	// Act.
	record, alreadyKnown, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, dir))

	// Assert.
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if alreadyKnown {
		t.Fatalf("RegisterRepository reported a repository the registry already held, want a fresh mint")
	}
	if record.Dir != dir {
		t.Fatalf("registered dir = %q, want the resolved main worktree %q", record.Dir, dir)
	}
}

func TestRegisterRepositoryResolvesTheMainWorktreeRatherThanThePathsOwnDirectory(t *testing.T) {
	// Arrange: the path is inside a LINKED worktree, whose repository is the
	// main worktree git names -- which is what RepositoryRef.dir means.
	f := newFixture(t)
	linked := worktreeDir(t)
	main := worktreeDir(t)
	f.git.mainWorktree = main

	// Act.
	record, _, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, linked))

	// Assert.
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if record.Dir != main {
		t.Fatalf("registered dir = %q, want git's main worktree %q", record.Dir, main)
	}
}

func TestRegisterRepositoryReportsARepositoryTheRegistryAlreadyHeld(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	first, _, err := f.verbs.RegisterRepository(context.Background(), dir)
	if err != nil {
		t.Fatalf("first RegisterRepository: %v", err)
	}

	// Act.
	again, alreadyKnown, err := f.verbs.RegisterRepository(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("second RegisterRepository: %v", err)
	}
	if !alreadyKnown {
		t.Fatalf("RegisterRepository reported a fresh mint for %s, want already known", dir)
	}
	if again.ID != first.ID {
		t.Fatalf("RegisterRepository = id %q, want the id %q the first call answered", again.ID, first.ID)
	}
}

func TestRegisterRepositoryRefusesAPathOutsideAnyRepository(t *testing.T) {
	// Arrange: the probe answers "not in a repository", which is an ordinary
	// answer rather than a git failure.
	f := newFixture(t)
	f.git.outsideEveryRepository = true
	dir := t.TempDir()

	// Act.
	_, _, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, dir))

	// Assert.
	asRefusal(t, err, ArmNotInARepository)
}

// TestRegisterRepositorySurfacesAProbeGitWouldNotAnswer keeps the two apart: a
// probe that could not be RUN is a failure, while a probe that ran and said
// "not in a repository" is the refusal arm above.
func TestRegisterRepositorySurfacesAProbeGitWouldNotAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.repositoryOfErr = errFake
	dir := t.TempDir()

	// Act.
	_, _, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, dir))

	// Assert.
	if _, refused := AsRefusal(err); refused {
		t.Fatalf("RegisterRepository = refusal %v, want git's own failure surfaced", err)
	}
	if !errors.Is(err, errFake) {
		t.Fatalf("RegisterRepository = %v, want git's own error wrapped", err)
	}
}

func TestRegisterRepositoryRefusesAPathThatIsNotThere(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, _, err := f.verbs.RegisterRepository(context.Background(), filepath.Join(t.TempDir(), "absent.txt"))

	// Assert.
	asRefusal(t, err, ArmUnreadablePath)
}

func TestRegisterRepositoryRefusesAnEmptyPath(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, _, err := f.verbs.RegisterRepository(context.Background(), "")

	// Assert.
	asRefusal(t, err, ArmUnreadablePath)
}

func TestRegisterRepositoryRecordsTheRepositoryDefaultBranch(t *testing.T) {
	// Arrange: a repository row with no default branch is a merge target
	// nobody can resolve later, so the verb reads it off git.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	f.git.defaultBranch = "trunk"

	// Act.
	if _, _, err := f.verbs.RegisterRepository(context.Background(), dir); err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}

	// Assert.
	if len(f.db.registeredRepos) != 1 || f.db.registeredRepos[0].DefaultBranch != "trunk" {
		t.Fatalf("registered repositories = %+v, want one carrying the default branch trunk", f.db.registeredRepos)
	}
}

// TestRegisterRepositorySurfacesADefaultBranchGitWouldNotAnswer pins that a git
// failure on the default branch is an ORDINARY failure and not a refusal arm:
// the path IS in a repository, so answering not_in_a_repository would tell the
// caller something false about their own checkout.
func TestRegisterRepositorySurfacesADefaultBranchGitWouldNotAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	f.git.defaultErr = errFake

	// Act.
	_, _, err := f.verbs.RegisterRepository(context.Background(), dir)

	// Assert.
	if err == nil {
		t.Fatalf("RegisterRepository = no error, want git's own failure surfaced")
	}
	if _, refused := AsRefusal(err); refused {
		t.Fatalf("RegisterRepository = refusal %v, want an ordinary failure", err)
	}
	if !errors.Is(err, errFake) {
		t.Fatalf("RegisterRepository = %v, want git's own error wrapped", err)
	}
}

func TestRegisterRepositorySurfacesARegistryWriteThatFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	f.db.registerRepoErr = errFake

	// Act.
	_, _, err := f.verbs.RegisterRepository(context.Background(), dir)

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("RegisterRepository = %v, want the registry's own failure", err)
	}
}

// TestRegisterRepositoryRepublishesTheRoster pins the reason the verb touches
// the sidebar at all: the new repository draws an EMPTY SECTION, and a
// registration the rail does not show is one the user cannot act on.
func TestRegisterRepositoryRepublishesTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir

	// Act.
	if _, _, err := f.verbs.RegisterRepository(context.Background(), dir); err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("the sidebar was handed %d registries, want exactly 1", len(f.sidebar.registries))
	}
	var found bool
	for _, repo := range f.sidebar.registries[0].Repositories {
		if repo.Dir == dir {
			found = true
		}
	}
	if !found {
		t.Fatalf("the republished roster carries %+v, want the newly registered repository %s",
			f.sidebar.registries[0].Repositories, dir)
	}
}

// TestRegisterRepositoryLeavesTheWorkspaceRegistryAlone pins the verb's whole
// scope: it mints a repository and nothing else. A repository registered on its
// own has NO workspace, and minting one would put a row in the roster the user
// never asked for.
func TestRegisterRepositoryLeavesTheWorkspaceRegistryAlone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir

	// Act.
	if _, _, err := f.verbs.RegisterRepository(context.Background(), dir); err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}

	// Assert.
	if len(f.db.registered) != 0 {
		t.Fatalf("the verb registered %v, want no workspace at all", f.db.registered)
	}
}
