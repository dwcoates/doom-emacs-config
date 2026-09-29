package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
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
	registered, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, dir))

	// Assert.
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if registered.RepositoryAlreadyKnown {
		t.Fatalf("RegisterRepository reported a repository the registry already held, want a fresh mint")
	}
	if registered.Repository.Dir != dir {
		t.Fatalf("registered dir = %q, want the resolved main worktree %q", registered.Repository.Dir, dir)
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
	registered, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, linked))

	// Assert.
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if registered.Repository.Dir != main {
		t.Fatalf("registered dir = %q, want git's main worktree %q", registered.Repository.Dir, main)
	}
}

func TestRegisterRepositoryReportsARepositoryTheRegistryAlreadyHeld(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	first, err := f.verbs.RegisterRepository(context.Background(), dir)
	if err != nil {
		t.Fatalf("first RegisterRepository: %v", err)
	}

	// Act.
	again, err := f.verbs.RegisterRepository(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("second RegisterRepository: %v", err)
	}
	if !again.RepositoryAlreadyKnown {
		t.Fatalf("RegisterRepository reported a fresh mint for %s, want already known", dir)
	}
	if again.Repository.ID != first.Repository.ID {
		t.Fatalf("RegisterRepository = id %q, want the id %q the first call answered", again.Repository.ID, first.Repository.ID)
	}
}

func TestRegisterRepositoryRefusesAPathOutsideAnyRepository(t *testing.T) {
	// Arrange: the probe answers "not in a repository", which is an ordinary
	// answer rather than a git failure.
	f := newFixture(t)
	f.git.outsideEveryRepository = true
	dir := t.TempDir()

	// Act.
	_, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, dir))

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
	_, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, dir))

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
	_, err := f.verbs.RegisterRepository(context.Background(), filepath.Join(t.TempDir(), "absent.txt"))

	// Assert.
	asRefusal(t, err, ArmUnreadablePath)
}

func TestRegisterRepositoryRefusesAnEmptyPath(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.RegisterRepository(context.Background(), "")

	// Assert.
	asRefusal(t, err, ArmUnreadablePath)
}

func TestRegisterRepositoryRefusesARelativePath(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.RegisterRepository(context.Background(), "some/repo/file.go")

	// Assert.
	asRefusal(t, err, ArmUnreadablePath)
	if !strings.Contains(err.Error(), "not an absolute path") {
		t.Fatalf("RegisterRepository = %v, want the relative path named as the cause", err)
	}
}

func TestRegisterRepositoryRecordsTheRepositoryDefaultBranch(t *testing.T) {
	// Arrange: a repository row with no default branch is a merge target
	// nobody can resolve later, so the verb reads it off git.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	f.git.defaultBranch = "trunk"

	// Act.
	if _, err := f.verbs.RegisterRepository(context.Background(), dir); err != nil {
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
	_, err := f.verbs.RegisterRepository(context.Background(), dir)

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
	_, err := f.verbs.RegisterRepository(context.Background(), dir)

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
	if _, err := f.verbs.RegisterRepository(context.Background(), dir); err != nil {
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

// TestRegisterRepositoryRegistersTheMainWorktreeAsAWorkspace is the owner's
// ruling of 2026-09-14: registering the repository you are standing in must
// leave you with a workspace you can switch to, because `SPC p p' completes
// over live workspaces and a repository-only row is not one.
func TestRegisterRepositoryRegistersTheMainWorktreeAsAWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir

	// Act.
	registered, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, dir))

	// Assert.
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if registered.Workspace.Dir != dir {
		t.Fatalf("registered workspace dir = %q, want the repository's main worktree %q",
			registered.Workspace.Dir, dir)
	}
}

// TestRegisterRepositoryMintsTheWorkspaceThroughTheSameRegistration pins that
// the workspace is not half-made: it goes through `register', so the facts
// RegisterWorkspace derives from git are on the row this verb wrote too.
func TestRegisterRepositoryMintsTheWorkspaceThroughTheSameRegistration(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	f.git.currentBranch = "trunk"

	// Act.
	if _, err := f.verbs.RegisterRepository(context.Background(), dir); err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}

	// Assert.
	if len(f.db.registered) != 1 {
		t.Fatalf("the verb registered %d workspaces, want exactly the main worktree", len(f.db.registered))
	}
	if f.db.registered[0].Branch != "trunk" {
		t.Fatalf("registered branch = %q, want the branch git reports", f.db.registered[0].Branch)
	}
}

// TestRegisterRepositoryReportsAFreshlyMintedWorkspace pins the fresh-repo
// case: nothing was known, so neither half is flagged already known.
func TestRegisterRepositoryReportsAFreshlyMintedWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir

	// Act.
	registered, err := f.verbs.RegisterRepository(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if registered.WorkspaceAlreadyKnown {
		t.Fatalf("RegisterRepository reported a workspace the registry already held, want a fresh mint")
	}
}

// TestRegisterRepositoryMintsTheWorkspaceForARepositoryItAlreadyHeld is the
// mixed case the two independent bools exist for: a repository registered
// before this half of the verb landed is already known while its workspace is
// minted right now.
func TestRegisterRepositoryMintsTheWorkspaceForARepositoryItAlreadyHeld(t *testing.T) {
	// Arrange: the repository row is already in the registry, with no workspace
	// under it.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	if _, _, err := f.db.RegisterRepository(context.Background(), dir, "master"); err != nil {
		t.Fatalf("seed the repository: %v", err)
	}

	// Act.
	registered, err := f.verbs.RegisterRepository(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if !registered.RepositoryAlreadyKnown {
		t.Fatalf("RegisterRepository = repository already_known false, want the seeded repository")
	}
	if registered.WorkspaceAlreadyKnown {
		t.Fatalf("RegisterRepository = workspace already_known true, want a workspace minted now")
	}
}

// TestRegisterRepositoryAdoptsAWorkspaceItAlreadyHeld pins the both-known case:
// re-registering reuses the workspace rather than minting a second one.
func TestRegisterRepositoryAdoptsAWorkspaceItAlreadyHeld(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	first, err := f.verbs.RegisterRepository(context.Background(), dir)
	if err != nil {
		t.Fatalf("first RegisterRepository: %v", err)
	}

	// Act.
	again, err := f.verbs.RegisterRepository(context.Background(), dir)

	// Assert.
	if err != nil {
		t.Fatalf("second RegisterRepository: %v", err)
	}
	if !again.WorkspaceAlreadyKnown {
		t.Fatalf("the second RegisterRepository = workspace already_known false, want true")
	}
	if again.Workspace.ID != first.Workspace.ID {
		t.Fatalf("RegisterRepository = workspace %q, want the one the first call registered, %q",
			again.Workspace.ID, first.Workspace.ID)
	}
}

// TestRegisterRepositorySurfacesTheRegistrationsOwnRefusalUnderItsOwnRpc pins
// that the SHARED registration's refusal is answered as RegisterRepository's.
// The body raises it unnamed precisely because it serves two rpcs, and a
// refusal reaching the client as RegisterWorkspaceError would name an rpc the
// caller never made.
func TestRegisterRepositorySurfacesTheRegistrationsOwnRefusalUnderItsOwnRpc(t *testing.T) {
	// Arrange: git resolves a main worktree that is not a worktree on disk.
	f := newFixture(t)
	f.git.mainWorktree = t.TempDir()
	dir := worktreeDir(t)

	// Act.
	_, err := f.verbs.RegisterRepository(context.Background(), repoFile(t, dir))

	// Assert.
	if refusal := asRefusal(t, err, ArmNotAWorktree); refusal.Rpc != "RegisterRepository" {
		t.Fatalf("refusal rpc = %q, want RegisterRepository", refusal.Rpc)
	}
}
