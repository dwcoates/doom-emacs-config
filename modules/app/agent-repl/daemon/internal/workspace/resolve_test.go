package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

func TestResolveKeysOnTheId(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := t.TempDir()
	f.workspace("w1", dir)

	// Act.
	got, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1", Dir: dir})

	// Assert.
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if got.ID != "w1" {
		t.Fatalf("Resolve() = %q, want w1", got.ID)
	}
}

func TestResolveRefusesADirThatDisagreesWithTheRegistry(t *testing.T) {
	// Arrange: the client echoes a stale directory.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1", Dir: t.TempDir()})

	// Assert.
	asRefusal(t, err, ArmWorkspaceRefMismatch)
}

func TestResolveAcceptsARefThatEchoesNoDir(t *testing.T) {
	// Arrange: a client holding only the id still names one workspace.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	got, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1"})

	// Assert.
	if err != nil || got.ID != "w1" {
		t.Fatalf("Resolve() = (%q, %v), want w1", got.ID, err)
	}
}

func TestResolveAcceptsAnUnnormalizedButEquivalentDir(t *testing.T) {
	// Arrange: the echoed dir spells the same tree with a "." element.
	f := newFixture(t)
	dir := t.TempDir()
	f.workspace("w1", dir)

	// Act.
	got, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{
		Id: "w1", Dir: filepath.Join(dir, "."),
	})

	// Assert.
	if err != nil || got.ID != "w1" {
		t.Fatalf("Resolve() = (%q, %v), want w1", got.ID, err)
	}
}

func TestResolveRefusesAnUnknownId(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "nope"})

	// Assert.
	refusal := asRefusal(t, err, ArmUnknownWorkspace)
	if !refusal.NotFound {
		t.Fatal("the unknown-workspace refusal is not marked not-found")
	}
}

func TestResolveRefusesARefWithNoId(t *testing.T) {
	// Arrange: a path is never an identity.
	f := newFixture(t)
	dir := t.TempDir()
	f.workspace("w1", dir)

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Dir: dir})

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestResolveRefusesANilRef(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.Resolve(context.Background(), nil)

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestResolveRefusesAWorkspaceTransferringAway(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1"})

	// Assert.
	asRefusal(t, err, ArmTransferringAway)
}

func TestResolveRefusesAWorkspaceNotYetAdopted(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.standing = StandingNotYetAdopted

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1"})

	// Assert.
	asRefusal(t, err, ArmNotYetAdopted)
}

func TestResolveSurfacesAnOwnershipProbeFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.err = errors.New("rollout is not answering")

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1"})

	// Assert.
	if err == nil {
		t.Fatal("Resolve() = nil error, want the probe failure surfaced")
	}
	if _, ok := AsRefusal(err); ok {
		t.Fatalf("Resolve() = %v, want a failure rather than a refusal", err)
	}
}

// ---- The roster drops what is no longer on disk ------------------------
//
// `SPC TAB n' reads its repository straight out of the roster's repository
// sections, so a section is a CREATE TARGET. Nothing ever forgets a repository
// row: a repository whose tree has been deleted stayed in the picker for the
// life of the registry, and when its base name matched a live repository's the
// two drew the SAME label. Choosing that label could not say which one was
// meant, and the create that picked the dead one failed at git with no
// workspace appearing at all.

// goneRepositoriesLog is the logger the filter records through. The fixture's
// own global sink is what every other verb test reads.
func goneRepositoriesLog(t *testing.T) dlog.Logger {
	t.Helper()
	return newFixture(t).log.Global()
}

func TestRosterDropsARepositoryWhoseDirectoryIsGone(t *testing.T) {
	// Arrange.
	gone := filepath.Join(t.TempDir(), "deleted")
	repos := []wsm.Repository{{ID: "repo-gone", Dir: gone, Name: "scratch-repo"}}

	// Act.
	kept, _, _ := withoutGoneRepositories(goneRepositoriesLog(t), repos, nil, nil)

	// Assert.
	if len(kept) != 0 {
		t.Fatalf("the roster published %d repository row(s) for a directory that is gone, want none", len(kept))
	}
}

func TestRosterKeepsARepositoryWhoseDirectoryIsThere(t *testing.T) {
	// Arrange.
	repos := []wsm.Repository{{ID: "repo-live", Dir: t.TempDir(), Name: "scratch-repo"}}

	// Act.
	kept, _, _ := withoutGoneRepositories(goneRepositoriesLog(t), repos, nil, nil)

	// Assert.
	if len(kept) != 1 {
		t.Fatalf("the roster published %d repository row(s) for a directory that is there, want one", len(kept))
	}
}

// TestRosterDropsTheWorkspacesOfAGoneRepository keeps the sidebar's own
// invariant intact: a workspace naming no published repository is an ERROR the
// repository view reports and leaves out of the grouping.
func TestRosterDropsTheWorkspacesOfAGoneRepository(t *testing.T) {
	// Arrange.
	gone := filepath.Join(t.TempDir(), "deleted")
	repos := []wsm.Repository{{ID: "repo-gone", Dir: gone}}
	workspaces := []wsm.Workspace{{ID: "w-gone", Repo: "repo-gone", Dir: gone + "/tree"}}

	// Act.
	_, kept, _ := withoutGoneRepositories(goneRepositoriesLog(t), repos, workspaces, nil)

	// Assert.
	if len(kept) != 0 {
		t.Fatalf("the roster published %d workspace row(s) under a repository that is gone, want none", len(kept))
	}
}

// TestRosterKeepsTheWorkspacesOfALiveRepository is the negative arm: the walk
// drops nothing it was not asked to.
func TestRosterKeepsTheWorkspacesOfALiveRepository(t *testing.T) {
	// Arrange: one live repository beside one that is gone.
	live := t.TempDir()
	gone := filepath.Join(t.TempDir(), "deleted")
	repos := []wsm.Repository{{ID: "repo-live", Dir: live}, {ID: "repo-gone", Dir: gone}}
	workspaces := []wsm.Workspace{
		{ID: "w-live", Repo: "repo-live", Dir: live},
		{ID: "w-gone", Repo: "repo-gone", Dir: gone},
	}

	// Act.
	_, kept, _ := withoutGoneRepositories(goneRepositoriesLog(t), repos, workspaces, nil)

	// Assert.
	if len(kept) != 1 || kept[0].ID != "w-live" {
		t.Fatalf("the roster published %v, want only the live repository's workspace", kept)
	}
}

// TestRosterDropsTheSessionRecordsOfAGoneRepository keeps the published
// registry self-consistent: a session record naming a workspace no row carries
// resolves nothing.
func TestRosterDropsTheSessionRecordsOfAGoneRepository(t *testing.T) {
	// Arrange.
	gone := filepath.Join(t.TempDir(), "deleted")
	repos := []wsm.Repository{{ID: "repo-gone", Dir: gone}}
	workspaces := []wsm.Workspace{{ID: "w-gone", Repo: "repo-gone", Dir: gone}}
	sessions := []wsm.Session{{Workspace: "w-gone"}}

	// Act.
	_, _, kept := withoutGoneRepositories(goneRepositoriesLog(t), repos, workspaces, sessions)

	// Assert.
	if len(kept) != 0 {
		t.Fatalf("the roster published %d session record(s) under a repository that is gone, want none", len(kept))
	}
}

// TestRosterKeepsARepositoryTheFilesystemCouldNotAnswerFor holds the
// discipline Open and the boot reconciliation already apply: "could not tell"
// is not "gone", and dropping a whole tree on a transient error would hide it.
func TestRosterKeepsARepositoryWhoseDirectoryIsAFile(t *testing.T) {
	// Arrange: a path that exists and is not a directory still EXISTS, so the
	// stat says nothing about the repository being gone.
	path := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(path, []byte("x"), 0o644); err != nil {
		t.Fatalf("write the placeholder: %v", err)
	}
	repos := []wsm.Repository{{ID: "repo-odd", Dir: path}}

	// Act.
	kept, _, _ := withoutGoneRepositories(goneRepositoriesLog(t), repos, nil, nil)

	// Assert.
	if len(kept) != 1 {
		t.Fatalf("the roster dropped a repository whose path exists; only a not-exist answer is 'gone'")
	}
}
