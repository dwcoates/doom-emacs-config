package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
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
	kept, _, _, _ := withoutGoneRepositories(goneRepositoriesLog(t), wsm.Arrangement{Repositories: repos}, nil)

	// Assert.
	if len(kept) != 0 {
		t.Fatalf("the roster published %d repository row(s) for a directory that is gone, want none", len(kept))
	}
}

func TestRosterKeepsARepositoryWhoseDirectoryIsThere(t *testing.T) {
	// Arrange.
	repos := []wsm.Repository{{ID: "repo-live", Dir: t.TempDir(), Name: "scratch-repo"}}

	// Act.
	kept, _, _, _ := withoutGoneRepositories(goneRepositoriesLog(t), wsm.Arrangement{Repositories: repos}, nil)

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
	_, kept, _, _ := withoutGoneRepositories(goneRepositoriesLog(t), wsm.Arrangement{Repositories: repos, Workspaces: workspaces}, nil)

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
	_, kept, _, _ := withoutGoneRepositories(goneRepositoriesLog(t), wsm.Arrangement{Repositories: repos, Workspaces: workspaces}, nil)

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
	_, _, _, kept := withoutGoneRepositories(goneRepositoriesLog(t), wsm.Arrangement{Repositories: repos, Workspaces: workspaces}, sessions)

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
	kept, _, _, _ := withoutGoneRepositories(goneRepositoriesLog(t), wsm.Arrangement{Repositories: repos}, nil)

	// Assert.
	if len(kept) != 1 {
		t.Fatalf("the roster dropped a repository whose path exists; only a not-exist answer is 'gone'")
	}
}

// TestRosterDropsTheSubsectionsOfAGoneRepository keeps the resolver's
// arrangement invariant intact: a subsection naming no published repository
// would fail the registry install.
func TestRosterDropsTheSubsectionsOfAGoneRepository(t *testing.T) {
	// Arrange: one live repository beside one that is gone, a subsection each.
	live := t.TempDir()
	gone := filepath.Join(t.TempDir(), "deleted")
	arrangement := wsm.Arrangement{
		Repositories: []wsm.Repository{{ID: "repo-live", Dir: live}, {ID: "repo-gone", Dir: gone}},
		Subsections:  []wsm.Subsection{{ID: "sub-live", Repo: "repo-live", Name: "a", Place: 1}, {ID: "sub-gone", Repo: "repo-gone", Name: "b", Place: 1}},
	}

	// Act.
	_, _, kept, _ := withoutGoneRepositories(goneRepositoriesLog(t), arrangement, nil)

	// Assert.
	if len(kept) != 1 || kept[0].ID != "sub-live" {
		t.Fatalf("the roster published %v, want only the live repository's subsection", kept)
	}
}

func TestSessionRecords(t *testing.T) {
	cases := []struct {
		name       string
		stored     []ids.WorkspaceID
		workspaces []ids.WorkspaceID
		want       []ids.WorkspaceID
	}{
		{name: "the list's order is kept", stored: []ids.WorkspaceID{"a", "b", "c"}, workspaces: []ids.WorkspaceID{"c", "a", "b"}, want: []ids.WorkspaceID{"c", "a", "b"}},
		{name: "a workspace with no session contributes no record", stored: []ids.WorkspaceID{"a"}, workspaces: []ids.WorkspaceID{"a", "b"}, want: []ids.WorkspaceID{"a"}},
		{name: "a session outside the list is left out", stored: []ids.WorkspaceID{"a", "gone"}, workspaces: []ids.WorkspaceID{"a"}, want: []ids.WorkspaceID{"a"}},
		{name: "no workspaces reads no records", stored: []ids.WorkspaceID{"a"}, workspaces: nil, want: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			db := newFakeDB()
			for _, ws := range tc.stored {
				db.sessions[ws] = wsm.Session{Workspace: ws}
			}
			workspaces := make([]wsm.Workspace, 0, len(tc.workspaces))
			for _, ws := range tc.workspaces {
				workspaces = append(workspaces, wsm.Workspace{ID: ws})
			}

			// Act
			got, err := sessionRecords(context.Background(), db, workspaces)

			// Assert
			if err != nil {
				t.Fatalf("sessionRecords: %v", err)
			}
			var gotIDs []ids.WorkspaceID
			for _, s := range got {
				gotIDs = append(gotIDs, s.Workspace)
			}
			if !slices.Equal(gotIDs, tc.want) {
				t.Fatalf("sessionRecords = %v, want %v", gotIDs, tc.want)
			}
		})
	}
}

// TestSessionRecordsReadsEverySessionAtOnce pins the one bulk read: a read per
// workspace queued each behind every other reader on the store's one
// connection, and stretched a republish past its callers' budget.
func TestSessionRecordsReadsEverySessionAtOnce(t *testing.T) {
	// Arrange
	db := newFakeDB()
	var workspaces []wsm.Workspace
	for _, ws := range []ids.WorkspaceID{"a", "b", "c", "d"} {
		db.sessions[ws] = wsm.Session{Workspace: ws}
		workspaces = append(workspaces, wsm.Workspace{ID: ws})
	}

	// Act
	if _, err := sessionRecords(context.Background(), db, workspaces); err != nil {
		t.Fatalf("sessionRecords: %v", err)
	}

	// Assert
	if db.sessionsReads != 1 {
		t.Fatalf("bulk session reads = %d, want exactly one for %d workspaces", db.sessionsReads, len(workspaces))
	}
}

func TestSessionRecordsSurfacesTheReadsFailure(t *testing.T) {
	// Arrange
	db := newFakeDB()
	db.sessionErr = errors.New("disk gone")

	// Act
	got, err := sessionRecords(context.Background(), db, []wsm.Workspace{{ID: "a"}})

	// Assert
	if !errors.Is(err, db.sessionErr) {
		t.Fatalf("sessionRecords err = %v, want it to wrap %v", err, db.sessionErr)
	}
	if got != nil {
		t.Fatalf("sessionRecords = %v, want no records beside the failure", got)
	}
}
