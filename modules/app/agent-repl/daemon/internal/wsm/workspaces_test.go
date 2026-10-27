package wsm

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/sourcescan"
)

func TestNormalizeDirCollapsesEverySpelling(t *testing.T) {
	base := t.TempDir()
	canonical, err := normalizeDir(base)
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}
	if err := os.MkdirAll(filepath.Join(base, "sub"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	tests := []struct {
		name    string
		spelled string
	}{
		{name: "plain", spelled: base},
		{name: "trailing slash", spelled: base + string(filepath.Separator)},
		{name: "dot element", spelled: filepath.Join(base, ".")},
		{name: "parent element", spelled: filepath.Join(base, "sub", "..")},
		{name: "doubled separator", spelled: base + string(filepath.Separator) + string(filepath.Separator)},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got, err := normalizeDir(tc.spelled)

			// Assert
			if err != nil {
				t.Fatalf("normalizeDir(%q): %v", tc.spelled, err)
			}
			if got != canonical {
				t.Fatalf("normalizeDir(%q) = %q, want %q", tc.spelled, got, canonical)
			}
		})
	}
}

func TestNormalizeDirResolvesASymlink(t *testing.T) {
	// Arrange
	base := t.TempDir()
	real := filepath.Join(base, "real")
	if err := os.Mkdir(real, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	link := filepath.Join(base, "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act
	viaLink, err := normalizeDir(link)
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}
	viaReal, err := normalizeDir(real)
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}

	// Assert
	if viaLink != viaReal {
		t.Fatalf("normalizeDir via symlink = %q, want %q", viaLink, viaReal)
	}
}

func TestNormalizeDirResolvesASymlinkedAncestorOfAnAbsentLeaf(t *testing.T) {
	// Arrange
	base := t.TempDir()
	real := filepath.Join(base, "real")
	if err := os.Mkdir(real, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	link := filepath.Join(base, "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}
	resolvedReal, err := normalizeDir(real)
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}

	// Act
	got, err := normalizeDir(filepath.Join(link, "absent"))

	// Assert
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}
	if want := filepath.Join(resolvedReal, "absent"); got != want {
		t.Fatalf("normalizeDir = %q, want %q", got, want)
	}
}

func TestNormalizeDirRefusesAnEmptyPath(t *testing.T) {
	// Arrange / Act
	_, err := normalizeDir("")

	// Assert
	if err == nil {
		t.Fatalf("normalizeDir(\"\") succeeded")
	}
}

func TestRegisterWorkspaceMintsIdentitiesOnFirstSight(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	dir := t.TempDir()

	// Act
	ws, created, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{Name: "one", Branch: "b", ParentBranch: "master", RepoDir: dir})

	// Assert
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	if !created {
		t.Fatalf("created = false on first sight")
	}
	if len(ws.ID) != IDLength || len(ws.Repo) != IDLength {
		t.Fatalf("minted ids = %q/%q, want %d characters each", ws.ID, ws.Repo, IDLength)
	}
}

func TestRegisterWorkspaceRecordsTheParentWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parentDir := t.TempDir()
	parent, _, err := s.RegisterWorkspace(context.Background(), parentDir, RegisterFacts{Branch: "parent", RepoDir: parentDir})
	if err != nil {
		t.Fatalf("RegisterWorkspace(parent): %v", err)
	}
	childDir := t.TempDir()

	// Act
	child, _, err := s.RegisterWorkspace(context.Background(), childDir, RegisterFacts{
		Branch: "child", RepoDir: parentDir, Parent: &parent.ID})
	if err != nil {
		t.Fatalf("RegisterWorkspace(child): %v", err)
	}
	loaded, err := s.Workspace(context.Background(), child.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Assert
	if loaded.Parent == nil || *loaded.Parent != parent.ID {
		t.Fatalf("parent = %v, want %q", loaded.Parent, parent.ID)
	}
}

func TestRegisterWorkspaceLeavesTheParentUnsetWhenNoneIsNamed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	dir := t.TempDir()

	// Act
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{Branch: "b", RepoDir: dir})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	loaded, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Assert
	if loaded.Parent != nil {
		t.Fatalf("parent = %q, want none for a workspace spawned from nothing", *loaded.Parent)
	}
}

func TestRegisterWorkspaceLeavesLastActivityUnset(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	dir := t.TempDir()

	// Act — a freshly registered workspace has taken no turn yet.
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{Branch: "b", RepoDir: dir})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	loaded, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Assert — nil last activity, which the when-column reads as "fall back to
	// created", never a zero instant.
	if loaded.LastActivityAt != nil {
		t.Fatalf("last activity = %v, want none for a never-active workspace", loaded.LastActivityAt)
	}
}

func TestRegisterWorkspaceIsIdempotentAcrossDirSpellings(t *testing.T) {
	base := t.TempDir()
	if err := os.MkdirAll(filepath.Join(base, "sub"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	tests := []struct {
		name    string
		spelled string
	}{
		{name: "trailing slash", spelled: base + string(filepath.Separator)},
		{name: "dot element", spelled: filepath.Join(base, ".")},
		{name: "parent element", spelled: filepath.Join(base, "sub", "..")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			first, _, err := s.RegisterWorkspace(context.Background(), base, RegisterFacts{RepoDir: base})
			if err != nil {
				t.Fatalf("RegisterWorkspace: %v", err)
			}

			// Act
			again, created, err := s.RegisterWorkspace(context.Background(), tc.spelled, RegisterFacts{RepoDir: base})

			// Assert
			if err != nil {
				t.Fatalf("RegisterWorkspace(%q): %v", tc.spelled, err)
			}
			if created {
				t.Fatalf("created = true for %q, want the existing record", tc.spelled)
			}
			if again.ID != first.ID {
				t.Fatalf("id = %q, want %q", again.ID, first.ID)
			}
		})
	}
}

func TestRegisterWorkspaceSharesOneRepoAcrossWorktrees(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := t.TempDir()
	main := registerMain(t, s, repo)

	// Act
	first, _, err := s.RegisterWorkspace(context.Background(), t.TempDir(), RegisterFacts{RepoDir: repo})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	second, _, err := s.RegisterWorkspace(context.Background(), t.TempDir(), RegisterFacts{RepoDir: repo})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Assert
	if first.Repo != main.Repo || second.Repo != main.Repo {
		t.Fatalf("repo ids = %q, %q and %q, want one shared identity", main.Repo, first.Repo, second.Repo)
	}
}

func TestRegisteringTheMainWorktreeMintsTheRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := t.TempDir()

	// Act
	main, created, err := s.RegisterWorkspace(context.Background(), repo, RegisterFacts{RepoDir: repo, DefaultBranch: "main"})

	// Assert
	if err != nil || !created {
		t.Fatalf("RegisterWorkspace(main worktree) = created %v, %v; want a minted workspace", created, err)
	}
	if got := scalar[string](t, s, `SELECT default_branch FROM repositories WHERE id = ?`, main.Repo); got != "main" {
		t.Fatalf("repository default branch = %q, want main", got)
	}
}

func TestALinkedWorktreeOfAnUnknownRepositoryIsRefusedWritingNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, _, err := s.RegisterWorkspace(context.Background(), t.TempDir(), RegisterFacts{RepoDir: t.TempDir()})

	// Assert
	if !errors.Is(err, ErrMainWorktreeUnregistered) {
		t.Fatalf("RegisterWorkspace(linked worktree, unknown repository) = %v, want ErrMainWorktreeUnregistered", err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM repositories`); n != 0 {
		t.Fatalf("repository rows = %d after the refusal, want none", n)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspaces`); n != 0 {
		t.Fatalf("workspace rows = %d after the refusal, want none", n)
	}
}

func TestAWorkspaceBornClosedCarriesItsCloseRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := t.TempDir()
	closure := Closure{At: instant, By: CloseByDaemon}

	// Act
	got, _, err := s.RegisterWorkspace(context.Background(), repo, RegisterFacts{RepoDir: repo, Closure: &closure})

	// Assert
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	stored, err := s.Workspace(context.Background(), got.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	for name, ws := range map[string]Workspace{"answered": got, "stored": stored} {
		if !ws.Closed || ws.Closure == nil || ws.Closure.By != CloseByDaemon || !ws.Closure.At.Equal(instant) {
			t.Fatalf("%s workspace = closed %v closure %+v, want closed by the daemon at %v", name, ws.Closed, ws.Closure, instant)
		}
	}
}

func TestABornClosedRegistrationWithAnUndeclaredCauseIsRefusedWritingNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := t.TempDir()

	// Act
	_, _, err := s.RegisterWorkspace(context.Background(), repo, RegisterFacts{RepoDir: repo, Closure: &Closure{At: instant, By: "nobody"}})

	// Assert
	if err == nil {
		t.Fatal("RegisterWorkspace with an undeclared close cause succeeded")
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspaces`); n != 0 {
		t.Fatalf("workspace rows = %d after the refusal, want none", n)
	}
}

func TestAnAnnouncementOfAnOpenWorkspaceIgnoresABornClosedFact(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := t.TempDir()
	open, _, err := s.RegisterWorkspace(context.Background(), repo, RegisterFacts{RepoDir: repo})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Act
	again, created, err := s.RegisterWorkspace(context.Background(), repo, RegisterFacts{RepoDir: repo, Closure: &Closure{At: instant, By: CloseByDaemon}})

	// Assert
	if err != nil || created || again.ID != open.ID || again.Closed {
		t.Fatalf("RegisterWorkspace again = %+v created %v err %v, want the open row answered unchanged", again, created, err)
	}
}

func TestRegisterWorkspaceDerivesTheNameFromTheDir(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	dir := filepath.Join(t.TempDir(), "derived")
	if err := os.Mkdir(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{RepoDir: dir})

	// Assert
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	if ws.Name != "derived" {
		t.Fatalf("name = %q, want %q", ws.Name, "derived")
	}
}

func TestRegisterWorkspaceRefusesAnEmptyDir(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, _, err := s.RegisterWorkspace(context.Background(), "", RegisterFacts{RepoDir: t.TempDir()})

	// Assert
	if err == nil {
		t.Fatalf("RegisterWorkspace with no dir succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.register_workspace", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestWorkspaceRefusesAnUnknownId(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.Workspace(context.Background(), WorkspaceID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Workspace = %v, want ErrNotFound", err)
	}
}

func TestWorkspaceRoundTripsEveryFact(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	registered := testWorkspace(t, s)

	// Act
	got, err := s.Workspace(context.Background(), registered.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Dir != registered.Dir || got.Name != "sample" || got.Branch != "feature" || got.ParentBranch != "master" {
		t.Fatalf("workspace = %+v, want the registered facts", got)
	}
}

func TestWorkspaceByDirFindsAnyDirSpelling(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	registered := testWorkspace(t, s)

	// Act
	got, err := s.WorkspaceByDir(context.Background(), registered.Dir+string(filepath.Separator))

	// Assert
	if err != nil {
		t.Fatalf("WorkspaceByDir: %v", err)
	}
	if got.ID != registered.ID {
		t.Fatalf("id = %q, want %q", got.ID, registered.ID)
	}
}

func TestWorkspaceByDirRefusesAnUnregisteredDir(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.WorkspaceByDir(context.Background(), t.TempDir())

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("WorkspaceByDir = %v, want ErrNotFound", err)
	}
}

func TestListWorkspacesLoadsEveryRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	testWorkspace(t, s)
	testWorkspace(t, s)

	// Act
	got, err := s.ListWorkspaces(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ListWorkspaces: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("loaded %d workspaces, want 2", len(got))
	}
}

func TestListWorkspacesFailsWholeOnACorruptRow(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	testWorkspace(t, s)
	broken := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET priority = 99 WHERE id = ?`, broken.ID)

	// Act
	got, err := s.ListWorkspaces(context.Background())

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "workspaces" || refusal.Field != "priority" {
		t.Fatalf("ListWorkspaces = %v, want a *DecodeError naming workspaces.priority", err)
	}
	if got != nil {
		t.Fatalf("loaded %d workspaces alongside the refusal, want none", len(got))
	}
	if !loggedOperation(log, "daemon.wsm.list_workspaces", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}

func TestListRepositoriesLoadsEveryRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	testWorkspace(t, s)
	testWorkspace(t, s)

	// Act
	got, err := s.ListRepositories(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ListRepositories: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("loaded %d repositories, want 2", len(got))
	}
}

func TestMarkClosedRecordsTheTeardown(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: time.Now(), By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if !got.Closed {
		t.Fatalf("closed = false after MarkClosed")
	}
}

func TestMarkClosedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.MarkClosed(context.Background(), WorkspaceID("absent"), Closure{At: time.Now(), By: CloseByUser})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("MarkClosed = %v, want ErrNotFound", err)
	}
}

func TestCloseAndReopenOwnership(t *testing.T) {
	claimed := NewInstanceID()
	tests := []struct {
		name      string
		write     func(ctx context.Context, s *store, id WorkspaceID) error
		wantOwner *InstanceID // the serving owner the write leaves
		wantPID   bool        // whether the spawned shim pid survives the write
	}{
		{
			name: "a close releases serving and clears the spawned shim pid",
			write: func(ctx context.Context, s *store, id WorkspaceID) error {
				return s.MarkClosed(ctx, id, Closure{At: time.Now(), By: CloseByUser})
			},
			wantOwner: nil,
			wantPID:   false,
		},
		{
			name:      "a reopen of an open row claims serving for the reopener and keeps the spawned shim pid",
			write:     func(ctx context.Context, s *store, id WorkspaceID) error { return s.Reopen(ctx, id, testServing) },
			wantOwner: ptr(testServing),
			wantPID:   true,
		},
		{
			name: "a reopen of a closed row claims back the serving its close released",
			write: func(ctx context.Context, s *store, id WorkspaceID) error {
				if err := s.MarkClosed(ctx, id, Closure{At: time.Now(), By: CloseByDaemon}); err != nil {
					return err
				}
				return s.Reopen(ctx, id, testServing)
			},
			wantOwner: ptr(testServing),
			wantPID:   false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if err := s.ClaimServing(ctx, ws.ID, claimed); err != nil {
				t.Fatalf("ClaimServing: %v", err)
			}
			pid := 63777
			if err := s.SetSpawnedShimPID(ctx, ws.ID, &pid); err != nil {
				t.Fatalf("SetSpawnedShimPID: %v", err)
			}

			// Act
			if err := tt.write(ctx, s, ws.ID); err != nil {
				t.Fatalf("write: %v", err)
			}

			// Assert
			owner, err := s.Serving(ctx, ws.ID)
			if err != nil {
				t.Fatalf("Serving: %v", err)
			}
			got, err := s.Workspace(ctx, ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if describeOwner(owner) != describeOwner(tt.wantOwner) {
				t.Fatalf("serving owner = %s, want %s", describeOwner(owner), describeOwner(tt.wantOwner))
			}
			if (got.SpawnedShimPID != nil) != tt.wantPID {
				t.Fatalf("spawned shim pid = %v, want present=%v", got.SpawnedShimPID, tt.wantPID)
			}
		})
	}
}

// describeOwner spells a serving owner for a comparison and its failure.
func describeOwner(owner *InstanceID) string {
	if owner == nil {
		return "none"
	}
	return string(*owner)
}

func TestReopenRefusesAnEmptyServingInstance(t *testing.T) {
	// Arrange
	ctx := context.Background()
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.MarkClosed(ctx, ws.ID, Closure{At: firstClose, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	err := s.Reopen(ctx, ws.ID, "")

	// Assert
	if err == nil {
		t.Fatalf("Reopen with no instance succeeded, want a refusal")
	}
	if !loggedOperation(log, "daemon.wsm.reopen", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
	got, err := s.Workspace(ctx, ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if !got.Closed {
		t.Fatalf("the refused re-open opened the row")
	}
}

func TestSetAttentionRecordsTheMarker(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	if err := s.SetAttention(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("SetAttention: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if !got.Attention {
		t.Fatalf("attention = false after SetAttention(true)")
	}
}

func TestSetMergedAtPersistsTheInstant(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	if err := s.SetMergedAt(context.Background(), ws.ID, instant, nil); err != nil {
		t.Fatalf("SetMergedAt: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.MergedAt == nil || !got.MergedAt.Equal(instant) {
		t.Fatalf("merged at = %v, want %v", got.MergedAt, instant)
	}
}

func TestSetPriorityStoresADeclaredArm(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	want := PriorityP1

	// Act
	if err := s.SetPriority(context.Background(), ws.ID, &want); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Priority == nil || *got.Priority != want {
		t.Fatalf("priority = %v, want %v", got.Priority, want)
	}
}

func TestSetPriorityClearsIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	set := PriorityP2
	if err := s.SetPriority(context.Background(), ws.ID, &set); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Act
	if err := s.SetPriority(context.Background(), ws.ID, nil); err != nil {
		t.Fatalf("SetPriority(nil): %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Priority != nil {
		t.Fatalf("priority = %v after clearing, want nil", *got.Priority)
	}
}

func TestSetPriorityRefusesAnUndeclaredArm(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	bogus := Priority(99)

	// Act
	err := s.SetPriority(context.Background(), ws.ID, &bogus)

	// Assert
	if err == nil {
		t.Fatalf("SetPriority with an undeclared arm succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.set_priority", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestANewWorkspaceIsShown(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ws := testWorkspace(t, s)

	// Assert
	if ws.Hidden {
		t.Fatalf("a newly registered workspace is hidden")
	}
}

func TestSetHiddenIsReadBack(t *testing.T) {
	tests := []struct {
		name   string
		writes []bool
		want   bool
	}{
		{name: "hiding a workspace", writes: []bool{true}, want: true},
		{name: "showing a hidden workspace again", writes: []bool{true, false}, want: false},
		{name: "hiding a hidden workspace again", writes: []bool{true, true}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, hidden := range tt.writes {
				if err := s.SetHidden(context.Background(), ws.ID, hidden, time.Unix(1_700_000_000, 0)); err != nil {
					t.Fatalf("SetHidden(%v): %v", hidden, err)
				}
			}

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.Hidden != tt.want {
				t.Fatalf("hidden = %v, want %v", got.Hidden, tt.want)
			}
		})
	}
}

func TestSetHiddenRecordsWhenTheWorkspaceWasHidden(t *testing.T) {
	first, second := time.Unix(1_700_000_000, 0), time.Unix(1_700_000_600, 0)
	tests := []struct {
		name   string
		writes []struct {
			hidden bool
			at     time.Time
		}
		want *time.Time
	}{
		{name: "hiding stamps the instant", writes: []struct {
			hidden bool
			at     time.Time
		}{{true, first}}, want: &first},
		{name: "hiding again re-stamps the latest hide", writes: []struct {
			hidden bool
			at     time.Time
		}{{true, first}, {true, second}}, want: &second},
		{name: "showing clears the instant", writes: []struct {
			hidden bool
			at     time.Time
		}{{true, first}, {false, second}}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, w := range tt.writes {
				if err := s.SetHidden(context.Background(), ws.ID, w.hidden, w.at); err != nil {
					t.Fatalf("SetHidden(%v): %v", w.hidden, err)
				}
			}

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if (got.HiddenAt == nil) != (tt.want == nil) || got.HiddenAt != nil && !got.HiddenAt.Equal(*tt.want) {
				t.Fatalf("hidden at = %v, want %v", got.HiddenAt, tt.want)
			}
		})
	}
}

func TestAHiddenRowWithNoHideInstantIsCorrupt(t *testing.T) {
	// Arrange: a row hidden with no instant, which no write path produces.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.db().Exec(`UPDATE workspaces SET hidden = 1, hidden_at = NULL WHERE id = ?`, ws.ID); err != nil {
		t.Fatalf("corrupting the row: %v", err)
	}

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "hidden_at" {
		t.Fatalf("Workspace = %v, want a hidden_at DecodeError", err)
	}
}

func TestSetHiddenRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.SetHidden(context.Background(), WorkspaceID("no-such-workspace"), true, time.Unix(1_700_000_000, 0))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetHidden on an unknown workspace = %v, want ErrNotFound", err)
	}
	if !loggedOperation(log, "daemon.wsm.set_hidden", "debug") {
		t.Fatalf("the missed write was not recorded under its operation: %v", log.Records())
	}
}

func TestSetCurrentSelectsExactlyOneWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)
	if err := s.SetCurrent(context.Background(), first.ID, instant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Act
	if err := s.SetCurrent(context.Background(), second.ID, instant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	got, err := s.Current(context.Background())
	if err != nil {
		t.Fatalf("Current: %v", err)
	}
	if got == nil || *got != second.ID {
		t.Fatalf("current = %v, want %q", got, second.ID)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE is_current = 1`); n != 1 {
		t.Fatalf("%d workspaces are current, want exactly 1", n)
	}
}

func TestWorkspaceReadsWhetherItIsCurrent(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)
	if err := s.SetCurrent(context.Background(), first.ID, instant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Act
	if err := s.SetCurrent(context.Background(), second.ID, instant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	gotFirst, err := s.Workspace(context.Background(), first.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	gotSecond, err := s.Workspace(context.Background(), second.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if gotFirst.Current || !gotSecond.Current {
		t.Fatalf("Current = %v, %v; want false, true", gotFirst.Current, gotSecond.Current)
	}
}

func TestSetCurrentStampsLastSelected(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	if err := s.SetCurrent(context.Background(), ws.ID, instant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.LastSelectedAt == nil || !got.LastSelectedAt.Equal(instant) {
		t.Fatalf("last selected at = %v, want %v", got.LastSelectedAt, instant)
	}
}

func TestSetCurrentKeepingLastSelectedLeavesTheInstant(t *testing.T) {
	tests := []struct {
		name  string
		prior *time.Time
	}{
		{"a stamped workspace keeps its instant", &instant},
		{"a never-selected workspace stays unstamped", nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			other := testWorkspace(t, s)
			if tt.prior != nil {
				if err := s.SetCurrent(context.Background(), ws.ID, *tt.prior); err != nil {
					t.Fatalf("SetCurrent: %v", err)
				}
			}
			if err := s.SetCurrent(context.Background(), other.ID, instant.Add(time.Hour)); err != nil {
				t.Fatalf("SetCurrent: %v", err)
			}

			// Act
			if err := s.SetCurrentKeepingLastSelected(context.Background(), ws.ID); err != nil {
				t.Fatalf("SetCurrentKeepingLastSelected: %v", err)
			}

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if !got.Current {
				t.Fatal("the walked-onto workspace is not current")
			}
			switch {
			case tt.prior == nil && got.LastSelectedAt != nil:
				t.Fatalf("last selected = %v, want unstamped", got.LastSelectedAt)
			case tt.prior != nil && (got.LastSelectedAt == nil || !got.LastSelectedAt.Equal(*tt.prior)):
				t.Fatalf("last selected = %v, want the standing %v", got.LastSelectedAt, *tt.prior)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE is_current = 1`); n != 1 {
				t.Fatalf("%d workspaces are current, want exactly 1", n)
			}
		})
	}
}

func TestSetCurrentKeepingLastSelectedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	kept := testWorkspace(t, s)
	if err := s.SetCurrent(context.Background(), kept.ID, instant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Act
	err := s.SetCurrentKeepingLastSelected(context.Background(), WorkspaceID("absent"))

	// Assert — the refusal rolls back the clear, and is recorded under its own operation.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetCurrentKeepingLastSelected = %v, want ErrNotFound", err)
	}
	got, err := s.Current(context.Background())
	if err != nil {
		t.Fatalf("Current: %v", err)
	}
	if got == nil || *got != kept.ID {
		t.Fatalf("current = %v after the refusal, want %q", got, kept.ID)
	}
	if !loggedOperation(log, "daemon.wsm.set_current_keeping_last_selected", "debug") {
		t.Fatalf("the missed write was not recorded under its operation: %v", log.Records())
	}
}

func TestSetCurrentRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	kept := testWorkspace(t, s)
	if err := s.SetCurrent(context.Background(), kept.ID, instant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Act
	err := s.SetCurrent(context.Background(), WorkspaceID("absent"), instant)

	// Assert — the refusal rolls back the clear, so the standing selection holds.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetCurrent = %v, want ErrNotFound", err)
	}
	got, err := s.Current(context.Background())
	if err != nil {
		t.Fatalf("Current: %v", err)
	}
	if got == nil || *got != kept.ID {
		t.Fatalf("current = %v after the refusal, want %q", got, kept.ID)
	}
}

func TestCurrentReportsNoneWhenNothingIsSelected(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	testWorkspace(t, s)

	// Act
	got, err := s.Current(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Current: %v", err)
	}
	if got != nil {
		t.Fatalf("current = %q, want none", *got)
	}
}

func TestForgetDeletesEveryDependentRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, CreatedAt: instant}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	for _, query := range []string{
		`SELECT count(*) FROM workspaces WHERE id = ?`,
		`SELECT count(*) FROM creation_jobs WHERE workspace_id = ?`,
		`SELECT count(*) FROM sessions WHERE workspace_id = ?`,
	} {
		if n := scalar[int](t, s, query, ws.ID); n != 0 {
			t.Fatalf("%q left %d rows, want none", query, n)
		}
	}
}

func TestForgetRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.Forget(context.Background(), WorkspaceID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Forget = %v, want ErrNotFound", err)
	}
}

func TestRegisterWorkspaceRecordsTheRepositoryDefaultBranch(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	dir := t.TempDir()

	// Act.
	if _, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{
		Branch: "feature", ParentBranch: "main", RepoDir: dir, DefaultBranch: "main",
	}); err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Assert.
	repos, err := s.ListRepositories(context.Background())
	if err != nil {
		t.Fatalf("ListRepositories: %v", err)
	}
	if len(repos) != 1 || repos[0].DefaultBranch != "main" {
		t.Fatalf("repositories = %+v, want one whose default branch is main", repos)
	}
}

func TestRegisterWorkspaceKeepsAKnownDefaultBranchWhenNoneIsSupplied(t *testing.T) {
	// Arrange: the first announcement records the branch; the second omits it,
	// which means "not looked up", never "no default branch".
	s, _ := testStore(t)
	repoDir := t.TempDir()
	if _, _, err := s.RegisterWorkspace(context.Background(), repoDir, RegisterFacts{
		RepoDir: repoDir, DefaultBranch: "main",
	}); err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	second := filepath.Join(repoDir, "second")
	if err := os.MkdirAll(second, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	if _, _, err := s.RegisterWorkspace(context.Background(), second, RegisterFacts{RepoDir: repoDir}); err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Assert.
	repos, err := s.ListRepositories(context.Background())
	if err != nil {
		t.Fatalf("ListRepositories: %v", err)
	}
	if len(repos) != 1 || repos[0].DefaultBranch != "main" {
		t.Fatalf("repositories = %+v, want the recorded default branch kept", repos)
	}
}

// registerUnder registers one workspace at its own directory but under a NAMED
// repository, which is the arrangement every repository-disposition case needs
// and testWorkspace cannot make: it registers each workspace as its own
// repository.
// registerMain registers repoDir's main worktree, idempotently and born
// closed: the one registration that mints the repository, made first as the
// daemon makes it when a linked worktree is announced.
func registerMain(t *testing.T, s *store, repoDir string) Workspace {
	t.Helper()
	ws, _, err := s.RegisterWorkspace(context.Background(), repoDir, RegisterFacts{
		Branch: "master", ParentBranch: "master", RepoDir: repoDir, DefaultBranch: "master",
		Closure: &Closure{At: instant, By: CloseByDaemon},
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(main worktree %s): %v", repoDir, err)
	}
	return ws
}

// registerUnder registers a linked worktree under repoDir, after its main
// worktree.
func registerUnder(t *testing.T, s *store, repoDir string) Workspace {
	t.Helper()
	registerMain(t, s, repoDir)
	ws, created, err := s.RegisterWorkspace(context.Background(), t.TempDir(), RegisterFacts{
		Name: "sample", Branch: "feature", ParentBranch: "master", RepoDir: repoDir,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	if !created {
		t.Fatalf("RegisterWorkspace reported an existing record for a fresh dir")
	}
	return ws
}

func TestForgetDisposesOfTheRepositoryOnlyWithItsMainWorktree(t *testing.T) {
	tests := []struct {
		name string
		// forgetMain forgets the main worktree's workspace rather than the
		// linked one, after the linked one is gone.
		forgetMain bool
		// wantRepoRows is how many repository rows survive the forget.
		wantRepoRows int
	}{
		{name: "a linked workspace leaves the repository to its main worktree", forgetMain: false, wantRepoRows: 1},
		{name: "the main worktree as the last workspace takes its repository with it", forgetMain: true, wantRepoRows: 0},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repoDir := t.TempDir()
			linked := registerUnder(t, s, repoDir)
			target := linked
			if test.forgetMain {
				if _, err := s.Forget(context.Background(), linked.ID); err != nil {
					t.Fatalf("Forget(linked): %v", err)
				}
				target = registerMain(t, s, repoDir)
			}

			// Act
			report, err := s.Forget(context.Background(), target.ID)

			// Assert
			if err != nil {
				t.Fatalf("Forget: %v", err)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM repositories WHERE id = ?`, linked.Repo); n != test.wantRepoRows {
				t.Fatalf("repository rows = %d, want %d", n, test.wantRepoRows)
			}
			if reported := report.Repository == linked.Repo; reported != test.forgetMain {
				t.Fatalf("report.Repository = %q, want reported = %v", report.Repository, test.forgetMain)
			}
		})
	}
}

func TestForgetRefusesTheMainWorktreeWhileTheRepositoryHasOtherWorkspaces(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := t.TempDir()
	linked := registerUnder(t, s, repoDir)
	main := registerMain(t, s, repoDir)

	// Act
	_, err := s.Forget(context.Background(), main.ID)

	// Assert
	if !errors.Is(err, ErrMainWorktreeInUse) {
		t.Fatalf("Forget(main worktree) = %v, want ErrMainWorktreeInUse", err)
	}
	for _, kept := range []Workspace{main, linked} {
		if _, err := s.Workspace(context.Background(), kept.ID); err != nil {
			t.Fatalf("Workspace(%s) after the refusal: %v, want it kept", kept.Name, err)
		}
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM repositories WHERE id = ?`, main.Repo); n != 1 {
		t.Fatalf("repository rows = %d after the refusal, want 1", n)
	}
}

func TestForgetReportsTheForgottenRepositoryDir(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := t.TempDir()
	ws := registerMain(t, s, repoDir)
	want := scalar[string](t, s, `SELECT dir FROM repositories WHERE id = ?`, ws.Repo)

	// Act
	report, err := s.Forget(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Forget: %v", err)
	}
	if report.RepositoryDir != want {
		t.Fatalf("report.RepositoryDir = %q, want %q", report.RepositoryDir, want)
	}
}

func TestForgetLeavesTheOtherWorkspacesAlone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := t.TempDir()
	ws := registerUnder(t, s, repoDir)
	kept := registerUnder(t, s, repoDir)
	elsewhere := testWorkspace(t, s)

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	for _, survivor := range []Workspace{kept, elsewhere} {
		got, err := s.Workspace(context.Background(), survivor.ID)
		if err != nil {
			t.Fatalf("Workspace(%q): %v", survivor.ID, err)
		}
		if got.Dir != survivor.Dir || got.Repo != survivor.Repo {
			t.Fatalf("workspace %q = %+v, want it unchanged from %+v", survivor.ID, got, survivor)
		}
	}
}

func TestForgetLeavesAnotherRepositoryAlone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	other := testWorkspace(t, s)

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM repositories WHERE id = ?`, other.Repo); n != 1 {
		t.Fatalf("the other workspace's repository rows = %d, want 1", n)
	}
}

func TestForgetDeletesTheRepositorysMergeQueuePauseRow(t *testing.T) {
	// Arrange — the pause row is keyed by the repository's DIR, so no foreign
	// key cascade can reach it when the repository record goes.
	s, _ := testStore(t)
	repoDir := t.TempDir()
	ws := registerMain(t, s, repoDir)
	key := RepoKey(scalar[string](t, s, `SELECT dir FROM repositories WHERE id = ?`, ws.Repo))
	if err := s.SetMergeQueuePaused(context.Background(), key, testPause()); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM merge_queue_repos WHERE repo_key = ?`, string(key)); n != 0 {
		t.Fatalf("merge_queue_repos left %d rows, want none", n)
	}
}

func TestForgetLeavesAReferencedRepositorysPauseRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := t.TempDir()
	ws := registerUnder(t, s, repoDir)
	registerUnder(t, s, repoDir)
	key := RepoKey(scalar[string](t, s, `SELECT dir FROM repositories WHERE id = ?`, ws.Repo))
	if err := s.SetMergeQueuePaused(context.Background(), key, testPause()); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM merge_queue_repos WHERE repo_key = ?`, string(key)); n != 1 {
		t.Fatalf("merge_queue_repos rows = %d, want the referenced repository's row kept", n)
	}
}

func TestForgetWritesNothingWhenTheWorkspaceIsUnknown(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.Forget(context.Background(), WorkspaceID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Forget = %v, want ErrNotFound", err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM repositories WHERE id = ?`, ws.Repo); n != 1 {
		t.Fatalf("a refused Forget removed %d repository rows, want none removed", 1-n)
	}
}

// TestSetSpawnedShimPIDRecordsTheForksPidWithoutASession pins the fact the
// whole starting-shim adoption rests on: the pid is durable for a workspace
// that has NO session row at all, which is exactly the state a registered
// workspace is in when its first shim is forked.
func TestSetSpawnedShimPIDRecordsTheForksPidWithoutASession(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	pid := 4242

	// Act
	if err := s.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
		t.Fatalf("SetSpawnedShimPID: %v", err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.SpawnedShimPID == nil || *got.SpawnedShimPID != pid {
		t.Fatalf("spawned shim pid = %v, want %d", got.SpawnedShimPID, pid)
	}
}

// TestSetSpawnedShimPIDClearsTheRecordedSpawn pins the retraction a stopped
// spawn performs: a pid left behind makes the next boot wait out its whole
// adoption bound for a process that is gone.
func TestSetSpawnedShimPIDClearsTheRecordedSpawn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	pid := 4242
	if err := s.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
		t.Fatalf("SetSpawnedShimPID: %v", err)
	}

	// Act
	if err := s.SetSpawnedShimPID(context.Background(), ws.ID, nil); err != nil {
		t.Fatalf("SetSpawnedShimPID(nil): %v", err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.SpawnedShimPID != nil {
		t.Fatalf("spawned shim pid = %d, want nil after the spawn was stood down", *got.SpawnedShimPID)
	}
}

// TestSetSpawnedShimPIDRefusesANonPositivePid pins the refusal: the value is
// handed to kill(pid, 0), where 0 and negatives address process groups.
func TestSetSpawnedShimPIDRefusesANonPositivePid(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	zero := 0

	// Act
	err := s.SetSpawnedShimPID(context.Background(), ws.ID, &zero)

	// Assert
	if err == nil {
		t.Fatalf("SetSpawnedShimPID accepted a non-positive pid")
	}
}

// TestSetSpawnedShimPIDRefusesAnUnknownWorkspace pins that a write matching no
// row is a refusal rather than a silent success.
func TestSetSpawnedShimPIDRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	pid := 4242

	// Act
	err := s.SetSpawnedShimPID(context.Background(), WorkspaceID("no-such-workspace"), &pid)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetSpawnedShimPID on an unknown workspace = %v, want ErrNotFound", err)
	}
}

// TestWorkspaceRefusesACorruptSpawnedShimPid pins the decode: a non-positive
// pid on disk is a corrupt row, never a silently dropped value.
func TestWorkspaceRefusesACorruptSpawnedShimPid(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET spawned_shim_pid = 0 WHERE id = ?`, ws.ID)

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decodeErr *DecodeError
	if !errors.As(err, &decodeErr) {
		t.Fatalf("Workspace = %v, want a DecodeError for a non-positive recorded pid", err)
	}
}

func TestSetResultStoresTheLastTurnResult(t *testing.T) {
	tests := []struct {
		name string
		want TurnResult
	}{
		{name: "an unread done", want: TurnResult{End: TurnResultDone}},
		{name: "a read interrupted", want: TurnResult{End: TurnResultInterrupted, Read: true}},
		{name: "an unread failed turn", want: TurnResult{End: TurnResultFailed}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			if err := s.SetResult(context.Background(), ws.ID, &tt.want); err != nil {
				t.Fatalf("SetResult: %v", err)
			}

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.Result == nil || *got.Result != tt.want {
				t.Fatalf("result = %v, want %v", got.Result, tt.want)
			}
		})
	}
}

func TestSetResultClearsIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.SetResult(context.Background(), ws.ID, &TurnResult{End: TurnResultDone, Read: true}); err != nil {
		t.Fatalf("SetResult: %v", err)
	}

	// Act
	if err := s.SetResult(context.Background(), ws.ID, nil); err != nil {
		t.Fatalf("SetResult(nil): %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Result != nil {
		t.Fatalf("result = %v, want none", got.Result)
	}
}

func TestANewWorkspaceHasNoResult(t *testing.T) {
	// Arrange / Act
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Result != nil {
		t.Fatalf("result = %v, want none", got.Result)
	}
}

func TestSetResultRefusesAnUndeclaredArm(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetResult(context.Background(), ws.ID, &TurnResult{End: "sideways"})

	// Assert
	if err == nil {
		t.Fatal("SetResult with an undeclared arm succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.set_result", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Result != nil {
		t.Fatalf("result = %v, want nothing stored by a refused write", got.Result)
	}
}

func TestSetResultOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetResult(context.Background(), "no-such", &TurnResult{End: TurnResultDone})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetResult = %v, want ErrNotFound", err)
	}
}

func TestACorruptStoredResultFailsTheDecode(t *testing.T) {
	// Arrange: a row whose stored arm this build does not declare.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.db().ExecContext(context.Background(), `UPDATE workspaces SET result_end = 'sideways' WHERE id = ?`, ws.ID); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "result_end" {
		t.Fatalf("Workspace = %v, want a result_end decode failure", err)
	}
}

// TestScanRepositoryReadsEveryRepositoryColumn covers the one column list
// both repository reads share: a registered repository reads back whole
// through the single read and the list alike.
func TestScanRepositoryReadsEveryRepositoryColumn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	listed, err := s.ListRepositories(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ListRepositories: %v", err)
	}
	if len(listed) != 1 || listed[0].ID != ws.Repo || listed[0].Dir == "" || listed[0].Name == "" {
		t.Fatalf("repositories = %+v, want the workspace's repository read whole", listed)
	}
}

func TestSetRepositoryFoldedIsReadBack(t *testing.T) {
	tests := []struct {
		name   string
		folded bool
	}{{"collapsed", true}, {"expanded", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if err := s.SetRepositoryFolded(context.Background(), ws.Repo, !tt.folded); err != nil {
				t.Fatalf("SetRepositoryFolded: %v", err)
			}

			// Act
			err := s.SetRepositoryFolded(context.Background(), ws.Repo, tt.folded)

			// Assert
			if err != nil {
				t.Fatalf("SetRepositoryFolded: %v", err)
			}
			listed, _ := s.ListRepositories(context.Background())
			if len(listed) != 1 || listed[0].Folded != tt.folded {
				t.Fatalf("repositories = %+v, want folded=%v", listed, tt.folded)
			}
		})
	}
}

func TestANewRepositoryIsExpanded(t *testing.T) {
	// Arrange, Act
	s, _ := testStore(t)
	testWorkspace(t, s)

	// Assert
	listed, _ := s.ListRepositories(context.Background())
	if len(listed) != 1 || listed[0].Folded {
		t.Fatalf("repositories = %+v, want a new repository expanded", listed)
	}
}

func TestSetRepositoryFoldedRefusesAnUnknownRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetRepositoryFolded(context.Background(), RepoID("absent"), true)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetRepositoryFolded = %v, want ErrNotFound", err)
	}
}

// TestTheMigrationAddsTheRepositoriesFoldedColumn pins the layout-17 step.
func TestTheMigrationAddsTheRepositoriesFoldedColumn(t *testing.T) {
	// Arrange — a file the build at layout 16 left.
	path := fixtureAt(t, 16)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-16 database: %v", err)
	}
	defer handle.Close()

	// Assert
	got := scalar[int](t, handle.(*store), `SELECT count(*) FROM pragma_table_info('repositories') WHERE name = 'folded'`)
	if got != 1 {
		t.Fatalf("repositories carries the folded column %d times after the migration, want 1", got)
	}
}

// selectionFoldWorld is a workspace in a subsection of its repository, a child
// of it, and a workspace of another repository, every fold collapsed.
type selectionFoldWorld struct {
	root, child, other Workspace
	sub                Subsection
}

func newSelectionFoldWorld(t *testing.T, s *store) selectionFoldWorld {
	t.Helper()
	ctx := context.Background()
	repo := arrangeRepo(t, "repo")
	root := registerIn(t, s, repo, "root", nil)
	child := registerChild(t, s, repo, "child", root.ID)
	other := registerIn(t, s, arrangeRepo(t, "other"), "other", nil)
	sub := mustSubsection(t, s, root.Repo, "later")
	mustPlace(t, s, root.ID, &sub.ID, AtEnd{})
	for _, repo := range []RepoID{root.Repo, other.Repo} {
		if err := s.SetRepositoryFolded(ctx, repo, true); err != nil {
			t.Fatalf("SetRepositoryFolded: %v", err)
		}
	}
	if err := s.SetSubsectionFolded(ctx, sub.ID, true); err != nil {
		t.Fatalf("SetSubsectionFolded: %v", err)
	}
	return selectionFoldWorld{root: root, child: child, other: other, sub: sub}
}

// repositoryFolded reads whether REPO's section is collapsed.
func repositoryFolded(t *testing.T, s *store, repo RepoID) bool {
	t.Helper()
	return scalar[bool](t, s, `SELECT folded FROM repositories WHERE id = ?`, repo)
}

func TestSelectingAWorkspaceExpandsTheFoldsHoldingIt(t *testing.T) {
	tests := []struct {
		name   string
		pick   func(selectionFoldWorld) Workspace
		act    func(*store, WorkspaceID) error
		wantOp string
	}{
		{
			name: "a selection expands the root's repository and subsection",
			pick: func(w selectionFoldWorld) Workspace { return w.root },
			act: func(s *store, id WorkspaceID) error {
				return s.SetCurrent(context.Background(), id, instant)
			},
			wantOp: "daemon.wsm.set_current",
		},
		{
			name: "a selection expands a child's family subsection",
			pick: func(w selectionFoldWorld) Workspace { return w.child },
			act: func(s *store, id WorkspaceID) error {
				return s.SetCurrent(context.Background(), id, instant)
			},
			wantOp: "daemon.wsm.set_current",
		},
		{
			name: "a recency walk's step expands them too",
			pick: func(w selectionFoldWorld) Workspace { return w.root },
			act: func(s *store, id WorkspaceID) error {
				return s.SetCurrentKeepingLastSelected(context.Background(), id)
			},
			wantOp: "daemon.wsm.set_current_keeping_last_selected",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			w := newSelectionFoldWorld(t, s)
			ws := tt.pick(w)

			// Act
			err := tt.act(s, ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("select: %v", err)
			}
			if repositoryFolded(t, s, ws.Repo) {
				t.Fatal("the selected workspace's repository is still collapsed")
			}
			if subsectionNow(t, s, w.sub.ID).Folded {
				t.Fatal("the selected workspace's subsection is still collapsed")
			}
			if !repositoryFolded(t, s, w.other.Repo) {
				t.Fatal("another repository's collapse was expanded")
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: tt.wantOp,
				Message: "the selection expanded the collapsed folds holding the workspace"})
			if !ok {
				t.Fatalf("the expansion was not recorded at info: %v", log.Records())
			}
			if rec.Context["workspace"] != string(ws.ID) || rec.Context["expanded_repository"] != true || rec.Context["expanded_subsection"] != true {
				t.Fatalf("the expansion's record = %+v, want the workspace and both folds named", rec.Context)
			}
		})
	}
}

func TestSelectingAWorkspaceInExpandedFoldsRecordsNoExpansion(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetCurrent(context.Background(), ws.ID, instant)

	// Assert
	if err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}
	if repositoryFolded(t, s, ws.Repo) {
		t.Fatal("an expanded repository was collapsed by the selection")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Message: "the selection expanded the collapsed folds holding the workspace"}); ok {
		t.Fatalf("a selection that expanded nothing recorded an expansion: %v", log.Records())
	}
}

func TestSelectingAWorkspaceIsAtomicWithItsExpansion(t *testing.T) {
	tests := []struct {
		name    string
		trigger string
	}{
		{"the repository's expansion fails", `CREATE TRIGGER fail_the_expansion BEFORE UPDATE OF folded ON repositories
			BEGIN SELECT RAISE(ABORT, 'the expansion failed'); END`},
		{"the subsection's expansion fails", `CREATE TRIGGER fail_the_expansion BEFORE UPDATE OF folded ON subsections
			BEGIN SELECT RAISE(ABORT, 'the expansion failed'); END`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			w := newSelectionFoldWorld(t, s)
			if err := s.SetCurrent(context.Background(), w.other.ID, instant); err != nil {
				t.Fatalf("SetCurrent: %v", err)
			}
			corrupt(t, s, tt.trigger)

			// Act
			err := s.SetCurrent(context.Background(), w.root.ID, instant)

			// Assert -- the failure rolls the selection back with the folds.
			if err == nil {
				t.Fatal("SetCurrent succeeded through a failed expansion")
			}
			got, err := s.Current(context.Background())
			if err != nil {
				t.Fatalf("Current: %v", err)
			}
			if got == nil || *got != w.other.ID {
				t.Fatalf("current = %v after the failure, want %q", got, w.other.ID)
			}
			if !repositoryFolded(t, s, w.root.Repo) || !subsectionNow(t, s, w.sub.ID).Folded {
				t.Fatal("a fold was expanded by a selection that failed")
			}
			if !loggedOperation(log, "daemon.wsm.set_current", "error") {
				t.Fatalf("the failed selection was not logged at error: %v", log.Records())
			}
		})
	}
}

// affected is a sql.Result answering N rows affected, or ERR.
type affected struct {
	n   int64
	err error
}

func (affected) LastInsertId() (int64, error)   { return 0, errors.New("not asked") }
func (a affected) RowsAffected() (int64, error) { return a.n, a.err }

func TestTouchedRow(t *testing.T) {
	failed := errors.New("rows affected unreadable")
	tests := []struct {
		name    string
		res     affected
		want    bool
		wantErr error
	}{
		{"no row is untouched", affected{n: 0}, false, nil},
		{"one row is touched", affected{n: 1}, true, nil},
		{"many rows are touched", affected{n: 3}, true, nil},
		{"an unreadable count is the error", affected{err: failed}, false, failed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := touchedRow(tt.res)

			// Assert
			if !errors.Is(err, tt.wantErr) {
				t.Fatalf("touchedRow err = %v, want %v", err, tt.wantErr)
			}
			if got != tt.want {
				t.Fatalf("touchedRow = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestEveryTouchedQuestionAsksTouchedRow pins that no store write hand-rolls
// "did it touch a row" off RowsAffected: a count compared with zero is
// touchedRow's one question.
func TestEveryTouchedQuestionAsksTouchedRow(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`n, err :?= res\.RowsAffected\(\)(?s:.{0,200}?)\bn (>|==|!=) 0\b`)

	for _, file := range sourcescan.Production(t) {
		// Act
		src := strings.Replace(string(file.Source), "func touchedRow(res sql.Result) (bool, error) {\n\tn, err := res.RowsAffected()\n\treturn n > 0, err", "", 1)

		// Assert
		if loc := handRolled.FindStringIndex(src); loc != nil {
			t.Errorf("%s hand-rolls touchedRow: %q", file.Name, src[loc[0]:loc[1]])
		}
	}
}
