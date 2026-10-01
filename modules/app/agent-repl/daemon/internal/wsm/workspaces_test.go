package wsm

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"
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
	if first.Repo != second.Repo {
		t.Fatalf("repo ids = %q and %q, want one shared identity", first.Repo, second.Repo)
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

func TestRegisterRepositoryMintsARepositoryWithNoWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	dir := t.TempDir()

	// Act
	repo, created, err := s.RegisterRepository(context.Background(), dir, "main")

	// Assert
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if !created {
		t.Fatalf("RegisterRepository reported an existing record for a fresh store")
	}
	if repo.ID == "" || repo.DefaultBranch != "main" {
		t.Fatalf("RegisterRepository = %+v, want a minted id and the default branch main", repo)
	}
}

func TestRegisterRepositoryReportsARepositoryItAlreadyHeld(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	dir := t.TempDir()
	first, _, err := s.RegisterRepository(context.Background(), dir, "main")
	if err != nil {
		t.Fatalf("first RegisterRepository: %v", err)
	}

	// Act
	again, created, err := s.RegisterRepository(context.Background(), dir, "main")

	// Assert
	if err != nil {
		t.Fatalf("second RegisterRepository: %v", err)
	}
	if created {
		t.Fatalf("RegisterRepository minted a second record for %s", dir)
	}
	if again.ID != first.ID {
		t.Fatalf("RegisterRepository = id %q, want the id %q the first mint answered", again.ID, first.ID)
	}
}

// TestRegisterRepositoryAdoptsTheRepositoryARegisteredWorkspaceMinted pins the
// two mint paths on ONE row: a repository RegisterWorkspace minted through
// ensureRepo is the same repository this verb answers, never a second row for
// one directory.
func TestRegisterRepositoryAdoptsTheRepositoryARegisteredWorkspaceMinted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	repo, created, err := s.RegisterRepository(context.Background(), ws.Dir, "main")

	// Assert
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	if created {
		t.Fatalf("RegisterRepository minted a second row for the workspace's own repository")
	}
	if repo.ID != ws.Repo {
		t.Fatalf("RegisterRepository = id %q, want the workspace's repository %q", repo.ID, ws.Repo)
	}
}

func TestRegisterRepositoryRefusesABlankDirectory(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, _, err := s.RegisterRepository(context.Background(), "", "main")

	// Assert
	if err == nil {
		t.Fatalf("RegisterRepository(\"\") = no error, want a refusal")
	}
}

func TestSetClosedRecordsTheTeardown(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	if err := s.SetClosed(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("SetClosed: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if !got.Closed {
		t.Fatalf("closed = false after SetClosed(true)")
	}
}

func TestSetClosedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetClosed(context.Background(), WorkspaceID("absent"), true)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetClosed = %v, want ErrNotFound", err)
	}
}

func TestSetClosedReleasesOwnership(t *testing.T) {
	tests := []struct {
		name   string
		closed bool
		want   bool // whether serving and the spawned pid survive the write
	}{
		{name: "a close releases serving and clears the spawned shim pid", closed: true, want: false},
		{name: "a reopen leaves serving and the spawned shim pid untouched", closed: false, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if err := s.ClaimServing(ctx, ws.ID, NewInstanceID()); err != nil {
				t.Fatalf("ClaimServing: %v", err)
			}
			pid := 63777
			if err := s.SetSpawnedShimPID(ctx, ws.ID, &pid); err != nil {
				t.Fatalf("SetSpawnedShimPID: %v", err)
			}

			// Act
			if err := s.SetClosed(ctx, ws.ID, tt.closed); err != nil {
				t.Fatalf("SetClosed: %v", err)
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
			if (owner != nil) != tt.want {
				t.Fatalf("serving owner = %v, want present=%v", owner, tt.want)
			}
			if (got.SpawnedShimPID != nil) != tt.want {
				t.Fatalf("spawned shim pid = %v, want present=%v", got.SpawnedShimPID, tt.want)
			}
		})
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
	if err := s.SetMergedAt(context.Background(), ws.ID, instant); err != nil {
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
func registerUnder(t *testing.T, s *store, repoDir string) Workspace {
	t.Helper()
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

func TestForgetDisposesOfTheRepositoryOnlyWhenUnreferenced(t *testing.T) {
	tests := []struct {
		name string
		// siblings is how many OTHER workspaces are registered under the same
		// repository before the forget.
		siblings int
		// wantRepoRows is how many repository rows survive the forget.
		wantRepoRows int
		// wantReported is whether the report names the repository it removed.
		wantReported bool
	}{
		{name: "the last workspace takes its repository with it", siblings: 0, wantRepoRows: 0, wantReported: true},
		{name: "a sibling workspace keeps the repository", siblings: 1, wantRepoRows: 1, wantReported: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repoDir := t.TempDir()
			ws := registerUnder(t, s, repoDir)
			for i := 0; i < test.siblings; i++ {
				registerUnder(t, s, repoDir)
			}

			// Act
			report, err := s.Forget(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("Forget: %v", err)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM repositories WHERE id = ?`, ws.Repo); n != test.wantRepoRows {
				t.Fatalf("repository rows = %d, want %d", n, test.wantRepoRows)
			}
			reported := report.Repository != ""
			if reported != test.wantReported {
				t.Fatalf("report.Repository = %q, want reported = %v", report.Repository, test.wantReported)
			}
			if test.wantReported && report.Repository != ws.Repo {
				t.Fatalf("report.Repository = %q, want %q", report.Repository, ws.Repo)
			}
		})
	}
}

func TestForgetReportsTheForgottenRepositoryDir(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := t.TempDir()
	ws := registerUnder(t, s, repoDir)
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
	ws := registerUnder(t, s, repoDir)
	key := RepoKey(scalar[string](t, s, `SELECT dir FROM repositories WHERE id = ?`, ws.Repo))
	if err := s.SetMergeQueuePaused(context.Background(), key, true); err != nil {
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
	if err := s.SetMergeQueuePaused(context.Background(), key, true); err != nil {
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
