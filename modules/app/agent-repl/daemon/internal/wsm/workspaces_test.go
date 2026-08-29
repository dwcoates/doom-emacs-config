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
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}

	// Act
	if err := s.Forget(context.Background(), ws.ID); err != nil {
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
	err := s.Forget(context.Background(), WorkspaceID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Forget = %v, want ErrNotFound", err)
	}
}
