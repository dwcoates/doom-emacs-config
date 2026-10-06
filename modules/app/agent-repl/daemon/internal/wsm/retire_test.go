package wsm

import (
	"context"
	"errors"
	"testing"
)

// closedUnder registers a workspace under repoDir and closes it, which is the
// state every retirable workspace is in.
func closedUnder(t *testing.T, s *store, repoDir string) Workspace {
	t.Helper()
	ws := registerUnder(t, s, repoDir)
	if err := s.SetClosed(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("SetClosed: %v", err)
	}
	return ws
}

func TestRetireRepositoryRemovesTheRepositoryAndItsClosedWorkspaces(t *testing.T) {
	tests := []struct {
		name       string
		workspaces int
	}{
		{name: "a repository registered on its own", workspaces: 0},
		{name: "a repository with one closed workspace", workspaces: 1},
		{name: "a repository with two closed workspaces", workspaces: 2},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo, _, err := s.RegisterRepository(context.Background(), t.TempDir(), "main")
			if err != nil {
				t.Fatalf("RegisterRepository: %v", err)
			}
			for i := 0; i < test.workspaces; i++ {
				closedUnder(t, s, repo.Dir)
			}

			// Act
			report, err := s.RetireRepository(context.Background(), repo.ID)

			// Assert
			if err != nil {
				t.Fatalf("RetireRepository: %v", err)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM repositories WHERE id = ?`, repo.ID); n != 0 {
				t.Fatalf("repository rows = %d, want 0", n)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE repo_id = ?`, repo.ID); n != 0 {
				t.Fatalf("workspace rows = %d, want 0", n)
			}
			if len(report.Workspaces) != test.workspaces {
				t.Fatalf("report.Workspaces = %v, want %d", report.Workspaces, test.workspaces)
			}
		})
	}
}

func TestRetireRepositoryReportsTheRetiredDir(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo, _, err := s.RegisterRepository(context.Background(), t.TempDir(), "main")
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}

	// Act
	report, err := s.RetireRepository(context.Background(), repo.ID)

	// Assert
	if err != nil {
		t.Fatalf("RetireRepository: %v", err)
	}
	if report.RepositoryDir != repo.Dir {
		t.Fatalf("report.RepositoryDir = %q, want %q", report.RepositoryDir, repo.Dir)
	}
}

func TestRetireRepositoryDeletesTheWorkspacesDependentRecords(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := t.TempDir()
	ws := closedUnder(t, s, repoDir)
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, CreatedAt: instant}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}

	// Act
	if _, err := s.RetireRepository(context.Background(), ws.Repo); err != nil {
		t.Fatalf("RetireRepository: %v", err)
	}

	// Assert
	for _, query := range []string{
		`SELECT count(*) FROM creation_jobs WHERE workspace_id = ?`,
		`SELECT count(*) FROM sessions WHERE workspace_id = ?`,
	} {
		if n := scalar[int](t, s, query, ws.ID); n != 0 {
			t.Fatalf("%q left %d rows, want none", query, n)
		}
	}
}

func TestRetireRepositoryDeletesTheMergeQueuePauseRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := closedUnder(t, s, t.TempDir())
	key := RepoKey(scalar[string](t, s, `SELECT dir FROM repositories WHERE id = ?`, ws.Repo))
	if err := s.SetMergeQueuePaused(context.Background(), key, true); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}

	// Act
	if _, err := s.RetireRepository(context.Background(), ws.Repo); err != nil {
		t.Fatalf("RetireRepository: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM merge_queue_repos WHERE repo_key = ?`, string(key)); n != 0 {
		t.Fatalf("merge_queue_repos left %d rows, want none", n)
	}
}

func TestRetireRepositoryLeavesAnotherRepositoryAlone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	retired := closedUnder(t, s, t.TempDir())
	kept := closedUnder(t, s, t.TempDir())

	// Act
	if _, err := s.RetireRepository(context.Background(), retired.Repo); err != nil {
		t.Fatalf("RetireRepository: %v", err)
	}

	// Assert
	if _, err := s.Workspace(context.Background(), kept.ID); err != nil {
		t.Fatalf("Workspace(%q) = %v, want the other repository's workspace kept", kept.ID, err)
	}
}

func TestRetireRepositoryRefusesWhileLiveStateRemains(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, ws Workspace)
	}{
		{
			name: "an open workspace",
			arrange: func(t *testing.T, s *store, ws Workspace) {
				if err := s.SetClosed(context.Background(), ws.ID, false); err != nil {
					t.Fatalf("SetClosed: %v", err)
				}
			},
		},
		{
			name: "an undelivered held prompt",
			arrange: func(t *testing.T, s *store, ws Workspace) {
				if err := s.PutHeldPrompt(context.Background(), HeldPrompt{
					Workspace: ws.ID, Turn: NewTurnID(), Said: said("parked"), Origin: "webapp", QueuedAt: instant,
				}); err != nil {
					t.Fatalf("PutHeldPrompt: %v", err)
				}
			},
		},
		{
			name: "a lease",
			arrange: func(t *testing.T, s *store, ws Workspace) {
				if _, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse); err != nil {
					t.Fatalf("AcquireLease: %v", err)
				}
			},
		},
		{
			name: "a merge-queue entry",
			arrange: func(t *testing.T, s *store, ws Workspace) {
				key := RepoKey(scalar[string](t, s, `SELECT dir FROM repositories WHERE id = ?`, ws.Repo))
				if err := s.RequestMerge(context.Background(), key, ws.ID, MergeSource{Kind: MergeSourceOwnBranch}, instant); err != nil {
					t.Fatalf("RequestMerge: %v", err)
				}
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := closedUnder(t, s, t.TempDir())
			test.arrange(t, s, ws)

			// Act
			_, err := s.RetireRepository(context.Background(), ws.Repo)

			// Assert
			if !errors.Is(err, ErrRepositoryInUse) {
				t.Fatalf("RetireRepository = %v, want ErrRepositoryInUse", err)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE id = ?`, ws.ID); n != 1 {
				t.Fatalf("a refused retirement left %d workspace rows, want the workspace kept", n)
			}
		})
	}
}

func TestRetireRepositoryRefusesAnUnknownRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.RetireRepository(context.Background(), RepoID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RetireRepository = %v, want ErrNotFound", err)
	}
}

func TestRetireRepositoryRecordsARefusalBelowError(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := registerUnder(t, s, t.TempDir())

	// Act
	_, err := s.RetireRepository(context.Background(), ws.Repo)

	// Assert
	if !errors.Is(err, ErrRepositoryInUse) {
		t.Fatalf("RetireRepository = %v, want ErrRepositoryInUse", err)
	}
	for _, record := range log.Records() {
		if record.Level == "error" {
			t.Fatalf("a refused retirement logged ERROR %q; the caller states it at its own level", record.Message)
		}
	}
}
