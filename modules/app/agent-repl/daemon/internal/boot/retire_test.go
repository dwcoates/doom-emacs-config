package boot

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/sessionlock"
	"claude-repld/internal/wsm"
)

const opRetire = "daemon.boot.retire_gone_repository"

// failingRetire is a state client whose repository retirement fails for a
// reason that is not the store refusing it.
type failingRetire struct {
	wsm.DB
	err error
}

func (d failingRetire) RetireRepository(context.Context, wsm.RepoID) (wsm.RetireReport, error) {
	return wsm.RetireReport{}, d.err
}

// goneRepository is one repository arranged by a case: registered at a
// directory of its own, with the workspaces the case puts under it, and its
// directory then removed.
type goneRepository struct {
	repo       wsm.RepoID
	workspaces []wsm.Workspace
}

// registerUnderRepo registers a workspace at dir under the repository at repoDir.
func (h *harness) registerUnderRepo(t *testing.T, dir, repoDir string) wsm.Workspace {
	t.Helper()
	ws, _, err := h.db.RegisterWorkspace(context.Background(), dir, wsm.RegisterFacts{
		Branch: "feature", ParentBranch: "master", RepoDir: repoDir, DefaultBranch: "master",
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", dir, err)
	}
	h.probes[ws.Dir] = sessionlock.StateFree
	return ws
}

func (h *harness) closeWorkspace(t *testing.T, ws wsm.Workspace) {
	t.Helper()
	if err := h.db.SetClosed(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("SetClosed(%s): %v", ws.ID, err)
	}
}

func repositoryRows(t *testing.T, h *harness, id wsm.RepoID) int {
	t.Helper()
	repos, err := h.db.ListRepositories(context.Background())
	if err != nil {
		t.Fatalf("ListRepositories: %v", err)
	}
	n := 0
	for _, repo := range repos {
		if repo.ID == id {
			n++
		}
	}
	return n
}

func TestAGoneRepositoryIsRetiredOrKept(t *testing.T) {
	tests := []struct {
		name string
		// arrange registers the repository and removes its directory.
		arrange     func(t *testing.T, h *harness) goneRepository
		wantRetired bool
		wantLevel   string
	}{
		{
			name: "a gone repository with no workspace is retired",
			arrange: func(t *testing.T, h *harness) goneRepository {
				repoDir := t.TempDir()
				repo, _, err := h.db.RegisterRepository(context.Background(), repoDir, "master")
				if err != nil {
					t.Fatalf("RegisterRepository: %v", err)
				}
				mustRemove(t, repoDir)
				return goneRepository{repo: repo.ID}
			},
			wantRetired: true,
			wantLevel:   "info",
		},
		{
			name: "a gone repository whose workspaces are all closed is retired",
			arrange: func(t *testing.T, h *harness) goneRepository {
				repoDir := t.TempDir()
				ws := h.registerUnderRepo(t, t.TempDir(), repoDir)
				h.closeWorkspace(t, ws)
				mustRemove(t, repoDir)
				return goneRepository{repo: ws.Repo, workspaces: []wsm.Workspace{ws}}
			},
			wantRetired: true,
			wantLevel:   "info",
		},
		{
			name: "a gone repository whose last open workspace this boot closed is retired",
			arrange: func(t *testing.T, h *harness) goneRepository {
				repoDir := t.TempDir()
				wsDir := t.TempDir()
				ws := h.registerUnderRepo(t, wsDir, repoDir)
				mustRemove(t, wsDir)
				mustRemove(t, repoDir)
				return goneRepository{repo: ws.Repo, workspaces: []wsm.Workspace{ws}}
			},
			wantRetired: true,
			wantLevel:   "info",
		},
		{
			name: "a gone repository with an open workspace is kept",
			arrange: func(t *testing.T, h *harness) goneRepository {
				repoDir := t.TempDir()
				ws := h.registerUnderRepo(t, t.TempDir(), repoDir)
				mustRemove(t, repoDir)
				return goneRepository{repo: ws.Repo, workspaces: []wsm.Workspace{ws}}
			},
			wantRetired: false,
			wantLevel:   "warn",
		},
		{
			name: "a gone repository whose closed workspace holds a prompt is kept",
			arrange: func(t *testing.T, h *harness) goneRepository {
				repoDir := t.TempDir()
				ws := h.registerUnderRepo(t, t.TempDir(), repoDir)
				h.closeWorkspace(t, ws)
				if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{
					Turn: wsm.NewTurnID(), Workspace: ws.ID, Said: userSaid("parked"),
					Origin: "PROMPT_ORIGIN_USER", QueuedAt: instant,
				}); err != nil {
					t.Fatalf("PutHeldPrompt: %v", err)
				}
				mustRemove(t, repoDir)
				return goneRepository{repo: ws.Repo, workspaces: []wsm.Workspace{ws}}
			},
			wantRetired: false,
			wantLevel:   "warn",
		},
		{
			name: "a repository whose directory cannot be stat-ed is never read as gone",
			arrange: func(t *testing.T, h *harness) goneRepository {
				parent := t.TempDir()
				repoDir := filepath.Join(parent, "repo")
				if err := os.Mkdir(repoDir, 0o755); err != nil {
					t.Fatalf("mkdir: %v", err)
				}
				ws := h.registerUnderRepo(t, t.TempDir(), repoDir)
				h.closeWorkspace(t, ws)
				// A parent with no search permission makes the stat fail
				// with EACCES, which is "could not tell", not "gone".
				if err := os.Chmod(parent, 0o000); err != nil {
					t.Fatalf("chmod: %v", err)
				}
				t.Cleanup(func() { _ = os.Chmod(parent, 0o755) })
				return goneRepository{repo: ws.Repo, workspaces: []wsm.Workspace{ws}}
			},
			wantRetired: false,
			wantLevel:   "warn",
		},
		{
			name: "a repository whose directory is present is kept",
			arrange: func(t *testing.T, h *harness) goneRepository {
				ws := h.registerUnderRepo(t, t.TempDir(), t.TempDir())
				h.closeWorkspace(t, ws)
				return goneRepository{repo: ws.Repo, workspaces: []wsm.Workspace{ws}}
			},
			wantRetired: false,
			wantLevel:   "",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			gone := test.arrange(t, h)

			// Act.
			report, err := h.seq.Run(context.Background())

			// Assert.
			if err != nil {
				t.Fatalf("Run = error %v, want a completed boot", err)
			}
			retired := len(report.RetiredRepositories) == 1 && report.RetiredRepositories[0] == gone.repo
			if retired != test.wantRetired {
				t.Fatalf("report.RetiredRepositories = %v, want retired=%v for %v", report.RetiredRepositories, test.wantRetired, gone.repo)
			}
			wantRows := 1
			if test.wantRetired {
				wantRows = 0
			}
			if n := repositoryRows(t, h, gone.repo); n != wantRows {
				t.Fatalf("repository rows = %d, want %d", n, wantRows)
			}
			for _, ws := range gone.workspaces {
				_, err := h.db.Workspace(context.Background(), ws.ID)
				if test.wantRetired != errors.Is(err, wsm.ErrNotFound) {
					t.Fatalf("Workspace(%s) = %v, want retired=%v", ws.ID, err, test.wantRetired)
				}
			}
			for _, level := range []string{"info", "warn", "error"} {
				if got := h.hasRecord(level, opRetire); got != (level == test.wantLevel) {
					t.Fatalf("a %s record under %s = %v, want only %q", level, opRetire, got, test.wantLevel)
				}
			}
		})
	}
}

func TestARetirementTheStoreCannotWriteFailsTheBoot(t *testing.T) {
	// Arrange.
	errBoom := errors.New("disk on fire")
	h := newHarness(t, func(d *Deps, h *harness) { d.DB = failingRetire{DB: h.db, err: errBoom} })
	repoDir := t.TempDir()
	if _, _, err := h.db.RegisterRepository(context.Background(), repoDir, "master"); err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	mustRemove(t, repoDir)

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if !errors.Is(err, errBoom) {
		t.Fatalf("Run = %v, want the retirement's failure", err)
	}
	if !h.hasRecord("error", opRetire) {
		t.Fatal("the failed retirement was not recorded at ERROR")
	}
}

func TestAJoiningDaemonRetiresNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps, _ *harness) { d.JoiningAddress = "127.0.0.1:41111" })
	repoDir := t.TempDir()
	repo, _, err := h.db.RegisterRepository(context.Background(), repoDir, "master")
	if err != nil {
		t.Fatalf("RegisterRepository: %v", err)
	}
	mustRemove(t, repoDir)

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.RetiredRepositories) != 0 || repositoryRows(t, h, repo.ID) != 1 {
		t.Fatalf("a joining daemon retired %v; it reconciles nothing", report.RetiredRepositories)
	}
}

func mustRemove(t *testing.T, dir string) {
	t.Helper()
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove %s: %v", dir, err)
	}
}
