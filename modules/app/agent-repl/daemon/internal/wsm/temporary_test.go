package wsm

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/tempdirs"
)

// outsideTheRun is a temporary folder this test run does NOT own: the run's
// exempt root is a /tmp directory of its own, and /var/tmp is another root.
const outsideTheRun = "/var/tmp/agent-repl-wsm-test/scratch"

// assertNoRows fails t when the registry holds any repository or workspace.
func assertNoRows(t *testing.T, s *store) {
	t.Helper()
	workspaces, err := s.ListWorkspaces(context.Background())
	if err != nil {
		t.Fatalf("ListWorkspaces: %v", err)
	}
	repositories, err := s.ListRepositories(context.Background())
	if err != nil {
		t.Fatalf("ListRepositories: %v", err)
	}
	if len(workspaces) != 0 || len(repositories) != 0 {
		t.Fatalf("the refusal wrote rows: %d workspaces, %d repositories", len(workspaces), len(repositories))
	}
}

// assertRefusalRecorded fails t unless log holds the store's DEBUG record of
// the refusal under op, naming the temporary root.
func assertRefusalRecorded(t *testing.T, log *dlog.TestLogger, op string) {
	t.Helper()
	for _, r := range log.Records() {
		if r.Operation == op && r.Level == "debug" && r.Context["temporary_root"] != nil {
			return
		}
	}
	t.Fatalf("no debug record of the refusal under %s: %+v", op, log.Records())
}

func TestRegisterWorkspaceRefusesATemporaryDirectory(t *testing.T) {
	tests := []struct {
		name    string
		dir     func(t *testing.T) string
		repoDir func(t *testing.T) string
	}{
		{
			name:    "the workspace directory is temporary",
			dir:     func(*testing.T) string { return outsideTheRun },
			repoDir: func(t *testing.T) string { return t.TempDir() },
		},
		{
			name:    "the repository it would mint is temporary",
			dir:     func(t *testing.T) string { return t.TempDir() },
			repoDir: func(*testing.T) string { return outsideTheRun },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			_, _, err := s.RegisterWorkspace(context.Background(), tc.dir(t), RegisterFacts{
				Branch: "feature", ParentBranch: "master", RepoDir: tc.repoDir(t),
			})

			// Assert
			inside, ok := tempdirs.AsInside(err)
			if !ok {
				t.Fatalf("RegisterWorkspace = %v, want a temporary-directory refusal", err)
			}
			if want, _ := filepath.EvalSymlinks("/var/tmp"); inside.Root != want {
				t.Errorf("Root = %q, want %q", inside.Root, want)
			}
			assertNoRows(t, s)
			assertRefusalRecorded(t, log, "daemon.wsm.register_workspace")
		})
	}
}

func TestRegisterRepositoryRefusesATemporaryDirectory(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, _, err := s.RegisterRepository(context.Background(), outsideTheRun, "master")

	// Assert
	if _, ok := tempdirs.AsInside(err); !ok {
		t.Fatalf("RegisterRepository = %v, want a temporary-directory refusal", err)
	}
	assertNoRows(t, s)
	assertRefusalRecorded(t, log, "daemon.wsm.register_repository")
}

func TestRegisterWorkspaceDoesNotHandBackAStandingTemporaryRow(t *testing.T) {
	// Arrange: a row registered under the run's exempt root, then the same
	// file opened with the PRODUCTION guard, which exempts nothing.
	path := filepath.Join(t.TempDir(), "wsm.db")
	dir := t.TempDir()
	first, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if _, _, err := first.RegisterWorkspace(context.Background(), dir, RegisterFacts{Branch: "feature", ParentBranch: "master", RepoDir: dir}); err != nil {
		t.Fatalf("RegisterWorkspace under the exempt root: %v", err)
	}
	first.Close()
	production, err := tempdirs.New(os.TempDir(), "")
	if err != nil {
		t.Fatalf("build the production guard: %v", err)
	}
	handle, err := Open(context.Background(), path, WithTemporaryGuard(production))
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { handle.Close() })

	// Act
	_, _, err = handle.RegisterWorkspace(context.Background(), dir, RegisterFacts{Branch: "feature", ParentBranch: "master", RepoDir: dir})

	// Assert
	if _, ok := tempdirs.AsInside(err); !ok {
		t.Fatalf("re-registering a standing temporary row = %v, want a refusal", err)
	}
}

func TestRegistrationOutsideEveryTemporaryRootIsUnaffected(t *testing.T) {
	// Arrange: the exempt run root stands in for a non-temporary directory,
	// which is what the guard makes of it.
	s, _ := testStore(t)
	dir := t.TempDir()

	// Act
	_, created, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{Branch: "feature", ParentBranch: "master", RepoDir: dir})

	// Assert
	if err != nil || !created {
		t.Fatalf("RegisterWorkspace = (created %v, %v), want a fresh row", created, err)
	}
}
