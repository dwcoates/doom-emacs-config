package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
)

func TestRegisterThreadMintsAThreadInTheZettelHome(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	home := t.TempDir()

	// Act
	ws, err := s.RegisterThread(context.Background(), filepath.Join(home, "hydrogen-ions"), "hydrogen-ions")

	// Assert
	if err != nil {
		t.Fatalf("RegisterThread: %v", err)
	}
	loaded, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatal(err)
	}
	if !loaded.IsThread() || loaded.Name != "hydrogen-ions" || loaded.Branch != "" {
		t.Errorf("loaded = %+v, want a thread named after its directory with no branch", loaded)
	}
	repos, err := s.ListRepositories(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(repos) != 1 || !repos[0].IsZettelHome() || repos[0].ID != loaded.Repo {
		t.Errorf("repositories = %+v, want the one zettel home holding the thread", repos)
	}
}

func TestRegisterThreadSharesOneZettelHome(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	home := t.TempDir()
	first, err := s.RegisterThread(context.Background(), filepath.Join(home, "a"), "a")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	second, err := s.RegisterThread(context.Background(), filepath.Join(home, "b"), "b")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if first.Repo != second.Repo {
		t.Errorf("threads in one home got two repositories: %s, %s", first.Repo, second.Repo)
	}
}

func TestRegisterThreadRefusesARegisteredDirectory(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	dir := filepath.Join(t.TempDir(), "a")
	if _, err := s.RegisterThread(context.Background(), dir, "a"); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := s.RegisterThread(context.Background(), dir, "a")

	// Assert
	if !errors.Is(err, ErrThreadNameTaken) {
		t.Fatalf("err = %v, want ErrThreadNameTaken", err)
	}
	recorded := false
	for _, rec := range log.Records() {
		if rec.Level == "info" && rec.Operation == "daemon.wsm.register_thread" && strings.HasSuffix(rec.Context["dir"].(string), "/a") {
			recorded = true
		}
	}
	if !recorded {
		t.Errorf("records = %+v, want the refusal at info naming the directory", log.Records())
	}
	for _, rec := range log.Records() {
		if rec.Level == "error" {
			t.Errorf("a refusal was recorded at error: %+v", rec)
		}
	}
}

func TestRegisterWorkspaceStampsTheRepositoryKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	dir := t.TempDir()

	// Act
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{Branch: "b", RepoDir: dir})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	loaded, _ := s.Workspace(context.Background(), ws.ID)
	if ws.Kind != WorkspaceKindRepository || loaded.Kind != WorkspaceKindRepository || loaded.IsThread() {
		t.Errorf("kinds = %q (returned) %q (loaded), want repository", ws.Kind, loaded.Kind)
	}
	repos, _ := s.ListRepositories(context.Background())
	if len(repos) != 1 || repos[0].Kind != RepositoryKindGit {
		t.Errorf("repositories = %+v, want one git repository", repos)
	}
}

func TestTheZettelHomePairingIsEnforcedByTheDatabase(t *testing.T) {
	cases := []struct {
		name string
		act  func(s *store, thread Workspace, repository Workspace) error
	}{
		{"a repository workspace moved into the zettel home", func(s *store, thread, repository Workspace) error {
			_, err := s.db().Exec(`UPDATE workspaces SET repo_id = ? WHERE id = ?`, thread.Repo, repository.ID)
			return err
		}},
		{"a thread moved into a git repository", func(s *store, thread, repository Workspace) error {
			_, err := s.db().Exec(`UPDATE workspaces SET repo_id = ? WHERE id = ?`, repository.Repo, thread.ID)
			return err
		}},
		{"a repository workspace relabeled a thread", func(s *store, thread, repository Workspace) error {
			_, err := s.db().Exec(`UPDATE workspaces SET kind = 'thread' WHERE id = ?`, repository.ID)
			return err
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			thread, err := s.RegisterThread(context.Background(), filepath.Join(t.TempDir(), "t"), "t")
			if err != nil {
				t.Fatal(err)
			}
			dir := t.TempDir()
			repository, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{Branch: "b", RepoDir: dir})
			if err != nil {
				t.Fatal(err)
			}

			// Act
			err = tc.act(s, thread, repository)

			// Assert
			if err == nil || !strings.Contains(err.Error(), "a thread lives in the zettel home") {
				t.Errorf("err = %v, want the trigger's refusal", err)
			}
		})
	}
}

func TestAnUnknownKindIsADecodeFailure(t *testing.T) {
	cases := []struct {
		name   string
		break_ string
		read   func(s *store, ws Workspace) error
	}{
		{"workspace", `UPDATE workspaces SET kind = 'mystery'`, func(s *store, ws Workspace) error {
			_, err := s.Workspace(context.Background(), ws.ID)
			return err
		}},
		{"repository", `UPDATE repositories SET kind = 'mystery'`, func(s *store, ws Workspace) error {
			_, err := s.ListRepositories(context.Background())
			return err
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if _, err := s.db().Exec(`PRAGMA ignore_check_constraints = ON`); err != nil {
				t.Fatal(err)
			}
			if _, err := s.db().Exec(`DROP TRIGGER workspaces_thread_lives_in_the_zettel_home_update`); err != nil {
				t.Fatal(err)
			}
			if _, err := s.db().Exec(tc.break_); err != nil {
				t.Fatal(err)
			}

			// Act
			err := tc.read(s, ws)

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Field != "kind" {
				t.Errorf("err = %v, want a kind decode failure", err)
			}
		})
	}
}
