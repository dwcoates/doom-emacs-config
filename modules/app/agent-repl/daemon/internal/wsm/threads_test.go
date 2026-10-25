package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	zettelv1 "agentrepl/proto/zettel/v1"
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
	var kind string
	if err := s.db().QueryRow(`SELECT kind FROM repositories WHERE id = ?`, loaded.Repo).Scan(&kind); err != nil {
		t.Fatal(err)
	}
	if RepositoryKind(kind) != RepositoryKindZettel {
		t.Errorf("the thread's repository is a %q, want the zettel home", kind)
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

func threadQuestion() *ThreadQuestion {
	return &ThreadQuestion{Text: "What is hydronium?", Parent: &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
		Node: &zettelv1.NodeId{Id: "n1"}, Revision: &zettelv1.RevisionNumber{Number: 1},
		Quote: &zettelv1.NodeQuote{Text: "forms hydronium"},
	}}}}
}

func TestAThreadQuestionRidesTheTurn(t *testing.T) {
	cases := []struct {
		name string
		put  func(s *store, ws Workspace) error
		read func(s *store, ws Workspace) (*ThreadQuestion, error)
	}{
		{"turn", func(s *store, ws Workspace) error {
			return s.PutTurn(context.Background(), Turn{ID: "turn-1", Workspace: ws.ID, Text: "q", Origin: "PROMPT_ORIGIN_COMPOSER", Question: threadQuestion()})
		}, func(s *store, ws Workspace) (*ThreadQuestion, error) {
			turn, err := s.Turn(context.Background(), ws.ID, "turn-1")
			return turn.Question, err
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			if err := tc.put(s, ws); err != nil {
				t.Fatal(err)
			}
			got, err := tc.read(s, ws)

			// Assert
			if err != nil || got == nil || got.Text != "What is hydronium?" || got.Parent.GetNode().GetQuote().GetText() != "forms hydronium" {
				t.Errorf("question = %+v, err = %v, want it carried whole", got, err)
			}
		})
	}
}

func TestAThreadQuestionIsStoredWholeOrRefused(t *testing.T) {
	cases := []struct {
		name     string
		question *ThreadQuestion
	}{
		{"a blank question", &ThreadQuestion{Text: " ", Parent: threadQuestion().Parent}},
		{"no origin", &ThreadQuestion{Text: "q", Parent: &zettelv1.NodeParent{}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.PutTurn(context.Background(), Turn{ID: "turn-1", Workspace: ws.ID, Text: "q", Origin: "PROMPT_ORIGIN_COMPOSER", Question: tc.question})

			// Assert
			if err == nil {
				t.Fatal("an incomplete thread question was stored")
			}
			if _, readErr := s.Turn(context.Background(), ws.ID, "turn-1"); !errors.Is(readErr, ErrNotFound) {
				t.Errorf("read after refusal = %v, want nothing stored", readErr)
			}
		})
	}
}

func TestAHalfStoredThreadQuestionIsADecodeFailure(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutTurn(context.Background(), Turn{ID: "turn-1", Workspace: ws.ID, Text: "q", Origin: "PROMPT_ORIGIN_COMPOSER", Question: threadQuestion()}); err != nil {
		t.Fatal(err)
	}
	corrupt(t, s, `UPDATE turns SET thread_parent = NULL`)

	// Act
	_, err := s.Turn(context.Background(), ws.ID, "turn-1")

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "thread_question" {
		t.Errorf("err = %v, want a thread_question decode failure", err)
	}
}

func TestAThreadAskRidesTheHeldPrompt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{Workspace: ws.ID, Turn: "turn-1", Said: said("q"), Origin: "PROMPT_ORIGIN_COMPOSER", Ask: &ThreadAsk{Parent: threadQuestion().Parent}})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	held, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil || len(held) != 1 || held[0].Ask == nil || held[0].Ask.Parent.GetNode().GetQuote().GetText() != "forms hydronium" {
		t.Errorf("held = %+v, err = %v, want the ask carried", held, err)
	}
}

func TestAThreadAskWithNoOriginIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{Workspace: ws.ID, Turn: "turn-1", Said: said("q"), Origin: "PROMPT_ORIGIN_COMPOSER", Ask: &ThreadAsk{Parent: &zettelv1.NodeParent{}}})

	// Assert
	if err == nil {
		t.Fatal("an ask with no origin was stored")
	}
}

func TestThreadAskGetParentNodeID(t *testing.T) {
	cases := []struct {
		name string
		ask  *ThreadAsk
		want string
	}{
		{"no ask", nil, ""},
		{"a thread start", &ThreadAsk{Parent: &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_ThreadStart{ThreadStart: &zettelv1.NodeParentThreadStart{}}}}, ""},
		{"asked of a node", &ThreadAsk{Parent: threadQuestion().Parent}, "n1"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act
			got := tc.ask.GetParentNodeID()

			// Assert
			if got != tc.want {
				t.Errorf("GetParentNodeID = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestLastInteractedAtIsTheLatestOfSelectionActivityAndCreation(t *testing.T) {
	created := time.Unix(100, 0)
	early, late := time.Unix(200, 0), time.Unix(300, 0)
	cases := []struct {
		name     string
		selected *time.Time
		activity *time.Time
		want     time.Time
	}{
		{name: "never selected nor active", want: created},
		{name: "selected last", selected: &late, activity: &early, want: late},
		{name: "active last", selected: &early, activity: &late, want: late},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			ws := Workspace{CreatedAt: created, LastSelectedAt: tc.selected, LastActivityAt: tc.activity}

			// Act
			got := ws.LastInteractedAt()

			// Assert
			if !got.Equal(tc.want) {
				t.Fatalf("LastInteractedAt = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestListRepositoriesNeverListsTheZettelHome(t *testing.T) {
	// Arrange
	db, _ := testStore(t)
	ctx := context.Background()
	if _, err := db.RegisterThread(ctx, filepath.Join(t.TempDir(), "threads", "why"), "why"); err != nil {
		t.Fatalf("RegisterThread: %v", err)
	}

	// Act
	repos, err := db.ListRepositories(ctx)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, repo := range repos {
		if repo.IsZettelHome() {
			t.Fatalf("repositories = %+v, want no zettel home", repos)
		}
	}
}

func TestAskedAtIsTheNewestQuestionOrTheCreation(t *testing.T) {
	created := time.Unix(100, 0)
	asked := time.Unix(300, 0)
	cases := []struct {
		name  string
		asked *time.Time
		want  time.Time
	}{
		{name: "never asked", want: created},
		{name: "asked since", asked: &asked, want: asked},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			selected := time.Unix(900, 0)
			ws := Workspace{CreatedAt: created, LastAskedAt: tc.asked, LastSelectedAt: &selected}

			// Act
			got := ws.AskedAt()

			// Assert: a later selection moves nothing.
			if !got.Equal(tc.want) {
				t.Fatalf("AskedAt = %v, want %v", got, tc.want)
			}
		})
	}
}
