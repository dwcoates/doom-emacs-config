package wsm

import (
	"context"
	"path/filepath"
	"testing"
)

// registerBornIntake registers a linked workspace born an intake workspace.
func registerBornIntake(t *testing.T, s *store, repoDir, name string) Workspace {
	t.Helper()
	registerMain(t, s, repoDir)
	ws, _, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), name), RegisterFacts{
		Name: name, Branch: name, ParentBranch: "master", RepoDir: repoDir, Intake: true,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", name, err)
	}
	return ws
}

func TestAnIntakeWorkspaceIsBornMarkedAndHidden(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ws := registerBornIntake(t, s, arrangeRepo(t, "repo"), "intake")

	// Assert
	intake, err := s.IsIntake(context.Background(), ws.ID)
	if err != nil || !intake {
		t.Fatalf("IsIntake = %v, %v; want the intake workspace", intake, err)
	}
	if !ws.Hidden || ws.HiddenAt == nil {
		t.Fatalf("the record Register answered = hidden %v at %v, want an intake workspace born hidden", ws.Hidden, ws.HiddenAt)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || !got.Hidden {
		t.Fatalf("Workspace hidden = %v, %v; want the intake workspace hidden", got.Hidden, err)
	}
}

func TestAWorkspaceNeverMarkedIsNoIntakeWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	intake, err := s.IsIntake(context.Background(), ws.ID)

	// Assert
	if err != nil || intake {
		t.Fatalf("IsIntake = %v, %v; want false", intake, err)
	}
}

func TestForgettingAnIntakeWorkspaceForgetsItsMark(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerBornIntake(t, s, arrangeRepo(t, "repo"), "intake")
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByDaemon}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	_, err := s.Forget(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Forget: %v", err)
	}
	intake, err := s.IsIntake(context.Background(), ws.ID)
	if err != nil || intake {
		t.Fatalf("IsIntake after the forget = %v, %v; want false", intake, err)
	}
}

// registerCardIntake registers a linked workspace born the intake of CARD.
func registerCardIntake(t *testing.T, s *store, repoDir, name string, card IntakeCard) Workspace {
	t.Helper()
	ws, _, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), name), RegisterFacts{
		Name: name, Branch: name, ParentBranch: "master", RepoDir: repoDir, Intake: true, IntakeCard: &card,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", name, err)
	}
	return ws
}

// registerRequested registers a linked workspace whose creation BY asked for.
func registerRequested(t *testing.T, s *store, repoDir, name string, by WorkspaceID) Workspace {
	t.Helper()
	ws, _, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), name), RegisterFacts{
		Name: name, Branch: name, ParentBranch: "master", RepoDir: repoDir, RequestedBy: &by,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", name, err)
	}
	return ws
}

// card is the card every addressee test asks about.
var card = IntakeCard{Asker: "w-asker", Row: "row-1"}

func TestCardAddressees(t *testing.T) {
	tests := []struct {
		name string
		// arrange registers the card's history, answering the wanted
		// work workspace and open intake by name ("" for none).
		arrange    func(t *testing.T, s *store, repo string)
		wantWork   string
		wantIntake string
	}{
		{name: "a card never asked about has none", arrange: func(*testing.T, *store, string) {}},
		{name: "the intake still taking the question in", arrange: func(t *testing.T, s *store, repo string) {
			registerCardIntake(t, s, repo, "intake-1", card)
		}, wantIntake: "intake-1"},
		{name: "the workspace the intake created", arrange: func(t *testing.T, s *store, repo string) {
			intake := registerCardIntake(t, s, repo, "intake-1", card)
			registerRequested(t, s, repo, "work-1", intake.ID)
			closeWorkspace(t, s, intake.ID)
		}, wantWork: "work-1"},
		{name: "a closed created workspace addresses nothing", arrange: func(t *testing.T, s *store, repo string) {
			intake := registerCardIntake(t, s, repo, "intake-1", card)
			work := registerRequested(t, s, repo, "work-1", intake.ID)
			closeWorkspace(t, s, intake.ID)
			closeWorkspace(t, s, work.ID)
		}},
		{name: "the newest open created workspace of every intake", arrange: func(t *testing.T, s *store, repo string) {
			first := registerCardIntake(t, s, repo, "intake-1", card)
			registerRequested(t, s, repo, "work-1", first.ID)
			second := registerCardIntake(t, s, repo, "intake-2", card)
			registerRequested(t, s, repo, "work-2", second.ID)
			closeWorkspace(t, s, first.ID)
			closeWorkspace(t, s, second.ID)
		}, wantWork: "work-2"},
		{name: "an intake that created an intake is no created workspace", arrange: func(t *testing.T, s *store, repo string) {
			intake := registerCardIntake(t, s, repo, "intake-1", card)
			by := intake.ID
			if _, _, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), "intake-2"), RegisterFacts{
				Name: "intake-2", Branch: "intake-2", ParentBranch: "master", RepoDir: repo, Intake: true, RequestedBy: &by,
			}); err != nil {
				t.Fatalf("RegisterWorkspace: %v", err)
			}
			closeWorkspace(t, s, intake.ID)
		}},
		{name: "another card's workspaces address nothing", arrange: func(t *testing.T, s *store, repo string) {
			intake := registerCardIntake(t, s, repo, "intake-1", IntakeCard{Asker: card.Asker, Row: "row-2"})
			registerRequested(t, s, repo, "work-1", intake.ID)
			registerCardIntake(t, s, repo, "intake-3", IntakeCard{Asker: "w-other", Row: card.Row})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := arrangeRepo(t, "repo")
			registerMain(t, s, repo)
			tt.arrange(t, s, repo)

			// Act
			got, err := s.CardAddressees(context.Background(), card)

			// Assert
			if err != nil {
				t.Fatalf("CardAddressees: %v", err)
			}
			if name := addresseeName(t, s, got.Work); name != tt.wantWork {
				t.Errorf("work = %q, want %q", name, tt.wantWork)
			}
			if name := addresseeName(t, s, got.OpenIntake); name != tt.wantIntake {
				t.Errorf("open intake = %q, want %q", name, tt.wantIntake)
			}
		})
	}
}

// addresseeName is ID's workspace name, "" for nil.
func addresseeName(t *testing.T, s *store, id *WorkspaceID) string {
	t.Helper()
	if id == nil {
		return ""
	}
	ws, err := s.Workspace(context.Background(), *id)
	if err != nil {
		t.Fatalf("Workspace(%s): %v", *id, err)
	}
	return ws.Name
}

func TestAnIntakeCardOnNoIntakeWorkspaceRegistersNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := arrangeRepo(t, "repo")
	registerMain(t, s, repo)
	before := scalar[int](t, s, `SELECT COUNT(*) FROM workspaces`)

	// Act
	_, _, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), "plain"), RegisterFacts{
		Name: "plain", Branch: "plain", ParentBranch: "master", RepoDir: repo, IntakeCard: &card,
	})

	// Assert
	if err == nil {
		t.Fatal("RegisterWorkspace recorded an intake card on no intake workspace")
	}
	if got := scalar[int](t, s, `SELECT COUNT(*) FROM workspaces`); got != before {
		t.Fatalf("workspaces = %d, want %d: nothing registered", got, before)
	}
	if got := scalar[int](t, s, `SELECT COUNT(*) FROM intake_cards`); got != 0 {
		t.Fatalf("intake cards = %d, want none", got)
	}
}
