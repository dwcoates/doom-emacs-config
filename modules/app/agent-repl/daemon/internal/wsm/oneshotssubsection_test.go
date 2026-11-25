package wsm

import (
	"context"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

// oneShotsPlacement files a registered workspace as a one-shot the user
// dispatched.
var oneShotsPlacement = RegisterFacts{OneShot: true, Placement: &RegisterPlacement{OneShots: true}}

// registerUserOneShot registers a one-shot NAME under repoDir filed into the
// one-shots subsection, failing the test on any error.
func registerUserOneShot(t *testing.T, s *store, repoDir, name string) Workspace {
	t.Helper()
	ws, err := registerWith(t, s, repoDir, name, oneShotsPlacement)
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", name, err)
	}
	if ws.Subsection == nil {
		t.Fatalf("%s was registered in no subsection, want the one-shots one", name)
	}
	return ws
}

// oneShotsRepo arranges repository NAME with its main worktree registered,
// as a one-shot's repository always has.
func oneShotsRepo(t *testing.T, s *store, name string) string {
	t.Helper()
	repoDir := arrangeRepo(t, name)
	registerMain(t, s, repoDir)
	return repoDir
}

func TestRegisterIntoTheOneShotsSubsectionCreatesItWhenAbsent(t *testing.T) {
	// Arrange -- a user subsection of the same name is not the one-shots one.
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	other := registerIn(t, s, repoDir, "other", nil)
	user := mustSubsection(t, s, other.Repo, OneShotsSubsectionName)

	// Act
	ws, err := registerWith(t, s, repoDir, "shot", oneShotsPlacement)

	// Assert
	if err != nil || ws.Subsection == nil {
		t.Fatalf("RegisterWorkspace = %+v, %v; want a workspace in the one-shots subsection", ws.Subsection, err)
	}
	if *ws.Subsection == user.ID {
		t.Fatal("the one-shot joined the user's subsection of the same name, want the daemon's own")
	}
	listed, ok := listedSubsection(t, s, *ws.Subsection)
	if !ok || listed.Name != OneShotsSubsectionName || !listed.OneShots || listed.CreatedBy != nil {
		t.Fatalf("listed = %+v (present %v), want %q marked one-shots with no creator", listed, ok, OneShotsSubsectionName)
	}
	stored, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || stored.Subsection == nil || *stored.Subsection != listed.ID {
		t.Fatalf("stored subsection = %v, %v; want %s", stored.Subsection, err, listed.ID)
	}
}

func TestRegisterIntoTheOneShotsSubsectionJoinsTheExistingOne(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := oneShotsRepo(t, s, "repo")
	first := registerUserOneShot(t, s, repoDir, "first")

	// Act
	second := registerUserOneShot(t, s, repoDir, "second")

	// Assert
	if *second.Subsection != *first.Subsection {
		t.Fatalf("second sits in %s, want the first's one-shots subsection %s", *second.Subsection, *first.Subsection)
	}
	if got := scalar[int](t, s, `SELECT COUNT(*) FROM subsections WHERE one_shots`); got != 1 {
		t.Fatalf("one-shots subsections = %d, want 1", got)
	}
}

func TestEachRepositoryHasItsOwnOneShotsSubsection(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	here := registerUserOneShot(t, s, oneShotsRepo(t, s, "here"), "a")

	// Act
	there := registerUserOneShot(t, s, oneShotsRepo(t, s, "there"), "b")

	// Assert
	if *here.Subsection == *there.Subsection {
		t.Fatalf("both repositories share subsection %s, want one each", *here.Subsection)
	}
}

func TestARepositoryHoldsAtMostOneOneShotsSubsection(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerUserOneShot(t, s, oneShotsRepo(t, s, "repo"), "shot")

	// Act -- a second marked row for the same repository.
	_, err := s.db().ExecContext(context.Background(),
		`INSERT INTO subsections (id, repo_id, name, place, one_shots) VALUES (?, ?, 'again', 99, 1)`, NewSubsectionID(), ws.Repo)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "UNIQUE") {
		t.Fatalf("second one-shots subsection insert = %v, want the unique index to refuse it", err)
	}
}

func TestTheOneShotsSubsectionGoesWithItsLastOpenOneShot(t *testing.T) {
	tests := []struct {
		name  string
		empty func(t *testing.T, s *store, a, b Workspace)
	}{
		{name: "the user closes or kills them", empty: func(t *testing.T, s *store, a, b Workspace) {
			closeWorkspace(t, s, a.ID)
			closeWorkspace(t, s, b.ID)
		}},
		{name: "they land", empty: func(t *testing.T, s *store, a, b Workspace) {
			closeWorkspaceBy(t, s, a.ID, CloseByMerge)
			closeWorkspaceBy(t, s, b.ID, CloseByMerge)
		}},
		{name: "they close themselves", empty: func(t *testing.T, s *store, a, b Workspace) {
			closeWorkspaceBy(t, s, a.ID, CloseByAgent)
			closeWorkspaceBy(t, s, b.ID, CloseByDaemon)
		}},
		{name: "they are dragged out", empty: func(t *testing.T, s *store, a, b Workspace) {
			mustPlace(t, s, a.ID, nil, AtEnd{})
			mustPlace(t, s, b.ID, nil, AtEnd{})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			repoDir := oneShotsRepo(t, s, "repo")
			a := registerUserOneShot(t, s, repoDir, "a")
			b := registerUserOneShot(t, s, repoDir, "b")
			sub := *a.Subsection

			// Act
			tt.empty(t, s, a, b)

			// Assert
			if _, ok := listedSubsection(t, s, sub); ok {
				t.Fatal("the empty one-shots subsection is still there")
			}
			record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opPruneAgentSubsection})
			if !ok || record.Context["subsection"] != string(sub) || record.Context["one_shots"] != true {
				t.Fatalf("prune record = %+v (found %v), want one naming the one-shots subsection %s", record, ok, sub)
			}
			if _, named := record.Context["created_by"]; named {
				t.Fatalf("prune record names a creator %v, want none for the daemon's subsection", record.Context["created_by"])
			}
		})
	}
}

func TestTheOneShotsSubsectionStaysWhileOneOneShotIsOpen(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := oneShotsRepo(t, s, "repo")
	a := registerUserOneShot(t, s, repoDir, "a")
	registerUserOneShot(t, s, repoDir, "b")

	// Act
	closeWorkspace(t, s, a.ID)

	// Assert
	if _, ok := listedSubsection(t, s, *a.Subsection); !ok {
		t.Fatal("the one-shots subsection was removed with a one-shot still open in it")
	}
}

func TestTheOneShotsSubsectionIsCreatedAgainAfterItsRemoval(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := oneShotsRepo(t, s, "repo")
	first := registerUserOneShot(t, s, repoDir, "first")
	closeWorkspace(t, s, first.ID)

	// Act
	second := registerUserOneShot(t, s, repoDir, "second")

	// Assert
	if *second.Subsection == *first.Subsection {
		t.Fatalf("second joined the removed subsection %s", *first.Subsection)
	}
	if listed, ok := listedSubsection(t, s, *second.Subsection); !ok || !listed.OneShots {
		t.Fatalf("listed = %+v (present %v), want a fresh one-shots subsection", listed, ok)
	}
}

func TestTheUserSubsectionQueryLeavesOutTheOneShotsSubsection(t *testing.T) {
	// Arrange -- an empty one-shots subsection, as only a failed prune leaves.
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "one", nil)
	if _, err := s.db().ExecContext(context.Background(),
		`INSERT INTO subsections (id, repo_id, name, place, one_shots) VALUES (?, ?, ?, 1, 1)`, NewSubsectionID(), ws.Repo, OneShotsSubsectionName); err != nil {
		t.Fatalf("insert the one-shots subsection: %v", err)
	}

	// Act
	user, userErr := listRows(context.Background(), s.db(), emptyUserSubsectionsQuery, scanSubsection)
	pruned, prunedErr := listRows(context.Background(), s.db(), emptyPrunedSubsectionsQuery, scanSubsection)

	// Assert
	if userErr != nil || prunedErr != nil {
		t.Fatalf("list the empty subsections: %v, %v", userErr, prunedErr)
	}
	if len(user) != 0 {
		t.Fatalf("empty user subsections = %+v, want none", user)
	}
	if len(pruned) != 1 || !pruned[0].OneShots {
		t.Fatalf("empty pruned subsections = %+v, want the one-shots subsection", pruned)
	}
}

func TestAOneShotDraggedOutOfTheOneShotsSubsectionLandsWherePlacedAndKeepsItsMark(t *testing.T) {
	// Arrange -- a registered workspace beside a one-shot the user dispatched.
	s, _ := testStore(t)
	repoDir := oneShotsRepo(t, s, "repo")
	a := registerIn(t, s, repoDir, "a", nil)
	shot := registerUserOneShot(t, s, repoDir, "shot")
	markOneShot(t, s, shot)
	if a.Subsection != nil {
		t.Fatalf("a was filed in subsection %s, want none", *a.Subsection)
	}

	// Act
	mustPlace(t, s, shot.ID, nil, AtEnd{})

	// Assert
	if got := subsectionOf(t, s, shot.ID); got != nil {
		t.Fatalf("the placed one-shot sits in subsection %s, want none", *got)
	}
	if !oneShotOf(t, s, shot.ID) {
		t.Fatal("the placed one-shot lost its one-shot mark")
	}
	if names := orderedNames(t, s); names[len(names)-1] != "shot" {
		t.Fatalf("place order = %v, want the placed one-shot last", names)
	}
	if _, ok := listedSubsection(t, s, *shot.Subsection); ok {
		t.Fatal("the emptied one-shots subsection is still there")
	}
}
