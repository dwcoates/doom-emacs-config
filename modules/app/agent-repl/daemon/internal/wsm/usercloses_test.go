package wsm

import (
	"context"
	"testing"

	"agent-repld/internal/dlog"
)

// userSubsectionPair registers workspaces a and b in a new repository and
// files both into a subsection the user created.
func userSubsectionPair(t *testing.T, s *store) (Workspace, Workspace, Subsection) {
	t.Helper()
	repoDir := arrangeRepo(t, "repo")
	a := registerIn(t, s, repoDir, "a", nil)
	b := registerIn(t, s, repoDir, "b", nil)
	sub := mustSubsection(t, s, a.Repo, "pair")
	if err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{a.ID, b.ID}, &sub.ID, AtEnd{}); err != nil {
		t.Fatalf("PlaceWorkspaces: %v", err)
	}
	return a, b, sub
}

func TestTheUsersCloseOfTheLastOpenWorkspaceRemovesItsSubsection(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	a, b, sub := userSubsectionPair(t, s)
	closeWorkspaceBy(t, s, a.ID, CloseByAgent)

	// Act
	closeWorkspace(t, s, b.ID)

	// Assert
	if _, ok := listedSubsection(t, s, sub.ID); ok {
		t.Fatal("the subsection the user's close emptied is still there")
	}
	for _, ws := range []WorkspaceID{a.ID, b.ID} {
		if got := subsectionOf(t, s, ws); got != nil {
			t.Errorf("%s still names subsection %s", ws, *got)
		}
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRemoveUserClosedSubsection})
	if !ok || record.Context["subsection"] != string(sub.ID) || record.Context["workspace"] != string(b.ID) ||
		record.Context["name"] != "pair" || record.Context["write"] != "daemon.wsm.mark_closed" {
		t.Fatalf("removal record = %+v (found %v), want one naming %s emptied by %s's close", record, ok, sub.ID, b.ID)
	}
}

func TestTheUsersCloseLeavesAnAgentSubsectionToThePrune(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	a := registerIn(t, s, repoDir, "a", nil)
	b := registerIn(t, s, repoDir, "b", nil)
	sub, err := s.CreateAgentSubsection(context.Background(), a.ID, "pair", []WorkspaceID{a.ID, b.ID})
	if err != nil {
		t.Fatalf("CreateAgentSubsection: %v", err)
	}
	closeWorkspaceBy(t, s, a.ID, CloseByAgent)

	// Act
	closeWorkspace(t, s, b.ID)

	// Assert
	if _, ok := listedSubsection(t, s, sub.ID); ok {
		t.Fatal("the agent subsection the user's close emptied is still there")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opPruneAgentSubsection}); !ok {
		t.Fatal("the agent subsection was not removed by the write's prune")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: opRemoveUserClosedSubsection}); ok {
		t.Error("the user's close recorded removing an agent subsection the prune owns")
	}
}

func TestTheUsersCloseOfAChildWhoseRootIsClosedRemovesTheRootsSubsection(t *testing.T) {
	// Arrange -- a landing closed the root while its child stayed open.
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	root := registerIn(t, s, repoDir, "root", nil)
	child := registerChild(t, s, repoDir, "child", root.ID)
	sub := mustSubsection(t, s, root.Repo, "family")
	mustPlace(t, s, root.ID, &sub.ID, AtEnd{})
	closeWorkspaceBy(t, s, root.ID, CloseByMerge)

	// Act
	closeWorkspace(t, s, child.ID)

	// Assert
	if _, ok := listedSubsection(t, s, sub.ID); ok {
		t.Fatal("the subsection the child's close emptied is still there")
	}
}

func TestTheUsersCloseKeepsASubsectionWithAnOpenWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	a, b, sub := userSubsectionPair(t, s)

	// Act
	closeWorkspace(t, s, a.ID)

	// Assert
	if _, ok := listedSubsection(t, s, sub.ID); !ok {
		t.Fatal("the subsection was removed while one of its workspaces is open")
	}
	if got := subsectionOf(t, s, b.ID); got == nil || *got != sub.ID {
		t.Errorf("%s sits in %v, want %s", b.ID, got, sub.ID)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: opRemoveUserClosedSubsection}); ok {
		t.Error("a removal was recorded for a subsection still standing")
	}
}

func TestACloseNotTheUsersKeepsTheUsersEmptiedSubsection(t *testing.T) {
	for _, cause := range []CloseCause{CloseByAgent, CloseByMerge, CloseByDaemon} {
		t.Run(string(cause), func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			a, b, sub := userSubsectionPair(t, s)
			closeWorkspace(t, s, a.ID)

			// Act
			closeWorkspaceBy(t, s, b.ID, cause)

			// Assert
			if _, ok := listedSubsection(t, s, sub.ID); !ok {
				t.Fatalf("a %s close removed the user's subsection", cause)
			}
		})
	}
}

func TestTheUsersCloseOfAnAlreadyClosedWorkspaceRemovesNothing(t *testing.T) {
	// Arrange -- an agent's close emptied the user's subsection; a kill's
	// second close record follows on the closed row.
	s, _ := testStore(t)
	a, b, sub := userSubsectionPair(t, s)
	closeWorkspace(t, s, a.ID)
	closeWorkspaceBy(t, s, b.ID, CloseByAgent)

	// Act
	closeWorkspace(t, s, b.ID)

	// Assert
	if _, ok := listedSubsection(t, s, sub.ID); !ok {
		t.Fatal("a close of an already closed workspace removed its subsection")
	}
}

func TestAUsersCloseWhoseSubsectionRemovalFailsCommitsNothing(t *testing.T) {
	// Arrange -- the subsection's delete is refused by the database.
	s, log := testStore(t)
	a, b, sub := userSubsectionPair(t, s)
	closeWorkspace(t, s, a.ID)
	if _, err := s.db().ExecContext(context.Background(),
		`CREATE TRIGGER refuse_subsection_delete BEFORE DELETE ON subsections BEGIN SELECT RAISE(ABORT, 'refused'); END`); err != nil {
		t.Fatalf("create the trigger: %v", err)
	}

	// Act
	err := s.MarkClosed(context.Background(), b.ID, Closure{At: backloggedAtInstant, By: CloseByUser})

	// Assert
	if err == nil {
		t.Fatal("MarkClosed succeeded with a subsection removal that cannot run")
	}
	got, readErr := s.Workspace(context.Background(), b.ID)
	if readErr != nil || got.Closed {
		t.Fatalf("closed = %v (%v), want the close rolled back", got.Closed, readErr)
	}
	if _, ok := listedSubsection(t, s, sub.ID); !ok {
		t.Fatal("the subsection is gone after a rolled-back close")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.mark_closed",
		Message: "refused the write"}); !ok {
		t.Fatal("the failed close was not recorded at error under the close's operation")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: opRemoveUserClosedSubsection}); ok {
		t.Error("a removal was recorded for a close that committed nothing")
	}
}
