package wsm

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
)

// listedSubsection reads the subsection ID as the store lists it, false when it
// is gone.
func listedSubsection(t *testing.T, s *store, id SubsectionID) (Subsection, bool) {
	t.Helper()
	subs, err := s.ListSubsections(context.Background())
	if err != nil {
		t.Fatalf("ListSubsections: %v", err)
	}
	for _, sub := range subs {
		if sub.ID == id {
			return sub, true
		}
	}
	return Subsection{}, false
}

// closeWorkspace closes WS as the user does.
func closeWorkspace(t *testing.T, s *store, ws WorkspaceID) {
	t.Helper()
	closeWorkspaceBy(t, s, ws, CloseByUser)
}

// closeWorkspaceBy closes WS for CAUSE: a landing closes a parent whose
// children are still open, which the user's close refuses.
func closeWorkspaceBy(t *testing.T, s *store, ws WorkspaceID, cause CloseCause) {
	t.Helper()
	if err := s.MarkClosed(context.Background(), ws, Closure{At: hiddenAtInstant, By: cause}); err != nil {
		t.Fatalf("MarkClosed(%s): %v", ws, err)
	}
}

func TestCreateAgentSubsectionRecordsItsCreatorAndItsMembers(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	creator := registerIn(t, s, repoDir, "creator", nil)
	related := registerIn(t, s, repoDir, "related", nil)
	markOneShot(t, s, creator)

	// Act
	sub, err := s.CreateAgentSubsection(context.Background(), creator.ID, " auth work ", []WorkspaceID{creator.ID, related.ID})

	// Assert
	if err != nil {
		t.Fatalf("CreateAgentSubsection: %v", err)
	}
	listed, ok := listedSubsection(t, s, sub.ID)
	if !ok || listed.Name != "auth work" || listed.CreatedBy == nil || *listed.CreatedBy != creator.ID {
		t.Fatalf("listed = %+v (present %v), want %q created by %s", listed, ok, "auth work", creator.ID)
	}
	for _, ws := range []WorkspaceID{creator.ID, related.ID} {
		if got := subsectionOf(t, s, ws); got == nil || *got != sub.ID {
			t.Errorf("%s sits in %v, want %s", ws, got, sub.ID)
		}
	}
	if !oneShotOf(t, s, creator.ID) {
		t.Error("filing the creator cleared its one-shot mark")
	}
}

func TestAUserSubsectionCarriesNoCreatorAndSurvivesEmpty(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "one", nil)
	later := mustSubsection(t, s, ws.Repo, "later")

	// Act -- another write commits with the user's subsection empty.
	mustPlace(t, s, ws.ID, nil, AtEnd{})

	// Assert
	listed, ok := listedSubsection(t, s, later.ID)
	if !ok {
		t.Fatal("the empty user subsection was removed")
	}
	if listed.CreatedBy != nil {
		t.Fatalf("created by = %s, want none", *listed.CreatedBy)
	}
}

func TestAnAgentSubsectionGoesWithItsLastOpenConstituent(t *testing.T) {
	tests := []struct {
		name  string
		empty func(t *testing.T, s *store, a, b Workspace)
	}{
		{name: "its members close", empty: func(t *testing.T, s *store, a, b Workspace) {
			closeWorkspace(t, s, a.ID)
			closeWorkspace(t, s, b.ID)
		}},
		{name: "its members are placed elsewhere", empty: func(t *testing.T, s *store, a, b Workspace) {
			if err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{a.ID, b.ID}, nil, AtEnd{}, ClearOneShotMarks); err != nil {
				t.Fatalf("PlaceWorkspaces: %v", err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			a := registerIn(t, s, repoDir, "a", nil)
			b := registerIn(t, s, repoDir, "b", nil)
			sub, err := s.CreateAgentSubsection(context.Background(), a.ID, "pair", []WorkspaceID{a.ID, b.ID})
			if err != nil {
				t.Fatalf("CreateAgentSubsection: %v", err)
			}

			// Act
			tt.empty(t, s, a, b)

			// Assert
			if _, ok := listedSubsection(t, s, sub.ID); ok {
				t.Fatal("the empty agent subsection is still there")
			}
			for _, ws := range []WorkspaceID{a.ID, b.ID} {
				if got := subsectionOf(t, s, ws); got != nil {
					t.Errorf("%s still names subsection %s", ws, *got)
				}
			}
			record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opPruneAgentSubsection})
			if !ok || record.Context["subsection"] != string(sub.ID) || record.Context["created_by"] != string(a.ID) {
				t.Fatalf("prune record = %+v (found %v), want one naming %s created by %s", record, ok, sub.ID, a.ID)
			}
		})
	}
}

func TestAnAgentSubsectionStaysWhileOneConstituentIsOpen(t *testing.T) {
	tests := []struct {
		name  string
		close func(t *testing.T, s *store, a, b, child Workspace)
	}{
		{name: "one member closed", close: func(t *testing.T, s *store, a, _, _ Workspace) {
			closeWorkspace(t, s, a.ID)
		}},
		{name: "every root closed but a child open", close: func(t *testing.T, s *store, a, b, _ Workspace) {
			closeWorkspace(t, s, a.ID)
			closeWorkspaceBy(t, s, b.ID, CloseByMerge)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			a := registerIn(t, s, repoDir, "a", nil)
			b := registerIn(t, s, repoDir, "b", nil)
			child := registerChild(t, s, repoDir, "child", b.ID)
			sub, err := s.CreateAgentSubsection(context.Background(), a.ID, "pair", []WorkspaceID{a.ID, b.ID})
			if err != nil {
				t.Fatalf("CreateAgentSubsection: %v", err)
			}

			// Act
			tt.close(t, s, a, b, child)

			// Assert
			if _, ok := listedSubsection(t, s, sub.ID); !ok {
				t.Fatal("the agent subsection was removed with an open constituent")
			}
		})
	}
}

func TestCreateAgentSubsectionRefusesWhatItCannotFile(t *testing.T) {
	tests := []struct {
		name    string
		creator func(a Workspace) WorkspaceID
		members func(a, closed, foreign Workspace) []WorkspaceID
		label   string
		want    error
	}{
		{name: "an unknown creator", creator: func(Workspace) WorkspaceID { return "absent" },
			members: func(a, _, _ Workspace) []WorkspaceID { return []WorkspaceID{a.ID} }, label: "x", want: ErrNotFound},
		{name: "an unknown member", members: func(a, _, _ Workspace) []WorkspaceID { return []WorkspaceID{a.ID, "absent"} },
			label: "x", want: ErrNotFound},
		{name: "members of two repositories", members: func(a, _, foreign Workspace) []WorkspaceID { return []WorkspaceID{a.ID, foreign.ID} },
			label: "x", want: ErrOtherRepository},
		{name: "no open member", members: func(_, closed, _ Workspace) []WorkspaceID { return []WorkspaceID{closed.ID} },
			label: "x", want: ErrNoOpenConstituent},
		{name: "a blank name", members: func(a, _, _ Workspace) []WorkspaceID { return []WorkspaceID{a.ID} },
			label: "  ", want: ErrBlankSubsectionName},
		{name: "no member", members: func(Workspace, Workspace, Workspace) []WorkspaceID { return nil }, label: "x"},
		{name: "a member twice", members: func(a, _, _ Workspace) []WorkspaceID { return []WorkspaceID{a.ID, a.ID} }, label: "x"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			a := registerIn(t, s, repoDir, "a", nil)
			closed := registerIn(t, s, repoDir, "closed", nil)
			closeWorkspace(t, s, closed.ID)
			foreign := registerIn(t, s, arrangeRepo(t, "other"), "foreign", nil)
			creator := a.ID
			if tt.creator != nil {
				creator = tt.creator(a)
			}

			// Act
			_, err := s.CreateAgentSubsection(context.Background(), creator, tt.label, tt.members(a, closed, foreign))

			// Assert
			if err == nil {
				t.Fatal("CreateAgentSubsection succeeded")
			}
			if tt.want != nil && !errors.Is(err, tt.want) {
				t.Fatalf("err = %v, want %v", err, tt.want)
			}
			subs, listErr := s.ListSubsections(context.Background())
			if listErr != nil || len(subs) != 0 {
				t.Fatalf("subsections = %+v (%v), want none written", subs, listErr)
			}
			if got := subsectionOf(t, s, a.ID); got != nil {
				t.Fatalf("a sits in %s, want nothing written", *got)
			}
		})
	}
}

func TestAWriteWhoseSubsectionPruneFailsCommitsNothing(t *testing.T) {
	// Arrange -- the prune's read cannot run once its column is gone.
	s, log := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "one", nil)
	if _, err := s.db().ExecContext(context.Background(), `ALTER TABLE subsections RENAME COLUMN created_by TO retired`); err != nil {
		t.Fatalf("rename the column: %v", err)
	}

	// Act
	err := s.SetAttention(context.Background(), ws.ID, true)

	// Assert
	if err == nil {
		t.Fatal("SetAttention succeeded with a prune that cannot run")
	}
	got, readErr := s.Workspace(context.Background(), ws.ID)
	if readErr != nil || got.Attention {
		t.Fatalf("attention = %v (%v), want the write rolled back", got.Attention, readErr)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.set_attention",
		Message: "could not remove the agent-created subsections the write left empty"}); !ok {
		t.Fatal("the failed prune was not recorded at error under the write's operation")
	}
}
