package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"testing"

	"agent-repld/internal/dlog"
)

// registerWith registers a linked workspace NAME under repoDir with FACTS
// (its name, branch and repository filled in), answering the error.
func registerWith(t *testing.T, s *store, repoDir, name string, facts RegisterFacts) (Workspace, error) {
	t.Helper()
	facts.Name, facts.Branch, facts.ParentBranch, facts.RepoDir = name, name, "master", repoDir
	ws, _, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), name), facts)
	return ws, err
}

// workspaceCount is how many workspace rows the store holds.
func workspaceCount(t *testing.T, s *store) int {
	t.Helper()
	return scalar[int](t, s, `SELECT COUNT(*) FROM workspaces`)
}

func subsectionCount(t *testing.T, s *store) int {
	t.Helper()
	return scalar[int](t, s, `SELECT COUNT(*) FROM subsections`)
}

func TestRegisterBornHiddenWritesTheHiddenColumnsInTheInsert(t *testing.T) {
	tests := []struct {
		name       string
		facts      RegisterFacts
		wantHidden bool
	}{
		{name: "hidden", facts: RegisterFacts{Hidden: true}, wantHidden: true},
		{name: "intake implies hidden", facts: RegisterFacts{Intake: true}, wantHidden: true},
		{name: "neither", facts: RegisterFacts{}, wantHidden: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			registerMain(t, s, repoDir)

			// Act
			ws, err := registerWith(t, s, repoDir, "born", tt.facts)

			// Assert
			if err != nil {
				t.Fatalf("RegisterWorkspace: %v", err)
			}
			if ws.Hidden != tt.wantHidden || (ws.HiddenAt != nil) != tt.wantHidden {
				t.Fatalf("answered record hidden = %v at %v, want hidden %v", ws.Hidden, ws.HiddenAt, tt.wantHidden)
			}
			if stored := scalar[bool](t, s, `SELECT hidden FROM workspaces WHERE id = ?`, ws.ID); stored != tt.wantHidden {
				t.Fatalf("stored hidden = %v, want %v", stored, tt.wantHidden)
			}
			if marked := scalar[int](t, s, `SELECT COUNT(*) FROM intake_workspaces WHERE workspace_id = ?`, ws.ID); (marked == 1) != tt.facts.Intake {
				t.Fatalf("intake rows = %d, want one exactly when the workspace is an intake", marked)
			}
		})
	}
}

func TestRegisterPlacedFilesTheWorkspaceInTheInsert(t *testing.T) {
	t.Run("into an existing subsection by name", func(t *testing.T) {
		// Arrange
		s, _ := testStore(t)
		repoDir := arrangeRepo(t, "repo")
		creator := registerIn(t, s, repoDir, "creator", nil)
		sub := mustSubsection(t, s, creator.Repo, "auth")

		// Act
		ws, err := registerWith(t, s, repoDir, "born", RegisterFacts{Placement: &RegisterPlacement{Into: " auth ", CreatedBy: creator.ID}})

		// Assert
		if err != nil {
			t.Fatalf("RegisterWorkspace: %v", err)
		}
		if ws.Subsection == nil || *ws.Subsection != sub.ID {
			t.Fatalf("answered subsection = %v, want %s", ws.Subsection, sub.ID)
		}
		got, err := s.Workspace(context.Background(), ws.ID)
		if err != nil || got.Subsection == nil || *got.Subsection != sub.ID {
			t.Fatalf("stored subsection = %+v, %v; want %s", got.Subsection, err, sub.ID)
		}
	})

	t.Run("into a new subsection gathering open unsectioned workspaces", func(t *testing.T) {
		// Arrange
		s, _ := testStore(t)
		repoDir := arrangeRepo(t, "repo")
		creator := registerIn(t, s, repoDir, "creator", nil)
		a := registerIn(t, s, repoDir, "gather-a", nil)
		b := registerIn(t, s, repoDir, "gather-b", nil)

		// Act
		ws, err := registerWith(t, s, repoDir, "born", RegisterFacts{Placement: &RegisterPlacement{
			New: &NewSubsectionPlacement{Name: " intake work ", Gather: []string{"gather-a", "gather-b"}}, CreatedBy: creator.ID,
		}})

		// Assert
		if err != nil {
			t.Fatalf("RegisterWorkspace: %v", err)
		}
		if ws.Subsection == nil {
			t.Fatalf("answered subsection = nil, want the new subsection")
		}
		listed, ok := listedSubsection(t, s, *ws.Subsection)
		if !ok || listed.Name != "intake work" || listed.CreatedBy == nil || *listed.CreatedBy != creator.ID {
			t.Fatalf("listed = %+v (present %v), want %q created by %s", listed, ok, "intake work", creator.ID)
		}
		for _, member := range []WorkspaceID{a.ID, b.ID, ws.ID} {
			got, err := s.Workspace(context.Background(), member)
			if err != nil || got.Subsection == nil || *got.Subsection != listed.ID {
				t.Fatalf("workspace %s subsection = %v, %v; want %s", member, got.Subsection, err, listed.ID)
			}
		}
		if creatorNow, _ := s.Workspace(context.Background(), creator.ID); creatorNow.Subsection != nil {
			t.Fatalf("the creator was moved into %v, want it left unsectioned", creatorNow.Subsection)
		}
	})

	t.Run("into a new subsection of its own", func(t *testing.T) {
		// Arrange
		s, _ := testStore(t)
		repoDir := arrangeRepo(t, "repo")
		creator := registerIn(t, s, repoDir, "creator", nil)

		// Act
		ws, err := registerWith(t, s, repoDir, "born", RegisterFacts{Placement: &RegisterPlacement{
			New: &NewSubsectionPlacement{Name: "solo"}, CreatedBy: creator.ID,
		}})

		// Assert
		if err != nil || ws.Subsection == nil {
			t.Fatalf("RegisterWorkspace = %+v, %v; want a workspace in the new subsection", ws, err)
		}
		if members := scalar[int](t, s, `SELECT COUNT(*) FROM workspaces WHERE subsection_id = ?`, *ws.Subsection); members != 1 {
			t.Fatalf("members = %d, want only the registered workspace", members)
		}
	})

	t.Run("born hidden and placed together", func(t *testing.T) {
		// Arrange
		s, _ := testStore(t)
		repoDir := arrangeRepo(t, "repo")
		creator := registerIn(t, s, repoDir, "creator", nil)
		sub := mustSubsection(t, s, creator.Repo, "auth")

		// Act
		ws, err := registerWith(t, s, repoDir, "born", RegisterFacts{Intake: true, Placement: &RegisterPlacement{Into: "auth", CreatedBy: creator.ID}})

		// Assert
		if err != nil || !ws.Hidden || ws.Subsection == nil || *ws.Subsection != sub.ID {
			t.Fatalf("RegisterWorkspace = hidden %v subsection %v, %v; want hidden and in %s", ws.Hidden, ws.Subsection, err, sub.ID)
		}
	})
}

// placementRefusalCase is one refused placement and the error it is.
type placementRefusalCase struct {
	name    string
	arrange func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error)
	check   func(t *testing.T, err error)
}

func refusalCases() []placementRefusalCase {
	newWith := func(creator Workspace, name string, gather ...string) *RegisterPlacement {
		return &RegisterPlacement{New: &NewSubsectionPlacement{Name: name, Gather: gather}, CreatedBy: creator.ID}
	}
	gatherRefused := func(want string) func(*testing.T, error) {
		return func(t *testing.T, err error) {
			var refused *GatherRefusedError
			if !errors.As(err, &refused) || refused.Why != want {
				t.Fatalf("err = %v, want a gather refusal %q", err, want)
			}
		}
	}
	return []placementRefusalCase{
		{name: "a child", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			parent := creator.ID
			return RegisterFacts{Parent: &parent, Placement: newWith(creator, "x")}, ErrPlacementOnChild
		}, check: func(t *testing.T, err error) {}},
		{name: "a merge workspace", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			return RegisterFacts{MergeOf: &MergeOrigin{Requester: creator.ID, Request: NewDetachedMergeID()}, Placement: newWith(creator, "x")}, ErrPlacementOnMerge
		}, check: func(t *testing.T, err error) {}},
		{name: "an unknown subsection", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			return RegisterFacts{Placement: &RegisterPlacement{Into: "nowhere", CreatedBy: creator.ID}}, nil
		}, check: func(t *testing.T, err error) {
			var unknown *UnknownPlacementSubsectionError
			if !errors.As(err, &unknown) || unknown.Name != "nowhere" {
				t.Fatalf("err = %v, want an unknown subsection error naming %q", err, "nowhere")
			}
		}},
		{name: "a subsection of another repository", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			other := registerIn(t, s, arrangeRepo(t, "other"), "elsewhere", nil)
			mustSubsection(t, s, other.Repo, "foreign")
			return RegisterFacts{Placement: &RegisterPlacement{Into: "foreign", CreatedBy: creator.ID}}, nil
		}, check: func(t *testing.T, err error) {
			var unknown *UnknownPlacementSubsectionError
			if !errors.As(err, &unknown) {
				t.Fatalf("err = %v, want an unknown subsection error", err)
			}
		}},
		{name: "a blank new subsection name", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			return RegisterFacts{Placement: newWith(creator, "  ")}, ErrBlankSubsectionName
		}, check: func(t *testing.T, err error) {}},
		{name: "an unknown gathered workspace", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			return RegisterFacts{Placement: newWith(creator, "x", "ghost")}, nil
		}, check: func(t *testing.T, err error) {
			var unknown *GatherUnknownError
			if !errors.As(err, &unknown) || unknown.Name != "ghost" {
				t.Fatalf("err = %v, want an unknown gather error naming %q", err, "ghost")
			}
		}},
		{name: "a gathered workspace of another repository", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			registerIn(t, s, arrangeRepo(t, "other"), "elsewhere", nil)
			return RegisterFacts{Placement: newWith(creator, "x", "elsewhere")}, nil
		}, check: func(t *testing.T, err error) {
			var unknown *GatherUnknownError
			if !errors.As(err, &unknown) {
				t.Fatalf("err = %v, want an unknown gather error", err)
			}
		}},
		{name: "a closed gathered workspace", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			gone := registerIn(t, s, repoDir, "gone", nil)
			closeWorkspace(t, s, gone.ID)
			return RegisterFacts{Placement: newWith(creator, "x", "gone")}, nil
		}, check: gatherRefused("it is closed")},
		{name: "a child gathered", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			registerChild(t, s, repoDir, "kid", creator.ID)
			return RegisterFacts{Placement: newWith(creator, "x", "kid")}, nil
		}, check: gatherRefused("it is a child, which sits wherever its owner does")},
		{name: "a gathered workspace already in a subsection", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			filed := registerIn(t, s, repoDir, "filed", nil)
			sub := mustSubsection(t, s, filed.Repo, "taken")
			sid := sub.ID
			if err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{filed.ID}, &sid, AtEnd{}); err != nil {
				t.Fatalf("PlaceWorkspaces: %v", err)
			}
			return RegisterFacts{Placement: newWith(creator, "x", "filed")}, nil
		}, check: gatherRefused("it is already in a subsection")},
		{name: "a main workspace gathered", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			if err := s.Reopen(context.Background(), registerMain(t, s, repoDir).ID, "test-instance"); err != nil {
				t.Fatalf("Reopen the main workspace: %v", err)
			}
			return RegisterFacts{Placement: newWith(creator, "x", MainWorkspaceName(filepath.Base(repoDir)))}, nil
		}, check: gatherRefused("it is a main workspace, which is never sorted by an agent")},
		{name: "a workspace gathered twice", arrange: func(t *testing.T, s *store, repoDir string, creator Workspace) (RegisterFacts, error) {
			registerIn(t, s, repoDir, "twice", nil)
			return RegisterFacts{Placement: newWith(creator, "x", "twice", "twice")}, nil
		}, check: gatherRefused("it is named twice")},
	}
}

func TestRegisterRefusesAPlacementAndWritesNothing(t *testing.T) {
	for _, tt := range refusalCases() {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			creator := registerIn(t, s, repoDir, "creator", nil)
			facts, wantErr := tt.arrange(t, s, repoDir, creator)
			facts.Intake = true
			workspacesBefore, subsectionsBefore := workspaceCount(t, s), subsectionCount(t, s)
			intakeBefore := scalar[int](t, s, `SELECT COUNT(*) FROM intake_workspaces`)

			// Act
			_, err := registerWith(t, s, repoDir, "born", facts)

			// Assert
			if err == nil || !IsPlacementRefusal(err) || (wantErr != nil && !errors.Is(err, wantErr)) {
				t.Fatalf("RegisterWorkspace error = %v, want the placement refusal %v", err, wantErr)
			}
			tt.check(t, err)
			if got := workspaceCount(t, s); got != workspacesBefore {
				t.Fatalf("workspaces = %d, want %d: a refused placement writes no row", got, workspacesBefore)
			}
			if got := subsectionCount(t, s); got != subsectionsBefore {
				t.Fatalf("subsections = %d, want %d: a refused placement creates no subsection", got, subsectionsBefore)
			}
			if got := scalar[int](t, s, `SELECT COUNT(*) FROM intake_workspaces`); got != intakeBefore {
				t.Fatalf("intake rows = %d, want %d: a refused placement leaves no intake mark", got, intakeBefore)
			}
			record, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelDebug, Operation: "daemon.wsm.register_workspace", Message: "the placement was refused; nothing was written"})
			if !ok {
				t.Fatalf("no DEBUG daemon.wsm.register_workspace record of the refusal; records %+v", logs.Records())
			}
			if _, errored := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.register_workspace"}); errored {
				t.Fatalf("the refusal was recorded at ERROR; record %+v", record)
			}
		})
	}
}

func TestRefusePlacementAnswersTheSameRefusalsBeforeAnythingIsMade(t *testing.T) {
	for _, tt := range refusalCases() {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			creator := registerIn(t, s, repoDir, "creator", nil)
			facts, wantErr := tt.arrange(t, s, repoDir, creator)

			// Act
			err := s.RefusePlacement(context.Background(), repoDir, *facts.Placement, facts.Parent, facts.MergeOf != nil)

			// Assert
			if err == nil || !IsPlacementRefusal(err) || (wantErr != nil && !errors.Is(err, wantErr)) {
				t.Fatalf("RefusePlacement = %v, want the placement refusal %v", err, wantErr)
			}
			tt.check(t, err)
			if _, errored := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.refuse_placement"}); errored {
				t.Fatalf("the refusal was recorded at ERROR; records %+v", logs.Records())
			}
		})
	}
}

func TestRefusePlacementAcceptsAGatherableCreate(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	creator := registerIn(t, s, repoDir, "creator", nil)
	registerIn(t, s, repoDir, "mate", nil)

	// Act
	err := s.RefusePlacement(context.Background(), repoDir, RegisterPlacement{New: &NewSubsectionPlacement{Name: "x", Gather: []string{"mate"}}, CreatedBy: creator.ID}, nil, false)

	// Assert
	if err != nil {
		t.Fatalf("RefusePlacement = %v, want nil", err)
	}
	if got := subsectionCount(t, s); got != 0 {
		t.Fatalf("subsections = %d, want the check to write none", got)
	}
}

func TestRefusePlacementFailsOnAnUnknownRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.RefusePlacement(context.Background(), arrangeRepo(t, "nowhere"), RegisterPlacement{Into: "x"}, nil, false)

	// Assert
	if !errors.Is(err, ErrUnknownRepository) {
		t.Fatalf("RefusePlacement = %v, want ErrUnknownRepository", err)
	}
}

func TestAPlacementNamingBothOrNeitherTargetPanics(t *testing.T) {
	tests := []struct {
		name      string
		placement RegisterPlacement
	}{
		{name: "neither", placement: RegisterPlacement{}},
		{name: "both", placement: RegisterPlacement{Into: "a", New: &NewSubsectionPlacement{Name: "b"}, CreatedBy: "ws"}},
		{name: "a new subsection with no creator", placement: RegisterPlacement{New: &NewSubsectionPlacement{Name: "b"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatalf("checkPlacement(%+v) did not panic", tt.placement)
				}
			}()

			// Act
			_, _ = checkPlacement(context.Background(), nil, "repo", tt.placement, nil, false)
		})
	}
}
