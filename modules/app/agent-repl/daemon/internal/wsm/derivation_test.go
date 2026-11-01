package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
)

// registerDerived registers a workspace of repoDir with derivation facts.
func registerDerived(t *testing.T, s *store, repoDir, name string, facts RegisterFacts) (Workspace, error) {
	t.Helper()
	registerMain(t, s, repoDir)
	facts.Name, facts.Branch, facts.ParentBranch, facts.RepoDir, facts.DefaultBranch = name, name, "master", repoDir, "master"
	ws, _, err := s.RegisterWorkspace(context.Background(), mkdir(t, filepath.Join(canonicalTemp(t), name)), facts)
	return ws, err
}

func TestRegisterWorkspaceRecordsItsDerivation(t *testing.T) {
	tests := []struct {
		name            string
		ownerLands      bool
		withParent      bool
		withContextFrom bool
		withRequester   bool
	}{
		{name: "a child its owner lands, forked from a third workspace, asked for by the owner", ownerLands: true, withParent: true, withContextFrom: true, withRequester: true},
		{name: "a child that lands itself, forked from its owner", withParent: true, withContextFrom: true},
		{name: "a top-level workspace with a fresh context the user asked for"},
		{name: "a top-level fork asked for by a workspace", withContextFrom: true, withRequester: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "owner", "source", "asker")
			facts := RegisterFacts{LacksParentContext: tc.ownerLands}
			if tc.withParent {
				id := f.byName["owner"].ID
				facts.Parent = &id
			}
			if tc.withContextFrom {
				id := f.byName["source"].ID
				facts.ContextFrom = &id
			}
			if tc.withRequester {
				id := f.byName["asker"].ID
				facts.RequestedBy = &id
			}

			// Act
			ws, err := registerDerived(t, s, f.repoDir, "derived", facts)
			if err != nil {
				t.Fatalf("RegisterWorkspace: %v", err)
			}
			got, err := s.Workspace(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.OwnerLands != tc.ownerLands {
				t.Errorf("OwnerLands = %v, want %v", got.OwnerLands, tc.ownerLands)
			}
			if (got.ContextFrom != nil) != tc.withContextFrom || (tc.withContextFrom && *got.ContextFrom != f.byName["source"].ID) {
				t.Errorf("ContextFrom = %v, want source %v", got.ContextFrom, tc.withContextFrom)
			}
			if (got.RequestedBy != nil) != tc.withRequester || (tc.withRequester && *got.RequestedBy != f.byName["asker"].ID) {
				t.Errorf("RequestedBy = %v, want asker %v", got.RequestedBy, tc.withRequester)
			}
		})
	}
}

func TestRegisterWorkspaceRefusesAnOwnerLandedWorkspaceWithNoOwner(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	f := newParentFamily(t, s, "other")

	// Act
	_, err := registerDerived(t, s, f.repoDir, "orphan", RegisterFacts{LacksParentContext: true})

	// Assert
	if err == nil {
		t.Fatal("RegisterWorkspace accepted an owner-landed workspace with no owner")
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM workspaces WHERE name = 'orphan'`); n != 0 {
		t.Errorf("rows named orphan = %d, want 0", n)
	}
}

func TestRegisterWorkspaceAnswersAOneShotChildLandingItself(t *testing.T) {
	tests := []struct {
		name    string
		oneShot bool
		want    bool
	}{
		{name: "a child lacking its owner's context that is no one-shot is its owner's to land", want: true},
		{name: "a one-shot child lacking its owner's context lands itself", oneShot: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "owner")
			owner := f.byName["owner"].ID

			// Act
			ws, err := registerDerived(t, s, f.repoDir, "derived", RegisterFacts{Parent: &owner, LacksParentContext: true, OneShot: tc.oneShot})

			// Assert
			if err != nil {
				t.Fatalf("RegisterWorkspace: %v", err)
			}
			if ws.OwnerLands != tc.want || ws.OneShot != tc.oneShot {
				t.Errorf("answered OwnerLands = %v, OneShot = %v; want %v, %v", ws.OwnerLands, ws.OneShot, tc.want, tc.oneShot)
			}
		})
	}
}

// TestOwnerLandsFollowsTheOneShotMark pins that who lands a child lacking its
// owner's context is read off the one-shot mark, so each write of the mark
// moves it with nothing else written.
func TestOwnerLandsFollowsTheOneShotMark(t *testing.T) {
	// Arrange: one fresh child of its owner, with a creation job to mark.
	s, _ := testStore(t)
	f := newParentFamily(t, s, "owner")
	owner := f.byName["owner"].ID
	ws, err := registerDerived(t, s, f.repoDir, "derived", RegisterFacts{Parent: &owner, LacksParentContext: true})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, Layout: MergeLayout{SourceBranch: ws.Branch, SourceDir: ws.Dir, TargetDir: ws.Dir}}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}
	steps := []struct {
		name    string
		oneShot bool
		want    bool
	}{
		{name: "marked a one-shot, it lands itself", oneShot: true, want: false},
		{name: "unmarked, its owner lands it again", oneShot: false, want: true},
	}
	for _, step := range steps {
		t.Run(step.name, func(t *testing.T) {
			// Act
			if err := s.SetOneShot(context.Background(), ws.ID, step.oneShot); err != nil {
				t.Fatalf("SetOneShot(%v): %v", step.oneShot, err)
			}
			got, err := s.Workspace(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.OwnerLands != step.want {
				t.Errorf("OwnerLands = %v, want %v", got.OwnerLands, step.want)
			}
		})
	}
}

func TestOwnerLands(t *testing.T) {
	tests := []struct {
		name                        string
		lacksParentContext, oneShot bool
		want                        bool
	}{
		{name: "a child lacking its owner's context is its owner's to land", lacksParentContext: true, want: true},
		{name: "a one-shot lacking its owner's context lands itself", lacksParentContext: true, oneShot: true},
		{name: "a workspace carrying its owner's context lands itself"},
		{name: "a one-shot carrying its owner's context lands itself", oneShot: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := OwnerLands(tc.lacksParentContext, tc.oneShot)

			// Assert
			if got != tc.want {
				t.Errorf("OwnerLands(%v, %v) = %v, want %v", tc.lacksParentContext, tc.oneShot, got, tc.want)
			}
		})
	}
}

func TestScanWorkspaceRefusesAStoredOwnerLandedRowWithNoOwner(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET owner_lands = 1, parent_id = NULL WHERE id = ?`, ws.ID)

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "owner_lands" {
		t.Fatalf("Workspace error = %v, want a DecodeError on owner_lands", err)
	}
}

func TestForgettingADerivationSourceLeavesTheDerivedWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	f := newParentFamily(t, s, "source", "asker")
	source, asker := f.byName["source"].ID, f.byName["asker"].ID
	ws, err := registerDerived(t, s, f.repoDir, "derived", RegisterFacts{ContextFrom: &source, RequestedBy: &asker})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}

	// Act
	corrupt(t, s, `DELETE FROM creation_jobs WHERE workspace_id IN (?, ?)`, source, asker)
	corrupt(t, s, `DELETE FROM workspaces WHERE id IN (?, ?)`, source, asker)
	got, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.ContextFrom != nil || got.RequestedBy != nil {
		t.Errorf("ContextFrom = %v, RequestedBy = %v, want both nil", got.ContextFrom, got.RequestedBy)
	}
}

func TestMayActIsTheUserTheWorkspaceItsOwnerOrWhereItLands(t *testing.T) {
	owner := WorkspaceID("ws-owner")
	into := WorkspaceID("ws-into")
	child := Workspace{ID: "ws-child", Parent: &owner, LandsInto: &into}
	tests := []struct {
		name  string
		ws    Workspace
		asker WorkspaceID
		want  bool
	}{
		{name: "the user", ws: child, asker: "", want: true},
		{name: "the workspace itself", ws: child, asker: "ws-child", want: true},
		{name: "its owner", ws: child, asker: owner, want: true},
		{name: "the workspace it lands into", ws: child, asker: into, want: true},
		{name: "the workspace a peer lands into", ws: Workspace{ID: "ws-peer", LandsInto: &into}, asker: into, want: true},
		{name: "a stranger", ws: child, asker: "ws-stranger"},
		{name: "a stranger to a workspace with no owner", ws: Workspace{ID: "ws-top"}, asker: "ws-stranger"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.ws.MayAct(tt.asker)

			// Assert
			if got != tt.want {
				t.Errorf("MayAct(%q) = %v, want %v", tt.asker, got, tt.want)
			}
		})
	}
}
