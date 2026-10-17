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
			facts := RegisterFacts{OwnerLands: tc.ownerLands}
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
	_, err := registerDerived(t, s, f.repoDir, "orphan", RegisterFacts{OwnerLands: true})

	// Assert
	if err == nil {
		t.Fatal("RegisterWorkspace accepted an owner-landed workspace with no owner")
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM workspaces WHERE name = 'orphan'`); n != 0 {
		t.Errorf("rows named orphan = %d, want 0", n)
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
