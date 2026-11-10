package wsm

import (
	"context"
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
		withParent      bool
		withContextFrom bool
		withRequester   bool
	}{
		{name: "a child forked from a third workspace, asked for by its owner", withParent: true, withContextFrom: true, withRequester: true},
		{name: "a child forked from its owner", withParent: true, withContextFrom: true},
		{name: "a top-level workspace with a fresh context the user asked for"},
		{name: "a top-level fork asked for by a workspace", withContextFrom: true, withRequester: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "owner", "source", "asker")
			var facts RegisterFacts
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
			if (got.ContextFrom != nil) != tc.withContextFrom || (tc.withContextFrom && *got.ContextFrom != f.byName["source"].ID) {
				t.Errorf("ContextFrom = %v, want source %v", got.ContextFrom, tc.withContextFrom)
			}
			if (got.RequestedBy != nil) != tc.withRequester || (tc.withRequester && *got.RequestedBy != f.byName["asker"].ID) {
				t.Errorf("RequestedBy = %v, want asker %v", got.RequestedBy, tc.withRequester)
			}
		})
	}
}

// TestScanWorkspaceReadsALegacyOwnerLandsRow pins that the inert owner_lands
// column is read by nothing: a row an earlier build wrote with it set, even
// with no parent, decodes like any other.
func TestScanWorkspaceReadsALegacyOwnerLandsRow(t *testing.T) {
	tests := []struct {
		name       string
		withParent bool
	}{
		{name: "a legacy child row its owner once landed", withParent: true},
		{name: "a legacy row set with no parent"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "owner")
			var facts RegisterFacts
			if tc.withParent {
				id := f.byName["owner"].ID
				facts.Parent = &id
			}
			ws, err := registerDerived(t, s, f.repoDir, "derived", facts)
			if err != nil {
				t.Fatalf("RegisterWorkspace: %v", err)
			}
			corrupt(t, s, `UPDATE workspaces SET owner_lands = 1 WHERE id = ?`, ws.ID)

			// Act
			got, err := s.Workspace(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.ID != ws.ID || (got.Parent != nil) != tc.withParent {
				t.Errorf("Workspace = %s parent %v, want %s with parent %v", got.ID, got.Parent, ws.ID, tc.withParent)
			}
		})
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
