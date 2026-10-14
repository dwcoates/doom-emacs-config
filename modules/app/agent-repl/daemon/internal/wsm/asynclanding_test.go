package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
)

func TestRegisterWorkspaceRecordsAMergeWorkspacesLandingEffects(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	dir := t.TempDir()
	origin := MergeOrigin{Requester: requester.ID, Request: NewDetachedMergeID(), ForkHead: "f00d",
		CloseOnLanding: "child", CleanupWorktree: true, CleanupDir: "/wt/fix"}

	// Act
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{
		Name: "sample-merge", Branch: "sample-merge", ParentBranch: "master", Parent: &requester.ID, RepoDir: dir,
		MergeOf: &origin,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	loaded, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil || loaded.MergeOf == nil || !reflect.DeepEqual(*loaded.MergeOf, origin) {
		t.Fatalf("MergeOf = %+v, %v; want %+v", loaded.MergeOf, err, origin)
	}
}

func TestLandingEffectsOnARowThatIsNoMergeWorkspaceAreRefused(t *testing.T) {
	tests := []struct {
		name   string
		update string
	}{
		{name: "a workspace to close", update: `UPDATE workspaces SET merge_close_on_landing = 'w' WHERE id = ?`},
		{name: "a cleanup", update: `UPDATE workspaces SET merge_cleanup_worktree = 1 WHERE id = ?`},
		{name: "a cleanup directory", update: `UPDATE workspaces SET merge_cleanup_dir = '/x' WHERE id = ?`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			corrupt(t, s, tt.update, ws.ID)

			// Act
			_, err := s.Workspace(context.Background(), ws.ID)

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Field != "merge_close_on_landing" {
				t.Fatalf("Workspace = %v, want a merge_close_on_landing decode error", err)
			}
		})
	}
}
