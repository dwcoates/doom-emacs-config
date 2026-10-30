package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/wsm"
)

func TestIsMainWorkspace(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture, ws wsm.Workspace)
		want    bool
		wantErr bool
	}{
		{name: "the workspace on the repository's main checkout", arrange: func(f *fixture, ws wsm.Workspace) {
			f.db.repositories = []wsm.Repository{{ID: ws.Repo, Dir: ws.Dir}}
		}, want: true},
		{name: "a linked worktree of the repository", arrange: func(*fixture, wsm.Workspace) {}, want: false},
		{name: "a repository the registry does not hold", arrange: func(f *fixture, _ wsm.Workspace) {
			f.db.repositories = nil
		}, wantErr: true},
		{name: "a registry that cannot be read", arrange: func(f *fixture, _ wsm.Workspace) {
			f.db.listRepositoriesErr = errors.New("registry gone")
		}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := f.workspace("w1", t.TempDir())
			tt.arrange(f, ws)

			// Act
			got, err := isMainWorkspace(context.Background(), f.db, ws)

			// Assert
			if (err != nil) != tt.wantErr || got != tt.want {
				t.Fatalf("isMainWorkspace = %v, %v; want %v (error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}
