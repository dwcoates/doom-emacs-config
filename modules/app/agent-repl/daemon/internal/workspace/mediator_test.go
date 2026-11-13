package workspace

import (
	"context"
	"errors"
	"testing"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

func TestTheMediatorIsMadeHiddenOnItsModelWithNoPrompt(t *testing.T) {
	// Act
	spec := mediatorSpec("/repo", "deploy-mediator")

	// Assert
	if spec.RepoDir != "/repo" || spec.Name != "deploy-mediator" || !spec.Hidden || spec.Model != mediatorModel || spec.InitialPrompt != "" {
		t.Fatalf("spec = %+v, want a hidden, unprompted deploy-mediator on %s", spec, mediatorModel)
	}
}

// mediatorFiles answers one lifecycle config, or an error.
type mediatorFiles struct {
	cfg *repoconfigv1.LifecycleConfig
	err error
}

func (m mediatorFiles) Load(string) (*repoconfigv1.LifecycleConfig, error) { return m.cfg, m.err }

func TestMediatesFrom(t *testing.T) {
	named := &repoconfigv1.LifecycleConfig{DeployMediator: &repoconfigv1.DeployMediator{Name: "deploy-mediator"}}
	cases := []struct {
		name    string
		files   mediatorFiles
		ws      wsm.Workspace
		want    bool
		wantErr bool
	}{
		{name: "the workspace its config names", files: mediatorFiles{cfg: named}, ws: wsm.Workspace{Name: "deploy-mediator", Repo: "repo-1"}, want: true},
		{name: "another workspace of that repository", files: mediatorFiles{cfg: named}, ws: wsm.Workspace{Name: "feature", Repo: "repo-1"}},
		{name: "a workspace of an unregistered repository", files: mediatorFiles{cfg: named}, ws: wsm.Workspace{Name: "deploy-mediator", Repo: "repo-gone"}},
		{name: "a repository with no lifecycle config", files: mediatorFiles{err: &lifecycleconfig.MissingError{Path: "x"}}, ws: wsm.Workspace{Name: "deploy-mediator", Repo: "repo-1"}},
		{name: "an unreadable config", files: mediatorFiles{err: errors.New("disk")}, ws: wsm.Workspace{Name: "deploy-mediator", Repo: "repo-1"}, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			db := newFakeDB()
			db.repositories = []wsm.Repository{{ID: "repo-1", Dir: "/repo"}}

			// Act
			got, err := MediatesFrom(tc.files, db)(context.Background(), tc.ws)

			// Assert
			if got != tc.want || (err != nil) != tc.wantErr {
				t.Fatalf("MediatesFrom = (%v, %v), want (%v, error %v)", got, err, tc.want, tc.wantErr)
			}
		})
	}
}
