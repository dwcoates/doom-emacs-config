package main

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/slackwatch"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

func TestCreateSlackIntakeHandsTheIntakeHelperItsBrief(t *testing.T) {
	// Arrange
	var got workspace.IntakeSpec
	intakes := slackIntakes{
		createIntake: func(_ context.Context, spec workspace.IntakeSpec) (wsm.Workspace, error) {
			got = spec
			return wsm.Workspace{}, nil
		},
		repositories: func(context.Context) ([]wsm.Repository, error) {
			return []wsm.Repository{{ID: "repo-1", Dir: "/repos/ee"}}, nil
		},
	}

	// Act
	err := intakes.CreateSlackIntake(context.Background(), slackwatch.IntakeRequest{RepoID: "repo-1", Name: "slack-intake-agent-ack-1-2", Brief: slackwatch.BriefIntake, Values: map[string]string{"link": "L"}})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got.RepoDir != "/repos/ee" || got.Name != "slack-intake-agent-ack-1-2" || got.Brief != slackwatch.BriefIntake || got.Values["link"] != "L" || got.Card != nil {
		t.Fatalf("created %+v", got)
	}
}

func TestCreateSlackIntakeRefusesAnUnregisteredRepository(t *testing.T) {
	// Arrange
	created := false
	intakes := slackIntakes{
		createIntake: func(context.Context, workspace.IntakeSpec) (wsm.Workspace, error) {
			created = true
			return wsm.Workspace{}, nil
		},
		repositories: func(context.Context) ([]wsm.Repository, error) { return nil, nil },
	}

	// Act
	err := intakes.CreateSlackIntake(context.Background(), slackwatch.IntakeRequest{RepoID: "repo-gone"})

	// Assert
	if err == nil || created {
		t.Fatalf("CreateSlackIntake = %v, created %v", err, created)
	}
}

func TestCreateSlackIntakeAnswersTheCreatesFailure(t *testing.T) {
	// Arrange
	boom := errors.New("worktree add failed")
	intakes := slackIntakes{
		createIntake: func(context.Context, workspace.IntakeSpec) (wsm.Workspace, error) { return wsm.Workspace{}, boom },
		repositories: func(context.Context) ([]wsm.Repository, error) {
			return []wsm.Repository{{ID: "repo-1", Dir: "/r"}}, nil
		},
	}

	// Act
	err := intakes.CreateSlackIntake(context.Background(), slackwatch.IntakeRequest{RepoID: "repo-1"})

	// Assert
	if !errors.Is(err, boom) {
		t.Fatalf("CreateSlackIntake = %v", err)
	}
}

// linksVerbs is a workspace.Verbs whose only live method is the links
// republish.
type linksVerbs struct {
	workspace.Verbs
	got []ids.WorkspaceID
	err error
}

func (v *linksVerbs) RepublishLinks(_ context.Context, ws ids.WorkspaceID) error {
	v.got = append(v.got, ws)
	return v.err
}

func TestSlackLinksRepublishesThroughTheBoundVerbs(t *testing.T) {
	tests := []struct {
		name string
		err  error
	}{
		{name: "a republish that succeeds", err: nil},
		{name: "a republish that fails", err: errors.New("links unreadable")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			target := &linksVerbs{err: tc.err}
			f := &verbsForwarder{}
			f.bind(target)

			// Act.
			err := slackLinks{verbs: f}.RepublishLinks(context.Background(), "ws-1")

			// Assert.
			if len(target.got) != 1 || target.got[0] != "ws-1" || !errors.Is(err, tc.err) {
				t.Fatalf("got = %v, err = %v; want one republish of ws-1 answering %v", target.got, err, tc.err)
			}
		})
	}
}

func TestSlackLinksBeforeTheVerbsIsAnError(t *testing.T) {
	// Act.
	err := slackLinks{verbs: &verbsForwarder{}}.RepublishLinks(context.Background(), "ws-1")

	// Assert.
	if err == nil || err.Error() != "agent-repld: a Slack-thread subscription's links republish before the workspace verbs existed" {
		t.Fatalf("err = %v, want the wiring defect named", err)
	}
}
