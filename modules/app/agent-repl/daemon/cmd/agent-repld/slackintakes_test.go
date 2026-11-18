package main

import (
	"context"
	"errors"
	"testing"

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
