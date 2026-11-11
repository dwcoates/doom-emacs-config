package main

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/slackwatch"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

func TestCreateSlackIntakeCreatesAHiddenPeerOfTheRepository(t *testing.T) {
	// Arrange
	var got workspace.CreateSpec
	intakes := slackIntakes{
		create: func(_ context.Context, spec workspace.CreateSpec) (wsm.Workspace, error) {
			got = spec
			return wsm.Workspace{}, nil
		},
		repositories: func(context.Context) ([]wsm.Repository, error) {
			return []wsm.Repository{{ID: "repo-1", Dir: "/repos/ee"}}, nil
		},
	}

	// Act
	err := intakes.CreateSlackIntake(context.Background(), slackwatch.IntakeRequest{RepoID: "repo-1", Name: "slack-intake-agent-ack-1-2", Prompt: "read the thread"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got.RepoDir != "/repos/ee" || !got.Hidden || got.Name != "slack-intake-agent-ack-1-2" || got.InitialPrompt != "read the thread" ||
		got.Originator != lifecycleconfig.OriginatorUserOrWorkspace || got.Landing == nil || got.Landing.Into != nil || got.Parent != nil || got.ForkFrom != nil {
		t.Fatalf("created %+v", got)
	}
}

func TestCreateSlackIntakeRefusesAnUnregisteredRepository(t *testing.T) {
	// Arrange
	created := false
	intakes := slackIntakes{
		create: func(context.Context, workspace.CreateSpec) (wsm.Workspace, error) {
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
		create: func(context.Context, workspace.CreateSpec) (wsm.Workspace, error) { return wsm.Workspace{}, boom },
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
