package main

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"

	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/slackwatch"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// slackIntakes creates a fired Slack binding's intake workspace through the
// workspace verbs, bound after the watcher is built (slackwatch.IntakeCreator).
type slackIntakes struct {
	// create is the verbs' Create, late-bound.
	create func(ctx context.Context, spec workspace.CreateSpec) (wsm.Workspace, error)
	// repositories reads the registry the request's repository is found in.
	repositories func(ctx context.Context) ([]wsm.Repository, error)
}

// CreateSlackIntake creates REQ's intake: a hidden peer of its repository,
// landing into the main branch, with a fresh context and REQ's prompt. The
// intake forks itself into the workspace the binding names; it is no
// emoji-ack workspace itself, since that kind belongs to the workspace that
// does the work.
func (s slackIntakes) CreateSlackIntake(ctx context.Context, req slackwatch.IntakeRequest) error {
	repositories, err := s.repositories(ctx)
	if err != nil {
		return fmt.Errorf("read the repositories: %w", err)
	}
	repository, found := wsm.RepositoryWithID(repositories, ids.RepoID(req.RepoID))
	if !found {
		return fmt.Errorf("the reaction settings' repository %s is not in the registry", req.RepoID)
	}
	_, err = s.create(ctx, workspace.CreateSpec{
		Originator:    lifecycleconfig.OriginatorUserOrWorkspace,
		RepoDir:       repository.Dir,
		Landing:       &wsm.LandingChoice{},
		InitialPrompt: req.Prompt,
		Name:          req.Name,
		Hidden:        true,
	})
	return err
}

// createVia is the bound verbs' Create, refused before they exist.
func (f *verbsForwarder) createVia(ctx context.Context, spec workspace.CreateSpec) (wsm.Workspace, error) {
	verbs, err := f.bound("a Slack reaction's intake create")
	if err != nil {
		return wsm.Workspace{}, err
	}
	return verbs.Create(ctx, spec)
}

// slackNonce mints a Slack post's nonce: 16 random hex digits, so two
// posts of one subscription never share one (slackwatch.Deps.NewNonce).
func slackNonce() (string, error) {
	var b [8]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("read a random nonce: %w", err)
	}
	return hex.EncodeToString(b[:]), nil
}
