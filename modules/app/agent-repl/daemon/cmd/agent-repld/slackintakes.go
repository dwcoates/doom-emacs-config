package main

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"

	"agent-repld/internal/ids"
	"agent-repld/internal/slackwatch"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// slackIntakes creates a fired Slack binding's intake workspace through the
// workspace verbs, bound after the watcher is built (slackwatch.IntakeCreator).
type slackIntakes struct {
	// createIntake is the verbs' CreateIntake, late-bound.
	createIntake func(ctx context.Context, spec workspace.IntakeSpec) (wsm.Workspace, error)
	// repositories reads the registry the request's repository is found in.
	repositories func(ctx context.Context) ([]wsm.Repository, error)
}

// CreateSlackIntake creates REQ's intake through the one intake helper
// (workspace.CreateIntake): a backlogged, fresh-context workspace of its
// repository whose first prompt is REQ's brief, retired when its turn ends.
// The intake forks itself into the workspace the binding names; it is no
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
	_, err = s.createIntake(ctx, workspace.IntakeSpec{RepoDir: repository.Dir, Name: req.Name, Brief: req.Brief, Values: req.Values})
	return err
}

// createIntakeVia is the bound verbs' CreateIntake, refused before they
// exist.
func (f *verbsForwarder) createIntakeVia(ctx context.Context, spec workspace.IntakeSpec) (wsm.Workspace, error) {
	verbs, err := f.bound("a Slack reaction's intake create")
	if err != nil {
		return wsm.Workspace{}, err
	}
	return verbs.CreateIntake(ctx, spec)
}

// slackLinks republishes a workspace's links for the Slack watcher
// (slackwatch.Deps.Links) through the late-bound workspace verbs.
type slackLinks struct{ verbs *verbsForwarder }

// RepublishLinks republishes WS's links (workspace.Verbs.RepublishLinks).
func (l slackLinks) RepublishLinks(ctx context.Context, ws ids.WorkspaceID) error {
	verbs, err := l.verbs.bound("a Slack-thread subscription's links republish")
	if err != nil {
		return err
	}
	return verbs.RepublishLinks(ctx, ws)
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
