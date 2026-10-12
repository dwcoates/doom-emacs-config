// Package signin carries a completed sign-in to every workspace it answers.
//
// A login made through agent-repl logs an ACCOUNT ROOT in, never one
// workspace (internal/login), so every workspace whose session spends from
// that root had the same rejected credential and is answered by the one
// sign-in. internal/agentreplsession's LoginWatch decides that a flow made a
// login (nothing parses the login TUI); this package routes that edge to each
// such workspace's feed, footer and roster, which each lift the account
// failure the sign-in answers.
package signin

import (
	"context"
	"errors"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// Feed is the feed's half: it retires the answered failure rows. Its error
// says only that recording the retirement durably failed, which the feed has
// already logged and raised.
type Feed interface {
	OnSignedIn(ws ids.WorkspaceID) error
}

// Surface is the footer's and the roster's half: each lifts the answered
// fault from the workspace status.
type Surface interface {
	OnSignedIn(ws ids.WorkspaceID)
}

// Deps is what the fan-out needs.
type Deps struct {
	// Workspaces lists every registered workspace (wsm.DB.ListWorkspaces).
	Workspaces func(ctx context.Context) ([]wsm.Workspace, error)
	// ConfigDirFor routes a workspace directory to the account root its
	// session spends from — the same routing the login flow opens under.
	ConfigDirFor func(workspaceDir string) string
	// Feed, Footer and Roster are the three surfaces a sign-in answers.
	Feed   Feed
	Footer Surface
	Roster Surface
	// Log is the daemon's logger.
	Log dlog.Logger
}

// Fanout routes a completed sign-in to the workspaces on its account root.
type Fanout struct {
	deps Deps
}

// New builds the fan-out, refusing a missing collaborator.
func New(deps Deps) (*Fanout, error) {
	switch {
	case deps.Workspaces == nil:
		return nil, errors.New("signin: a workspace lister is required")
	case deps.ConfigDirFor == nil:
		return nil, errors.New("signin: an account-root router is required")
	case deps.Feed == nil, deps.Footer == nil, deps.Roster == nil:
		return nil, errors.New("signin: the feed, the footer and the roster are all required")
	case deps.Log == nil:
		return nil, errors.New("signin: a logger is required")
	}
	return &Fanout{deps: deps}, nil
}

const opSignedIn = "daemon.signin.signed_in"

// SignedIn takes a login made through agent-repl into CONFIGDIR and tells
// every workspace on that root. A workspace list that cannot be read is
// logged at ERROR and answers no workspace: the failures stay drawn, which is
// the honest account of a sign-in nothing could route.
func (f *Fanout) SignedIn(ctx context.Context, configDir string) {
	fields := dlog.Context{"config_dir": configDir}
	workspaces, err := f.deps.Workspaces(ctx)
	if err != nil {
		fields["cause"] = err.Error()
		f.deps.Log.Error(opSignedIn, "a sign-in completed and the workspaces could not be listed; no workspace's failure was lifted", fields)
		return
	}
	var answered []string
	for _, ws := range workspaces {
		if f.deps.ConfigDirFor(ws.Dir) != configDir {
			continue
		}
		answered = append(answered, string(ws.ID))
		// THE FEED'S RECORDING ERROR IS ALREADY LOGGED AND RAISED by the
		// feed, which owns it; the removal itself has happened either way.
		_ = f.deps.Feed.OnSignedIn(ws.ID)
		f.deps.Footer.OnSignedIn(ws.ID)
		f.deps.Roster.OnSignedIn(ws.ID)
	}
	fields["workspaces"] = answered
	f.deps.Log.Info(opSignedIn, "a sign-in completed; every workspace on its account root was told", fields)
}
