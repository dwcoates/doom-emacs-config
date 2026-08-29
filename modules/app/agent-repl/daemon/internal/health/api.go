// Package health answers DaemonHealth and SessionHealth.
//
// UNHEALTHY IS AN ANSWER, never an rpc error: a caller asking whether
// something is healthy always gets a health report back. Fault records are
// read from WSM, where they persist their resolved-at instant.
package health

import (
	"context"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/wsm"
)

// Reporter answers the two health verbs and records faults.
type Reporter interface {
	// Daemon answers DaemonHealth. An unhealthy daemon answers; it does not
	// error.
	Daemon(ctx context.Context) (*agentreplv1.DaemonHealthResponse, error)
	// Session answers SessionHealth for one workspace. An unhealthy or absent
	// session answers; it does not error.
	Session(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.SessionHealthResponse, error)
	// OpenFault records a fault and returns its id. Faults stay open until
	// they are explicitly closed.
	OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error)
	// CloseFault stamps a fault's persisted resolved-at.
	CloseFault(ctx context.Context, id ids.FaultID) error
	// OpenFaults lists the open faults in scope, for the topbar's warnings and
	// the health answers.
	OpenFaults(ctx context.Context, scope wsm.FaultScope) ([]wsm.Fault, error)
}

// Deps are the reporter's collaborators.
type Deps struct {
	// DB holds the fault records.
	DB wsm.DB
	// Live reports whether a workspace currently has a live session and its
	// link state, injected so health does not own the fleet.
	Live LiveFunc
	// Log is the reporter's logger.
	Log dlog.Surfaces
}

// LiveFunc reports one workspace's live session standing: whether a session
// exists, and whether the daemon-to-shim link is serving.
type LiveFunc func(ws ids.WorkspaceID) (exists, connected bool)

// New builds the reporter.
func New(deps Deps) (Reporter, error) {
	return nil, notimpl.Err
}
