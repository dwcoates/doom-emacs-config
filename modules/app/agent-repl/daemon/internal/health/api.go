// Package health answers DaemonHealth and SessionHealth.
//
// UNHEALTHY IS AN ANSWER, never an rpc error: a caller asking whether
// something is healthy always gets a health report back. Fault records are
// read from WSM, where they persist their resolved-at instant.
package health

import (
	"context"
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
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
	// Now supplies the instant a fault's open and resolved marks are stamped
	// with. It is injected so a test asserts an exact instant rather than a
	// window; nil means time.Now.
	Now func() time.Time
}

// LiveFunc reports one workspace's live session standing: whether a session
// exists, and whether the daemon-to-shim link is serving.
type LiveFunc func(ws ids.WorkspaceID) (exists, connected bool)

// New builds the reporter. Every collaborator is required: a reporter with no
// state client or no liveness probe could only answer by guessing, and a health
// answer is never a guess.
func New(deps Deps) (Reporter, error) {
	if deps.DB == nil {
		return nil, fmt.Errorf("health: a state client is required")
	}
	if deps.Live == nil {
		return nil, fmt.Errorf("health: a liveness probe is required")
	}
	if deps.Log == nil {
		return nil, fmt.Errorf("health: log surfaces are required")
	}
	now := deps.Now
	if now == nil {
		now = time.Now
	}
	return &reporter{db: deps.DB, live: deps.Live, log: deps.Log, now: now}, nil
}
