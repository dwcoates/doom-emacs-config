// Package selectionfollow keeps this daemon's roster on the selection WSM
// stores when ANOTHER daemon stamped it.
//
// A HANDOVER RUNS TWO DAEMONS, EACH SERVING A ROSTER, and the rollout has no
// daemon-to-daemon channel (internal/rollout). A select lands on the daemon
// that owns the workspace, which stamps the selection into the state file both
// share and pushes its own roster; a client watching the OTHER daemon's
// roster heard nothing and stayed on the old selection for as long as the
// handover lasted. The state file is the channel: SQLite's data_version moves
// on this daemon's one connection exactly when another process commits, so the
// follower watches it and, on a move, asks the workspace verbs to bring the
// roster in line with the stored selection (workspace.Verbs.
// FollowStoredSelection), which pushes only when the two disagree.
package selectionfollow

import (
	"context"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
)

const (
	opRun   = "daemon.selectionfollow.run"
	opCheck = "daemon.selectionfollow.check"

	// DefaultEvery is how often the state file's data_version is read. A
	// switch made on the other daemon reaches this roster within it, on top
	// of the select's own round trip, and one PRAGMA read on the state
	// connection is all a quiet tick costs.
	DefaultEvery = 50 * time.Millisecond
)

// Versions is the state file's commit signal (wsm.DB.DataVersion).
type Versions interface {
	DataVersion(ctx context.Context) (int64, error)
}

// Follow brings the roster's selection in line with the stored one
// (workspace.Verbs.FollowStoredSelection). It records its own failures.
type Follow func(ctx context.Context) error

// Deps are the follower's collaborators and cadence.
type Deps struct {
	// Versions is the state client.
	Versions Versions
	// Follow is the workspace verbs' follow.
	Follow Follow
	// Clock is the schedule's view of time.
	Clock clock.Clock
	// Every is the cadence between data_version reads.
	Every time.Duration
	// Log is the global logger: the selection is no one workspace's fact
	// until it is read.
	Log dlog.Logger
}

// Follower watches the state file for another daemon's commits.
type Follower struct {
	deps Deps
	// seen is the data_version the last successful follow answered for;
	// valid only once known is true. Touched only by the one goroutine Run
	// is called on.
	seen  int64
	known bool
}

// New builds the follower, refusing a missing collaborator or a non-positive
// cadence rather than running on a zero value.
func New(deps Deps) (*Follower, error) {
	switch {
	case deps.Versions == nil:
		return nil, errors.New("selectionfollow: Versions is required")
	case deps.Follow == nil:
		return nil, errors.New("selectionfollow: Follow is required")
	case deps.Clock == nil:
		return nil, errors.New("selectionfollow: Clock is required")
	case deps.Log == nil:
		return nil, errors.New("selectionfollow: Log is required")
	case deps.Every <= 0:
		return nil, fmt.Errorf("selectionfollow: the cadence must be positive (every %v)", deps.Every)
	}
	return &Follower{deps: deps}, nil
}

// Run checks once at once, so a daemon that starts beside another converges
// before the first commit, then once every Every for the daemon's whole
// serving lifetime. A check's failures never end the loop.
func (f *Follower) Run(ctx context.Context) error {
	f.deps.Log.Debug(opRun, "the selection follower is scheduled", dlog.Context{"every": f.deps.Every.String()})
	f.Check(ctx)
	err := clock.Repeat(ctx, f.deps.Clock, f.deps.Every, f.deps.Every, f.Check)
	f.deps.Log.Debug(opRun, "the selection follower stopped with its context", nil)
	return err
}

// Check reads data_version and follows the stored selection when it moved
// since the last follow that succeeded, or when no follow has succeeded yet.
//
// A FAILED FOLLOW IS RETRIED ON THE NEXT TICK: the version it answered for is
// not recorded as seen, so a roster left behind by an unreadable registry is
// not left behind until some unrelated later commit.
func (f *Follower) Check(ctx context.Context) {
	version, err := f.deps.Versions.DataVersion(ctx)
	if err != nil {
		if ctxend.Is(err) {
			f.deps.Log.Debug(opCheck, "the data_version read ended with its context", dlog.Context{"cause": err.Error()})
			return
		}
		f.deps.Log.Error(opCheck, "could not read the state file's data_version; another daemon's selection is not followed this tick",
			dlog.Context{"cause": err.Error()})
		return
	}
	if f.known && version == f.seen {
		return
	}
	if err := f.deps.Follow(ctx); err != nil {
		f.deps.Log.Info(opCheck, "the follow did not land; the next tick retries it", dlog.Context{"cause": err.Error()})
		return
	}
	f.seen, f.known = version, true
}
