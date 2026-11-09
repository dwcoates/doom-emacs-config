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
// follower reads it on every commit the state file signals (wsm.DB.Changes)
// and, on a move, asks the workspace verbs to bring the
// roster in line with the stored selection (workspace.Verbs.
// FollowStoredSelection), which pushes only when the two disagree.
package selectionfollow

import (
	"context"
	"errors"

	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/kernelevent"
)

const (
	opRun   = "daemon.selectionfollow.run"
	opCheck = "daemon.selectionfollow.check"
)

// Versions is the state file's commit signal (wsm.DB.DataVersion).
type Versions interface {
	DataVersion(ctx context.Context) (int64, error)
}

// Follow brings the roster's selection in line with the stored one
// (workspace.Verbs.FollowStoredSelection). It records its own failures.
type Follow func(ctx context.Context) error

// Deps are the follower's collaborators.
type Deps struct {
	// Versions is the state client.
	Versions Versions
	// Follow is the workspace verbs' follow.
	Follow Follow
	// Changes signals every commit to the state file, this daemon's or
	// another's (wsm.DB.Changes): a read happens on a commit, never on a
	// clock, so another daemon's switch reaches this roster as it lands.
	Changes kernelevent.Source
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

// New builds the follower, refusing a missing collaborator rather than running
// on a zero value.
func New(deps Deps) (*Follower, error) {
	switch {
	case deps.Versions == nil:
		return nil, errors.New("selectionfollow: Versions is required")
	case deps.Follow == nil:
		return nil, errors.New("selectionfollow: Follow is required")
	case deps.Changes == nil:
		return nil, errors.New("selectionfollow: Changes is required")
	case deps.Log == nil:
		return nil, errors.New("selectionfollow: Log is required")
	}
	return &Follower{deps: deps}, nil
}

// Run follows the state file's commits for the daemon's whole serving
// lifetime: the watch is taken FIRST and a check made at once, so a daemon
// that starts beside another converges before the first commit and no commit
// between the two is missed. A check's failures never end the loop; a watch
// that fails does, recorded at ERROR, because nothing would wake it again.
func (f *Follower) Run(ctx context.Context) error {
	changes, stop := f.deps.Changes(ctx)
	defer stop()
	f.deps.Log.Debug(opRun, "the selection follower follows the state file's commits", nil)
	f.Check(ctx)
	for {
		select {
		case <-ctx.Done():
			f.deps.Log.Debug(opRun, "the selection follower stopped with its context", nil)
			return ctx.Err()
		case err := <-changes:
			if err != nil {
				f.deps.Log.Error(opRun, "the state file's commits could not be followed; another daemon's selection is no longer followed",
					dlog.Context{"cause": err.Error()})
				return err
			}
			f.Check(ctx)
		}
	}
}

// Check reads data_version and follows the stored selection when it moved
// since the last follow that succeeded, or when no follow has succeeded yet.
//
// A FAILED FOLLOW IS TAKEN AGAIN ON THE NEXT COMMIT: the version it answered
// for is not recorded as seen, so the next check follows whatever moved.
func (f *Follower) Check(ctx context.Context) {
	version, err := f.deps.Versions.DataVersion(ctx)
	if err != nil {
		ctxend.Log(err, f.deps.Log.Debug, f.deps.Log.Error, opCheck, "the data_version read ended with its context",
			"could not read the state file's data_version; another daemon's selection is not followed on this commit",
			dlog.Context{"cause": err.Error()})
		return
	}
	if f.known && version == f.seen {
		return
	}
	if err := f.deps.Follow(ctx); err != nil {
		f.deps.Log.Info(opCheck, "the follow did not land; the next commit takes it again", dlog.Context{"cause": err.Error()})
		return
	}
	f.seen, f.known = version, true
}
