package merge

import (
	"context"
	"fmt"
)

// This file holds the merge pipeline's THREE outbound ports beyond the ones the
// package already had (merge.ConflictResolver, merge.TestFailureResolver,
// merge.PostMergeHook). Each needs the session controller or the workspace
// creation records, which this package must not import, so each is an interface
// the server binds at stitch.

// SessionBringUp makes a workspace's agent session live.
//
// WHY A MERGE NEEDS IT AT ALL. A merge drives the merging workspace's OWN
// session — the before-action, a conflict resolution, a test fix all run there —
// and a workspace can perfectly well be hibernated when its merge comes up: the
// idle sweeper reaps anything left alone for an hour, and a merge sitting behind
// three others on a busy repository is exactly that. Taking the lease over a
// dead session and only then discovering it cannot be prompted wastes the lease
// and reports the failure a phase too late.
//
// IT RUNS BEFORE THE LEASE, and that ordering is the point: a bring-up can be
// slow, and holding the exclusivity lease across it would block the user out of
// their own session for the whole of it.
type SessionBringUp interface {
	// EnsureLive returns once ws has a live, promptable session, or with the
	// error that makes that impossible.
	//
	// A workspace whose newest session was DELETED is a hard failure: there is
	// nothing to bring up and nothing to prompt, and a merge that needs the
	// session cannot proceed without one.
	EnsureLive(ctx context.Context, ws string) error
}

// SessionDeaths answers the ONE question merge.SessionBringUp cannot: was this
// workspace's newest session destroyed ON PURPOSE?
//
// The two facts look identical from the bring-up path — neither a hibernated nor
// a deleted workspace has a live session — and they resolve in OPPOSITE
// directions. Hibernation is recoverable and a merge rehydrates it, because a
// workspace that became unmergeable by sitting idle is the one thing hibernation
// must never cost. A deletion is the user destroying the session, and bringing
// it back to run a merge action would resurrect exactly what they asked to be
// rid of.
//
// It is asked BEFORE SessionBringUp for that reason: `EnsureLive` on a deleted
// workspace would spawn the resurrection this port exists to refuse.
type SessionDeaths interface {
	// DeletedSession names the deleted session and reports the deletion, or
	// ("", false, nil) when the workspace's newest session is alive, asleep, or
	// terminal for some other reason.
	//
	// A read failure is an ERROR, never the benign answer: "the records could
	// not be read" and "the session is fine" differ by exactly the resurrection
	// this port refuses.
	DeletedSession(ws string) (sessionID string, deleted bool, err error)
}

// BeforeActionSource resolves the before_ws_merge action a workspace was created
// with, from the daemon's own creation-time record.
//
// A workspace with no action reports "", nil. An unreadable record is an ERROR:
// "there is no action" and "the record could not be read" are different answers,
// and collapsing them would silently skip an action the user asked for.
type BeforeActionSource interface {
	BeforeAction(ws string) (string, error)
}

// BeforeAction is one before_ws_merge delivery: the recorded prompt, run against
// the merging workspace's OWN session under the merge lease.
type BeforeAction struct {
	// Workspace is the merging workspace, whose session runs the action.
	Workspace string
	// RequestID correlates the delivery with its turn in the logs.
	RequestID string
	// Prompt is the recorded action text, verbatim.
	Prompt string
}

func (a BeforeAction) validate() error {
	switch {
	case a.Workspace == "":
		return fmt.Errorf("merge: before-action Workspace is required")
	case a.RequestID == "":
		return fmt.Errorf("merge: before-action RequestID is required")
	case a.Prompt == "":
		return fmt.Errorf("merge: before-action Prompt is required")
	}
	return nil
}

// BeforeActionRunner delivers a before_ws_merge action to the merging
// workspace's own session and returns once THE TURN HAS ENDED.
//
// "DONE" IS A TERMINAL TURN, NOT A TIMEOUT. The implementation rides the
// session controller's turn-lifecycle signals, exactly as
// merge.ConflictResolver and merge.TestFailureResolver do; nothing here sleeps,
// polls, or gives the agent a deadline. Anything other than a clean terminal
// turn is returned as an error, and merge.Coordinator fails the run on it —
// cherry-picking a plan the action was supposed to have changed would land the
// wrong commits.
type BeforeActionRunner interface {
	Run(ctx context.Context, act BeforeAction) error
}

// newBeforeActionRequestID mints the correlation id one before-action delivery
// carries.
func newBeforeActionRequestID() string { return "before-" + newRunID() }
