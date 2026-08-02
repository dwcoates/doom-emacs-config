package merge

import (
	"context"
	"errors"
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
	// "PROMPTABLE" IS THE WHOLE CONTRACT, not "a process was started". The run's
	// very next act is a SEND — the lease's interrupt — so an implementation that
	// returns while the shim is still handshaking hands the merge a session it
	// cannot drive, and the lease fails on a connection that was milliseconds
	// away.
	//
	// A workspace whose newest session was DELETED is a hard failure: there is
	// nothing to bring up and nothing to prompt, and a merge that needs the
	// session cannot proceed without one.
	//
	// A workspace with NO session record at all reports ErrNoSession, which is
	// not a failure — see that sentinel.
	EnsureLive(ctx context.Context, ws string) error
}

// ErrNoSession reports that a workspace has NO session record to bring up.
//
// IT IS NOT A BRING-UP FAILURE, and conflating the two is what this sentinel
// exists to prevent. A plain worktree that nobody has ever opened a session on
// is perfectly mergeable: the cherry-pick, the test gate and the rollback are
// all git, and none of them needs an agent. Failing such a merge would make
// "has this workspace ever been opened in the editor?" a precondition of
// merging it, which it has never been.
//
// The merge therefore proceeds SESSIONLESS on it. That is not a degraded mode
// papering over an absence: the steps that genuinely need a session — the
// before-action, a conflict resolution, a test fix — each fail loudly on their
// own path when there is no session to run them, and they are the only steps
// that can tell whether the absence matters.
var ErrNoSession = errors.New("merge: the workspace has no session record to bring up")

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

// AfterAction is one postprocessing delivery: the prompt the workspace was
// CREATED with, run against its OWN session once every commit has landed.
//
// IT IS THE MERGING WORKSPACE'S SESSION, exactly as the before-action's is, and
// the render vocabulary says so: `merge_after_action` projects to MERGING
// precisely because "a merge run owns this workspace's session and nothing else
// may use it" (see internal/ssm/resolve.go). It runs under the lease the merge
// already holds, which is why it happens before the queue entry is retired.
type AfterAction struct {
	// Workspace is the merged workspace, whose session runs the action.
	Workspace string
	// RequestID correlates the delivery with its turn in the logs.
	RequestID string
	// Prompt is the recorded action text, verbatim.
	Prompt string
}

func (a AfterAction) validate() error {
	switch {
	case a.Workspace == "":
		return fmt.Errorf("merge: after-action Workspace is required")
	case a.RequestID == "":
		return fmt.Errorf("merge: after-action RequestID is required")
	case a.Prompt == "":
		return fmt.Errorf("merge: after-action Prompt is required")
	}
	return nil
}

// AfterActionRunner delivers a postprocessing action to the merged workspace's
// own session and returns once THE TURN HAS ENDED.
//
// It is the before-action's sibling in every respect but one: its failure does
// NOT fail the run. Every commit is on the target before the action starts, so
// reporting `failed` would deny a merge that demonstrably happened. The error is
// carried on the terminal merged status as after_action_error instead — visible
// without being fatal, and never swallowed.
type AfterActionRunner interface {
	Run(ctx context.Context, act AfterAction) error
}

// newAfterActionRequestID mints the correlation id one after-action delivery
// carries.
func newAfterActionRequestID() string { return "after-" + newRunID() }
