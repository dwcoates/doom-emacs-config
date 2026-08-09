package merge

import (
	"context"
	"errors"
	"fmt"
	"sort"
)

// This file is the coordinator's DEQUEUE surface: where a workspace's merge
// stands on its queue, and how a user's answered dequeue offer takes it off.
//
// It sits beside Evict rather than replacing it. Evict is the pure queue
// operation — drop a waiting entry nobody has been handed — and it stays
// exactly as narrow as it was. What is new here is the other half the user can
// now reach: the merge that is ALREADY RUNNING, which no command on the wire
// could stop before, and which the interrupt's silent eviction deliberately
// never touched.

// dequeuedCause is the terminal `failed` cause a dequeued run carries.
//
// It is deliberately distinct from evictedCause. Both take a merge off the
// queue, but they answer different questions and a reader has to be able to
// tell which happened: an eviction is the queue half of an interrupt performed
// without asking, and a dequeue is a question the user answered yes to.
const dequeuedCause = "dequeued from the merge queue: the user answered the interrupt's dequeue offer"

// Standing is where a workspace's merge sits on its repository's queue, which
// is the whole content of the question a dequeue offer asks.
//
// IT IS A SNAPSHOT AND SAYS SO. The queue advances on its own, so a standing
// read now can be stale by the time anyone answers. That costs nothing here:
// nothing decides anything from it — the answer re-reads the world under the
// queue's own lock — and it exists solely so the card can say "2 ahead of it"
// rather than "queued".
type Standing struct {
	// Repo is the repository queue the entry sits on.
	Repo string
	// RunID identifies the merge run, the same id its MergeStatus carries.
	RunID string
	// Position is the 1-based place in the repository's queue, and Depth is how
	// many entries that queue holds.
	Position int
	Depth    int
	// Head reports that this entry IS the queue's head — the merge in flight,
	// holding the shim lease. It is `Position == 1` named, so a caller never
	// re-derives the one distinction the two dequeue paths turn on.
	Head bool
}

// Ahead is how many merges are in front of this one. Zero for the head.
func (s Standing) Ahead() int { return s.Position - 1 }

// Standing reports where workspace's outstanding merge sits, and whether it has
// one at all.
//
// A WORKSPACE HAS AT MOST ONE MERGE THAT MATTERS HERE. It can in principle hold
// entries on more than one repository's queue, so the scan is over all of them
// in a stable order and takes the FIRST — the earliest-positioned entry on the
// lowest-sorted repo. That is the merge a user watching the workspace is
// watching, and a dequeue answered against it reaches every entry the workspace
// holds anyway (see QueueCoordinator.Dequeue).
func (c *QueueCoordinator) Standing(workspace string) (Standing, bool) {
	if workspace == "" {
		return Standing{}, false
	}
	snap := c.queue.Snapshot()
	repos := make([]string, 0, len(snap))
	for repo := range snap {
		repos = append(repos, repo)
	}
	sort.Strings(repos)
	for _, repo := range repos {
		reqs := snap[repo]
		for i, req := range reqs {
			if req.Workspace != workspace {
				continue
			}
			return Standing{
				Repo:     repo,
				RunID:    req.runIdentity(),
				Position: i + 1,
				Depth:    len(reqs),
				Head:     i == 0,
			}, true
		}
	}
	return Standing{}, false
}

// Dequeue takes EVERY merge this workspace has off the queue — the waiting ones
// and, if it holds a head, the one in flight — and reports how many left.
//
// IT IS THE ANSWERED OFFER'S WHOLE EFFECT, and it covers both standings on
// purpose. The offer names one standing because that is what the card shows,
// but a user answering "take my merge off the queue" means all of it; leaving a
// second entry behind because the card happened to describe the first would be
// answering a question nobody asked.
//
// THE RUNNING HALF GOES FIRST. Aborting the head releases its lease and frees
// its queue entry, and doing that before the waiting sweep means the sweep sees
// a queue the abort has already advanced rather than one it is about to.
//
// NEITHER HALF SWALLOWS THE OTHER'S FAILURE, exactly as in the interrupt that
// raises the offer: a head that could not be aborted must not cancel the
// eviction of the entries behind it, and both errors travel back joined.
func (c *QueueCoordinator) Dequeue(ctx context.Context, workspace string) (int, error) {
	if workspace == "" {
		return 0, fmt.Errorf("merge: dequeue needs a workspace")
	}
	aborted, abortErr := c.AbortRunning(ctx, workspace)
	evicted, evictErr := c.Evict(ctx, workspace)
	total := evicted
	if aborted {
		total++
	}
	c.logf("merge: DEQUEUED {ws=%s aborted_running=%t evicted_waiting=%d total=%d}", workspace, aborted, evicted, total)
	if err := errors.Join(abortErr, evictErr); err != nil {
		return total, fmt.Errorf("merge: dequeue %q: %w", workspace, err)
	}
	return total, nil
}

// AbortRunning stops the merge workspace has IN FLIGHT, if any, and reports
// whether there was one.
//
// IT CANCELS THE RUN'S CONTEXT rather than sending on a rendezvous channel, and
// that is what makes it reach a merge at any stage. Abandon — the abandonment a
// workspace close performs — rendezvouses with the park, so it completes only
// once the merge is parked on a conflict with nothing running. A dequeue cannot
// wait for that: the merge the user is trying to stop is usually mid-cherry-pick
// or inside a test suite, and those are precisely the states no rendezvous is
// being served in.
//
// IT WAITS FOR THE RUN TO UNWIND. The call returns once the run has published
// its terminal word, released its lease and dropped its durable entry — never
// merely once the cancel was delivered. An answer that returned early would ack
// a dequeue while the merge it named was still cherry-picking.
//
// WHAT THE RUN ALREADY LANDED ON THE TARGET STAYS THERE, loudly logged by the
// unwinding run itself. Every commit of a merge is replayed in a temporary
// rebase worktree and only the finished result reaches the target, so an abort
// before that point costs the target nothing at all; an abort after it leaves
// the landed work exactly where an abandoned conflict leaves its half-resolved
// tree, for the same reason — the daemon does not destroy someone's work on one
// keystroke's behalf.
func (c *QueueCoordinator) AbortRunning(ctx context.Context, workspace string) (bool, error) {
	if workspace == "" {
		return false, fmt.Errorf("merge: abort needs a workspace")
	}
	c.mu.Lock()
	var park *conflictPark
	for _, p := range c.parks {
		if p.req.Workspace == workspace {
			park = p
			break
		}
	}
	c.mu.Unlock()
	if park == nil {
		c.logf("merge: abort found NO RUN IN FLIGHT {ws=%s} — the workspace's merges, if any, are all waiting their turn", workspace)
		return false, nil
	}
	// THE FLAG IS SET BEFORE THE CANCEL, never after. The run reads it the
	// moment its context fires to decide whether it was dequeued or the daemon
	// is going down, and those retire in opposite directions — so a cancel that
	// arrived ahead of its own flag would retire a dequeued merge as a shutdown
	// and hand the next boot a merge the user had taken off the queue.
	park.aborted.Store(true)
	c.logf("merge: abort CANCELLING the run in flight {ws=%s name=%s} — whatever it has already landed on the target STAYS, exactly as an abandoned conflict's tree does",
		workspace, park.req.Name)
	park.abort()
	select {
	case <-park.ended:
		c.logf("merge: abort COMPLETE {ws=%s name=%s} — the run published its terminal word, released its lease and left the queue", workspace, park.req.Name)
		return true, nil
	case <-ctx.Done():
		// The run is still unwinding. It WILL finish — the cancel stands and
		// nothing rescinds it — so this is a caller that stopped waiting, not a
		// dequeue that did not happen, and it says exactly that.
		c.logf("merge: abort WAIT ABANDONED {ws=%s name=%s}: %v — the cancel STANDS and the run is still unwinding; only this caller stopped waiting for it",
			workspace, park.req.Name, ctx.Err())
		return false, fmt.Errorf("merge: abort %q: stopped waiting for the run to unwind (the cancel stands): %w", workspace, ctx.Err())
	}
}

// retireDequeued publishes a dequeued run's terminal `failed` word and drops its
// durable entry.
//
// THE ENTRY GOES, unlike on a shutdown cancel of the same context. That is the
// entire difference the two cancellations make, and it is why park.aborted
// exists: a shutdown keeps the entry so the next boot resumes the merge, and a
// dequeue drops it so no boot ever does. Keeping it here would put back exactly
// what the user asked to remove.
func (c *QueueCoordinator) retireDequeued(repo string, req, driven Request, release func()) bool {
	fail := c.failRun(driven, dequeuedCause)
	c.logf("merge: run DEQUEUED {repo=%s ws=%s name=%s run=%s}", repo, req.Workspace, req.Name, driven.Run.RunID())
	return c.retireFailed(repo, req, release, fail)
}

// dequeuedCauseFor replaces a run's failure cause with the dequeue's own
// account of itself when the failure is one the user's dequeue caused.
//
// IT IS THE ONE PLACE THAT SUBSTITUTION HAPPENS, and it sits inside failRun
// rather than at the call sites for exactly that reason. A dequeue's cancel
// surfaces as an ordinary error at whichever step was running — a lease
// acquire, a before-action turn, a cherry-pick, a suite — and each of those
// reports it in its own words ("merge driver failed: context canceled"). Every
// one of them is really this, and a reader told the mechanical text instead
// would go looking for a git failure that never happened. Substituting per site
// would mean remembering to at each new one; substituting in the funnel every
// terminal `failed` word already passes through cannot be forgotten.
func (c *QueueCoordinator) dequeuedCauseFor(workspace, cause string) string {
	if cause == dequeuedCause {
		return cause
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, p := range c.parks {
		if p.req.Workspace == workspace && p.aborted.Load() {
			return dequeuedCause
		}
	}
	return cause
}
