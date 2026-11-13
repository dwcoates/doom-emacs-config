package merge

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// opStanding is the operation every record of the queue's standing is filed
// under.
const opStanding = "daemon.merge.standing"

// queueHeld reports whether REPO's queue may advance nothing now: it is
// paused, or this daemon may not advance its queues across a handover
// (Standing.ServesQueue). It is THE ONE ANSWER every step that advances a
// queue asks -- an admission, a chain, a resume, a drain, a waiting rebase --
// so none of them can advance a queue another of them holds.
func (o *orchestrator) queueHeld(ctx context.Context, repo wsm.RepoKey) (bool, error) {
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		return false, err
	}
	return paused || !o.deps.Standing.ServesQueue(), nil
}

// FollowStanding implements Orchestrator.
//
// A HANDOVER HOLDS EVERY QUEUE (owner ruling, 2026-11-13). The incumbent's
// answer turns false the moment it claims the rollout: every running merge is
// suspended at its step exactly as a pause suspends it, and nothing is
// admitted, so no merge lands while the daemons trade workspaces. The
// successor's turns true once it is up: every queue is kicked, and each merge
// is admitted once its own workspace is served here (admitFront), without
// waiting for the rest. An abandoned handover gives the incumbent its queues
// back the same way.
func (o *orchestrator) FollowStanding(ctx context.Context) error {
	last := o.deps.Standing.ServesQueue()
	o.deps.Log.Global().Info(opStanding, "the queue's standing is followed", dlog.Context{"serves": last})
	for {
		var changed <-chan struct{}
		last, changed = o.followStandingOnce(ctx, last)
		select {
		case <-ctx.Done():
			return nil
		case <-changed:
		}
	}
}

// followStandingOnce takes the standing as it is now against LAST, the
// answer last acted on, and answers the one it acts on from here and the edge
// its next change closes. The edge is read BEFORE the answer, so a change
// between the two closes the edge rather than being missed.
func (o *orchestrator) followStandingOnce(ctx context.Context, last bool) (bool, <-chan struct{}) {
	changed := o.deps.Standing.QueueChanged()
	serves := o.deps.Standing.ServesQueue()
	if serves != last {
		o.standingMoved(ctx, serves)
	}
	return serves, changed
}

// standingMoved holds or takes on every queue as SERVES says.
func (o *orchestrator) standingMoved(ctx context.Context, serves bool) {
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		o.deps.Log.Global().Error(opStanding, "could not read the queues whose standing moved", dlog.Context{"serves": serves, "error": err.Error()})
		return
	}
	repos := make([]wsm.RepoKey, 0, len(queues))
	for repo := range queues {
		repos = append(repos, repo)
	}
	if !serves {
		o.deps.Log.Global().Info(opStanding, "a handover holds every queue: running merges are suspended at their step and nothing is admitted", dlog.Context{"repos": len(repos)})
		if err := o.suspendForPause(ctx, repos); err != nil {
			o.deps.Log.Global().Error(opStanding, "could not suspend every running merge for the handover", dlog.Context{"error": err.Error()})
		}
		return
	}
	o.deps.Log.Global().Info(opStanding, "this daemon may advance its queues again; each merge is admitted once its workspace is served here", dlog.Context{"repos": len(repos)})
	for _, repo := range repos {
		o.kick(repo)
	}
}
