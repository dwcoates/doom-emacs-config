package merge

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// opStanding is the operation every record of the queue's standing is filed
// under.
const opStanding = "daemon.merge.standing"

// queueHeld reports whether REPO's queue's HEAD may advance nothing now: it
// is paused, this daemon may not advance its queues across a handover
// (Standing.ServesQueue), or its deploy mediator holds it (deployHold): a
// deploy owes a check its mediator has not decided (wsm.Mediation), or the
// mediator's own remediation finish stands (wsm.RemediationFinish), from its
// first finish until its deploy owes the next check. It is THE ONE ANSWER every
// step that moves the head asks -- an admission, a resume, a drain -- so none
// of them can advance a queue another of them holds, and no order between an
// approval and a finish can let the head through while the fix is under way.
func (o *orchestrator) queueHeld(ctx context.Context, repo wsm.RepoKey) (bool, error) {
	return o.held(ctx, repo, deployHold.holdsQueue)
}

// speculationHeld reports whether REPO's queue may start no new SPECULATIVE
// work behind its head now -- a chain's extension, a waiting merge's rebase:
// it is paused, a handover holds it, a fix is dispatched for the deploy its
// check is owed of (owner ruling, 2026-11-14), or the mediator's remediation
// finish stands. While the intake only assesses, that work goes on: most
// checks approve, and stopping it would add the assessment to every merge's
// wait. Once a fix is dispatched or finishing, its landing would make that
// work stale, so none starts until it lands.
func (o *orchestrator) speculationHeld(ctx context.Context, repo wsm.RepoKey) (bool, error) {
	return o.held(ctx, repo, deployHold.holdsSpeculation)
}

// held answers whether REPO's queue is paused, held by a handover, or held
// by its deploy mediator as HOLDS judges it. A read that fails is answered,
// never taken for "not held"; the store records it.
func (o *orchestrator) held(ctx context.Context, repo wsm.RepoKey, holds func(deployHold) bool) (bool, error) {
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		return false, err
	}
	if paused || !o.deps.Standing.ServesQueue() {
		return true, nil
	}
	hold, err := o.deployHoldOf(ctx, repo)
	if err != nil {
		return false, err
	}
	return holds(hold), nil
}

// What holds a queue for its deploy mediator, as a record's held_by names it.
const (
	heldByDeployCheck       = "deploy_check"
	heldByRemediationFinish = "remediation_finish"
)

// deployHold is what holds one queue for its deploy mediator: the check its
// last deploy owes, and the mediator's remediation finish standing. It is
// read once (deployHoldOf) and answers both the hold (held) and its drawing
// (the roster's paused queue), so the two can never disagree.
//
// A QUEUE IS HELD FROM A DEPLOY'S START UNTIL THE MEDIATOR APPROVES A DEPLOY
// (owner ruling, 2026-11-22). A remediation never releases it: a finish holds
// the queue through its gate, its landing and its deploy, and sent back to
// fix, through the fixing too; its deploy owes the next check before the
// finish leaves its deploying step (OweMediations, then remediationDeployed),
// so the hold passes from the finish to that check with no instant between.
// A finish deployed is held by that check alone, and stays recorded until its
// chain ends only for the next check's brief.
type deployHold struct {
	// check is the queue's owed check, nil for none.
	check *wsm.Mediation
	// finish is the oldest remediation finish holding the queue, nil for
	// none.
	finish *wsm.RemediationFinish
}

// deployHoldOf reads what holds REPO's queue for its deploy mediator, in one
// snapshot of the store (wsm.DeployCheckHoldOf).
func (o *orchestrator) deployHoldOf(ctx context.Context, repo wsm.RepoKey) (deployHold, error) {
	read, err := o.deps.DB.DeployCheckHoldOf(ctx, repo)
	if err != nil {
		return deployHold{}, err
	}
	out := deployHold{check: read.Mediation}
	for i := range read.Finishes {
		if finishHolds(read.Finishes[i].Step) {
			out.finish = &read.Finishes[i]
			break
		}
	}
	return out, nil
}

// finishHolds reports whether a finish at STEP holds its queue: every step
// before its deploy began, the fixing a failed gate sent it back to included.
func finishHolds(step wsm.RemediationStep) bool {
	switch step {
	case wsm.RemediationGating, wsm.RemediationFixing, wsm.RemediationLanding, wsm.RemediationDeploying:
		return true
	}
	return false
}

// holdsQueue reports whether H holds the queue's head.
func (h deployHold) holdsQueue() bool { return h.check != nil || h.finish != nil }

// holdsSpeculation reports whether H holds the speculative work behind the
// head: a fix dispatched or finishing would make it stale.
func (h deployHold) holdsSpeculation() bool {
	return h.finish != nil || (h.check != nil && h.check.Dispatched)
}

// heldBy names what holds the queue, a standing finish before a check (a
// release of the check leaves the finish holding it), empty for nothing.
func (h deployHold) heldBy() string {
	switch {
	case h.finish != nil:
		return heldByRemediationFinish
	case h.check != nil:
		return heldByDeployCheck
	}
	return ""
}

// since is when H began holding the queue: its check's chain's first owed
// deploy, or its finish's first finish, whichever came first. Zero when H
// holds nothing.
func (h deployHold) since() time.Time {
	var out time.Time
	if h.check != nil {
		out = h.check.ChainStartedAt
	}
	if h.finish != nil && (out.IsZero() || h.finish.StartedAt.Before(out)) {
		out = h.finish.StartedAt
	}
	return out
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
	if serves {
		// A ROUND A HANDOVER CUT RUNS AGAIN, whatever the queues' read
		// answers: the mediation is read by the pass itself.
		defer o.resumeHandedOver(ctx)
	}
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
		if err := o.suspendHeld(ctx, repos, suspendHandover); err != nil {
			o.deps.Log.Global().Error(opStanding, "could not suspend every running merge for the handover", dlog.Context{"error": err.Error()})
		}
		return
	}
	o.deps.Log.Global().Info(opStanding, "this daemon may advance its queues again; each merge is admitted once its workspace is served here", dlog.Context{"repos": len(repos)})
	for _, repo := range repos {
		o.kick(repo)
	}
}
