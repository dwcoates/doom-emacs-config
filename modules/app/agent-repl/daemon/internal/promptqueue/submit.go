package promptqueue

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// ArmMerging is the refusal arm a submission under a merge lease answers with.
const ArmMerging = "merging"

// Submit runs one submission through the ONE delivery path: the lease policy,
// then the shim, then — when a turn is already running — the classifier and a
// hold. A hold is an answer here, never a failure.
func (q *queue) Submit(ctx context.Context, sub Submission) (Disposition, error) {
	log, err := q.logger(ctx, sub.WS)
	if err != nil {
		return Disposition{}, err
	}
	log = log.With(dlog.Context{"turn": string(sub.Turn), "origin": sub.Origin.String()})

	if sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		log.Error(opSubmit, "a submission reached the queue with no origin", nil)
		return Disposition{}, fmt.Errorf("submit to %q: the prompt origin is required", sub.WS)
	}

	disposition, handled, err := q.applyLeasePolicy(ctx, sub, log)
	if handled || err != nil {
		return disposition, err
	}

	sender, ok := q.deps.Client(sub.WS)
	if !ok {
		log.Warn(opSubmit, "the workspace has no session to submit to", nil)
		return Disposition{}, ErrNoSession
	}

	// A BUBBLE-ADDRESSED prompt goes to THAT agent through UpdateAgent.prompt.
	// It is not the session's turn, so it is neither classified nor held: the
	// main turn's queue has no say over a subagent's own composer.
	if sub.Target != nil {
		return q.deliverToAgent(ctx, sub, sender, log)
	}

	watcher, ok := q.deps.Watcher(sub.WS)
	if !ok {
		log.Warn(opSubmit, "the workspace has no session watcher", nil)
		return Disposition{}, ErrNoSession
	}
	if running := watcher.TurnInFlight(); running != nil {
		return q.hold(ctx, sub, *running, nil, log)
	}
	return q.deliver(ctx, sub, sender, watcher, log)
}

// applyLeasePolicy projects the workspace's occupancy lease onto the
// submission. The bool reports that the lease answered the submission and no
// delivery path runs.
func (q *queue) applyLeasePolicy(ctx context.Context, sub Submission, log dlog.Logger) (Disposition, bool, error) {
	lease, held, err := q.deps.DB.Lease(ctx, sub.WS)
	if err != nil {
		log.Error(opSubmit, "could not read the occupancy lease", dlog.Context{"cause": err.Error()})
		return Disposition{}, true, fmt.Errorf("read the lease for %q: %w", sub.WS, err)
	}
	if !held {
		log.Debug(opSubmit, "no lease stands; the submission takes the ordinary path", nil)
		return Disposition{}, false, nil
	}
	fields := dlog.Context{"lease": string(lease.ID), "holder": holderName(lease.Holder)}

	switch lease.Policy {
	case wsm.PolicyRefuse:
		log.Warn(opSubmit, "the submission is refused: a merge is in flight", fields)
		q.noteDrainRefusal(lease.Holder, sub.WS)
		return Disposition{RefusedArm: ArmMerging}, true, ErrMerging

	case wsm.PolicyParked:
		if q.deps.ParkedRoute == nil {
			log.Error(opSubmit, "a parked lease stands but no parked route is wired", fields)
			return Disposition{}, true, fmt.Errorf("route the parked submission on %q: no parked route is wired", sub.WS)
		}
		turn, err := q.deps.ParkedRoute(ctx, sub.WS, sub.Said)
		if err != nil {
			log.Error(opSubmit, "the parked route refused the submission",
				merged(fields, dlog.Context{"cause": err.Error()}))
			return Disposition{}, true, fmt.Errorf("route the parked submission on %q: %w", sub.WS, err)
		}
		log.Info(opSubmit, "routed the submission to the resolution agent as guidance",
			merged(fields, dlog.Context{"guidance_turn": string(turn)}))
		return Disposition{Delivered: true}, true, nil

	case wsm.PolicyHold:
		kind, scheduleID, err := q.holdForLease(ctx, lease, log)
		if err != nil {
			return Disposition{}, true, err
		}
		q.noteDrainRefusal(lease.Holder, sub.WS)
		disposition, err := q.hold(ctx, sub, "", &leaseHold{kind: kind, scheduleID: scheduleID}, log)
		return disposition, true, err

	default:
		log.Error(opSubmit, "the lease carries a policy the queue does not know", fields)
		return Disposition{}, true, fmt.Errorf("lease policy %d on %q is unknown", lease.Policy, sub.WS)
	}
}

// leaseHold is the daemon-side condition a holding lease projects.
type leaseHold struct {
	kind       wsm.HoldKind
	scheduleID string
}

// holdForLease names the hold kind a holding lease projects, and the drain
// schedule a shutdown hold waits on.
//
// RECORDED JUDGEMENT: the contract names three genuine daemon holds — shutdown
// drain, revival pending, build refresh — and four lease holders. The drain
// lease is the shutdown hold; a shim relaunch is the build-refresh bounce; a
// hibernation's revival is the session-starting hold. The merge holder never
// reaches here (its policy refuses).
func (q *queue) holdForLease(ctx context.Context, lease wsm.Lease, log dlog.Logger) (wsm.HoldKind, string, error) {
	switch lease.Holder {
	case wsm.HolderDrain:
		schedule, err := q.deps.DB.DrainSchedule(ctx)
		if err != nil {
			log.Error(opHold, "could not read the drain schedule the hold waits on",
				dlog.Context{"cause": err.Error()})
			return 0, "", fmt.Errorf("read the drain schedule: %w", err)
		}
		if schedule == nil {
			log.Error(opHold, "a drain lease stands with no schedule in force", nil)
			return 0, "", fmt.Errorf("a drain lease stands on %q with no schedule in force", lease.Workspace)
		}
		return wsm.HoldShutdown, drain.ScheduleID(*schedule), nil
	case wsm.HolderRestart:
		return wsm.HoldBuildRefresh, "", nil
	case wsm.HolderHibernate:
		return wsm.HoldSessionStarting, "", nil
	default:
		log.Error(opHold, "a holding lease names a holder with no hold kind",
			dlog.Context{"holder": holderName(lease.Holder)})
		return 0, "", fmt.Errorf("lease holder %d projects no hold kind", lease.Holder)
	}
}

// noteDrainRefusal reports one drain-refused submission to the controller,
// which rate-limits its own record.
func (q *queue) noteDrainRefusal(holder wsm.LeaseHolder, ws ids.WorkspaceID) {
	if holder == wsm.HolderDrain && q.deps.DrainRefusals != nil {
		q.deps.DrainRefusals.NoteRefusal(ws)
	}
}

// merged folds two record contexts into one, so a branch can add to the fields
// its caller already assembled without mutating them.
func merged(base, extra dlog.Context) dlog.Context {
	out := make(dlog.Context, len(base)+len(extra))
	for k, v := range base {
		out[k] = v
	}
	for k, v := range extra {
		out[k] = v
	}
	return out
}

// holderName renders a lease holder for a log record.
func holderName(h wsm.LeaseHolder) string {
	switch h {
	case wsm.HolderMerge:
		return "merge"
	case wsm.HolderRestart:
		return "restart"
	case wsm.HolderDrain:
		return "drain"
	case wsm.HolderHibernate:
		return "hibernate"
	default:
		return fmt.Sprintf("holder(%d)", h)
	}
}
