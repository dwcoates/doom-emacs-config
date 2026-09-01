package promptqueue

import (
	"context"
	"fmt"
	"sort"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// OnTurnEnded is the LifecycleSink's turn end: the interrupting status clears,
// the turn's close is stamped, the one-shot finish hook runs, the acts queued
// behind the turn drain, and the next prompt is popped and delivered.
func (q *queue) OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose) {
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		return
	}
	log = log.With(dlog.Context{"turn": string(turn), "close": closeName(how)})

	q.mu.Lock()
	if state, ok := q.states[ws]; ok {
		state.interrupting = false
		state.uninterruptible = 0
	}
	q.mu.Unlock()
	q.deps.Footer.SetInterrupting(ws, false)

	if err := q.deps.DB.CloseTurn(ctx, turn, q.deps.Now(), how); err != nil {
		log.Error(opTurnEnded, "could not stamp the turn's close", dlog.Context{"cause": err.Error()})
	}

	q.runFinishHook(ctx, ws, turn, how, log)
	q.drainActs(ctx, ws, log)

	if err := q.popAndDeliver(ctx, ws, log); err != nil {
		log.Error(opTurnEnded, "the next held prompt was not delivered", dlog.Context{"cause": err.Error()})
	}
}

// runFinishHook takes a one-shot workspace's finish action at the SUCCESS
// terminal.
//
// RECORDED READING: "the success marker" is the turn concluding successfully —
// CloseCompleted. A failed, killed or orphaned turn never earns the finish,
// because the brief gates the wrap-up on implementation, tests and commits all
// succeeding. The hook itself is a no-op on a workspace that owes no action, so
// the queue calls it on every successful conclusion rather than deciding which
// workspaces are one-shots.
func (q *queue) runFinishHook(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose, log dlog.Logger) {
	if q.deps.OneShotFinish == nil || how != wsm.CloseCompleted {
		return
	}
	if err := q.deps.OneShotFinish(ctx, ws, turn); err != nil {
		log.Error(opFinish, "the one-shot finish action failed", dlog.Context{"cause": err.Error()})
		return
	}
	log.Debug(opFinish, "the one-shot finish hook ran", nil)
}

// popAndDeliver delivers the next deliverable hold: the SEMANTIC HEAD an
// interjection or a release installed, else the oldest standing hold no
// daemon-side condition is holding.
func (q *queue) popAndDeliver(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) error {
	next, ok, err := q.nextDeliverable(ctx, ws)
	if err != nil {
		return err
	}
	if !ok {
		log.Debug(opTurnEnded, "nothing is waiting to be delivered", nil)
		return nil
	}
	log.Info(opTurnEnded, "delivering the next held prompt", dlog.Context{"next_turn": string(next.Turn)})
	return q.deliverHeld(ctx, ws, next, log)
}

// nextDeliverable picks the hold a turn end delivers.
func (q *queue) nextDeliverable(ctx context.Context, ws ids.WorkspaceID) (wsm.HeldPrompt, bool, error) {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return wsm.HeldPrompt{}, false, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	free := make([]wsm.HeldPrompt, 0, len(standing))
	for _, h := range standing {
		if h.Tombstone == nil && h.Hold == nil {
			free = append(free, h)
		}
	}
	if len(free) == 0 {
		return wsm.HeldPrompt{}, false, nil
	}

	q.mu.Lock()
	var head *ids.TurnID
	if state, ok := q.states[ws]; ok {
		head = state.head
	}
	q.mu.Unlock()
	if head != nil {
		for _, h := range free {
			if h.Turn == *head {
				return h, true, nil
			}
		}
	}

	sort.SliceStable(free, func(i, j int) bool { return free[i].QueuedAt.Before(free[j].QueuedAt) })
	return free[0], true, nil
}

// OnLeaseChanged re-evaluates every standing hold against the workspace's new
// lease policy: a hold the lease no longer projects is released, and a hold a
// new lease projects is stamped.
func (q *queue) OnLeaseChanged(ws ids.WorkspaceID) {
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		return
	}

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opLeaseChange, "could not read the standing holds", dlog.Context{"cause": err.Error()})
		return
	}

	lease, held, err := q.deps.DB.Lease(ctx, ws)
	if err != nil {
		log.Error(opLeaseChange, "could not read the occupancy lease", dlog.Context{"cause": err.Error()})
		return
	}
	var want *leaseHold
	if held && lease.Policy == wsm.PolicyHold {
		kind, scheduleID, err := q.holdForLease(ctx, lease, log)
		if err != nil {
			return
		}
		want = &leaseHold{kind: kind, scheduleID: scheduleID}
	}

	changed := false
	for _, h := range standing {
		if h.Tombstone != nil || sameHold(h, want) {
			continue
		}
		var kind *wsm.HoldKind
		scheduleID := ""
		if want != nil {
			k := want.kind
			kind, scheduleID = &k, want.scheduleID
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, kind, scheduleID); err != nil {
			log.Error(opLeaseChange, "could not re-stamp a hold against the new lease",
				dlog.Context{"turn": string(h.Turn), "cause": err.Error()})
			continue
		}
		changed = true
	}
	if !changed {
		log.Debug(opLeaseChange, "no standing hold changed under the new lease",
			dlog.Context{"holds": len(standing)})
		return
	}
	if err := q.pushTray(ctx, ws, log); err != nil {
		return
	}

	// A hold the lease no longer projects is deliverable the moment nothing is
	// running.
	if want != nil {
		return
	}
	if watcher, ok := q.deps.Watcher(ws); ok && watcher.TurnInFlight() != nil {
		return
	}
	if err := q.popAndDeliver(ctx, ws, log); err != nil {
		log.Error(opLeaseChange, "the released hold was not delivered", dlog.Context{"cause": err.Error()})
	}
}

// sameHold reports whether a standing hold already carries the condition the
// lease projects.
func sameHold(h wsm.HeldPrompt, want *leaseHold) bool {
	if want == nil {
		return h.Hold == nil
	}
	return h.Hold != nil && *h.Hold == want.kind && h.ScheduleID == want.scheduleID
}

// RestoreHolds reloads every standing hold at boot, ALL-OR-NOTHING, and
// reconciles the turns that were in flight when this daemon's predecessor
// stopped.
//
// RECORDED READING of the boot-reconciliation ruling: re-opening a watch is the
// session watcher's, and its adoption path learns the in-flight turn from the
// main watch's opening history page. What the QUEUE owes is the other half —
// a workspace with no session left cannot have its turn watched to completion,
// so its open turns are closed as orphans rather than left in flight forever.
func (q *queue) RestoreHolds(ctx context.Context) error {
	global := q.deps.Log.Global()

	all, err := q.deps.DB.AllHeldPrompts(ctx)
	if err != nil {
		global.Error(opRestore, "the hold restore failed; nothing was loaded",
			dlog.Context{"cause": err.Error()})
		return fmt.Errorf("restore the standing holds: %w", err)
	}

	byWorkspace := make(map[ids.WorkspaceID][]wsm.HeldPrompt)
	for _, h := range all {
		byWorkspace[h.Workspace] = append(byWorkspace[h.Workspace], h)
	}
	for ws, standing := range byWorkspace {
		q.deps.Holds.SetHeldPrompts(ws, standing)
	}
	global.Info(opRestore, "restored the standing holds", dlog.Context{
		"holds": len(all), "workspaces": len(byWorkspace),
	})

	workspaces, err := q.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		global.Error(opRestore, "could not list the workspaces to reconcile",
			dlog.Context{"cause": err.Error()})
		return fmt.Errorf("restore the standing holds: list the workspaces: %w", err)
	}
	for _, record := range workspaces {
		q.reconcileTurns(ctx, record.ID, global)
	}
	return nil
}

// reconcileTurns closes the turns of a workspace that has no session to finish
// them, and reports the ones a live session will still be watched to a close.
func (q *queue) reconcileTurns(ctx context.Context, ws ids.WorkspaceID, global dlog.Logger) {
	open, err := q.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		global.Error(opRestore, "could not read a workspace's open turns", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return
	}
	if len(open) == 0 {
		return
	}
	if _, live := q.deps.Client(ws); live {
		global.Info(opRestore, "a live session's in-flight turns are left to the watcher", dlog.Context{
			"workspace": string(ws), "open_turns": len(open),
		})
		return
	}
	report, err := q.deps.DB.CloseOrphans(ctx, ws, q.deps.Now())
	if err != nil {
		global.Error(opRestore, "could not close a dead session's in-flight turns", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return
	}
	global.Warn(opRestore, "closed the in-flight turns of a workspace with no session", dlog.Context{
		"workspace": string(ws), "orphans": len(report.Turns),
	})
}

// closeName renders a turn close for a log record.
func closeName(how sessionwatcher.TurnClose) string {
	switch how {
	case wsm.CloseCompleted:
		return "completed"
	case wsm.CloseFailed:
		return "failed"
	case wsm.CloseKilled:
		return "killed"
	case wsm.CloseOrphaned:
		return "orphaned"
	default:
		return fmt.Sprintf("close(%d)", how)
	}
}
