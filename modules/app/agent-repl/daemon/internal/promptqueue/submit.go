package promptqueue

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
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

	// A COLD GATE IS ITS OWN REFUSAL, AND IT IS DECIDED BEFORE THE SESSION IS
	// LOOKED FOR. A parked workspace HAS a shim and has no watcher, so read
	// through the session lookup it answered `no_session` — a sentence about a
	// session that was up and serving, which sent three prompts to the wrong
	// explanation on 2026-09-14. The gate is the fact; it is named as the fact.
	if q.deps.ColdGate != nil {
		if detail, gated := q.deps.ColdGate(sub.WS); gated {
			refusalLevel(ctx, log, log.Info)(opSubmit, "the session is parked at its cold gate, so the submission is refused by the gate's own name",
				dlog.Context{"detail": detail})
			return Disposition{}, &ColdGateRefusal{Detail: detail}
		}
	}

	// THE DISPATCH DECISION IS TAKEN UNDER THE WORKSPACE'S DELIVERY LOCK, the
	// same lock a turn end, a lease change and the bounce registry decide
	// under. Without it a submission could start a turn in the instant after
	// the registry judged the workspace free and before the bounce began.
	d := q.lockDelivery(sub.WS)
	defer d.unlock()
	if q.isDraining(sub.WS) {
		log.Info(opSubmit, "the workspace is draining for a bounce; the submission is held for the new shim", nil)
		return q.hold(ctx, sub, "", &leaseHold{kind: wsm.HoldBuildRefresh}, log)
	}
	// A SHIM CALL IN FLIGHT IS A DELIVERY ALREADY UNDER WAY, made with this
	// lock released (call.go): the submission is held behind it, never
	// delivered beside it, and the call's settle takes it from there.
	if call, ok := q.standingCall(sub.WS); ok {
		return q.holdBehindCall(ctx, d, sub, call, log)
	}

	sender, ok := q.deps.Client(sub.WS)
	if !ok {
		// A HIBERNATED SESSION IS IDLE, NOT DEAD. The prompt is its revival,
		// exactly as mounting the frontend is; only a workspace that will not
		// come back refuses.
		if q.deps.Revive == nil {
			refusalLevel(ctx, log, log.Warn)(opSubmit, "the workspace has no session to submit to", nil)
			return Disposition{}, ErrNoSession
		}
		return q.holdForRevival(ctx, sub, log)
	}

	// A SHIM WITH NO STARTED SESSION TAKES NO PROMPT. A relaunch installs its
	// shim before it resumes, and a vendor start being retried -- or one that
	// failed -- leaves a shim with no session at all. Sent there, the prompt
	// was drawn in the feed and then refused `no_session` and lost
	// (2026-10-02). It waits under the reconnect hold, and the session's
	// coming up delivers it (ReleaseReconnectHolds).
	if sub.Target == nil && !q.deps.SessionStarted(sub.WS) {
		log.Info(opSubmit, "the workspace's shim holds no started session; the prompt is held until it reconnects", nil)
		return q.hold(ctx, sub, "", &leaseHold{kind: wsm.HoldReconnect}, log)
	}

	// A MID-SESSION VENDOR BLOCK HOLDS THE PROMPT AFTER RECONNECT, UNCLASSIFIED
	// (owner ruling, 2026-10-06; vendorblock.go), and a vendor that serves
	// again delivers the prompts it held before this one.
	if sub.Target == nil {
		// PROMPTS HELD WITH NOTHING RUNNING: the submission is the user's
		// "try now" (owner ruling, 2026-10-06; trynow.go).
		if disposition, handled, err := q.submitBehindHeld(ctx, d, sub, log); handled || err != nil {
			return disposition, err
		}
		if block, blocked := q.vendorBlocked(sub.WS); blocked {
			return q.holdForVendorBlock(ctx, sub, block, log)
		}
		q.releaseBeforeSubmit(ctx, d, log)
	}

	// A BUBBLE-ADDRESSED prompt goes to THAT agent through UpdateAgent.prompt.
	// It is not the session's turn, so it is neither classified nor held: the
	// main turn's queue has no say over a subagent's own composer.
	if sub.Target != nil {
		return q.deliverToAgent(ctx, d, sub, sender, log)
	}

	watcher, ok := q.deps.Watcher(sub.WS)
	if !ok {
		refusalLevel(ctx, log, log.Warn)(opSubmit, "the workspace has no session watcher", nil)
		return Disposition{}, ErrNoSession
	}
	if running := watcher.TurnInFlight(); running != nil {
		if *running == sub.Turn {
			q.logRepeatedStart(log, "submit", sub.Turn)
			return Disposition{Delivered: true}, nil
		}
		return q.hold(ctx, sub, *running, nil, log)
	}
	// A RECORDED CONTEXT CUT IS A RUNNING TURN even in the instant before the
	// watcher learns of it: runContextCut records the cut before it tells the
	// watcher or asks the shim, so a submission landing between the two is
	// held behind the act, never started beside it.
	if cut, ok := q.runningCut(sub.WS); ok {
		return q.hold(ctx, sub, cut.turn, nil, log)
	}
	// A SUBMISSION GOING STRAIGHT TO THE SHIM IS A DELIVERY DECISION, taken
	// under the delivery lock held since the bounce check above, where a
	// standing edit is read: a prompt submitted while an edit stands is queued
	// after the edited one and is withheld with it.
	if claim, editing := q.Editing(sub.WS); editing {
		return q.holdBehindEdit(ctx, sub, claim, log)
	}
	return q.deliver(ctx, d, sub, sender, watcher, log)
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

	if exemptFromLease(lease, sub.Origin) {
		log.Debug(opSubmit, "the lease holder's own submission takes the ordinary path", fields)
		return Disposition{}, false, nil
	}

	switch lease.Policy {
	// A RETIRED PARKED LEASE refuses as any merge lease does: nothing parks
	// any more, a build before 2026-09-30 is the only writer of one, and the
	// boot's merge recovery releases every merge lease it finds.
	case wsm.PolicyRefuse, wsm.PolicyParked:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.PolicyRefuse"})
		refusalLevel(ctx, log, log.Warn)(opSubmit, "the submission is refused: a merge is in flight", fields)
		q.noteDrainRefusal(lease.Holder, sub.WS)
		return Disposition{RefusedArm: ArmMerging}, true, ErrMerging

	case wsm.PolicyHold:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.PolicyHold"})
		kind, scheduleID, err := q.holdForLease(ctx, lease, log)
		if err != nil {
			return Disposition{}, true, err
		}
		disposition, err := q.hold(ctx, sub, "", &leaseHold{kind: kind, scheduleID: scheduleID}, log)
		// THE MERGE RELEASED ITS LEASE between the read above and the hold's
		// write, and the store refused the hold (wsm.bindMergeHold): the
		// workspace has no merge now, so the submission takes the path of a
		// workspace with none.
		if errors.Is(err, wsm.ErrMergeLeaseGone) {
			log.Info(opSubmit, "the merge released its lease as the prompt was held; the submission takes the ordinary path", fields)
			return Disposition{}, false, nil
		}
		q.noteDrainRefusal(lease.Holder, sub.WS)
		return disposition, true, err

	default:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "default"})
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
// hibernation's revival is the session-starting hold; a merge holds until it
// ends. A merge lease an older build wrote refuses and never reaches here.
func (q *queue) holdForLease(ctx context.Context, lease wsm.Lease, log dlog.Logger) (wsm.HoldKind, string, error) {
	switch lease.Holder {
	case wsm.HolderDrain:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.HolderDrain"})
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
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.HolderRestart"})
		return wsm.HoldBuildRefresh, "", nil
	case wsm.HolderHibernate:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.HolderHibernate"})
		return wsm.HoldReconnect, "", nil
	case wsm.HolderMerge:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.HolderMerge"})
		return wsm.HoldMerge, "", nil
	default:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "default"})
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
// exemptFromLease reports whether a submission of origin passes the lease
// untouched: THE ONE RULE both the submission path and the restamp of
// standing holds (OnLeaseChanged) apply.
//
// A LEASE EXCLUDES EVERYONE BUT ITS HOLDER. The merge orchestrator holds the
// lease precisely so it can drive the session, and its own briefs -- the
// conflict repair, the test repair, the configured before/after actions, the
// displaced turn's resume -- carry merge origins. Holding or refusing those
// against the merge's own lease deadlocks the merge: it submits the brief it
// is waiting on and the brief waits for the merge to end.
func exemptFromLease(lease wsm.Lease, origin conversationv1.PromptOrigin) bool {
	return lease.Holder == wsm.HolderMerge && mergeOrigin(origin)
}

// mergeOrigin reports whether an origin is one the merge orchestrator submits
// under. They are exactly conversation.v1 PromptOrigin's merge arms.
func mergeOrigin(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME:
		return true
	}
	return false
}

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

// holdForRevival parks a submission whose workspace has no live session and
// brings that session up OUT OF BAND.
//
// THE REVIVAL IS NEVER AWAITED INSIDE THE SUBMISSION. Bring-up gates on the
// shim's first healthy diagnostics (internal/workspace/sessions.go's settled
// readiness ruling), so a shim that is slow to answer them would hold the rpc
// open for exactly as long as it takes -- and the caller would be left with no
// minted TurnId, no hold in the tray, and nothing to act on. The prompt is
// held under the revival-pending hold instead, the rpc answers at once with
// the turn it minted, and the revival releases the hold when the session is
// up, delivering it down the ordinary path.
func (q *queue) holdForRevival(ctx context.Context, sub Submission, log dlog.Logger) (Disposition, error) {
	disposition, err := q.hold(ctx, sub, "", &leaseHold{kind: wsm.HoldReconnect}, log)
	if err != nil {
		return Disposition{}, err
	}
	// THE ROSTER TAKES THE TURN THE MOMENT IT IS ACCEPTED, not when the
	// revival delivers it. The rpc answers with a minted TurnId right here,
	// and sidebar.proto's `submitting` is "the turn is accepted and the shim
	// has not acked it" -- which this turn is for the whole bring-up. Left to
	// learn of it from deliverToSession, the roster read a live, idle session
	// with no turn between the shim's SessionStarted and the hold's release
	// and published `ready` for a workspace whose prompt the daemon had
	// already taken. MEASURED in a headless run's cold start: the tab walked
	// `none` -> `init` -> `ready` -> `submitting`. The link arm still
	// outranks this while the route is coming up, so the walk is now
	// `none` -> `init` -> `submitting` -> `thinking`.
	q.deps.Sidebar.SetTurn(sub.WS, &footer.TurnStarted{At: q.deps.Now(), Act: footer.ActPrompt})
	q.reviveInBackground(context.WithoutCancel(ctx), sub.WS, log)
	return disposition, nil
}

// reviveInBackground brings one workspace's session up off the request path and
// releases every revival-pending hold once it is up.
//
// EXACTLY ONE REVIVAL RUNS PER WORKSPACE: a second submission arriving while
// the first one's bring-up is still running joins its outcome rather than
// spawning a second bring-up, and its own hold is released by the same sweep.
func (q *queue) reviveInBackground(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) {
	s := q.state(ws)
	q.mu.Lock()
	if s.reviving {
		q.mu.Unlock()
		log.Debug(opSubmit, "a revival is already in flight; the hold waits on it", nil)
		return
	}
	beforeReviving := s.reviving
	s.reviving = true
	q.mu.Unlock()
	log.Debug("daemon.promptqueue.state_transition", "the workspace revival state changed", dlog.Context{
		"state": "reviving", "before": beforeReviving, "after": true,
	})
	log.Info(opSubmit, "started the background session revival", nil)

	q.reviving.Add(1)
	go func() {
		defer q.reviving.Done()
		defer func() {
			q.mu.Lock()
			before := s.reviving
			s.reviving = false
			q.mu.Unlock()
			log.Debug("daemon.promptqueue.state_transition", "the workspace revival state changed", dlog.Context{
				"state": "reviving", "before": before, "after": false,
			})
		}()
		revived, err := q.revive(ctx, ws, log)
		if err != nil {
			// THE BRING-UP FAILED, and daemon_hold.proto settles what that
			// means for the entries waiting on it: "A FAILED BRING-UP NEVER
			// DROPS THESE ENTRIES." They wait under the reconnect hold for
			// whatever brings a session up next -- a retry, a restart, a
			// revival -- and that edge delivers them (ReleaseReconnectHolds).
			q.keepReconnectHolds(ctx, ws, log, err.Error())
			return
		}
		if !revived {
			// NO REVIVAL WAS ATTEMPTED — this queue has no revival hook at
			// all. Nothing failed, so nothing is dropped: the prompt stays
			// held and the tray keeps showing it.
			log.Debug(opSubmit, "no revival hook is wired; the prompt stays held", nil)
			return
		}
		if _, ok := q.deps.Client(ws); !ok {
			// A SESSION THAT CAME UP AND HAS SINCE DIED is not a revival that
			// lied: its shim was reaped between the bring-up and this read, and
			// the departure edge (decideDeparture) owns what follows -- a
			// revival of its own, or the loop guard leaving it down until the
			// next prompt. Its holds stand for whichever brings a session up.
			// Regression, 2026-09-27: a revived shim killed the instant it came
			// up read as this ERROR (TestAShimThatDiesAgainBeforeAnyTurnEnds...).
			if watcher, served := q.deps.Watcher(ws); served {
				if departure, departed := watcher.Departed(); departed {
					log.Info(opSubmit, "the revived session came up and has since departed; its departure decides what follows", dlog.Context{
						"ordered": departure.Ordered, "cause": string(departure.Cause),
					})
					return
				}
			}
			log.Error(opSubmit, "the revival reported success but the workspace still has no session", nil)
			q.keepReconnectHolds(ctx, ws, log, "the revival reported success but no session came up")
			return
		}
		q.releaseReconnectHolds(ctx, ws, log)
	}()
}

// keepReconnectHolds records, loudly, that a FAILED bring-up leaves every
// reconnect hold on the workspace standing.
//
// A failed bring-up NEVER drops these entries (daemon_hold.proto,
// HeldPromptReconnectHold): the prompt waits for the session to reconnect, and
// whatever brings a session up next delivers it. Before 2026-10-02 the entries
// were tombstoned here, and a prompt sent to a workspace whose vendor would not
// start was lost. The bring-up's own fault is the operator's record of WHY the
// session is down; this record names the prompts that wait on it. The turn the
// roster took at acceptance is not running, so the row falls back to what the
// route reports -- the bring-up's own fault arm -- rather than standing at
// `submitting` for a prompt that waits.
func (q *queue) keepReconnectHolds(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger, cause string) {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opSubmit, "could not read the holds a failed revival leaves standing",
			dlog.Context{"cause": err.Error()})
		return
	}
	var waiting []string
	for _, h := range standing {
		if h.Tombstone != nil || h.Hold == nil || *h.Hold != wsm.HoldReconnect {
			continue
		}
		waiting = append(waiting, string(h.Turn))
	}
	q.deps.Sidebar.SetTurn(ws, nil)
	if len(waiting) == 0 {
		log.Debug(opSubmit, "the failed revival leaves no reconnect hold standing", dlog.Context{"cause": cause})
		return
	}
	log.Warn(opSubmit, "the revival failed; its prompts stay held under the reconnect hold until a session comes up",
		dlog.Context{"cause": cause, "held": len(waiting), "turns": waiting})
}

// ReleaseReconnectHolds implements Queue.
func (q *queue) ReleaseReconnectHolds(ws ids.WorkspaceID) {
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		return
	}
	q.releaseReconnectHolds(ctx, ws, log)
}

// releaseReconnectHolds un-stamps every reconnect hold on a workspace whose
// session is now up and whose vendor serves it, and classifies and delivers
// what it released (classifyReleased).
func (q *queue) releaseReconnectHolds(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) {
	// SERIALIZED AGAINST A TURN END AND A LEASE CHANGE, for the reason
	// OnTurnEnded states: all three deliver from the same standing holds.
	d := q.lockDelivery(ws)
	defer d.unlock()
	q.releaseReconnectHoldsLocked(ctx, d, log)
}

// releaseReconnectHoldsLocked is releaseReconnectHolds under the delivery
// lock the caller holds (d).
func (q *queue) releaseReconnectHoldsLocked(ctx context.Context, d *delivery, log dlog.Logger) {
	ws := d.ws
	// A VENDOR THAT STILL REFUSES THE SESSION SERVES NOTHING: the holds stand
	// until the edge on which the block stops standing (OnVendorServes).
	if block, blocked := q.vendorBlocked(ws); blocked {
		log.Info(opSubmit, "the vendor still does not serve the session; the after-reconnect holds stay standing",
			dlog.Context{"vendor_block": block})
		return
	}

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opSubmit, "could not read the holds the revival should release",
			dlog.Context{"cause": err.Error()})
		return
	}
	var released []wsm.HeldPrompt
	for _, h := range standing {
		if h.Tombstone != nil || h.Hold == nil || *h.Hold != wsm.HoldReconnect {
			continue
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, nil, ""); err != nil {
			log.Error(opSubmit, "could not release a revival-pending hold",
				dlog.Context{"turn": string(h.Turn), "cause": err.Error()})
			continue
		}
		released = append(released, h)
	}
	if len(released) == 0 {
		log.Debug(opSubmit, "the session's coming up released no reconnect hold", dlog.Context{"holds": len(standing)})
		return
	}
	log.Info(opSubmit, "the session is up and the vendor serves it; released the prompts held after reconnect", dlog.Context{"released": len(released)})
	if err := q.pushTray(ctx, ws, log); err != nil {
		return
	}
	q.classifyReleased(ctx, d, released, log)
}

// revive brings a parked session back up for a submission that found none.
// It reports whether a revival was attempted AND succeeded; a revival that
// fails is the submission's failure, never a silent refusal.
func (q *queue) revive(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) (bool, error) {
	if q.deps.Revive == nil {
		return false, nil
	}
	log.Debug(opSubmit, "no session is live; reviving the workspace for the submission", nil)
	q.noteBringUp(ws, 1, log)
	defer q.noteBringUp(ws, -1, log)
	if err := q.deps.Revive(ctx, ws); err != nil {
		log.Error(opSubmit, "the revival failed; the submission cannot be delivered",
			dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("submit to %q: revive the session: %w", ws, err)
	}
	log.Info(opSubmit, "revived the workspace's session for the submission", nil)
	return true, nil
}

// noteBringUp records one revival entering or leaving flight.
func (q *queue) noteBringUp(ws ids.WorkspaceID, delta int, log dlog.Logger) {
	s := q.state(ws)
	q.mu.Lock()
	before := s.bringUps
	s.bringUps += delta
	after := s.bringUps
	q.mu.Unlock()
	log.Debug("daemon.promptqueue.state_transition", "the workspace bring-up count changed", dlog.Context{
		"state": "bring_ups", "before": before, "after": after, "delta": delta,
	})
}

// Reviving is Queue.Reviving.
//
// A REVIVAL IS ENGAGEMENT UNTIL ITS PROMPT IS DELIVERED. The bring-up makes the
// fleet serve the session (and retires its terminal record) BEFORE it records
// the session's facts, and the revived prompt is delivered only after that, so
// for the whole window the durable session reads live, free and idle since the
// engagement before the hibernation. The sweep hibernated it there: the
// revived hold then met a dead query (`query_dead`) or no session at all
// (TestRevivalAfterHibernate, TestRevivalRunsInTheSessionsModeNotTheSummarizers).
// `reviving` is raised before the bring-up starts and lowered only after the
// released hold has been delivered, when the turn it started is in flight, so
// between the two the session is never both free and unclaimed.
func (q *queue) Reviving(ws ids.WorkspaceID) bool {
	return q.isReviving(ws)
}

// isReviving reports whether a bring-up for a workspace is running or about to
// run: either a background revival goroutine is in flight, or a revival call
// itself is. Both mean the session is still coming up.
func (q *queue) isReviving(ws ids.WorkspaceID) bool {
	s := q.state(ws)
	q.mu.Lock()
	defer q.mu.Unlock()
	return s.reviving || s.bringUps > 0
}
