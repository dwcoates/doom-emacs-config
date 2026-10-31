package promptqueue

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/drain"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
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
	if err := promptorigin.CheckSender(sub.Origin, sub.SentBy != nil); err != nil {
		log.Error(opSubmit, "a submission reached the queue with a sender that disagrees with its origin", dlog.Context{"sender_named": sub.SentBy != nil})
		return Disposition{}, fmt.Errorf("submit to %q: %w", sub.WS, err)
	}

	// THE DISPATCH DECISION IS TAKEN UNDER THE WORKSPACE'S DELIVERY LOCK, the
	// same lock a turn end, a lease change and the bounce registry decide
	// under. Without it a submission could start a turn in the instant after
	// the registry judged the workspace free and before the bounce began.
	//
	// THE LEASE IS READ UNDER IT TOO. A stand-down takes its hibernation
	// lease and then asks, under this lock, whether a delivery is under way
	// (Delivering), so a submission either reads the lease and is held, or
	// stands its shim call before the stand-down looks and the stand-down
	// defers. Read before the lock, the lease could be taken in between: the
	// prompt went to a shim the stand-down had just hibernated and was
	// refused `query_dead` (e2e TestKeepAliveAnswerAfterVendorTurnNeverServed,
	// 2026-10-31).
	d := q.lockDelivery(sub.WS)
	defer d.unlock()

	disposition, handled, err := q.applyLeasePolicy(ctx, sub, log)
	if handled || err != nil {
		return disposition, err
	}

	// A COLD GATE IS ITS OWN REFUSAL, AND IT IS DECIDED BEFORE THE SESSION IS
	// LOOKED FOR. A parked workspace HAS a shim and has no watcher, so read
	// through the session lookup it answered `no_session` — a sentence about a
	// session that was up and serving, which sent three prompts to the wrong
	// explanation on 2026-09-14. The gate is the fact; it is named as the fact.
	//
	// A PROMPT WHOSE ORIGIN WAITS OUT THE GATE (waitsOutColdGate) is not
	// refused: it falls through to the session wait below, which holds it
	// under wsm.HoldColdGate, and the gate's answer settles it as it settles
	// every prompt held there (gatefate.go).
	if q.deps.ColdGate != nil && !waitsOutColdGate(sub.Origin) {
		if detail, gated := q.deps.ColdGate(sub.WS); gated {
			refusalLevel(ctx, log, log.Info)(opSubmit, "the session is parked at its cold gate, so the submission is refused by the gate's own name",
				dlog.Context{"detail": detail})
			return Disposition{}, &ColdGateRefusal{Detail: detail}
		}
	}

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

	// A PROMPT THAT WAITS OUT A STANDING GATE IS HELD THERE before any shim is
	// looked for: the gate runs none until it is answered (owner ruling,
	// 2026-10-27), and the answer, not a revival, is what serves it.
	if sub.Target == nil {
		if kind, why, waiting := q.sessionWait(sub.WS); waiting && kind == wsm.HoldColdGate {
			log.Info(opSubmit, "the workspace's session does not serve yet; the prompt is held until it comes up",
				dlog.Context{"hold": kind.String(), "why": why})
			return q.hold(ctx, sub, "", &leaseHold{kind: kind}, log)
		}
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

	// A SESSION THAT DOES NOT SERVE YET TAKES NO PROMPT. A relaunch installs
	// its shim before it resumes, and a vendor start being retried -- or one
	// that failed -- leaves a shim with no session at all. Sent there, the
	// prompt was drawn in the feed and then refused `no_session` and lost
	// (2026-10-02). A started session's watcher opens after the session is
	// marked started, and a prompt landing between the two was refused the
	// same way (integration TestAPromptSentRightAfterABootOverADeadShimIsDelivered,
	// 2026-10-12). It waits under the hold sessionWait names, and the session's
	// coming up -- raised only once its watcher is installed -- delivers it
	// (ReleaseReconnectHolds).
	if sub.Target == nil {
		if kind, why, waiting := q.sessionWait(sub.WS); waiting {
			log.Info(opSubmit, "the workspace's session does not serve yet; the prompt is held until it comes up",
				dlog.Context{"hold": kind.String(), "why": why})
			return q.hold(ctx, sub, "", &leaseHold{kind: kind}, log)
		}
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

// waitsOutColdGate reports whether a prompt of ORIGIN is HELD at a standing
// cold gate rather than refused by it: a prompt nobody is present to resend,
// which a refusal would drop. The gate's answer settles it as it settles every
// prompt held there (gatefate.go), so it spends nothing without the user's say.
//
//   - The merge-revert brief is queued right after the open that brings a
//     reopened merged workspace's session up, and that bring-up is exactly
//     where a large conversation parks at its gate.
//   - A detached merge's handback and a merge queue notice tell a requester
//     what became of its merges. Refused, the handback was retried at every
//     deploy and the notice was lost, each at ERROR (2026-10-17: the parked
//     `doom` workspace, every landing for an hour).
//   - A workspace subscription's notice, a derived workspace's notice and a
//     child's notice to its owner tell a workspace what another workspace
//     did, for the same reason: the daemon composed them, so nobody is there
//     to send them again.
//   - The brief asking a workspace that left its one-shot state to file
//     itself into a subsection, for the same reason.
//
// A prompt the user types is still refused, so the user knows it did not go,
// and so is one an agent sent, whose refusal its command prints.
func waitsOutColdGate(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_OWNER_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT:
		return true
	}
	return false
}

// mergeOrigin reports whether an origin is one the merge orchestrator submits
// under: conversation.v1 PromptOrigin's merge arms, and the deploy stage's
// prompt hook, which runs as the deploy's own turn while the deploy holds the
// lease (internal/merge/deploy.go's stageDeploy).
func mergeOrigin(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_HOOK:
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
	// THE STATUS TAKES THE TURN THE MOMENT IT IS ACCEPTED, not when the
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
	q.setTurn(sub.WS, &footer.TurnStarted{At: q.deps.Now(), Act: footer.ActPrompt})
	q.reviveInBackground(context.WithoutCancel(ctx), sub.WS, revivalLoad, log)
	return disposition, nil
}

// revivalCause is why a revival brings a session up: a load (a prompt, a
// lease's holds) or the relaunch of a shim that died, which badges the
// prompts held for it "after reconnect" rather than "after loading"
// (holds.Sink.SetRelaunching).
type revivalCause int

const (
	revivalLoad revivalCause = iota
	revivalRelaunch
	// revivalSettings is a model, permission-mode or effort pick on a
	// workspace with no shim: the shim it starts is stood down on the switch
	// away unless a turn runs meanwhile (Deps.SettingsStarted).
	revivalSettings
)

// reviveInBackground brings one workspace's session up off the request path and
// releases every revival-pending hold once it is up.
//
// EXACTLY ONE REVIVAL RUNS PER WORKSPACE: a second submission arriving while
// the first one's bring-up is still running joins its outcome rather than
// spawning a second bring-up, and its own hold is released by the same sweep.
func (q *queue) reviveInBackground(ctx context.Context, ws ids.WorkspaceID, cause revivalCause, log dlog.Logger) {
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
	log.Info(opSubmit, "started the background session revival", dlog.Context{"relaunch": cause == revivalRelaunch, "settings": cause == revivalSettings})
	if cause == revivalRelaunch {
		q.deps.Holds.SetRelaunching(ws, true)
	}
	if cause == revivalSettings && q.deps.SettingsStarted != nil {
		q.deps.SettingsStarted(ws, q.deps.Now())
	}

	q.reviving.Add(1)
	go func() {
		defer q.reviving.Done()
		defer func() {
			if cause == revivalRelaunch {
				q.deps.Holds.SetRelaunching(ws, false)
			}
			q.mu.Lock()
			before := s.reviving
			s.reviving = false
			q.mu.Unlock()
			log.Debug("daemon.promptqueue.state_transition", "the workspace revival state changed", dlog.Context{
				"state": "reviving", "before": before, "after": false,
			})
		}()
		revived, err := q.revive(ctx, ws, cause, log)
		if err != nil {
			// THE BRING-UP FAILED, and feed_held.proto settles what that
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
		// A REVIVAL THAT PARKED AT ITS COLD GATE HAS NO SHIM BY DESIGN: the gate
		// runs none until it is answered (owner ruling, 2026-10-27), so no
		// client is the gate's state, never a revival that lied. Its prompts
		// move under the gate's hold (sessionWait) exactly as a session's
		// would under its own.
		if q.deps.ColdGate != nil {
			if _, gated := q.deps.ColdGate(ws); gated {
				q.releaseReconnectHolds(ctx, ws, log)
				return
			}
		}
		if _, ok := q.deps.Client(ws); !ok {
			// A SESSION THAT CAME UP AND HAS SINCE DIED is not a revival that
			// lied: its shim was reaped between the bring-up and this read, and
			// the departure edge (decideDeparture) owns what follows -- a
			// revival of its own, or the loop guard leaving it down until the
			// next prompt. Its holds stand for whichever brings a session up.
			// Regression, 2026-09-27: a revived shim killed the instant it came
			// up read as this ERROR (TestAShimThatDiesAgainBeforeAnyTurnEnds...).
			// Regression, 2026-10-12: the same kill landed after the client was
			// reaped and before the watcher recorded the departure. ShimGone
			// reads the reap itself, the very fact that made Client answer
			// no session, so there is no window between the two reads.
			if watcher, served := q.deps.Watcher(ws); served && watcher.ShimGone() {
				fields := dlog.Context{"departure": "pending"}
				if departure, departed := watcher.Departed(); departed {
					fields = dlog.Context{"ordered": departure.Ordered, "cause": string(departure.Cause)}
				}
				log.Info(opSubmit, "the revived session came up and has since departed; its departure decides what follows", fields)
				return
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
// A failed bring-up NEVER drops these entries (feed_held.proto,
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
		if !standingSessionHold(h) {
			continue
		}
		waiting = append(waiting, string(h.Turn))
	}
	q.setTurn(ws, nil)
	if len(waiting) == 0 {
		log.Debug(opSubmit, "the failed revival leaves no reconnect hold standing", dlog.Context{"cause": cause})
		return
	}
	log.Warn(opSubmit, "the revival failed; its prompts stay held under the reconnect hold until a session comes up",
		dlog.Context{"cause": cause, "held": len(waiting), "turns": waiting})
}

// WithdrawRevivalTurn implements Queue.
//
// UNDER THE DELIVERY LOCK, which is what makes the stop and the delivery
// exclusive rather than a race: the revival's release un-stamps and delivers
// its holds under the same lock, and marks the delivered turn in flight on the
// session before releasing it. So either the accepted turn still waits here and
// is withdrawn, or it has already been handed to the session, this answers
// false, and the caller's kill finds it in flight.
//
// ONLY THE FIRST waiting prompt is withdrawn: it is the turn the status shows,
// and a stop stops that one turn. Prompts sent behind it stay held, the next
// of them is the accepted turn, and the status keeps showing it.
func (q *queue) WithdrawRevivalTurn(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return false, err
	}
	d := q.lockDelivery(ws)
	defer d.unlock()

	if !q.isReviving(ws) {
		log.Debug(opWithdraw, "no revival is in flight; no accepted turn waits on a bring-up", nil)
		return false, nil
	}
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opWithdraw, "could not read the holds waiting on the bring-up", dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("withdraw the revival turn on %q: %w", ws, err)
	}
	var waiting []wsm.HeldPrompt
	for _, h := range standing {
		if standingSessionHold(h) {
			waiting = append(waiting, h)
		}
	}
	if len(waiting) == 0 {
		log.Debug(opWithdraw, "the revival holds no prompt; the accepted turn has been delivered", dlog.Context{"holds": len(standing)})
		return false, nil
	}
	withdrawn := waiting[0]
	turnLog := log.With(dlog.Context{"turn": string(withdrawn.Turn)})
	if err := q.dropLocked(ctx, d, withdrawn, tombstoneWithdrawn, opWithdraw, turnLog); err != nil {
		return false, err
	}
	if len(waiting) == 1 {
		q.setTurn(ws, nil)
	}
	turnLog.Info(opWithdraw, "a stop withdrew the accepted turn before its session came up", dlog.Context{
		"still_held": len(waiting) - 1,
	})
	return true, nil
}

// AwaitTurnStarted implements TurnStops.
//
// THE QUEUE MARKS A TURN IN FLIGHT BEFORE THE SHIM HAS IT: delivery claims the
// StartTurn call, releases the delivery lock, and only then calls, and the
// call itself first waits for the main agent's watch to serve. A kill sent in
// that window reached the shim ahead of the start, found no turn open, and the
// turn then ran on unstopped. Waiting on the claim's settle orders the kill
// behind the start.
func (q *queue) AwaitTurnStarted(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	call, standing := q.standingCall(ws)
	if !standing || !call.opensTurn || call.turn != turn {
		return nil
	}
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	began := q.deps.Now()
	select {
	case <-call.settled:
		log.Info(opCall, "a turn stop waited for its turn's start to reach the shim", dlog.Context{
			"call": call.what, "turn": string(turn), "waited_ms": q.deps.Now().Sub(began).Milliseconds(),
		})
		return nil
	case <-ctx.Done():
		log.Info(opCall, "a turn stop stopped waiting for its turn's start", dlog.Context{
			"call": call.what, "turn": string(turn), "cause": ctx.Err().Error(),
		})
		return fmt.Errorf("await the start of %q on %q: %w", turn, ws, ctx.Err())
	}
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

// releaseReconnectHolds settles the holds that wait on the session of a
// workspace whose session has come up: released and delivered when the session
// serves, held under the hold that names why when it does not (sessionhold.go).
func (q *queue) releaseReconnectHolds(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) {
	// SERIALIZED AGAINST A TURN END AND A LEASE CHANGE, for the reason
	// OnTurnEnded states: all three deliver from the same standing holds.
	d := q.lockDelivery(ws)
	defer d.unlock()
	q.releaseReconnectHoldsLocked(ctx, d, log)
}

// releaseReconnectHoldsLocked is releaseReconnectHolds under the delivery
// lock the caller holds (d).
//
// IT RELEASES ONLY INTO A SESSION THAT SERVES. A revival that parks at its
// cold gate returns success with a shim and no session, and before 2026-10-10
// this released the revival's prompt into it, logged "the session is up and the
// vendor serves it", and stranded the prompt when the delivery found no watcher.
//
// IT ALSO DRIVES WHAT WAITS UNHELD. A free entry with nothing running is
// delivered here, and an unjudged free entry is classified against the turn the
// release starts: the session serving is a guaranteed edge, and an entry an
// older build left standing with no hold (the 2026-10-10 doom prompt) has no
// other one.
func (q *queue) releaseReconnectHoldsLocked(ctx context.Context, d *delivery, log dlog.Logger) {
	ws := d.ws
	// A SHIM CALL IN FLIGHT SETTLES FIRST. Its outcome may be the very hold
	// this edge exists to release (a delivery refused because the session was
	// not up yet), stamped only after the call returns; a release that ran now
	// would find nothing and the hold would wait for an edge already past. So
	// the release is owed to the call's settle and taken there.
	if q.deferToCall(d, log, "session-up release") {
		q.mu.Lock()
		d.state.releaseOwed = true
		q.mu.Unlock()
		return
	}
	// A VENDOR THAT STILL REFUSES THE SESSION SERVES NOTHING: the holds stand
	// until the edge on which the block stops standing (OnVendorServes).
	if block, blocked := q.vendorBlocked(ws); blocked {
		log.Info(opSubmit, "the vendor still does not serve the session; the after-reconnect holds stay standing",
			dlog.Context{"vendor_block": block})
		q.reportResume(ws, ResumeNotNow, "the vendor does not serve the session")
		return
	}
	// A SESSION THAT DOES NOT SERVE IS HANDED NOTHING: what waits on it is
	// held under the hold that names why, and the session coming up releases it.
	if kind, why, waiting := q.sessionWait(ws); waiting {
		moved := q.holdForSession(ctx, ws, kind, opSubmit, log)
		message := "the session does not serve yet; the prompts waiting on it stay held until it comes up"
		if kind == wsm.HoldColdGate {
			message = "the session is parked at its cold gate; the prompts waiting on it are held until the gate is answered, which a prompt never does"
		}
		log.Info(opSubmit, message, dlog.Context{"hold": kind.String(), "why": why, "moved": moved})
		q.reportResume(ws, ResumeNotNow, "the session does not serve: "+why)
		return
	}
	// THE FACTS COME FIRST: until they are in, an adopted shim's running turn
	// reads as none, and the release would deliver beside it (sessionhold.go).
	if q.factsPending(ws, "the session-up release", log, q.releaseReconnectHoldsLocked) {
		return
	}
	// WHAT A STAND-DOWN CUT RUNS FIRST (restartredrive.go): the prompts that
	// waited out the restart are released below and judged against it, as any
	// prompt that arrives while a turn runs.
	q.redriveOwedLocked(ctx, d, log)

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opSubmit, "could not read the holds the revival should release",
			dlog.Context{"cause": err.Error()})
		return
	}
	var released, unjudged []wsm.HeldPrompt
	for _, h := range standing {
		if h.Tombstone != nil || q.claimedByCall(ws, h.Turn) {
			continue
		}
		if h.Hold == nil {
			// ONLY A STRANDED ORDINARY PROMPT IS DRIVEN: no hold, no verdict,
			// ordinary delivery, not a session act. A deferred prompt and a
			// verdicted one wait for the turn's end, and a session act for what
			// is ahead of it; the session coming up is neither.
			if stranded(h) {
				unjudged = append(unjudged, h)
			}
			continue
		}
		if !standingSessionHold(h) {
			continue
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, nil, ""); err != nil {
			log.Error(opSubmit, "could not release a revival-pending hold",
				dlog.Context{"turn": string(h.Turn), "cause": err.Error()})
			continue
		}
		released = append(released, h)
	}
	_, running := q.runningTurn(ws)
	if len(released) == 0 && len(unjudged) == 0 {
		log.Debug(opSubmit, "the session's coming up released no reconnect hold", dlog.Context{"holds": len(standing)})
		return
	}
	if len(released) > 0 {
		log.Info(opSubmit, "the session is up and the vendor serves it; released the prompts held after reconnect", dlog.Context{"released": len(released)})
		if err := q.pushTray(ctx, ws, log); err != nil {
			return
		}
	}
	if len(unjudged) > 0 {
		log.Info(opSubmit, "stranded prompts stand unheld on a session that serves; the session's serving drives them",
			dlog.Context{"stranded": len(unjudged), "running": running})
	}
	q.classifyReleased(ctx, d, append(released, unjudged...), log)
}

// revive brings a parked session back up for a submission that found none.
// It reports whether a revival was attempted AND succeeded; a revival that
// fails is the submission's failure, never a silent refusal.
func (q *queue) revive(ctx context.Context, ws ids.WorkspaceID, cause revivalCause, log dlog.Logger) (bool, error) {
	if q.deps.Revive == nil {
		return false, nil
	}
	log.Debug(opSubmit, "no session is live; reviving the workspace for the submission", nil)
	q.noteBringUp(ws, 1, log)
	defer q.noteBringUp(ws, -1, log)
	life := shimlife.StartPrompt
	switch cause {
	case revivalRelaunch:
		life = shimlife.StartRelaunch
	case revivalSettings:
		life = shimlife.StartSettings
	}
	if err := q.deps.Revive(shimlife.WithStart(ctx, life), ws); err != nil {
		log.Error(opSubmit, "the revival failed; the submission cannot be delivered",
			dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("submit to %q: revive the session: %w", ws, err)
	}
	// A REVIVAL THAT PARKS AT ITS COLD GATE IS NOT A SESSION UP. The bring-up
	// succeeded and left a shim with no session, waiting on the user's answer;
	// saying "revived" here is how the 2026-10-10 record came to read as a
	// served session.
	if q.deps.ColdGate != nil {
		if detail, gated := q.deps.ColdGate(ws); gated {
			log.Info(opSubmit, "the revival brought the shim up parked at its cold gate; the prompts wait for the gate's answer", dlog.Context{"detail": detail})
			return true, nil
		}
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

// Delivering is Queue.Delivering. It answers under the delivery lock, so it
// waits out a dispatch decision being taken and sees the shim call that
// decision stood.
func (q *queue) Delivering(ws ids.WorkspaceID) bool {
	d := q.lockDelivery(ws)
	defer d.unlock()
	_, standing := q.standingCall(ws)
	return standing
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

// ReviveForSettings starts WS's shim for an effort pick (Queue).
func (q *queue) ReviveForSettings(ctx context.Context, ws ids.WorkspaceID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	if _, shim := q.deps.Client(ws); shim {
		log.Debug(opSubmit, "the shim is up; the effort pick needs no revival", nil)
		return nil
	}
	if q.deps.Revive == nil {
		refusalLevel(ctx, log, log.Warn)(opSubmit, "the workspace has no shim and no revival is wired for an effort pick", nil)
		return ErrNoSession
	}
	q.reviveInBackground(context.WithoutCancel(ctx), ws, revivalSettings, log)
	return nil
}
