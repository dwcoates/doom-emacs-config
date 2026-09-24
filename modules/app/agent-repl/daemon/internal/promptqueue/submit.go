package promptqueue

import (
	"context"
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
			log.Info(opSubmit, "the session is parked at its cold gate, so the submission is refused by the gate's own name",
				dlog.Context{"detail": detail})
			return Disposition{}, &ColdGateRefusal{Detail: detail}
		}
	}

	sender, ok := q.deps.Client(sub.WS)
	if !ok {
		// A HIBERNATED SESSION IS IDLE, NOT DEAD. The prompt is its revival,
		// exactly as mounting the frontend is; only a workspace that will not
		// come back refuses.
		if q.deps.Revive == nil {
			log.Warn(opSubmit, "the workspace has no session to submit to", nil)
			return Disposition{}, ErrNoSession
		}
		return q.holdForRevival(ctx, sub, log)
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
	// A SUBMISSION GOING STRAIGHT TO THE SHIM IS A DELIVERY DECISION, so it is
	// taken under the delivery lock, where a standing edit is read: a prompt
	// submitted while an edit stands is queued after the edited one and is
	// withheld with it.
	drain := &q.state(sub.WS).drain
	drain.Lock()
	defer drain.Unlock()
	if claim, editing := q.Editing(sub.WS); editing {
		return q.holdBehindEdit(ctx, sub, claim, log)
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

	// A LEASE EXCLUDES EVERYONE BUT ITS HOLDER. The merge orchestrator holds
	// the lease precisely so it can drive the session, and its own briefs --
	// the conflict repair, the test repair, the configured before/after
	// actions, the displaced turn's resume -- carry merge origins. Refusing
	// those against the merge's own lease deadlocks the merge: it submits the
	// brief it is waiting on and is told a merge is in flight.
	if lease.Holder == wsm.HolderMerge && mergeOrigin(sub.Origin) {
		log.Debug(opSubmit, "the lease holder's own submission takes the ordinary path", fields)
		return Disposition{}, false, nil
	}

	switch lease.Policy {
	case wsm.PolicyRefuse:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.PolicyRefuse"})
		log.Warn(opSubmit, "the submission is refused: a merge is in flight", fields)
		q.noteDrainRefusal(lease.Holder, sub.WS)
		return Disposition{RefusedArm: ArmMerging}, true, ErrMerging

	case wsm.PolicyParked:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.PolicyParked"})
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
		// THE GUIDANCE IS STILL SOMETHING THE USER TYPED, so it is drawn like
		// every other accepted prompt -- at the session's standing output
		// address, which the parked lease holder has pointed at its own tab, so
		// the guidance lands there and never on the root feed.
		q.mirrorAccepted(sub.WS, sub.Turn, sub.Said, sub.Origin)
		log.Info(opSubmit, "routed the submission to the resolution agent as guidance",
			merged(fields, dlog.Context{"guidance_turn": string(turn)}))
		return Disposition{Delivered: true}, true, nil

	case wsm.PolicyHold:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case wsm.PolicyHold"})
		kind, scheduleID, err := q.holdForLease(ctx, lease, log)
		if err != nil {
			return Disposition{}, true, err
		}
		q.noteDrainRefusal(lease.Holder, sub.WS)
		disposition, err := q.hold(ctx, sub, "", &leaseHold{kind: kind, scheduleID: scheduleID}, log)
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
// hibernation's revival is the session-starting hold. The merge holder never
// reaches here (its policy refuses).
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
		return wsm.HoldSessionStarting, "", nil
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
	disposition, err := q.hold(ctx, sub, "", &leaseHold{kind: wsm.HoldSessionStarting}, log)
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
			// means for the entries waiting on it: "a loud drop when the
			// bring-up fails (a session that never comes up can never
			// deliver, so a retained entry would be a leak, not a delay)".
			// The workspace fault the bring-up already opened is the
			// operator's record; this is the tray's.
			q.dropRevivalHolds(ctx, ws, log, err.Error())
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
			log.Error(opSubmit, "the revival reported success but the workspace still has no session", nil)
			q.dropRevivalHolds(ctx, ws, log, "the revival reported success but no session came up")
			return
		}
		q.releaseRevivalHolds(ctx, ws, log)
	}()
}

// dropRevivalHolds retires every revival-pending hold on a workspace whose
// bring-up FAILED, loudly.
//
// A session that never came up can never deliver these entries, and the hold
// has no force-through: left standing they would sit in the tray forever
// under a state whose only exit never arrives. Dropping them is what makes the
// failure VISIBLE and actionable — the tray re-pushes without them, and the
// warning names each turn so nothing vanishes unrecorded.
func (q *queue) dropRevivalHolds(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger, cause string) {
	drain := &q.state(ws).drain
	drain.Lock()
	defer drain.Unlock()

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opSubmit, "could not read the holds the failed revival should drop",
			dlog.Context{"cause": err.Error()})
		return
	}
	dropped := 0
	for _, h := range standing {
		if h.Tombstone != nil || h.Hold == nil || *h.Hold != wsm.HoldSessionStarting {
			continue
		}
		if err := q.deps.DB.TombstoneHeldPrompt(ctx, h.Turn, wsm.Tombstone{
			Kind: tombstoneDropped, At: q.deps.Now(),
		}); err != nil {
			log.Error(opSubmit, "could not drop a revival-pending hold",
				dlog.Context{"turn": string(h.Turn), "cause": err.Error()})
			continue
		}
		log.Warn(opSubmit, "dropped a revival-pending hold whose bring-up failed",
			dlog.Context{"turn": string(h.Turn), "cause": cause})
		q.retireEditIf(ctx, ws, h.Turn, tombstoneDropped, log)
		dropped++
	}
	if dropped == 0 {
		return
	}
	// WHAT THE FAILURE COST GOES ON THE FOOTER'S LINE. The bring-up already
	// installed the standing failure and its cause; the drop is decided here,
	// afterwards, so the count joins the line the strip is already drawing
	// rather than being left to the tray alone.
	q.deps.Footer.AddDroppedPrompts(ws, uint32(dropped))
	// The turn the roster took at acceptance is never going to run: the row
	// falls back to whatever the route reports, which is the bring-up's own
	// fault arm, rather than standing at `submitting` for a dropped prompt.
	q.deps.Sidebar.SetTurn(ws, nil)
	if err := q.pushTray(ctx, ws, log); err != nil {
		return
	}
}

// releaseRevivalHolds un-stamps every revival-pending hold on a workspace whose
// session is now up and delivers the next one down the ordinary path.
func (q *queue) releaseRevivalHolds(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) {
	// SERIALIZED AGAINST A TURN END AND A LEASE CHANGE, for the reason
	// OnTurnEnded states: all three deliver from the same standing holds.
	drain := &q.state(ws).drain
	drain.Lock()
	defer drain.Unlock()

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opSubmit, "could not read the holds the revival should release",
			dlog.Context{"cause": err.Error()})
		return
	}
	released := 0
	for _, h := range standing {
		if h.Tombstone != nil || h.Hold == nil || *h.Hold != wsm.HoldSessionStarting {
			continue
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, nil, ""); err != nil {
			log.Error(opSubmit, "could not release a revival-pending hold",
				dlog.Context{"turn": string(h.Turn), "cause": err.Error()})
			continue
		}
		released++
	}
	if released == 0 {
		log.Debug(opSubmit, "the revival released no hold", dlog.Context{"holds": len(standing)})
		return
	}
	log.Debug(opSubmit, "the revival released its pending holds", dlog.Context{"released": released})
	log.Info(opSubmit, "released the revival-pending prompts", dlog.Context{"released": released})
	if err := q.pushTray(ctx, ws, log); err != nil {
		return
	}
	if _, err := q.popAndDeliver(ctx, ws, log); err != nil {
		log.Error(opSubmit, "the revived hold was not delivered", dlog.Context{"cause": err.Error()})
	}
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
