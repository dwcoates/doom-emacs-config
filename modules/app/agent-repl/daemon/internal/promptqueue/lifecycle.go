package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

// OnTurnEnded is the LifecycleSink's turn end: the interrupting status clears,
// the turn's close is stamped, the acts queued behind the turn drain, and the
// next prompt is popped and delivered.
func (q *queue) OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose) {
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		// THE TURN STILL CLOSES. A workspace this queue cannot resolve is
		// recorded by q.logger at ERROR; its turn is closed and its ending
		// drawn all the same, and nothing is delivered.
		global := q.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
		q.mu.Lock()
		folded := q.stateLocked(ws).takeFoldedLocked(turn)
		q.mu.Unlock()
		if how.Failed() && len(folded) > 0 {
			global.Error(opJoin, "a turn that took folded prompts failed in a workspace the queue cannot resolve; they are not resubmitted",
				dlog.Context{"turn": string(turn), "folded": len(folded)})
		}
		q.endTurn(ctx, ws, turn, how, global)
		return
	}
	log = log.With(dlog.Context{"turn": string(turn), "close": how.String()})

	// A FOLDED PROMPT'S TURN NEVER RAN: the vendor took it into the running
	// turn, which goes on. Nothing about that turn changes here.
	if how == wsm.CloseFolded {
		q.onFolded(ctx, ws, turn, log)
		return
	}
	// A WITHDRAWN PROMPT IS HELD AGAIN. One still waiting to join ran no turn,
	// and nothing about the running turn changes; one that had already stood
	// as the session's turn ends here like any turn, so what is held -- the
	// withdrawn prompt first -- is delivered.
	withdrawn := how == wsm.CloseWithdrawn
	if withdrawn && q.onWithdrawn(ctx, ws, turn, log) {
		return
	}
	q.deps.Edges.TurnEnded(ws)

	// SERIALIZED AGAINST A LEASE CHANGE. The turn end that frees a workspace
	// and the handover's quiesce arrive together, and both deliver from the
	// same standing holds: taken concurrently the intake leaves out of order.
	//
	// THE PROMPTS FOLDED INTO THIS TURN END WITH IT. A failed turn never
	// answered them, so they are resubmitted — deferred here so the
	// resubmission runs only after the delivery lock below is released, since
	// Submit takes it.
	q.mu.Lock()
	folded := q.stateLocked(ws).takeFoldedLocked(turn)
	q.mu.Unlock()
	if how.Failed() && len(folded) > 0 {
		defer q.resubmitFolded(ws, turn, folded, log)
	}

	d := q.lockDelivery(ws)
	defer d.unlock()

	q.mu.Lock()
	var joined joiningPrompt
	joinStood := false
	if state, ok := q.states[ws]; ok {
		state.interrupting = false
		state.cut = nil
		// A TURN ENDED, so whatever serves the workspace held a session long
		// enough to finish one: a later death is not a crash loop.
		state.unattendedRevival = false
		// THE TURN PROMPTS WERE SENT TO JOIN ENDED WITHOUT FOLDING THEM IN, so
		// the oldest now runs as its own turn: its join is over, and the rest
		// now join it.
		joined, joinStood = state.standJoinsLocked(turn)
	}
	q.mu.Unlock()
	q.deps.Footer.SetInterrupting(ws, false)

	// ANOTHER TURN ALREADY RUNS. A vendor-started turn adopted on the same
	// flush (OnTurnAdopted is told before any end), or a turn delivered in the
	// instant between the watcher clearing this one and this call, is now the
	// session's turn: the roster's thinking is ITS, and a prompt popped here
	// would be started into it and refused. This turn's row still closes
	// through the door; what is held waits for the running turn's own end.
	if watcher, ok := q.deps.Watcher(ws); ok {
		if running := watcher.TurnInFlight(); running != nil && *running != turn {
			q.endTurn(ctx, ws, turn, how, log)
			q.oweResumption(ctx, ws, turn, how, log)
			q.forgetTry(ws, turn, log)
			log.Info(opTurnEnded, "the turn ended while another turn already runs; what is held waits for that one", dlog.Context{
				"turn_in_flight": string(*running),
			})
			if joinStood && *running == joined.turn {
				q.onJoinedTurnStood(ws, joined, log)
				return
			}
			if joinStood {
				joinNotStood(log, joined)
			}
			return
		}
	}
	if joinStood {
		joinNotStood(log, joined)
	}

	// The roster's turn fact is the daemon's own, so its close is too. The
	// footer takes the SAME close at the same instant: it is what raises a
	// failed turn's fault on both surfaces (ladder.ResolveTurnFault), so the
	// strip and the dot cannot disagree about it.
	if withdrawn {
		// A WITHDRAWN TURN NEVER RAN, so it leaves no result to read: the
		// surfaces drop it as they drop a turn the shim refused.
		q.setTurn(ws, nil)
	} else {
		q.deps.Sidebar.SetTurnEnded(ws, how)
		q.deps.Footer.SetTurnEnded(ws, how)
	}

	// THE DOOR: the row closes and the feed draws the ending together. A
	// failed write is recorded there, and the queue goes on to deliver.
	q.endTurn(ctx, ws, turn, how, log)
	q.oweResumption(ctx, ws, turn, how, log)

	// A TRY-NOW THE VENDOR TOOK AND THEN BLOCKED goes back on the hold, in its
	// place, before anything queued is decided (rehold.go).
	q.reholdFailedTry(ctx, d, turn, how, log)

	// THE BOUNCE REGISTRY IS CHECKED FIRST, before anything queued is
	// dispatched. A shim registered for a bounce that this turn end leaves
	// free is bounced now, and the workspace DRAINS: the next prompt is held
	// for the new shim rather than started on the one about to be stood down.
	// Deciding it here, under the delivery lock, is what makes the race
	// "a queued prompt starts a turn between is-free and bounce" impossible.
	if q.checkRegistryLocked(d, edgeWork, log) {
		log.Info(opTurnEnded, "the turn ended into a bounce; what is queued waits for the new shim", nil)
		return
	}

	// A STAND-DOWN STARTS NOTHING. The shim is about to go with the daemon,
	// and a held prompt delivered now would open a turn the same stand-down
	// cuts in its first instant (the queued turn of the 2026-10-12 hard
	// bounce). It stays held, and the next session that serves the workspace
	// delivers it after the resumption it is owed.
	if q.deps.StandingDown() {
		log.Info(opTurnEnded, "the daemon is standing down; what is held waits for the next session", nil)
		return
	}

	delivered, err := q.popAndDeliver(ctx, d, log)
	if err != nil {
		log.Error(opTurnEnded, "the next held prompt was not delivered", dlog.Context{"cause": err.Error()})
		return
	}
	if !delivered {
		// EVERY TURN END THAT REACHES THE QUEUE IS ON DISK. A delivery says so
		// at INFO; without this, a turn end with nothing behind it left no
		// record at all, and "the queue was never told" could not be told apart
		// from "the queue was told and had nothing to do".
		log.Info(opTurnEnded, "the turn ended; nothing is waiting to be delivered", nil)
	}
}

// endTurn is a LIVE turn end's close: the turn closes through the door, whose
// feed half draws (and files) its ending, and then the turn's desktop banner
// is raised from that ending. Every branch of OnTurnEnded ends its turn here,
// so no live turn end closes without the banner it earns.
//
// A TURN THE MERGE QUEUE CAUSED EARNS NONE. The merge orchestrator's turns (a
// conflict's resolution, a suite's fix, the configured pre and post prompts)
// are steps of a merge run, and a detached merge's handback reports an end the
// merge queue has already announced; the merge queue's own two banners, as a
// merge is put in line and as it is resolved, are what the user is told.
func (q *queue) endTurn(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose, log dlog.Logger) {
	_ = q.closeTurn(ctx, ws, turn, how, log)
	if q.earnsBanner(ctx, ws, turn, log) {
		q.deps.TurnBanners.OnTurnEnded(ws, turn, how)
	}
}

// earnsBanner reports whether a turn's end raises its desktop banner: every
// turn but one the merge queue caused (causedByMergeQueue).
//
// A TURN THE WORKSPACE NEVER RECORDED IS NO MERGE'S: the queue records every
// turn it delivers, a merge's included, before delivering it. A turn whose
// record cannot be read is NOT GUESSED AT: it is recorded at ERROR and raises
// nothing, since a merge's step must never sound.
func (q *queue) earnsBanner(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, log dlog.Logger) bool {
	record, err := q.deps.DB.Turn(ctx, ws, turn)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		return true
	case err != nil:
		log.Error(opTurnEnded, "could not read the turn's origin; no desktop banner", dlog.Context{
			"turn": string(turn), "cause": err.Error(),
		})
		return false
	case causedByMergeQueue(record.Origin):
		log.Info(opTurnEnded, "a merge's own turn ended; no desktop banner", dlog.Context{
			"turn": string(turn), "origin": record.Origin,
		})
		return false
	case causedByDaemonNotice(record.Origin):
		log.Info(opTurnEnded, "a turn a daemon notice or an agent's compaction caused ended; no desktop banner", dlog.Context{
			"turn": string(turn), "origin": record.Origin,
		})
		return false
	}
	return true
}

// causedByDaemonNotice reports whether a recorded origin is one of a turn the
// user had no part in beyond what the merge queue causes: an unseen daemon
// notice (promptorigin.Unseen) answered, or a compaction an agent asked for.
// Such a turn ends with no banner, as a merge queue notice's does.
func causedByDaemonNotice(origin string) bool {
	return promptorigin.UnseenNamed(origin) || origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT.String()
}

// OnTurnAdopted is the LifecycleSink's vendor-started turn: a turn the vendor
// ran with no prompt through this queue, which the shim adopted. The queue owns
// the turn rows and the roster's turn fact, so it records both exactly as it
// does for a turn it delivered -- the durable row first, so the turn's end
// closes it through the door and draws its ending, then the roster's thinking.
// The watcher already stands the turn in flight, which is what holds every
// submission behind it.
//
// NOTHING IS DELIVERED AND NO LOCK IS TAKEN: nothing waits on an adoption, and
// the watcher tells this before the same turn's OnTurnEnded on the same flush.
// A failed durable write is recorded at ERROR, and the roster still takes the
// turn: it is running whether or not its row was written.
func (q *queue) OnTurnAdopted(ws ids.WorkspaceID, turn ids.TurnID) {
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		log = q.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	}
	log = log.With(dlog.Context{"turn": string(turn)})
	at := q.deps.Now()
	record := wsm.Turn{
		ID:        turn,
		Workspace: ws,
		Origin:    conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED.String(),
		StartedAt: at,
	}
	if err := q.recordTurn(ctx, record, log); err != nil {
		log.Error(opTurnAdopted, "could not record the vendor-started turn; its close will find no row", dlog.Context{
			"cause": err.Error(),
		})
	}
	// THE TURN FACT IS THE DAEMON'S OWN. Taken and acknowledged in one step:
	// the vendor is already answering, so there is no submitting window.
	q.setTurn(ws, &footer.TurnStarted{At: at, Act: footer.ActPrompt})
	q.ackTurn(ws)
	log.Info(opTurnAdopted, "recorded a turn the vendor started on its own; what is held waits behind it", nil)
}

// OnTurnsEndedUnobserved is the LifecycleSink's adoption reconciliation: the
// turns an adoption found open that the adopted shim no longer runs ended while
// no daemon was watching, so each durable row is closed as orphaned -- the
// close written for a turn that had no terminal when the daemon reconciled.
//
// IT TAKES NO DELIVERY LOCK AND DELIVERS NOTHING. None of these turns was the
// adopted session's turn in flight, so nothing waits behind them, and the
// watcher may tell this from inside a bring-up the lock's holder is running.
// A close that fails is recorded at ERROR and the others are still closed.
func (q *queue) OnTurnsEndedUnobserved(ws ids.WorkspaceID, turns []ids.TurnID) {
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		return
	}
	for _, turn := range turns {
		fields := dlog.Context{"turn": string(turn), "close": wsm.CloseOrphaned.String()}
		if err := q.closeTurn(ctx, ws, turn, wsm.CloseOrphaned, log); err != nil {
			continue
		}
		log.Info(opTurnEnded, "closed a turn that ended while no daemon was watching", fields)
	}
	if len(turns) > 0 {
		q.deps.Edges.TurnEndedUnobserved(ws)
	}
}

// popAndDeliver delivers the next deliverable hold: the SEMANTIC HEAD an
// interjection or a release installed, else the oldest standing hold no
// daemon-side condition is holding. It reports whether a hold was delivered.
// The caller holds the delivery lock (d).
//
// EVERY ENTRY IS DECIDED AFRESH. A held act's call releases the lock (call.go),
// so the conditions below -- a bounce, a running cut, a lease -- are read
// again before each entry rather than once for the whole pop.
func (q *queue) popAndDeliver(ctx context.Context, d *delivery, log dlog.Logger) (bool, error) {
	// A SHIM CALL IN FLIGHT POPS NOTHING BESIDE IT: its settle pops (call.go).
	if q.deferToCall(d, log, "pop") {
		return false, nil
	}
	delivered := false
	for {
		holding, ok, err := q.mayPop(ctx, d.ws, log)
		if err != nil || !ok {
			return delivered, err
		}
		next, found, withheld, err := q.nextDeliverable(ctx, d.ws, holding)
		if err != nil {
			return delivered, err
		}
		if !found {
			if withheld > 0 {
				// AN EDIT IS WHY NOTHING WENT. Said at INFO, because "the turn
				// ended and my prompt did not go" is exactly the question it
				// answers.
				log.Info(opTurnEnded, "a held prompt is being edited; it and every prompt after it stay held",
					dlog.Context{"withheld": withheld})
				return delivered, nil
			}
			log.Debug(opTurnEnded, "nothing is waiting to be delivered", nil)
			return delivered, nil
		}
		log.Info(opTurnEnded, "delivering the next held entry", dlog.Context{"next_turn": string(next.Turn), "session_act": next.Act != nil})
		sent, err := q.deliverHeld(ctx, d, next, log)
		if err != nil {
			return delivered, err
		}
		if !sent {
			// The entry is held again (the shim held no session); nothing
			// behind it goes before it.
			return delivered, nil
		}
		delivered = true
		// A HELD ACT OPENS NO TURN, so the pop goes on to the entry behind it:
		// every act at the head of the queue is applied, in order, and the
		// first prompt behind them is delivered as the turn they preceded. A
		// context cut is a turn, so it ends the pop like any prompt.
		if next.Act == nil {
			return true, nil
		}
	}
}

// mayPop reports whether the pop may deliver the next entry -- no bounce
// drains the workspace, no session act runs, and no refusing lease owns the
// session -- and answers the holding lease the entry is picked under, nil
// when none holds.
func (q *queue) mayPop(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) (*wsm.Lease, bool, error) {
	// A DRAINING WORKSPACE DISPATCHES NOTHING: its bounce is replacing the
	// shim, and the bounce's own finish delivers what is held.
	if q.isDraining(ws) {
		log.Debug(opTurnEnded, "the workspace is draining for a bounce; the held prompts wait for the new shim", nil)
		return nil, false, nil
	}
	// A RUNNING SESSION ACT IS OVERTAKEN BY NOTHING. A turn end that drained a
	// queued /clear or /compact has just started it as the session's turn, and
	// every prompt held behind it — a still-classifying one included — waits
	// for ITS end, which pops them in order. The running-cut record is the
	// queue's own fact, read under q.mu, so this holds whichever caller pops.
	if cut, ok := q.runningCut(ws); ok {
		log.Info(opTurnEnded, "a session act is running; the held prompts wait for it to end", dlog.Context{
			"session_act_turn": string(cut.turn), "session_act": cut.command.String(),
		})
		return nil, false, nil
	}
	// A VENDOR THAT REFUSES THE SESSION IS DELIVERED NOTHING (owner ruling,
	// 2026-10-06; vendorblock.go): what waits is held after reconnect, and the
	// edge on which the vendor serves again releases and classifies it.
	if block, blocked := q.vendorBlocked(ws); blocked {
		q.holdFreeForVendorBlock(ctx, ws, block, log)
		return nil, false, nil
	}
	// A SESSION THAT IS THERE AND DOES NOT SERVE IS POPPED NOTHING
	// (sessionhold.go): a standing cold gate, or a shim with no started
	// session or no watcher. What waits is held under the hold that names why,
	// and the session coming up releases it. A workspace with NO shim is not
	// this case: the delivery revives it (deliverHeld), as a hibernated
	// session's lease release requires.
	if kind, why, waiting := q.sessionWait(ws); waiting {
		if _, shim := q.deps.Client(ws); shim || kind == wsm.HoldColdGate {
			moved := q.holdForSession(ctx, ws, kind, opTurnEnded, log)
			log.Info(opTurnEnded, "the session does not serve; nothing is delivered and the waiting prompts are held for it",
				dlog.Context{"hold": kind.String(), "why": why, "moved": moved})
			return nil, false, nil
		}
	}
	// A REFUSING LEASE OWNS THE SESSION, so nothing held is delivered into it.
	// PolicyHold stamps every standing hold and nextDeliverable filters those,
	// but PolicyRefuse (a merge lease an older build wrote) stamps none — it
	// refuses NEW submissions — so a turn ending underneath it would otherwise
	// release a prompt straight into the session the merge is driving. The
	// lease's release re-runs this through OnLeaseChanged, so nothing is lost.
	lease, held, err := q.deps.DB.Lease(ctx, ws)
	if err != nil {
		log.Error(opTurnEnded, "could not read the occupancy lease before delivering", dlog.Context{"cause": err.Error()})
		return nil, false, fmt.Errorf("read the lease for %q: %w", ws, err)
	}
	if held && lease.Policy == wsm.PolicyRefuse {
		log.Debug(opTurnEnded, "a refusing lease stands; the held prompts wait for its release",
			dlog.Context{"lease": string(lease.ID), "holder": holderName(lease.Holder)})
		return nil, false, nil
	}
	if held && lease.Policy == wsm.PolicyHold {
		return &lease, true, nil
	}
	return nil, true, nil
}

// nextDeliverable picks the hold a turn end delivers, and counts the holds a
// standing edit withheld. The caller holds the delivery lock, which is what
// keeps the edit it reads from moving under the pick.
//
// A HOLDING LEASE HOLDS EVERY ENTRY IT DOES NOT EXEMPT, STAMPED OR NOT. The
// stamp is the lease's projection, written by OnLeaseChanged after the lease
// is taken; deciding from the lease itself (holding, nil when none holds)
// leaves no instant between the two in which a turn end delivers an entry
// into the session the lease holder is driving.
func (q *queue) nextDeliverable(ctx context.Context, ws ids.WorkspaceID, holding *wsm.Lease) (wsm.HeldPrompt, bool, int, error) {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return wsm.HeldPrompt{}, false, 0, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	free := make([]wsm.HeldPrompt, 0, len(standing))
	withheld := 0
	for _, h := range standing {
		if h.Tombstone != nil || h.Hold != nil {
			continue
		}
		if holding != nil && !exemptFromLease(*holding, submissionOf(h).Origin) {
			continue
		}
		// AN EDIT WITHHOLDS ITS PROMPT AND EVERYTHING AFTER IT, the semantic
		// head included: a jump an interjection earned does not carry a
		// prompt past an edit standing ahead of it.
		if q.withheldByEdit(ws, h) {
			withheld++
			continue
		}
		free = append(free, h)
	}
	if len(free) == 0 {
		return wsm.HeldPrompt{}, false, withheld, nil
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
				return h, true, withheld, nil
			}
		}
	}

	sort.SliceStable(free, func(i, j int) bool { return free[i].QueuedAt.Before(free[j].QueuedAt) })
	return free[0], true, withheld, nil
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

	// SERIALIZED AGAINST A TURN END, for the reason OnTurnEnded states: the
	// two events deliver from the same standing holds.
	d := q.lockDelivery(ws)
	defer d.unlock()
	// A LEASE CHANGE IS A FREENESS EDGE for a registered bounce: a merge's
	// release frees the workspace a stale-build bounce was deferred behind
	// (rollout shimBounce), and nothing else would take it. Taken as a turn's
	// end takes one, deferring to a shim call in flight. It is a LEASE edge:
	// it never re-judges a replacement deferred behind live work (edgeSet).
	defer q.checkRegistryLocked(d, edgeLease, log)

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

	// A LEASE ENDING IS NOT A SESSION ARRIVING. A hibernation's lease is
	// dropped as the hibernation completes, while the workspace it parked
	// still has no shim; un-stamping its holds there would publish a tray
	// that calls them deliverable and would leave a release with no state to
	// name -- the session_starting refusal gone, and no session either, so
	// the only honest answer left would be the untyped "no session" about a
	// workspace that is in fact still coming up. The hold that survives the
	// lease is therefore restamped as session_starting and the bring-up is
	// driven by the same background revival a fresh submission takes, which
	// un-stamps and delivers when the session is actually up.
	revivalPending := false
	if want == nil && len(standing) > 0 {
		if _, live := q.deps.Client(ws); !live {
			want = &leaseHold{kind: wsm.HoldReconnect}
			revivalPending = true
		} else if kind, why, waiting := q.sessionWait(ws); waiting && kind == wsm.HoldColdGate {
			// A LEASE ENDING OVER A SESSION PARKED AT ITS COLD GATE delivers
			// nothing into it: its holds wait for the gate's answer.
			log.Info(opLeaseChange, "the lease ended while the session is parked at its cold gate; its holds wait for the gate's answer",
				dlog.Context{"why": why, "holds": len(standing)})
			want = &leaseHold{kind: wsm.HoldColdGate}
		} else if block, blocked := q.vendorBlocked(ws); blocked {
			// A LEASE ENDING UNDER A MID-SESSION VENDOR BLOCK delivers
			// nothing into it: its holds are held after reconnect, and the
			// vendor serving again releases them (vendorblock.go).
			log.Info(opLeaseChange, "the lease ended while the vendor does not serve the session; its holds are held after reconnect",
				dlog.Context{"vendor_block": block, "holds": len(standing)})
			want = &leaseHold{kind: wsm.HoldReconnect}
		}
	}

	changed := false
	for _, h := range standing {
		// THE LEASE HOLDER'S OWN ENTRY IS NEVER STAMPED BY ITS LEASE, by the
		// rule the submission path applies (exemptFromLease).
		target := want
		if held && exemptFromLease(lease, submissionOf(h).Origin) {
			target = nil
		}
		if h.Tombstone != nil || sameHold(h, target) {
			continue
		}
		var kind *wsm.HoldKind
		scheduleID := ""
		if target != nil {
			k := target.kind
			kind, scheduleID = &k, target.scheduleID
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, kind, scheduleID); err != nil {
			// THE MERGE RELEASED ITS LEASE after the read above. Its release
			// re-runs this evaluation, which un-stamps what this one could
			// not stamp, so nothing is left to do here.
			if errors.Is(err, wsm.ErrMergeLeaseGone) {
				log.Info(opLeaseChange, "the merge released its lease before the hold was stamped; the release re-evaluates it",
					dlog.Context{"turn": string(h.Turn)})
				continue
			}
			log.Error(opLeaseChange, "could not re-stamp a hold against the new lease",
				dlog.Context{"turn": string(h.Turn), "cause": err.Error()})
			continue
		}
		changed = true
	}
	if !changed {
		log.Debug(opLeaseChange, "no standing hold changed under the new lease",
			dlog.Context{"holds": len(standing)})
		if revivalPending {
			q.reviveInBackground(ctx, ws, log)
			return
		}
		// AN UNCHANGED TRAY CAN STILL HOLD AN ENTRY NOTHING DRIVES: a free
		// entry with nothing running, which an older build stranded (doom,
		// 2026-10-10) and which a handover's drain reaches through here with
		// no session-up edge of its own. With no lease holding it, it is
		// delivered as a turn end would deliver it.
		if want == nil {
			q.popIfIdle(ctx, d, log)
		}
		return
	}
	if err := q.pushTray(ctx, ws, log); err != nil {
		return
	}
	if revivalPending {
		log.Debug(opLeaseChange, "the lease ended with no session up; the holds wait on a revival",
			dlog.Context{"holds": len(standing)})
		q.reviveInBackground(ctx, ws, log)
		return
	}

	// A hold the lease no longer projects is deliverable the moment nothing is
	// running.
	if want != nil {
		return
	}
	q.popIfIdle(ctx, d, log)
}

// popIfIdle delivers the next free entry when no turn runs, as a turn end
// would. The caller holds the delivery lock (d).
func (q *queue) popIfIdle(ctx context.Context, d *delivery, log dlog.Logger) {
	if q.factsPending(d.ws, "the idle delivery", log, q.popIfIdle) {
		return
	}
	if _, running := q.turnInFlight(d.ws); running {
		return
	}
	if _, err := q.popAndDeliver(ctx, d, log); err != nil {
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
	report, err := q.CloseOrphans(ctx, ws, q.deps.Now())
	if err != nil {
		global.Error(opRestore, "could not close a dead session's in-flight turns", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return
	}
	// A TURN THE DAEMON'S OWN STAND-DOWN CUT IS NO CASUALTY. A forced stop can
	// take a shim down before it writes the turn's terminal, so the turn ends
	// here as an orphan; when the stand-down recorded it as owed, the cut was
	// ordered and the work is resumed when the session serves
	// (restartredrive.go). Only an orphan nobody owes is a turn lost.
	if q.allOwed(ctx, ws, report.Turns, global) {
		global.Info(opRestore, "closed the in-flight turns the daemon's own stand-down cut; their work is resumed when the session serves", dlog.Context{
			"workspace": string(ws), "orphans": len(report.Turns),
		})
		return
	}
	global.Warn(opRestore, "closed the in-flight turns of a workspace with no session", dlog.Context{
		"workspace": string(ws), "orphans": len(report.Turns),
	})
}

// allOwed reports whether every turn in turns is owed a resumption. A read that
// fails answers false, so the orphans are reported as the casualties they may be.
func (q *queue) allOwed(ctx context.Context, ws ids.WorkspaceID, turns []ids.TurnID, global dlog.Logger) bool {
	owed, err := q.deps.DB.OwedTurns(ctx, ws)
	if err != nil {
		global.Error(opRestore, "could not read the resumptions the workspace is owed", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return false
	}
	set := make(map[ids.TurnID]bool, len(owed))
	for _, turn := range owed {
		set[turn] = true
	}
	for _, turn := range turns {
		if !set[turn] {
			return false
		}
	}
	return len(turns) > 0
}

// joinNotStood records, at ERROR, a prompt sent to join a turn that ended
// without it running in that turn's place. THE WATCHER STANDS THE JOINING
// PROMPT IN FLIGHT the moment the turn it joined ends, before that end reaches
// the queue, so the end always finds it running: anything else is a defect.
func joinNotStood(log dlog.Logger, joined joiningPrompt) {
	log.Error(opJoin, "the turn a prompt was sent to join ended, but the prompt does not run in its place", dlog.Context{
		"joining_turn":        string(joined.turn),
		"joined_turn":         string(joined.into),
		"invariant_violation": "the watcher stands a joining prompt in flight when the turn it joined ends",
	})
}
