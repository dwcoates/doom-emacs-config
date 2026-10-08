package promptqueue

import (
	"context"
	"sort"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// A MID-SESSION VENDOR BLOCK HOLDS EVERY PROMPT AFTER RECONNECT, UNCLASSIFIED
// (owner ruling, 2026-10-06). While the vendor or the account refuses a
// session that is up — a usage limit, an authentication or billing refusal, a
// vendor error, a retry holding the running turn (footer.Resolver.VendorBlock)
// — a prompt submitted to the workspace is placed on the after-reconnect hold
// (wsm.HoldReconnect, the tray's `reconnect` arm) at once: no classifier is
// asked and nothing is sent to the vendor. Nothing is classified while it sits
// there.
//
// THE EDGE THAT RELEASES THEM is the footer's: the mutation in which the
// mid-session vendor block stops standing (footer.WithVendorServes) — a
// session (re)start, a rate-limit verdict that is not rejected, a new turn
// opening, the retried call answered, the retried turn ending. A session
// coming up (ReleaseReconnectHolds) releases them too, which is what carries
// them across a daemon restart: the holds are durable, and the adoption's
// session-up edge releases them.
//
// WHEN THEY ARE POPPED OFF THE HOLD THEY ARE CLASSIFIED (classifyReleased): in
// queue order, each against what is then ahead of it — the running turn, or
// the prompt the release has just delivered — and delivered per its verdict.

// OnVendorServes implements Queue. It never blocks: the footer tells it from
// inside whatever change lifted the block, which can be this queue's own
// delivery under its delivery lock, so the release runs on a goroutine of its
// own, which Drain joins.
func (q *queue) OnVendorServes(ws ids.WorkspaceID) {
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		return
	}
	q.mu.Lock()
	if q.exiting {
		q.mu.Unlock()
		log.Debug(opSubmit, "the daemon is exiting; the vendor serving again releases nothing", nil)
		return
	}
	q.releasing.Add(1)
	q.mu.Unlock()
	go func() {
		defer q.releasing.Done()
		// A SESSION THAT IS NOT UP IS NOT SERVED by the vendor that stopped
		// refusing it: the session's coming up releases the holds instead.
		if !q.deps.SessionStarted(ws) {
			log.Info(opSubmit, "the vendor block lifted while no session is up; the session's coming up releases the after-reconnect holds", nil)
			return
		}
		q.releaseReconnectHolds(ctx, ws, log)
	}()
}

// vendorBlocked answers the standing mid-session vendor block by name.
func (q *queue) vendorBlocked(ws ids.WorkspaceID) (string, bool) {
	return q.deps.Footer.VendorBlock(ws)
}

// holdForVendorBlock places a submission on the after-reconnect hold,
// unclassified, because the vendor does not serve the session that is up.
func (q *queue) holdForVendorBlock(ctx context.Context, sub Submission, block string, log dlog.Logger) (Disposition, error) {
	log.Info(opSubmit, "the vendor does not serve the session; the prompt is held after reconnect, unclassified, until it serves again",
		dlog.Context{"vendor_block": block})
	return q.hold(ctx, sub, "", &leaseHold{kind: wsm.HoldReconnect}, log)
}

// releaseBeforeSubmit releases the after-reconnect holds standing on a
// workspace whose session is up and whose vendor serves, BEFORE a new
// submission is decided, so the new prompt never overtakes them. The edge
// that lifted the block releases them off the submission's path
// (OnVendorServes); a submission that lands between that edge and its release
// performs the release itself, under the same delivery lock, and the edge's
// own release then finds nothing standing. The caller holds the lock (d).
func (q *queue) releaseBeforeSubmit(ctx context.Context, d *delivery, log dlog.Logger) {
	standing, err := q.deps.DB.HeldPrompts(ctx, d.ws)
	if err != nil {
		log.Error(opSubmit, "could not read the after-reconnect holds a submission must not overtake", dlog.Context{"cause": err.Error()})
		return
	}
	waiting := 0
	for _, h := range standing {
		if standingReconnectHold(h) {
			waiting++
		}
	}
	if waiting == 0 {
		return
	}
	log.Info(opSubmit, "after-reconnect holds stand on a session the vendor serves; they are released ahead of the submission",
		dlog.Context{"held": waiting})
	q.releaseReconnectHoldsLocked(ctx, d, log)
}

// classifyReleased classifies the prompts just popped off the after-reconnect
// hold, in queue order, and delivers them per their verdicts. With a turn
// running, each is judged against what is ahead of it; with none, the head of
// the queue is delivered first (a prompt with nothing running is delivered,
// the classifier having nothing to judge it against) and every released
// prompt behind it is judged against the turn it started. The caller holds the
// delivery lock (d).
func (q *queue) classifyReleased(ctx context.Context, d *delivery, released []wsm.HeldPrompt, log dlog.Logger) {
	sort.SliceStable(released, func(i, j int) bool { return released[i].QueuedAt.Before(released[j].QueuedAt) })
	if _, running := q.runningTurn(d.ws); !running {
		if _, err := q.popAndDeliver(ctx, d, log); err != nil {
			log.Error(opSubmit, "a prompt released from the after-reconnect hold was not delivered", dlog.Context{"cause": err.Error()})
			return
		}
	}
	running, ok := q.runningTurn(d.ws)
	if !ok {
		log.Info(opSubmit, "no turn runs after the release; the released prompts wait their turn in order", dlog.Context{"released": len(released)})
		return
	}
	classified := 0
	for _, h := range released {
		standing, err := q.standingHold(ctx, d.ws, h.Turn)
		if err != nil || standing.Tombstone != nil || standing.Hold != nil {
			// Delivered by the pop above, dropped, or held again by another
			// condition: there is nothing to classify.
			continue
		}
		if q.withheldByEdit(d.ws, standing) {
			log.Info(opSubmit, "a released prompt is being edited; its commit classifies it", dlog.Context{"held_turn": string(h.Turn)})
			continue
		}
		if q.claimedByCall(d.ws, standing.Turn) {
			continue
		}
		log.Info(opClassify, "a prompt popped off the after-reconnect hold is classified now", dlog.Context{
			"held_turn": string(h.Turn), "running_turn": string(running),
		})
		q.classifyHeld(ctx, submissionOf(standing), running, log)
		classified++
	}
	log.Info(opSubmit, "classified the prompts released from the after-reconnect hold", dlog.Context{
		"released": len(released), "classified": classified, "running_turn": string(running),
	})
}

// holdFreeForVendorBlock moves every free entry onto the after-reconnect hold
// while a mid-session vendor block stands, so a pop never delivers into a
// vendor that refuses the session. It reports whether any entry moved. The
// caller holds the delivery lock.
func (q *queue) holdFreeForVendorBlock(ctx context.Context, ws ids.WorkspaceID, block string, log dlog.Logger) bool {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opTurnEnded, "could not read the holds a vendor block keeps from delivery", dlog.Context{"cause": err.Error()})
		return false
	}
	kind := wsm.HoldReconnect
	moved := 0
	for _, h := range standing {
		if h.Tombstone != nil || h.Hold != nil || q.claimedByCall(ws, h.Turn) {
			continue
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, &kind, ""); err != nil {
			log.Error(opTurnEnded, "could not hold a prompt after reconnect while the vendor block stands",
				dlog.Context{"turn": string(h.Turn), "cause": err.Error()})
			continue
		}
		moved++
	}
	log.Info(opTurnEnded, "the vendor does not serve the session; nothing is delivered and the waiting prompts are held after reconnect",
		dlog.Context{"vendor_block": block, "moved": moved})
	if moved > 0 {
		if err := q.pushTray(ctx, ws, log); err != nil {
			return true
		}
	}
	return moved > 0
}
