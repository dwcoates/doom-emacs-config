package promptqueue

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// NOTHING IS DELIVERED INTO A SESSION THAT DOES NOT SERVE IT, AND NOTHING THAT
// WAITS ON ONE IS LEFT UNHELD (2026-10-10).
//
// The incident (doom, 2026-10-10 21:36): a prompt held for a revival was
// released the moment the revival returned, and the revival had parked the
// session at its cold gate. The queue logged "the session is up and the vendor
// serves it", tried to deliver into a session with no watcher, and failed. The
// release had already un-stamped the hold, so the prompt stood with no hold and
// no verdict, and nothing ever re-drove it.
//
// So the session is asked, at every place the queue would hand it a prompt,
// whether it SERVES one (sessionWait):
//
//   - a STANDING COLD GATE serves nothing, and a prompt never answers the gate
//     (REALTEST-JUDGEMENT-CALLS.md "The cold gate's feedback", call 11; owner
//     ruling 2, 2026-09-13, re-affirmed 2026-10-10). Every entry that waits on
//     the session is held under wsm.HoldColdGate;
//   - a SESSION THAT IS NOT UP (no shim client, no started session, no
//     watcher) serves nothing either. Those entries are held under
//     wsm.HoldReconnect.
//
// Both holds are released by the ONE edge that is guaranteed to come: the
// session coming up (ReleaseReconnectHolds). For a gate, the entries still
// on wsm.HoldColdGate when the answer brings the session up are the ones a
// compact-and-resume answer sends; every other answer has already discarded
// them or moved them to wsm.HoldColdGateAnswered (gatefate.go). For a session
// that is not up, it is whatever brings it up: a retry, a restart, a revival,
// an adoption.

// sessionWait reports whether the workspace's session does not serve a
// delivery now, the hold that names why, and the reason in words for the
// record. waiting is false when the session serves.
func (q *queue) sessionWait(ws ids.WorkspaceID) (kind wsm.HoldKind, why string, waiting bool) {
	if q.deps.ColdGate != nil {
		if detail, gated := q.deps.ColdGate(ws); gated {
			return wsm.HoldColdGate, "the session is parked at its cold gate: " + detail, true
		}
	}
	if _, ok := q.deps.Client(ws); !ok {
		return wsm.HoldReconnect, "the workspace has no shim", true
	}
	if !q.deps.SessionStarted(ws) {
		return wsm.HoldReconnect, "the shim holds no started session", true
	}
	if _, ok := q.deps.Watcher(ws); !ok {
		return wsm.HoldReconnect, "the session has no watcher", true
	}
	return 0, "", false
}

// holdForSession stamps KIND on every standing entry that waits on the session:
// the free entries (no hold) and those already held for the session under the
// other kind. An entry a shim call claims, and one held by another condition (a
// drain, a build refresh, a merge), is left as it stands. It reports how many
// entries moved, and republishes the tray when any did. The caller holds the
// delivery lock.
func (q *queue) holdForSession(ctx context.Context, ws ids.WorkspaceID, kind wsm.HoldKind, op string, log dlog.Logger) int {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the holds that wait on the session", dlog.Context{"cause": err.Error()})
		return 0
	}
	moved := 0
	for _, h := range standing {
		if h.Tombstone != nil || q.claimedByCall(ws, h.Turn) {
			continue
		}
		if h.Hold != nil && (*h.Hold == kind || !waitsOnSession(*h.Hold)) {
			continue
		}
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, &kind, ""); err != nil {
			log.Error(op, "could not hold a prompt for the session", dlog.Context{
				"turn": string(h.Turn), "hold": kind.String(), "cause": err.Error(),
			})
			continue
		}
		moved++
	}
	if moved > 0 {
		// A tray that could not be republished is recorded at ERROR by
		// pushTray; the holds themselves are durable either way.
		_ = q.pushTray(ctx, ws, log)
	}
	return moved
}

// reholdForSession puts ONE entry a delivery could not hand to the session back
// on the hold that names why, so the session coming up delivers it rather than
// leaving it standing with no hold for nothing to re-drive. The caller holds the
// delivery lock.
func (q *queue) reholdForSession(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, kind wsm.HoldKind, log dlog.Logger) error {
	if err := q.deps.DB.UpdateHeldPromptHold(ctx, turn, &kind, ""); err != nil {
		log.Error(opDeliver, "could not put an undelivered prompt back on the hold that waits for the session", dlog.Context{
			"turn": string(turn), "hold": kind.String(), "cause": err.Error(),
		})
		return fmt.Errorf("hold %q on %q for the session: %w", turn, ws, err)
	}
	return q.pushTray(ctx, ws, log)
}

// factsPending reports whether the workspace's session watcher has not yet
// taken up the session facts, and when so arranges for AGAIN to run under the
// delivery lock once they are in.
//
// UNTIL THE FACTS ARE IN, "NO TURN RUNS" CANNOT BE READ. An adopted shim's turn
// is unknown to the watcher until the shim re-announces its session, so
// TurnInFlight answers nil for a turn that is running. A boot's session-up
// release read that nil as idle and delivered a restored deferred prompt into
// the adopted shim's running turn (integration TestADeferredHoldSurvivesA
// DaemonRestartStillDeferred under load, 2026-10-10). So nothing is delivered
// on that nil: the step is taken again from the facts-in edge.
func (q *queue) factsPending(ws ids.WorkspaceID, what string, log dlog.Logger, again func(context.Context, *delivery, dlog.Logger)) bool {
	watcher, ok := q.deps.Watcher(ws)
	if !ok || watcher.SessionFactsIn() {
		return false
	}
	q.mu.Lock()
	if q.exiting {
		q.mu.Unlock()
		log.Debug(opSubmit, "the daemon is exiting; a step waiting on the session's facts is not taken", dlog.Context{"step": what})
		return true
	}
	q.releasing.Add(1)
	q.mu.Unlock()
	log.Info(opSubmit, "the session's facts are not in, so whether a turn runs is unknown; the step waits for them", dlog.Context{"step": what})
	go func() {
		defer q.releasing.Done()
		if err := watcher.AwaitSessionFacts(q.life); err != nil {
			log.Info(opSubmit, "the session's facts never came in; the step is left to the session's next edge", dlog.Context{
				"step": what, "cause": err.Error(),
			})
			return
		}
		d := q.lockDelivery(ws)
		defer d.unlock()
		log.Debug(opSubmit, "the session's facts are in; the step waiting on them is taken", dlog.Context{"step": what})
		again(context.Background(), d, log)
	}()
	return true
}

// stranded reports whether a standing entry is a prompt nothing holds and
// nothing has judged, on the ordinary delivery: the shape an older build left
// behind when a delivery failed after its hold was lifted (doom, 2026-10-10).
func stranded(h wsm.HeldPrompt) bool {
	return h.Tombstone == nil && h.Hold == nil && h.Classification == nil &&
		h.Delivery == wsm.DeliveryOrdinary && h.Act == nil
}
