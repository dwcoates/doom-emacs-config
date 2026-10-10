package promptqueue

import (
	"context"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
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
// session coming up (ReleaseReconnectHolds). For a gate, that is the gate's own
// answer bringing the session up. For a session that is not up, it is whatever
// brings it up: a retry, a restart, a revival, an adoption.

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
