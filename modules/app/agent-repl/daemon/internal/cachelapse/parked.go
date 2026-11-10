package cachelapse

import (
	"context"
	"errors"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
)

// A PARKED CONVERSATION IS KEPT WARM IN TANDEM WITH THE ONE THE SESSION RUNS
// (owner request, 2026-11-10). A workspace that took the queue bypass mode
// keeps two conversations, and only one runs as its session; the other is
// parked with its own cache clock (wsm.QueueBypass.ParkedClockAt), which a
// switch swaps with the workspace's (wsm.SwapParkedConversation). The beat
// plan is due at the earlier of the two clocks' next beats, and each beat
// beats both (beatPlanFor).
//
// THE PARKED BEAT RUNS OFF THE LOOP. The shim sends it on a query of its own
// and answers once the vendor has, which takes a vendor process's start and
// one request, so the loop never waits on it: the call runs on its own
// goroutine, bounded by AnswerBound, and its answer comes back as an edge.
// The shim refuses to be stood down while the beat runs, so no stand-down the
// loop asks for meanwhile can cut it off.

// parkedDue answers when WS's parked conversation's next beat is due, false
// when it parks none, its cache is known cold, SPENT judges its last beat
// spent, or one of its keep-alives is out.
func (k *Keeper) parkedDue(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, spent func(time.Time) bool) (time.Time, bool) {
	if k.parkedFlying[ws] {
		return time.Time{}, false
	}
	standing, err := k.deps.DB.QueueBypass(ctx, ws)
	if err != nil {
		if !leaving(ctx, log, opDrive, "reading the workspace's parked conversation to time its keep-alive", err) {
			log.Error(opDrive, "could not read the workspace's parked conversation; its keep-alive is not armed", dlog.Context{"cause": err.Error()})
		}
		return time.Time{}, false
	}
	if standing.ParkedVendorSessionID == "" || standing.ParkedCold || standing.ParkedClockAt == nil {
		return time.Time{}, false
	}
	clock := *standing.ParkedClockAt
	if spent(clock) {
		return time.Time{}, false
	}
	due := clock.Add(k.deps.Interval)
	if retry, ok := k.parkedRetry[ws]; ok && retry.After(due) {
		due = retry
	}
	return due, true
}

// beatParked sends WS's parked conversation's keep-alive through SHIM, off
// the loop, and marks it out until its answer comes back.
func (k *Keeper) beatParked(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, shim Keepalives) {
	parked, err := k.deps.Parked(ctx, ws)
	if err != nil {
		// Recorded at ERROR where it was read (workspace.Fleet.ParkedKeepalive).
		k.parkedRetry[ws] = k.deps.Clock.Now().Add(BeatRetry)
		return
	}
	if parked == nil {
		log.Error(opDrive, "the beat plan owed a parked keep-alive and the workspace parks no conversation", dlog.Context{
			"invariant_violation": "a parked conversation the plan reads is one the fleet can name",
		})
		return
	}
	k.parkedFlying[ws] = true
	log.Info(opDrive, "sent the parked conversation's keep-alive; its answer comes back on its own", dlog.Context{
		"parked_vendor_session_id": parked.GetVendorSessionId(), "answer_bound": AnswerBound.String(),
	})
	inflight.Go(func() {
		// THE BOUND IS A KEEP-ALIVE'S ANSWER BOUND (AnswerBound, measured over
		// 2,798 keep-alives): the parked beat is one keep-alive request.
		beatCtx, cancel := context.WithTimeout(ctx, AnswerBound)
		answer, err := shim.BeatParked(beatCtx, parked)
		cancel()
		k.enqueue(edge{ws: ws, kind: edgeParkedAnswered, parked: answer, err: err, why: "the parked conversation's keep-alive was answered"})
	})
}

// parkedAnswered records how WS's parked keep-alive ended and times its next
// beat: an answered one moves the parked clock, a lapsed cache is marked cold
// and beaten no more, and a failure is asked again after BeatRetry.
func (k *Keeper) parkedAnswered(ctx context.Context, ws ids.WorkspaceID, answer *shimv1.SendKeepaliveSuccess, err error) {
	delete(k.parkedFlying, ws)
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	now := k.deps.Clock.Now()
	var failed *BeatSubmitFailed
	switch {
	case errors.Is(err, ErrNoSession):
		log.Info(opDrive, "the shim holds no session for the parked keep-alive; it is asked again shortly", dlog.Context{"retry_in": BeatRetry.String()})
		k.parkedRetry[ws] = now.Add(BeatRetry)
	case errors.As(err, &failed):
		log.Error(opDrive, "the parked conversation's keep-alive failed; it is asked again shortly",
			dlog.Context{"cause": failed.Detail, "retry_in": BeatRetry.String()})
		k.parkedRetry[ws] = now.Add(BeatRetry)
	case err != nil:
		if !leaving(ctx, log, opDrive, "asking the shim for the parked keep-alive", err) {
			log.Error(opDrive, "could not ask the shim for the parked keep-alive; it is asked again shortly",
				dlog.Context{"cause": err.Error(), "retry_in": BeatRetry.String(), "bound": AnswerBound.String()})
		}
		k.parkedRetry[ws] = now.Add(BeatRetry)
	case answer.GetSent() != nil:
		delete(k.parkedRetry, ws)
		if err := k.deps.DB.SetParkedKeepalive(ctx, ws, now); err != nil && !leaving(ctx, log, opDrive, "recording the parked keep-alive", err) {
			log.Error(opDrive, "could not record the parked conversation's keep-alive; it is asked again shortly", dlog.Context{"cause": err.Error()})
			k.parkedRetry[ws] = now.Add(BeatRetry)
		} else {
			log.Info(opDrive, "the parked conversation's keep-alive was answered; its cache is warm", nil)
		}
	case answer.GetSkipped().GetCacheLapsed() != nil:
		delete(k.parkedRetry, ws)
		log.Info(opDrive, "the parked conversation's cache had already lapsed; it is beaten no more until it runs again",
			dlog.Context{"lapsed_at_ms": answer.GetSkipped().GetCacheLapsed().GetLapsedAtMs()})
		if err := k.deps.DB.MarkParkedCold(ctx, ws); err != nil && !leaving(ctx, log, opDrive, "marking the parked conversation cold", err) {
			log.Error(opDrive, "could not mark the parked conversation's cache cold; it is asked again shortly", dlog.Context{"cause": err.Error()})
			k.parkedRetry[ws] = now.Add(BeatRetry)
		}
	default:
		log.Error(opDrive, "the shim answered the parked keep-alive with a judgment a parked keep-alive never takes", dlog.Context{
			"invariant_violation": "a parked keep-alive is answered sent or cache_lapsed",
		})
		k.parkedRetry[ws] = now.Add(BeatRetry)
	}
	k.schedule(ctx, ws, "the parked conversation's keep-alive was answered")
}

// ConversationSwitched is a queue-bypass switch of WS's conversations, their
// clocks already swapped on the record (wsm.SwapParkedConversation): each
// conversation's beat is timed afresh from its own clock. It never blocks.
func (k *Keeper) ConversationSwitched(ws ids.WorkspaceID) {
	k.enqueue(edge{ws: ws, kind: edgeSwitched, why: "the workspace switched between its queue and bypass conversations"})
}

// switched forgets what this Keeper held for the conversation WS stepped out
// of, whose clock now lives on the parked row, and times both beats afresh.
func (k *Keeper) switched(ctx context.Context, ws ids.WorkspaceID) {
	delete(k.sent, ws)
	delete(k.parkedRetry, ws)
	k.schedule(ctx, ws, "the workspace switched between its queue and bypass conversations")
}
