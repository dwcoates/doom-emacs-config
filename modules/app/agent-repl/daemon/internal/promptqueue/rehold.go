package promptqueue

import (
	"context"
	"errors"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// A TRY-NOW THE VENDOR TAKES AND THEN BLOCKS GOES BACK ON THE HOLD (owner
// ruling, 2026-10-06). The prompt a try-now delivered (trynow.go) whose turn
// then fails while a vendor block stands — a usage limit, an authentication or
// billing refusal, any arm ladder.ClassifyFailure reads as a vendor block, as
// the footer stands it from the turn's own terminal — returns to the
// after-reconnect hold in its ORIGINAL place, and the block stands on the
// status as it did.
//
// THE VENDOR HAS ALREADY RECORDED IT. Evidence (the owner's own transcripts,
// 2026-10-06): a rate-limited turn leaves the user's record in the vendor
// transcript, followed by a `<synthetic>` assistant record
// (`isApiErrorMessage: true`, `error: "rate_limit"`, `apiErrorStatus: 429`),
// and the next prompt chains after both — so the failed prompt stays in the
// conversation the model is sent. Redelivered as it stands, it would be said
// twice. So before it is re-held, the shim CUTS it out of the vendor
// conversation (RollBackSession, files kept: the mechanism the rollback verb
// uses), and the feed drops the failed attempt for good (Feed.RollBackTurns):
// the feed shows no failed exchange, and the prompt is drawn where every held
// prompt is, in the tray.
//
// IT IS RE-HELD UNDER A NEW TURN ID with its original QueuedAt, which is its
// place. A turn id is started once (deliver.go), and the shim derives the
// prompt's vendor uuid from it, so the redelivery must not reuse the cut one.
//
// A CUT THAT CANNOT BE MADE LEAVES THE ATTEMPT AS IT IS: the prompt stays in
// the vendor conversation, so re-holding it would duplicate it. The failed
// turn stays drawn as the failed turn it was, and nothing is re-held. A
// prompt the vendor never recorded (prompt_not_recorded) has nothing to cut
// and is re-held.

// forgetTry drops the try a turn end can no longer act on.
func (q *queue) forgetTry(ws ids.WorkspaceID, turn ids.TurnID, log dlog.Logger) {
	if tried, ok := q.takeTry(ws, turn); ok {
		log.Info(opTurnEnded, "the tried prompt's turn ended while another turn runs; it is not re-held", dlog.Context{"tried_turn": string(tried.Turn)})
	}
}

// takeTry takes the workspace's remembered try when TURN is its turn.
func (q *queue) takeTry(ws ids.WorkspaceID, turn ids.TurnID) (wsm.HeldPrompt, bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	state := q.stateLocked(ws)
	if state.tried == nil || state.tried.Turn != turn {
		return wsm.HeldPrompt{}, false
	}
	tried := *state.tried
	state.tried = nil
	return tried, true
}

// reholdFailedTry puts the tried prompt of TURN back on the hold when its turn
// failed under a vendor block. The caller holds the delivery lock (d).
func (q *queue) reholdFailedTry(ctx context.Context, d *delivery, turn ids.TurnID, how sessionwatcher.TurnClose, log dlog.Logger) {
	tried, ok := q.takeTry(d.ws, turn)
	if !ok {
		return
	}
	fields := dlog.Context{"tried_turn": string(turn)}
	if !how.Failed() {
		log.Debug(opTurnEnded, "the tried prompt's turn did not fail; nothing is re-held", fields)
		return
	}
	block, blocked := q.vendorBlocked(d.ws)
	if !blocked {
		log.Info(opTurnEnded, "the tried prompt's turn failed with no vendor block standing; it stays the failed turn it was", fields)
		return
	}
	fields["vendor_block"] = block
	sender, ok := q.deps.Client(d.ws)
	if !ok {
		log.Info(opTurnEnded, "the vendor blocked the tried prompt, but no session is up to cut it from; it stays the failed turn it was", fields)
		return
	}
	if call, standing := q.standingCall(d.ws); standing {
		log.Info(opTurnEnded, "the vendor blocked the tried prompt, but a shim call stands; it stays the failed turn it was",
			merged(fields, dlog.Context{"call": call.what}))
		return
	}
	var err error
	d.outside(shimCall{what: "rollback", turn: turn}, log, func() {
		err = sender.RollBackTurn(ctx, turn)
	})
	switch {
	case err == nil:
		log.Info(opTurnEnded, "the vendor blocked the tried prompt; it was cut out of the vendor conversation", fields)
	case errors.Is(err, ErrPromptNotRecorded):
		log.Info(opTurnEnded, "the vendor blocked the tried prompt before recording it; there is nothing to cut", fields)
	default:
		log.Warn(opTurnEnded, "the vendor blocked the tried prompt and it could not be cut out of the vendor conversation; it stays the failed turn it was, so it is never said twice",
			merged(fields, dlog.Context{"cause": err.Error()}))
		return
	}
	// THE FEED'S REMOVAL ALWAYS HAPPENS; an error means only that recording
	// it durably failed, which the feed has already logged and raised.
	_ = q.deps.Feed.RollBackTurns(d.ws, []ids.TurnID{turn})

	kind := wsm.HoldReconnect
	again := tried
	again.Turn = wsm.NewTurnID()
	again.Hold = &kind
	again.ScheduleID = ""
	again.Classification = nil
	again.Accepted = false
	again.Tombstone = nil
	if err := q.deps.DB.PutHeldPrompt(ctx, again); err != nil {
		log.Error(opTurnEnded, "could not put the blocked tried prompt back on the hold; it is lost from the queue",
			merged(fields, dlog.Context{"cause": err.Error(), "held_turn": string(again.Turn)}))
		return
	}
	log.Info(opTurnEnded, "the vendor blocked the tried prompt; it is back on the after-reconnect hold in its place",
		merged(fields, dlog.Context{"held_turn": string(again.Turn)}))
	_ = q.pushTray(ctx, d.ws, log)
}
