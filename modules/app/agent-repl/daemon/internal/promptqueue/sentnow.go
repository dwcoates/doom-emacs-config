package promptqueue

import (
	"context"
	"errors"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// A "SEND NOW" THAT CANNOT GO AT ONCE WAITS, AND SAYS WHAT IT WAITS FOR
// (owner ruling, 2026-11-10: "Send now" means send now). Release accepts the
// press and marks the prompt sent now, DURABLY (wsm.SentNow), naming what
// keeps it. The mark does two things:
//
//   - THE POP TAKES IT FIRST (nextDeliverable): a turn end, a session coming
//     up, a bounce's finish or a merge's end delivers the marked prompt ahead
//     of every other held one but the head an interjection installed.
//   - EVERY RELEASE OF THE DELIVERY LOCK TAKES IT AGAIN (replaySentNow): each
//     blocker a waiting press names clears under that lock (a shim call's
//     settle, a turn starting, the interjection ahead of it going), so the
//     decision that cleared it is the one that sends the prompt, on the same
//     path a fresh press takes (decideRelease). Nothing waits on a timer.
//
// Being durable, the mark survives a restart and a handover: the successor
// reads it with the hold.

// ReleaseRefusal is why a release was refused: the
// agentrepl.v1.UpdateHeldPromptReleaseRefused arm, by its proto name.
type ReleaseRefusal string

// The causes a release is refused for, one per
// agentrepl.v1.UpdateHeldPromptReleaseRefused arm.
const (
	// RefusedBeingEdited: the prompt is open in the editor's input, and
	// sending it would discard the edit.
	RefusedBeingEdited ReleaseRefusal = "being_edited"
	// RefusedBehindEdit: a prompt queued ahead of it is being edited, and an
	// edit keeps everything after it in order.
	RefusedBehindEdit ReleaseRefusal = "behind_edit"
	// RefusedColdGate: the session is parked at its cold gate, which only the
	// user's answer decides.
	RefusedColdGate ReleaseRefusal = "cold_gate"
	// RefusedSessionRefused: the session refused the prompt into the running
	// turn; it waits for that turn's end.
	RefusedSessionRefused ReleaseRefusal = "session_refused"
)

// ReleaseRefusedError is a refused release and its cause. It answers
// errors.Is against ErrReleaseRefused.
type ReleaseRefusedError struct {
	Cause ReleaseRefusal
}

func (e *ReleaseRefusedError) Error() string {
	return ErrReleaseRefused.Error() + ": " + string(e.Cause)
}

// Is makes a ReleaseRefusedError an ErrReleaseRefused.
func (e *ReleaseRefusedError) Is(target error) bool { return target == ErrReleaseRefused }

// The messages a release's records carry, one per outcome, shared by a fresh
// press and a replayed one so the two read alike.
const (
	msgReleaseWaits     = "the release cannot go yet; it is accepted and goes the moment what keeps it clears"
	msgReleaseRefused   = "the release is refused"
	msgReleaseWithdrawn = "the waiting Send now is withdrawn: the release is now refused"
)

// settleRelease applies DECISION to HELD: a waiting press is marked (or its
// mark updated) and the tray republished, a refusal clears any mark and
// answers its typed cause, and a sent prompt answers nothing. REPLAY is a
// waiting press taken again (replaySentNow), whose unchanged wait is recorded
// at DEBUG rather than said again. The caller holds the delivery lock.
func (q *queue) settleRelease(ctx context.Context, d *delivery, held wsm.HeldPrompt, decision releaseDecision, replay bool, log dlog.Logger) error {
	switch decision.verdict {
	case releaseSent:
		return nil
	case releaseFailed:
		return decision.err
	case releaseWaits:
		if held.SentNow != nil && held.SentNow.WaitsFor == decision.waitsFor {
			log.Debug(opRelease, "the waiting Send now still waits", dlog.Context{"waits_for": decision.waitsFor, "replay": replay})
			return nil
		}
		if err := q.markSentNow(ctx, d, held, &wsm.SentNow{At: q.deps.Now(), WaitsFor: decision.waitsFor}, log); err != nil {
			return err
		}
		log.Info(opRelease, msgReleaseWaits, dlog.Context{"waits_for": decision.waitsFor, "replay": replay})
		return nil
	case releaseRefused:
		message := msgReleaseRefused
		if held.SentNow != nil {
			message = msgReleaseWithdrawn
			if err := q.markSentNow(ctx, d, held, nil, log); err != nil {
				return err
			}
		}
		log.Info(opRelease, message, dlog.Context{"cause": string(decision.refusal), "replay": replay})
		return &ReleaseRefusedError{Cause: decision.refusal}
	default:
		panic("promptqueue: unknown release verdict")
	}
}

// markSentNow records MARK on HELD (nil clears it) and republishes the tray.
// A hold retired meanwhile has gone, and is answered as delivered.
func (q *queue) markSentNow(ctx context.Context, d *delivery, held wsm.HeldPrompt, mark *wsm.SentNow, log dlog.Logger) error {
	if err := q.deps.DB.SetHeldPromptSentNow(ctx, held.Turn, mark); err != nil {
		if errors.Is(err, wsm.ErrHoldRetired) {
			log.Info(opRelease, "the prompt was retired before its Send now could be recorded", dlog.Context{"cause": err.Error()})
			return ErrAlreadyDelivered
		}
		log.Error(opRelease, "could not record the Send now on the held prompt", dlog.Context{
			"cause": err.Error(), "sent_now": mark != nil,
		})
		return err
	}
	if mark != nil {
		q.mu.Lock()
		d.state.sentNowNone = false
		q.mu.Unlock()
	}
	return q.pushTray(ctx, d.ws, log)
}

// replaySentNow takes again every "Send now" waiting on the workspace, in the
// order the prompts were queued, on the path a fresh press takes. It runs at
// every release of the delivery lock (delivery.unlock), with the lock held
// and no shim call standing; a call's settle replays instead.
func (q *queue) replaySentNow(d *delivery) {
	q.mu.Lock()
	skip := d.state.sentNowNone || d.state.call != nil
	q.mu.Unlock()
	if skip {
		return
	}
	ctx := context.Background()
	log, err := q.logger(ctx, d.ws)
	if err != nil {
		// q.logger recorded the failure. The marks are durable, and the pop
		// still takes them first.
		return
	}
	standing, err := q.deps.DB.HeldPrompts(ctx, d.ws)
	if err != nil {
		log.Error(opRelease, "could not read the holds to take a waiting Send now again; the marks stand", dlog.Context{"cause": err.Error()})
		return
	}
	marked := 0
	for _, held := range inQueueOrder(standing) {
		if held.Tombstone != nil || held.SentNow == nil {
			continue
		}
		marked++
		held := held
		hlog := log.With(dlog.Context{"turn": string(held.Turn)})
		if q.claimedByCall(d.ws, held.Turn) {
			continue
		}
		if err := q.settleRelease(ctx, d, held, q.decideRelease(ctx, d, held, hlog), true, hlog); err != nil {
			var refusal *ReleaseRefusedError
			if !errors.As(err, &refusal) {
				hlog.Info(opRelease, "a waiting Send now could not be taken again; its mark stands", dlog.Context{"cause": err.Error()})
			}
		}
		// A send makes a call, which can stand only inside it; anything owed
		// to it is the unlock's to run before the next replay.
		if q.redriveOwed(d.state) {
			return
		}
	}
	if marked == 0 {
		q.mu.Lock()
		d.state.sentNowNone = true
		q.mu.Unlock()
	}
}
