package cachelapse

import (
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A KEEP-ALIVE'S SESSION IS STOOD DOWN ONLY ONCE THE VENDOR HAS ANSWERED IT
// (owner request, 2026-10-29). The stand-down used to be asked the moment the
// shim said the keep-alive was SENT. The keep-alive is the shim's own turn,
// which the stand-down gate cannot see, so the gate found the session free and
// stood the shim down under it: the vendor never answered, the cache was not
// re-anchored, and the next restore met a cold gate. Of the keep-alives sent
// from 2026-10-27 to 2026-10-29, 92 of 168 were interrupted that way (ship-gns,
// 2026-10-29 17:57:11: interrupted 48ms after it was sent; cold at 18:48).
//
// THE ANSWER IS THE RESTATED LAPSE. The shim re-anchors the cache when the
// keep-alive's request answers and restates the projected lapse on the session
// stream (shim.v1 SendKeepaliveSent). A keep-alive goes out only for a cache
// bought at the hour tier, so a lapse anchored at or after the send falls at
// least an interval after it; a restatement from the anchor before it falls
// minutes after the send, since the beat itself is due an interval after that
// anchor.

// AnswerBound is how long a sent keep-alive's answer is awaited before its
// session is stood down anyway: just over twice the slowest healthy answer
// measured (p99 57s over 2,798 keep-alives in the owner's transcripts,
// 2026-10-20 to 2026-10-29).
const AnswerBound = 2 * time.Minute

// awaitAnswer holds WS's stand-down until the keep-alive sent at SENT is
// answered, or AnswerBound passes. KEEPUP awaits the answer without a
// stand-down after it: a keep-live run on a session within its grace waits
// for the vendor all the same.
func (k *Keeper) awaitAnswer(log dlog.Logger, ws ids.WorkspaceID, sent time.Time, fields dlog.Context, keepUp bool) {
	k.answering[ws] = sent
	k.answerBy[ws] = sent.Add(AnswerBound)
	if keepUp {
		k.answerKeepsUp[ws] = true
		log.Info(opDrive, "the keep-alive was sent; the vendor's answer is awaited, and the session stays up after it",
			dlog.Merge(fields, dlog.Context{"answer_by": k.answerBy[ws]}))
		return
	}
	log.Info(opDrive, "the keep-alive was sent; its session is stood down once the vendor answers it",
		dlog.Merge(fields, dlog.Context{"answer_by": k.answerBy[ws]}))
}

// answered stands WS's session down when STATED is the restated lapse of the
// keep-alive its stand-down awaits. Any other restatement leaves it waiting.
func (k *Keeper) answered(ws ids.WorkspaceID, stated *conversationv1.SessionCacheLapse) {
	sent, waiting := k.answering[ws]
	if !waiting {
		return
	}
	lapsesAt := time.UnixMilli(stated.GetLapsesAtMs())
	fields := dlog.Context{"workspace": string(ws), "trigger": k.triggerOf(ws), "sent": sent, "lapses_at": lapsesAt}
	log := k.deps.Log.Global()
	if lapsesAt.Before(sent.Add(k.deps.Interval)) {
		log.Debug(opDrive, "a lapse restated from the anchor before the keep-alive; its answer is still awaited", fields)
		return
	}
	log.Info(opDrive, k.answerWords(ws,
		"the vendor answered the keep-alive and the cache is re-anchored; its session is stood down until the next one",
		"the vendor answered the keep-alive and the cache is re-anchored; its session stays up"), fields)
	k.settleAnswer(ws, sent, answeredAck(lapsesAt))
}

// answeredFailed stands WS's session down when FAILED, a keep-alive whose
// turn ended in a failure, answers the keep-alive its stand-down awaits. A
// failure with nothing awaited is one whose lapse already answered it, since
// the shim restates a lapse its request moved before the failure on the same
// ordered stream (conversation.v1.SessionKeepaliveFailed).
//
// INFO: the vendor answered, and the cache lapses at its old anchor exactly as
// it would have; the failure itself is the shim's to record (its vendor
// failure record, its network fault). Before this answer existed the session
// waited out AnswerBound and was recorded unanswered at WARN (2026-11-17
// 13:19:20: answered in 0.3s on ENOTFOUND, warned at 13:21:19).
func (k *Keeper) answeredFailed(ws ids.WorkspaceID, failed *conversationv1.SessionKeepaliveFailed) {
	sent, waiting := k.answering[ws]
	if !waiting {
		k.deps.Log.Global().Debug(opDrive, "a failed keep-alive with no stand-down awaiting it; its lapse already answered", dlog.Context{
			"workspace": string(ws), "detail": failed.GetDetail(),
		})
		return
	}
	k.deps.Log.Global().Info(opDrive, k.answerWords(ws,
		"the keep-alive's request failed; its session is stood down, and the cache lapses at its old anchor",
		"the keep-alive's request failed; its session stays up, and the cache lapses at its old anchor"), dlog.Context{
		"workspace": string(ws), "trigger": k.triggerOf(ws), "sent": sent, "detail": failed.GetDetail(),
	})
	k.settleAnswer(ws, sent, failedAck(failed.GetDetail()))
}

// answersDue stands down every session whose keep-alive went unanswered past
// AnswerBound: the shim is not held up for an answer that is not coming.
func (k *Keeper) answersDue() {
	now := k.deps.Clock.Now()
	for ws, by := range k.answerBy {
		if now.Before(by) {
			continue
		}
		sent := k.answering[ws]
		// warn: a keep-alive the vendor never answered leaves the cache to
		// lapse at its old anchor, and the next restore may meet a cold gate.
		k.deps.Log.Global().Warn(opDrive, k.answerWords(ws,
			"the keep-alive went unanswered past its bound; its session is stood down, and the cache lapses at its old anchor",
			"the keep-alive went unanswered past its bound; its session stays up, and the cache lapses at its old anchor"),
			dlog.Context{"workspace": string(ws), "trigger": k.triggerOf(ws), "sent": sent, "bound": AnswerBound.String()})
		k.settleAnswer(ws, sent, nil)
	}
}

// settleAnswer ends WS's wait on the keep-alive sent at SENT, however it
// ended (ACK answered or failed, or nil when unanswered past its bound), and
// stands its session down until the next one unless the wait kept it up: the
// one settlement every outcome shares, a keep-live run's included.
func (k *Keeper) settleAnswer(ws ids.WorkspaceID, sent time.Time, ack *agentreplv1.KeepLiveWorkspaceAck) {
	standDown := !k.answerKeepsUp[ws]
	delete(k.answering, ws)
	delete(k.answerBy, ws)
	delete(k.answerKeepsUp, ws)
	k.keepLiveAnswered(ws, ack, standDown)
	if !standDown {
		return
	}
	k.deps.CycleDown(ws, sent, k.stoodDownThen(ws))
}

// answerWords is the record of WS's settled answer: DOWN when its session is
// stood down after it, UP when the wait keeps it up (a keep-live on a session
// within its grace).
func (k *Keeper) answerWords(ws ids.WorkspaceID, down, up string) string {
	if k.answerKeepsUp[ws] {
		return up
	}
	return down
}
