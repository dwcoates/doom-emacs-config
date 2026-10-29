package cachelapse

import (
	"time"

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
// answered, or AnswerBound passes.
func (k *Keeper) awaitAnswer(log dlog.Logger, ws ids.WorkspaceID, sent time.Time, fields dlog.Context) {
	k.answering[ws] = sent
	k.answerBy[ws] = sent.Add(AnswerBound)
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
	fields := dlog.Context{"workspace": string(ws), "sent": sent, "lapses_at": lapsesAt}
	log := k.deps.Log.Global()
	if lapsesAt.Before(sent.Add(k.deps.Interval)) {
		log.Debug(opDrive, "a lapse restated from the anchor before the keep-alive; its answer is still awaited", fields)
		return
	}
	delete(k.answering, ws)
	delete(k.answerBy, ws)
	log.Info(opDrive, "the vendor answered the keep-alive and the cache is re-anchored; its session is stood down until the next one", fields)
	k.deps.CycleDown(ws, sent)
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
		delete(k.answering, ws)
		delete(k.answerBy, ws)
		// warn: a keep-alive the vendor never answered leaves the cache to
		// lapse at its old anchor, and the next restore may meet a cold gate.
		k.deps.Log.Global().Warn(opDrive, "the keep-alive went unanswered past its bound; its session is stood down, and the cache lapses at its old anchor",
			dlog.Context{"workspace": string(ws), "sent": sent, "bound": AnswerBound.String()})
		k.deps.CycleDown(ws, sent)
	}
}
