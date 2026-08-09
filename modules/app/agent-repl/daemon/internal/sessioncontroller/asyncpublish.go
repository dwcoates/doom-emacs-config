// asyncpublish.go is the consumer's side of the async plane: it hands each
// event's detached content to the bubble store and pushes what comes back as
// one fenced AsyncBubbleDelta.
//
// ONE PUSH PER EVENT. Everything an event produced for the async plane — the
// bubbles it opened and the updates it folded — goes out as a single frame, so
// a frontend applies an event's whole async effect or none of it. Splitting it
// would let an update land in a client that has not yet been told about its
// bubble, which the contract requires that client to reject as a gap.
package sessioncontroller

import (
	"fmt"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// observeAsync folds one curated event's detached content into the session's
// bubbles.
//
// A FOLD FAILURE DEGRADES THE ASYNC PLANE ONLY. The conversation push
// continues, because the feed's correctness does not depend on the bubble's,
// and the failure is never swallowed: the record names the session, the event,
// and the refusal the apparatus returned.
//
// A CLASSIFIED DAEMON BUG IS A FAILURE CARD, not a warn. Every refusal the
// async plane calls a daemon bug — an update whose arm contradicts its bubble's
// kind, a source that rewound under an append-only fold — has one user-visible
// consequence: a bubble that stops growing, which is exactly what a quiet agent
// looks like. Nobody watching the screen can tell those apart from a log line,
// so those refusals are routed to the same card path an unattributable
// detachment takes, carrying the very sentence this warn would have carried.
// What is left over is still warned about here.
func (c *consumer) observeAsync(curated frontend.Curation, ev *corev1.Event) asyncPush {
	// THE CURATION VERDICT, RECORDED. Which records left the feed for a bubble
	// and which stayed is the single decision this whole plane turns on, and it
	// was previously unobservable: a subagent rendering at the top level and a
	// subagent rendering inside its bubble produced byte-identical logs. One
	// record per event states the split it made, so the verdict can be read back
	// rather than inferred from what appeared on screen.
	for _, fold := range curated.Detached {
		c.logf("session-controller: async fold CLASSIFIED session=%s ws=%q seq=%d source_tool_use_id=%q agent_id=%q emissions=%d — these records left the top-level feed for the bubble of the detachment that produced them",
			c.sessionID, c.workspace, ev.GetSeq(), fold.SourceToolUseID, fold.AgentID, len(fold.Emissions))
	}
	push, err := c.bubbles.observeCuration(curated, c.asyncInstant(ev))
	gaps, residual := splitAsyncGaps(err)
	push.Faults = append(push.Faults, gaps...)
	if residual != nil {
		c.warn("session-controller: ASYNC BUBBLE FOLD DEGRADED session=%s ws=%q seq=%d — detached work could not be folded and its bubble will not show the affected records, but the conversation push is unaffected: %v",
			c.sessionID, c.workspace, ev.GetSeq(), residual)
	}
	for _, uuid := range curated.WithheldDetached {
		c.logf("session-controller: detached record WITHHELD session=%s seq=%d uuid=%s — it belongs to a detached agent and has no emission arm to carry it, so it is withheld from the bubble rather than promoted to the top-level feed",
			c.sessionID, ev.GetSeq(), uuid)
	}
	return push
}

// observeAsyncTask folds a task-lifecycle event into the session's bubbles. It
// classifies its refusals exactly as observeAsync does: a daemon bug earns the
// card, everything else the degraded warn.
func (c *consumer) observeAsyncTask(ev *corev1.Event) asyncPush {
	var push asyncPush
	var err error
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_TaskStarted:
		push, err = c.bubbles.observeTaskStarted(p.TaskStarted, c.asyncInstant(ev))
	case *corev1.Event_TaskEnded:
		push, err = c.bubbles.observeTaskEnded(p.TaskEnded, c.asyncInstant(ev))
	default:
		return asyncPush{}
	}
	gaps, residual := splitAsyncGaps(err)
	push.Faults = append(push.Faults, gaps...)
	if residual != nil {
		c.warn("session-controller: ASYNC BUBBLE LIFECYCLE DEGRADED session=%s ws=%q seq=%d kind=%s — the detachment's bubble could not be updated, but the task catalog and the conversation are unaffected: %v",
			c.sessionID, c.workspace, ev.GetSeq(), stateKind(ev), residual)
	}
	return push
}

// pushAsync publishes one event's async effect, and surfaces every fault it
// carried.
//
// A FAULT IS A FAILURE CARD ON THE CONVERSATION, not a silent log line and not
// a blank-id bubble. The user started work the daemon cannot show them, and the
// card is the only place that fact can be said. It is pushed on its own
// ConversationDelta rather than appended to the event's, because the event's
// delta may be an all-detached one whose items were legitimately emptied, and a
// card is not one of that event's conversation items — it is the daemon's own
// account of failing to handle it.
//
// THAT IS A STATEMENT ABOUT A LIVE FAULT. A fault carried by a replayed
// historical event is the same account being read back out of the store, and it
// takes the withhold arm below instead: classified, recorded in full at info,
// and given no live card.
func (c *consumer) pushAsync(push asyncPush, ev *corev1.Event) {
	if push.empty() {
		return
	}
	// A REPLAYED FAULT IS HISTORY, NOT NEWS, and it is classified through the
	// same shared classifier every other replayed anomaly on this consumer uses
	// (rejectionIsHistorical), so the async plane can never disagree with the
	// rest of the daemon about which epoch an event belongs to.
	historical := rejectionIsHistorical(c.accounting, ev)
	for _, fault := range push.Faults {
		if historical {
			// INFO, NOT WARN, AND NO LIVE CARD — the same withhold discipline
			// surfaceUnexpectedQueryTermination applies to a replayed query death.
			// The anomaly was surfaced loudly at its original occurrence; this row
			// is its durable history being replayed on backfill, and re-alarming
			// on it every boot says a fault is happening now that is not. The
			// record is kept whole — same channel, same identity, same sentence —
			// and only its severity and its card are withheld. The LIVE arm below
			// is untouched.
			c.logf("session-controller: ASYNC DETACHMENT FAULT WITHHELD session=%s ws=%q seq=%d card_uuid=%s decision=retain_history_no_live_card — %s",
				c.sessionID, c.workspace, ev.GetSeq(), fault.UUID, fault.Detail)
			continue
		}
		c.warn("session-controller: ASYNC DETACHMENT FAULT session=%s ws=%q seq=%d card_uuid=%s — %s",
			c.sessionID, c.workspace, ev.GetSeq(), fault.UUID, fault.Detail)
		c.push.PushConversationDelta(&frontendv1.ConversationDelta{
			Workspace:  c.workspace,
			Fence:      c.fence(),
			ThroughSeq: ev.GetSeq(),
			Items: []*frontendv1.ConversationItem{{
				Uuid:   fault.UUID,
				TsMs:   c.asyncInstant(ev),
				Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
				Item:   &frontendv1.ConversationItem_FailureCard{FailureCard: fault.Card},
			}},
		})
	}
	if len(push.Opened) == 0 && len(push.Updates) == 0 {
		return
	}
	// THE BUBBLE IDS, NOT JUST THEIR COUNT. An update is addressed to a bubble
	// by id, so a push reporting only "updates=1" cannot be matched to the
	// bubble it filled — which is precisely the question asked of this record.
	c.logf("session-controller: async bubble push session=%s ws=%q seq=%d opened=%d updates=%d opened_bubbles=%s updated_bubbles=%s",
		c.sessionID, c.workspace, ev.GetSeq(), len(push.Opened), len(push.Updates),
		openedBubbleIDs(push.Opened), updatedBubbleIDs(push.Updates))
	c.pushAnchors(push.Opened, ev)
	c.push.PushAsyncBubbleDelta(&frontendv1.AsyncBubbleDelta{
		Workspace:  c.workspace,
		Opened:     push.Opened,
		Updates:    push.Updates,
		ThroughSeq: ev.GetSeq(),
		Fence:      c.fence(),
	})
}

// pushAnchors publishes the FEED ANCHOR of every bubble this event opened: one
// top-level ConversationItem on arm 38, carrying the bubble's OPENING state, at
// the point in the conversation the work was launched from.
//
// WHY IT IS HERE AND NOWHERE ELSE. This is the one site where a bubble becomes
// wire traffic, so the anchor and the AsyncBubbleDelta.opened that carries the
// same bubble are emitted from a single producer off a single list. A second
// producer somewhere else would be a second opinion about which launches got an
// anchor, and the two would disagree exactly on the bubbles one of them missed.
//
// THE ANCHOR PRECEDES THE ASYNC PUSH. The anchor is what gives the bubble a
// place in the conversation; the push is what starts filling it. Sending the
// fill first would momentarily describe a bubble the reader has nowhere to draw.
//
// EVERY OPENED BUBBLE GETS EXACTLY ONE. asyncPush.Opened lists first-time opens
// only — a re-observed detachment folds as an update — so the "exactly one
// anchor per launching call" the contract requires is a property of that list
// rather than of a dedup kept here.
//
// The item's uuid is DERIVED from the bubble id for the reason every synthesized
// uuid on this path is: a resync replays the same records, and a minted uuid
// would anchor the same bubble a second time on every pass.
func (c *consumer) pushAnchors(opened []*frontendv1.AsyncBubble, ev *corev1.Event) {
	if len(opened) == 0 {
		return
	}
	items := make([]*frontendv1.ConversationItem, 0, len(opened))
	for _, b := range opened {
		items = append(items, &frontendv1.ConversationItem{
			Uuid: "async-anchor:" + b.GetId(),
			TsMs: c.asyncInstant(ev),
			// The launch FOLLOWED FROM a user turn, which is what
			// CONVERSATION_SOURCE_USER states (feed.proto ConversationSource);
			// UNSPECIFIED is a malformed frame a receiver must reject.
			Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
			Item:   &frontendv1.ConversationItem_AsyncBubble{AsyncBubble: b},
		})
	}
	c.logf("session-controller: async bubble ANCHORED session=%s ws=%q seq=%d anchors=%d bubbles=%s",
		c.sessionID, c.workspace, ev.GetSeq(), len(items), openedBubbleIDs(opened))
	c.push.PushConversationDelta(&frontendv1.ConversationDelta{
		Workspace:  c.workspace,
		Fence:      c.fence(),
		ThroughSeq: ev.GetSeq(),
		Items:      items,
	})
}

// openedBubbleIDs renders an opened list as "id(origin_tool_use_id)" pairs.
//
// The ORIGIN IS RENDERED BESIDE THE ID because they answer different halves of
// the same question: the id is what every later update is addressed to, and the
// origin is the launching call that ties the bubble to the tool card it hangs
// under. A record carrying one without the other leaves the correlation to
// guesswork.
func openedBubbleIDs(opened []*frontendv1.AsyncBubble) string {
	ids := make([]string, 0, len(opened))
	for _, b := range opened {
		ids = append(ids, fmt.Sprintf("%s(origin_tool_use_id=%s)", b.GetId(), b.GetOriginToolUseId()))
	}
	return "[" + strings.Join(ids, " ") + "]"
}

// updatedBubbleIDs renders the bubble each update in a push is addressed to.
func updatedBubbleIDs(updates []*frontendv1.AsyncBubbleUpdate) string {
	ids := make([]string, 0, len(updates))
	for _, u := range updates {
		ids = append(ids, u.GetBubbleId())
	}
	return "[" + strings.Join(ids, " ") + "]"
}

// asyncInstant is the moment a bubble's fold records for an event: the
// PRODUCER's stamp when it has one, and the consumer's clock only when it does
// not.
//
// The producer's stamp is preferred for the same reason every other timestamp
// on this path prefers it: a replay must fold the same instants it folded the
// first time, and the consumer's clock would rewrite a year-old detachment as
// having just happened.
func (c *consumer) asyncInstant(ev *corev1.Event) int64 {
	if ms := ev.GetProducedAtMs(); ms > 0 {
		return ms
	}
	return c.now()
}
