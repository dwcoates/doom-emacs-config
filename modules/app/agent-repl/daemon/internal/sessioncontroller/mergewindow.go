// mergewindow.go is the MERGE WINDOW: the span of a session between the merge
// skill's invocation and the user taking the session back, during which every
// emission the session produces belongs to the Merge bubble rather than to the
// top-level feed.
//
// WHY MEMBERSHIP IS TEMPORAL. Every other bubble on this contract owns its
// content by a JOIN: a sidechain record names the call that dispatched it, a
// retrieval names its task id. A merge has no such key — it drives THIS
// workspace's own session, so its records are the session's ordinary top-level
// records and are indistinguishable from the user's own conversation by
// anything on the record itself. What separates them is WHEN they happened, and
// so the window is the membership rule. async-bubble.proto says exactly this on
// AsyncMergeBubble: "Membership is TEMPORAL — the window between that
// classification and the user taking the session back — not a join key."
//
// THE THREE EDGES.
//
//   - OPEN, at the classification of the Skill call (frontend.MergeSkillCall).
//     The bubble is minted through the same frontend.OpenAsyncBubble every
//     other kind goes through, and filed under the call's tool_use id, which is
//     what makes StampSpawnedBubbleIDs stamp the card exactly as an agent spawn
//     stamps its own.
//   - FOLD, for every item of every later event, until an edge closes the
//     window. The Skill call's OWN card is the one exception: its result and
//     its body settle the card on the feed, because the card is what the bubble
//     hangs under.
//   - SETTLE, on the user's own next prompt or on an interrupt.
//
// HOW THE FOLD REACHES THE CLIENT. By APPEND, on the contract's own `merge`
// update arm — AsyncAgentUpdate carried under AsyncBubbleUpdate.merge, whose
// semantics are the agent arm's exactly. An earlier shape re-delivered the whole
// bubble through AsyncBubbleDelta.opened, which existed only because no merge
// update arm did; the update oneof's own rule ("Never a re-send of the whole
// bubble") is what retires it. See frontend.AppendWindowEmissions.
package sessioncontroller

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// --- the store's window state ----------------------------------------------

// mergeWindowOpen reports whether a merge run currently owns the session's
// emissions.
func (s *asyncBubbleStore) mergeWindowOpen() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.mergeWindow != ""
}

// mergeOriginToolUseID reports the Skill call the open window was opened by,
// empty when no window is open.
func (s *asyncBubbleStore) mergeOriginToolUseID() string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.mergeOrigin
}

// openMergeWindow opens the Merge bubble for one classified merge invocation.
//
// A SECOND INVOCATION WHILE ONE IS OPEN IS A DAEMON FAULT, surfaced as a
// failure card through the very same path an unattributable detachment takes.
// It is NOT nested and it is NOT allowed to replace the open window, and the
// reason is what each alternative would do to the conversation: nesting would
// need a second temporal window inside the first, and since both windows claim
// every emission of one session there is nothing that could ever assign an
// emission to one rather than the other; replacing would strand the first
// bubble live forever, since the settle edge that was going to close it now
// closes the replacement. Refusing keeps the open window's account of the
// session intact, keeps the second invocation's own records in that account
// (they are emissions inside the window, and fold there), and says loudly on
// the conversation that the daemon saw something it cannot model.
//
// A REPLAY OF THE SAME INVOCATION IS NOT A SECOND ONE. The bubble id is derived
// from the originating call, so a re-consumed event finds the bubble it already
// opened; it re-adopts the window rather than faulting or opening a twin.
func (s *asyncBubbleStore) openMergeWindow(originToolUseID, label string, atMs int64) (*frontendv1.AsyncBubble, *asyncFault, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if b := s.lookupLocked(originToolUseID); b != nil {
		// Already open for THIS call: a replay of the classifying event. Re-adopt
		// the window so the fold resumes where it left off.
		s.mergeWindow, s.mergeOrigin = b.GetId(), originToolUseID
		return nil, nil, nil
	}
	if s.mergeWindow != "" {
		return nil, s.faultLocked(originToolUseID, fmt.Sprintf(
			"the merge skill was invoked on call %q while the merge run opened by call %q (bubble %q) is still open. A merge window claims EVERY emission of this session until the user takes it back, so two open windows have no rule that could assign an emission to one rather than the other: the second invocation is refused, its own records fold into the window already open, and this card is the daemon saying so rather than nesting one merge inside another",
			originToolUseID, s.mergeOrigin, s.mergeWindow)), nil
	}
	b, err := frontend.OpenAsyncBubble(frontend.BubbleSpec{
		// The originating call IS the identity: no task id exists for a merge
		// (nothing detached), and deriving the bubble id from the call is what
		// makes a replay resolve to the same bubble instead of a twin.
		TaskID:          originToolUseID,
		Workspace:       s.workspace,
		Kind:            frontend.DetachMerge,
		OriginToolUseID: originToolUseID,
		ParentBubbleID:  s.parentByToolUse[originToolUseID],
		Label:           label,
		StartedAtMs:     atMs,
	})
	if err != nil {
		return nil, s.faultLocked(originToolUseID, err.Error()), nil
	}
	s.adoptLocked(b, originToolUseID, originToolUseID)
	s.mergeWindow, s.mergeOrigin = b.GetId(), originToolUseID
	s.logf("session-controller: merge window OPENED bubble=%s ws=%s origin_tool_use_id=%s label=%q started_at_ms=%d — every emission of this session folds here until the user's next prompt or an interrupt",
		b.GetId(), s.workspace, originToolUseID, label, atMs)
	return b, nil, nil
}

// foldMergeEmissions folds one batch of the session's emissions into the open
// Merge bubble and returns the incremental update that carries them.
//
// It also INDEXES the calls the batch made, which is what gives a subagent
// dispatched inside the merge its parent pointer: a nested detachment's bubble
// resolves its parent through parentByToolUse exactly as a detached agent's
// nested dispatch does, so the tree reads truthfully rather than hanging the
// child at the top level beside the merge that started it.
func (s *asyncBubbleStore) foldMergeEmissions(ems []*frontendv1.AgentEmission, atMs int64) (*frontendv1.AsyncBubbleUpdate, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.mergeWindow == "" {
		return nil, nil
	}
	b := s.byID[s.mergeWindow]
	if b == nil {
		return nil, fmt.Errorf("session-controller: merge fold REFUSED — the window names bubble %q, which the store does not hold; the emissions are not folded rather than being attributed to a bubble nobody can route to", s.mergeWindow)
	}
	s.indexCallsLocked(ems, b.GetId())
	up, err := frontend.AppendWindowEmissions(b, ems, atMs)
	if err != nil || up == nil {
		return nil, err
	}
	s.logf("session-controller: merge fold append bubble=%s kind=%s ws=%s appended_emissions=%d folded_emissions=%d dropped_before=%d delivery=merge_update_arm",
		b.GetId(), frontend.AsyncBubbleKind(b), s.workspace,
		len(ems), len(b.GetMerge().GetEmissions()), b.GetMerge().GetFold().GetDroppedBefore())
	return up, nil
}

// settleMergeWindow settles the open Merge bubble and closes the window,
// reporting nothing when no window is open.
//
// CLOSING IS UNCONDITIONAL ONCE THE EDGE IS REACHED, even if the settlement
// itself is refused: a window left open on a bubble that failed to settle would
// go on swallowing the user's own conversation, which is a far worse failure
// than the unsettled bubble the error already reports.
func (s *asyncBubbleStore) settleMergeWindow(v frontend.AsyncVerdict, edge string) (*frontendv1.AsyncBubbleUpdate, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.mergeWindow == "" {
		return nil, nil
	}
	id, origin := s.mergeWindow, s.mergeOrigin
	s.mergeWindow, s.mergeOrigin = "", ""
	b := s.byID[id]
	if b == nil {
		return nil, fmt.Errorf("session-controller: merge settle REFUSED — the window named bubble %q, which the store does not hold; the window is closed so it can swallow no further emissions, but the bubble cannot be settled", id)
	}
	s.logf("session-controller: merge window CLOSED bubble=%s ws=%s origin_tool_use_id=%s edge=%s — the session's emissions return to the top-level feed",
		id, s.workspace, origin, edge)
	return s.settleLocked(b, v)
}

// --- the consumer's edges ---------------------------------------------------

// observeMergeSpawn classifies this event's FEED items for the merge skill's
// invocation and opens the window it starts.
//
// It runs BEFORE frontend.StampSpawnedBubbleIDs, which is why the Skill call's
// own card carries spawned_bubble_id in the very delta that announces the call:
// the store already holds the bubble under that call's tool_use id by the time
// the stamp resolves it.
func (c *consumer) observeMergeSpawn(curated frontend.Curation, ev *corev1.Event) asyncPush {
	var push asyncPush
	for _, item := range curated.Feed.GetItems() {
		toolUseID, label, ok := frontend.MergeSkillCallInItem(item)
		if !ok {
			continue
		}
		c.logf("session-controller: merge skill CLASSIFIED session=%s ws=%q seq=%d tool_use_id=%s label=%q — the invocation opens a Merge bubble rather than a synchronous skill card",
			c.sessionID, c.workspace, ev.GetSeq(), toolUseID, label)
		opened, fault, err := c.bubbles.openMergeWindow(toolUseID, label, c.asyncInstant(ev))
		if err != nil {
			c.warn("session-controller: MERGE WINDOW OPEN DEGRADED session=%s ws=%q seq=%d tool_use_id=%s — the merge's conversation will render in the top-level feed rather than in a bubble: %v",
				c.sessionID, c.workspace, ev.GetSeq(), toolUseID, err)
			continue
		}
		if fault != nil {
			push.Faults = append(push.Faults, *fault)
		}
		if opened != nil {
			push.Opened = append(push.Opened, opened)
		}
	}
	return push
}

// foldMergeWindow diverts the session's own feed items into the open Merge
// bubble, and settles the window on the edge that closes it.
//
// IT RUNS LAST, after every curator, and that placement is load-bearing: the
// items it sees are the ones that would have been PUSHED, so a harness meta
// record, a machinery record or a keep-alive turn is already gone and cannot be
// folded into a merge's conversation as though the merge had said it.
//
// WHAT STAYS ON THE FEED, and why each one:
//
//   - the merge Skill call's own result and body, so its card settles normally
//     — the card is where the bubble hangs.
//   - the USER's own next prompt, which is the settle edge itself: it is the
//     user taking the session back, so it belongs to them and to the feed.
//   - any item with no emission arm to carry it (a permission request, a
//     failure card). Dropping those would DELETE them: there is nothing to fold
//     and no second copy anywhere, and a permission prompt that vanishes wedges
//     the session behind a question nobody can answer.
func (c *consumer) foldMergeWindow(cd *frontendv1.ConversationDelta, ev *corev1.Event) asyncPush {
	if !c.bubbles.mergeWindowOpen() {
		return asyncPush{}
	}
	origin := c.bubbles.mergeOriginToolUseID()
	var push asyncPush
	var folded []*frontendv1.AgentEmission
	items := cd.GetItems()
	kept := items[:0]
	for i, item := range items {
		if frontend.ItemBelongsToCall(item, origin) {
			kept = append(kept, item)
			continue
		}
		if item.GetUserMessage() != nil && len(frontend.EmissionsFromItem(item)) == 0 {
			// THE SETTLE EDGE. Every synthetic user record is already withheld by
			// the curators above, and a user record carrying tool results is the
			// harness handing the merge its own results — so what is left is a
			// person typing, which is the user taking the session back.
			push.absorb(c.foldMergeBatch(folded, ev))
			folded = nil
			push.absorb(c.settleMergeWindowOnPrompt(item, ev))
			kept = append(kept, items[i:]...)
			break
		}
		ems := frontend.EmissionsFromItem(item)
		if len(ems) == 0 {
			c.logf("session-controller: merge window RETAINED item on the feed session=%s ws=%q seq=%d uuid=%s — it carries no emission arm a bubble can hold, and folding it would delete it",
				c.sessionID, c.workspace, ev.GetSeq(), item.GetUuid())
			kept = append(kept, item)
			continue
		}
		folded = append(folded, ems...)
	}
	cd.Items = kept
	push.absorb(c.foldMergeBatch(folded, ev))
	return push
}

// foldMergeBatch hands one event's diverted emissions to the store and
// classifies the refusal if it comes back with one.
func (c *consumer) foldMergeBatch(ems []*frontendv1.AgentEmission, ev *corev1.Event) asyncPush {
	if len(ems) == 0 {
		return asyncPush{}
	}
	up, err := c.bubbles.foldMergeEmissions(ems, c.asyncInstant(ev))
	push := asyncPush{}
	gaps, residual := splitAsyncGaps(err)
	push.Faults = append(push.Faults, gaps...)
	if residual != nil {
		c.warn("session-controller: MERGE BUBBLE FOLD DEGRADED session=%s ws=%q seq=%d — %d emission(s) of the merge run will not appear in its bubble, and they have already left the top-level feed: %v",
			c.sessionID, c.workspace, ev.GetSeq(), len(ems), residual)
	}
	if up != nil {
		push.Updates = append(push.Updates, up)
	}
	return push
}

// settleMergeWindowOnPrompt settles the window on the user's own next prompt —
// the boundary async-bubble.proto names as the merge's ending.
//
// DONE, not error: the user typing again is the window ending, not the merge
// failing. A merge that actually failed said so inside its own conversation,
// which the bubble holds.
func (c *consumer) settleMergeWindowOnPrompt(item *frontendv1.ConversationItem, ev *corev1.Event) asyncPush {
	up, err := c.bubbles.settleMergeWindow(frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE,
		AtMs:   c.asyncInstant(ev),
	}, "user_prompt")
	return c.mergeSettlePush(up, err, fmt.Sprintf("seq=%d uuid=%s", ev.GetSeq(), item.GetUuid()))
}

// settleMergeWindowOnInterrupt settles the window on a user-commanded stop.
//
// KILLED, not done, and that verdict is the machinery's own: an interrupt is
// TERMINAL_STATUS_STOPPED, which frontend.SettleAsyncBubble resolves to the
// killed arm — "the work did not fail, it was not allowed to conclude". The
// daemon does not re-decide that mapping here.
//
// It is reached from the ONE user-commanded stop (Manager.Interrupt), so an
// interject's machinery stop structurally cannot close a user's merge window.
func (c *consumer) settleMergeWindowOnInterrupt(reason string) {
	if !c.bubbles.mergeWindowOpen() {
		return
	}
	up, err := c.bubbles.settleMergeWindow(frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED,
		AtMs:   c.now(),
		Reason: reason,
	}, "interrupt")
	push := c.mergeSettlePush(up, err, "reason="+reason)
	if push.empty() {
		return
	}
	// The stop arrives on a control path, not on a store event, so the push
	// carries the newest seq the session has SEEN rather than a seq of its own:
	// through_seq is the client's replay cursor and inventing a number ahead of
	// the stream would move it past events it never received.
	c.publishMergeSettle(push, c.newestRetainedSeq())
}

// mergeSettlePush turns one settlement into its push, classifying a refusal
// exactly as every other async refusal is classified.
func (c *consumer) mergeSettlePush(up *frontendv1.AsyncBubbleUpdate, err error, where string) asyncPush {
	var push asyncPush
	gaps, residual := splitAsyncGaps(err)
	push.Faults = append(push.Faults, gaps...)
	if residual != nil {
		c.warn("session-controller: MERGE BUBBLE SETTLE DEGRADED session=%s ws=%q %s — the merge window is closed so the feed is restored, but its bubble will render as still live: %v",
			c.sessionID, c.workspace, where, residual)
	}
	if up != nil {
		push.Updates = append(push.Updates, up)
	}
	return push
}

// publishMergeSettle pushes a settlement that no store event produced.
//
// It goes out through the SAME frame every async push uses, fenced and
// sequenced identically, so a reconnecting client applies it with the same
// staleness rule as any other push.
func (c *consumer) publishMergeSettle(push asyncPush, throughSeq uint64) {
	for _, fault := range push.Faults {
		c.warn("session-controller: MERGE SETTLE FAULT session=%s ws=%q card_uuid=%s — %s",
			c.sessionID, c.workspace, fault.UUID, fault.Detail)
	}
	if len(push.Updates) == 0 {
		return
	}
	c.logf("session-controller: merge settle push session=%s ws=%q through_seq=%d updates=%s",
		c.sessionID, c.workspace, throughSeq, updatedBubbleIDs(push.Updates))
	c.push.PushAsyncBubbleDelta(&frontendv1.AsyncBubbleDelta{
		Workspace:  c.workspace,
		Updates:    push.Updates,
		ThroughSeq: throughSeq,
		Fence:      c.fence(),
	})
}
