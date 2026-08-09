// asyncwindows.go is the WINDOW APPARATUS: the span of a session between a
// skill's invocation and the user taking the session back, during which every
// emission the session produces belongs to that invocation's bubble rather than
// to the top-level feed.
//
// TWO KINDS, ONE RULE. `/create-or-update-workspace merge` opens a MERGE bubble;
// every other Skill invocation opens a SKILL bubble. async-bubble.proto draws
// exactly that line — "`merge` is the one skill with an arm of its own …  every
// other skill arrives as `skill`" — and both arms describe their membership with
// the same sentence: the window between the classification and the user's own
// next prompt or an interrupt. So the two kinds are two CONFIGURATIONS of one
// window here, not two mechanisms.
//
// WHY MEMBERSHIP IS TEMPORAL. Every other bubble on this contract owns its
// content by a JOIN: a sidechain record names the call that dispatched it, a
// retrieval names its task id. A window has no such key — the work drives THIS
// workspace's own session, so its records are the session's ordinary top-level
// records and are indistinguishable from the user's own conversation by anything
// on the record itself. What separates them is WHEN they happened, and so the
// window is the membership rule.
//
// THE THREE EDGES.
//
//   - OPEN, at the classification of the Skill call (frontend.SkillCall). The
//     bubble is minted through the same frontend.OpenAsyncBubble every other
//     kind goes through, and filed under the call's tool_use id, which is what
//     makes StampSpawnedBubbleIDs stamp the card exactly as an agent spawn
//     stamps its own.
//   - FOLD, for every item of every later event, until an edge closes the
//     window. The Skill calls' OWN cards are the one exception: a call's result
//     and its body settle the card on the feed, because the card is what the
//     bubble hangs under.
//   - SETTLE, on the user's own next prompt or on an interrupt — for EVERY open
//     window at once, because both edges are the user taking the whole session
//     back rather than closing one frame of it.
//
// WHY SKILLS NEST AND MERGES DO NOT. Skills legitimately chain: a skill's own
// conversation invokes another skill, and the inner one is a real child of the
// outer. So the open windows are a STACK, the innermost captures, and the child
// carries parent_bubble_id — which is exactly the tree the contract describes,
// expressed by parent pointers rather than by nesting bubbles inside each other.
// A second MERGE while one is open is a different thing entirely and stays the
// daemon fault it always was: two merge windows over one session would both
// claim every emission with no rule that could assign one to either.
//
// HOW THE FOLD REACHES THE CLIENT. By APPEND, on the contract's own update arms
// — `merge = 15` and `skill = 16`. An earlier shape re-delivered the whole
// bubble through AsyncBubbleDelta.opened, which existed only because no such arm
// did; the update oneof's own rule ("Never a re-send of the whole bubble") is
// what retires it. See frontend.AppendWindowEmissions.
package sessioncontroller

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// --- the store's window state ----------------------------------------------

// asyncWindow is one open window: the bubble the session's emissions fold into,
// the call that opened it, and which kind it is.
//
// The kind is held rather than re-derived from the bubble because it is what
// decides the OPENING rule — merge faults on a second, skill nests — and a rule
// that read the bubble back would be asking the wire what the daemon already
// resolved.
type asyncWindow struct {
	bubbleID string
	origin   string
	kind     frontend.DetachKind
}

// windowsOpen reports whether any window currently owns the session's
// emissions.
func (s *asyncBubbleStore) windowsOpen() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return len(s.windows) > 0
}

// mergeWindowLocked reports the open MERGE window, nil when none is open. It is
// the merge kind's own query because the merge kind has its own opening rule —
// one at a time — and nothing else on the stack constrains that.
func (s *asyncBubbleStore) mergeWindowLocked() *asyncWindow {
	for i := range s.windows {
		if s.windows[i].kind == frontend.DetachMerge {
			return &s.windows[i]
		}
	}
	return nil
}

// windowOriginToolUseIDs reports the calls every open window was opened by.
//
// EVERY window's, not just the innermost one's: each of those cards is where a
// bubble hangs, and folding one away would leave its bubble anchored to a card
// the reader never sees.
func (s *asyncBubbleStore) windowOriginToolUseIDs() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]string, 0, len(s.windows))
	for _, w := range s.windows {
		out = append(out, w.origin)
	}
	return out
}

// openMergeWindow opens the Merge bubble for one classified merge invocation.
//
// A SECOND MERGE INVOCATION WHILE ONE IS OPEN IS A DAEMON FAULT, surfaced as a
// failure card through the very same path an unattributable detachment takes.
// It is NOT nested and it is NOT allowed to replace the open window, and the
// reason is what each alternative would do to the conversation: nesting would
// need a second temporal window inside the first, and since both merge windows
// claim every emission of one session there is nothing that could ever assign an
// emission to one rather than the other; replacing would strand the first bubble
// live forever, since the settle edge that was going to close it now closes the
// replacement. Refusing keeps the open window's account of the session intact,
// keeps the second invocation's own records in that account (they are emissions
// inside the window, and fold there), and says loudly on the conversation that
// the daemon saw something it cannot model.
//
// A merge opening INSIDE an open skill window is a different case and is
// allowed: the skill window is the merge's genuine parent, the merge captures
// from there as the innermost window, and the two do not compete for the same
// emissions because only the innermost claims them.
//
// A REPLAY OF THE SAME INVOCATION IS NOT A SECOND ONE. The bubble id is derived
// from the originating call, so a re-consumed event finds the bubble it already
// opened; it re-adopts the window rather than faulting or opening a twin.
func (s *asyncBubbleStore) openMergeWindow(originToolUseID, label string, atMs int64) (*frontendv1.AsyncBubble, *asyncFault, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, readopted := s.readoptWindowLocked(originToolUseID, frontend.DetachMerge); readopted {
		return nil, nil, nil
	}
	if open := s.mergeWindowLocked(); open != nil {
		return nil, s.faultLocked(originToolUseID, fmt.Sprintf(
			"the merge skill was invoked on call %q while the merge run opened by call %q (bubble %q) is still open. A merge window claims EVERY emission of this session until the user takes it back, so two open merge windows have no rule that could assign an emission to one rather than the other: the second invocation is refused, its own records fold into the window already open, and this card is the daemon saying so rather than nesting one merge inside another",
			originToolUseID, open.origin, open.bubbleID)), nil
	}
	return s.openWindowLocked(frontend.BubbleSpec{
		// The originating call IS the identity: no task id exists for a merge
		// (nothing detached), and deriving the bubble id from the call is what
		// makes a replay resolve to the same bubble instead of a twin.
		TaskID:          originToolUseID,
		Kind:            frontend.DetachMerge,
		OriginToolUseID: originToolUseID,
		Label:           label,
		StartedAtMs:     atMs,
	})
}

// openSkillWindow opens the Skill bubble for one classified skill invocation.
//
// A SKILL INVOKED INSIDE AN OPEN WINDOW IS A CHILD, not a fault: skills chain,
// and the inner invocation is genuinely work the outer one started. It takes the
// innermost open window as its parent and becomes the innermost window itself,
// so the emissions that follow belong to it until it settles.
func (s *asyncBubbleStore) openSkillWindow(inv frontend.SkillInvocation, atMs int64) (*frontendv1.AsyncBubble, *asyncFault, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, readopted := s.readoptWindowLocked(inv.ToolUseID, frontend.DetachSkill); readopted {
		return nil, nil, nil
	}
	return s.openWindowLocked(frontend.BubbleSpec{
		TaskID:          inv.ToolUseID,
		Kind:            frontend.DetachSkill,
		OriginToolUseID: inv.ToolUseID,
		SkillName:       inv.SkillName,
		Args:            inv.Args,
		Label:           inv.Label,
		StartedAtMs:     atMs,
	})
}

// readoptWindowLocked handles a REPLAY of a classifying event: the bubble is
// already in the store under this call, so the window resumes on it rather than
// faulting or opening a twin. It reports nothing for a call the store has never
// seen.
func (s *asyncBubbleStore) readoptWindowLocked(originToolUseID string, kind frontend.DetachKind) (*frontendv1.AsyncBubble, bool) {
	b := s.lookupLocked(originToolUseID)
	if b == nil {
		return nil, false
	}
	for _, w := range s.windows {
		if w.bubbleID == b.GetId() {
			return b, true // already the open window; nothing to re-adopt
		}
	}
	s.windows = append(s.windows, asyncWindow{bubbleID: b.GetId(), origin: originToolUseID, kind: kind})
	s.logf("session-controller: %s window RE-ADOPTED bubble=%s ws=%s origin_tool_use_id=%s depth=%d — the classifying event was consumed again and the fold resumes on the bubble it already opened",
		kind, b.GetId(), s.workspace, originToolUseID, len(s.windows))
	return b, true
}

// openWindowLocked mints one window's bubble and pushes it onto the stack.
//
// It is the ONE site both kinds go through, so the parent pointer, the stack
// discipline and the record below cannot differ between them. The parent is the
// INNERMOST open window: a window opened inside another is that one's child, and
// resolving it from the stack rather than from parentByToolUse is what makes the
// pointer right even though the opening call has not been indexed as an emission
// yet (classification runs before the fold that indexes it).
func (s *asyncBubbleStore) openWindowLocked(spec frontend.BubbleSpec) (*frontendv1.AsyncBubble, *asyncFault, error) {
	spec.Workspace = s.workspace
	if parent := s.innermostWindowLocked(); parent != nil {
		spec.ParentBubbleID = parent.bubbleID
	} else {
		spec.ParentBubbleID = s.parentByToolUse[spec.OriginToolUseID]
	}
	b, err := frontend.OpenAsyncBubble(spec)
	if err != nil {
		return nil, s.faultLocked(spec.OriginToolUseID, err.Error()), nil
	}
	s.adoptLocked(b, spec.OriginToolUseID, spec.OriginToolUseID)
	s.windows = append(s.windows, asyncWindow{bubbleID: b.GetId(), origin: spec.OriginToolUseID, kind: spec.Kind})
	s.logf("session-controller: %s window OPENED bubble=%s ws=%s origin_tool_use_id=%s parent_bubble_id=%q label=%q started_at_ms=%d depth=%d — every emission of this session folds here until the user's next prompt or an interrupt",
		spec.Kind, b.GetId(), s.workspace, spec.OriginToolUseID, spec.ParentBubbleID, spec.Label, spec.StartedAtMs, len(s.windows))
	return b, nil, nil
}

// innermostWindowLocked is the window that CAPTURES: the deepest one open.
func (s *asyncBubbleStore) innermostWindowLocked() *asyncWindow {
	if len(s.windows) == 0 {
		return nil
	}
	return &s.windows[len(s.windows)-1]
}

// foldWindowEmissions folds one batch of the session's emissions into the
// INNERMOST open window and returns the incremental update that carries them.
//
// DEEPEST WINS, and there is nothing to arbitrate: an emission produced while a
// skill invoked inside another skill is running belongs to the inner one, which
// is the whole reason the stack is a stack. The outer window still holds it
// transitively, through the child's parent pointer.
//
// It also INDEXES the calls the batch made, which is what gives a subagent
// dispatched inside the window its parent pointer: a nested detachment's bubble
// resolves its parent through parentByToolUse exactly as a detached agent's
// nested dispatch does, so the tree reads truthfully rather than hanging the
// child at the top level beside the work that started it.
func (s *asyncBubbleStore) foldWindowEmissions(ems []*frontendv1.AgentEmission, atMs int64) (*frontendv1.AsyncBubbleUpdate, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	w := s.innermostWindowLocked()
	if w == nil {
		return nil, nil
	}
	b := s.byID[w.bubbleID]
	if b == nil {
		return nil, fmt.Errorf("session-controller: window fold REFUSED — the open %s window names bubble %q, which the store does not hold; the emissions are not folded rather than being attributed to a bubble nobody can route to", w.kind, w.bubbleID)
	}
	s.indexCallsLocked(ems, b.GetId())
	up, err := frontend.AppendWindowEmissions(b, ems, atMs)
	if err != nil || up == nil {
		return nil, err
	}
	folded, dropped := windowFoldCounts(b)
	s.logf("session-controller: window fold append bubble=%s kind=%s ws=%s appended_emissions=%d folded_emissions=%d dropped_before=%d depth=%d delivery=update_arm",
		b.GetId(), frontend.AsyncBubbleKind(b), s.workspace, len(ems), folded, dropped, len(s.windows))
	return up, nil
}

// windowFoldCounts reads a window bubble's fold accounting off whichever arm it
// carries, so one record serves both kinds rather than each having its own.
func windowFoldCounts(b *frontendv1.AsyncBubble) (folded int, dropped int64) {
	if m := b.GetMerge(); m != nil {
		return len(m.GetEmissions()), m.GetFold().GetDroppedBefore()
	}
	sk := b.GetSkill()
	return len(sk.GetEmissions()), sk.GetFold().GetDroppedBefore()
}

// skillWindowBubbleID reports the SKILL bubble one call opened, empty when that
// call opened none. It is what tells the body curator whether a skill's contents
// have a bubble to land in.
func (s *asyncBubbleStore) skillWindowBubbleID(originToolUseID string) string {
	s.mu.Lock()
	defer s.mu.Unlock()
	b := s.lookupLocked(originToolUseID)
	if b == nil || b.GetSkill() == nil {
		return ""
	}
	return b.GetId()
}

// resolveSkillWindowBody delivers a skill file's contents as its bubble's own
// body.
//
// THE BODY IS NOT AN EMISSION. It is the SKILL's content rather than something
// the conversation said, which is why it has its own arm and its own field, and
// why it is delivered here rather than folded through foldWindowEmissions.
func (s *asyncBubbleStore) resolveSkillWindowBody(originToolUseID, contents string) (*frontendv1.AsyncBubbleUpdate, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	b := s.lookupLocked(originToolUseID)
	if b == nil {
		return nil, fmt.Errorf("session-controller: skill body REFUSED — call %q opened no bubble the body could land in, so it is not delivered rather than being attributed to a bubble nobody can route to", originToolUseID)
	}
	up, err := frontend.ResolveSkillBody(b, contents)
	if err != nil {
		return nil, err
	}
	s.logf("session-controller: skill body RESOLVED bubble=%s ws=%s origin_tool_use_id=%s len=%d — the skill file's contents are the bubble's own body, and no longer a card of their own",
		b.GetId(), s.workspace, originToolUseID, len(contents))
	return up, nil
}

// settleWindows settles EVERY open window and empties the stack, reporting
// nothing when none is open.
//
// ALL OF THEM, INNERMOST FIRST. Both settling edges are the user taking the
// whole session back, not the ending of one frame of it: a prompt typed while
// three skills are nested ends all three, and leaving the outer ones open would
// go on swallowing the user's own conversation into work they already walked
// away from.
//
// CLOSING IS UNCONDITIONAL ONCE THE EDGE IS REACHED, even if a settlement itself
// is refused: a window left open on a bubble that failed to settle would go on
// swallowing the user's own conversation, which is a far worse failure than the
// unsettled bubble the error already reports.
func (s *asyncBubbleStore) settleWindows(v frontend.AsyncVerdict, edge string) ([]*frontendv1.AsyncBubbleUpdate, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if len(s.windows) == 0 {
		return nil, nil
	}
	windows := s.windows
	s.windows = nil
	var updates []*frontendv1.AsyncBubbleUpdate
	var errs []error
	for i := len(windows) - 1; i >= 0; i-- {
		w := windows[i]
		b := s.byID[w.bubbleID]
		if b == nil {
			errs = append(errs, fmt.Errorf("session-controller: window settle REFUSED — the open %s window named bubble %q, which the store does not hold; the window is closed so it can swallow no further emissions, but the bubble cannot be settled", w.kind, w.bubbleID))
			continue
		}
		up, err := s.settleLocked(b, v)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		s.logf("session-controller: %s window CLOSED bubble=%s ws=%s origin_tool_use_id=%s edge=%s — the session's emissions return to the window outside it, or to the top-level feed",
			w.kind, w.bubbleID, s.workspace, w.origin, edge)
		updates = append(updates, up)
	}
	return updates, joinAsyncErrors(errs)
}

// --- the consumer's edges ---------------------------------------------------

// observeSkillSpawn classifies this event's FEED items for skill invocations and
// opens the windows they start.
//
// It runs BEFORE frontend.StampSpawnedBubbleIDs, which is why a Skill call's own
// card carries spawned_bubble_id in the very delta that announces the call: the
// store already holds the bubble under that call's tool_use id by the time the
// stamp resolves it.
//
// ONE PASS DECIDES BOTH KINDS (frontend.SkillCall), so merge detection and skill
// detection cannot disagree about the same call.
func (c *consumer) observeSkillSpawn(curated frontend.Curation, ev *corev1.Event) asyncPush {
	var push asyncPush
	for _, item := range curated.Feed.GetItems() {
		for _, use := range frontend.SkillToolCallsInItem(item) {
			inv, ok := frontend.SkillCall(use)
			if !ok {
				// A NAMELESS SKILL CALL IS SAID OUT LOUD. It opens no bubble —
				// there is nothing to label it or fold under — and its card still
				// renders on the feed, so the invocation is visible even though
				// the daemon cannot model it as work.
				c.warn("session-controller: SKILL CALL NOT CLASSIFIED session=%s ws=%q seq=%d tool_use_id=%s — the call's input named no skill, so no bubble is opened for it and its conversation stays in the top-level feed; the card still renders",
					c.sessionID, c.workspace, ev.GetSeq(), use.GetId())
				continue
			}
			push.absorb(c.openSkillInvocationWindow(inv, ev))
		}
	}
	return push
}

// openSkillInvocationWindow opens the one bubble a classified invocation earns —
// Merge for the merge run, Skill for every other invocation.
func (c *consumer) openSkillInvocationWindow(inv frontend.SkillInvocation, ev *corev1.Event) asyncPush {
	kind := frontend.DetachSkill
	if inv.IsMerge {
		kind = frontend.DetachMerge
	}
	c.logf("session-controller: skill CLASSIFIED session=%s ws=%q seq=%d tool_use_id=%s skill=%q kind=%s label=%q — the invocation opens a bubble that owns the session's emissions rather than a synchronous card",
		c.sessionID, c.workspace, ev.GetSeq(), inv.ToolUseID, inv.SkillName, kind, inv.Label)
	var (
		opened *frontendv1.AsyncBubble
		fault  *asyncFault
		err    error
	)
	if inv.IsMerge {
		opened, fault, err = c.bubbles.openMergeWindow(inv.ToolUseID, inv.Label, c.asyncInstant(ev))
	} else {
		opened, fault, err = c.bubbles.openSkillWindow(inv, c.asyncInstant(ev))
	}
	if err != nil {
		c.warn("session-controller: %s WINDOW OPEN DEGRADED session=%s ws=%q seq=%d tool_use_id=%s — the invocation's conversation will render in the top-level feed rather than in a bubble: %v",
			kind, c.sessionID, c.workspace, ev.GetSeq(), inv.ToolUseID, err)
		return asyncPush{}
	}
	var push asyncPush
	if fault != nil {
		push.Faults = append(push.Faults, *fault)
	}
	if opened != nil {
		push.Opened = append(push.Opened, opened)
	}
	return push
}

// foldWindows diverts the session's own feed items into the innermost open
// window, and settles every open window on the edge that closes them.
//
// IT RUNS LAST, after every curator, and that placement is load-bearing: the
// items it sees are the ones that would have been PUSHED, so a harness meta
// record, a machinery record or a keep-alive turn is already gone and cannot be
// folded into a window's conversation as though the work had said it.
//
// WHAT STAYS ON THE FEED, and why each one:
//
//   - every open window's Skill call's own result and body, so its card settles
//     normally — the card is where the bubble hangs.
//   - the USER's own next prompt, which is the settle edge itself: it is the
//     user taking the session back, so it belongs to them and to the feed.
//   - any item with no emission arm to carry it (a permission request, a
//     failure card). Dropping those would DELETE them: there is nothing to fold
//     and no second copy anywhere, and a permission prompt that vanishes wedges
//     the session behind a question nobody can answer.
func (c *consumer) foldWindows(cd *frontendv1.ConversationDelta, ev *corev1.Event) asyncPush {
	if !c.bubbles.windowsOpen() {
		return asyncPush{}
	}
	origins := c.bubbles.windowOriginToolUseIDs()
	var push asyncPush
	var folded []*frontendv1.AgentEmission
	items := cd.GetItems()
	kept := items[:0]
	for i, item := range items {
		if itemBelongsToAnyCall(item, origins) {
			kept = append(kept, item)
			continue
		}
		if item.GetUserMessage() != nil && len(frontend.EmissionsFromItem(item)) == 0 {
			// THE SETTLE EDGE. Every synthetic user record is already withheld by
			// the curators above, and a user record carrying tool results is the
			// harness handing the work its own results — so what is left is a
			// person typing, which is the user taking the session back.
			push.absorb(c.foldWindowBatch(folded, ev))
			folded = nil
			push.absorb(c.settleWindowsOnPrompt(item, ev))
			kept = append(kept, items[i:]...)
			break
		}
		ems := frontend.EmissionsFromItem(item)
		if len(ems) == 0 {
			c.logf("session-controller: window RETAINED item on the feed session=%s ws=%q seq=%d uuid=%s — it carries no emission arm a bubble can hold, and folding it would delete it",
				c.sessionID, c.workspace, ev.GetSeq(), item.GetUuid())
			kept = append(kept, item)
			continue
		}
		folded = append(folded, ems...)
	}
	cd.Items = kept
	push.absorb(c.foldWindowBatch(folded, ev))
	return push
}

// itemBelongsToAnyCall reports whether one item is part of the card of ANY open
// window's invocation.
func itemBelongsToAnyCall(item *frontendv1.ConversationItem, toolUseIDs []string) bool {
	for _, id := range toolUseIDs {
		if frontend.ItemBelongsToCall(item, id) {
			return true
		}
	}
	return false
}

// foldWindowBatch hands one event's diverted emissions to the store and
// classifies the refusal if it comes back with one.
func (c *consumer) foldWindowBatch(ems []*frontendv1.AgentEmission, ev *corev1.Event) asyncPush {
	if len(ems) == 0 {
		return asyncPush{}
	}
	up, err := c.bubbles.foldWindowEmissions(ems, c.asyncInstant(ev))
	push := asyncPush{}
	gaps, residual := splitAsyncGaps(err)
	push.Faults = append(push.Faults, gaps...)
	if residual != nil {
		c.warn("session-controller: WINDOW BUBBLE FOLD DEGRADED session=%s ws=%q seq=%d — %d emission(s) of the window's work will not appear in its bubble, and they have already left the top-level feed: %v",
			c.sessionID, c.workspace, ev.GetSeq(), len(ems), residual)
	}
	if up != nil {
		push.Updates = append(push.Updates, up)
	}
	return push
}

// resolveSkillBodyIntoWindow delivers one skill's file contents to the bubble
// that invocation opened, and reports the push that carries them.
func (c *consumer) resolveSkillBodyIntoWindow(originToolUseID, contents string, seq uint64) asyncPush {
	up, err := c.bubbles.resolveSkillWindowBody(originToolUseID, contents)
	var push asyncPush
	gaps, residual := splitAsyncGaps(err)
	push.Faults = append(push.Faults, gaps...)
	if residual != nil {
		c.warn("session-controller: SKILL BODY DELIVERY DEGRADED session=%s ws=%q seq=%d tool_use_id=%s — the skill's own body will not appear in its bubble: %v",
			c.sessionID, c.workspace, seq, originToolUseID, residual)
	}
	if up != nil {
		push.Updates = append(push.Updates, up)
	}
	return push
}

// settleWindowsOnPrompt settles every open window on the user's own next prompt
// — the boundary async-bubble.proto names as a window's ending.
//
// DONE, not error: the user typing again is the window ending, not the work
// failing. Work that actually failed said so inside its own conversation, which
// the bubble holds.
func (c *consumer) settleWindowsOnPrompt(item *frontendv1.ConversationItem, ev *corev1.Event) asyncPush {
	ups, err := c.bubbles.settleWindows(frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE,
		AtMs:   c.asyncInstant(ev),
	}, "user_prompt")
	return c.windowSettlePush(ups, err, fmt.Sprintf("seq=%d uuid=%s", ev.GetSeq(), item.GetUuid()))
}

// settleWindowsOnInterrupt settles every open window on a user-commanded stop.
//
// KILLED, not done, and that verdict is the machinery's own: an interrupt is
// TERMINAL_STATUS_STOPPED, which frontend.SettleAsyncBubble resolves to the
// killed arm — "the work did not fail, it was not allowed to conclude". The
// daemon does not re-decide that mapping here.
//
// It is reached from the ONE user-commanded stop (Manager.Interrupt), so an
// interject's machinery stop structurally cannot close a user's window.
func (c *consumer) settleWindowsOnInterrupt(reason string) {
	if !c.bubbles.windowsOpen() {
		return
	}
	ups, err := c.bubbles.settleWindows(frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED,
		AtMs:   c.now(),
		Reason: reason,
	}, "interrupt")
	push := c.windowSettlePush(ups, err, "reason="+reason)
	if push.empty() {
		return
	}
	// The stop arrives on a control path, not on a store event, so the push
	// carries the newest seq the session has SEEN rather than a seq of its own:
	// through_seq is the client's replay cursor and inventing a number ahead of
	// the stream would move it past events it never received.
	c.publishWindowSettle(push, c.newestRetainedSeq())
}

// windowSettlePush turns one settlement into its push, classifying a refusal
// exactly as every other async refusal is classified.
func (c *consumer) windowSettlePush(ups []*frontendv1.AsyncBubbleUpdate, err error, where string) asyncPush {
	var push asyncPush
	gaps, residual := splitAsyncGaps(err)
	push.Faults = append(push.Faults, gaps...)
	if residual != nil {
		c.warn("session-controller: WINDOW BUBBLE SETTLE DEGRADED session=%s ws=%q %s — the window is closed so the feed is restored, but its bubble will render as still live: %v",
			c.sessionID, c.workspace, where, residual)
	}
	push.Updates = append(push.Updates, ups...)
	return push
}

// publishWindowSettle pushes a settlement that no store event produced.
//
// It goes out through the SAME frame every async push uses, fenced and
// sequenced identically, so a reconnecting client applies it with the same
// staleness rule as any other push.
func (c *consumer) publishWindowSettle(push asyncPush, throughSeq uint64) {
	for _, fault := range push.Faults {
		c.warn("session-controller: WINDOW SETTLE FAULT session=%s ws=%q card_uuid=%s — %s",
			c.sessionID, c.workspace, fault.UUID, fault.Detail)
	}
	if len(push.Updates) == 0 {
		return
	}
	c.logf("session-controller: window settle push session=%s ws=%q through_seq=%d updates=%s",
		c.sessionID, c.workspace, throughSeq, updatedBubbleIDs(push.Updates))
	c.push.PushAsyncBubbleDelta(&frontendv1.AsyncBubbleDelta{
		Workspace:  c.workspace,
		Updates:    push.Updates,
		ThroughSeq: throughSeq,
		Fence:      c.fence(),
	})
}
