// The MERGE WINDOW, driven through the store directly and through the real
// consumer: the merge skill's invocation opens a Merge bubble, every emission
// of the session folds there until the user takes the session back, and the
// window's two closing edges — the user's own next prompt, and an interrupt —
// settle it.
package sessioncontroller

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"

	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// --- fixtures ---------------------------------------------------------------

const mergeOriginCall = "toolu_merge"

// mergeSkillEmission is the assistant emission making one Skill call, as
// CurateEvent hands it over.
func mergeSkillEmission(t *testing.T, toolUseID, skill, args string) *frontendv1.ConversationItem {
	t.Helper()
	input, err := structpb.NewStruct(map[string]any{"skill": skill, "args": args})
	if err != nil {
		t.Fatalf("structpb.NewStruct: %v", err)
	}
	return &frontendv1.ConversationItem{
		Uuid: "a-" + toolUseID,
		Item: &frontendv1.ConversationItem_Agent{Agent: &frontendv1.AgentEmission{
			Emission: &frontendv1.AgentEmission_Response{Response: &frontendv1.AgentResponse{
				Body: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: toolUseID, Name: "Skill", Input: input}}},
				}},
			}},
		}},
	}
}

// mergeSkillCallEvent is the transcript record making one Skill call, for the
// consumer-level tests.
func mergeSkillCallEvent(t *testing.T, seq uint64, uuid, toolUseID, skill, args string) *corev1.Event {
	t.Helper()
	input, err := structpb.NewStruct(map[string]any{"skill": skill, "args": args})
	if err != nil {
		t.Fatalf("structpb.NewStruct: %v", err)
	}
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: toolUseID, Name: "Skill", Input: input}}},
			}},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// assistantTextEvent is one ordinary assistant utterance.
func assistantTextEvent(t *testing.T, seq uint64, uuid, text string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
			}},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// userPromptEvent is a person typing: an unflagged user record carrying prose.
func userPromptEvent(t *testing.T, seq uint64, uuid, text string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message:  &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{ContentString: text}},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// mergeEmissions is one batch of a session's own emissions.
func mergeEmissions(text string) []*frontendv1.AgentEmission {
	return []*frontendv1.AgentEmission{{Emission: &frontendv1.AgentEmission_Response{
		Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
		}}},
	}}}
}

// openWindow opens a merge window on a bare store and returns the bubble.
func openWindow(t *testing.T, s *asyncBubbleStore) *frontendv1.AsyncBubble {
	t.Helper()
	b, fault, err := s.openMergeWindow(mergeOriginCall, "/create-or-update-workspace merge", 10)
	if err != nil {
		t.Fatalf("openMergeWindow: %v", err)
	}
	if fault != nil {
		t.Fatalf("openMergeWindow faulted on the first invocation: %s", fault.Detail)
	}
	if b == nil {
		t.Fatal("the first invocation must open a bubble")
	}
	return b
}

// mergeBubbles returns every merge-kind bubble the frontend was OPENED with.
// A merge bubble is opened once and advances by its update arm thereafter, so
// this list has one entry per merge run.
func (h *queueHarness) mergeBubbles() []*frontendv1.AsyncBubble {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.AsyncBubble
	for _, d := range h.push.bubbles {
		for _, b := range d.GetOpened() {
			if b.GetMerge() != nil {
				out = append(out, b)
			}
		}
	}
	return out
}

// mergeAppends returns the assistant prose every MERGE-ARM update addressed to
// bubbleID carried, in order. It reads the wire arm rather than the store's own
// bubble object, which is the only way to tell an update that shipped from a
// fold that merely happened.
func (h *queueHarness) mergeAppends(bubbleID string) []string {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []string
	for _, d := range h.push.bubbles {
		for _, u := range d.GetUpdates() {
			if u.GetBubbleId() != bubbleID || u.GetMerge() == nil {
				continue
			}
			for _, em := range u.GetMerge().GetEmissions() {
				for _, block := range em.GetResponse().GetBody().GetContent() {
					if text := block.GetText().GetText(); text != "" {
						out = append(out, text)
					}
				}
			}
		}
	}
	return out
}

// mergeLiveness returns the liveness updates addressed to bubbleID, in order.
func (h *queueHarness) mergeLiveness(bubbleID string) []*frontendv1.AsyncLiveness {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.AsyncLiveness
	for _, d := range h.push.bubbles {
		for _, u := range d.GetUpdates() {
			if u.GetBubbleId() == bubbleID && u.GetLiveness() != nil {
				out = append(out, u.GetLiveness().GetLiveness())
			}
		}
	}
	return out
}

// feedTexts returns the assistant prose the frontend was pushed on the FEED.
func (h *queueHarness) feedTexts() []string {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []string
	for _, cd := range h.push.convo {
		for _, it := range cd.GetItems() {
			for _, block := range it.GetAgent().GetResponse().GetBody().GetContent() {
				if text := block.GetText().GetText(); text != "" {
					out = append(out, text)
				}
			}
		}
	}
	return out
}

// --- the opening edge -------------------------------------------------------

func TestOpenMergeWindowGivesTheBubbleTheMergeArm(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	b := openWindow(t, s)

	// Assert
	if b.GetMerge() == nil {
		t.Fatalf("a merge run opened on arm %T, want the merge arm the contract gives it", b.GetKind())
	}
}

func TestOpenMergeWindowFilesTheBubbleUnderItsSpawningCall(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	b := openWindow(t, s)

	// Assert: this lookup IS what StampSpawnedBubbleIDs stamps on the card.
	if got := s.spawnedBubbleID(mergeOriginCall); got != b.GetId() {
		t.Fatalf("spawnedBubbleID(%q) = %q, want the merge bubble %q: the card's spawned_bubble_id is this one resolution",
			mergeOriginCall, got, b.GetId())
	}
}

func TestOpenMergeWindowAnchorsTheBubbleOnItsSpawningCall(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	b := openWindow(t, s)

	// Assert
	if got := b.GetOriginToolUseId(); got != mergeOriginCall {
		t.Fatalf("origin_tool_use_id = %q, want the Skill call %q the bubble hangs under", got, mergeOriginCall)
	}
}

func TestOpenMergeWindowOpensLive(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	b := openWindow(t, s)

	// Assert
	if b.GetLiveness().GetLive() == nil {
		t.Fatalf("a merge window opened with liveness %v, want the live arm", b.GetLiveness().GetState())
	}
}

func TestOpenMergeWindowFaultsOnASecondInvocationWhileOneIsOpen(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	first := openWindow(t, s)

	// Act
	b, fault, err := s.openMergeWindow("toolu_second", "/create-or-update-workspace merge again", 11)
	if err != nil {
		t.Fatalf("openMergeWindow: %v", err)
	}

	// Assert
	if fault == nil {
		t.Fatal("a second merge invocation while one is open has no representable membership rule and must be a classified fault")
	}
	if b != nil {
		t.Fatalf("the second invocation opened bubble %q; the first window must stand", b.GetId())
	}
	if got := s.spawnedBubbleID(mergeOriginCall); got != first.GetId() {
		t.Errorf("the open window moved to %q, want the first bubble %q left untouched", got, first.GetId())
	}
}

func TestOpenMergeWindowReadoptsItsOwnInvocationOnReplay(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	first := openWindow(t, s)

	// Act — the same classifying event consumed a second time.
	b, fault, err := s.openMergeWindow(mergeOriginCall, "/create-or-update-workspace merge", 10)
	if err != nil {
		t.Fatalf("openMergeWindow: %v", err)
	}

	// Assert
	if fault != nil {
		t.Fatalf("a replay of one invocation is not a second one: %s", fault.Detail)
	}
	if b != nil {
		t.Fatalf("a replay opened a twin bubble %q", b.GetId())
	}
	if got := len(s.snapshot()); got != 1 {
		t.Errorf("the store holds %d bubbles after a replay, want the one merge bubble %q", got, first.GetId())
	}
}

// --- the fold ---------------------------------------------------------------

func TestFoldMergeEmissionsFoldsIntoTheOpenBubble(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	b := openWindow(t, s)

	// Act
	if _, err := s.foldMergeEmissions(mergeEmissions("working on it"), 11); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := len(b.GetMerge().GetEmissions()); got != 1 {
		t.Fatalf("the merge bubble holds %d emissions, want the one that folded", got)
	}
}

// AMENDED: this test pinned whole-bubble re-delivery, the interim shape the
// window advanced by before the contract had an arm for it. The update oneof's
// own rule — "Never a re-send of the whole bubble" — is what retires that
// mechanism, and `merge = 15` is the arm it names instead.
func TestFoldMergeEmissionsDeliversOnTheMergeUpdateArm(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	b := openWindow(t, s)

	// Act
	got, err := s.foldMergeEmissions(mergeEmissions("working on it"), 11)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got.GetMerge() == nil {
		t.Fatalf("the fold delivered on arm %T, want the merge arm", got.GetUpdate())
	}
	if got.GetBubbleId() != b.GetId() {
		t.Fatalf("the update is addressed to %q, want the open window %q", got.GetBubbleId(), b.GetId())
	}
}

func TestFoldMergeEmissionsReportsNothingWithNoWindowOpen(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	got, err := s.foldMergeEmissions(mergeEmissions("stray"), 11)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got != nil {
		t.Fatalf("with no window open the session's emissions belong to the feed, got a fold into %q", got.GetBubbleId())
	}
}

func TestFoldMergeEmissionsParentsANestedDispatchOnTheMergeBubble(t *testing.T) {
	// Arrange: the merge's own conversation dispatches a subagent.
	s := newAsyncBubbleStore("/ws", nil)
	merge := openWindow(t, s)
	nested := []*frontendv1.AgentEmission{{Emission: &frontendv1.AgentEmission_Response{
		Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: "tu_nested", Name: "Task"}}},
		}}},
	}}}
	if _, err := s.foldMergeEmissions(nested, 11); err != nil {
		t.Fatal(err)
	}

	// Act — the subagent's own first record.
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_nested", "agent_nested", "hi")}}, 12)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if len(push.Opened) != 1 {
		t.Fatalf("the nested dispatch opened %d bubbles, want one", len(push.Opened))
	}
	if got := push.Opened[0].GetParentBubbleId(); got != merge.GetId() {
		t.Fatalf("the nested bubble's parent = %q, want the merge bubble %q so the tree reads truthfully", got, merge.GetId())
	}
}

func TestSnapshotCarriesTheFoldedMergeBubble(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	openWindow(t, s)
	if _, err := s.foldMergeEmissions(mergeEmissions("working on it"), 11); err != nil {
		t.Fatal(err)
	}

	// Act
	snap := s.snapshot()

	// Assert
	if len(snap) != 1 {
		t.Fatalf("the snapshot carries %d bubbles, want the merge run's", len(snap))
	}
	if got := len(snap[0].GetMerge().GetEmissions()); got != 1 {
		t.Fatalf("the snapshot's merge bubble carries %d emissions, want everything folded to date", got)
	}
}

// --- the settling edges -----------------------------------------------------

func TestSettleMergeWindowSettlesTheBubble(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	b := openWindow(t, s)

	// Act
	up, err := s.settleMergeWindow(frontend.AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 12}, "user_prompt")
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if up.GetLiveness().GetLiveness().GetSettled().GetDone() == nil {
		t.Fatalf("the window settled on %v, want the done outcome", b.GetLiveness().GetState())
	}
}

func TestSettleMergeWindowClosesTheWindow(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)
	openWindow(t, s)

	// Act
	if _, err := s.settleMergeWindow(frontend.AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 12}, "user_prompt"); err != nil {
		t.Fatal(err)
	}

	// Assert
	if s.mergeWindowOpen() {
		t.Fatal("a settled window must claim no further emissions")
	}
}

func TestSettleMergeWindowReportsNothingWithNoWindowOpen(t *testing.T) {
	// Arrange
	s := newAsyncBubbleStore("/ws", nil)

	// Act
	up, err := s.settleMergeWindow(frontend.AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 12}, "user_prompt")

	// Assert
	if err != nil || up != nil {
		t.Fatalf("settling no window produced (%v, %v), want nothing at all", up, err)
	}
}

// --- through the real consumer ---------------------------------------------

func TestTheMergeSkillInvocationOpensABubbleOnTheAsyncPlane(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))

	// Assert
	bubbles := h.mergeBubbles()
	if len(bubbles) != 1 {
		t.Fatalf("the merge invocation opened %d merge bubbles, want exactly one", len(bubbles))
	}
	if got := bubbles[0].GetOriginToolUseId(); got != mergeOriginCall {
		t.Errorf("the merge bubble names origin %q, want the Skill call %q", got, mergeOriginCall)
	}
}

func TestANonMergeSkillInvocationOpensNoBubble(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act — the same skill, a different verb.
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-create", mergeOriginCall, "create-or-update-workspace", "create feat/thing"))

	// Assert
	if got := len(h.mergeBubbles()); got != 0 {
		t.Fatalf("a non-merge verb opened %d merge bubbles: every other verb is an ordinary skill card", got)
	}
}

// AMENDED: this read the fold off the bubble the store had already handed the
// pusher by pointer, which a re-delivery mechanism made meaningful and an append
// mechanism does not — the store's object grows whether or not anything shipped.
// It now reads the merge-arm update, which is what the client actually applies.
func TestTheSessionsEmissionsFoldIntoTheOpenMergeBubble(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	bubbleID := h.mergeBubbles()[0].GetId()

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 11, "a-inside", "cherry-picking the workspace"))

	// Assert
	var folded bool
	for _, text := range h.mergeAppends(bubbleID) {
		if text == "cherry-picking the workspace" {
			folded = true
		}
	}
	if !folded {
		t.Fatalf("the merge bubble was appended %v; the merge run's own utterance never reached it on the merge arm", h.mergeAppends(bubbleID))
	}
}

func TestTheSessionsEmissionsLeaveTheTopLevelFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 11, "a-inside", "cherry-picking the workspace"))

	// Assert
	for _, text := range h.feedTexts() {
		if text == "cherry-picking the workspace" {
			t.Fatal("the merge run's utterance reached the top-level feed as well as its bubble")
		}
	}
}

func TestAnEmissionOutsideTheWindowStaysOnTheFeed(t *testing.T) {
	// Arrange: no merge invocation at all.
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 11, "a-plain", "ordinary answer"))

	// Assert
	var found bool
	for _, text := range h.feedTexts() {
		if text == "ordinary answer" {
			found = true
		}
	}
	if !found {
		t.Fatal("with no window open the session's own utterance belongs to the top-level feed")
	}
}

func TestTheUsersNextPromptSettlesTheMergeWindow(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	bubbleID := h.mergeBubbles()[0].GetId()

	// Act
	h.controller().consumer.Consume(userPromptEvent(t, 11, "u-next", "thanks, now do the other thing"))

	// Assert
	liveness := h.mergeLiveness(bubbleID)
	if len(liveness) == 0 {
		t.Fatal("the user taking the session back must settle the merge bubble")
	}
	if liveness[len(liveness)-1].GetSettled().GetDone() == nil {
		t.Fatalf("the window settled on %v, want done: the user typing again is the window ending, not the merge failing",
			liveness[len(liveness)-1].GetState())
	}
}

func TestTheUsersNextPromptStaysOnTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))

	// Act
	h.controller().consumer.Consume(userPromptEvent(t, 11, "u-next", "thanks, now do the other thing"))

	// Assert
	var found bool
	for _, turn := range h.userTurns() {
		if turn.item.GetUuid() == "u-next" {
			found = true
		}
	}
	if !found {
		t.Fatal("the prompt that ends the window is the user taking the session back, and belongs to them and to the feed")
	}
}

func TestAnEmissionAfterTheWindowSettlesReturnsToTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	h.controller().consumer.Consume(userPromptEvent(t, 11, "u-next", "thanks, now do the other thing"))

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 12, "a-after", "on it"))

	// Assert
	var found bool
	for _, text := range h.feedTexts() {
		if text == "on it" {
			found = true
		}
	}
	if !found {
		t.Fatal("once the window is closed the session's emissions belong to the top-level feed again")
	}
}

func TestAnInterruptSettlesTheMergeWindow(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	bubbleID := h.mergeBubbles()[0].GetId()
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)

	// Act
	if err := h.m.Interrupt(context.Background(), "ws", "fe-merge-stop"); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert
	liveness := h.mergeLiveness(bubbleID)
	if len(liveness) == 0 {
		t.Fatal("a user-commanded stop is one of the two boundaries that end a merge window")
	}
	if liveness[len(liveness)-1].GetSettled().GetKilled() == nil {
		t.Fatalf("the interrupted window settled on %v, want the killed arm: the merge did not fail, it was not allowed to conclude",
			liveness[len(liveness)-1].GetState())
	}
}

func TestAnUndeliverableInterruptLeavesTheMergeWindowOpen(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(mergeSkillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED)

	// Act
	_ = h.m.Interrupt(context.Background(), "ws", "fe-merge-stop")

	// Assert
	if !h.controller().consumer.bubbles.mergeWindowOpen() {
		t.Fatal("a stop that was never delivered took nothing back from the merge, so its window must still stand")
	}
}
