// The WINDOW APPARATUS, driven through the store directly and through the real
// consumer: a Skill invocation opens a work — Merge for the merge run, Skill
// for every other skill — every emission of the session folds into the innermost
// open one until the user takes the session back, and the two closing edges —
// the user's own next prompt, and an interrupt — settle every open window.
package sessioncontroller

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/frontend"

	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// --- fixtures ---------------------------------------------------------------

const mergeOriginCall = "toolu_merge"

// skillCallEvent is the transcript record making one Skill call, for the
// consumer-level tests. It builds ANY invocation — the merge run and every
// other skill are the same record shape and differ only by what they name.
func skillCallEvent(t *testing.T, seq uint64, uuid, toolUseID, skill, args string) *corev1.Event {
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

// openWindow opens a merge window on a bare store and returns the work.
func openWindow(t *testing.T, s *detachedWorkStore) *frontendv1.Message {
	t.Helper()
	b, fault, err := s.openMergeWindow(mergeOriginCall, "/create-or-update-workspace merge", 10)
	if err != nil {
		t.Fatalf("openMergeWindow: %v", err)
	}
	if fault != nil {
		t.Fatalf("openMergeWindow faulted on the first invocation: %s", fault.Detail)
	}
	if b == nil {
		t.Fatal("the first invocation must open a work")
	}
	return b
}

// mergeDetachedWork returns every merge-kind work the frontend was OPENED with.
// A merge work is opened once and advances by its update arm thereafter, so
// this list has one entry per merge run.
func (h *queueHarness) mergeDetachedWork() []*frontendv1.Message {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.Message
	for _, d := range h.push.work {
		for _, b := range d.GetOpened() {
			if b.GetDetachedWork().GetMerge() != nil {
				out = append(out, b)
			}
		}
	}
	return out
}

// mergeAppends returns the assistant prose every MERGE-ARM update addressed to
// messageID carried, in order. It reads the wire arm rather than the store's own
// work object, which is the only way to tell an update that shipped from a
// fold that merely happened.
func (h *queueHarness) mergeAppends(messageID string) []string {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []string
	for _, d := range h.push.work {
		for _, u := range d.GetUpdates() {
			if u.GetMessageId() != messageID || u.GetMerge() == nil {
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

// mergeLiveness returns the liveness updates addressed to messageID, in order.
func (h *queueHarness) mergeLiveness(messageID string) []*frontendv1.DetachedWorkLiveness {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.DetachedWorkLiveness
	for _, d := range h.push.work {
		for _, u := range d.GetUpdates() {
			if u.GetMessageId() == messageID && u.GetLiveness() != nil {
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
		for _, it := range cd.GetMessages() {
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

func TestOpenMergeWindowGivesTheDetachedWorkTheMergeArm(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)

	// Act
	b := openWindow(t, s)

	// Assert
	if b.GetDetachedWork().GetMerge() == nil {
		t.Fatalf("a merge run opened on arm %T, want the merge arm the contract gives it", b.GetDetachedWork().GetKind())
	}
}

func TestOpenMergeWindowFilesTheDetachedWorkUnderItsSpawningCall(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)

	// Act
	b := openWindow(t, s)

	// Assert: this lookup IS what StampSpawnedMessageIDs stamps on the card.
	if got := s.spawnedMessageID(mergeOriginCall); got != b.GetUuid() {
		t.Fatalf("spawnedMessageID(%q) = %q, want the merge work %q: the card's spawned_message_id is this one resolution",
			mergeOriginCall, got, b.GetUuid())
	}
}

func TestOpenMergeWindowAnchorsTheDetachedWorkOnItsSpawningCall(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)

	// Act
	b := openWindow(t, s)

	// Assert
	if got := b.GetDetachedWork().GetOriginToolUseId(); got != mergeOriginCall {
		t.Fatalf("origin_tool_use_id = %q, want the Skill call %q the work hangs under", got, mergeOriginCall)
	}
}

func TestOpenMergeWindowOpensLive(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)

	// Act
	b := openWindow(t, s)

	// Assert
	if b.GetDetachedWork().GetLiveness().GetLive() == nil {
		t.Fatalf("a merge window opened with liveness %v, want the live arm", b.GetDetachedWork().GetLiveness().GetState())
	}
}

func TestOpenMergeWindowFaultsOnASecondInvocationWhileOneIsOpen(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)
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
		t.Fatalf("the second invocation opened work %q; the first window must stand", b.GetUuid())
	}
	if got := s.spawnedMessageID(mergeOriginCall); got != first.GetUuid() {
		t.Errorf("the open window moved to %q, want the first work %q left untouched", got, first.GetUuid())
	}
}

func TestOpenMergeWindowReadoptsItsOwnInvocationOnReplay(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)
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
		t.Fatalf("a replay opened a twin work %q", b.GetUuid())
	}
	if got := len(s.snapshot()); got != 1 {
		t.Errorf("the store holds %d work after a replay, want the one merge work %q", got, first.GetUuid())
	}
}

// --- the fold ---------------------------------------------------------------

func TestFoldWindowEmissionsFoldsIntoTheOpenDetachedWork(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)
	b := openWindow(t, s)

	// Act
	if _, err := s.foldWindowEmissions(b.GetUuid(), mergeEmissions("working on it"), 11); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := len(b.GetDetachedWork().GetMerge().GetEmissions()); got != 1 {
		t.Fatalf("the merge work holds %d emissions, want the one that folded", got)
	}
}

// AMENDED: this test pinned whole-work re-delivery, the interim shape the
// window advanced by before the contract had an arm for it. The update oneof's
// own rule — "Never a re-send of the whole work" — is what retires that
// mechanism, and `merge = 15` is the arm it names instead.
func TestFoldWindowEmissionsDeliversAMergeWindowOnTheMergeUpdateArm(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)
	b := openWindow(t, s)

	// Act
	got, err := s.foldWindowEmissions(b.GetUuid(), mergeEmissions("working on it"), 11)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got.GetMerge() == nil {
		t.Fatalf("the fold delivered on arm %T, want the merge arm", got.GetUpdate())
	}
	if got.GetMessageId() != b.GetUuid() {
		t.Fatalf("the update is addressed to %q, want the open window %q", got.GetMessageId(), b.GetUuid())
	}
}

// AMENDED: the fold's destination is now the CALLER's, because two
// destinations exist in one event (the innermost window, and the parent of a
// nested window whose card the event carried). A destination the stack no
// longer holds is therefore a stale snapshot rather than "nothing is open", and
// it is refused loudly instead of silently swallowing the session's
// conversation.
func TestFoldWindowEmissionsRefusesADetachedWorkNoOpenWindowNames(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)

	// Act
	got, err := s.foldWindowEmissions("work_no_window_names", mergeEmissions("stray"), 11)

	// Assert
	if err == nil {
		t.Fatalf("a fold aimed at a work no open window names was applied, want a refusal; got update %v", got)
	}
	if got != nil {
		t.Fatalf("the refused fold still produced an update addressed to %q", got.GetMessageId())
	}
}

func TestFoldWindowEmissionsParentsANestedDispatchOnTheMergeDetachedWork(t *testing.T) {
	// Arrange: the merge's own conversation dispatches a subagent.
	s := newDetachedWorkStore("/ws", nil)
	merge := openWindow(t, s)
	nested := []*frontendv1.AgentEmission{{Emission: &frontendv1.AgentEmission_Response{
		Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: "tu_nested", Name: "Task"}}},
		}}},
	}}}
	if _, err := s.foldWindowEmissions(merge.GetUuid(), nested, 11); err != nil {
		t.Fatal(err)
	}

	// Act — the subagent's own first record.
	push, err := s.observeCuration(frontend.Curation{Detached: []frontend.DetachedFold{detachedFold("tu_nested", "agent_nested", "hi")}}, 12)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if len(push.Opened) != 1 {
		t.Fatalf("the nested dispatch opened %d work, want one", len(push.Opened))
	}
	if got := push.Opened[0].GetLineage().GetParentMessageId(); got != merge.GetUuid() {
		t.Fatalf("the nested work's parent = %q, want the merge work %q so the tree reads truthfully", got, merge.GetUuid())
	}
}

func TestSnapshotCarriesTheFoldedMergeDetachedWork(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)
	b := openWindow(t, s)
	if _, err := s.foldWindowEmissions(b.GetUuid(), mergeEmissions("working on it"), 11); err != nil {
		t.Fatal(err)
	}

	// Act
	snap := s.snapshot()

	// Assert
	if len(snap) != 1 {
		t.Fatalf("the snapshot carries %d work, want the merge run's", len(snap))
	}
	if got := len(snap[0].GetDetachedWork().GetMerge().GetEmissions()); got != 1 {
		t.Fatalf("the snapshot's merge work carries %d emissions, want everything folded to date", got)
	}
}

// --- the settling edges -----------------------------------------------------

func TestSettleWindowsSettlesTheDetachedWork(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)
	b := openWindow(t, s)

	// Act
	ups, err := s.settleWindows(frontend.DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 12}, "user_prompt")
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if len(ups) != 1 || ups[0].GetLiveness().GetLiveness().GetSettled().GetDone() == nil {
		t.Fatalf("the window settled on %v, want the done outcome", b.GetDetachedWork().GetLiveness().GetState())
	}
}

func TestSettleWindowsClosesTheWindow(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)
	openWindow(t, s)

	// Act
	if _, err := s.settleWindows(frontend.DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 12}, "user_prompt"); err != nil {
		t.Fatal(err)
	}

	// Assert
	if s.windowsOpen() {
		t.Fatal("a settled window must claim no further emissions")
	}
}

func TestSettleWindowsReportsNothingWithNoWindowOpen(t *testing.T) {
	// Arrange
	s := newDetachedWorkStore("/ws", nil)

	// Act
	ups, err := s.settleWindows(frontend.DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 12}, "user_prompt")

	// Assert
	if err != nil || len(ups) != 0 {
		t.Fatalf("settling no window produced (%v, %v), want nothing at all", ups, err)
	}
}

// --- through the real consumer ---------------------------------------------

func TestTheMergeSkillInvocationOpensADetachedWorkOnTheAsyncPlane(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))

	// Assert
	work := h.mergeDetachedWork()
	if len(work) != 1 {
		t.Fatalf("the merge invocation opened %d merge work, want exactly one", len(work))
	}
	if got := work[0].GetDetachedWork().GetOriginToolUseId(); got != mergeOriginCall {
		t.Errorf("the merge work names origin %q, want the Skill call %q", got, mergeOriginCall)
	}
}

// AMENDED: this asserted that a non-merge verb opened NO work at all, which
// was true while only the merge skill was work-forming. async-work.proto now
// says every Skill invocation is work-forming and that "`merge` is the one
// skill with an arm of its own", so what the near-miss must not do is open a
// MERGE work — it opens a Skill one.
func TestANonMergeSkillInvocationOpensNoMergeDetachedWork(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act — the same skill, a different verb.
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-create", mergeOriginCall, "create-or-update-workspace", "create feat/thing"))

	// Assert
	if got := len(h.mergeDetachedWork()); got != 0 {
		t.Fatalf("a non-merge verb opened %d merge work: every other verb is an ordinary skill invocation", got)
	}
}

// AMENDED: this read the fold off the work the store had already handed the
// pusher by pointer, which a re-delivery mechanism made meaningful and an append
// mechanism does not — the store's object grows whether or not anything shipped.
// It now reads the merge-arm update, which is what the client actually applies.
func TestTheSessionsEmissionsFoldIntoTheOpenMergeDetachedWork(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	messageID := h.mergeDetachedWork()[0].GetUuid()

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 11, "a-inside", "cherry-picking the workspace"))

	// Assert
	var folded bool
	for _, text := range h.mergeAppends(messageID) {
		if text == "cherry-picking the workspace" {
			folded = true
		}
	}
	if !folded {
		t.Fatalf("the merge work was appended %v; the merge run's own utterance never reached it on the merge arm", h.mergeAppends(messageID))
	}
}

func TestTheSessionsEmissionsLeaveTheTopLevelFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 11, "a-inside", "cherry-picking the workspace"))

	// Assert
	for _, text := range h.feedTexts() {
		if text == "cherry-picking the workspace" {
			t.Fatal("the merge run's utterance reached the top-level feed as well as its work")
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
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	messageID := h.mergeDetachedWork()[0].GetUuid()

	// Act
	h.controller().consumer.Consume(userPromptEvent(t, 11, "u-next", "thanks, now do the other thing"))

	// Assert
	liveness := h.mergeLiveness(messageID)
	if len(liveness) == 0 {
		t.Fatal("the user taking the session back must settle the merge work")
	}
	if liveness[len(liveness)-1].GetSettled().GetDone() == nil {
		t.Fatalf("the window settled on %v, want done: the user typing again is the window ending, not the merge failing",
			liveness[len(liveness)-1].GetState())
	}
}

func TestTheUsersNextPromptStaysOnTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))

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
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
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
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	messageID := h.mergeDetachedWork()[0].GetUuid()
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)

	// Act
	if err := h.m.Interrupt(context.Background(), "ws", "fe-merge-stop"); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert
	liveness := h.mergeLiveness(messageID)
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
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED)

	// Act
	_ = h.m.Interrupt(context.Background(), "ws", "fe-merge-stop")

	// Assert
	if !h.controller().consumer.work.windowsOpen() {
		t.Fatal("a stop that was never delivered took nothing back from the merge, so its window must still stand")
	}
}

// --- skill windows ----------------------------------------------------------

// skillDetachedWork returns every skill-kind work the frontend was opened with.
func (h *queueHarness) skillDetachedWork() []*frontendv1.Message {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.Message
	for _, d := range h.push.work {
		for _, b := range d.GetOpened() {
			if b.GetDetachedWork().GetSkill() != nil {
				out = append(out, b)
			}
		}
	}
	return out
}

// skillAppends returns the assistant prose every SKILL-ARM emissions update
// addressed to messageID carried, in order.
func (h *queueHarness) skillAppends(messageID string) []string {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []string
	for _, d := range h.push.work {
		for _, u := range d.GetUpdates() {
			if u.GetMessageId() != messageID {
				continue
			}
			for _, em := range u.GetSkill().GetEmissions().GetEmissions() {
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

// skillToolCallAppends returns the tool_use ids every SKILL-ARM emissions
// update addressed to messageID carried, in order. It is what says WHICH CARDS
// landed inside a work's conversation, which is where a nested window's
// work hangs.
func (h *queueHarness) skillToolCallAppends(messageID string) []string {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []string
	for _, d := range h.push.work {
		for _, u := range d.GetUpdates() {
			if u.GetMessageId() != messageID {
				continue
			}
			for _, em := range u.GetSkill().GetEmissions().GetEmissions() {
				out = append(out, toolUseIDsIn(em)...)
			}
		}
	}
	return out
}

// toolUseIDsIn returns the tool_use ids one agent emission carries, whether it
// arrived as a tool-call emission or as a tool_use block inside a response.
// Both a work's fold and the top-level feed are read through it, so the two
// cannot disagree about which cards an emission made.
func toolUseIDsIn(em *frontendv1.AgentEmission) []string {
	var out []string
	if id := em.GetToolCall().GetCall().GetId(); id != "" {
		out = append(out, id)
	}
	for _, block := range em.GetResponse().GetBody().GetContent() {
		if id := block.GetToolUse().GetId(); id != "" {
			out = append(out, id)
		}
	}
	return out
}

// feedToolUseIDs returns the tool_use ids every item pushed on the TOP-LEVEL
// feed carried, in order.
func (h *queueHarness) feedToolUseIDs() []string {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []string
	for _, cd := range h.push.convo {
		for _, it := range cd.GetMessages() {
			out = append(out, toolUseIDsIn(it.GetAgent())...)
		}
	}
	return out
}

// skillBodyDeliveries returns the contents every BODY-arm update addressed to
// messageID carried, in order.
func (h *queueHarness) skillBodyDeliveries(messageID string) []string {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []string
	for _, d := range h.push.work {
		for _, u := range d.GetUpdates() {
			if u.GetMessageId() == messageID && u.GetSkill().GetBody() != nil {
				out = append(out, u.GetSkill().GetBody().GetContents())
			}
		}
	}
	return out
}

// workByID finds one work in the store's own snapshot — what a reconnecting
// client is served.
func (h *queueHarness) snapshotDetachedWork(id string) *frontendv1.Message {
	for _, b := range h.controller().consumer.work.snapshot() {
		if b.GetUuid() == id {
			return b
		}
	}
	return nil
}

// invokeSkill consumes one non-merge Skill invocation and returns its work.
func invokeSkill(t *testing.T, h *queueHarness, seq uint64, uuid, toolUseID, skill, args string) *frontendv1.Message {
	t.Helper()
	h.controller().consumer.Consume(skillCallEvent(t, seq, uuid, toolUseID, skill, args))
	for _, b := range h.skillDetachedWork() {
		if b.GetDetachedWork().GetOriginToolUseId() == toolUseID {
			return b
		}
	}
	t.Fatalf("the %q invocation on call %q opened no skill work", skill, toolUseID)
	return nil
}

// --- classification ---------------------------------------------------------

func TestANonMergeSkillInvocationOpensASkillDetachedWork(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Assert
	if b.GetDetachedWork().GetSkill() == nil {
		t.Fatalf("the invocation opened arm %T, want the skill arm every non-merge skill arrives on", b.GetDetachedWork().GetKind())
	}
}

func TestASkillDetachedWorkCarriesItsSkillNameVerbatim(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Assert
	if got := b.GetDetachedWork().GetSkill().GetSkillName(); got != "demo" {
		t.Fatalf("skill_name = %q, want the name the call made, verbatim", got)
	}
}

func TestASkillDetachedWorkCarriesItsArgsVerbatim(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Assert
	if got := b.GetDetachedWork().GetSkill().GetArgs(); got != "run it" {
		t.Fatalf("args = %q, want the arguments the call made, verbatim", got)
	}
}

func TestASkillDetachedWorkWearsTheInvocationAsItsLabel(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Assert
	if got := b.GetDetachedWork().GetLabel(); got != "/demo run it" {
		t.Fatalf("label = %q, want the invocation as the agent wrote it", got)
	}
}

func TestASkillDetachedWorkIsStampedOnItsOwnCall(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Assert: this lookup IS what StampSpawnedMessageIDs stamps on the card.
	if got := h.controller().consumer.work.spawnedMessageID("toolu_demo"); got != b.GetUuid() {
		t.Fatalf("spawnedMessageID = %q, want the skill work %q", got, b.GetUuid())
	}
}

func TestASkillCallNamingNoSkillOpensNoDetachedWork(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act — a Skill call whose input carries no skill name.
	h.controller().consumer.Consume(transcriptToolUseEvent(t, 10, "a-bare", "toolu_bare", "Skill"))

	// Assert
	if got := len(h.skillDetachedWork()); got != 0 {
		t.Fatalf("a nameless Skill call opened %d skill work(s): there is nothing to label one with", got)
	}
}

func TestASkillCallNamingNoSkillIsLoud(t *testing.T) {
	// Arrange: a silent skip is indistinguishable from a classification that
	// worked.
	cl := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, cl.logf)

	// Act
	h.controller().consumer.Consume(transcriptToolUseEvent(t, 10, "a-bare", "toolu_bare", "Skill"))

	// Assert
	if !cl.contains("SKILL CALL NOT CLASSIFIED") {
		t.Error("a Skill call the daemon could not classify was passed over without a word")
	}
}

// --- the body ---------------------------------------------------------------

func TestASkillsBodyIsDeliveredOnTheBodyArm(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 11, "u-result", "toolu_demo"))

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-body", "u-result", skillBody))

	// Assert
	got := h.skillBodyDeliveries(b.GetUuid())
	if len(got) != 1 || got[0] != skillBody {
		t.Fatalf("the work was delivered bodies %v, want the SKILL.md once on the body arm", got)
	}
}

func TestASkillsBodyIsCarriedByItsSnapshot(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 11, "u-result", "toolu_demo"))

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-body", "u-result", skillBody))

	// Assert: a reconnecting client is served the fold, and the body is part of
	// it.
	if got := h.snapshotDetachedWork(b.GetUuid()).GetDetachedWork().GetSkill().GetBody(); got != skillBody {
		t.Fatalf("the snapshot's body = %q, want the SKILL.md the update delivered", got)
	}
}

func TestASkillWithADetachedWorkEmitsNoSkillBodyCard(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 11, "u-result", "toolu_demo"))

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-body", "u-result", skillBody))

	// Assert: the contents have exactly one home, and the card rendering of them
	// is retired by the arm that gave them one.
	if got := h.skillBodies(); len(got) != 0 {
		t.Fatalf("pushed %d skill_body card(s) for a call whose work already carries the body, want none", len(got))
	}
}

func TestASkillWhoseBodyNeverArrivesKeepsAnEmptyBody(t *testing.T) {
	// Arrange: the harness could not read the skill file, so it writes the
	// failure as the call's result and no body record ever follows.
	h := newQueueHarness(t, nil)
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Act
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 11, "u-result", "toolu_demo"))

	// Assert
	if got := h.snapshotDetachedWork(b.GetUuid()).GetDetachedWork().GetSkill().GetBody(); got != "" {
		t.Fatalf("the work's body = %q, want it empty: nothing is invented for a body that never resolved", got)
	}
}

func TestASkillWhoseBodyNeverArrivesStillShowsItsCallsResult(t *testing.T) {
	// Arrange: the failure has to reach the user through the ORDINARY channel —
	// an empty body plus a silent card is the one outcome that must not happen.
	h := newQueueHarness(t, nil)
	invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Act
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 11, "u-result", "toolu_demo"))

	// Assert
	var found bool
	for _, turn := range h.userTurns() {
		if turn.item.GetUuid() == "u-result" {
			found = true
		}
	}
	if !found {
		t.Fatal("the invocation's own result was folded away: the call's card is where a skill's failure is said, and the window must leave it on the feed")
	}
}

// --- the fold ---------------------------------------------------------------

func TestTheSessionsEmissionsArriveOnTheSkillUpdateArm(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 11, "a-inside", "reading the workspace"))

	// Assert
	var folded bool
	for _, text := range h.skillAppends(b.GetUuid()) {
		if text == "reading the workspace" {
			folded = true
		}
	}
	if !folded {
		t.Fatalf("the skill work was appended %v on its own arm, want the invocation's own utterance", h.skillAppends(b.GetUuid()))
	}
}

func TestTheSessionsEmissionsLeaveTheFeedForTheSkillDetachedWork(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 11, "a-inside", "reading the workspace"))

	// Assert
	for _, text := range h.feedTexts() {
		if text == "reading the workspace" {
			t.Fatal("the skill's utterance reached the top-level feed as well as its work")
		}
	}
}

// --- nesting ----------------------------------------------------------------

func TestASkillInvokedInsideASkillParentsUnderIt(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	outer := invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")

	// Act
	inner := invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")

	// Assert
	if got := inner.GetLineage().GetParentMessageId(); got != outer.GetUuid() {
		t.Fatalf("the inner skill's parent = %q, want the open window %q: skills chain, and the tree must read truthfully", got, outer.GetUuid())
	}
}

func TestTheDeepestOpenWindowCapturesTheSessionsEmissions(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")
	inner := invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 12, "a-inside", "the inner skill is working"))

	// Assert
	var folded bool
	for _, text := range h.skillAppends(inner.GetUuid()) {
		if text == "the inner skill is working" {
			folded = true
		}
	}
	if !folded {
		t.Fatalf("the innermost window was appended %v, want the utterance that happened while it was open", h.skillAppends(inner.GetUuid()))
	}
}

func TestAnEmissionInsideANestedSkillDoesNotAlsoFoldIntoTheOuterOne(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	outer := invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")
	invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 12, "a-inside", "the inner skill is working"))

	// Assert: the outer window holds it through the child's parent pointer, not
	// as a second copy of its own.
	for _, text := range h.skillAppends(outer.GetUuid()) {
		if text == "the inner skill is working" {
			t.Fatal("one emission folded into two windows: exactly one work owns an emission, and it is the innermost")
		}
	}
}

func TestANestedSkillsOwnCardFoldsIntoTheWindowOutsideIt(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	outer := invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")

	// Act
	invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")

	// Assert: the card is where the inner work hangs, so it must land inside
	// the outer conversation for the inner work to render there.
	var landed bool
	for _, id := range h.skillToolCallAppends(outer.GetUuid()) {
		if id == "toolu_inner" {
			landed = true
		}
	}
	if !landed {
		t.Fatalf("the outer window was appended cards %v, want the inner invocation's own card", h.skillToolCallAppends(outer.GetUuid()))
	}
}

func TestANestedSkillsOwnCardLeavesTheTopLevelFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")

	// Act
	invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")

	// Assert
	for _, id := range h.feedToolUseIDs() {
		if id == "toolu_inner" {
			t.Fatal("the nested invocation's card reached the top-level feed as well as its parent's conversation, so its work renders twice and outside the work that started it")
		}
	}
}

func TestANestedSkillsAnchorCardLandsInsideItsParentsConversation(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	outer := invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")

	// Act
	inner := invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")

	// Assert: a frontend attaches a work to its card by matching
	// origin_tool_use_id, so the inner work renders inside the outer one only
	// if the card carrying that id is part of the outer conversation.
	var anchored bool
	for _, id := range h.skillToolCallAppends(outer.GetUuid()) {
		if id == inner.GetDetachedWork().GetOriginToolUseId() {
			anchored = true
		}
	}
	if !anchored {
		t.Fatalf("the outer conversation carries cards %v, want the inner work's anchor %q", h.skillToolCallAppends(outer.GetUuid()), inner.GetDetachedWork().GetOriginToolUseId())
	}
}

func TestANestedSkillsOwnCardDoesNotFoldIntoItsOwnDetachedWork(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")

	// Act
	inner := invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")

	// Assert: a work hanging under a card inside itself is not a tree.
	for _, id := range h.skillToolCallAppends(inner.GetUuid()) {
		if id == "toolu_inner" {
			t.Fatal("the inner window swallowed its own opening card, so its work hangs inside itself")
		}
	}
}

func TestAnOutermostSkillsOwnCardStaysOnTheTopLevelFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")

	// Assert: there is no conversation outside it, so the feed is where its
	// card — and the work hanging under it — belongs.
	var onFeed bool
	for _, id := range h.feedToolUseIDs() {
		if id == "toolu_demo" {
			onFeed = true
		}
	}
	if !onFeed {
		t.Fatalf("the feed carried cards %v, want the outermost invocation's own card", h.feedToolUseIDs())
	}
}

func TestAMergeInvokedInsideASkillWindowParentsUnderIt(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	outer := invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")

	// Act
	h.controller().consumer.Consume(skillCallEvent(t, 11, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))

	// Assert
	work := h.mergeDetachedWork()
	if len(work) != 1 {
		t.Fatalf("the merge invocation opened %d merge work inside a skill window, want one", len(work))
	}
	if got := work[0].GetLineage().GetParentMessageId(); got != outer.GetUuid() {
		t.Fatalf("the merge work's parent = %q, want the skill window %q it was invoked inside", got, outer.GetUuid())
	}
}

// --- the settling edges -----------------------------------------------------

func TestTheUsersNextPromptSettlesEveryOpenWindow(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	outer := invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")
	inner := invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")

	// Act
	h.controller().consumer.Consume(userPromptEvent(t, 12, "u-next", "thanks, now do the other thing"))

	// Assert
	for _, b := range []*frontendv1.Message{outer, inner} {
		liveness := h.mergeLiveness(b.GetUuid())
		if len(liveness) == 0 || liveness[len(liveness)-1].GetSettled() == nil {
			t.Fatalf("work %q never settled: the user taking the session back ends every open window, not only the innermost", b.GetUuid())
		}
	}
}

func TestAnEmissionAfterEveryWindowSettlesReturnsToTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	invokeSkill(t, h, 10, "a-outer", "toolu_outer", "outer", "go")
	invokeSkill(t, h, 11, "a-inner", "toolu_inner", "inner", "go")
	h.controller().consumer.Consume(userPromptEvent(t, 12, "u-next", "thanks, now do the other thing"))

	// Act
	h.controller().consumer.Consume(assistantTextEvent(t, 13, "a-after", "on it"))

	// Assert
	for _, text := range h.feedTexts() {
		if text == "on it" {
			return
		}
	}
	t.Fatal("with every window closed the session's emissions belong to the top-level feed again")
}

func TestAnInterruptSettlesTheSkillWindow(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	b := invokeSkill(t, h, 10, "a-demo", "toolu_demo", "demo", "run it")
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)

	// Act
	if err := h.m.Interrupt(context.Background(), "ws", "fe-skill-stop"); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert
	liveness := h.mergeLiveness(b.GetUuid())
	if len(liveness) == 0 {
		t.Fatal("a user-commanded stop is one of the two boundaries that end a skill window")
	}
	if liveness[len(liveness)-1].GetSettled().GetKilled() == nil {
		t.Fatalf("the interrupted window settled on %v, want the killed arm: the skill did not fail, it was not allowed to conclude",
			liveness[len(liveness)-1].GetState())
	}
}
