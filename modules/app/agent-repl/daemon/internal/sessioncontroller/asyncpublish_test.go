package sessioncontroller

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// sidechainAssistantEvent is the store event carrying one DETACHED agent's
// spoken turn: an assistant transcript record flagged isSidechain and linked to
// the call that launched it. It is the exact stimulus the acceptance criterion
// is about.
func sidechainAssistantEvent(t *testing.T, seq uint64, uuid, sourceToolUseID, text string) *corev1.Event {
	t.Helper()
	return transcriptRecordEvent(t, seq, &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{
		Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{
				Uuid: uuid, IsSidechain: true, SourceToolUseId: sourceToolUseID, AgentId: "agent_1",
			},
			Message: &datav1.ApiAssistantMessage{Id: "msg_" + uuid, Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
			}},
		},
	}})
}

// mainAssistantEvent is the same record WITHOUT the sidechain flag: the
// session's own agent speaking.
func mainAssistantEvent(t *testing.T, seq uint64, uuid, text string) *corev1.Event {
	t.Helper()
	return transcriptRecordEvent(t, seq, &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{
		Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message: &datav1.ApiAssistantMessage{Id: "msg_" + uuid, Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
			}},
		},
	}})
}

func transcriptRecordEvent(t *testing.T, seq uint64, tl *datav1.TranscriptLine) *corev1.Event {
	t.Helper()
	a, err := anypb.New(tl)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId:    "s1",
		Seq:          seq,
		ProducedAtMs: 1700000000000,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

// feedTexts collects every response body text the consumer pushed onto the
// TOP-LEVEL feed.
func feedTexts(push *fakePusher) []string {
	var out []string
	push.mu.Lock()
	defer push.mu.Unlock()
	for _, cd := range push.convo {
		for _, item := range cd.GetItems() {
			for _, block := range item.GetAgent().GetResponse().GetBody().GetContent() {
				if text := block.GetText().GetText(); text != "" {
					out = append(out, text)
				}
			}
		}
	}
	return out
}

// bubbleTexts collects every response body text the consumer pushed INSIDE an
// async bubble's agent update.
func bubbleTexts(push *fakePusher) []string {
	var out []string
	push.mu.Lock()
	defer push.mu.Unlock()
	for _, delta := range push.bubbles {
		for _, up := range delta.GetUpdates() {
			for _, em := range up.GetAgent().GetEmissions() {
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

func containsText(haystack []string, needle string) bool {
	for _, s := range haystack {
		if s == needle {
			return true
		}
	}
	return false
}

// --- THE ACCEPTANCE CRITERION, through the consumer ------------------------

func TestADetachedAgentsResponseAppearsInItsBubbleDelta(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "subagent speaking"), true)
	if !containsText(bubbleTexts(push), "subagent speaking") {
		t.Fatalf("a detached agent's emissions must reach frontends inside its bubble, got %v", bubbleTexts(push))
	}
}

func TestADetachedAgentsResponseDoesNotAppearAsAFeedItem(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "subagent speaking"), true)
	if containsText(feedTexts(push), "subagent speaking") {
		t.Fatalf("a subagent's response mis-landing in the top-level feed is the defect this repairs, got %v", feedTexts(push))
	}
}

func TestTheMainAgentsResponseStillAppearsAsAFeedItem(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(mainAssistantEvent(t, 1, "u1", "main agent speaking"), true)
	if !containsText(feedTexts(push), "main agent speaking") {
		t.Fatalf("the session's own conversation must be unaffected, got %v", feedTexts(push))
	}
}

func TestTheMainAgentsResponseIsNotPushedIntoAnyBubble(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(mainAssistantEvent(t, 1, "u1", "main agent speaking"), true)
	if len(bubbleTexts(push)) != 0 {
		t.Fatalf("nothing the main agent said is detached work, got %v", bubbleTexts(push))
	}
}

func TestADetachedAgentsFirstRecordOpensItsBubbleInTheSamePush(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "subagent speaking"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.bubbles) != 1 || len(push.bubbles[0].GetOpened()) != 1 {
		t.Fatal("an update must never land in a client that has not been told about its bubble")
	}
}

func TestTheAsyncPushCarriesTheWorkspacesFence(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if push.bubbles[0].GetFence() != c.fence() {
		t.Fatalf("a stale push must be discardable whole: want fence %q, got %q", c.fence(), push.bubbles[0].GetFence())
	}
}

func TestTheAsyncPushCarriesTheEventsReplayCursor(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 42, "u1", "tu_task", "x"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if push.bubbles[0].GetThroughSeq() != 42 {
		t.Fatalf("want through_seq=42, got %d", push.bubbles[0].GetThroughSeq())
	}
}

func TestAnAllDetachedEventStillPushesItsFeedDelta(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 7, "u1", "tu_task", "x"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.convo) != 1 || push.convo[0].GetThroughSeq() != 7 {
		t.Fatal("swallowing the delta would strand every client's replay cursor behind this event")
	}
}

func TestASecondDetachedRecordFoldsWithoutReopeningItsBubble(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "one"), true)
	c.pushConversation(sidechainAssistantEvent(t, 2, "u2", "tu_task", "two"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.bubbles[1].GetOpened()) != 0 {
		t.Fatal("a bubble the receiver already knows is not re-opened by every later record")
	}
}

func TestABubbleTheSessionOpenedReachesTheReconnectSnapshot(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "one"), true)
	snap := c.bubbles.snapshot()
	if len(snap) != 1 || len(snap[0].GetAgent().GetEmissions()) != 1 {
		t.Fatalf("a reconnecting client must be handed the fold the pushes had been building, got %v", snap)
	}
}
