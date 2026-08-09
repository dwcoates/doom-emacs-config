package sessioncontroller

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"

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

func TestABubbleNamesTheSameWorkspaceItsDeltaDoes(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	delta := push.bubbles[0]
	if delta.GetOpened()[0].GetWorkspace() != delta.GetWorkspace() {
		t.Fatalf("the bubble and its envelope must always name one workspace: bubble=%q delta=%q",
			delta.GetOpened()[0].GetWorkspace(), delta.GetWorkspace())
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

// --- daemon-bug fold gaps become failure cards -----------------------------

// gapConsumer is a consumer whose whole log is captured, so a test can assert
// how MANY records one fault produced as well as what they said.
func gapConsumer(t *testing.T, push Pusher) (*consumer, *[]string) {
	t.Helper()
	var lines []string
	c := newConsumer("ws", "s1", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{},
		func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) },
		nil, nil, nil, nil, nil)
	c.now = func() int64 { return 1000 }
	return c, &lines
}

func gapEvent(seq uint64) *corev1.Event {
	return &corev1.Event{SessionId: "s1", Seq: seq, ProducedAtMs: 1700000000000}
}

// openWorkflowBubble opens a WORKFLOW bubble, whose fold is a row journal.
func openWorkflowBubble(t *testing.T, c *consumer) {
	t.Helper()
	if _, err := c.bubbles.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_WORKFLOW, ToolUseId: "tu_1",
	}, 10); err != nil {
		t.Fatal(err)
	}
}

// openShellBubble opens a SHELL bubble, whose fold is a byte spool.
func openShellBubble(t *testing.T, c *consumer) {
	t.Helper()
	if _, err := c.bubbles.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_SHELL, ToolUseId: "tu_1",
	}, 10); err != nil {
		t.Fatal(err)
	}
}

// retrievalOutcome is one task-output retrieval restating `text` in full, which
// is the shape both rewind cases arrive in.
func retrievalOutcome(text string) frontend.Curation {
	return frontend.Curation{Outcomes: []frontend.ToolOutcome{{
		ToolUseID: "tu_out",
		Result: &datav1.ToolUseResult{Result: &datav1.ToolUseResult_TaskOutput{
			TaskOutput: &datav1.TaskOutputResult{Task: &datav1.TaskOutputResult_LocalBash{
				LocalBash: &datav1.LocalBashTask{
					TaskId: "task_1", Output: text, Status: datav1.RawTaskStatus_RAW_TASK_STATUS_RUNNING,
				},
			}},
		}},
	}}}
}

// agentFoldOntoTu1 is a detached agent's emissions addressed to whatever bubble
// tu_1 opened — an AGENT update, whichever kind that bubble actually is.
func agentFoldOntoTu1() frontend.Curation {
	return frontend.Curation{Detached: []frontend.DetachedFold{{
		SourceToolUseID: "tu_1",
		Emissions: []*frontendv1.AgentEmission{{Emission: &frontendv1.AgentEmission_Response{
			Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{Id: "m1"}},
		}}},
	}}}
}

// failureCards collects every failure card the consumer pushed onto the feed,
// keyed by the uuid it addressed the card by.
func failureCards(push *fakePusher) map[string]string {
	out := map[string]string{}
	push.mu.Lock()
	defer push.mu.Unlock()
	for _, cd := range push.convo {
		for _, item := range cd.GetItems() {
			if card := item.GetFailureCard(); card != nil {
				out[item.GetUuid()] = card.GetDetail()
			}
		}
	}
	return out
}

// countLinesWith counts the log records mentioning a marker, which is how the
// "one canonical record per fault" claim is checked.
func countLinesWith(lines []string, marker string) int {
	n := 0
	for _, line := range lines {
		if strings.Contains(line, marker) {
			n++
		}
	}
	return n
}

func TestAKindMismatchedFoldBecomesAFailureCard(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openWorkflowBubble(t, c)

	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(1)), gapEvent(1))

	cards := failureCards(push)
	if _, ok := cards[gapCardUUID(t, c, "kind_mismatch")]; !ok {
		t.Fatalf("a bubble that silently stops growing is indistinguishable from a quiet agent and must earn a card, got %v", cards)
	}
}

func TestAKindMismatchedFoldsCardCarriesTheRefusalsEvidence(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openWorkflowBubble(t, c)

	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(1)), gapEvent(1))

	if detail := failureCards(push)[gapCardUUID(t, c, "kind_mismatch")]; !strings.Contains(detail, "daemon bug") {
		t.Fatalf("the warn's diagnostic content becomes the card's evidence, got %q", detail)
	}
}

func TestAKindMismatchedFoldIsRecordedExactlyOnce(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)
	openWorkflowBubble(t, c)

	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(1)), gapEvent(1))

	if got := countLinesWith(*lines, "is a daemon bug and is rejected"); got != 1 {
		t.Fatalf("one fault is one canonical record, got %d in %v", got, *lines)
	}
}

func TestAKindMismatchedFoldReplaysOntoTheSameCard(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openWorkflowBubble(t, c)

	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(1)), gapEvent(1))
	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(2)), gapEvent(2))

	if got := len(failureCards(push)); got != 1 {
		t.Fatalf("a replay must reconcile onto the card it already wrote rather than accumulate twins, got %d", got)
	}
}

func TestARewoundOutputSpoolBecomesAFailureCard(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openShellBubble(t, c)
	c.pushAsync(c.observeAsync(retrievalOutcome("hello world"), gapEvent(1)), gapEvent(1))

	c.pushAsync(c.observeAsync(retrievalOutcome("hi"), gapEvent(2)), gapEvent(2))

	cards := failureCards(push)
	if _, ok := cards[gapCardUUID(t, c, "spool_rewind")]; !ok {
		t.Fatalf("a source that rewound under an append-only spool must reach the user, got %v", cards)
	}
}

func TestARewoundOutputSpoolIsRecordedExactlyOnce(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)
	openShellBubble(t, c)
	c.pushAsync(c.observeAsync(retrievalOutcome("hello world"), gapEvent(1)), gapEvent(1))

	c.pushAsync(c.observeAsync(retrievalOutcome("hi"), gapEvent(2)), gapEvent(2))

	if got := countLinesWith(*lines, "output REWOUND"); got != 1 {
		t.Fatalf("one fault is one canonical record, got %d in %v", got, *lines)
	}
}

func TestARewoundWorkflowJournalBecomesAFailureCard(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openWorkflowBubble(t, c)
	c.pushAsync(c.observeAsync(retrievalOutcome(`{"label":"a"}`+"\n"+`{"label":"b"}`+"\n"), gapEvent(1)), gapEvent(1))

	c.pushAsync(c.observeAsync(retrievalOutcome(`{"label":"a"}`+"\n"), gapEvent(2)), gapEvent(2))

	cards := failureCards(push)
	if _, ok := cards[gapCardUUID(t, c, "journal_rewind")]; !ok {
		t.Fatalf("a journal that rewound under an append-only fold must reach the user, got %v", cards)
	}
}

func TestARewoundWorkflowJournalIsRecordedExactlyOnce(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)
	openWorkflowBubble(t, c)
	c.pushAsync(c.observeAsync(retrievalOutcome(`{"label":"a"}`+"\n"+`{"label":"b"}`+"\n"), gapEvent(1)), gapEvent(1))

	c.pushAsync(c.observeAsync(retrievalOutcome(`{"label":"a"}`+"\n"), gapEvent(2)), gapEvent(2))

	if got := countLinesWith(*lines, "journal for bubble"); got != 1 {
		t.Fatalf("one fault is one canonical record, got %d in %v", got, *lines)
	}
}

func TestAnUnclassifiedFoldRefusalStillTakesTheDegradedWarn(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)

	// A detached record naming neither a source call nor an open bubble is a
	// refusal with no daemon-bug class: it keeps the warn it always had.
	c.pushAsync(c.observeAsync(frontend.Curation{Detached: []frontend.DetachedFold{{AgentID: "agent_x"}}}, gapEvent(1)), gapEvent(1))

	if got := countLinesWith(*lines, "ASYNC BUBBLE FOLD DEGRADED"); got != 1 {
		t.Fatalf("an unclassified refusal must keep its warn rather than disappear, got %d in %v", got, *lines)
	}
}

// gapCardUUID is the address a gap card is expected under: the store's own
// bubble id and the gap class, so the test names the same key the production
// derivation does rather than a hand-copied string.
func gapCardUUID(t *testing.T, c *consumer, gap string) string {
	t.Helper()
	snap := c.bubbles.snapshot()
	if len(snap) != 1 {
		t.Fatalf("the arrange step must leave exactly one bubble, got %d", len(snap))
	}
	return fmt.Sprintf("async-gap:%s:%s", snap[0].GetId(), gap)
}
