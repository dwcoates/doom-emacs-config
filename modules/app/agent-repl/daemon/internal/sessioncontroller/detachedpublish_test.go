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
		for _, item := range cd.GetMessages() {
			for _, block := range item.GetAgent().GetResponse().GetBody().GetContent() {
				if text := block.GetText().GetText(); text != "" {
					out = append(out, text)
				}
			}
		}
	}
	return out
}

// workTexts collects every response body text the consumer pushed INSIDE an
// detached work's agent update.
func workTexts(push *fakePusher) []string {
	var out []string
	push.mu.Lock()
	defer push.mu.Unlock()
	for _, delta := range push.work {
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

func TestADetachedAgentsResponseAppearsInItsDetachedWorkDelta(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "subagent speaking"), true)
	if !containsText(workTexts(push), "subagent speaking") {
		t.Fatalf("a detached agent's emissions must reach frontends inside its work, got %v", workTexts(push))
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

func TestTheMainAgentsResponseIsNotPushedIntoAnyDetachedWork(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(mainAssistantEvent(t, 1, "u1", "main agent speaking"), true)
	if len(workTexts(push)) != 0 {
		t.Fatalf("nothing the main agent said is detached work, got %v", workTexts(push))
	}
}

func TestADetachedAgentsFirstRecordOpensItsDetachedWorkInTheSamePush(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "subagent speaking"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.work) != 1 || len(push.work[0].GetOpened()) != 1 {
		t.Fatal("an update must never land in a client that has not been told about its work")
	}
}

func TestTheAsyncPushCarriesTheWorkspacesFence(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if push.work[0].GetFence() != c.fence() {
		t.Fatalf("a stale push must be discardable whole: want fence %q, got %q", c.fence(), push.work[0].GetFence())
	}
}

func TestTheAsyncPushCarriesTheEventsReplayCursor(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 42, "u1", "tu_task", "x"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if push.work[0].GetThroughSeq() != 42 {
		t.Fatalf("want through_seq=42, got %d", push.work[0].GetThroughSeq())
	}
}

// The subject is the EVENT'S OWN feed delta. It is identified by its cursor
// rather than by a bare count of the conversation deltas, which survives the
// anchor's retirement unchanged: a detaching event still owes the feed the
// delta that advances every client's replay cursor past it.
func TestAnAllDetachedEventStillPushesItsFeedDelta(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 7, "u1", "tu_task", "x"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	own := 0
	for _, cd := range push.convo {
		if len(cd.GetMessages()) == 0 && cd.GetThroughSeq() == 7 {
			own++
		}
	}
	if own != 1 {
		t.Fatalf("the event's own feed delta was pushed %d times, want 1: swallowing it would strand every client's replay cursor behind this event", own)
	}
}

func TestASecondDetachedRecordFoldsWithoutReopeningItsDetachedWork(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "one"), true)
	c.pushConversation(sidechainAssistantEvent(t, 2, "u2", "tu_task", "two"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.work[1].GetOpened()) != 0 {
		t.Fatal("a work the receiver already knows is not re-opened by every later record")
	}
}

func TestADetachedWorkNamesTheSameWorkspaceItsDeltaDoes(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)
	push.mu.Lock()
	defer push.mu.Unlock()
	delta := push.work[0]
	if delta.GetOpened()[0].GetDetachedWork().GetWorkspace() != delta.GetWorkspace() {
		t.Fatalf("the work and its envelope must always name one workspace: work=%q delta=%q",
			delta.GetOpened()[0].GetDetachedWork().GetWorkspace(), delta.GetWorkspace())
	}
}

func TestADetachedWorkTheSessionOpenedReachesTheReconnectSnapshot(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "one"), true)
	snap := c.work.snapshot()
	if len(snap) != 1 || len(snap[0].GetDetachedWork().GetAgent().GetEmissions()) != 1 {
		t.Fatalf("a reconnecting client must be handed the fold the pushes had been building, got %v", snap)
	}
}

// --- the launch's ONE DELIVERY of its detached-work message ----------------
//
// REWRITTEN, not retired. These tests used to pin a SECOND delivery: the daemon
// synthesized an "anchor" Message with uuid "async-anchor:"+id onto the
// ConversationDelta so a frontend had somewhere to draw a work that otherwise
// only existed as a raw bubble on the detached-work delta. Detached work IS a
// Message now, so the one it travels as IS its place in the feed and the anchor
// is a duplicate rather than a companion.
//
// What these were protecting still holds and is still covered here: every
// opened piece of work reaches the feed EXACTLY ONCE, carrying its opening
// liveness and its provenance, with a uuid derived from its task rather than
// minted — plus the new fact the collapse creates, that the ConversationDelta
// carries no second copy.

// openedWork collects every detached-work MESSAGE the consumer opened, across
// every detached-work delta it pushed.
func openedWork(push *fakePusher) []*frontendv1.Message {
	var out []*frontendv1.Message
	push.mu.Lock()
	defer push.mu.Unlock()
	for _, delta := range push.work {
		out = append(out, delta.GetOpened()...)
	}
	return out
}

// feedDetachedWork collects every detached-work Message that reached the
// TOP-LEVEL feed. With the anchor retired this must always be empty: the one
// delivery is on the detached-work delta.
func feedDetachedWork(push *fakePusher) []*frontendv1.Message {
	var out []*frontendv1.Message
	push.mu.Lock()
	defer push.mu.Unlock()
	for _, cd := range push.convo {
		for _, item := range cd.GetMessages() {
			if item.GetDetachedWork() != nil {
				out = append(out, item)
			}
		}
	}
	return out
}

func TestALaunchDeliversItsDetachedWorkMessageOnce(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)

	// Assert
	if got := len(openedWork(push)); got != 1 {
		t.Fatalf("opened detached work = %d, want 1: work the feed never receives has no place in the conversation that started it", got)
	}
}

// The anchor's whole reason for existing was that the work travelled as a raw
// bubble the feed could not hold. It travels as a Message now, so a SECOND copy
// on the ConversationDelta would make a frontend draw the same work twice.
func TestADetachedWorkMessageDoesNotAlsoTravelOnTheConversationDelta(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)

	// Assert
	if got := len(feedDetachedWork(push)); got != 0 {
		t.Fatalf("detached-work messages on the conversation delta = %d, want 0: the retired anchor was that second copy", got)
	}
}

func TestTheOpenedDetachedWorkNamesTheCallThatLaunchedIt(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)

	// Assert
	opened := openedWork(push)
	if len(opened) != 1 || opened[0].GetDetachedWork().GetOriginToolUseId() != "tu_task" {
		t.Fatalf("origin_tool_use_id = %q, want %q: the reader cannot tell which call started the work otherwise",
			opened[0].GetDetachedWork().GetOriginToolUseId(), "tu_task")
	}
}

func TestTheOpenedDetachedWorkCarriesItsOpeningLiveness(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)

	// Assert
	if openedWork(push)[0].GetDetachedWork().GetLiveness().GetLive() == nil {
		t.Fatal("work delivered already-settled is unrepresentable while its agent is still running")
	}
}

func TestTheOpenedDetachedWorkDeclaresItsProvenance(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)

	// Assert
	if got := openedWork(push)[0].GetSource(); got != frontendv1.ConversationSource_CONVERSATION_SOURCE_USER {
		t.Fatalf("source = %s, want CONVERSATION_SOURCE_USER: proto3's zero is the malformed-frame value a receiver must reject", got)
	}
}

func TestTheOpenedDetachedWorksUuidIsDerivedFromItsTask(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)

	// Assert: the sidechain envelope names agent_1, and the id is DERIVED from
	// it — a minted uuid would open the same work again on every resync pass.
	if got, want := openedWork(push)[0].GetUuid(), "detached-work:agent_1"; got != want {
		t.Fatalf("uuid = %q, want %q", got, want)
	}
}

// RULING 5: detached work is a FEED ROW, so the message it travels as names no
// parent and IS its own top-level row.
func TestTheOpenedDetachedWorkIsAFeedRow(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)

	// Assert
	m := openedWork(push)[0]
	if m.GetLineage().GetParentMessageId() != "" || m.GetLineage().GetTopLevelMessageId() != m.GetUuid() {
		t.Fatalf("lineage = %v for uuid %q, want no parent and a self-referential root", m.GetLineage(), m.GetUuid())
	}
}

func TestASecondDetachedRecordDoesNotDeliverItsDetachedWorkTwice(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "one"), true)
	c.pushConversation(sidechainAssistantEvent(t, 2, "u2", "tu_task", "two"), true)

	// Assert
	if got := len(openedWork(push)); got != 1 {
		t.Fatalf("opened detached work = %d, want 1: a second delivery makes a frontend draw the same work twice", got)
	}
}

func TestTheDeltaThatOpensDetachedWorkCarriesTheWorkspacesFence(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(sidechainAssistantEvent(t, 1, "u1", "tu_task", "x"), true)

	// Assert
	push.mu.Lock()
	defer push.mu.Unlock()
	for _, delta := range push.work {
		if len(delta.GetOpened()) == 0 {
			continue
		}
		if delta.GetFence() != c.fence() {
			t.Fatalf("the opening delta carries fence %q, want %q: a stale push must be discardable whole", delta.GetFence(), c.fence())
		}
	}
}

func TestAnEventThatOpensNoDetachedWorkDeliversNone(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act
	c.pushConversation(mainAssistantEvent(t, 1, "u1", "main agent speaking"), true)

	// Assert
	if got := len(openedWork(push)); got != 0 {
		t.Fatalf("opened detached work = %d, want 0: nothing the main agent said detached any work", got)
	}
}

// --- the same criterion, through the STREAM plane --------------------------
//
// A Task dispatch's subagent is observed on the stream plane, where the
// detachment is stated by parent_tool_use_id rather than by a transcript
// envelope. The consumer must route it exactly as it routes the file plane's.

// streamSubagentEvent is the store event carrying one detached agent's spoken
// turn as the SDK streams it: no transcript envelope, parent_tool_use_id naming
// the call that launched it.
func streamSubagentEvent(t *testing.T, seq uint64, uuid, parentToolUseID, text string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
			Uuid:            uuid,
			ParentToolUseId: parentToolUseID,
			Message: &datav1.ApiAssistantMessage{Id: "msg_" + uuid, Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
			}},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId:    "s1",
		Seq:          seq,
		ProducedAtMs: 1700000000000,
		Plane:        corev1.Plane_PLANE_STREAM,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

func TestAStreamedSubagentsResponseAppearsInItsDetachedWorkDelta(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(streamSubagentEvent(t, 1, "u1", "toolu_launch", "subagent speaking"), true)
	if !containsText(workTexts(push), "subagent speaking") {
		t.Fatalf("a subagent observed on the stream plane must reach frontends inside its work, got %v", workTexts(push))
	}
}

func TestAStreamedSubagentsResponseDoesNotAppearAsAFeedItem(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(streamSubagentEvent(t, 1, "u1", "toolu_launch", "subagent speaking"), true)
	if containsText(feedTexts(push), "subagent speaking") {
		t.Fatalf("a subagent's streamed response mis-landing in the top-level feed is the defect this repairs, got %v", feedTexts(push))
	}
}

func TestAStreamedMainAgentResponseStillAppearsAsAFeedItem(t *testing.T) {
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	c.pushConversation(streamSubagentEvent(t, 1, "u1", "", "main agent speaking"), true)
	if !containsText(feedTexts(push), "main agent speaking") {
		t.Fatalf("an empty parent_tool_use_id is not a detachment, got %v", feedTexts(push))
	}
}

// --- the curation verdict is observable ------------------------------------

func TestTheCurationVerdictNamesTheCallItRoutedTheRecordsTo(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)
	c.pushConversation(streamSubagentEvent(t, 1, "u1", "toolu_launch", "subagent speaking"), true)
	if countLinesWith(*lines, "async fold CLASSIFIED") != 1 {
		t.Fatalf("the split's verdict must be readable from the log, got %v", *lines)
	}
}

func TestTheCurationVerdictIsSilentForTheMainConversation(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)
	c.pushConversation(streamSubagentEvent(t, 1, "u1", "", "main agent speaking"), true)
	if got := countLinesWith(*lines, "async fold CLASSIFIED"); got != 0 {
		t.Fatalf("records = %d, want 0: the main conversation detaches nothing, and a record per ordinary turn would bury the ones that matter", got)
	}
}

func TestTheAsyncPushNamesTheDetachedWorkItOpened(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)
	c.pushConversation(streamSubagentEvent(t, 1, "u1", "toolu_launch", "subagent speaking"), true)
	if countLinesWith(*lines, "origin_tool_use_id=toolu_launch") == 0 {
		t.Fatalf("an opened work must be recorded with the launching call it hangs under, got %v", *lines)
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

// openWorkflowDetachedWork opens a WORKFLOW work, whose fold is a row journal.
func openWorkflowDetachedWork(t *testing.T, c *consumer) {
	t.Helper()
	if _, err := c.work.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "task_1", Kind: corev1.TaskKind_TASK_KIND_WORKFLOW, ToolUseId: "tu_1",
	}, 10); err != nil {
		t.Fatal(err)
	}
}

// openShellDetachedWork opens a SHELL work, whose fold is a byte spool.
func openShellDetachedWork(t *testing.T, c *consumer) {
	t.Helper()
	if _, err := c.work.observeTaskStarted(&corev1.TaskStarted{
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

// agentFoldOntoTu1 is a detached agent's emissions addressed to whatever work
// tu_1 opened — an AGENT update, whichever kind that work actually is.
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
		for _, item := range cd.GetMessages() {
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
	openWorkflowDetachedWork(t, c)

	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(1)), gapEvent(1))

	cards := failureCards(push)
	if _, ok := cards[gapCardUUID(t, c, "kind_mismatch")]; !ok {
		t.Fatalf("a work that silently stops growing is indistinguishable from a quiet agent and must earn a card, got %v", cards)
	}
}

func TestAKindMismatchedFoldsCardCarriesTheRefusalsEvidence(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openWorkflowDetachedWork(t, c)

	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(1)), gapEvent(1))

	if detail := failureCards(push)[gapCardUUID(t, c, "kind_mismatch")]; !strings.Contains(detail, "daemon bug") {
		t.Fatalf("the warn's diagnostic content becomes the card's evidence, got %q", detail)
	}
}

func TestAKindMismatchedFoldIsRecordedExactlyOnce(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)
	openWorkflowDetachedWork(t, c)

	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(1)), gapEvent(1))

	if got := countLinesWith(*lines, "is a daemon bug and is rejected"); got != 1 {
		t.Fatalf("one fault is one canonical record, got %d in %v", got, *lines)
	}
}

func TestAKindMismatchedFoldReplaysOntoTheSameCard(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openWorkflowDetachedWork(t, c)

	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(1)), gapEvent(1))
	c.pushAsync(c.observeAsync(agentFoldOntoTu1(), gapEvent(2)), gapEvent(2))

	if got := len(failureCards(push)); got != 1 {
		t.Fatalf("a replay must reconcile onto the card it already wrote rather than accumulate twins, got %d", got)
	}
}

func TestARewoundOutputSpoolBecomesAFailureCard(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openShellDetachedWork(t, c)
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
	openShellDetachedWork(t, c)
	c.pushAsync(c.observeAsync(retrievalOutcome("hello world"), gapEvent(1)), gapEvent(1))

	c.pushAsync(c.observeAsync(retrievalOutcome("hi"), gapEvent(2)), gapEvent(2))

	if got := countLinesWith(*lines, "output REWOUND"); got != 1 {
		t.Fatalf("one fault is one canonical record, got %d in %v", got, *lines)
	}
}

func TestARewoundWorkflowJournalBecomesAFailureCard(t *testing.T) {
	push := &fakePusher{}
	c, _ := gapConsumer(t, push)
	openWorkflowDetachedWork(t, c)
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
	openWorkflowDetachedWork(t, c)
	c.pushAsync(c.observeAsync(retrievalOutcome(`{"label":"a"}`+"\n"+`{"label":"b"}`+"\n"), gapEvent(1)), gapEvent(1))

	c.pushAsync(c.observeAsync(retrievalOutcome(`{"label":"a"}`+"\n"), gapEvent(2)), gapEvent(2))

	if got := countLinesWith(*lines, "journal for work"); got != 1 {
		t.Fatalf("one fault is one canonical record, got %d in %v", got, *lines)
	}
}

func TestAnUnclassifiedFoldRefusalStillTakesTheDegradedWarn(t *testing.T) {
	push := &fakePusher{}
	c, lines := gapConsumer(t, push)

	// A detached record naming neither a source call nor an open work is a
	// refusal with no daemon-bug class: it keeps the warn it always had.
	c.pushAsync(c.observeAsync(frontend.Curation{Detached: []frontend.DetachedFold{{AgentID: "agent_x"}}}, gapEvent(1)), gapEvent(1))

	if got := countLinesWith(*lines, "DETACHED WORK FOLD DEGRADED"); got != 1 {
		t.Fatalf("an unclassified refusal must keep its warn rather than disappear, got %d in %v", got, *lines)
	}
}

// --- a REPLAYED fault is history, not news ---------------------------------
//
// The 16 async detachment faults observed on one boot were every one of them a
// replayed historical record, re-warning and re-carding on every subsequent
// boot about detachments that had happened once. A replayed fault is classified
// through the same shared classifier every other replayed anomaly uses: the
// record is kept whole at info, and no live card goes up. The LIVE arm is
// untouched.

// faultLevelConsumer is a consumer bound to the live query with its info and
// warn channels split and its pusher visible, which is the shape a resuming
// daemon has while the store replays rows at it.
func faultLevelConsumer(t *testing.T, push Pusher) (*consumer, *levelSplitLogs) {
	t.Helper()
	logs := &levelSplitLogs{}
	c := newConsumer("ws", "s1", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{},
		logs.logf, nil, nil, nil, nil, nil)
	c.warnf = logs.warnf
	c.now = func() int64 { return 1000 }
	if err := c.accounting.bindHandshakeIdentity(&corev1.ShimHello{
		QueryInstanceId: "live-query", QueryCreatedSeq: 100, VendorSessionId: "vendor-session",
	}); err != nil {
		t.Fatalf("bind handshake: %v", err)
	}
	return c, logs
}

// unclassifiableFault is the push one genuinely unattributable detachment
// produces: a task announced with neither a recognizable kind nor a call to
// look a tool name up by.
func unclassifiableFault(t *testing.T, c *consumer) asyncPush {
	t.Helper()
	push, err := c.work.observeTaskStarted(&corev1.TaskStarted{TaskId: "task_x"}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if len(push.Faults) != 1 {
		t.Fatalf("the arrange step must produce exactly one fault, got %d", len(push.Faults))
	}
	return push
}

// faultEvent is a detachment-fault-bearing event stamped as produced by
// ENVELOPEQUERY, which is the only fact the historical classification reads.
func faultEvent(seq uint64, envelopeQuery string) *corev1.Event {
	return &corev1.Event{SessionId: "s1", Seq: seq, ProducedAtMs: 1700000000000, QueryInstanceId: envelopeQuery}
}

func TestAReplayedDetachmentFaultTakesTheInfoChannel(t *testing.T) {
	// Arrange
	pusher := &fakePusher{}
	c, logs := faultLevelConsumer(t, pusher)
	push := unclassifiableFault(t, c)

	// Act
	c.pushAsync(push, faultEvent(1, "retired-query"))

	// Assert
	if got := countLinesWith(logs.info, "ASYNC DETACHMENT FAULT WITHHELD"); got != 1 {
		t.Fatalf("info records = %d, want 1: a durable row replayed on every boot must be classified, not re-alarmed on; got info=%v warn=%v", got, logs.info, logs.warn)
	}
}

func TestAReplayedDetachmentFaultRaisesNoWarn(t *testing.T) {
	// Arrange
	pusher := &fakePusher{}
	c, logs := faultLevelConsumer(t, pusher)
	push := unclassifiableFault(t, c)

	// Act
	c.pushAsync(push, faultEvent(1, "retired-query"))

	// Assert
	if got := countLinesWith(logs.warn, "ASYNC DETACHMENT FAULT"); got != 0 {
		t.Fatalf("warn records = %d, want 0: the anomaly was surfaced loudly at its original occurrence; got %v", got, logs.warn)
	}
}

func TestAReplayedDetachmentFaultPushesNoLiveCard(t *testing.T) {
	// Arrange
	pusher := &fakePusher{}
	c, _ := faultLevelConsumer(t, pusher)
	push := unclassifiableFault(t, c)

	// Act
	c.pushAsync(push, faultEvent(1, "retired-query"))

	// Assert
	if got := len(failureCards(pusher)); got != 0 {
		t.Fatalf("failure cards = %d, want 0: a replayed fault re-carded on every boot claims a failure is happening now that is not", got)
	}
}

func TestAReplayedDetachmentFaultStillNamesTheDetachmentItWithheld(t *testing.T) {
	// Arrange
	pusher := &fakePusher{}
	c, logs := faultLevelConsumer(t, pusher)
	push := unclassifiableFault(t, c)

	// Act
	c.pushAsync(push, faultEvent(1, "retired-query"))

	// Assert
	if got := countLinesWith(logs.info, push.Faults[0].UUID); got != 1 {
		t.Fatalf("the withheld record must carry the same identity the card would have: got %v", logs.info)
	}
}

func TestALiveDetachmentFaultStillTakesTheWarnChannel(t *testing.T) {
	// Arrange
	pusher := &fakePusher{}
	c, logs := faultLevelConsumer(t, pusher)
	push := unclassifiableFault(t, c)

	// Act
	c.pushAsync(push, faultEvent(1, "live-query"))

	// Assert
	if got := countLinesWith(logs.warn, "ASYNC DETACHMENT FAULT"); got != 1 {
		t.Fatalf("warn records = %d, want 1: a live detachment the daemon cannot show IS new news; got %v", got, logs.warn)
	}
}

func TestALiveDetachmentFaultStillPushesItsCard(t *testing.T) {
	// Arrange
	pusher := &fakePusher{}
	c, _ := faultLevelConsumer(t, pusher)
	push := unclassifiableFault(t, c)

	// Act
	c.pushAsync(push, faultEvent(1, "live-query"))

	// Assert
	if _, ok := failureCards(pusher)[push.Faults[0].UUID]; !ok {
		t.Fatalf("the card is the only place a user learns work is running the daemon cannot show them, got %v", failureCards(pusher))
	}
}

func TestAnAnnouncementBornDetachmentReplaysWithoutAnyFaultRecord(t *testing.T) {
	// Arrange
	pusher := &fakePusher{}
	c, logs := faultLevelConsumer(t, pusher)
	push, err := c.work.observeTaskStarted(&corev1.TaskStarted{
		TaskId: "bgbjlnfrv", Kind: corev1.TaskKind_TASK_KIND_SHELL,
	}, 10)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	c.pushAsync(push, faultEvent(1, "retired-query"))

	// Assert
	if got := countLinesWith(logs.info, "ASYNC DETACHMENT FAULT") + countLinesWith(logs.warn, "ASYNC DETACHMENT FAULT"); got != 0 {
		t.Fatalf("fault records = %d, want 0: a replayed harness background shell was never a fault at either severity; info=%v warn=%v", got, logs.info, logs.warn)
	}
}

// gapCardUUID is the address a gap card is expected under: the store's own
// work id and the gap class, so the test names the same key the production
// derivation does rather than a hand-copied string.
func gapCardUUID(t *testing.T, c *consumer, gap string) string {
	t.Helper()
	snap := c.work.snapshot()
	if len(snap) != 1 {
		t.Fatalf("the arrange step must leave exactly one work, got %d", len(snap))
	}
	return fmt.Sprintf("async-gap:%s:%s", snap[0].GetUuid(), gap)
}
