// The skill-card curator, driven through the real consumer: a launched skill's
// SKILL.md body must reach the frontend as part of the Skill call's card and
// never as a prompt bubble, and every other isMeta record the harness writes
// must reach it not at all.
//
// THIS FILE DRIVES THE CARD PATH, which is the path for an invocation that
// opened no SKILL bubble. An invocation that did opens one gets its body as the
// bubble's own body instead and emits no card at all — async-bubble.proto puts
// the contents on AsyncSkillBubble.body and retires their old rendering — and
// that path is covered in asyncwindows_test.go. Two invocations still reach the
// card: the MERGE run, whose bubble has no body field (covered below), and a
// Skill call whose input names no skill, which is what the bare-`Skill` fixtures
// here are.
//
// THE RECORD SHAPES ARE VERBATIM from a real transcript (a
// /create-or-update-workspace launch): the Skill tool_use, a tool_result whose
// tool_use_id names it, and an isMeta "user" record parented on that
// tool_result whose text opens "Base directory for this skill: …".
package sessioncontroller

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// --- record fixtures --------------------------------------------------------

const skillBody = "Base directory for this skill: /Users/x/.claude/skills/demo\n\n# Demo skill\n\nDo the thing."

// transcriptToolUseEvent is an assistant record making one tool call.
func transcriptToolUseEvent(t *testing.T, seq uint64, uuid, toolUseID, toolName string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: toolUseID, Name: toolName}}},
			}},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// transcriptToolResultEvent is the user record reporting one tool's result.
func transcriptToolResultEvent(t *testing.T, seq uint64, uuid, toolUseID string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
				ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{
						ToolUseId: toolUseID,
						Content:   &datav1.ToolResultBlock_ContentString{ContentString: "Launching skill: demo"},
					}}},
				}},
			}},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// transcriptMetaUserEvent is a user record the harness flagged isMeta, parented
// on the record it answers.
func transcriptMetaUserEvent(t *testing.T, seq uint64, uuid, parentUUID, text string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid, ParentUuid: parentUUID, IsMeta: true},
			Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
				ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
				}},
			}},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// launchSkill feeds the two records that precede a body: the Skill call and its
// tool_result. Returns the call's tool_use id.
func launchSkill(t *testing.T, h *queueHarness) string {
	t.Helper()
	h.controller().consumer.Consume(transcriptToolUseEvent(t, 10, "a-call", "toolu_demo", "Skill"))
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 11, "u-result", "toolu_demo"))
	return "toolu_demo"
}

// skillBodies are every skill_body item the frontend was pushed.
func (h *queueHarness) skillBodies() []*frontendv1.SkillBodyItem {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.SkillBodyItem
	for _, cd := range h.push.convo {
		for _, it := range cd.GetItems() {
			if sb := it.GetAgent().GetSkillBody(); sb != nil {
				out = append(out, sb)
			}
		}
	}
	return out
}

// --- the body-text predicate ------------------------------------------------

func TestIsSkillBodyText(t *testing.T) {
	tests := []struct {
		name    string
		content string
		want    bool
	}{
		{"the harness's own body header", skillBody, true},
		{"leading newlines before the marker", "\n\n" + skillBody, true},
		{"leading spaces and tabs before the marker", " \t" + skillBody, true},
		{"a re-invocation notice on the same chain", "(Re-invocation of /demo — the skill is already loaded.)", false},
		{"a continue nudge after a question", "Continue from where you left off.", false},
		{"a prompt that discusses the marker mid-sentence", "why does it say Base directory for this skill: anywhere?", false},
		{"an empty record", "", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got := isSkillBodyText(tc.content)

			// Assert
			if got != tc.want {
				t.Errorf("isSkillBodyText(%q) = %v, want %v", tc.content, got, tc.want)
			}
		})
	}
}

// --- attachment -------------------------------------------------------------

func TestASkillBodyAttachesToItsOwnCall(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	toolUseID := launchSkill(t, h)

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-body", "u-result", skillBody))

	// Assert
	bodies := h.skillBodies()
	if len(bodies) != 1 {
		t.Fatalf("pushed %d skill_body item(s), want exactly one", len(bodies))
	}
	if got := bodies[0].GetToolUseId(); got != toolUseID {
		t.Errorf("skill_body tool_use_id = %q, want the Skill call's %q", got, toolUseID)
	}
	if got := bodies[0].GetBodyMarkdown(); got != skillBody {
		t.Errorf("skill_body body_markdown = %q, want the SKILL.md verbatim", got)
	}
}

func TestTheMergeRunsBodyStillAttachesToItsCard(t *testing.T) {
	// Arrange: AsyncMergeBubble has no body field, so the merge invocation is the
	// one live skill whose contents still belong on its card.
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(skillCallEvent(t, 10, "a-merge", mergeOriginCall, "create-or-update-workspace", "merge"))
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 11, "u-merge-result", mergeOriginCall))

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-merge-body", "u-merge-result", skillBody))

	// Assert
	bodies := h.skillBodies()
	if len(bodies) != 1 {
		t.Fatalf("pushed %d skill_body item(s) for the merge invocation, want the one its card carries", len(bodies))
	}
	if got := bodies[0].GetToolUseId(); got != mergeOriginCall {
		t.Errorf("skill_body tool_use_id = %q, want the merge call's %q", got, mergeOriginCall)
	}
}

func TestASkillBodyDrawsNoPromptBubble(t *testing.T) {
	// Arrange: this IS the defect — the body used to render as a purple prompt.
	h := newQueueHarness(t, nil)
	launchSkill(t, h)

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-body", "u-result", skillBody))

	// Assert: the tool_result record is the only user item, and it carries no
	// prompt text of its own.
	for _, turn := range h.userTurns() {
		if turn.item.GetUuid() == "u-body" {
			t.Error("the skill body reached the feed as a user turn")
		}
	}
}

func TestOneInvocationYieldsOneSkillBodyItem(t *testing.T) {
	// Arrange: the notice and the body both hang off the same Skill call.
	h := newQueueHarness(t, nil)
	launchSkill(t, h)

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-notice", "u-result", "(Re-invocation of /demo — already loaded.)"))
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 13, "u-body", "u-notice", skillBody))

	// Assert: only the body is a body, and the notice did not become one.
	bodies := h.skillBodies()
	if len(bodies) != 1 {
		t.Fatalf("pushed %d skill_body item(s) for one invocation, want exactly one", len(bodies))
	}
	if got := bodies[0].GetBodyMarkdown(); got != skillBody {
		t.Errorf("the one skill_body carried %q, want the SKILL.md rather than the notice", got)
	}
}

func TestABodyChainedThroughANoticeStillResolves(t *testing.T) {
	// Arrange: the harness parents a re-invocation's body on the NOTICE, not on
	// the tool_result, so the chain has to be followed through isMeta records.
	h := newQueueHarness(t, nil)
	toolUseID := launchSkill(t, h)
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-notice", "u-result", "(Re-invocation of /demo — already loaded.)"))

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 13, "u-body", "u-notice", skillBody))

	// Assert
	bodies := h.skillBodies()
	if len(bodies) != 1 {
		t.Fatalf("pushed %d skill_body item(s), want the chained one", len(bodies))
	}
	if got := bodies[0].GetToolUseId(); got != toolUseID {
		t.Errorf("chained skill_body tool_use_id = %q, want %q", got, toolUseID)
	}
}

func TestConcurrentSkillsEachGetTheirOwnBody(t *testing.T) {
	// Arrange: two live Skill calls, whose bodies a "nearest preceding call"
	// rule could not tell apart.
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(transcriptToolUseEvent(t, 10, "a-one", "toolu_one", "Skill"))
	h.controller().consumer.Consume(transcriptToolUseEvent(t, 11, "a-two", "toolu_two", "Skill"))
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 12, "u-res-one", "toolu_one"))
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 13, "u-res-two", "toolu_two"))

	// Act: the SECOND call's body lands first.
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 14, "u-body-two", "u-res-two", skillBody+" two"))
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 15, "u-body-one", "u-res-one", skillBody+" one"))

	// Assert
	got := map[string]string{}
	for _, b := range h.skillBodies() {
		got[b.GetToolUseId()] = b.GetBodyMarkdown()
	}
	if got["toolu_one"] != skillBody+" one" {
		t.Errorf("toolu_one's body = %q, want its own", got["toolu_one"])
	}
	if got["toolu_two"] != skillBody+" two" {
		t.Errorf("toolu_two's body = %q, want its own", got["toolu_two"])
	}
}

func TestAttachingASkillBodyIsLoud(t *testing.T) {
	// Arrange
	cl := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, cl.logf)
	launchSkill(t, h)

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-body", "u-result", skillBody))

	// Assert
	if !cl.contains("skill body ATTACHED to its card") {
		t.Error("no line accounts for the body reaching its card")
	}
	if !cl.contains("tool_use_id=toolu_demo") {
		t.Error("the line does not name the card the body attached to")
	}
}

// --- withholding ------------------------------------------------------------

func TestANonSkillMetaRecordIsWithheldFromTheFeed(t *testing.T) {
	// Arrange: the harness's "continue" nudge answers an AskUserQuestion, not a
	// skill, and is a prompt nobody typed either way.
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(transcriptToolUseEvent(t, 10, "a-ask", "toolu_ask", "AskUserQuestion"))
	h.controller().consumer.Consume(transcriptToolResultEvent(t, 11, "u-ask-res", "toolu_ask"))

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-nudge", "u-ask-res", "Continue from where you left off."))

	// Assert
	for _, turn := range h.userTurns() {
		if turn.item.GetUuid() == "u-nudge" {
			t.Error("a harness meta record reached the feed as a user turn")
		}
	}
	if bodies := h.skillBodies(); len(bodies) != 0 {
		t.Errorf("pushed %d skill_body item(s) for a non-skill meta record, want none", len(bodies))
	}
}

func TestWithholdingANonSkillMetaRecordIsLoud(t *testing.T) {
	// Arrange: a silent drop is indistinguishable from a lost record.
	cl := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, cl.logf)

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-nudge", "u-unknown", "Continue from where you left off."))

	// Assert
	if !cl.contains("user turn WITHHELD as harness meta record") {
		t.Error("no loud line accounts for the withheld meta record")
	}
	if !cl.contains("uuid=u-nudge") {
		t.Error("the loud line does not name the record it withheld")
	}
}

func TestAnUncorrelatableBodyIsWithheldRatherThanGuessed(t *testing.T) {
	// Arrange: the body arrives before anything establishes its chain, which is
	// exactly when a proximity rule would file it on the wrong card.
	cl := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, cl.logf)
	h.controller().consumer.Consume(transcriptToolUseEvent(t, 10, "a-call", "toolu_demo", "Skill"))

	// Act: the tool_result the body is parented on has not arrived yet.
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 11, "u-body", "u-result", skillBody))

	// Assert
	if bodies := h.skillBodies(); len(bodies) != 0 {
		t.Errorf("attached %d body/bodies with no chain to attach by, want none", len(bodies))
	}
	if !cl.contains("user turn WITHHELD as harness meta record") {
		t.Error("the uncorrelatable body was dropped without a word")
	}
}

func TestAMetaRecordClaimsNoPromptReceipt(t *testing.T) {
	// Arrange: a real submit is outstanding when the skill body lands.
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "run the demo skill"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	launchSkill(t, h)

	// Act
	h.controller().consumer.Consume(transcriptMetaUserEvent(t, 12, "u-body", "u-result", skillBody))

	// Assert: the receipt still awaits the line that really answers it.
	if got := len(h.controller().consumer.snapshotEchoes()); got != 1 {
		t.Errorf("outstanding receipts = %d, want the real prompt's still outstanding", got)
	}
}

// --- replay -----------------------------------------------------------------

func TestAReplayedBodyResolvesToTheSameCard(t *testing.T) {
	// Arrange: a live pass has already correlated the body.
	h := newQueueHarness(t, nil)
	toolUseID := launchSkill(t, h)
	body := transcriptMetaUserEvent(t, 12, "u-body", "u-result", skillBody)
	h.controller().consumer.Consume(body)

	// Act: a re-pull pushes the same record again, not live.
	h.controller().consumer.pushConversation(body, false)

	// Assert: two pushes, one identity — the frontend replaces rather than
	// accumulates.
	bodies := h.skillBodies()
	if len(bodies) != 2 {
		t.Fatalf("pushed %d skill_body item(s) over a live pass plus a replay, want 2", len(bodies))
	}
	for i, b := range bodies {
		if got := b.GetToolUseId(); got != toolUseID {
			t.Errorf("push %d addressed tool_use_id %q, want the same card %q every time", i, got, toolUseID)
		}
	}
}

func TestAReplayedMetaRecordIsWithheldToo(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	nudge := transcriptMetaUserEvent(t, 12, "u-nudge", "u-unknown", "Continue from where you left off.")

	// Act: the resync/re-pull path, which never goes through the live branch.
	h.controller().consumer.pushConversation(nudge, false)

	// Assert
	for _, turn := range h.userTurns() {
		if turn.item.GetUuid() == "u-nudge" {
			t.Error("a replayed meta record reached the feed as a user turn")
		}
	}
}

// --- the existing envelope predicate is untouched ---------------------------

func TestAnUnflaggedMachineryRecordIsStillWithheldByItsEnvelope(t *testing.T) {
	// Arrange: slash-command bookkeeping is NOT flagged isMeta, so the head
	// predicate remains the only thing that catches it.
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(transcriptUserEvent(t, 12, "u-machinery", compactMachinery))

	// Assert
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for unflagged machinery, want none", len(turns))
	}
}

func TestARealPromptIsNeitherWithheldNorAttached(t *testing.T) {
	// Arrange: a prompt during a live skill, which must survive both curators.
	h := newQueueHarness(t, nil)
	launchSkill(t, h)

	// Act
	h.controller().consumer.Consume(transcriptUserEvent(t, 12, "u-real", "carry on"))

	// Assert
	var saw bool
	for _, turn := range h.userTurns() {
		if turn.item.GetUuid() == "u-real" {
			saw = true
		}
	}
	if !saw {
		t.Error("a real prompt was withheld alongside the harness's meta records")
	}
	if bodies := h.skillBodies(); len(bodies) != 0 {
		t.Errorf("a real prompt produced %d skill_body item(s), want none", len(bodies))
	}
}
