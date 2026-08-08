// A launched skill's SKILL.md, end to end over the REAL processes: the three
// transcript records a skill invocation writes are put into the real
// shim-store, fanned out to the real TS shim, forwarded to the daemon, and must
// reach a connected frontend as ONE skill_body item addressed to the Skill
// call — never as the user turn the isMeta record would otherwise draw.
//
// WHY THE RECORDS ARE INJECTED RATHER THAN PROVOKED. Same reason as
// machinery_e2e_test.go and clearcompact_e2e_test.go, whose helpers this file
// reuses READ-ONLY (liveSession, storeProducer.write, awaitItem): the sidecar
// is the sole producer of file-plane records and it produces them by tailing a
// real vendor transcript, which the `--fake` harness has none of. So the test
// writes the store exactly the event shape the sidecar writes for each
// transcript line and exercises everything downstream for real.
package e2e

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// e2eSkillBody is a VERBATIM-shaped skill body: the "Base directory" header
// the harness prepends, then the SKILL.md itself.
const e2eSkillBody = "Base directory for this skill: /Users/x/.claude/skills/demo\n\n# Demo skill\n\nDo the thing."

// sidecarLineEvent wraps one transcript line the way handler.vendorEvent does:
// file plane, PERSISTENT, no dedup key (the store derives its own uuid: key).
func sidecarLineEvent(t *testing.T, vendorSessionID string, line *datav1.TranscriptLine) *corev1.Event {
	t.Helper()
	a, err := anypb.New(line)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

// skillCallLine is the assistant record making the Skill call.
func skillCallLine(uuid, toolUseID string) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{Uuid: uuid},
		Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: toolUseID, Name: "Skill"}}},
		}},
	}}}
}

// skillResultLine is the user record reporting the Skill call's result.
func skillResultLine(uuid, toolUseID string) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
		Envelope: &datav1.LineEnvelope{Uuid: uuid},
		Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
			ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{
					ToolUseId: toolUseID,
					Content:   &datav1.ToolResultBlock_ContentString{ContentString: "Launching skill: demo"},
				}}},
			}},
		}},
	}}}
}

// metaUserLine is the isMeta "user" record the harness writes for a skill body.
func metaUserLine(uuid, parentUUID, text string) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
		Envelope: &datav1.LineEnvelope{Uuid: uuid, ParentUuid: parentUUID, IsMeta: true},
		Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
			ContentBlocks: &datav1.ApiContentBlocks{Blocks: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
			}},
		}},
	}}}
}

// TestE2ESkillBodyReachesTheFrontendAsItsOwnArm drives the happy path over the
// real pipeline: the isMeta record survives the store and the shim with its
// flag and its parent linkage intact, and the daemon resolves it to the Skill
// call it belongs to.
func TestE2ESkillBodyReachesTheFrontendAsItsOwnArm(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const toolUseID = "toolu_e2e_demo"

	// Act — the three records a skill invocation writes, in transcript order.
	store.write(sidecarLineEvent(t, vendorID, skillCallLine("e2e-skill-call", toolUseID)))
	store.write(sidecarLineEvent(t, vendorID, skillResultLine("e2e-skill-result", toolUseID)))
	store.write(sidecarLineEvent(t, vendorID, metaUserLine("e2e-skill-body", "e2e-skill-result", e2eSkillBody)))

	// Assert
	item, _ := awaitItem(t, conn, cwd, "the skill body item", func(it *frontendv1.ConversationItem) bool {
		return it.GetAgent().GetSkillBody() != nil
	})
	if got, want := item.GetAgent().GetSkillBody().GetToolUseId(), toolUseID; got != want {
		t.Errorf("skill_body tool_use_id = %q, want the Skill call's %q", got, want)
	}
	if got := item.GetAgent().GetSkillBody().GetBodyMarkdown(); got != e2eSkillBody {
		t.Errorf("skill_body body_markdown = %q, want the SKILL.md verbatim", got)
	}
}

// TestE2ESkillBodyNeverReachesTheFrontendAsAUserTurn drives the defect itself:
// before the curation, the body's own record drew a page-long prompt bubble.
//
// The trailing real prompt is the synchronization point, and a sound one: the
// store preserves per-session write order and the daemon curates in that order,
// so once the real prompt's item has arrived every record before it has been
// through the whole pipeline. No sleeping for a guessed duration.
func TestE2ESkillBodyNeverReachesTheFrontendAsAUserTurn(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const realPrompt = "now carry on"

	// Act
	store.write(sidecarLineEvent(t, vendorID, skillCallLine("e2e-call-2", "toolu_two")))
	store.write(sidecarLineEvent(t, vendorID, skillResultLine("e2e-result-2", "toolu_two")))
	store.write(sidecarLineEvent(t, vendorID, metaUserLine("e2e-body-2", "e2e-result-2", e2eSkillBody)))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-real-2", realPrompt))

	// Assert
	_, before := awaitItem(t, conn, cwd, "the real prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == realPrompt
	})
	for _, it := range before {
		if it.GetUuid() == "e2e-body-2" && it.GetUserMessage() != nil {
			t.Errorf("the skill body reached the frontend as a user item uuid=%q", it.GetUuid())
		}
	}
}

// TestE2EANonSkillMetaRecordReachesTheFrontendAtAll drives the other isMeta
// shape: a record the harness wrote for the model that belongs to no skill must
// be withheld entirely rather than attached to whatever card came last.
func TestE2EANonSkillMetaRecordIsWithheldEntirely(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const realPrompt = "and now the real one"

	// Act — a live Skill call, then a meta record that is NOT its body.
	store.write(sidecarLineEvent(t, vendorID, skillCallLine("e2e-call-3", "toolu_three")))
	store.write(sidecarLineEvent(t, vendorID, skillResultLine("e2e-result-3", "toolu_three")))
	store.write(sidecarLineEvent(t, vendorID, metaUserLine("e2e-nudge-3", "e2e-result-3", "Continue from where you left off.")))
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-real-3", realPrompt))

	// Assert
	_, before := awaitItem(t, conn, cwd, "the real prompt's user item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == realPrompt
	})
	for _, it := range before {
		if it.GetUuid() == "e2e-nudge-3" {
			t.Errorf("a harness meta record reached the frontend as item uuid=%q", it.GetUuid())
		}
		if it.GetAgent().GetSkillBody() != nil {
			t.Errorf("a non-body meta record was attached to card %q", it.GetAgent().GetSkillBody().GetToolUseId())
		}
	}
}
