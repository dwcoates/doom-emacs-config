package frontend

import (
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// captureWarn is a warn sink that renders each record whole, so a test can
// assert on the evidence a defect carried and not merely on its count.
func captureWarn(lines *[]string) func(string, ...any) {
	return func(format string, args ...any) { *lines = append(*lines, fmt.Sprintf(format, args...)) }
}

// conversationFrame wraps one message in the frame arm the audit reads feed
// messages from.
func conversationFrame(m *frontendv1.Message) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_ConversationDelta{
		ConversationDelta: &frontendv1.ConversationDelta{Messages: []*frontendv1.Message{m}},
	}}
}

// --- the four defect classes -----------------------------------------------

func TestAuditFrameLineageRecordsEachDefectClass(t *testing.T) {
	tests := []struct {
		name    string
		message *frontendv1.Message
		marker  string
	}{
		{
			// A message with no MessageLineage at all has no
			// top_level_message_id for a page query to select it by, so it is
			// invisible to paging rather than merely mis-placed.
			name:    "no lineage at all",
			message: &frontendv1.Message{Uuid: "m1"},
			marker:  "MESSAGE LINEAGE MISSING",
		},
		{
			// top_level_message_id is never empty, on any message including a
			// feed row, where it equals the message's own uuid.
			name:    "empty root",
			message: &frontendv1.Message{Uuid: "m1", Lineage: &frontendv1.MessageLineage{}},
			marker:  "MESSAGE LINEAGE ROOTLESS",
		},
		{
			// No parent means the message sits directly in the feed, and the
			// contract states its root IS its own uuid. A different root is the
			// denormalization having drifted.
			name:    "feed row whose root is not itself",
			message: &frontendv1.Message{Uuid: "m1", Lineage: &frontendv1.MessageLineage{TopLevelMessageId: "other"}},
			marker:  "CONTRADICTS ITSELF",
		},
		{
			// Naming a parent and then naming yourself as the feed row would
			// make the message a page slot AND a child at once.
			name: "contained message claiming to be its own root",
			message: &frontendv1.Message{Uuid: "m1", Lineage: &frontendv1.MessageLineage{
				TopLevelMessageId: "m1", ParentMessageId: "p1",
			}},
			marker: "CONTRADICTS ITSELF",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var lines []string

			// Act
			auditFrameLineage(conversationFrame(tc.message), captureWarn(&lines))

			// Assert
			if len(lines) != 1 || !strings.Contains(lines[0], tc.marker) {
				t.Fatalf("records = %v, want exactly one naming %q: a silent drift is found only as a short page nobody can explain", lines, tc.marker)
			}
		})
	}
}

// --- what the audit must NOT do --------------------------------------------

func TestAuditFrameLineageIsSilentForAWellFormedFeedRow(t *testing.T) {
	// Arrange
	var lines []string
	m := &frontendv1.Message{Uuid: "m1", Lineage: FeedRowLineage("m1")}

	// Act
	auditFrameLineage(conversationFrame(m), captureWarn(&lines))

	// Assert
	if len(lines) != 0 {
		t.Fatalf("records = %v, want none: a record per well-formed message would bury the ones that matter", lines)
	}
}

func TestAuditFrameLineageIsSilentForAWellFormedContainedMessage(t *testing.T) {
	// Arrange
	var lines []string
	m := &frontendv1.Message{Uuid: "m2", Lineage: &frontendv1.MessageLineage{
		TopLevelMessageId: "m1", ParentMessageId: "m1",
	}}

	// Act
	auditFrameLineage(conversationFrame(m), captureWarn(&lines))

	// Assert
	if len(lines) != 0 {
		t.Fatalf("records = %v, want none", lines)
	}
}

// IT NEVER DROPS THE FRAME. A broken lineage is a bookkeeping defect; the
// message is still the user's content, and withholding it would turn that
// defect into missing conversation.
func TestAuditFrameLineageNeverDropsTheFrame(t *testing.T) {
	// Arrange
	var lines []string
	frame := conversationFrame(&frontendv1.Message{Uuid: "m1"})

	// Act
	auditFrameLineage(frame, captureWarn(&lines))

	// Assert
	if got := len(frame.GetConversationDelta().GetMessages()); got != 1 {
		t.Fatalf("the frame carries %d messages after the audit, want 1 left untouched", got)
	}
}

func TestAuditFrameLineageRecordsTheDefectiveMessagesUuid(t *testing.T) {
	// Arrange
	var lines []string

	// Act
	auditFrameLineage(conversationFrame(&frontendv1.Message{Uuid: "m-broken"}), captureWarn(&lines))

	// Assert: the record has to name the message, or the defect is unactionable.
	if len(lines) != 1 || !strings.Contains(lines[0], "m-broken") {
		t.Fatalf("records = %v, want one naming the offending uuid", lines)
	}
}

// --- every Message-bearing arm is walked ------------------------------------

func TestAuditFrameLineageWalksEveryMessageBearingArm(t *testing.T) {
	broken := func() *frontendv1.Message { return &frontendv1.Message{Uuid: "m1"} }
	tests := []struct {
		name  string
		frame *frontendv1.FrontendFrame
		where string
	}{
		{
			name:  "conversation delta",
			frame: conversationFrame(broken()),
			where: "conversation_delta",
		},
		{
			name: "detached work delta opened",
			frame: &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_DetachedWorkDelta{
				DetachedWorkDelta: &frontendv1.DetachedWorkDelta{Opened: []*frontendv1.Message{broken()}},
			}},
			where: "detached_work_delta.opened",
		},
		{
			name: "conversation page",
			frame: &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_ConversationPage{
				ConversationPage: &frontendv1.ConversationPage{Messages: []*frontendv1.Message{broken()}},
			}},
			where: "conversation_page",
		},
		{
			name: "snapshot detached work",
			frame: &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_Snapshot{
				Snapshot: &frontendv1.StateSnapshot{DetachedWork: []*frontendv1.Message{broken()}},
			}},
			where: "snapshot.detached_work",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var lines []string

			// Act
			auditFrameLineage(tc.frame, captureWarn(&lines))

			// Assert
			if len(lines) != 1 || !strings.Contains(lines[0], "frame="+tc.where) {
				t.Fatalf("records = %v, want one naming frame=%s: an unwalked arm is an arm whose drift is silent", lines, tc.where)
			}
		})
	}
}

// A frame carrying no messages at all — a typing delta, a topbar — has no
// lineage to check and must not be made to look like one that does.
func TestAuditFrameLineageIsSilentForAFrameWithNoMessages(t *testing.T) {
	// Arrange
	var lines []string
	frame := &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_TypingDelta{
		TypingDelta: &frontendv1.TypingDelta{Workspace: "/ws"},
	}}

	// Act
	auditFrameLineage(frame, captureWarn(&lines))

	// Assert
	if len(lines) != 0 {
		t.Fatalf("records = %v, want none: a frame with no Message-bearing arm has nothing to audit", lines)
	}
}

// --- the audit's own guards -------------------------------------------------

func TestAuditFrameLineageToleratesANilFrame(t *testing.T) {
	// Arrange
	var lines []string

	// Act
	auditFrameLineage(nil, captureWarn(&lines))

	// Assert
	if len(lines) != 0 {
		t.Fatalf("records = %v, want none for a nil frame", lines)
	}
}

func TestAuditFrameLineageToleratesANilWarnSink(t *testing.T) {
	// Arrange, Act, Assert: it must not panic when no sink was wired.
	auditFrameLineage(conversationFrame(&frontendv1.Message{Uuid: "m1"}), nil)
}
