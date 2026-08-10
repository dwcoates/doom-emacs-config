package sessioncontroller

import (
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// taskNotificationBody is the completion notice the harness writes into the
// conversation when detached work finishes.
const taskNotificationBody = "<task-notification>\n<task-id>a8abc09d2bc681337</task-id>\n<tool-use-id>toolu_012Hbds</tool-use-id>\n<status>completed</status>\n</task-notification>"

// --- the predicate ----------------------------------------------------------

func TestIsTaskNotificationRecord(t *testing.T) {
	userItem := func(uuid string) *frontendv1.ConversationItem {
		return &frontendv1.ConversationItem{
			Uuid: uuid,
			Item: &frontendv1.ConversationItem_UserMessage{UserMessage: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: taskNotificationBody},
			}},
		}
	}
	assistantItem := func(uuid string) *frontendv1.ConversationItem {
		return &frontendv1.ConversationItem{
			Uuid: uuid,
			Item: &frontendv1.ConversationItem_Agent{Agent: &frontendv1.AgentEmission{}},
		}
	}
	tests := []struct {
		name string
		item *frontendv1.ConversationItem
		kind datav1.OriginKind
		want bool
	}{
		{
			name: "a user record the harness stamped task-notification",
			item: userItem("u1"),
			kind: datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION,
			want: true,
		},
		{
			name: "a user record the harness stamped human",
			item: userItem("u1"),
			kind: datav1.OriginKind_ORIGIN_KIND_HUMAN,
			want: false,
		},
		{
			name: "an unattributed user record is never assumed to be one",
			item: userItem("u1"),
			kind: datav1.OriginKind_ORIGIN_KIND_UNSPECIFIED,
			want: false,
		},
		{
			name: "a peer session's record is a different producer",
			item: userItem("u1"),
			kind: datav1.OriginKind_ORIGIN_KIND_PEER,
			want: false,
		},
		{
			name: "a non-user arm draws no prompt bubble to withhold",
			item: assistantItem("u1"),
			kind: datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION,
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			env := frontend.RecordEnvelope{OriginKind: tc.kind}

			// Act
			got := isTaskNotificationRecord(tc.item, env)

			// Assert
			if got != tc.want {
				t.Errorf("isTaskNotificationRecord = %v, want %v", got, tc.want)
			}
		})
	}
}

// --- the curator ------------------------------------------------------------

func TestATaskNotificationRecordIsWithheldFromTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act: detached work completes and the harness writes its notice.
	h.controller().consumer.Consume(userLineEvent(t, 12, "u-notify", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))

	// Assert: the user never typed it, so no bubble is drawn for it.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a task notification, want none", len(turns))
	}
}

func TestARealPromptBesideATaskNotificationStillReachesTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(userLineEvent(t, 12, "u-notify", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))
	h.controller().consumer.Consume(transcriptUserEvent(t, 13, "u-real", "carry on"))

	// Assert
	turns := h.userTurns()
	if len(turns) != 1 {
		t.Fatalf("pushed %d user turn(s), want the real prompt alone", len(turns))
	}
	if got := turns[0].item.GetUuid(); got != "u-real" {
		t.Errorf("pushed user turn uuid = %q, want the real prompt u-real", got)
	}
}

func TestAPromptQuotingTheTaskNotificationEnvelopeStillReachesTheFeed(t *testing.T) {
	// Arrange: the verdict is the harness's typed origin, never the prose — a
	// person asking about the markup is asking a real question.
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(userLineEvent(t, 12, "u-quote", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_HUMAN))

	// Assert
	turns := h.userTurns()
	if len(turns) != 1 {
		t.Fatalf("pushed %d user turn(s), want the quoted prompt kept", len(turns))
	}
	if got := turns[0].item.GetUuid(); got != "u-quote" {
		t.Errorf("pushed user turn uuid = %q, want u-quote", got)
	}
}

func TestWithholdingATaskNotificationIsLoud(t *testing.T) {
	// Arrange: a silent drop is indistinguishable from a lost record.
	cl := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, cl.logf)

	// Act
	h.controller().consumer.Consume(userLineEvent(t, 12, "u-notify", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))

	// Assert
	if !cl.contains("user turn WITHHELD as a detached-work task notification") {
		t.Error("no loud line accounts for the withheld task notification")
	}
	if !cl.contains("uuid=u-notify") {
		t.Error("the loud line does not name the record it withheld")
	}
}

func TestAWithheldTaskNotificationStillAdvancesTheSeq(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(userLineEvent(t, 12, "u-notify", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))

	// Assert: the record is retained and seq-accounted exactly as any other —
	// only the rendered item is withheld.
	if got := h.controller().consumer.newestRetainedSeq(); got != 12 {
		t.Errorf("newest retained seq = %d, want the notification's own 12", got)
	}
	var delta *frontendv1.ConversationDelta
	h.push.mu.Lock()
	for _, cd := range h.push.convo {
		if cd.GetThroughSeq() == 12 {
			delta = cd
		}
	}
	h.push.mu.Unlock()
	if delta == nil {
		t.Fatal("no delta carried through_seq 12, so no frontend cursor advanced past the task notification")
	}
	if got := len(delta.GetItems()); got != 0 {
		t.Errorf("the through_seq-12 delta carried %d item(s), want none", got)
	}
}

func TestATaskNotificationClaimsNoPromptReceipt(t *testing.T) {
	// Arrange: a real submit is outstanding when detached work reports back.
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: the notification arrives BEFORE the real prompt's durable line.
	h.controller().consumer.Consume(userLineEvent(t, 12, "u-notify", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))

	// Assert: the receipt is still outstanding for the line that really answers
	// it — a notification retiring it would leave the real prompt's own line
	// unattributed and the bubble duplicated.
	if got := len(h.controller().consumer.snapshotEchoes()); got != 1 {
		t.Fatalf("outstanding receipts = %d, want the real submit's still held", got)
	}
}

func TestTheRealLineIsStillAttributedAfterATaskNotification(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act
	h.controller().consumer.Consume(userLineEvent(t, 12, "u-notify", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))
	h.controller().consumer.Consume(transcriptUserEvent(t, 13, "u-real", "hello there"))

	// Assert
	turns := h.userTurns()
	if len(turns) != 2 {
		t.Fatalf("pushed %d user turn(s), want the receipt and the real durable line", len(turns))
	}
	if got := turns[1].item.GetRequestId(); got != "r1" {
		t.Errorf("durable line request_id = %q, want the submit's id r1", got)
	}
}

func TestAStreamPlaneTaskNotificationIsWithheldFromTheFeed(t *testing.T) {
	// Arrange: the notification reaches the daemon on the stream plane before
	// the transcript carries it, so curating only the file plane would still
	// draw the bubble.
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(userStreamEvent(t, 12, "u-notify-stream", "", taskNotificationBody, 0, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))

	// Assert
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a stream-plane task notification, want none", len(turns))
	}
}

func TestAStreamPlaneHumanPromptStillReachesTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(userStreamEvent(t, 12, "u-human-stream", "", "carry on", 0, datav1.OriginKind_ORIGIN_KIND_HUMAN))

	// Assert
	turns := h.userTurns()
	if len(turns) != 1 {
		t.Fatalf("pushed %d user turn(s), want the human prompt", len(turns))
	}
	if got := turns[0].item.GetUuid(); got != "u-human-stream" {
		t.Errorf("pushed user turn uuid = %q, want u-human-stream", got)
	}
}

func TestAReplayedTaskNotificationIsWithheldToo(t *testing.T) {
	// Arrange: a resync must not re-pollute a feed the live path kept clean.
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.pushConversation(userLineEvent(t, 12, "u-notify", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION), false)

	// Assert
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("replayed %d user turn(s) for a task notification, want none", len(turns))
	}
}
