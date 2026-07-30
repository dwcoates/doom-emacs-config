// The no-response-placeholder curator, driven through the real consumer.
//
// THE RECORD SHAPES ARE VERBATIM from live transcripts: a `<synthetic>`
// assistant line whose sole text block reads "No response requested.", the
// `<synthetic>` API-failure lines that share its model id, and the genuine
// model turn that once produced the very same sentence.
package sessioncontroller

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// --- fixtures ---------------------------------------------------------------

// assistantTextItem is one already-curated assistant item carrying text blocks.
func assistantTextItem(uuid, model string, texts ...string) *frontendv1.ConversationItem {
	blocks := make([]*datav1.ContentBlock, 0, len(texts))
	for _, tx := range texts {
		blocks = append(blocks, &datav1.ContentBlock{
			Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: tx}},
		})
	}
	return &frontendv1.ConversationItem{
		Uuid: uuid,
		Item: &frontendv1.ConversationItem_AssistantMessage{
			AssistantMessage: &datav1.ApiAssistantMessage{Model: model, Content: blocks},
		},
	}
}

// transcriptAssistantTextEvent is the DURABLE account of an assistant record as
// the real pipeline delivers it: a file-plane transcript assistant line.
func transcriptAssistantTextEvent(t *testing.T, seq uint64, uuid, model, text string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message: &datav1.ApiAssistantMessage{Model: model, Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
			}},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// assistantTurns returns every pushed conversation item carrying an assistant
// message, in push order.
func (h *queueHarness) assistantTurns() []*frontendv1.ConversationItem {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []*frontendv1.ConversationItem
	for _, cd := range h.push.convo {
		for _, it := range cd.GetItems() {
			if it.GetAssistantMessage() != nil {
				out = append(out, it)
			}
		}
	}
	return out
}

// --- the predicate ----------------------------------------------------------

func TestIsNoResponsePlaceholder(t *testing.T) {
	tests := []struct {
		name string
		item *frontendv1.ConversationItem
		want bool
	}{
		{
			name: "the placeholder itself",
			item: assistantTextItem("a1", syntheticModel, "No response requested."),
			want: true,
		},
		{
			name: "the placeholder with surrounding whitespace",
			item: assistantTextItem("a1", syntheticModel, "\n  No response requested.\n"),
			want: true,
		},
		{
			name: "a synthetic rate-limit card shares the model id and must be shown",
			item: assistantTextItem("a1", syntheticModel, "You've hit your session limit · resets 5pm (America/New_York)"),
			want: false,
		},
		{
			name: "a synthetic API-error card must be shown",
			item: assistantTextItem("a1", syntheticModel, "API Error: Connection closed mid-response. The response above may be incomplete."),
			want: false,
		},
		{
			name: "a genuine model turn that really said the sentence",
			item: assistantTextItem("a1", "claude-fable-5", "No response requested."),
			want: false,
		},
		{
			name: "a synthetic record that opens with the sentence and says more",
			item: assistantTextItem("a1", syntheticModel, "No response requested. Here is why that happened."),
			want: false,
		},
		{
			name: "a synthetic record whose second block carries content",
			item: assistantTextItem("a1", syntheticModel, "No response requested.", "and one more thing"),
			want: false,
		},
		{
			name: "a synthetic record whose sole block is not text",
			item: &frontendv1.ConversationItem{
				Uuid: "a1",
				Item: &frontendv1.ConversationItem_AssistantMessage{
					AssistantMessage: &datav1.ApiAssistantMessage{Model: syntheticModel, Content: []*datav1.ContentBlock{
						{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: "t1", Name: "Read"}}},
					}},
				},
			},
			want: false,
		},
		{
			name: "an assistant record with no model id at all",
			item: assistantTextItem("a1", "", "No response requested."),
			want: false,
		},
		{
			name: "a user record quoting the sentence",
			item: blockUserItem("u1", "No response requested."),
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			got := isNoResponsePlaceholder(tc.item)

			// Assert
			if got != tc.want {
				t.Errorf("isNoResponsePlaceholder = %v, want %v", got, tc.want)
			}
		})
	}
}

// --- the curator ------------------------------------------------------------

func TestANoResponsePlaceholderIsWithheldFromTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(transcriptAssistantTextEvent(t, 12, "a-placeholder", syntheticModel, noResponseRequestedText))

	// Assert: no model produced it and it says nothing, so no bubble is drawn.
	if turns := h.assistantTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d assistant turn(s) for a placeholder record, want none", len(turns))
	}
}

func TestARealAnswerBesideAPlaceholderStillReachesTheFeed(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(transcriptAssistantTextEvent(t, 12, "a-placeholder", syntheticModel, noResponseRequestedText))
	h.controller().consumer.Consume(transcriptAssistantTextEvent(t, 13, "a-real", "claude-fable-5", "here is the answer"))

	// Assert
	turns := h.assistantTurns()
	if len(turns) != 1 {
		t.Fatalf("pushed %d assistant turn(s), want the real answer alone", len(turns))
	}
	if got := turns[0].GetUuid(); got != "a-real" {
		t.Errorf("pushed assistant turn uuid = %q, want the real answer a-real", got)
	}
}

func TestASyntheticApiFailureCardStillReachesTheFeed(t *testing.T) {
	// Arrange: the failure cards share the placeholder's model id, and a user
	// whose turn died to a rate limit needs to be told so.
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(transcriptAssistantTextEvent(t, 12, "a-ratelimit", syntheticModel,
		"You've hit your session limit · resets 5pm (America/New_York)"))

	// Assert
	turns := h.assistantTurns()
	if len(turns) != 1 {
		t.Fatalf("pushed %d assistant turn(s), want the rate-limit card", len(turns))
	}
}

func TestWithholdingAPlaceholderIsLoud(t *testing.T) {
	// Arrange: a silent drop is indistinguishable from a lost record.
	cl := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, cl.logf)

	// Act
	h.controller().consumer.Consume(transcriptAssistantTextEvent(t, 12, "a-placeholder", syntheticModel, noResponseRequestedText))

	// Assert
	if !cl.contains("assistant turn WITHHELD as the vendor's no-response placeholder") {
		t.Error("no loud line accounts for the withheld placeholder record")
	}
	if !cl.contains("uuid=a-placeholder") {
		t.Error("the loud line does not name the record it withheld")
	}
}

func TestAWithheldPlaceholderStillAdvancesTheSeq(t *testing.T) {
	// Arrange
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.Consume(transcriptAssistantTextEvent(t, 12, "a-placeholder", syntheticModel, noResponseRequestedText))

	// Assert: the record is retained and seq-accounted exactly as any other —
	// only the rendered item is withheld.
	if got := h.controller().consumer.newestRetainedSeq(); got != 12 {
		t.Errorf("newest retained seq = %d, want the placeholder record's own 12", got)
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
		t.Fatal("no delta carried through_seq 12, so no frontend cursor advanced past the placeholder record")
	}
	if got := len(delta.GetItems()); got != 0 {
		t.Errorf("the through_seq-12 delta carried %d item(s), want none", got)
	}
}

func TestAReplayedPlaceholderIsWithheldToo(t *testing.T) {
	// Arrange: a resync must not re-pollute a feed the live path kept clean.
	h := newQueueHarness(t, nil)

	// Act
	h.controller().consumer.pushConversation(transcriptAssistantTextEvent(t, 12, "a-placeholder", syntheticModel, noResponseRequestedText), false)

	// Assert
	if turns := h.assistantTurns(); len(turns) != 0 {
		t.Fatalf("replayed %d assistant turn(s) for a placeholder record, want none", len(turns))
	}
}
