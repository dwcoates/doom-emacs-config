package dedup

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// mustVendor wraps a data.v1 message in a core Event's vendor Any.
func mustVendor(t *testing.T, msg proto.Message) *corev1.Event {
	t.Helper()
	a, err := anypb.New(msg)
	if err != nil {
		t.Fatalf("anypb.New(%T): %v", msg, err)
	}
	return &corev1.Event{Payload: &corev1.Event_Vendor{Vendor: a}}
}

// withKey stamps a producer-supplied dedup key on an event.
func withKey(ev *corev1.Event, key string) *corev1.Event {
	ev.DedupKey = key
	return ev
}

// toolResultMsg builds an ApiUserMessage carrying a single tool_result block.
func toolResultMsg(toolUseID string) *datav1.ApiUserMessage {
	return &datav1.ApiUserMessage{
		Content: &datav1.ApiUserMessage_ContentBlocks{
			ContentBlocks: &datav1.ApiContentBlocks{
				Blocks: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_ToolResult{
						ToolResult: &datav1.ToolResultBlock{ToolUseId: toolUseID},
					}},
				},
			},
		},
	}
}

func TestDerive(t *testing.T) {
	// Arrange: build the twins and the producer-keyed events.
	streamAssistant := mustVendor(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{
			Assistant: &datav1.AssistantMessage{Uuid: "A1"},
		},
	})
	diskAssistant := mustVendor(t, &datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_Assistant{
			Assistant: &datav1.AssistantLine{Envelope: &datav1.LineEnvelope{Uuid: "A1"}},
		},
	})
	streamResult := mustVendor(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{Uuid: "R1"}},
	})
	streamUser := mustVendor(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_User{User: &datav1.UserMessage{Uuid: "U1"}},
	})
	streamToolResult := mustVendor(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_User{User: &datav1.UserMessage{
			Uuid:    "ignored-because-tool-result",
			Message: toolResultMsg("T1"),
		}},
	})
	diskToolResultBlock := mustVendor(t, &datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: "irrelevant"},
			Message:  toolResultMsg("T1"),
		}},
	})
	diskToolResultEnvelope := mustVendor(t, &datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: "irrelevant", SourceToolUseId: "T1"},
		}},
	})

	tests := []struct {
		name string
		ev   *corev1.Event
		want string
	}{
		{name: "nil event", ev: nil, want: ""},
		{name: "stream assistant keys on uuid", ev: streamAssistant, want: "uuid:A1"},
		{name: "disk assistant keys on same uuid (collides)", ev: diskAssistant, want: "uuid:A1"},
		{name: "stream result keys on uuid", ev: streamResult, want: "uuid:R1"},
		{name: "stream plain user keys on uuid", ev: streamUser, want: "uuid:U1"},
		{name: "stream tool result keys on tool_use_id", ev: streamToolResult, want: "tur:T1"},
		{name: "disk tool result (content block) keys on tool_use_id", ev: diskToolResultBlock, want: "tur:T1"},
		{name: "disk tool result (envelope fallback) keys on tool_use_id", ev: diskToolResultEnvelope, want: "tur:T1"},
		{
			name: "producer-set turn key wins",
			ev:   &corev1.Event{DedupKey: "turn:s1:turnuuid", Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{}}},
			want: "turn:s1:turnuuid",
		},
		{
			name: "producer-set journal key wins over derivable payload",
			ev:   withKey(mustVendor(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Uuid: "A9"}}}), "wf:run7:v2:abc:result"),
			want: "wf:run7:v2:abc:result",
		},
		{
			name: "core lifecycle without identity is never deduped",
			ev:   &corev1.Event{Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{}}},
			want: "",
		},
		{
			name: "vendor message without uuid is never deduped",
			ev:   mustVendor(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{}}}),
			want: "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Derive(tc.ev)
			// Assert
			if got != tc.want {
				t.Fatalf("Derive() = %q, want %q", got, tc.want)
			}
		})
	}
}
