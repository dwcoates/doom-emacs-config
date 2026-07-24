package frontend

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// A fixed producer stamp so item `ts` fields are deterministic across goldens.
const producedMs int64 = 1700000000000

// mustAny wraps a proto message in an Any or fails the test.
func mustAny(t *testing.T, m proto.Message) *anypb.Any {
	t.Helper()
	a, err := anypb.New(m)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return a
}

// mustStructT builds a structpb.Struct from a map or fails the test.
func mustStructT(t *testing.T, m map[string]any) *structpb.Struct {
	t.Helper()
	s, err := structpb.NewStruct(m)
	if err != nil {
		t.Fatalf("structpb.NewStruct: %v", err)
	}
	return s
}

// --- ConversationDeltaFromEvent: the webapp contract kinds ------------------

func TestConversationDeltaFromEvent(t *testing.T) {
	ts := tsFromMs(producedMs)
	tests := []struct {
		name  string
		event *corev1.Event
		want  *frontendv1.ConversationDelta
	}{
		{
			name: "assistant text block",
			event: &corev1.Event{
				SessionId: "s1", Seq: 7, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.ClaudeStreamMessage{
					Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
						Uuid: "u1",
						Message: &datav1.ApiAssistantMessage{
							Content: []*datav1.ContentBlock{
								{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hello"}}},
							},
						},
					}},
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 7,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"kind": "text", "blockId": "u1:0", "messageId": "u1",
						"text": "hello", "done": true, "ts": ts,
					}),
				},
			},
		},
		{
			name: "assistant thinking block with signature",
			event: &corev1.Event{
				SessionId: "s1", Seq: 3, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.AssistantMessage{
					Uuid: "u2",
					Message: &datav1.ApiAssistantMessage{
						Content: []*datav1.ContentBlock{
							{Block: &datav1.ContentBlock_Thinking{Thinking: &datav1.ThinkingBlock{Thinking: "ponder", Signature: "sig"}}},
						},
					},
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 3,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"kind": "thinking", "blockId": "u2:0", "messageId": "u2",
						"text": "ponder", "done": true, "signature": "sig",
					}),
				},
			},
		},
		{
			name: "assistant tool_use with input",
			event: &corev1.Event{
				SessionId: "s1", Seq: 9, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.AssistantMessage{
					Uuid: "u3",
					Message: &datav1.ApiAssistantMessage{
						Content: []*datav1.ContentBlock{
							{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{
								Id: "tu_1", Name: "Bash",
								Input: mustStructHelper(t, map[string]any{"command": "ls"}),
							}}},
						},
					},
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 9,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"kind": "tool", "toolUseId": "tu_1", "toolName": "Bash",
						"messageId": "u3", "ts": ts, "inputDone": true,
						"input": map[string]any{"command": "ls"},
					}),
				},
			},
		},
		{
			name: "user tool_result string content pairs as a tool item, is_error set",
			event: &corev1.Event{
				SessionId: "s1", Seq: 10, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.UserMessage{
					Uuid: "u4",
					Message: &datav1.ApiUserMessage{
						Content: &datav1.ApiUserMessage_ContentBlocks{ContentBlocks: &datav1.ApiContentBlocks{
							Blocks: []*datav1.ContentBlock{
								{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{
									ToolUseId: "tu_1",
									Content:   &datav1.ToolResultBlock_ContentString{ContentString: "boom"},
									IsError:   true, IsErrorSet: true,
								}}},
							},
						}},
					},
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 10,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"kind": "tool", "toolUseId": "tu_1", "toolName": "",
						"messageId": "u4", "ts": ts, "inputDone": true, "resultTs": ts,
						"result": map[string]any{"isError": true, "content": "boom"},
					}),
				},
			},
		},
		{
			name: "user plain-text prompt becomes a user-turn",
			event: &corev1.Event{
				SessionId: "s1", Seq: 2, ProducedAtMs: producedMs, RequestId: "req-5",
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.UserMessage{
					Uuid:    "u5",
					Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{ContentString: "hi there"}},
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 2,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"kind": "user-turn", "requestId": "req-5", "ts": ts,
						"content": []any{map[string]any{"type": "text", "text": "hi there"}},
					}),
				},
			},
		},
		{
			name: "result message",
			event: &corev1.Event{
				SessionId: "s1", Seq: 12, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.ResultMessage{
					Subtype:    datav1.ResultSubtype_RESULT_SUBTYPE_SUCCESS,
					DurationMs: 1200, NumTurns: 3, TotalCostUsd: 0.5, IsError: false,
					Result: "done",
					Usage:  &datav1.Usage{InputTokens: 10, OutputTokens: 5},
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 12,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"kind": "result", "subtype": "success",
						"durationMs": float64(1200), "sincePrevFinalMs": float64(0),
						"numTurns": float64(3), "totalCostUsd": 0.5, "isError": false,
						"resultText": "done",
						"usage":      map[string]any{"input_tokens": float64(10), "output_tokens": float64(5)},
					}),
				},
			},
		},
		{
			name: "stream compact boundary (no post-tokens)",
			event: &corev1.Event{
				SessionId: "s1", Seq: 13, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.CompactBoundary{
					Trigger: datav1.CompactTrigger_COMPACT_TRIGGER_AUTO, PreTokens: 5000,
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 13,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"kind": "compact-boundary", "trigger": "auto",
						"preTokens": float64(5000), "postTokens": float64(0),
					}),
				},
			},
		},
		{
			name: "non-conversational payload yields nil",
			event: &corev1.Event{
				SessionId: "s1", Seq: 6,
				Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{PromptPreview: "go"}},
			},
			want: nil,
		},
		{
			name: "task-lifecycle event routes nothing (TaskCatalog covers it)",
			event: &corev1.Event{
				SessionId: "s1", Seq: 4,
				Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
					TaskId: "a1", Kind: corev1.TaskKind_TASK_KIND_AGENT, Description: "explore",
				}},
			},
			want: nil,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := ConversationDeltaFromEvent("ws", tc.event)
			// Assert.
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if !proto.Equal(got, tc.want) {
				t.Errorf("mismatch\n got: %v\nwant: %v", got, tc.want)
			}
		})
	}
}

func TestConversationDeltaFromEventToolResultBlocks(t *testing.T) {
	// Arrange: a tool_result whose content is nested blocks (a text block).
	ev := &corev1.Event{
		SessionId: "s1", Seq: 11, ProducedAtMs: producedMs,
		Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.UserMessage{
			Uuid: "u6",
			Message: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentBlocks{ContentBlocks: &datav1.ApiContentBlocks{
					Blocks: []*datav1.ContentBlock{
						{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{
							ToolUseId: "tu_9",
							Content: &datav1.ToolResultBlock_ContentBlocks{ContentBlocks: &datav1.ToolResultBlockList{
								Blocks: []*datav1.ContentBlock{
									{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "nested"}}},
								},
							}},
						}}},
					},
				}},
			},
		})},
	}
	ts := tsFromMs(producedMs)
	want := &frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 11,
		Items: []*structpb.Struct{
			mustStructHelper(t, map[string]any{
				"kind": "tool", "toolUseId": "tu_9", "toolName": "",
				"messageId": "u6", "ts": ts, "inputDone": true, "resultTs": ts,
				"result": map[string]any{
					"isError": false,
					"content": []any{map[string]any{"type": "text", "text": "nested"}},
				},
			}),
		},
	}

	// Act.
	got, err := ConversationDeltaFromEvent("ws", ev)

	// Assert.
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

func TestConversationDeltaFromEventCorruptVendorErrors(t *testing.T) {
	// Arrange: an Any with a type URL absent from the compiled schema set.
	ev := &corev1.Event{
		SessionId: "s1", Seq: 1,
		Payload: &corev1.Event_Vendor{Vendor: &anypb.Any{
			TypeUrl: "type.googleapis.com/agentshim.data.v1.NoSuchMessage",
			Value:   []byte{0x08, 0x01},
		}},
	}

	// Act.
	got, err := ConversationDeltaFromEvent("ws", ev)

	// Assert: hard error, never a silent nil.
	if err == nil {
		t.Fatalf("expected error for unknown vendor type, got nil (delta=%v)", got)
	}
}

// --- transcript (file) plane ------------------------------------------------

func TestConversationDeltaFromEventTranscriptAssistantUsesEnvelopeTs(t *testing.T) {
	// Arrange: a transcript AssistantLine carries its own on-disk timestamp,
	// which takes precedence over the Event's producer stamp.
	ev := &corev1.Event{
		SessionId: "s1", Seq: 20, ProducedAtMs: producedMs,
		Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.TranscriptLine{
			Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
				Envelope: &datav1.LineEnvelope{Uuid: "au1", Timestamp: "2026-01-02T03:04:05Z"},
				Message: &datav1.ApiAssistantMessage{
					Content: []*datav1.ContentBlock{
						{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "disk text"}}},
					},
				},
			}},
		})},
	}
	want := &frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 20,
		Items: []*structpb.Struct{
			mustStructHelper(t, map[string]any{
				"kind": "text", "blockId": "au1:0", "messageId": "au1",
				"text": "disk text", "done": true, "ts": "2026-01-02T03:04:05Z",
			}),
		},
	}

	// Act.
	got, err := ConversationDeltaFromEvent("ws", ev)

	// Assert.
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

func TestConversationDeltaFromEventTranscriptCompactBoundary(t *testing.T) {
	// Arrange: the on-disk compact boundary carries pre- AND post-tokens.
	ev := &corev1.Event{
		SessionId: "s1", Seq: 21,
		Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.TranscriptLine{
			Line: &datav1.TranscriptLine_System{System: &datav1.SystemLine{
				Envelope: &datav1.LineEnvelope{Uuid: "sy1"},
				Subtype: &datav1.SystemLine_CompactBoundary{CompactBoundary: &datav1.CompactBoundaryLine{
					CompactMetadata: &datav1.DiskCompactMetadata{Trigger: "manual", PreTokens: 100, PostTokens: 40},
				}},
			}},
		})},
	}
	want := &frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 21,
		Items: []*structpb.Struct{
			mustStructHelper(t, map[string]any{
				"kind": "compact-boundary", "trigger": "manual",
				"preTokens": float64(100), "postTokens": float64(40),
			}),
		},
	}

	// Act.
	got, err := ConversationDeltaFromEvent("ws", ev)

	// Assert.
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

func TestConversationDeltaFromEventApiErrorRetry(t *testing.T) {
	// Arrange: an api_error line mid-backoff (retry_attempt set) → retry item.
	ev := &corev1.Event{
		SessionId: "s1", Seq: 22,
		Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.TranscriptLine{
			Line: &datav1.TranscriptLine_System{System: &datav1.SystemLine{
				Envelope: &datav1.LineEnvelope{Uuid: "sy2"},
				Subtype: &datav1.SystemLine_ApiError{ApiError: &datav1.ApiErrorLine{
					Error:     &datav1.ApiErrorDetail{Message: "overloaded"},
					RetryInMs: 2000, RetryAttempt: 2, MaxRetries: 5,
				}},
			}},
		})},
	}
	want := &frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 22,
		Items: []*structpb.Struct{
			mustStructHelper(t, map[string]any{
				"kind": "retry", "attempt": float64(2), "reason": "overloaded", "fatal": false,
			}),
		},
	}

	// Act.
	got, err := ConversationDeltaFromEvent("ws", ev)

	// Assert.
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

func TestConversationDeltaFromEventApiErrorTerminal(t *testing.T) {
	// Arrange: an api_error line with no retry (network down) → error item.
	ev := &corev1.Event{
		SessionId: "s1", Seq: 23,
		Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.TranscriptLine{
			Line: &datav1.TranscriptLine_System{System: &datav1.SystemLine{
				Envelope: &datav1.LineEnvelope{Uuid: "sy3"},
				Subtype: &datav1.SystemLine_ApiError{ApiError: &datav1.ApiErrorLine{
					Error: &datav1.ApiErrorDetail{Message: "connection refused", IsNetworkDown: true},
				}},
			}},
		})},
	}
	want := &frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 23,
		Items: []*structpb.Struct{
			mustStructHelper(t, map[string]any{
				"kind": "error", "code": "api_error",
				"message": "connection refused", "recoverable": false,
			}),
		},
	}

	// Act.
	got, err := ConversationDeltaFromEvent("ws", ev)

	// Assert.
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

// --- protojson serialization uses default lowerCamelCase names --------------

func TestMarshalFrameLowerCamelCase(t *testing.T) {
	// Arrange: a ConversationDelta frame carrying a text item, whose interior
	// fields are authored camelCase, wrapped so the frame keys are exercised too.
	frame := ConversationDeltaFrame(&frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 5,
		Items: []*structpb.Struct{
			mustStructHelper(t, map[string]any{
				"kind": "text", "blockId": "u1:0", "messageId": "u1",
				"text": "hi", "done": true, "ts": "2026-01-01T00:00:00Z",
			}),
		},
	})

	// Act.
	b, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshalFrame: %v", err)
	}
	out := string(b)

	// Assert: the oneof arm and message fields are lowerCamelCase (no
	// UseProtoNames snake_case), and the camelCase item interior survives.
	for _, want := range []string{
		`"conversationDelta"`, `"sessionId"`, `"throughSeq"`,
		`"blockId"`, `"messageId"`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("serialized frame missing %s\n%s", want, out)
		}
	}
	for _, notWant := range []string{`"session_id"`, `"through_seq"`, `"block_id"`} {
		if strings.Contains(out, notWant) {
			t.Errorf("serialized frame has snake_case %s (want lowerCamelCase)\n%s", notWant, out)
		}
	}
}

func TestMarshalWorkspaceStateFrameLowerCamelCase(t *testing.T) {
	// Arrange.
	frame := WorkspaceStateFrame(&frontendv1.WorkspaceState{Workspace: "ws", SessionId: "s1"})

	// Act.
	b, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshalFrame: %v", err)
	}

	// Assert: the FrontendFrame oneof arm is "workspaceState".
	if !strings.Contains(string(b), `"workspaceState"`) {
		t.Errorf("serialized frame missing \"workspaceState\"\n%s", b)
	}
}

// --- TypingDeltaFromContentDelta -------------------------------------------

func TestTypingDeltaFromContentDelta(t *testing.T) {
	tests := []struct {
		name string
		cd   *corev1.ContentDelta
		want *frontendv1.TypingDelta
	}{
		{
			name: "text",
			cd:   &corev1.ContentDelta{Uuid: "u1", BlockIndex: 2, Delta: &corev1.ContentDelta_Text{Text: "abc"}},
			want: &frontendv1.TypingDelta{Workspace: "ws", SessionId: "s1", Uuid: "u1", BlockIndex: 2, Kind: "text", Delta: "abc"},
		},
		{
			name: "thinking",
			cd:   &corev1.ContentDelta{Uuid: "u1", BlockIndex: 0, Delta: &corev1.ContentDelta_Thinking{Thinking: "hm"}},
			want: &frontendv1.TypingDelta{Workspace: "ws", SessionId: "s1", Uuid: "u1", Kind: "thinking", Delta: "hm"},
		},
		{
			name: "input_json",
			cd:   &corev1.ContentDelta{Uuid: "u1", BlockIndex: 1, Delta: &corev1.ContentDelta_InputJson{InputJson: "{\"a\":1}"}},
			want: &frontendv1.TypingDelta{Workspace: "ws", SessionId: "s1", Uuid: "u1", BlockIndex: 1, Kind: "input_json", Delta: "{\"a\":1}"},
		},
		{
			name: "signature yields nil (no visible preview)",
			cd:   &corev1.ContentDelta{Uuid: "u1", Delta: &corev1.ContentDelta_Signature{Signature: "sig"}},
			want: nil,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := TypingDeltaFromContentDelta("ws", "s1", tc.cd)
			if !proto.Equal(got, tc.want) {
				t.Errorf("mismatch\n got: %v\nwant: %v", got, tc.want)
			}
		})
	}
}

// --- DegradedNoticeFromState ------------------------------------------------

func TestDegradedNoticeFromState(t *testing.T) {
	// Arrange.
	ds := &corev1.DegradedState{Component: "store-client", Reason: "store unreachable", Recovered: false}

	// Act.
	got := DegradedNoticeFromState(ds, 1234)

	// Assert.
	want := &frontendv1.DegradedNotice{Component: "store-client", Reason: "store unreachable", Recovered: false, AtMs: 1234}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

// --- BuildTaskCatalog -------------------------------------------------------

func TestBuildTaskCatalog(t *testing.T) {
	// Arrange: start two tasks, end one.
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "a1", Kind: corev1.TaskKind_TASK_KIND_AGENT, Description: "d1"}}},
		{ProducedAtMs: 150, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL}}},
		{ProducedAtMs: 200, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "a1", Kind: corev1.TaskKind_TASK_KIND_AGENT, Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE}}},
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events)

	// Assert.
	want := &frontendv1.TaskCatalog{
		Workspace: "ws", SessionId: "s1",
		Tasks: []*frontendv1.TaskEntry{
			{TaskId: "a1", Kind: "agent", Description: "d1", Status: "done", StartedAtMs: 100, EndedAtMs: 200},
			{TaskId: "b1", Kind: "shell", Status: "running", StartedAtMs: 150},
		},
	}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

// --- BuildSessionView -------------------------------------------------------

func TestBuildSessionView(t *testing.T) {
	// Arrange: a session start (model) then a result (cost + usage).
	events := []*corev1.Event{
		{Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{Model: "claude-x"}}},
		{Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.ResultMessage{
			TotalCostUsd: 0.25,
			Usage:        &datav1.Usage{InputTokens: 10, OutputTokens: 5, CacheReadInputTokens: 2, CacheCreationInputTokens: 3},
		})}},
	}

	// Act.
	got := BuildSessionView("ws", "s1", events)

	// Assert.
	want := &frontendv1.SessionView{Workspace: "ws", SessionId: "s1", Model: "claude-x", TotalCostUsd: 0.25, TotalTokens: 20}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

// --- Frame wrappers set the correct oneof arm ------------------------------

func TestFrameWrappers(t *testing.T) {
	if got := SnapshotFrame(&frontendv1.StateSnapshot{}); got.GetSnapshot() == nil {
		t.Error("SnapshotFrame did not set snapshot arm")
	}
	if got := WorkspaceStateFrame(&frontendv1.WorkspaceState{Workspace: "w"}); got.GetWorkspaceState().GetWorkspace() != "w" {
		t.Error("WorkspaceStateFrame did not set workspace_state arm")
	}
	if got := SessionViewFrame(&frontendv1.SessionView{Workspace: "w"}); got.GetSessionView().GetWorkspace() != "w" {
		t.Error("SessionViewFrame did not set session_view arm")
	}
	if got := ConversationDeltaFrame(&frontendv1.ConversationDelta{Workspace: "w"}); got.GetConversationDelta().GetWorkspace() != "w" {
		t.Error("ConversationDeltaFrame did not set conversation_delta arm")
	}
	if got := TypingDeltaFrame(&frontendv1.TypingDelta{Workspace: "w"}); got.GetTypingDelta().GetWorkspace() != "w" {
		t.Error("TypingDeltaFrame did not set typing_delta arm")
	}
	if got := TaskCatalogFrame(&frontendv1.TaskCatalog{Workspace: "w"}); got.GetTaskCatalog().GetWorkspace() != "w" {
		t.Error("TaskCatalogFrame did not set task_catalog arm")
	}
	if got := CommandAckFrame(&frontendv1.CommandAck{RequestId: "r"}); got.GetCommandAck().GetRequestId() != "r" {
		t.Error("CommandAckFrame did not set command_ack arm")
	}
	if got := DegradedNoticeFrame(&frontendv1.DegradedNotice{Component: "c"}); got.GetDegradedNotice().GetComponent() != "c" {
		t.Error("DegradedNoticeFrame did not set degraded_notice arm")
	}
}

// helpers that need *testing.T but are used inside table literals ------------

func mustAnyHelper(t *testing.T, m proto.Message) *anypb.Any { return mustAny(t, m) }
func mustStructHelper(t *testing.T, m map[string]any) *structpb.Struct {
	return mustStructT(t, m)
}
