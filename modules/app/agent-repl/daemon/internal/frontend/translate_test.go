package frontend

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

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

// --- ConversationDeltaFromEvent: assistant/user content ---------------------

func TestConversationDeltaFromEvent(t *testing.T) {
	tests := []struct {
		name  string
		event *corev1.Event
		want  *frontendv1.ConversationDelta
	}{
		{
			name: "assistant text block",
			event: &corev1.Event{
				SessionId: "s1",
				Seq:       7,
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
						"type": "text", "role": "assistant", "uuid": "u1",
						"block_index": float64(0), "text": "hello",
					}),
				},
			},
		},
		{
			name: "assistant thinking block with signature",
			event: &corev1.Event{
				SessionId: "s1", Seq: 3,
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
						"type": "thinking", "role": "assistant", "uuid": "u2",
						"block_index": float64(0), "thinking": "ponder", "signature": "sig",
					}),
				},
			},
		},
		{
			name: "assistant tool_use with input",
			event: &corev1.Event{
				SessionId: "s1", Seq: 9,
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
						"type": "tool_use", "role": "assistant", "uuid": "u3",
						"block_index": float64(0), "id": "tu_1", "name": "Bash",
						"input": map[string]any{"command": "ls"},
					}),
				},
			},
		},
		{
			name: "user tool_result string content, is_error set",
			event: &corev1.Event{
				SessionId: "s1", Seq: 10,
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
						"type": "tool_result", "role": "user", "uuid": "u4",
						"block_index": float64(0), "tool_use_id": "tu_1",
						"is_error": true, "content": "boom",
					}),
				},
			},
		},
		{
			name: "user plain text content",
			event: &corev1.Event{
				SessionId: "s1", Seq: 2,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.UserMessage{
					Uuid:    "u5",
					Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{ContentString: "hi there"}},
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 2,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"type": "text", "role": "user", "uuid": "u5",
						"block_index": float64(0), "text": "hi there",
					}),
				},
			},
		},
		{
			name: "task started chip",
			event: &corev1.Event{
				SessionId: "s1", Seq: 4,
				Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
					TaskId: "a1", Kind: corev1.TaskKind_TASK_KIND_AGENT,
					Description: "explore", OutputPath: "/tmp/a1.jsonl",
				}},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 4,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"type": "task", "task_id": "a1", "kind": "agent",
						"status": "running", "description": "explore", "output_path": "/tmp/a1.jsonl",
					}),
				},
			},
		},
		{
			name: "task ended lost chip carries inference",
			event: &corev1.Event{
				SessionId: "s1", Seq: 5,
				Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{
					TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL,
					Status: corev1.TerminalStatus_TERMINAL_STATUS_LOST, Inference: "vanished-file",
				}},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 5,
				Items: []*structpb.Struct{
					mustStructHelper(t, map[string]any{
						"type": "task", "task_id": "b1", "kind": "shell",
						"status": "lost", "inference": "vanished-file",
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
		SessionId: "s1", Seq: 11,
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
	want := &frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 11,
		Items: []*structpb.Struct{
			mustStructHelper(t, map[string]any{
				"type": "tool_result", "role": "user", "uuid": "u6",
				"block_index": float64(0), "tool_use_id": "tu_9",
				"content_blocks": []any{
					map[string]any{"type": "text", "role": "user", "uuid": "u6", "block_index": float64(0), "text": "nested"},
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
