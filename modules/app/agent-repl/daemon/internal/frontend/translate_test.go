package frontend

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// A fixed producer stamp so item ts_ms fields are deterministic across goldens.
const producedMs int64 = 1700000000000

// discardLogf satisfies the logger a translation requires in tests that assert
// on the translated frames rather than on the records a refusal writes.
func discardLogf(string, ...any) {}

// recordingLogf collects the formatted records a translation emits so a
// refusal's account can be asserted.
func recordingLogf(lines *[]string) dlog.Logf {
	return func(format string, args ...any) {
		*lines = append(*lines, fmt.Sprintf(format, args...))
	}
}

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

// --- ConversationDeltaFromEvent: the typed ConversationItem contract --------

func TestConversationDeltaFromEvent(t *testing.T) {
	// Reusable typed payloads so want/event share the exact same sub-message.
	assistantMsg := &datav1.ApiAssistantMessage{
		Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hello"}}},
		},
	}
	toolUseMsg := &datav1.ApiAssistantMessage{
		Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{
				Id: "tu_1", Name: "Bash",
				Input: mustStructHelper(t, map[string]any{"command": "ls"}),
			}}},
		},
	}
	toolResultMsg := &datav1.ApiUserMessage{
		Content: &datav1.ApiUserMessage_ContentBlocks{ContentBlocks: &datav1.ApiContentBlocks{
			Blocks: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{
					ToolUseId: "tu_1",
					Content:   &datav1.ToolResultBlock_ContentString{ContentString: "boom"},
					IsError:   true, IsErrorSet: true,
				}}},
			},
		}},
	}
	promptMsg := &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{ContentString: "hi there"}}
	resultMsg := &datav1.ResultMessage{
		Subtype:    datav1.ResultSubtype_RESULT_SUBTYPE_SUCCESS,
		DurationMs: 1200, NumTurns: 3, TotalCostUsd: 0.5, Result: "done",
		Usage: &datav1.Usage{InputTokens: 10, OutputTokens: 5},
	}

	tests := []struct {
		name  string
		event *corev1.Event
		want  *frontendv1.ConversationDelta
	}{
		{
			name: "assistant text block passes the ApiAssistantMessage through",
			event: &corev1.Event{
				SessionId: "s1", Seq: 7, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.ClaudeStreamMessage{
					Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
						Uuid: "u1", Message: assistantMsg,
					}},
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 7,
				Items: []*frontendv1.ConversationItem{{
					Uuid: "u1", TsMs: producedMs,
					Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
					Item:   &frontendv1.ConversationItem_AssistantMessage{AssistantMessage: assistantMsg},
				}},
			},
		},
		{
			name: "assistant tool_use rides inside the assistant_message item",
			event: &corev1.Event{
				SessionId: "s1", Seq: 9, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.AssistantMessage{
					Uuid: "u3", Message: toolUseMsg,
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 9,
				Items: []*frontendv1.ConversationItem{{
					Uuid: "u3", TsMs: producedMs,
					Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
					Item:   &frontendv1.ConversationItem_AssistantMessage{AssistantMessage: toolUseMsg},
				}},
			},
		},
		{
			name: "user tool_result rides inside the user_message item",
			event: &corev1.Event{
				SessionId: "s1", Seq: 10, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.UserMessage{
					Uuid: "u4", Message: toolResultMsg,
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 10,
				Items: []*frontendv1.ConversationItem{{
					Uuid: "u4", TsMs: producedMs,
					Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
					Item:   &frontendv1.ConversationItem_UserMessage{UserMessage: toolResultMsg},
				}},
			},
		},
		{
			name: "user plain-text prompt carries the request_id envelope",
			event: &corev1.Event{
				SessionId: "s1", Seq: 2, ProducedAtMs: producedMs, RequestId: "req-5",
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.UserMessage{
					Uuid: "u5", Message: promptMsg,
				})},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 2,
				Items: []*frontendv1.ConversationItem{{
					Uuid: "u5", TsMs: producedMs, RequestId: "req-5",
					Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
					Item:   &frontendv1.ConversationItem_UserMessage{UserMessage: promptMsg},
				}},
			},
		},
		{
			name: "result message passes through into the result arm",
			event: &corev1.Event{
				SessionId: "s1", Seq: 12, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, resultMsg)},
			},
			want: &frontendv1.ConversationDelta{
				Workspace: "ws", SessionId: "s1", ThroughSeq: 12,
				Items: []*frontendv1.ConversationItem{{
					TsMs:   producedMs,
					Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
					Item:   &frontendv1.ConversationItem_Result{Result: resultMsg},
				}},
			},
		},
		{
			name: "empty assistant message has no visual value and is dropped",
			event: &corev1.Event{
				SessionId: "s1", Seq: 14, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.AssistantMessage{
					Uuid: "u7", Message: &datav1.ApiAssistantMessage{},
				})},
			},
			want: nil,
		},
		{
			name: "empty user message has no visual value and is dropped",
			event: &corev1.Event{
				SessionId: "s1", Seq: 15, ProducedAtMs: producedMs,
				Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.UserMessage{
					Uuid: "u8", Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentString{ContentString: ""}},
				})},
			},
			want: nil,
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
			got, _, err := ConversationDeltaFromEvent("ws", tc.event)
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
	got, _, err := ConversationDeltaFromEvent("ws", ev)

	// Assert: hard error, never a silent nil.
	if err == nil {
		t.Fatalf("expected error for unknown vendor type, got nil (delta=%v)", got)
	}
}

// --- transcript (file) plane ------------------------------------------------

func TestConversationDeltaFromEventTranscriptAssistantUsesEnvelopeTs(t *testing.T) {
	// Arrange: a transcript AssistantLine carries its own on-disk timestamp,
	// which takes precedence over the Event's producer stamp (parsed to millis).
	msg := &datav1.ApiAssistantMessage{
		Content: []*datav1.ContentBlock{
			{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "disk text"}}},
		},
	}
	ev := &corev1.Event{
		SessionId: "s1", Seq: 20, ProducedAtMs: producedMs,
		Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.TranscriptLine{
			Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
				Envelope: &datav1.LineEnvelope{Uuid: "au1", Timestamp: "2026-01-02T03:04:05Z"},
				Message:  msg,
			}},
		})},
	}
	// 2026-01-02T03:04:05Z in unix millis.
	const wantTsMs int64 = 1767323045000
	want := &frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 20,
		Items: []*frontendv1.ConversationItem{{
			Uuid: "au1", TsMs: wantTsMs,
			Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
			Item:   &frontendv1.ConversationItem_AssistantMessage{AssistantMessage: msg},
		}},
	}

	// Act.
	got, _, err := ConversationDeltaFromEvent("ws", ev)

	// Assert.
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !proto.Equal(got, want) {
		t.Errorf("mismatch\n got: %v\nwant: %v", got, want)
	}
}

func TestConversationDeltaFromEventTranscriptApiErrorMidBackoffCuratesToNothing(t *testing.T) {
	// Arrange: a MID-BACKOFF api_error line. The retrying window (internal/
	// progress) is what covers it, so the curator emits no conversation item
	// at all for it — RETIRED (step 11): the legacy api_error passthrough
	// this arm used to also emit here is gone, not merely un-asserted.
	line := &datav1.ApiErrorLine{
		Error:     &datav1.ApiErrorDetail{Message: "overloaded"},
		RetryInMs: 2000, RetryAttempt: 2, MaxRetries: 5,
	}
	ev := &corev1.Event{
		SessionId: "s1", Seq: 22, ProducedAtMs: producedMs,
		Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.TranscriptLine{
			Line: &datav1.TranscriptLine_System{System: &datav1.SystemLine{
				Envelope: &datav1.LineEnvelope{Uuid: "sy2"},
				Subtype:  &datav1.SystemLine_ApiError{ApiError: line},
			}},
		})},
	}
	// Act.
	got, _, err := ConversationDeltaFromEvent("ws", ev)

	// Assert.
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got != nil {
		t.Errorf("got %v, want nil (no conversation-bearing items)", got)
	}
}

// --- the terminal API failure card (F4) -------------------------------------
//
// The webapp used to classify this same line by its own rule, and a third rule
// for "fatal" on top, while the daemon used a different one. Both processes
// held the fact; only the daemon holds the cause, so the daemon decides.

// apiErrorEvent wraps an ApiErrorLine as the transcript-plane vendor event the
// curator sees.
func apiErrorEvent(t *testing.T, uuid string, attempt, max int64) *corev1.Event {
	t.Helper()
	return &corev1.Event{
		SessionId: "s1", Seq: 22, ProducedAtMs: producedMs,
		Payload: &corev1.Event_Vendor{Vendor: mustAnyHelper(t, &datav1.TranscriptLine{
			Line: &datav1.TranscriptLine_System{System: &datav1.SystemLine{
				Envelope: &datav1.LineEnvelope{Uuid: uuid},
				Subtype: &datav1.SystemLine_ApiError{ApiError: &datav1.ApiErrorLine{
					Error:        &datav1.ApiErrorDetail{Message: "overloaded", Status: 529},
					RetryAttempt: attempt, MaxRetries: max,
				}},
			}},
		})},
	}
}

// failureOf returns the single system-failure item in a delta, or nil.
func failureOf(cd *frontendv1.ConversationDelta) *frontendv1.SystemFailureItem {
	for _, it := range cd.GetItems() {
		if f := it.GetSystemFailure(); f != nil {
			return f
		}
	}
	return nil
}

func TestQueryTerminationFailurePreservesTypedLifecycleEvidence(t *testing.T) {
	tests := []struct {
		name       string
		terminated *corev1.QueryTerminated
		check      func(*frontendv1.QueryTerminationFailure) bool
	}{
		{name: "unexpected eof", terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor"}, Reason: &corev1.QueryTerminated_UnexpectedEof{UnexpectedEof: &corev1.UnexpectedQueryEof{}}}, check: func(got *frontendv1.QueryTerminationFailure) bool { return got.GetUnexpectedEof() != nil }},
		{name: "iterator failure", terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor"}, Reason: &corev1.QueryTerminated_IteratorFailure{IteratorFailure: &corev1.QueryIteratorFailure{Cause: "child exited 137"}}}, check: func(got *frontendv1.QueryTerminationFailure) bool {
			return got.GetIteratorFailure().GetCause() == "child exited 137"
		}},
		{name: "startup failure", terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor"}, Reason: &corev1.QueryTerminated_StartupFailure{StartupFailure: &corev1.QueryStartupFailure{Cause: "resume rejected"}}}, check: func(got *frontendv1.QueryTerminationFailure) bool {
			return got.GetStartupFailure().GetCause() == "resume rejected"
		}},
		{name: "vendor unavailable", terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionIdentityUnavailable{VendorSessionIdentityUnavailable: &corev1.VendorSessionIdentityUnavailable{}}, Reason: &corev1.QueryTerminated_UnexpectedEof{UnexpectedEof: &corev1.UnexpectedQueryEof{}}}, check: func(got *frontendv1.QueryTerminationFailure) bool {
			return got.GetVendorSessionIdentityUnavailable() != nil
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lifecycle := &corev1.QueryLifecycle{QueryInstanceId: "query", Event: &corev1.QueryLifecycle_Terminated{Terminated: tc.terminated}}
			item, err := SystemFailureItemFromQueryTermination("session", lifecycle, 1234)
			if err != nil {
				t.Fatal(err)
			}
			got := item.GetQueryTermination()
			if got.GetAgentReplSessionId() != "session" || got.GetQueryInstanceId() != "query" || got.GetObservedAtMs() != 1234 || !tc.check(got) {
				t.Fatalf("query termination = %+v", got)
			}
		})
	}
}

func TestQueryTerminationFailureRejectsMissingIdentity(t *testing.T) {
	lifecycle := &corev1.QueryLifecycle{QueryInstanceId: "query", Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{Reason: &corev1.QueryTerminated_UnexpectedEof{UnexpectedEof: &corev1.UnexpectedQueryEof{}}}}}
	item, err := SystemFailureItemFromQueryTermination("session", lifecycle, 1234)
	if err == nil || item != nil || !strings.Contains(err.Error(), "no vendor identity") {
		t.Fatalf("item=%+v err=%v, want missing-vendor-identity rejection", item, err)
	}
}

func TestATerminalApiErrorGetsAFailureCard(t *testing.T) {
	// Arrange: retries exhausted.
	// Act.
	got, _, err := ConversationDeltaFromEvent("ws", apiErrorEvent(t, "sy2", 10, 10))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Assert.
	if failureOf(got) == nil {
		t.Fatal("a terminal API error produced no failure card")
	}
}

func TestAMidBackoffApiErrorGetsNoFailureCard(t *testing.T) {
	// Arrange: the SDK will try again.
	// Act.
	got, _, err := ConversationDeltaFromEvent("ws", apiErrorEvent(t, "sy2", 2, 10))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Assert: reporting a retry as a failure is how a working session came to
	// look broken between attempts.
	if f := failureOf(got); f != nil {
		t.Fatalf("failure card = %v, want none while retries remain", f)
	}
}

func TestTheFailureCardIsTheOnlyItem(t *testing.T) {
	// Arrange: RETIRED (step 11) — the legacy api_error passthrough that used
	// to ride beside the card is gone, so a terminal failure curates to
	// exactly the card and nothing else.
	// Act.
	got, _, err := ConversationDeltaFromEvent("ws", apiErrorEvent(t, "sy2", 10, 10))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Assert.
	if len(got.GetItems()) != 1 || got.GetItems()[0].GetSystemFailure() == nil {
		t.Fatalf("items = %v, want exactly one system_failure item", got.GetItems())
	}
}

func TestTheFailureCardUuidIsDerivedFromTheLine(t *testing.T) {
	// Arrange: a derived uuid cannot collide with the legacy item's and stays
	// stable across a resync, which a freshly minted one would not.
	// Act.
	got, _, err := ConversationDeltaFromEvent("ws", apiErrorEvent(t, "sy2", 10, 10))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Assert.
	if want := FailureUUID("sy2"); got.GetItems()[0].GetUuid() != want {
		t.Fatalf("card uuid = %q, want %q", got.GetItems()[0].GetUuid(), want)
	}
}

func TestTheFailureCardClassifiesTheStatus(t *testing.T) {
	// Arrange: a 529 is overload, which the raw line never said in words.
	// Act.
	got, _, err := ConversationDeltaFromEvent("ws", apiErrorEvent(t, "sy2", 10, 10))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Assert.
	if f := failureOf(got); f.GetErrorType() != string(errclass.TypeAPIOverloaded) {
		t.Fatalf("error_type = %q, want %q", f.GetErrorType(), errclass.TypeAPIOverloaded)
	}
}

// --- protojson serialization uses default lowerCamelCase names --------------

func TestMarshalConversationDeltaLowerCamelCase(t *testing.T) {
	// Arrange: a ConversationDelta carrying an assistant_message whose content
	// holds a tool_use block, so both the item arm key and the nested block
	// oneof arm key are exercised.
	frame := ConversationDeltaFrame(&frontendv1.ConversationDelta{
		Workspace: "ws", SessionId: "s1", ThroughSeq: 5,
		Items: []*frontendv1.ConversationItem{{
			Uuid: "u1", TsMs: producedMs,
			Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
			Item: &frontendv1.ConversationItem_AssistantMessage{AssistantMessage: &datav1.ApiAssistantMessage{
				Content: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: "tu_1", Name: "Bash"}}},
				},
			}},
		}},
	})

	// Act.
	b, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshalFrame: %v", err)
	}
	out := string(b)

	// Assert: the frame arm, item arm, and nested block arm are lowerCamelCase.
	for _, want := range []string{
		`"conversationDelta"`, `"sessionId"`, `"throughSeq"`,
		`"assistantMessage"`, `"toolUse"`, `"tsMs"`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("serialized frame missing %s\n%s", want, out)
		}
	}
	for _, notWant := range []string{`"session_id"`, `"through_seq"`, `"assistant_message"`, `"tool_use"`} {
		if strings.Contains(out, notWant) {
			t.Errorf("serialized frame has snake_case %s (want lowerCamelCase)\n%s", notWant, out)
		}
	}
}

func TestMarshalTypingDeltaLowerCamelCase(t *testing.T) {
	// Arrange: a TypingDelta embedding a ContentDelta, so the embedded
	// block_index and tool_use_id serialize as delta.blockIndex and
	// delta.toolUseId.
	frame := TypingDeltaFrame(&frontendv1.TypingDelta{
		Workspace: "ws", SessionId: "s1",
		Delta: &corev1.ContentDelta{Uuid: "u1", BlockIndex: 2, ToolUseId: proto.String("toolu_01"), Delta: &corev1.ContentDelta_InputJson{InputJson: `{"command":"pwd"}`}},
	})

	// Act.
	b, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshalFrame: %v", err)
	}
	out := string(b)

	// Assert: the frame arm and the embedded delta identity are lowerCamelCase.
	for _, want := range []string{`"typingDelta"`, `"delta"`, `"blockIndex"`, `"toolUseId"`} {
		if !strings.Contains(out, want) {
			t.Errorf("serialized frame missing %s\n%s", want, out)
		}
	}
	for _, notWant := range []string{`"block_index"`, `"tool_use_id"`} {
		if strings.Contains(out, notWant) {
			t.Errorf("serialized frame has snake_case %s\n%s", notWant, out)
		}
	}
}

func TestContentDeltaToolUseIDIsAdditiveAndPresenceAware(t *testing.T) {
	// Arrange: this byte sequence represents the prior ContentDelta layout
	// (uuid = 1 and text = 3), before field 8 existed on the wire.
	legacy := []byte{0x0a, 0x02, 'u', '1', 0x1a, 0x03, 'a', 'b', 'c'}
	var decoded corev1.ContentDelta

	// Act: the current reader must continue accepting a delta emitted by the
	// prior schema without manufacturing tool identity.
	if err := proto.Unmarshal(legacy, &decoded); err != nil {
		t.Fatalf("unmarshal legacy ContentDelta: %v", err)
	}
	field := decoded.ProtoReflect().Descriptor().Fields().ByName("tool_use_id")

	// Assert: the new identity occupies the next additive field number and an
	// older delta leaves its optional presence unset.
	if field == nil {
		t.Fatal("ContentDelta descriptor missing tool_use_id")
	}
	if got := field.Number(); got != 8 {
		t.Errorf("tool_use_id field number = %d, want 8", got)
	}
	if decoded.ProtoReflect().Has(field) {
		t.Error("legacy ContentDelta unexpectedly has tool_use_id presence")
	}
	if got := decoded.GetToolUseId(); got != "" {
		t.Errorf("legacy ContentDelta tool_use_id = %q, want empty", got)
	}
	if got := decoded.GetUuid(); got != "u1" {
		t.Errorf("legacy ContentDelta uuid = %q, want u1", got)
	}
	if got := decoded.GetText(); got != "abc" {
		t.Errorf("legacy ContentDelta text = %q, want abc", got)
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

// THE MERGED-AT FACT MUST REACH THE WIRE. It is an int64, which protojson
// renders as a STRING, and a frontend ordering its recently-merged section
// reads it off this frame — so the field being carried is the whole point of
// persisting it.
func TestMarshalWorkspaceStateFrameCarriesMergedAt(t *testing.T) {
	// Arrange.
	frame := WorkspaceStateFrame(&frontendv1.WorkspaceState{
		Workspace:  "ws",
		State:      frontendv1.RenderState_RENDER_STATE_MERGED,
		MergedAtMs: 1750000000123,
	})

	// Act.
	b, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshalFrame: %v", err)
	}

	// Assert.
	if !strings.Contains(string(b), `"mergedAtMs":"1750000000123"`) {
		t.Errorf("serialized frame missing mergedAtMs\n%s", b)
	}
}

// A WORKSPACE THAT NEVER MERGED CARRIES NO INSTANT, and protojson's zero-value
// omission is what says so on the wire: absent reads as never merged, which is
// exactly the field's documented meaning.
func TestMarshalWorkspaceStateFrameOmitsAnAbsentMergedAt(t *testing.T) {
	// Arrange.
	frame := WorkspaceStateFrame(&frontendv1.WorkspaceState{
		Workspace: "ws",
		State:     frontendv1.RenderState_RENDER_STATE_IDLE,
	})

	// Act.
	b, err := marshalFrame(frame)
	if err != nil {
		t.Fatalf("marshalFrame: %v", err)
	}

	// Assert.
	if strings.Contains(string(b), "mergedAtMs") {
		t.Errorf("serialized frame carries mergedAtMs for a workspace that never merged\n%s", b)
	}
}

// A SCOPED SNAPSHOT KEEPS THE FACT. The scope pass rebuilds the snapshot's
// slices, so a merged workspace that survives the filter must arrive with its
// instant intact rather than with a rebuilt, emptied view.
func TestFilterSnapshotKeepsMergedAt(t *testing.T) {
	// Arrange.
	snap := &frontendv1.StateSnapshot{Workspaces: []*frontendv1.WorkspaceState{{
		Workspace:  "/ws/alpha",
		SessionId:  "s1",
		State:      frontendv1.RenderState_RENDER_STATE_MERGED,
		MergedAtMs: 1750000000123,
	}}}

	// Act.
	got := filterSnapshot(snap, Scope{SessionID: "s1", Workspace: "/ws/alpha"})

	// Assert.
	if len(got.GetWorkspaces()) != 1 {
		t.Fatalf("filtered workspaces = %d, want the merged workspace retained", len(got.GetWorkspaces()))
	}
	if got.GetWorkspaces()[0].GetMergedAtMs() != 1750000000123 {
		t.Fatalf("filtered merged_at_ms = %d, want 1750000000123", got.GetWorkspaces()[0].GetMergedAtMs())
	}
}

// --- TypingDeltaFromContentDelta embeds the ContentDelta as-is --------------

func TestTypingDeltaFromContentDelta(t *testing.T) {
	tests := []struct {
		name string
		cd   *corev1.ContentDelta
		want *frontendv1.TypingDelta
	}{
		{
			name: "text delta embedded unchanged",
			cd:   &corev1.ContentDelta{Uuid: "u1", BlockIndex: 2, Delta: &corev1.ContentDelta_Text{Text: "abc"}},
			want: &frontendv1.TypingDelta{Workspace: "ws", SessionId: "s1", Delta: &corev1.ContentDelta{Uuid: "u1", BlockIndex: 2, Delta: &corev1.ContentDelta_Text{Text: "abc"}}},
		},
		{
			name: "signature delta forwarded as-is (no daemon curation of arms)",
			cd:   &corev1.ContentDelta{Uuid: "u1", Delta: &corev1.ContentDelta_Signature{Signature: "sig"}},
			want: &frontendv1.TypingDelta{Workspace: "ws", SessionId: "s1", Delta: &corev1.ContentDelta{Uuid: "u1", Delta: &corev1.ContentDelta_Signature{Signature: "sig"}}},
		},
		{
			name: "nil content delta yields nil",
			cd:   nil,
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

// --- BuildTaskCatalog -------------------------------------------------------

func TestBuildTaskCatalog(t *testing.T) {
	// Arrange: start two tasks, end one.
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "a1", Kind: corev1.TaskKind_TASK_KIND_AGENT, Description: "d1"}}},
		{ProducedAtMs: 150, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL}}},
		{ProducedAtMs: 200, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "a1", Kind: corev1.TaskKind_TASK_KIND_AGENT, Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE}}},
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

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

func TestBuildTaskCatalogFoldsADuplicateTaskEndedOntoOneEntry(t *testing.T) {
	// Arrange — one task ended twice, which a shell spool's EXIT= marker makes
	// real: the marker can report a completion another plane already reported.
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL}}},
		{ProducedAtMs: 200, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL, Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE}}},
		{ProducedAtMs: 250, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL, Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE}}},
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert — one entry, not two.
	if len(got.GetTasks()) != 1 {
		t.Fatalf("catalog holds %d entries for one task, want 1", len(got.GetTasks()))
	}
}

func TestBuildTaskCatalogKeepsTheTaskEndedOnADuplicate(t *testing.T) {
	// Arrange — a duplicate end must not reopen a settled task.
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL}}},
		{ProducedAtMs: 200, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL, Status: corev1.TerminalStatus_TERMINAL_STATUS_ERROR}}},
		{ProducedAtMs: 250, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL, Status: corev1.TerminalStatus_TERMINAL_STATUS_ERROR}}},
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert.
	if s := got.GetTasks()[0].GetStatus(); s != "error" {
		t.Fatalf("status after a duplicate end = %q, want error", s)
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
	if got := SessionInitViewFrame(&frontendv1.SessionInitView{Workspace: "w"}); got.GetSessionInit().GetWorkspace() != "w" {
		t.Error("SessionInitViewFrame did not set session_init arm")
	}
	if got := WorkspaceAvailableFrame(&frontendv1.WorkspaceAvailable{JobId: "j"}); got.GetWorkspaceAvailable().GetJobId() != "j" {
		t.Error("WorkspaceAvailableFrame did not set workspace_available arm")
	}
	if got := HostActionFrame(&frontendv1.HostAction{ActionId: "a"}); got.GetHostAction().GetActionId() != "a" {
		t.Error("HostActionFrame did not set host_action arm")
	}
}

// helpers that need *testing.T but are used inside table literals ------------

// --- BuildTaskCatalog: BackgroundTasksChanged reconciliation ----------------

// backgroundTasksEvent wraps a live-set snapshot as a vendor core Event.
func backgroundTasksEvent(t *testing.T, atMs int64, refs ...*datav1.BackgroundTaskRef) *corev1.Event {
	t.Helper()
	return &corev1.Event{
		ProducedAtMs: atMs,
		Payload: &corev1.Event_Vendor{Vendor: mustAny(t, &datav1.ClaudeStreamMessage{
			Msg: &datav1.ClaudeStreamMessage_BackgroundTasksChanged{
				BackgroundTasksChanged: &datav1.BackgroundTasksChanged{Tasks: refs},
			},
		})},
	}
}

func TestBuildTaskCatalogSweepsAGhostAbsentFromTheLiveSet(t *testing.T) {
	// Arrange — a task whose end never arrived, and a session that says it is
	// not running. Without the authority this stays "running" until a LOST
	// staleness sweep gets to it.
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "ghost", Kind: corev1.TaskKind_TASK_KIND_AGENT}}},
		backgroundTasksEvent(t, 300),
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert.
	if len(got.GetTasks()) != 1 || got.GetTasks()[0].GetStatus() != "lost" {
		t.Fatalf("catalog = %v, want the ghost swept to lost", got.GetTasks())
	}
}

func TestBuildTaskCatalogStampsTheSweepAtTheSnapshotTime(t *testing.T) {
	// Arrange
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "ghost", Kind: corev1.TaskKind_TASK_KIND_AGENT}}},
		backgroundTasksEvent(t, 300),
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert.
	if got.GetTasks()[0].GetEndedAtMs() != 300 {
		t.Fatalf("swept ended_at_ms = %d, want 300 (the snapshot's stamp)", got.GetTasks()[0].GetEndedAtMs())
	}
}

func TestBuildTaskCatalogLeavesASettledTaskAlone(t *testing.T) {
	// Arrange — a task that genuinely finished must keep its real status, not
	// be re-reported as lost because it is absent from the live set.
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "a1", Kind: corev1.TaskKind_TASK_KIND_AGENT}}},
		{ProducedAtMs: 200, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "a1", Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE}}},
		backgroundTasksEvent(t, 300),
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert.
	if got.GetTasks()[0].GetStatus() != "done" {
		t.Fatalf("settled task status = %q, want done", got.GetTasks()[0].GetStatus())
	}
}

func TestBuildTaskCatalogAdoptsATaskItNeverSawStart(t *testing.T) {
	// Arrange — the live set names a task with no TaskStarted in the window.
	events := []*corev1.Event{
		backgroundTasksEvent(t, 300, &datav1.BackgroundTaskRef{TaskId: "unseen", TaskType: "local_bash", Description: "npm test"}),
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert.
	want := &frontendv1.TaskEntry{TaskId: "unseen", Kind: "shell", Description: "npm test", Status: "running", StartedAtMs: 300}
	if len(got.GetTasks()) != 1 || !proto.Equal(got.GetTasks()[0], want) {
		t.Fatalf("catalog = %v, want one adopted running entry %v", got.GetTasks(), want)
	}
}

func TestBuildTaskCatalogKeepsALiveTaskRunning(t *testing.T) {
	// Arrange — a task present in the live set is untouched by the sweep.
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "a1", Kind: corev1.TaskKind_TASK_KIND_AGENT, Description: "d"}}},
		backgroundTasksEvent(t, 300, &datav1.BackgroundTaskRef{TaskId: "a1"}),
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert — its own start time and description survive the reconciliation.
	want := &frontendv1.TaskEntry{TaskId: "a1", Kind: "agent", Description: "d", Status: "running", StartedAtMs: 100}
	if !proto.Equal(got.GetTasks()[0], want) {
		t.Fatalf("live entry = %v, want %v", got.GetTasks()[0], want)
	}
}

func TestBuildTaskCatalogLetsALaterTaskEndedCloseAnAdoptedTask(t *testing.T) {
	// Arrange — the live set is authoritative AT ITS POINT in the stream; a
	// TaskEnded that folds after it still closes the task.
	events := []*corev1.Event{
		backgroundTasksEvent(t, 300, &datav1.BackgroundTaskRef{TaskId: "a1", TaskType: "local_bash"}),
		{ProducedAtMs: 400, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "a1", Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE}}},
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert.
	if got.GetTasks()[0].GetStatus() != "done" {
		t.Fatalf("status = %q, want done", got.GetTasks()[0].GetStatus())
	}
}

// --- BuildTaskCatalog: the shim task_type → frontend kind boundary ----------

func TestBuildTaskCatalogTranslatesTheShimTaskTypeOnTheRefPath(t *testing.T) {
	tests := []struct {
		name     string
		taskType string
		want     string
	}{
		{name: "local_agent is the frontend's agent kind", taskType: "local_agent", want: "agent"},
		{name: "local_bash is the frontend's shell kind", taskType: "local_bash", want: "shell"},
		{name: "local_workflow is the frontend's workflow kind", taskType: "local_workflow", want: "workflow"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a task referenced by the live set with no TaskStarted
			// folded, which is what a restart or a replay leaves behind.
			events := []*corev1.Event{
				backgroundTasksEvent(t, 300, &datav1.BackgroundTaskRef{TaskId: "t1", TaskType: tc.taskType}),
			}

			// Act.
			got := BuildTaskCatalog("ws", "s1", events, discardLogf)

			// Assert.
			if len(got.GetTasks()) != 1 || got.GetTasks()[0].GetKind() != tc.want {
				t.Fatalf("catalog = %v, want one entry of kind %q", got.GetTasks(), tc.want)
			}
		})
	}
}

func TestBuildTaskCatalogRefusesAnUnrecognizedShimTaskType(t *testing.T) {
	// Arrange — a task_type outside the shim vocabulary maps to no frontend
	// kind, and the frontend contract has no value for "unknown".
	events := []*corev1.Event{
		backgroundTasksEvent(t, 300, &datav1.BackgroundTaskRef{TaskId: "t1", TaskType: "local_teleport"}),
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert — the entry never reaches the contract, in any form.
	if len(got.GetTasks()) != 0 {
		t.Fatalf("catalog = %v, want the out-of-vocabulary entry omitted", got.GetTasks())
	}
}

func TestBuildTaskCatalogRecordsTheUnrecognizedShimTaskType(t *testing.T) {
	// Arrange — the refusal must leave a trace naming the task and the string.
	var lines []string
	events := []*corev1.Event{
		backgroundTasksEvent(t, 300, &datav1.BackgroundTaskRef{TaskId: "t1", TaskType: "local_teleport"}),
	}

	// Act.
	BuildTaskCatalog("ws", "s1", events, recordingLogf(&lines))

	// Assert.
	var found bool
	for _, line := range lines {
		if strings.Contains(line, `"t1"`) && strings.Contains(line, `"local_teleport"`) {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %q, want one naming task t1 and the offending task_type", lines)
	}
}

func TestBuildTaskCatalogNeverPassesARawShimTaskTypeThroughAsAKind(t *testing.T) {
	// Arrange — the exact live poisoning: a retained ref-only entry whose raw
	// task_type used to be copied into the frontend kind, which the webapp's
	// validating decoder then rejected for every snapshot of the session.
	events := []*corev1.Event{
		backgroundTasksEvent(t, 300, &datav1.BackgroundTaskRef{TaskId: "t1", TaskType: "local_agent"}),
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert.
	if got.GetTasks()[0].GetKind() == "local_agent" {
		t.Fatalf("kind = %q, want the frontend vocabulary rather than the shim's", got.GetTasks()[0].GetKind())
	}
}

func TestBuildTaskCatalogRefusesAnUnspecifiedTaskKind(t *testing.T) {
	// Arrange — a TaskStarted the shim could not type resolves to
	// "unspecified", which is no more decodable to a frontend than a raw
	// task_type is.
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "t1"}}},
	}

	// Act.
	got := BuildTaskCatalog("ws", "s1", events, discardLogf)

	// Assert.
	if len(got.GetTasks()) != 0 {
		t.Fatalf("catalog = %v, want the unspecified-kind entry omitted", got.GetTasks())
	}
}

func TestBuildTaskCatalogRecordsARefusedEntryByTaskID(t *testing.T) {
	// Arrange.
	var lines []string
	events := []*corev1.Event{
		{ProducedAtMs: 100, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "t1"}}},
	}

	// Act.
	BuildTaskCatalog("ws", "s1", events, recordingLogf(&lines))

	// Assert.
	var found bool
	for _, line := range lines {
		if strings.Contains(line, "REFUSING task \"t1\"") {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %q, want one naming the refused task", lines)
	}
}

func TestBuildTaskCatalogRequiresALogger(t *testing.T) {
	// Arrange — a refusal that cannot be recorded is a silent drop, so the
	// translation refuses to run without somewhere to record it.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatalf("BuildTaskCatalog with a nil logger returned normally, want a panic")
		}
	}()

	// Act.
	BuildTaskCatalog("ws", "s1", nil, nil)
}

func TestBackgroundTasksFromVendorIgnoresAnotherStreamArm(t *testing.T) {
	// Arrange — every vendor event shares one Any type URL; the inner oneof is
	// the discriminator.
	a := mustAny(t, &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Uuid: "u"}},
	})

	// Act.
	got := BackgroundTasksFromVendor(a)

	// Assert.
	if got != nil {
		t.Fatalf("BackgroundTasksFromVendor on an assistant arm = %v, want nil", got)
	}
}

func mustAnyHelper(t *testing.T, m proto.Message) *anypb.Any { return mustAny(t, m) }
func mustStructHelper(t *testing.T, m map[string]any) *structpb.Struct {
	return mustStructT(t, m)
}
