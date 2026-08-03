// Token utilization acceptance coverage.  The fake SDK exercises the real
// shim, store, daemon, and frontend route without allowing a vendor call.
package e2e

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// TestE2ETokenUtilizationPairsResponseUsageWithTiming verifies the end-to-end
// ownership boundary for one main-agent API response.  The fake emits one
// message_start and one completed assistant message with the same API message
// id, so a record without timing proves the shim did not correlate those two
// SDK observations.  The record must survive a frontend replay, which makes
// the assertion cover durable conversation state rather than only a live push.
func TestE2ETokenUtilizationPairsResponseUsageWithTiming(t *testing.T) {
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, conn, _, _ := liveSession(t, h, cwd)

	writeCmd(t, conn, `{"requestId":"e2e-token-main","submitPrompt":{"text":"measure response timing"}}`)
	item, _ := awaitItem(t, conn, cwd, "the completed assistant response carrying token utilization", func(item *frontendv1.ConversationItem) bool {
		return item.GetAssistantMessage().GetId() == "msg_fake_1"
	})

	usage := requireSingleTokenUtilization(t, item, "live assistant response")
	if got, want := usage.GetAgentReplSessionId(), id; got != want {
		t.Errorf("token utilization agent_repl_session_id = %q, want session %q", got, want)
	}
	if usage.GetClaudeSessionId() == "" {
		t.Error("token utilization claude_session_id is empty: response ownership must survive the shim-to-daemon boundary")
	}
	if got, want := usage.GetApiMessageId(), "msg_fake_1"; got != want {
		t.Errorf("token utilization api_message_id = %q, want message_start id %q", got, want)
	}
	if usage.GetMainAgent() == nil || usage.GetSubagent() != nil {
		t.Errorf("token utilization actor = %T, want main_agent only", usage.GetActor())
	}
	if got, want := usage.GetUsage().GetInputTokens(), int64(7); got != want {
		t.Errorf("token utilization input_tokens = %d, want fake SDK usage %d", got, want)
	}
	if got, want := usage.GetUsage().GetOutputTokens(), int64(11); got != want {
		t.Errorf("token utilization output_tokens = %d, want fake SDK usage %d", got, want)
	}
	if usage.GetResponseTiming() == nil || usage.GetResponseTiming().TimeToFirstTokenMs == nil || usage.GetResponseTiming().OutputGenerationDurationMs == nil {
		t.Fatal("token utilization omitted TTFT or output-generation duration: generation throughput must use paired response timing, never whole-turn duration")
	}

	replay := replayItems(t, h.dial(t, id), cwd, "e2e-token-replay")
	var replayed *frontendv1.ConversationItem
	for _, candidate := range replay {
		if candidate.GetAssistantMessage().GetId() == "msg_fake_1" {
			replayed = candidate
			break
		}
	}
	if replayed == nil {
		t.Fatal("replay omitted the assistant response carrying token utilization")
	}
	replayedUsage := requireSingleTokenUtilization(t, replayed, "replayed assistant response")
	if got, want := replayedUsage.GetApiMessageId(), usage.GetApiMessageId(); got != want {
		t.Errorf("replayed token utilization api_message_id = %q, want durable identity %q", got, want)
	}
	if replayedUsage.GetResponseTiming() == nil || replayedUsage.GetResponseTiming().OutputGenerationDurationMs == nil {
		t.Error("replayed token utilization lost output-generation duration")
	}
}

func requireSingleTokenUtilization(t *testing.T, item *frontendv1.ConversationItem, source string) *frontendv1.TokenUtilization {
	t.Helper()
	records := item.GetTokenUtilization()
	if len(records) != 1 {
		t.Fatalf("%s token utilization records = %d, want exactly one API-response record", source, len(records))
	}
	return records[0]
}

// TestE2EHistoricalUsageIsExplicitlyUntimedAndDeduplicated verifies that a
// transcript replay without a corresponding message_start remains intentionally
// untimed. The duplicate store write must yield exactly one durable response.
func TestE2EHistoricalUsageIsExplicitlyUntimedAndDeduplicated(t *testing.T) {
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, conn, vendorID, store := liveSession(t, h, cwd)

	event := sidecarAssistantUsageEvent(t, vendorID, "e2e-historical-usage", "msg-historical", "agent-nested")
	store.write(event)
	store.write(event)
	item, _ := awaitItem(t, conn, cwd, "the historical assistant response carrying token utilization", func(item *frontendv1.ConversationItem) bool {
		return item.GetAssistantMessage().GetId() == "msg-historical"
	})
	usage := requireSingleTokenUtilization(t, item, "historical assistant response")
	if usage.GetResponseTiming() != nil {
		t.Fatal("historical response without message_start has response timing: whole-turn duration must never be used as a generation-time fallback")
	}
	if got, want := usage.GetUsage().GetCacheReadInputTokens(), int64(800); got != want {
		t.Errorf("historical cache_read_input_tokens = %d, want %d", got, want)
	}
	if got, want := usage.GetUsage().GetCacheCreation().GetEphemeral_1HInputTokens(), int64(50); got != want {
		t.Errorf("historical cache_creation.ephemeral_1h_input_tokens = %d, want %d", got, want)
	}
	if got, want := usage.GetUsage().GetServerToolUse().GetWebFetchRequests(), int64(3); got != want {
		t.Errorf("historical server_tool_use.web_fetch_requests = %d, want %d", got, want)
	}
	if got, want := usage.GetUsage().GetServiceTier(), "priority"; got != want {
		t.Errorf("historical service_tier = %q, want %q", got, want)
	}
	if got, want := usage.GetUsage().GetSpeed(), "fast"; got != want {
		t.Errorf("historical speed = %q, want %q", got, want)
	}
	if usage.GetSubagent() == nil || usage.GetSubagent().GetAgentId() != "agent-nested" {
		t.Errorf("historical actor = %T id=%q, want attributed nested subagent", usage.GetActor(), usage.GetSubagent().GetAgentId())
	}

	replayed := replayItems(t, h.dial(t, id), cwd, "e2e-historical-usage-replay")
	seen := 0
	for _, candidate := range replayed {
		if candidate.GetAssistantMessage().GetId() == "msg-historical" {
			seen++
			records := candidate.GetTokenUtilization()
			if len(records) != 1 || records[0].GetResponseTiming() != nil {
				t.Errorf("replayed historical response records = %d, want one explicitly untimed record", len(records))
			}
		}
	}
	if seen != 1 {
		t.Errorf("duplicate store observations produced %d replayed historical responses, want exactly one", seen)
	}
}

// sidecarAssistantUsageEvent represents a completed file-plane transcript
// record. It has no stream-plane message_start, so it is the historical case
// that must retain usage without inventing a generation duration.
func sidecarAssistantUsageEvent(t *testing.T, vendorSessionID, lineUUID, messageID, agentID string) *corev1.Event {
	t.Helper()
	cacheCreation, err := structpb.NewStruct(map[string]any{"ephemeral_5m_input_tokens": 25, "ephemeral_1h_input_tokens": 50})
	if err != nil {
		t.Fatalf("build cache_creation fixture: %v", err)
	}
	serverToolUse, err := structpb.NewStruct(map[string]any{"web_search_requests": 2, "web_fetch_requests": 3})
	if err != nil {
		t.Fatalf("build server_tool_use fixture: %v", err)
	}
	line := &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{Uuid: lineUUID, SessionId: vendorSessionID, AgentId: agentID},
		Message: &datav1.ApiAssistantMessage{Id: messageID, Model: "claude-opus-test", Usage: &datav1.ApiUsage{
			InputTokens: 100, OutputTokens: 200, CacheReadInputTokens: 800, CacheCreationInputTokens: 75,
			CacheCreation: cacheCreation, ServerToolUse: serverToolUse, ServiceTier: "priority", Speed: "fast", InferenceGeo: "us-east-1",
		}},
	}}}
	payload, err := anypb.New(line)
	if err != nil {
		t.Fatalf("encode historical assistant transcript line: %v", err)
	}
	return &corev1.Event{SessionId: vendorSessionID, Plane: corev1.Plane_PLANE_FILE, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, ProducedAtMs: time.Now().UnixMilli(), Payload: &corev1.Event_Vendor{Vendor: payload}}
}
