// Token utilization acceptance coverage.  The fake SDK exercises the real
// shim, store, daemon, and frontend route without allowing a vendor call.
package e2e

import (
	"fmt"
	"math"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

func sessionViewFromSnapshot(t *testing.T, h *e2eHarness, sessionID string) *frontendv1.SessionView {
	t.Helper()
	conn := h.dialFrontend(t)
	defer conn.Close()
	snapshot := readFrame(t, conn).GetSnapshot()
	if snapshot == nil {
		t.Fatal("first frontend frame is not a StateSnapshot")
	}
	for _, view := range snapshot.GetSessions() {
		if view.GetSessionId() == sessionID {
			return view
		}
	}
	t.Fatalf("StateSnapshot omitted session %q", sessionID)
	return nil
}

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

// TestE2ESessionViewAggregatesTimedAndUntimedActors verifies that SessionView
// publishes additive all/main/subagent/model totals. Only output tokens paired
// with a measured generation duration may enter the throughput numerator.
func TestE2ESessionViewAggregatesTimedAndUntimedActors(t *testing.T) {
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, conn, vendorID, store := liveSession(t, h, cwd)
	writeCmd(t, conn, `{"requestId":"e2e-token-aggregate","submitPrompt":{"text":"timed main response"}}`)
	awaitItem(t, conn, cwd, "the timed main-agent response", func(item *frontendv1.ConversationItem) bool {
		return item.GetAssistantMessage().GetId() == "msg_fake_1"
	})
	store.write(sidecarAssistantUsageEvent(t, vendorID, "e2e-aggregate-subagent", "msg-subagent", "agent-child"))
	awaitItem(t, conn, cwd, "the untimed subagent response", func(item *frontendv1.ConversationItem) bool {
		return item.GetAssistantMessage().GetId() == "msg-subagent"
	})

	aggregate := sessionViewFromSnapshot(t, h, id).GetTokenUtilization()
	if aggregate == nil {
		t.Fatal("SessionView omitted token_utilization after completed main-agent and subagent responses")
	}
	all := aggregate.GetAllAgents()
	if got, want := []int64{all.GetInputTokens(), all.GetOutputTokens(), all.GetCacheReadInputTokens(), all.GetCacheCreationInputTokens()}, []int64{107, 211, 800, 75}; fmt.Sprint(got) != fmt.Sprint(want) {
		t.Errorf("all-agent token totals = %v, want %v", got, want)
	}
	if all.GetCacheCreation().GetEphemeral_5MInputTokens() != 25 || all.GetCacheCreation().GetEphemeral_1HInputTokens() != 50 {
		t.Errorf("all-agent cache TTL totals = %v, want 5m=25 and 1h=50", all.GetCacheCreation())
	}
	if all.GetServerToolUse().GetWebSearchRequests() != 2 || all.GetServerToolUse().GetWebFetchRequests() != 3 {
		t.Errorf("all-agent server-tool totals = %v, want search=2 and fetch=3", all.GetServerToolUse())
	}
	if got, want := all.GetCacheRates().GetCacheHitRate(), float64(800)/982; math.Abs(got-want) > 1e-12 {
		t.Errorf("all-agent cache_hit_rate = %.12f, want %.12f from authoritative counters", got, want)
	}
	timing := all.GetTiming()
	if timing == nil || timing.GetOutputTokensWithGenerationDuration() != 11 || timing.GetResponsesWithGenerationDuration() != 1 || timing.GetResponsesWithoutGenerationDuration() != 1 {
		t.Errorf("all-agent timing = %v, want 11 timed output tokens with response counts timed=1 untimed=1", timing)
	}
	if aggregate.GetMainAgent().GetOutputTokens() != 11 || aggregate.GetMainAgent().GetTiming().GetResponsesWithGenerationDuration() != 1 {
		t.Errorf("main-agent aggregate = %v, want only the timed fake response", aggregate.GetMainAgent())
	}
	if len(aggregate.GetSubagents()) != 1 || aggregate.GetSubagents()[0].GetAgent().GetAgentId() != "agent-child" || aggregate.GetSubagents()[0].GetTotals().GetOutputTokens() != 200 {
		t.Errorf("subagent aggregates = %v, want agent-child with 200 output tokens", aggregate.GetSubagents())
	}
	if len(aggregate.GetModels()) != 2 {
		t.Errorf("model aggregates = %d, want fake-model and claude-opus-test", len(aggregate.GetModels()))
	}
}

// TestE2ECreateResumeFailureIsTypedAndVisible verifies an explicit resume that
// cannot find its transcript is nacked with structured continuity evidence.
// Validation happens before shim spawn, so the fixture cannot contact a vendor.
func TestE2ECreateResumeFailureIsTypedAndVisible(t *testing.T) {
	h := newUDSHarness(t)
	conn := h.dialFrontend(t)
	defer conn.Close()
	if readFrame(t, conn).GetSnapshot() == nil {
		t.Fatal("first frontend frame is not a StateSnapshot")
	}
	cwd := t.TempDir()
	configDir := t.TempDir()
	const requestID = "e2e-missing-explicit-resume"
	const claudeID = "11111111-2222-4333-8444-555555555555"
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"createSession":{"cwd":%q,"configDir":%q,"fake":true,"resumeMode":"RESUME_MODE_EXPLICIT","explicitClaudeSessionId":%q}}`, requestID, cwd, configDir, claudeID))
	for {
		ack := readFrame(t, conn).GetCommandAck()
		if ack == nil || ack.GetRequestId() != requestID {
			continue
		}
		if ack.GetOk() {
			t.Fatal("explicit resume with no transcript succeeded: continuity failure must not fall back to a fresh conversation")
		}
		failure := ack.GetFailure().GetSessionResume()
		if failure == nil || failure.GetCreate() == nil || failure.GetTranscriptUnavailable() == nil {
			t.Fatalf("create resume nack structured detail = %v, want create + transcript_unavailable", ack.GetFailure())
		}
		if failure.GetClaudeSessionId() != claudeID || failure.GetCwd() != cwd || failure.GetResolvedConfigDir() == "" || len(failure.GetTranscriptUnavailable().GetSearchedPaths()) == 0 {
			t.Errorf("create resume failure evidence = %v, want exact Claude id cwd resolved config root and searched paths", failure)
		}
		return
	}
}

// TestE2EAutomaticRestoreFailureIsTypedAndVisible hibernates an offline fake
// session whose vendor UUID has no transcript, then reopens the workspace.
// Rehydration must preserve that exact UUID and surface a typed automatic
// restore failure rather than reconstructing history into a fresh prompt.
func TestE2EAutomaticRestoreFailureIsTypedAndVisible(t *testing.T) {
	h := newUDSHarness(t, withIdleSweeper())
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if readFrame(t, conn).GetSnapshot() == nil {
		t.Fatal("first session frame is not a StateSnapshot")
	}
	observedStates(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_READY)
	view := sessionViewFromSnapshot(t, h, id)
	if view.GetClaudeSessionId() == "" {
		t.Fatal("created session has no authoritative Claude session id to resume")
	}
	h.sweepIdle <- time.Now()
	observedStates(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_HIBERNATED)

	const requestID = "e2e-automatic-restore-missing-transcript"
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"openWorkspace":{}}`, requestID))
	for {
		ack := readFrame(t, conn).GetCommandAck()
		if ack == nil || ack.GetRequestId() != requestID {
			continue
		}
		if ack.GetOk() {
			t.Fatal("automatic restore without the authoritative transcript succeeded by reconstructing or starting fresh")
		}
		failure := ack.GetFailure().GetSessionResume()
		if failure == nil || failure.GetAutomaticRestore() == nil || failure.GetTranscriptUnavailable() == nil {
			t.Fatalf("automatic restore nack structured detail = %v, want automatic_restore + transcript_unavailable", ack.GetFailure())
		}
		if failure.GetAgentReplSessionId() != id || failure.GetClaudeSessionId() != view.GetClaudeSessionId() || failure.GetCwd() != cwd {
			t.Errorf("automatic restore failure evidence = %v, want session=%q Claude=%q cwd=%q", failure, id, view.GetClaudeSessionId(), cwd)
		}
		return
	}
}
