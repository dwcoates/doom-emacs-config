// Token utilization acceptance coverage.  The fake SDK exercises the real
// shim, store, daemon, and frontend route without allowing a vendor call.
package e2e

import (
	"fmt"
	"math"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
	"claude-repld/internal/statedb"

	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// isFakeTurnMessage reports whether id is the fake shim's api message id for
// turn n.
//
// IT MATCHES THE SHAPE RATHER THAN AN EXACT STRING, because the id gained
// PER-SPAWN uniqueness: the fake used to mint `msg_fake_<turn>` off a
// per-instance counter, so a session whose shim respawned — a revival, a
// rewind — re-minted turn 1's id under the same session, and the daemon's
// token-utilization store (keyed session plus api_message_id) correctly refused
// the divergent duplicate and tore the controller down. The id is now
// `msg_fake_<spawn>_<turn>`, which these assertions still recognize by prefix
// and turn suffix.
func isFakeTurnMessage(id string, n int) bool {
	return strings.HasPrefix(id, "msg_fake_") && strings.HasSuffix(id, fmt.Sprintf("_%d", n))
}

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

	writeCmd(t, conn, `{"requestId":"e2e-token-main","submitPrompt":{"text":"measure response timing","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitItem(t, conn, cwd, "the completed assistant response", func(item *frontendv1.ConversationItem) bool {
		return isFakeTurnMessage(item.GetAgent().GetResponse().GetBody().GetId(), 1)
	})
	// A file-plane assistant item can be pushed before its stream-plane
	// message_start timing reaches the correlator. The terminal result is the
	// turn's last conversation item, so wait for it before asserting against
	// the durable replay that this test promises to cover.
	awaitItem(t, conn, cwd, "the turn's terminal result", isResult)

	fresh, state := dialForReplay(t, h, id, cwd)
	replay := replayItems(t, fresh, state, cwd, "e2e-token-replay")
	var replayed *frontendv1.ConversationItem
	for _, candidate := range replay {
		if isFakeTurnMessage(candidate.GetAgent().GetResponse().GetBody().GetId(), 1) {
			replayed = candidate
			break
		}
	}
	if replayed == nil {
		t.Fatal("replay omitted the assistant response carrying token utilization")
	}
	usage := requireSingleTokenUtilization(t, h, id, replayed.GetAgent().GetResponse().GetBody().GetId(), "replayed assistant response")
	// The wire's half of the same fact: the bubble's corner shows what the
	// daemon resolved from this record, and it must be the record's own output.
	if stamp := requireResponseStamp(t, replayed, "replayed assistant response"); stamp.GetOutputTokens() != usage.GetUsage().GetOutputTokens() {
		t.Errorf("resolved stamp output_tokens = %d, want the durable record's %d", stamp.GetOutputTokens(), usage.GetUsage().GetOutputTokens())
	}
	if got, want := usage.GetAgentReplSessionId(), id; got != want {
		t.Errorf("token utilization agent_repl_session_id = %q, want session %q", got, want)
	}
	if usage.GetClaudeSessionId() == "" {
		t.Error("token utilization claude_session_id is empty: response ownership must survive the shim-to-daemon boundary")
	}
	if got, want := usage.GetApiMessageId(), replayed.GetAgent().GetResponse().GetBody().GetId(); got != want {
		t.Errorf("token utilization api_message_id = %q, want the response's own message id %q", got, want)
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
}

// requireSingleTokenUtilization reads the ONE durable utilization record a
// response produced, from the ledger that owns it.
//
// The record left the conversation item: the feed carries the RESOLVED stamp
// the bubble's corner renders, and the vendor-faithful evidence stays in the
// durable layer. Both are still asserted — this reads the evidence, and
// requireResponseStamp below reads what the wire resolved from it.
func requireSingleTokenUtilization(t *testing.T, h *e2eHarness, sessionID, apiMessageID, source string) *frontendv1.TokenUtilization {
	t.Helper()
	ledger, err := statedb.NewTokenUtilizations(h.stateDB)
	if err != nil {
		t.Fatalf("open durable token-utilization ledger: %v", err)
	}
	all, err := ledger.List(sessionID)
	if err != nil {
		t.Fatalf("list durable token-utilization ledger: %v", err)
	}
	var records []*frontendv1.TokenUtilization
	for _, record := range all {
		if record.GetApiMessageId() == apiMessageID {
			records = append(records, record)
		}
	}
	if len(records) != 1 {
		t.Fatalf("%s token utilization records = %d, want exactly one API-response record", source, len(records))
	}
	return records[0]
}

// requireResponseStamp reads the resolved figures a response's bubble renders.
// They are the daemon's own derivation from the durable record above, so a
// stamp that disagrees with it is the two halves drifting apart.
func requireResponseStamp(t *testing.T, item *frontendv1.ConversationItem, source string) *frontendv1.ResponseUsageStamp {
	t.Helper()
	stamp := item.GetAgent().GetResponse().GetUsageStamp()
	if stamp == nil {
		t.Fatalf("%s carries no resolved usage stamp", source)
	}
	return stamp
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
	awaitItem(t, conn, cwd, "the historical assistant response carrying token utilization", func(item *frontendv1.ConversationItem) bool {
		return item.GetAgent().GetResponse().GetBody().GetId() == "msg-historical"
	})
	usage := requireSingleTokenUtilization(t, h, id, "msg-historical", "historical assistant response")
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

	fresh, state := dialForReplay(t, h, id, cwd)
	replayed := replayItems(t, fresh, state, cwd, "e2e-historical-usage-replay")
	seen := 0
	for _, candidate := range replayed {
		if candidate.GetAgent().GetResponse().GetBody().GetId() == "msg-historical" {
			seen++
		}
	}
	if seen != 1 {
		t.Errorf("duplicate store observations produced %d replayed historical responses, want exactly one", seen)
	}
	// The de-duplication's other half: one durable record, still explicitly
	// untimed after the replay. It is asserted against the ledger because that
	// is where the record lives now.
	replayedUsage := requireSingleTokenUtilization(t, h, id, "msg-historical", "replayed historical assistant response")
	if replayedUsage.GetResponseTiming() != nil {
		t.Error("the replayed historical response acquired response timing: whole-turn duration must never be used as a generation-time fallback")
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
		Message: &datav1.ApiAssistantMessage{Id: messageID, Model: "claude-opus-test", Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "historical response"}}}}, Usage: &datav1.ApiUsage{
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
	writeCmd(t, conn, `{"requestId":"e2e-token-aggregate","submitPrompt":{"text":"timed main response","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitItem(t, conn, cwd, "the timed main-agent response", func(item *frontendv1.ConversationItem) bool {
		return isFakeTurnMessage(item.GetAgent().GetResponse().GetBody().GetId(), 1)
	})
	store.write(sidecarAssistantUsageEvent(t, vendorID, "e2e-aggregate-subagent", "msg-subagent", "agent-child"))
	awaitItem(t, conn, cwd, "the untimed subagent response", func(item *frontendv1.ConversationItem) bool {
		return item.GetAgent().GetResponse().GetBody().GetId() == "msg-subagent"
	})

	// The session aggregate left SessionView: economics reach a rendering
	// frontend as TokenBreakdownView, resolved. The aggregate itself is
	// unchanged and is read here from the durable ledger that computes it, so
	// every figure below is still asserted against the same numbers.
	aggregate, err := durableSessionAggregate(h, id)
	if err != nil {
		t.Fatalf("read the durable session token aggregate: %v", err)
	}
	if aggregate == nil {
		t.Fatal("the durable ledger holds no session aggregate after completed main-agent and subagent responses")
	}
	all := aggregate.GetAllAgents()
	if got, want := []int64{all.GetInputTokens(), all.GetOutputTokens(), all.GetCacheReadInputTokens(), all.GetCacheCreationInputTokens()}, []int64{107, 211, 880, 79}; fmt.Sprint(got) != fmt.Sprint(want) {
		t.Errorf("all-agent token totals = %v, want %v", got, want)
	}
	if all.GetCacheCreation().GetEphemeral_5MInputTokens() != 29 || all.GetCacheCreation().GetEphemeral_1HInputTokens() != 50 {
		t.Errorf("all-agent cache TTL totals = %v, want 5m=29 and 1h=50", all.GetCacheCreation())
	}
	if all.GetServerToolUse().GetWebSearchRequests() != 2 || all.GetServerToolUse().GetWebFetchRequests() != 3 {
		t.Errorf("all-agent server-tool totals = %v, want search=2 and fetch=3", all.GetServerToolUse())
	}
	if got, want := all.GetCacheRates().GetCacheHitRate(), float64(880)/1066; math.Abs(got-want) > 1e-12 {
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
// The harness forces only the spawned process onto the fake SDK. The command
// deliberately omits fake so the production resume gate cannot be bypassed;
// validation happens before shim spawn, so no vendor process can run.
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
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"createSession":{"cwd":%q,"configDir":%q,"resumeMode":"RESUME_MODE_EXPLICIT","explicitClaudeSessionId":%q}}`, requestID, cwd, configDir, claudeID))
	for {
		ack := readFrame(t, conn).GetCommandAck()
		if ack == nil || ack.GetRequestId() != requestID {
			continue
		}
		if ack.GetOk() {
			t.Fatal("explicit resume with no transcript succeeded: continuity failure must not fall back to a fresh conversation")
		}
		if got := errclass.KindName(ack.GetFailure()); got != string(errclass.TypeSessionResumeFailed) {
			t.Errorf("create resume nack error_type = %q, want %q", got, errclass.TypeSessionResumeFailed)
		}
		failure := ack.GetFailure().GetSessionResumeFailed().GetDetail()
		if failure == nil || failure.GetCreate() == nil || failure.GetTranscriptUnavailable() == nil {
			t.Fatalf("create resume nack structured detail = %v, want create + transcript_unavailable", ack.GetFailure())
		}
		if failure.GetClaudeSessionId() != claudeID || failure.GetCwd() != cwd || failure.GetResolvedConfigDir() == "" || len(failure.GetTranscriptUnavailable().GetSearchedPaths()) == 0 {
			t.Errorf("create resume failure evidence = %v, want exact Claude id cwd resolved config root and searched paths", failure)
		}
		if got := h.shimSpawns(); got != 0 {
			t.Errorf("shim spawn count after rejected resume = %d, want zero", got)
		}
		audit := h.dialFrontend(t)
		snapshot := readFrame(t, audit).GetSnapshot()
		audit.Close()
		if snapshot == nil {
			t.Fatal("post-rejection frontend frame is not a StateSnapshot")
		}
		for _, view := range snapshot.GetSessions() {
			if view.GetCwd() == cwd {
				t.Fatalf("rejected resume registered session %q for cwd %q", view.GetSessionId(), cwd)
			}
		}
		return
	}
}

// TestE2EAutomaticRestoreFailureIsTypedAndVisible hibernates an offline fake
// session, then makes its resumed fake SDK query die after the daemon
// handshake and before readiness. OpenWorkspace must wait for that exact
// restoration failure rather than acknowledging an unusable session.
func TestE2EAutomaticRestoreFailureIsTypedAndVisible(t *testing.T) {
	t.Setenv("AGENT_REPL_E2E_FAIL_RESUMED_FAKE_QUERY", "1")
	h := newUDSHarness(t, withIdleSweeper())
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	snapshot := readFrame(t, conn).GetSnapshot()
	if snapshot == nil {
		t.Fatal("first session frame is not a StateSnapshot")
	}
	ready := false
	for _, state := range snapshot.GetWorkspaces() {
		if state.GetWorkspace() == cwd && state.GetState() == frontendv1.RenderState_RENDER_STATE_READY {
			ready = true
			break
		}
	}
	if !ready {
		t.Fatalf("create acknowledgement snapshot workspaces = %v, want READY state for %q", snapshot.GetWorkspaces(), cwd)
	}
	// createSession returns only after the correlated successful CreateSession
	// ACK. This fresh global snapshot therefore proves that command completion
	// publishes the authoritative session/vendor identity it promises.
	view := sessionViewFromSnapshot(t, h, id)
	if view.GetClaudeSessionId() == "" {
		t.Fatal("created session has no authoritative Claude session id to resume")
	}
	h.sweepIdle <- time.Now()
	observedStates(t, snapshot, conn, cwd, frontendv1.RenderState_RENDER_STATE_HIBERNATED)

	const requestID = "e2e-automatic-restore-query-failure"
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"openWorkspace":{}}`, requestID))
	var acknowledged bool
	var resumeFailure *frontendv1.SessionResumeFailure
	var termination *frontendv1.QueryTerminationFailure
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) && (!acknowledged || termination == nil) {
		frame := readFrame(t, conn)
		if ack := frame.GetCommandAck(); ack != nil && ack.GetRequestId() == requestID {
			if ack.GetOk() {
				t.Fatal("OpenWorkspace acknowledged a session whose resumed query died before readiness")
			}
			acknowledged = true
			resumeFailure = ack.GetFailure().GetSessionResumeFailed().GetDetail()
		}
		for _, item := range deltaItems(frame, cwd) {
			failure := item.GetFailureCard()
			if failure != nil && errclass.TypeName(failure) == "unexpected_query_termination" {
				termination = failure.GetKind().GetQueryTermination().GetDetail()
			}
		}
	}
	if !acknowledged {
		t.Fatal("OpenWorkspace did not return a failure acknowledgement after the resumed query died")
	}
	if resumeFailure == nil || resumeFailure.GetAutomaticRestore() == nil || resumeFailure.GetClaudeSessionId() != view.GetClaudeSessionId() || resumeFailure.GetCwd() != cwd || resumeFailure.GetResolvedConfigDir() == "" || resumeFailure.GetQueryTermination() == nil || resumeFailure.GetQueryTermination().GetVendorSessionId() != view.GetClaudeSessionId() || resumeFailure.GetQueryTermination().GetIteratorFailure() == nil {
		t.Fatalf("automatic restore command nack = %v, want typed session.resume iterator failure with session=%q vendor=%q", resumeFailure, id, view.GetClaudeSessionId())
	}
	if termination == nil || termination.GetVendorSessionId() != view.GetClaudeSessionId() || termination.GetIteratorFailure() == nil {
		t.Fatalf("automatic restore query-termination evidence = %v, want session=%q vendor=%q iterator_failure", termination, id, view.GetClaudeSessionId())
	}
}

// durableSessionAggregate computes a session's token aggregate from the durable
// ledger, through the SAME aggregation the daemon uses.
//
// The aggregate left SessionView with the rest of the durable evidence layer;
// its rendering replacement is TokenBreakdownView. The figures themselves are
// unchanged, and this reads them where they are still authoritative.
func durableSessionAggregate(h *e2eHarness, sessionID string) (*frontendv1.SessionTokenUtilization, error) {
	ledger, err := statedb.NewTokenUtilizations(h.stateDB)
	if err != nil {
		return nil, err
	}
	records, err := ledger.List(sessionID)
	if err != nil {
		return nil, err
	}
	if err := frontend.ValidateTokenUtilizationAggregation(records); err != nil {
		return nil, err
	}
	return frontend.AggregateTokenUtilization(records), nil
}
