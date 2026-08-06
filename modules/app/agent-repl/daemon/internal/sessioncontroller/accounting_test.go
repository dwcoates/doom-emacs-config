package sessioncontroller

import (
	"errors"
	"fmt"
	"math"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

type failingTurnAccountingStore struct{ err error }

func (s failingTurnAccountingStore) Record(string, *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error) {
	return nil, s.err
}
func (s failingTurnAccountingStore) List(string) ([]*frontendv1.TurnAccounting, error) {
	return nil, s.err
}

type emptyTurnAccountingStore struct{}

func (emptyTurnAccountingStore) Record(_ string, accounting *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error) {
	return accounting, nil
}
func (emptyTurnAccountingStore) List(string) ([]*frontendv1.TurnAccounting, error) {
	return nil, nil
}

type replayTurnAccountingStore struct {
	accountings []*frontendv1.TurnAccounting
	err         error
}

type fakeHistoricalUsageStore struct {
	inserted bool
	err      error
	records  []*frontendv1.TokenUtilization
}

func (s *fakeHistoricalUsageStore) RecordHistorical(record *frontendv1.TokenUtilization) (bool, error) {
	if s.err != nil {
		return false, s.err
	}
	s.records = append(s.records, proto.Clone(record).(*frontendv1.TokenUtilization))
	return s.inserted, nil
}

func (s replayTurnAccountingStore) Record(_ string, accounting *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error) {
	return accounting, s.err
}
func (s replayTurnAccountingStore) List(string) ([]*frontendv1.TurnAccounting, error) {
	return s.accountings, s.err
}

func TestTurnAccountingReducerCompletesWithBoundaryAndMatchingLedger(t *testing.T) {
	r := newTurnAccountingReducer()
	r.observe(&corev1.Event{Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "q",
		Event: &corev1.QueryLifecycle_RuntimeObserved{RuntimeObserved: &corev1.QueryRuntimeObserved{
			Identity: completeRuntimeIdentity(),
		}},
	}}}, "s")
	r.observe(&corev1.Event{ProducedAtMs: 10, Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}}}, "s")
	r.observe(&corev1.Event{RequestId: "t", Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: usageObservation("t", true)}}, "s")
	r.observe(&corev1.Event{ProducedAtMs: 12, Payload: &corev1.Event_MessageLatency{MessageLatency: &corev1.MessageLatency{Uuid: "m", TtftMs: 42}}}, "s")
	r.observe(accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Message: &datav1.ApiAssistantMessage{Id: "m", Model: "model", Usage: &datav1.ApiUsage{InputTokens: 1, OutputTokens: 2, CacheReadInputTokens: 3, CacheCreationInputTokens: 4}}}}}), "s")
	r.observe(accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{Usage: &datav1.Usage{InputTokens: 1, OutputTokens: 2, CacheReadInputTokens: 3, CacheCreationInputTokens: 4}, ModelUsage: map[string]*datav1.ModelUsage{"model": {InputTokens: 1, OutputTokens: 2, CacheReadInputTokens: 3, CacheCreationInputTokens: 4}}}}}), "s")
	r.observe(&corev1.Event{RequestId: "t", Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: usageObservation("t", false)}}, "s")
	got := r.resolve(&corev1.Event{Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}}}, 30)
	if got.GetComplete() == nil || len(got.GetResponses()) != 1 || got.GetResponses()[0].GetResponseTiming().GetTimeToFirstTokenMs() != 42 || got.GetResponses()[0].GetResponseTiming().GetOutputGenerationDurationMs() != 8 || got.GetReconciliation().GetResponseMainAgent().GetOutputTokens() != 2 {
		t.Fatalf("accounting = %+v", got)
	}
}

func TestReconcileTokenUsageNamesEveryResponseInStableOrderWithoutResult(t *testing.T) {
	records := []*frontendv1.TokenUtilization{
		{ApiMessageId: "message-b"},
		{ApiMessageId: "message-c"},
		{ApiMessageId: "message-a"},
	}
	reconciliation := reconcileTokenUsage(records, nil).reconciliation
	got := reconciliation.GetApiMessageIds()
	if reconciliation.GetResponseRecordCount() != 3 || len(got) != 3 || got[0] != "message-a" || got[1] != "message-b" || got[2] != "message-c" {
		t.Fatalf("reconciliation = %+v, want one sorted nonempty id per response", reconciliation)
	}
}

func TestTurnAccountingReducerRejectsUsageWithoutAPIMessageID(t *testing.T) {
	r := newTurnAccountingReducer()
	if err := r.observe(&corev1.Event{Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "turn"}}}, "session"); err != nil {
		t.Fatal(err)
	}
	err := r.observe(accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Message: &datav1.ApiAssistantMessage{Usage: &datav1.ApiUsage{InputTokens: 1}}}}}), "session")
	if err == nil || !strings.Contains(err.Error(), "no API message id") {
		t.Fatalf("observe error = %v, want missing API message id rejection", err)
	}
	if got := r.turns["turn"].responses; len(got) != 0 {
		t.Fatalf("responses = %+v, want no unidentified usage record", got)
	}
}

func TestResponseWithoutMessageStartRemainsExplicitlyUntimed(t *testing.T) {
	r := newTurnAccountingReducer()
	if err := r.observe(&corev1.Event{Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}}}, "s"); err != nil {
		t.Fatal(err)
	}
	if err := r.observe(accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Message: &datav1.ApiAssistantMessage{Id: "historical", Usage: &datav1.ApiUsage{OutputTokens: 2}}}}}), "s"); err != nil {
		t.Fatal(err)
	}
	if got := r.turns["t"].responses[0].GetResponseTiming(); got != nil {
		t.Fatalf("historical response timing = %+v, want nil without message_start", got)
	}
}

func completeRuntimeIdentity() *corev1.QueryRuntimeIdentity {
	fingerprint := func(value string) *corev1.EvidenceFingerprint {
		return &corev1.EvidenceFingerprint{Evidence: &corev1.EvidenceFingerprint_Sha256{Sha256: value}}
	}
	return &corev1.QueryRuntimeIdentity{
		VendorSessionId: "vendor", EffectiveModel: "model", SdkVersion: "sdk", ClaudeCodeVersion: "code",
		ShimBuildSha: "shim", AuthSource: "auth", SubscriptionType: "subscription", FastModeState: "state", FastModeReason: "reason",
		EffectiveOptions: fingerprint("options"), Settings: fingerprint("settings"), Tools: fingerprint("tools"), Mcp: fingerprint("mcp"), ContextPrefix: fingerprint("prefix"),
	}
}

func TestTurnAccountingHandshakeBindsQueryAndRuntimeIdentityWithoutMutationOnRejection(t *testing.T) {
	r := newTurnAccountingReducer()
	runtime := completeRuntimeIdentity()
	hello := &corev1.ShimHello{QueryInstanceId: "query-1", QueryCreatedSeq: 17, VendorSessionId: "vendor", QueryRuntimeIdentity: runtime}
	if err := r.bindHandshakeIdentity(hello); err != nil {
		t.Fatalf("bind first hello: %v", err)
	}
	if err := r.bindHandshakeIdentity(hello); err != nil {
		t.Fatalf("bind same hello: %v", err)
	}
	if r.queryID != "query-1" || !proto.Equal(r.runtime, runtime) {
		t.Fatalf("bound identity query=%q runtime=%+v", r.queryID, r.runtime)
	}

	tests := []struct {
		name  string
		hello *corev1.ShimHello
		want  string
	}{
		{name: "blank query", hello: &corev1.ShimHello{VendorSessionId: "vendor"}, want: "omitted query_instance_id"},
		{name: "different query", hello: &corev1.ShimHello{QueryInstanceId: "query-2", VendorSessionId: "vendor"}, want: "does not match bound"},
		{name: "different creation sequence", hello: &corev1.ShimHello{QueryInstanceId: "query-1", QueryCreatedSeq: 18, VendorSessionId: "vendor"}, want: "does not match bound query_created_seq"},
		{name: "runtime lacks hello vendor", hello: &corev1.ShimHello{QueryInstanceId: "query-1", QueryRuntimeIdentity: runtime}, want: "without vendor_session_id"},
		{name: "runtime lacks vendor", hello: &corev1.ShimHello{QueryInstanceId: "query-1", QueryCreatedSeq: 17, VendorSessionId: "vendor", QueryRuntimeIdentity: &corev1.QueryRuntimeIdentity{}}, want: "omitted vendor_session_id"},
		{name: "runtime vendor mismatch", hello: &corev1.ShimHello{QueryInstanceId: "query-1", VendorSessionId: "vendor-other", QueryRuntimeIdentity: runtime}, want: "does not match vendor_session_id"},
		{name: "runtime differs within one vendor session", hello: &corev1.ShimHello{QueryInstanceId: "query-1", QueryCreatedSeq: 17, VendorSessionId: "vendor", QueryRuntimeIdentity: &corev1.QueryRuntimeIdentity{VendorSessionId: "vendor", EffectiveModel: "different"}}, want: "does not match the bound runtime identity"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			beforeQuery, beforeCreatedSeq, beforeStoreSession, beforeRuntime := r.queryID, r.queryCreatedSeq, r.queryStoreSessionID, r.runtime
			err := r.bindHandshakeIdentity(test.hello)
			if err == nil || !strings.Contains(err.Error(), test.want) {
				t.Fatalf("bind error = %v, want %q", err, test.want)
			}
			if r.queryID != beforeQuery || r.queryCreatedSeq != beforeCreatedSeq || r.queryStoreSessionID != beforeStoreSession || !proto.Equal(r.runtime, beforeRuntime) {
				t.Fatalf("rejected hello mutated reducer: query=%q runtime=%+v", r.queryID, r.runtime)
			}
		})
	}
	rotated := proto.Clone(runtime).(*corev1.QueryRuntimeIdentity)
	rotated.VendorSessionId = "vendor-rotated"
	if err := r.bindHandshakeIdentity(&corev1.ShimHello{QueryInstanceId: "query-1", VendorSessionId: "vendor-rotated", QueryRuntimeIdentity: rotated}); err != nil {
		t.Fatalf("bind rotated runtime: %v", err)
	}
	if r.queryID != "query-1" || !proto.Equal(r.runtime, rotated) {
		t.Fatalf("rotated binding query=%q runtime=%+v", r.queryID, r.runtime)
	}
}

func TestTurnAccountingAcceptsRetiredQueryLifecycleWithoutReplacingLiveHandshakeIdentity(t *testing.T) {
	var logs []string
	c := newConsumer("ws", "s", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(format string, args ...any) {
		logs = append(logs, fmt.Sprintf(format, args...))
	}, nil, nil, nil, nil, nil)
	liveRuntime := completeRuntimeIdentity()
	if err := c.accounting.bindHandshakeIdentity(&corev1.ShimHello{
		QueryInstanceId:      "live-query",
		QueryCreatedSeq:      8,
		VendorSessionId:      "vendor",
		QueryRuntimeIdentity: liveRuntime,
	}); err != nil {
		t.Fatalf("bind live handshake: %v", err)
	}

	historicalCreated := &corev1.Event{Seq: 5, QueryInstanceId: "retired-query", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "retired-query",
		Event: &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{
			Invocation: &corev1.QueryCreated_Resumed{Resumed: &corev1.ResumedQuery{RequestedVendorSessionId: "vendor"}},
		}},
	}}}
	historicalRuntime := &corev1.Event{Seq: 6, QueryInstanceId: "retired-query", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "retired-query",
		Event:           &corev1.QueryLifecycle_RuntimeObserved{RuntimeObserved: &corev1.QueryRuntimeObserved{Identity: liveRuntime}},
	}}}
	historicalTerminated := &corev1.Event{Seq: 7, QueryInstanceId: "retired-query", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "retired-query",
		ObservedAtMs:    1234,
		Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{
			VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor"},
			Reason: &corev1.QueryTerminated_Intentional{Intentional: &corev1.IntentionalQueryTermination{
				Reason: "SIGTERM",
			}},
		}},
	}}}
	for _, historical := range []*corev1.Event{historicalCreated, historicalRuntime, historicalTerminated} {
		if err := c.Consume(historical); err != nil {
			t.Fatalf("consume retired lifecycle seq=%d: %v", historical.GetSeq(), err)
		}
	}
	if c.accounting.queryID != "live-query" || !proto.Equal(c.accounting.runtime, liveRuntime) {
		t.Fatalf("retired lifecycle rebound accounting identity: query=%q runtime=%+v", c.accounting.queryID, c.accounting.runtime)
	}
	created := &corev1.Event{Seq: 8, QueryInstanceId: "live-query", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "live-query",
		Event: &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{
			Invocation: &corev1.QueryCreated_Resumed{Resumed: &corev1.ResumedQuery{RequestedVendorSessionId: "vendor"}},
		}},
	}}}
	if err := c.Consume(created); err != nil {
		t.Fatalf("consume live query creation: %v", err)
	}
	observedRuntime := proto.Clone(liveRuntime).(*corev1.QueryRuntimeIdentity)
	observedRuntime.FastModeReason = "runtime event"
	runtimeObserved := &corev1.Event{Seq: 9, QueryInstanceId: "live-query", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "live-query",
		Event: &corev1.QueryLifecycle_RuntimeObserved{RuntimeObserved: &corev1.QueryRuntimeObserved{
			Identity: observedRuntime,
		}},
	}}}
	if err := c.Consume(runtimeObserved); err != nil {
		t.Fatalf("consume live runtime: %v", err)
	}
	if c.accounting.queryID != "live-query" || !proto.Equal(c.accounting.runtime, observedRuntime) {
		t.Fatalf("live lifecycle was not authoritative: query=%q runtime=%+v", c.accounting.queryID, c.accounting.runtime)
	}
	// THE LIVE CONTRADICTION IS STILL FATAL. The same retired-query payload,
	// but PRODUCED BY the live query: the running invocation is claiming a
	// lifecycle that is not its own, which no provenance rule excuses.
	contradiction := proto.Clone(historicalTerminated).(*corev1.Event)
	contradiction.Seq = 10
	contradiction.QueryInstanceId = "live-query"
	if err := c.Consume(contradiction); err == nil || !strings.Contains(err.Error(), "does not match bound query_instance_id") {
		t.Fatalf("live-produced retired lifecycle error = %v, want hard identity failure", err)
	}
	log := strings.Join(logs, "\n")
	for _, field := range []string{
		"historical query lifecycle ACCEPTED without accounting rebind",
		"session=s",
		"seq=5",
		`historical_query_instance_id="retired-query"`,
		`live_query_instance_id="live-query"`,
		"decision=retain_history_keep_live_handshake_authority",
	} {
		if !strings.Contains(log, field) {
			t.Fatalf("acceptance log = %q, want field %q", log, field)
		}
	}
}

func TestHistoricalResumeIdentityMismatchRemainsFatalBeforeLiveQueryBoundary(t *testing.T) {
	c := newConsumer("ws", "s", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(string, ...any) {}, nil, nil, nil, nil, nil)
	if err := c.accounting.bindHandshakeIdentity(&corev1.ShimHello{QueryInstanceId: "live-query", QueryCreatedSeq: 10, VendorSessionId: "vendor"}); err != nil {
		t.Fatalf("bind live handshake: %v", err)
	}
	created := &corev1.Event{Seq: 5, QueryInstanceId: "retired-query", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "retired-query",
		Event:           &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{Invocation: &corev1.QueryCreated_Resumed{Resumed: &corev1.ResumedQuery{RequestedVendorSessionId: "requested-vendor"}}}},
	}}}
	if err := c.Consume(created); err != nil {
		t.Fatalf("consume historical QueryCreated: %v", err)
	}
	runtime := &corev1.Event{Seq: 6, QueryInstanceId: "retired-query", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "retired-query",
		Event:           &corev1.QueryLifecycle_RuntimeObserved{RuntimeObserved: &corev1.QueryRuntimeObserved{Identity: &corev1.QueryRuntimeIdentity{VendorSessionId: "replacement-vendor"}}},
	}}}
	err := c.Consume(runtime)
	if err == nil || !strings.Contains(err.Error(), `resumed query reported vendor session "replacement-vendor" instead of requested session "requested-vendor"`) {
		t.Fatalf("historical identity mismatch error = %v", err)
	}
	if c.accounting.queryID != "live-query" || c.accounting.queryCreatedSeq != 10 {
		t.Fatalf("rejected historical mismatch mutated accounting boundary: query=%q seq=%d", c.accounting.queryID, c.accounting.queryCreatedSeq)
	}
}

// ---------------------------------------------------------------------------
// THE BLAST-RADIUS CONTRACT, pinned once more directly on Apply (the
// lifecycle path the three diagnosed failures actually took), alongside the
// per-error-class coverage above: a bookkeeping-only accounting failure never
// denies the turn boundary or session establishment (the GUARANTEE), while
// the one accounting failure that is a genuine protocol contradiction — the
// live query lifecycle disagreeing with its own bound identity — stays
// exactly as fatal as it always was (the VIOLATION).
// ---------------------------------------------------------------------------

// TestAccountingBookkeepingFailureOnApplyDoesNotDenyEstablishment is the
// GUARANTEE: a malformed usage observation delivered through Apply (the
// lifecycle path, not just Consume's frame-translation path) still lets the
// turn boundary reach the SSM — establishment proceeds — with only this
// event's accounting degraded.
func TestAccountingBookkeepingFailureOnApplyDoesNotDenyEstablishment(t *testing.T) {
	// Arrange.
	applier := &fakeApplier{}
	c := newConsumer("ws", "s", &fakePusher{}, applier, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(string, ...any) {}, nil, nil, nil, nil, nil)
	c.accounting.queryID = "q"
	malformed := usageObservation("t-missing", true) // names a turn the reducer never admitted.

	// Act.
	err := c.Apply(&corev1.Event{Seq: 1, RequestId: "t-missing", Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: malformed}})

	// Assert.
	if err != nil {
		t.Fatalf("Apply error = %v, want the bookkeeping failure to leave the boundary and establishment unaffected", err)
	}
	if len(applier.applied) != 1 {
		t.Fatalf("ssm apply count = %d, want the event still applied", len(applier.applied))
	}
}

// TestAccountingQueryIdentityContradictionOnApplyStaysFatal is the
// VIOLATION: the live query lifecycle contradicting its own bound identity is
// not bookkeeping, so Apply still refuses the delivery for it exactly as it
// did before the blast-radius change.
func TestAccountingQueryIdentityContradictionOnApplyStaysFatal(t *testing.T) {
	// Arrange.
	c := newConsumer("ws", "s", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(string, ...any) {}, nil, nil, nil, nil, nil)
	c.accounting.queryID = "live-query"
	contradiction := &corev1.Event{Seq: 1, QueryInstanceId: "live-query", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "other-query",
	}}}

	// Act.
	err := c.Apply(contradiction)

	// Assert.
	if err == nil || !errors.Is(err, ErrAccountingQueryIdentityContradiction) {
		t.Fatalf("Apply error = %v, want the query identity contradiction to stay fatal", err)
	}
}

func TestTokenUsageFromAPIPreservesEveryRawUsageField(t *testing.T) {
	cache, _ := structpb.NewStruct(map[string]any{"ephemeral_5m_input_tokens": 11, "ephemeral_1h_input_tokens": 12})
	tools, _ := structpb.NewStruct(map[string]any{"web_search_requests": 2, "web_fetch_requests": 3})
	fallback, _ := structpb.NewStruct(map[string]any{"accepted": true})
	output, _ := structpb.NewStruct(map[string]any{"thinking_tokens": 9})
	unmodeled, _ := structpb.NewStruct(map[string]any{"future_counter": 4})
	diagnostic, _ := structpb.NewStruct(map[string]any{"cache_miss_reason": "tools_changed", "cache_missed_input_tokens": 17})
	iterations, _ := structpb.NewList([]any{map[string]any{"type": "fallback", "model": "fallback-model", "input_tokens": 1, "output_tokens": 2, "cache_read_input_tokens": 3, "cache_creation_input_tokens": 4, "cache_creation": map[string]any{"ephemeral_5m_input_tokens": 4}}})
	want := &datav1.ApiUsage{InputTokens: 1, OutputTokens: 2, CacheReadInputTokens: 3, CacheCreationInputTokens: 4, CacheCreation: cache, ServerToolUse: tools, ServiceTier: "priority", Iterations: iterations, Speed: "fast", InferenceGeo: "us", FallbackCredit: fallback, OutputTokensDetails: output, UnmodeledUsage: unmodeled, CacheDiagnostic: diagnostic}
	rates := &datav1.PromptCacheRates{TotalPromptInputTokens: 18, CacheHitRate: 0.125, CacheWriteRate: 0.25, UncachedInputRate: 0.625}
	got := tokenUsageFromAPI(want, rates)
	if got.GetCacheCreation().GetEphemeral_5MInputTokens() != 11 || got.GetCacheCreation().GetEphemeral_1HInputTokens() != 12 || got.GetServerToolUse().GetWebFetchRequests() != 3 || got.GetOutputDetails().GetThinkingTokens() != 9 || got.GetFallbackCredit().GetFields()["accepted"].GetBoolValue() != true || got.GetUnmodeledUsage().GetFields()["future_counter"].GetNumberValue() != 4 || got.GetCacheDiagnostic().GetToolsChanged().GetCacheMissedInputTokens() != 17 || got.GetIterations()[0].GetFallback().GetModel() != "fallback-model" || !proto.Equal(got.GetRawUsage(), want) {
		t.Fatalf("usage = %+v", got)
	}
	if got.GetCacheRates().GetTotalPromptInputTokens() != 18 || got.GetCacheRates().GetCacheHitRate() != 0.125 || got.GetCacheRates().GetCacheWriteRate() != 0.25 || got.GetCacheRates().GetUncachedInputRate() != 0.625 {
		t.Fatalf("cache rates = %+v, want exact shim-owned values", got.GetCacheRates())
	}
}

func TestTokenUtilizationFromEventMapsSubagentLineageExactly(t *testing.T) {
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
		AgentId: "agent", ParentToolUseId: "tool", ParentAgentId: "parent", SubagentType: "research", TaskDescription: "investigate",
		Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}},
	}}})

	record, err := tokenUtilizationFromEvent(event, "session", nil)
	if err != nil {
		t.Fatal(err)
	}
	got := record.GetSubagent()
	want := &frontendv1.TokenUtilizationSubagent{AgentId: "agent", ParentToolUseId: "tool", ParentAgentId: "parent", SubagentType: "research", TaskDescription: "investigate"}
	if !proto.Equal(got, want) {
		t.Fatalf("subagent = %v, want %v", got, want)
	}
}

func TestTokenUtilizationFromEventLeavesAbsentSubagentLineageEmpty(t *testing.T) {
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
		AgentId: "agent",
		Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}},
	}}})

	record, err := tokenUtilizationFromEvent(event, "session", nil)
	if err != nil {
		t.Fatal(err)
	}
	got := record.GetSubagent()
	if got.GetAgentId() != "agent" || got.GetParentToolUseId() != "" || got.GetParentAgentId() != "" || got.GetSubagentType() != "" || got.GetTaskDescription() != "" {
		t.Fatalf("subagent = %+v, want only agent_id", got)
	}
}

func TestTokenUtilizationFromEventClassifiesTaskDescriptionOnlyAsSubagent(t *testing.T) {
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
		TaskDescription: "inspect cache evidence",
		Message:         &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}},
	}}})

	record, err := tokenUtilizationFromEvent(event, "session", nil)
	if err != nil {
		t.Fatal(err)
	}
	got := record.GetSubagent()
	if got == nil || got.GetTaskDescription() != "inspect cache evidence" || got.GetAgentId() != "" || got.GetParentToolUseId() != "" || got.GetParentAgentId() != "" || got.GetSubagentType() != "" {
		t.Fatalf("subagent = %+v, want exact task-description-only provenance", got)
	}
}

func TestTokenUtilizationObservationMapsHistoricalTranscriptWithoutInventingTurnOrTiming(t *testing.T) {
	cacheCreation, err := structpb.NewStruct(map[string]any{"ephemeral_1h_input_tokens": 13})
	if err != nil {
		t.Fatal(err)
	}
	line := &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{SessionId: "claude", AgentId: "nested", SourceToolUseId: "tool", RequestId: "api-request"},
		Message: &datav1.ApiAssistantMessage{Id: "message", Model: "opus", Usage: &datav1.ApiUsage{
			InputTokens: 3, OutputTokens: 5, CacheReadInputTokens: 8, CacheCreationInputTokens: 13, CacheCreation: cacheCreation,
		}},
	}}}
	vendor, err := anypb.New(line)
	if err != nil {
		t.Fatal(err)
	}
	event := &corev1.Event{SessionId: "claude", Plane: corev1.Plane_PLANE_FILE, Payload: &corev1.Event_Vendor{Vendor: vendor}}

	observation, err := tokenUtilizationObservationFromEvent(event, "session", nil)
	if err != nil {
		t.Fatal(err)
	}
	got := observation.record
	if !observation.historical || got.GetAgentReplSessionId() != "session" || got.GetClaudeSessionId() != "claude" || got.GetRootTurnId() != "" || got.GetApiRequestId() != "api-request" || got.GetApiMessageId() != "message" || got.GetModel() != "opus" {
		t.Fatalf("historical observation = %+v, want exact source identity with no root turn", observation)
	}
	if got.GetResponseTiming() != nil || got.GetUsage().GetCacheCreation().GetEphemeral_1HInputTokens() != 13 {
		t.Fatalf("historical usage = %+v, want explicit untimed usage", got)
	}
	if actor := got.GetSubagent(); actor == nil || actor.GetAgentId() != "nested" || actor.GetParentToolUseId() != "tool" {
		t.Fatalf("historical actor = %+v, want transcript provenance", actor)
	}

	reducer := newTurnAccountingReducer()
	if err := reducer.observe(event, "session"); err != nil {
		t.Fatalf("historical observation claimed mutable live turn: %v", err)
	}
	if len(reducer.turns) != 0 || reducer.activeTurnID != "" {
		t.Fatalf("historical observation mutated live reducer: %+v", reducer)
	}
}

func TestTokenUtilizationObservationRejectsHistoricalTranscriptSessionMismatch(t *testing.T) {
	line := &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{SessionId: "other"},
		Message:  &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}},
	}}}
	vendor, err := anypb.New(line)
	if err != nil {
		t.Fatal(err)
	}
	event := &corev1.Event{SessionId: "claude", Plane: corev1.Plane_PLANE_FILE, Payload: &corev1.Event_Vendor{Vendor: vendor}}
	if _, err := tokenUtilizationObservationFromEvent(event, "session", nil); err == nil || !strings.Contains(err.Error(), "Claude session mismatch") {
		t.Fatalf("mismatched transcript error = %v, want Claude session mismatch", err)
	}
}

// ---------------------------------------------------------------------------
// A RESUMED CONVERSATION'S RETIRED VENDOR SESSION IS STILL THIS
// CONVERSATION'S OWN HISTORY. The live incident: workspace session
// s_109c53d47ad718f0 resumed a conversation from vendor session
// fe97f7a9-f138-45ec-b3cb-e608fa2fceb2 into 60f52b56-ed1d-4577-9f27-88d85d88dbb4.
// The SDK carries the prior transcript content into the resumed session's own
// file unchanged, so replaying api_message_id="msg_011Cdk3va97299HDHGq6tun3"
// off the CURRENT file (envelope session_id=60f52b56...) surfaces an
// assistant message still embedding the RETIRED session id
// (fe97f7a9-f138-45ec-b3cb-e608fa2fceb2) it was actually produced under. That
// is expected history, not corruption, once the conversation's own resume
// lineage proves the retired id belongs to it.
// ---------------------------------------------------------------------------

func historicalTranscriptEvent(t *testing.T, envelopeSessionID, assistantSessionID string) *corev1.Event {
	t.Helper()
	line := &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{SessionId: assistantSessionID},
		Message:  &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}},
	}}}
	vendor, err := anypb.New(line)
	if err != nil {
		t.Fatal(err)
	}
	return &corev1.Event{SessionId: envelopeSessionID, Plane: corev1.Plane_PLANE_FILE, Payload: &corev1.Event_Vendor{Vendor: vendor}}
}

// TestTokenUtilizationObservationAcceptsHistoricalResponseFromAKnownPriorVendorSession
// is the GUARANTEE: a historical transcript response naming a RETIRED vendor
// session this conversation has proven as its own resume ancestor is admitted
// as history rather than rejected.
func TestTokenUtilizationObservationAcceptsHistoricalResponseFromAKnownPriorVendorSession(t *testing.T) {
	event := historicalTranscriptEvent(t, "60f52b56-ed1d-4577-9f27-88d85d88dbb4", "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2")
	known := func(id string) bool { return id == "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2" }

	observation, err := tokenUtilizationObservationFromEvent(event, "session", known)

	if err != nil {
		t.Fatalf("known prior-session response = %v, want it admitted as history", err)
	}
	if !observation.historical || observation.priorVendorSession != "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2" {
		t.Fatalf("observation = %+v, want it marked as a prior-vendor-session record", observation)
	}
	if observation.record.GetClaudeSessionId() != "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2" {
		t.Fatalf("record claude_session_id = %q, want the RETIRED session preserved as the historical fact", observation.record.GetClaudeSessionId())
	}
}

// TestTokenUtilizationObservationRejectsHistoricalResponseFromAnUnknownVendorSession
// is the VIOLATION: the exception is not a blanket amnesty for any historical
// mismatch. A vendor session this conversation has never proven as its own
// stays fatal even with a knownVendorSession func wired in.
func TestTokenUtilizationObservationRejectsHistoricalResponseFromAnUnknownVendorSession(t *testing.T) {
	event := historicalTranscriptEvent(t, "60f52b56-ed1d-4577-9f27-88d85d88dbb4", "some-unrelated-conversations-session")
	known := func(id string) bool { return id == "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2" }

	if _, err := tokenUtilizationObservationFromEvent(event, "session", known); err == nil || !strings.Contains(err.Error(), "Claude session mismatch") {
		t.Fatalf("unknown prior-session response err = %v, want Claude session mismatch", err)
	}
}

// TestTokenUtilizationObservationRejectsLiveResponseFromAKnownPriorVendorSession
// is the VIOLATION's other edge: the prior-session exception applies only to
// HISTORICAL evidence. A LIVE stream response naming a session other than the
// one it is streaming on is a genuine contradiction and stays fatal, even
// when that other session is a proven prior session of this conversation.
func TestTokenUtilizationObservationRejectsLiveResponseFromAKnownPriorVendorSession(t *testing.T) {
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
		SessionId: "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2",
		Message:   &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}},
	}}})
	known := func(id string) bool { return id == "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2" }

	if _, err := tokenUtilizationObservationFromEvent(event, "session", known); err == nil || !strings.Contains(err.Error(), "Claude session mismatch") {
		t.Fatalf("live prior-session response err = %v, want Claude session mismatch", err)
	}
}

// TestReducerRecordsResumeLineageAndAdmitsTheRetiredSession is the reducer-
// level GUARANTEE: a QueryLifecycle stream naming the resume's requested
// vendor session durably teaches the reducer that lineage, so a LATER
// historical response naming that retired session is admitted through the
// full accounting path — the exact path settleTurnAccounting depends on.
func TestReducerRecordsResumeLineageAndAdmitsTheRetiredSession(t *testing.T) {
	// Arrange.
	reducer := newTurnAccountingReducer()
	created := &corev1.Event{Seq: 1, QueryInstanceId: "q2", Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
		QueryInstanceId: "q2",
		Event: &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{
			Invocation: &corev1.QueryCreated_Resumed{Resumed: &corev1.ResumedQuery{RequestedVendorSessionId: "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2"}},
		}},
	}}}
	if err := reducer.observe(created, "session"); err != nil {
		t.Fatalf("observe QueryCreated: %v", err)
	}

	// Act.
	response := historicalTranscriptEvent(t, "60f52b56-ed1d-4577-9f27-88d85d88dbb4", "fe97f7a9-f138-45ec-b3cb-e608fa2fceb2")
	err := reducer.observe(response, "session")

	// Assert.
	if err != nil {
		t.Fatalf("observe historical response from the resume's own prior session: %v", err)
	}
	if !reducer.isKnownVendorSession("fe97f7a9-f138-45ec-b3cb-e608fa2fceb2") {
		t.Fatal("resume lineage was not recorded as known")
	}
}

func TestTokenUtilizationFromEventRejectsSessionCorrelationViolations(t *testing.T) {
	tests := []struct {
		name            string
		daemonSessionID string
		eventSessionID  string
		assistantID     string
		want            string
	}{
		{name: "blank agent-repl session", eventSessionID: "vendor", assistantID: "vendor", want: "blank agent-repl"},
		{name: "blank authoritative Claude session", daemonSessionID: "agent", assistantID: "vendor", want: "blank authoritative"},
		{name: "blank assistant Claude session", daemonSessionID: "agent", eventSessionID: "vendor", want: "blank assistant"},
		{name: "mismatched Claude session", daemonSessionID: "agent", eventSessionID: "vendor-a", assistantID: "vendor-b", want: "Claude session mismatch"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			stream := &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{SessionId: tc.assistantID, Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}}}}}
			vendor, err := anypb.New(stream)
			if err != nil {
				t.Fatal(err)
			}
			event := &corev1.Event{SessionId: tc.eventSessionID, RequestId: "turn", Payload: &corev1.Event_Vendor{Vendor: vendor}}
			if _, err := tokenUtilizationFromEvent(event, tc.daemonSessionID, nil); err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestResponseUsageRejectsRootTurnMismatchBeforeReducerMutation(t *testing.T) {
	r := newTurnAccountingReducer()
	r.activeTurnID = "turn"
	r.turns["turn"] = &accountingTurn{}
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}}}}})
	event.RequestId = "other-turn"
	err := r.observe(event, "session")
	var claim *unattributedResponseUsageError
	if !errors.As(err, &claim) || claim.reason != "root_turn_id_mismatch" {
		t.Fatalf("error = %v, want root-turn mismatch", err)
	}
	if len(r.turns["turn"].responses) != 0 {
		t.Fatalf("mismatched response mutated reducer: %+v", r.turns["turn"].responses)
	}
}

func TestResponseUsageWithoutValidatedClaimFailsBeforeReducerMutation(t *testing.T) {
	r := newTurnAccountingReducer()
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}}}}})
	err := r.observe(event, "session")
	var claim *unattributedResponseUsageError
	if !errors.As(err, &claim) || claim.reason != "no_active_turn" || claim.apiMessageID != "message" {
		t.Fatalf("observe error = %v, want no-active-turn claim rejection", err)
	}
	if len(r.turns) != 0 || len(r.latencies) != 0 {
		t.Fatalf("reducer mutated after rejected response: turns=%+v latencies=%+v", r.turns, r.latencies)
	}
}

func TestResponseUsageAcceptsRootTurnAdmittedByRotationBridge(t *testing.T) {
	r := newTurnAccountingReducer()
	bridge := &corev1.Event{
		ProducedAtMs: 10,
		Payload: &corev1.Event_TurnClaimBridge{TurnClaimBridge: &corev1.TurnClaimBridge{
			TurnId: "turn", PreviousSessionId: "vendor-old",
		}},
	}
	r.observeTurnClaimBridge(bridge)
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}}}}})
	event.RequestId = "turn"

	if err := r.observe(event, "session"); err != nil {
		t.Fatalf("observe response after rotation bridge: %v", err)
	}
	if got := r.turns["turn"].responses; len(got) != 1 || got[0].GetRootTurnId() != "turn" {
		t.Fatalf("bridged responses = %+v, want one response owned by turn", got)
	}
}

func TestResponseUsagePreservesIndependentSDKRequestIdentityUnderRootTurnClaim(t *testing.T) {
	r := newTurnAccountingReducer()
	r.activeTurnID = "turn"
	r.turns["turn"] = &accountingTurn{}
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{RequestId: proto.String("api-request"), Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}}}}})
	event.RequestId = "turn"
	if err := r.observe(event, "session"); err != nil {
		t.Fatalf("observe independent request identities: %v", err)
	}
	if got := r.turns["turn"].responses; len(got) != 1 || got[0].GetRootTurnId() != "turn" || got[0].GetApiRequestId() != "api-request" {
		t.Fatalf("responses = %+v, want active-turn claim with exact SDK request identity", got)
	}
	if r.turns["turn"].responses[0].ApiRequestId == nil {
		t.Fatal("present SDK request identity lost proto presence")
	}
	if event.GetRequestId() != "turn" {
		t.Fatalf("event request id = %q, want root turn identity", event.GetRequestId())
	}
}

func TestResponseUsagePreservesAbsentSDKRequestIdentityUnderActiveTurnClaim(t *testing.T) {
	r := newTurnAccountingReducer()
	r.activeTurnID = "turn"
	r.turns["turn"] = &accountingTurn{}
	stream := &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{SessionId: "vendor-session", Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}}}}}
	vendor, err := anypb.New(stream)
	if err != nil {
		t.Fatal(err)
	}
	event := &corev1.Event{SessionId: "vendor-session", RequestId: "turn", ProducedAtMs: 20, Payload: &corev1.Event_Vendor{Vendor: vendor}}
	if err := r.observe(event, "session"); err != nil {
		t.Fatalf("observe absent SDK request identity: %v", err)
	}
	if got := r.turns["turn"].responses; len(got) != 1 || got[0].GetRootTurnId() != "turn" || got[0].GetApiRequestId() != "" {
		t.Fatalf("responses = %+v, want active-turn claim preserving absent SDK request identity", got)
	}
	if r.turns["turn"].responses[0].ApiRequestId != nil {
		t.Fatalf("absent SDK request identity became present empty value: %+v", r.turns["turn"].responses[0].ApiRequestId)
	}
}

func TestResponseUsageRejectsPresentBlankSDKRequestIdentityBeforeReducerMutation(t *testing.T) {
	r := newTurnAccountingReducer()
	r.activeTurnID = "turn"
	r.turns["turn"] = &accountingTurn{}
	event := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{RequestId: proto.String(""), Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{InputTokens: 1}}}}})
	event.RequestId = "turn"

	err := r.observe(event, "session")
	if err == nil || !strings.Contains(err.Error(), "present but blank api_request_id") {
		t.Fatalf("observe present blank SDK request identity error = %v", err)
	}
	if got := r.turns["turn"].responses; len(got) != 0 {
		t.Fatalf("reducer retained rejected response = %+v", got)
	}
}

func TestUnmodeledUsageIsPreservedAndLoudLogged(t *testing.T) {
	unmodeled, _ := structpb.NewStruct(map[string]any{"future_counter": 4})
	var logs []string
	c := newConsumer("ws", "s", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) }, nil, nil, nil, nil, nil)
	c.accounting.activeTurnID = "t"
	c.accounting.turns["t"] = &accountingTurn{}
	c.Consume(accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Message: &datav1.ApiAssistantMessage{Id: "m", Usage: &datav1.ApiUsage{UnmodeledUsage: unmodeled}}}}}))
	if len(c.accounting.turns["t"].responses) != 1 || c.accounting.turns["t"].responses[0].GetUsage().GetUnmodeledUsage().GetFields()["future_counter"] == nil {
		t.Fatal("unmodeled usage not preserved")
	}
	if !strings.Contains(strings.Join(logs, "\n"), "API usage contains unmodeled fields") {
		t.Fatalf("logs = %v", logs)
	}
}

func TestTurnAccountingReducerInvalidatesMissingEvidence(t *testing.T) {
	r := newTurnAccountingReducer()
	r.observe(&corev1.Event{Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}}}, "s")
	got := r.resolve(&corev1.Event{Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}}}, 30)
	if got.GetInvalid() == nil || len(got.GetInvalid().GetProblems()) < 3 {
		t.Fatalf("accounting = %+v", got)
	}
}

func TestTurnAccountingReducerInvalidatesIncompleteRuntimeIdentity(t *testing.T) {
	r := newTurnAccountingReducer()
	r.queryID = "q"
	r.runtime = &corev1.QueryRuntimeIdentity{EffectiveModel: "model"}
	r.observe(&corev1.Event{Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}}}, "s")
	got := r.resolve(&corev1.Event{Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}}}, 30)
	var paths []string
	for _, problem := range got.GetInvalid().GetProblems() {
		paths = append(paths, problem.GetRuntimeIdentityIncomplete().GetMissingFieldPaths()...)
	}
	for _, want := range []string{"vendor_session_id", "effective_options", "context_prefix"} {
		if !containsPath(paths, want) {
			t.Errorf("missing runtime identity paths = %v, want %q", paths, want)
		}
	}
}

func TestHydratePersistedAccountingFeedsBothReplayConsumers(t *testing.T) {
	want := &frontendv1.TurnAccounting{TurnId: "turn", Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	m := &Manager{cfg: Config{TurnAccountings: replayTurnAccountingStore{accountings: []*frontendv1.TurnAccounting{want}}}, logf: t.Logf}
	for _, cons := range []*consumer{
		newConsumer("ws", "session", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil),
		newConsumer("ws", "session", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil),
	} {
		if err := m.hydratePersistedAccounting(cons, "session"); err != nil {
			t.Fatalf("hydrate persisted accounting: %v", err)
		}
		if got := cons.replayedAccounting["turn"]; !proto.Equal(got, want) {
			t.Fatalf("hydrated accounting = %v, want %v", got, want)
		}
	}
}

func TestNewRejectsMissingTurnAccountingStore(t *testing.T) {
	_, err := New(Config{
		Push: &fakePusher{}, SSM: &fakeApplier{}, Spawner: &fakeSpawner{},
		Locator:  fakeLocator{m: map[string]string{"ws": "session"}},
		SeqStore: &fakeSeqStore{seq: map[string]uint64{}}, ClearCompactStore: newFakeClearCompactStore(),
		Source: stubSource{}, FileDiagnostics: fakeFileDiagnosticPersister{},
	})
	if err == nil || !strings.Contains(err.Error(), "TurnAccountingStore") {
		t.Fatalf("New error = %v", err)
	}
}

func TestNewConsumerRejectsMissingTurnAccountingStoreBeforeConstruction(t *testing.T) {
	defer func() {
		if got := recover(); got == nil || !strings.Contains(fmt.Sprint(got), "TurnAccountingStore") {
			t.Fatalf("panic = %v", got)
		}
	}()
	newConsumer("ws", "session", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), nil, t.Logf, nil, nil, nil, nil, nil)
}

func containsPath(paths []string, suffix string) bool {
	for _, path := range paths {
		if strings.HasSuffix(path, suffix) {
			return true
		}
	}
	return false
}

func TestTurnAccountingReducerInvalidatesUsageWindowReset(t *testing.T) {
	r := newTurnAccountingReducer()
	r.runtime = &corev1.QueryRuntimeIdentity{EffectiveModel: "model"}
	r.queryID = "q"
	r.observe(&corev1.Event{Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}}}, "s")
	start := usageObservation("t", true)
	end := usageObservation("t", false)
	end.GetAvailable().FiveHour.ResetsAtMs = 200
	r.observe(&corev1.Event{RequestId: "t", Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: start}}, "s")
	r.observe(&corev1.Event{RequestId: "t", Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: end}}, "s")
	got := r.resolve(&corev1.Event{Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}}}, 30)
	found := false
	for _, problem := range got.GetInvalid().GetProblems() {
		if problem.GetWindowReset() != nil {
			found = true
		}
	}
	if !found {
		t.Fatalf("accounting = %+v", got)
	}
}

// TestTerminalAccountingPersistenceFailureDegradesAccountingWithoutDenyingEstablishment
// covers the BLAST-RADIUS requirement: a terminal accounting persistence
// failure is bookkeeping, not a protocol violation, so it must not deny the
// turn boundary itself or the session's establishment — Apply succeeds, the
// turn's own lifecycle state is unaffected, and only its accounting (and the
// terminal conversation delivery that accounting gates) stays withheld,
// loudly logged.
func TestTerminalAccountingPersistenceFailureDegradesAccountingWithoutDenyingEstablishment(t *testing.T) {
	push := &fakePusher{}
	var logs []string
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) }, nil, nil, nil, nil, nil)
	c.accountingStore = failingTurnAccountingStore{err: errors.New("disk unavailable")}
	if err := c.Apply(&corev1.Event{Seq: 1, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t", Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}}}); err != nil {
		t.Fatal(err)
	}
	c.Consume(accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{}}}))
	err := c.Apply(&corev1.Event{Seq: 2, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t", Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}}})
	if err != nil {
		t.Fatalf("Apply error = %v, want the turn boundary accepted despite the persistence failure", err)
	}
	if len(push.convo) != 0 {
		t.Fatalf("terminal conversation delivered before persistence: %+v", push.convo)
	}
	if c.accounting.turns["t"] == nil || c.accounting.activeTurnID != "t" {
		t.Fatalf("failed terminal persistence retired reducer state: turns=%+v active=%q", c.accounting.turns, c.accounting.activeTurnID)
	}
	log := strings.Join(logs, "\n")
	if !strings.Contains(log, "terminal accounting persistence FAILED") {
		t.Fatalf("logs = %v", logs)
	}
	if !strings.Contains(log, "ACCOUNTING DEGRADED") {
		t.Fatalf("logs = %v, want the blast-radius demotion logged loudly", logs)
	}
}

func TestTerminalAccountingRepublishesSessionViewAfterTerminalConversation(t *testing.T) {
	push := &fakePusher{}
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	c.onTerminalAccountingPersisted = func() {
		push.mu.Lock()
		push.trace = append(push.trace, "session_view")
		push.mu.Unlock()
	}
	if err := c.Apply(&corev1.Event{Seq: 1, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t", Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}}}); err != nil {
		t.Fatal(err)
	}
	if err := c.Consume(accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{}}})); err != nil {
		t.Fatal(err)
	}
	if err := c.Apply(&corev1.Event{Seq: 2, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t", Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{TurnId: "t"}}}); err != nil {
		t.Fatal(err)
	}
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.trace) < 2 || push.trace[len(push.trace)-2] != "conversation" || push.trace[len(push.trace)-1] != "session_view" {
		t.Fatalf("terminal delivery order = %v", push.trace)
	}
}

func TestUnexpectedQueryTerminationUsesOneAuthoritativeDegradedState(t *testing.T) {
	push := &fakePusher{}
	var logs []string
	degraded := 0
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) }, nil, nil, nil, nil, nil)
	c.onDegraded = func(*corev1.DegradedState) { degraded++ }
	if err := c.Consume(&corev1.Event{Seq: 7, ProducedAtMs: 9999, Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{QueryInstanceId: "q", ObservedAtMs: 1234, Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor"}, Reason: &corev1.QueryTerminated_UnexpectedEof{UnexpectedEof: &corev1.UnexpectedQueryEof{}}}}}}}); err != nil {
		t.Fatal(err)
	}
	queryID := "q"
	c.Degraded("s", &corev1.DegradedState{Component: "claude-shim-sdk", Reason: "unexpected_query_termination", QueryInstanceId: &queryID})
	if degraded != 1 {
		t.Fatalf("degraded callbacks = %d", degraded)
	}
	if len(push.convo) != 1 || len(push.convo[0].GetItems()) != 1 || push.convo[0].GetItems()[0].GetSystemFailure() == nil {
		t.Fatalf("failure pushes = %+v", push.convo)
	}
	failure := push.convo[0].GetItems()[0].GetSystemFailure()
	if failure.GetErrorClass() != frontendv1.ErrorClass_ERROR_CLASS_INTERNAL || failure.GetErrorType() != "unexpected_query_termination" || failure.GetMessage() == "" || failure.GetSourceDetail() == "" || failure.GetQueryTermination().GetAgentReplSessionId() != "s" || failure.GetQueryTermination().GetQueryInstanceId() != "q" || failure.GetQueryTermination().GetVendorSessionId() != "vendor" || failure.GetQueryTermination().GetObservedAtMs() != 1234 || failure.GetQueryTermination().GetUnexpectedEof() == nil {
		t.Fatalf("typed query termination failure = %+v", failure)
	}
	if !strings.Contains(strings.Join(logs, "\n"), "duplicate unexpected query termination suppressed") {
		t.Fatalf("logs = %v", logs)
	}
}

func TestReplayOnlyUnexpectedQueryDegradedStateSurfacesOnce(t *testing.T) {
	push := &fakePusher{}
	degraded := 0
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	c.onDegraded = func(*corev1.DegradedState) { degraded++ }
	queryID := "q"
	c.Degraded("s", &corev1.DegradedState{Component: "claude-shim-sdk", Reason: "unexpected_query_termination", QueryInstanceId: &queryID})
	if degraded != 1 || len(push.convo) != 1 || push.convo[0].GetItems()[0].GetSystemFailure().GetErrorType() != "unexpected_query_termination" {
		t.Fatalf("replay-only degraded state: callbacks=%d pushes=%+v", degraded, push.convo)
	}
}

func TestDurableReplayAttachesByteEquivalentPersistedAccounting(t *testing.T) {
	push := &fakePusher{}
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	wantUsage := &frontendv1.TokenUtilization{ApiMessageId: "m", Usage: &frontendv1.TokenUsage{InputTokens: 7}}
	want := &frontendv1.TurnAccounting{TurnId: "t", QueryInstanceId: "q", Responses: []*frontendv1.TokenUtilization{wantUsage}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	c.replayedAccounting["t"] = want
	c.replayedResponses["m"] = wantUsage
	assistant := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Uuid: "assistant-record", Message: &datav1.ApiAssistantMessage{Id: "m", Usage: &datav1.ApiUsage{InputTokens: 7}, Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hello"}}}}}}}})
	c.pushConversation(assistant, false)
	ev := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{}}})
	ev.RequestId = "t"
	c.pushConversation(ev, false)
	if len(push.convo) != 2 || len(push.convo[0].GetItems()[0].GetTokenUtilization()) != 1 || !proto.Equal(push.convo[0].GetItems()[0].GetTokenUtilization()[0], wantUsage) || !proto.Equal(push.convo[1].GetItems()[0].GetTurnAccounting(), want) {
		t.Fatalf("replayed delta = %+v", push.convo)
	}
}

func TestHistoricalConversationNeverFallsBackToLiveReducerAccounting(t *testing.T) {
	push := &fakePusher{}
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	liveUsage := &frontendv1.TokenUtilization{ApiMessageId: "m", Usage: &frontendv1.TokenUsage{InputTokens: 99}}
	c.accounting.activeTurnID = "live-turn"
	c.accounting.turns["live-turn"] = &accountingTurn{responses: []*frontendv1.TokenUtilization{liveUsage}}
	ev := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Uuid: "assistant-record", Message: &datav1.ApiAssistantMessage{Id: "m", Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hello"}}}}}}}})

	c.pushConversation(ev, false)
	if got := push.convo[0].GetItems()[0].GetTokenUtilization(); len(got) != 0 {
		t.Fatalf("historical item attached mutable live accounting: %+v", got)
	}
	c.pushConversation(ev, true)
	if got := push.convo[1].GetItems()[0].GetTokenUtilization(); len(got) != 1 || !proto.Equal(got[0], liveUsage) {
		t.Fatalf("live item utilization = %+v, want %+v", got, liveUsage)
	}
}

func TestHistoricalConversationAttachesTranscriptUsageOnLiveAndReplayPaths(t *testing.T) {
	push := &fakePusher{}
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	line := &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{Uuid: "line", SessionId: "claude", AgentId: "nested"},
		Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{
			InputTokens: 2, OutputTokens: 3, CacheReadInputTokens: 5,
		}, Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hello"}}}}},
	}}}
	vendor, err := anypb.New(line)
	if err != nil {
		t.Fatal(err)
	}
	event := &corev1.Event{Seq: 9, SessionId: "claude", Plane: corev1.Plane_PLANE_FILE, Payload: &corev1.Event_Vendor{Vendor: vendor}}

	c.pushConversation(event, true)
	c.pushConversation(event, false)
	if len(push.convo) != 2 {
		t.Fatalf("conversation pushes = %d, want live and replay", len(push.convo))
	}
	for i, delta := range push.convo {
		records := delta.GetItems()[0].GetTokenUtilization()
		if len(records) != 1 || records[0].GetRootTurnId() != "" || records[0].GetResponseTiming() != nil || records[0].GetUsage().GetCacheReadInputTokens() != 5 || records[0].GetSubagent().GetAgentId() != "nested" {
			t.Fatalf("push[%d] historical token utilization = %+v, want one stable untimed record", i, records)
		}
	}
}

func TestHistoricalUsagePersistsBeforeConsumerMutationAndRepublishesAggregate(t *testing.T) {
	push := &fakePusher{}
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	store := &fakeHistoricalUsageStore{inserted: true}
	c.historicalUsageStore = store
	republished := 0
	c.onHistoricalUsagePersisted = func() { republished++ }
	event := historicalUsageEvent(t)

	if err := c.Consume(event); err != nil {
		t.Fatalf("Consume historical usage: %v", err)
	}
	if len(store.records) != 1 || store.records[0].GetRootTurnId() != "" || store.records[0].GetResponseTiming() != nil {
		t.Fatalf("persisted historical records = %+v", store.records)
	}
	if len(c.snapshotRing()) != 1 || len(push.convo) != 1 || republished != 1 {
		t.Fatalf("accepted historical mutation: retained=%d pushes=%d republishes=%d", len(c.snapshotRing()), len(push.convo), republished)
	}
	store.inserted = false
	event.Seq++
	if err := c.Consume(event); err != nil {
		t.Fatalf("Consume exact historical replay: %v", err)
	}
	if republished != 1 {
		t.Fatalf("exact historical replay republished unchanged aggregate %d times, want once", republished)
	}
}

func TestHistoricalUsagePersistenceFailurePrecedesConsumerMutation(t *testing.T) {
	for _, tc := range []struct {
		name  string
		store HistoricalTokenUtilizationStore
		want  string
	}{
		{name: "unwired", want: "not wired"},
		{name: "write failure", store: &fakeHistoricalUsageStore{err: errors.New("disk unavailable")}, want: "disk unavailable"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			push := &fakePusher{}
			c := newConsumer("ws", "s", push, &fakeApplier{}, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
			c.historicalUsageStore = tc.store
			err := c.Consume(historicalUsageEvent(t))
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Consume error = %v, want %q", err, tc.want)
			}
			if len(c.snapshotRing()) != 0 || len(push.convo) != 0 {
				t.Fatalf("failed historical persistence mutated consumer: retained=%d pushes=%d", len(c.snapshotRing()), len(push.convo))
			}
		})
	}
}

func historicalUsageEvent(t *testing.T) *corev1.Event {
	t.Helper()
	line := &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{
		Envelope: &datav1.LineEnvelope{Uuid: "line", SessionId: "claude", AgentId: "nested"},
		Message: &datav1.ApiAssistantMessage{Id: "message", Usage: &datav1.ApiUsage{
			InputTokens: 2, OutputTokens: 3, CacheReadInputTokens: 5,
		}, Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hello"}}}}},
	}}}
	vendor, err := anypb.New(line)
	if err != nil {
		t.Fatal(err)
	}
	return &corev1.Event{Seq: 9, SessionId: "claude", Plane: corev1.Plane_PLANE_FILE, Payload: &corev1.Event_Vendor{Vendor: vendor}}
}

// TestUnknownUsageObservationDegradesAccountingWithoutDenyingTheEvent is the
// blast-radius contract for one of the three accounting error classes: an
// observation naming a turn the reducer never admitted is bookkeeping
// evidence the reducer cannot use, not a protocol violation, so it degrades
// this event's accounting (loudly) rather than rejecting the event itself.
func TestUnknownUsageObservationDegradesAccountingWithoutDenyingTheEvent(t *testing.T) {
	applier := &fakeApplier{}
	var logs []string
	c := newConsumer("ws", "s", &fakePusher{}, applier, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) }, nil, nil, nil, nil, nil)
	c.accounting.queryID = "q"
	ev := &corev1.Event{Seq: 9, RequestId: "t-missing", Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: usageObservation("t-missing", true)}}
	err := c.Apply(ev)
	if err != nil {
		t.Fatalf("Apply error = %v, want the event still applied despite the unknown-turn observation", err)
	}
	if len(applier.applied) != 1 || len(c.snapshotRing()) != 1 {
		t.Fatalf("event was not applied and retained: applied=%d retained=%d", len(applier.applied), len(c.snapshotRing()))
	}
	log := strings.Join(logs, "\n")
	if !strings.Contains(log, "turn accounting observation REJECTED") || !strings.Contains(log, `turn_id="t-missing"`) {
		t.Fatalf("logs = %v", logs)
	}
	if !strings.Contains(log, "ACCOUNTING DEGRADED") {
		t.Fatalf("logs = %v, want the blast-radius demotion logged loudly", logs)
	}
}

func TestMalformedUsageObservationDegradesAccountingWithoutDenyingTheEvent(t *testing.T) {
	tests := []struct {
		name            string
		authoritativeID string
		requestID       string
		mutate          func(*corev1.AccountUsageObservation)
		want            string
	}{
		{name: "authoritative query blank", authoritativeID: "", requestID: "t", want: "authoritative query_instance_id is required"},
		{name: "observation query blank", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.QueryInstanceId = " " }, want: "query_instance_id is required"},
		{name: "observation query mismatches", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.QueryInstanceId = "other" }, want: "does not match authoritative query_instance_id"},
		{name: "turn blank", authoritativeID: "q", requestID: "", mutate: func(o *corev1.AccountUsageObservation) { o.TurnId = " " }, want: "turn_id is required"},
		{name: "request mismatches turn", authoritativeID: "q", requestID: "other", want: "event request_id \"other\" does not match turn_id \"t\""},
		{name: "boundary timestamp zero", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.BoundaryAtMs = 0 }, want: "boundary_at_ms must be positive"},
		{name: "boundary timestamp negative", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.BoundaryAtMs = -1 }, want: "boundary_at_ms must be positive"},
		{name: "observed timestamp zero", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.ObservedAtMs = 0 }, want: "observed_at_ms must be positive"},
		{name: "observed timestamp negative", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.ObservedAtMs = -1 }, want: "observed_at_ms must be positive"},
		{name: "observation predates boundary", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.ObservedAtMs = o.BoundaryAtMs - 1 }, want: "precedes boundary_at_ms"},
		{name: "sample latency negative", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.SampleLatencyMs = -1 }, want: "sample_latency_ms must be nonnegative"},
		{name: "boundary absent", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Boundary = nil }, want: "boundary is required"},
		{name: "start boundary wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Boundary = &corev1.AccountUsageObservation_TurnStart{} }, want: "turn_start boundary is nil"},
		{name: "start boundary oneof wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Boundary = (*corev1.AccountUsageObservation_TurnStart)(nil) }, want: "turn_start boundary is nil"},
		{name: "end boundary wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Boundary = &corev1.AccountUsageObservation_TurnEnd{} }, want: "turn_end boundary is nil"},
		{name: "end boundary oneof wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Boundary = (*corev1.AccountUsageObservation_TurnEnd)(nil) }, want: "turn_end boundary is nil"},
		{name: "outcome absent", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Outcome = nil }, want: "outcome is required"},
		{name: "available wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Outcome = &corev1.AccountUsageObservation_Available{} }, want: "available outcome requires five_hour"},
		{name: "available oneof wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Outcome = (*corev1.AccountUsageObservation_Available)(nil) }, want: "available outcome requires five_hour"},
		{name: "available five hour nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Available{Available: &corev1.AccountUsageAvailable{}}
		}, want: "available outcome requires five_hour"},
		{name: "utilization below range", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.GetAvailable().FiveHour.UtilizationPercent = -1 }, want: "utilization_percent must be finite and within [0,100]"},
		{name: "utilization above range", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.GetAvailable().FiveHour.UtilizationPercent = 101 }, want: "utilization_percent must be finite and within [0,100]"},
		{name: "utilization NaN", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.GetAvailable().FiveHour.UtilizationPercent = math.NaN() }, want: "utilization_percent must be finite and within [0,100]"},
		{name: "utilization positive infinity", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.GetAvailable().FiveHour.UtilizationPercent = math.Inf(1) }, want: "utilization_percent must be finite and within [0,100]"},
		{name: "utilization negative infinity", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.GetAvailable().FiveHour.UtilizationPercent = math.Inf(-1) }, want: "utilization_percent must be finite and within [0,100]"},
		{name: "reset timestamp zero", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.GetAvailable().FiveHour.ResetsAtMs = 0 }, want: "resets_at_ms must be positive"},
		{name: "reset timestamp negative", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.GetAvailable().FiveHour.ResetsAtMs = -1 }, want: "resets_at_ms must be positive"},
		{name: "unavailable wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) { o.Outcome = &corev1.AccountUsageObservation_Unavailable{} }, want: "unavailable outcome requires a reason"},
		{name: "unavailable oneof wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = (*corev1.AccountUsageObservation_Unavailable)(nil)
		}, want: "unavailable outcome requires a reason"},
		{name: "unavailable reason absent", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{}}
		}, want: "unavailable outcome requires a reason"},
		{name: "service unavailable wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_ServiceUnavailable{}}}
		}, want: "unavailable.service_unavailable is nil"},
		{name: "service unavailable oneof wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: (*corev1.AccountUsageUnavailable_ServiceUnavailable)(nil)}}
		}, want: "unavailable.service_unavailable is nil"},
		{name: "window unavailable wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_WindowUnavailable{}}}
		}, want: "unavailable.window_unavailable is nil"},
		{name: "window unavailable oneof wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: (*corev1.AccountUsageUnavailable_WindowUnavailable)(nil)}}
		}, want: "unavailable.window_unavailable is nil"},
		{name: "utilization unavailable wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_UtilizationUnavailable{}}}
		}, want: "unavailable.utilization_unavailable is nil"},
		{name: "utilization unavailable oneof wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: (*corev1.AccountUsageUnavailable_UtilizationUnavailable)(nil)}}
		}, want: "unavailable.utilization_unavailable is nil"},
		{name: "sampling failure wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_SamplingFailure{}}}
		}, want: "unavailable.sampling_failure is nil"},
		{name: "sampling failure oneof wrapper nil", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: (*corev1.AccountUsageUnavailable_SamplingFailure)(nil)}}
		}, want: "unavailable.sampling_failure is nil"},
		{name: "sampling failure cause blank", authoritativeID: "q", requestID: "t", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_SamplingFailure{SamplingFailure: &corev1.UsageSamplingFailure{Cause: " "}}}}
		}, want: "unavailable.sampling_failure.cause is required"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			observation := usageObservation("t", true)
			if test.mutate != nil {
				test.mutate(observation)
			}
			assertMalformedUsageObservationRejected(t, test.authoritativeID, test.requestID, observation, test.want)
		})
	}
}

func TestTurnAccountingReducerAcceptsEveryUnavailableReason(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*corev1.AccountUsageObservation)
	}{
		{name: "service unavailable", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_ServiceUnavailable{ServiceUnavailable: &corev1.UsageServiceUnavailable{}}}}
		}},
		{name: "window unavailable", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_WindowUnavailable{WindowUnavailable: &corev1.FiveHourWindowUnavailable{}}}}
		}},
		{name: "utilization unavailable", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_UtilizationUnavailable{UtilizationUnavailable: &corev1.UtilizationUnavailable{}}}}
		}},
		{name: "sampling failure", mutate: func(o *corev1.AccountUsageObservation) {
			o.Outcome = &corev1.AccountUsageObservation_Unavailable{Unavailable: &corev1.AccountUsageUnavailable{Reason: &corev1.AccountUsageUnavailable_SamplingFailure{SamplingFailure: &corev1.UsageSamplingFailure{Cause: "upstream timeout"}}}}
		}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			r := newTurnAccountingReducer()
			r.queryID = "q"
			r.turns["t"] = &accountingTurn{}
			observation := usageObservation("t", true)
			test.mutate(observation)
			if err := r.observe(&corev1.Event{RequestId: "t", Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: observation}}, "s"); err != nil {
				t.Fatalf("observe unavailable usage: %v", err)
			}
			if r.turns["t"].startUsage != observation {
				t.Fatalf("usage observation was not retained")
			}
		})
	}
}

// assertMalformedUsageObservationRejected is the blast-radius contract for
// the third accounting error class: a malformed observation is bookkeeping
// evidence the reducer must refuse to retain, not a protocol violation, so it
// degrades this event's accounting (loudly, with the exact malformed cause)
// rather than rejecting the event itself.
func assertMalformedUsageObservationRejected(t *testing.T, authoritativeID, requestID string, observation *corev1.AccountUsageObservation, want string) {
	t.Helper()
	wantCause := validateAccountUsageObservation(liveEvidence{queryID: authoritativeID}, requestID, observation)
	var malformed *malformedAccountUsageObservationError
	if !errors.As(wantCause, &malformed) || !strings.Contains(wantCause.Error(), want) {
		t.Fatalf("expected malformed cause = %v, want it containing %q", wantCause, want)
	}
	applier := &fakeApplier{}
	var logs []string
	c := newConsumer("ws", "s", &fakePusher{}, applier, nil, newFakeClearCompactStore(), emptyTurnAccountingStore{}, func(format string, args ...any) { logs = append(logs, fmt.Sprintf(format, args...)) }, nil, nil, nil, nil, nil)
	c.accounting.queryID = authoritativeID
	if err := c.Apply(&corev1.Event{Seq: 1, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: "t", Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "t"}}}); err != nil {
		t.Fatalf("start turn: %v", err)
	}
	beforeApplied, beforeRetained := len(applier.applied), len(c.snapshotRing())
	err := c.Apply(&corev1.Event{Seq: 2, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, RequestId: requestID, Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: observation}})
	if err != nil {
		t.Fatalf("Apply error = %v, want the event still applied despite the malformed observation", err)
	}
	if len(applier.applied) != beforeApplied+1 || len(c.snapshotRing()) != beforeRetained+1 {
		t.Fatalf("event was not applied and retained: applied=%d retained=%d", len(applier.applied), len(c.snapshotRing()))
	}
	log := strings.Join(logs, "\n")
	if !strings.Contains(log, "ACCOUNTING DEGRADED") {
		t.Fatalf("logs = %v, want the blast-radius demotion logged loudly", logs)
	}
	boundary := "unspecified"
	switch observation.GetBoundary().(type) {
	case *corev1.AccountUsageObservation_TurnStart:
		boundary = "turn_start"
	case *corev1.AccountUsageObservation_TurnEnd:
		boundary = "turn_end"
	}
	for _, field := range []string{
		"turn accounting observation REJECTED before mutation",
		"session=s",
		fmt.Sprintf("authoritative_query_instance_id=%q", authoritativeID),
		fmt.Sprintf("query_instance_id=%q", observation.GetQueryInstanceId()),
		"seq=2",
		fmt.Sprintf("request_id=%q", requestID),
		fmt.Sprintf("turn_id=%q", observation.GetTurnId()),
		"boundary=" + boundary,
		fmt.Sprintf("cause=%q", malformed.Error()),
	} {
		if !strings.Contains(log, field) {
			t.Fatalf("rejection log = %q, want field %q", log, field)
		}
	}
}

func usageObservation(turn string, start bool) *corev1.AccountUsageObservation {
	o := &corev1.AccountUsageObservation{QueryInstanceId: "q", TurnId: turn, BoundaryAtMs: 10, ObservedAtMs: 15, SampleLatencyMs: 5, Outcome: &corev1.AccountUsageObservation_Available{Available: &corev1.AccountUsageAvailable{FiveHour: &corev1.UsageWindow{UtilizationPercent: 10, ResetsAtMs: 100}}}}
	if start {
		o.Boundary = &corev1.AccountUsageObservation_TurnStart{TurnStart: &corev1.TurnStartUsageBoundary{}}
	} else {
		o.Boundary = &corev1.AccountUsageObservation_TurnEnd{TurnEnd: &corev1.TurnEndUsageBoundary{}}
	}
	return o
}

func accountingVendorEvent(t *testing.T, m *datav1.ClaudeStreamMessage) *corev1.Event {
	t.Helper()
	if assistant := m.GetAssistant(); assistant != nil && assistant.RequestId == nil {
		assistant.RequestId = proto.String("t")
	}
	if assistant := m.GetAssistant(); assistant != nil && assistant.GetSessionId() == "" {
		assistant.SessionId = "vendor-session"
	}
	a, err := anypb.New(m)
	if err != nil {
		t.Fatal(err)
	}
	requestID := "t"
	if assistant := m.GetAssistant(); assistant != nil {
		requestID = assistant.GetRequestId()
	}
	return &corev1.Event{SessionId: "vendor-session", ProducedAtMs: 20, RequestId: requestID, Payload: &corev1.Event_Vendor{Vendor: a}}
}
