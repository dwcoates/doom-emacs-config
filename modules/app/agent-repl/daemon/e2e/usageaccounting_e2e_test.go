// Usage-accounting acceptance coverage. The suite drives the real fake SDK,
// shim, durable store, daemon, and frontend route. It states the ordering and
// evidence contract needed to compare subscription usage between clients.
package e2e

import (
	"errors"
	"fmt"
	"net"
	"slices"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"agentrepl/wire"
	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/structpb"

	"claude-repld/internal/statedb"
)

// TestE2ETurnAccountingIsDurableAndUsesOneLongLivedQuery proves that two
// settled turns use the same query instance and that each terminal result is
// released only with a durable accounting record that a fresh frontend can
// replay verbatim. The fake SDK intentionally reports unavailable runtime
// identity fields, so its verdict records that limitation explicitly.
func TestE2ETurnAccountingIsDurableAndUsesOneLongLivedQuery(t *testing.T) {
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, conn, vendorID, _ := liveSession(t, h, cwd)

	first := submitAndAwaitAccounting(t, conn, cwd, "e2e-accounting-first", "first accounting turn")
	second := submitAndAwaitAccounting(t, conn, cwd, "e2e-accounting-second", "second accounting turn")
	assertInvalidAccountingForRuntimeIdentityAndUnmodeledUsage(t, first, id, []string{
		"cache_diagnostic.reason", "cache_diagnostic.status", "fake_extension_tokens",
	})
	assertInvalidAccountingForRuntimeIdentityAndUnmodeledUsage(t, second, id, []string{
		"cache_diagnostic.reason", "cache_diagnostic.status", "fake_extension_tokens",
	})
	if first.GetQueryInstanceId() != second.GetQueryInstanceId() {
		t.Fatalf("query instance changed between ordinary turns: first=%q second=%q; query() must remain alive for the shim lifetime", first.GetQueryInstanceId(), second.GetQueryInstanceId())
	}

	assertDurableQueryLifecycle(t, replayStoreEvents(t, vendorID), first.GetQueryInstanceId())
	assertStateDBResponseLedger(t, h, id, first, second)

	var replayed *frontendv1.TurnAccounting
	for _, item := range replayItems(t, h.dial(t, id), cwd, "e2e-accounting-replay") {
		if item.GetTurnAccounting() != nil && item.GetTurnAccounting().GetTurnId() == second.GetTurnId() {
			replayed = item.GetTurnAccounting()
			break
		}
	}
	if replayed == nil {
		t.Fatalf("replay omitted terminal accounting for turn %q", second.GetTurnId())
	}
	if !proto.Equal(replayed, second) {
		t.Fatalf("replayed terminal accounting differs from the live terminal accounting\nreplay=%v\nlive=%v", replayed, second)
	}
}

// TestE2ETurnAccountingIncludesMainAndSubagentRawUsage uses the fake SDK's
// deterministic !usage-subagent fixture. The fixture contains nonzero values
// for every raw cache and token family, including intentionally unmodeled API
// usage fields. Every such field must remain available in raw evidence and
// make the comparison verdict invalid rather than being silently discarded.
func TestE2ETurnAccountingIncludesMainAndSubagentRawUsage(t *testing.T) {
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id, conn, _, _ := liveSession(t, h, cwd)

	accounting := submitAndAwaitAccounting(t, conn, cwd, "e2e-accounting-subagent", "!usage-subagent")
	assertInvalidAccountingForUnmodeledUsage(t, accounting, id)
	if got, want := len(accounting.GetResponses()), 2; got != want {
		t.Fatalf("accounting response records = %d, want main-agent and subagent records", got)
	}

	var main, subagent *frontendv1.TokenUtilization
	for _, response := range accounting.GetResponses() {
		switch {
		case response.GetMainAgent() != nil:
			main = response
		case response.GetSubagent() != nil:
			subagent = response
		default:
			t.Errorf("response %q has no actor arm", response.GetApiMessageId())
		}
	}
	if main == nil || subagent == nil {
		t.Fatalf("accounting actors main=%v subagent=%v, want one of each", main != nil, subagent != nil)
	}
	assertCompleteRawUsage(t, "main-agent", main, rawUsageFixture(t, "main-agent"))
	assertCompleteRawUsage(t, "subagent", subagent, rawUsageFixture(t, "subagent"))
	assertSubagentLineage(t, subagent)

	reconciliation := accounting.GetReconciliation()
	if reconciliation == nil {
		t.Fatal("terminal accounting omitted token reconciliation")
	}
	if got, want := reconciliation.GetResponseRecordCount(), int64(2); got != want {
		t.Errorf("reconciliation response_record_count = %d, want %d", got, want)
	}
	if got, want := len(reconciliation.GetApiMessageIds()), 2; got != want {
		t.Errorf("reconciliation api_message_ids = %d, want %d", got, want)
	}
	if reconciliation.GetResponseAllAgents() == nil || reconciliation.GetResponseMainAgent() == nil || reconciliation.GetResultMainAgent() == nil {
		t.Errorf("reconciliation totals all=%v main=%v result_main=%v, want every ledger total", reconciliation.GetResponseAllAgents() != nil, reconciliation.GetResponseMainAgent() != nil, reconciliation.GetResultMainAgent() != nil)
	}
	if len(reconciliation.GetResponseModels()) == 0 || len(reconciliation.GetResultModels()) == 0 {
		t.Errorf("reconciliation omitted response or terminal per-model totals: response=%v result=%v", reconciliation.GetResponseModels(), reconciliation.GetResultModels())
	}
}

// assertSubagentLineage checks the actor identity emitted by the raw SDK
// fixture survives the shim wire, durable daemon accounting, and frontend
// terminal payload without reconstruction or fallback.
func assertSubagentLineage(t *testing.T, response *frontendv1.TokenUtilization) {
	t.Helper()
	subagent := response.GetSubagent()
	if subagent == nil {
		t.Fatal("subagent usage record omitted its subagent actor arm")
	}
	if got, want := subagent.GetAgentId(), "fake-subagent-agent-id"; got != want {
		t.Errorf("subagent agent_id = %q, want %q", got, want)
	}
	if got, want := subagent.GetParentAgentId(), "fake-parent-agent-id"; got != want {
		t.Errorf("subagent parent_agent_id = %q, want %q", got, want)
	}
	if got, want := subagent.GetParentToolUseId(), "toolu_fake_subagent"; got != want {
		t.Errorf("subagent parent_tool_use_id = %q, want %q", got, want)
	}
	if got, want := subagent.GetSubagentType(), "general-purpose"; got != want {
		t.Errorf("subagent subagent_type = %q, want %q", got, want)
	}
	if got, want := subagent.GetTaskDescription(), "deterministic usage-accounting subagent"; got != want {
		t.Errorf("subagent task_description = %q, want %q", got, want)
	}
}

// TestE2EUnexpectedQueryTerminationReachesTypedFailure injects the fake
// query's explicit iterator EOF branch. An SDK iterator ending while the shim
// is alive is a protocol failure, not a hibernation or a clean turn result.
func TestE2EUnexpectedQueryTerminationReachesTypedFailure(t *testing.T) {
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, _, _ := liveSession(t, h, cwd)

	writeCmd(t, conn, `{"requestId":"e2e-query-eof","submitPrompt":{"text":"!query-eof"}}`)
	item, _ := awaitItem(t, conn, cwd, "the typed unexpected-query-termination failure", func(item *frontendv1.ConversationItem) bool {
		failure := item.GetSystemFailure()
		return failure != nil && failure.GetErrorClass() == frontendv1.ErrorClass_ERROR_CLASS_INTERNAL && failure.GetErrorType() == "unexpected_query_termination"
	})
	failure := item.GetSystemFailure()
	if failure.GetMessage() == "" || failure.GetSourceDetail() == "" {
		t.Errorf("unexpected query termination failure lacks diagnostic evidence: %v", failure)
	}
	detail := failure.GetQueryTermination()
	if detail == nil || detail.GetAgentReplSessionId() == "" || detail.GetQueryInstanceId() == "" || detail.GetVendorSessionId() == "" || detail.GetObservedAtMs() <= 0 || detail.GetUnexpectedEof() == nil {
		t.Fatalf("unexpected query termination omitted exact typed identity or reason: %v", detail)
	}
}

func submitAndAwaitAccounting(t *testing.T, conn *websocket.Conn, workspace, requestID, text string) *frontendv1.TurnAccounting {
	t.Helper()
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"submitPrompt":{"text":%q}}`, requestID, text))
	item, _ := awaitItem(t, conn, workspace, "the terminal result carrying turn accounting", isResult)
	if item.GetTurnAccounting() == nil {
		t.Fatalf("terminal result %q arrived before durable turn accounting", item.GetUuid())
	}
	return item.GetTurnAccounting()
}

func assertInvalidAccountingForUnmodeledUsage(t *testing.T, accounting *frontendv1.TurnAccounting, sessionID string) {
	t.Helper()
	assertAccountingEvidence(t, accounting, sessionID)
	gotRuntime, gotUnmodeled := accountingInvalidEvidence(t, accounting)
	assertRuntimeIdentityUnavailable(t, accounting, gotRuntime)
	var got []string
	got = append(got, gotUnmodeled...)
	want := []string{
		"cache_diagnostic.reason", "cache_diagnostic.reason",
		"future_counter", "future_counter",
		"nested", "nested",
		"output_tokens_details.reasoning_model", "output_tokens_details.reasoning_model",
	}
	if !slices.Equal(got, want) {
		t.Fatalf("turn %q unmodeled usage paths = %v, want %v", accounting.GetTurnId(), got, want)
	}
}

func assertInvalidAccountingForRuntimeIdentityAndUnmodeledUsage(t *testing.T, accounting *frontendv1.TurnAccounting, sessionID string, wantUnmodeled []string) {
	t.Helper()
	assertAccountingEvidence(t, accounting, sessionID)
	gotRuntime, gotUnmodeled := accountingInvalidEvidence(t, accounting)
	assertRuntimeIdentityUnavailable(t, accounting, gotRuntime)
	if !slices.Equal(gotUnmodeled, wantUnmodeled) {
		t.Fatalf("turn %q unmodeled usage paths = %v, want %v", accounting.GetTurnId(), gotUnmodeled, wantUnmodeled)
	}
}

func accountingInvalidEvidence(t *testing.T, accounting *frontendv1.TurnAccounting) (runtime, unmodeled []string) {
	t.Helper()
	invalid := accounting.GetInvalid()
	if invalid == nil || accounting.GetComplete() != nil {
		t.Fatalf("turn %q verdict = %T, want invalid with preserved comparison evidence", accounting.GetTurnId(), accounting.GetVerdict())
	}
	for _, problem := range invalid.GetProblems() {
		switch {
		case problem.GetRuntimeIdentityIncomplete() != nil:
			runtime = append(runtime, problem.GetRuntimeIdentityIncomplete().GetMissingFieldPaths()...)
		case problem.GetUnmodeledUsageFields() != nil:
			unmodeled = append(unmodeled, problem.GetUnmodeledUsageFields().GetSourceFieldPaths()...)
		default:
			t.Errorf("turn %q invalidating problem = %T, want only explicit unavailable runtime or unmodeled SDK usage evidence", accounting.GetTurnId(), problem.GetProblem())
		}
	}
	return runtime, unmodeled
}

func assertRuntimeIdentityUnavailable(t *testing.T, accounting *frontendv1.TurnAccounting, paths []string) {
	t.Helper()
	got := make([]string, 0, len(paths))
	for _, path := range paths {
		_, suffix, found := strings.Cut(path, ".runtime_observed.identity.")
		if !found {
			t.Fatalf("turn %q runtime identity path = %q, want runtime-observed identity path", accounting.GetTurnId(), path)
		}
		got = append(got, suffix)
	}
	slices.Sort(got)
	want := []string{
		"auth_source", "claude_code_version", "context_prefix", "effective_options",
		"fast_mode_reason", "fast_mode_state", "mcp", "settings", "shim_build_sha",
		"subscription_type", "tools",
	}
	if !slices.Equal(got, want) {
		t.Fatalf("turn %q unavailable runtime identity fields = %v, want %v", accounting.GetTurnId(), got, want)
	}
}

func assertAccountingEvidence(t *testing.T, accounting *frontendv1.TurnAccounting, sessionID string) {
	t.Helper()
	if accounting.GetTurnId() == "" || accounting.GetQueryInstanceId() == "" {
		t.Fatalf("turn accounting has blank turn or query identity: %v", accounting)
	}
	if accounting.GetRuntime() == nil || accounting.GetRuntime().GetVendorSessionId() == "" {
		t.Fatalf("turn %q omitted effective query runtime identity", accounting.GetTurnId())
	}
	timing := accounting.GetTiming()
	if timing == nil || timing.GetPromptAdmittedAtMs() <= 0 || timing.GetResultReceivedAtMs() <= 0 || timing.GetAccountingSettledAtMs() <= 0 || timing.GetPromptToResultMs() <= 0 {
		t.Fatalf("turn %q omitted timing required to derive wall duration and throughput: %v", accounting.GetTurnId(), timing)
	}
	if timing.GetPromptAdmittedAtMs() > timing.GetResultReceivedAtMs() || timing.GetResultReceivedAtMs() > timing.GetAccountingSettledAtMs() {
		t.Fatalf("turn %q timing is causally inverted: %v", accounting.GetTurnId(), timing)
	}
	assertUsageBoundary(t, accounting.GetUsageAtStart(), accounting.GetTurnId(), accounting.GetQueryInstanceId(), true, timing.GetPromptAdmittedAtMs())
	assertUsageBoundary(t, accounting.GetUsageAtEnd(), accounting.GetTurnId(), accounting.GetQueryInstanceId(), false, timing.GetResultReceivedAtMs())
	for _, response := range accounting.GetResponses() {
		if response.GetAgentReplSessionId() != sessionID {
			t.Errorf("turn %q response %q session id = %q, want %q", accounting.GetTurnId(), response.GetApiMessageId(), response.GetAgentReplSessionId(), sessionID)
		}
		if response.GetRootTurnId() != accounting.GetTurnId() {
			t.Errorf("turn %q response %q root_turn_id = %q", accounting.GetTurnId(), response.GetApiMessageId(), response.GetRootTurnId())
		}
	}
}

func assertUsageBoundary(t *testing.T, observation *corev1.AccountUsageObservation, turnID, queryID string, start bool, turnBoundary int64) {
	t.Helper()
	if observation == nil {
		t.Fatalf("turn %q has no %s account-usage observation", turnID, boundaryName(start))
	}
	if observation.GetTurnId() != turnID || observation.GetQueryInstanceId() != queryID {
		t.Fatalf("%s usage observation belongs to turn=%q query=%q, want turn=%q query=%q", boundaryName(start), observation.GetTurnId(), observation.GetQueryInstanceId(), turnID, queryID)
	}
	if start && observation.GetTurnStart() == nil || !start && observation.GetTurnEnd() == nil {
		t.Fatalf("turn %q account-usage observation has wrong boundary arm: %T", turnID, observation.GetBoundary())
	}
	if observation.GetAvailable() == nil || observation.GetAvailable().GetFiveHour() == nil {
		t.Fatalf("turn %q %s account usage is unavailable: %v", turnID, boundaryName(start), observation.GetUnavailable())
	}
	if observation.GetObservedAtMs() < observation.GetBoundaryAtMs() || observation.GetSampleLatencyMs() < 0 {
		t.Fatalf("turn %q %s observation has impossible sampling times: %v", turnID, boundaryName(start), observation)
	}
	if start && observation.GetBoundaryAtMs() > turnBoundary {
		t.Fatalf("turn %q start usage boundary followed prompt admission: boundary=%d admitted=%d", turnID, observation.GetBoundaryAtMs(), turnBoundary)
	}
	if !start && observation.GetBoundaryAtMs() < turnBoundary {
		t.Fatalf("turn %q end usage boundary predates terminal result: boundary=%d result=%d", turnID, observation.GetBoundaryAtMs(), turnBoundary)
	}
}

func boundaryName(start bool) string {
	if start {
		return "turn-start"
	}
	return "turn-end"
}

func assertCompleteRawUsage(t *testing.T, actor string, response *frontendv1.TokenUtilization, wantRaw *datav1.ApiUsage) {
	t.Helper()
	usage := response.GetUsage()
	if usage == nil {
		t.Fatalf("%s response %q omitted raw usage", actor, response.GetApiMessageId())
	}
	if usage.GetInputTokens() <= 0 || usage.GetOutputTokens() <= 0 || usage.GetCacheReadInputTokens() <= 0 || usage.GetCacheCreationInputTokens() <= 0 {
		t.Fatalf("%s response %q omitted a nonzero raw token counter: %v", actor, response.GetApiMessageId(), usage)
	}
	if usage.GetCacheCreation() == nil || usage.GetCacheCreation().GetEphemeral_5MInputTokens() <= 0 || usage.GetCacheCreation().GetEphemeral_1HInputTokens() <= 0 {
		t.Fatalf("%s response %q omitted cache-creation TTL usage: %v", actor, response.GetApiMessageId(), usage.GetCacheCreation())
	}
	if usage.GetServerToolUse() == nil || usage.GetServerToolUse().GetWebSearchRequests() <= 0 || usage.GetServerToolUse().GetWebFetchRequests() <= 0 {
		t.Fatalf("%s response %q omitted server-tool usage: %v", actor, response.GetApiMessageId(), usage.GetServerToolUse())
	}
	if usage.GetServiceTier() == "" || usage.GetSpeed() == "" || usage.GetInferenceGeo() == "" || usage.GetOutputDetails() == nil || usage.GetOutputDetails().GetThinkingTokens() <= 0 || usage.GetCacheRates() == nil {
		t.Fatalf("%s response %q omitted tier, speed, geography, nonzero output details, or cache rates: %v", actor, response.GetApiMessageId(), usage)
	}
	if usage.GetFallbackCredit() == nil || len(usage.GetFallbackCredit().GetFields()) == 0 {
		t.Fatalf("%s response %q omitted nonempty fallback-credit API usage: %v", actor, response.GetApiMessageId(), usage.GetFallbackCredit())
	}
	if usage.GetUnmodeledUsage() == nil || len(usage.GetUnmodeledUsage().GetFields()) == 0 {
		t.Fatalf("%s response %q omitted nonempty unmodeled API usage: %v", actor, response.GetApiMessageId(), usage.GetUnmodeledUsage())
	}
	if usage.GetCacheDiagnostic() == nil || usage.GetCacheDiagnostic().GetReason() == nil {
		t.Fatalf("%s response %q omitted cache-miss diagnostic: %v", actor, response.GetApiMessageId(), usage.GetCacheDiagnostic())
	}
	if !proto.Equal(usage.GetRawUsage(), wantRaw) {
		t.Fatalf("%s response %q raw usage differs from the SDK payload\nwant=%v\ngot=%v", actor, response.GetApiMessageId(), wantRaw, usage.GetRawUsage())
	}
	if actor == "main-agent" && (response.GetResponseTiming() == nil || response.GetResponseTiming().OutputGenerationDurationMs == nil) {
		t.Fatalf("%s response %q omitted response timing needed for output rate", actor, response.GetApiMessageId())
	}
}

// rawUsageFixture is the API usage object the fake SDK emits for each actor.
// Distinct nested values make total-fidelity assertions detect a field drop,
// accidental normalization, actor crossover, or shallow copy.
func rawUsageFixture(t *testing.T, actor string) *datav1.ApiUsage {
	t.Helper()
	actorOffset := int64(0)
	if actor == "subagent" {
		actorOffset = 100
	}
	cacheCreationFields := map[string]any{
		"ephemeral_5m_input_tokens": 41 + actorOffset,
		"ephemeral_1h_input_tokens": 42 + actorOffset,
	}
	serverToolUseFields := map[string]any{
		"web_search_requests": 51 + actorOffset,
		"web_fetch_requests":  52 + actorOffset,
	}
	iterationFields := map[string]any{
		"type":                        "sampling",
		"model":                       actor + "-reasoning-model",
		"input_tokens":                61 + actorOffset,
		"output_tokens":               62 + actorOffset,
		"cache_read_input_tokens":     63 + actorOffset,
		"cache_creation_input_tokens": 64 + actorOffset,
		"cache_creation": map[string]any{
			"ephemeral_5m_input_tokens": 65 + actorOffset,
			"ephemeral_1h_input_tokens": 66 + actorOffset,
		},
	}
	outputDetailsFields := map[string]any{"reasoning_tokens": 71 + actorOffset, "reasoning_model": actor + "-reasoning-model"}
	cacheDiagnosticFields := map[string]any{"reason": "model_changed", "cache_missed_input_tokens": 72 + actorOffset}
	fallbackCreditFields := map[string]any{"status": map[string]any{"type": "redeemed", "actor": actor}, "credits": 73 + actorOffset}
	unmodeledUsageFields := map[string]any{"future_counter": 74 + actorOffset, "nested": map[string]any{"actor": actor}}
	rawSDKUsage := mustStruct(t, map[string]any{
		"input_tokens":                11 + actorOffset,
		"output_tokens":               12 + actorOffset,
		"cache_read_input_tokens":     13 + actorOffset,
		"cache_creation_input_tokens": 14 + actorOffset,
		"cache_creation":              cacheCreationFields,
		"server_tool_use":             serverToolUseFields,
		"service_tier":                "priority",
		"speed":                       "fast",
		"inference_geo":               "us-east-1",
		"iterations":                  []any{iterationFields},
		"output_tokens_details":       outputDetailsFields,
		"cache_diagnostic":            cacheDiagnosticFields,
		"fallback_credit":             fallbackCreditFields,
		"unmodeled_usage":             unmodeledUsageFields,
	})
	return &datav1.ApiUsage{
		InputTokens:              11 + actorOffset,
		OutputTokens:             12 + actorOffset,
		CacheReadInputTokens:     13 + actorOffset,
		CacheCreationInputTokens: 14 + actorOffset,
		CacheCreation:            mustStruct(t, cacheCreationFields),
		ServerToolUse:            mustStruct(t, serverToolUseFields),
		ServiceTier:              "priority",
		Speed:                    "fast",
		InferenceGeo:             "us-east-1",
		Iterations:               mustList(t, []any{iterationFields}),
		OutputTokensDetails:      mustStruct(t, outputDetailsFields),
		CacheDiagnostic:          mustStruct(t, cacheDiagnosticFields),
		FallbackCredit:           mustStruct(t, fallbackCreditFields),
		UnmodeledUsage:           mustStruct(t, unmodeledUsageFields),
		RawSdkUsage:              rawSDKUsage,
	}
}

func mustStruct(t *testing.T, fields map[string]any) *structpb.Struct {
	t.Helper()
	value, err := structpb.NewStruct(fields)
	if err != nil {
		t.Fatalf("build raw-usage struct fixture: %v", err)
	}
	return value
}

func mustList(t *testing.T, values []any) *structpb.ListValue {
	t.Helper()
	value, err := structpb.NewList(values)
	if err != nil {
		t.Fatalf("build raw-usage list fixture: %v", err)
	}
	return value
}

func assertDurableQueryLifecycle(t *testing.T, events []*corev1.Event, queryID string) {
	t.Helper()
	created, runtimeObserved, terminated := 0, 0, 0
	for _, event := range events {
		lifecycle := event.GetQueryLifecycle()
		if lifecycle == nil || lifecycle.GetQueryInstanceId() != queryID {
			continue
		}
		switch {
		case lifecycle.GetCreated() != nil:
			created++
		case lifecycle.GetRuntimeObserved() != nil:
			runtimeObserved++
		case lifecycle.GetTerminated() != nil:
			terminated++
		}
	}
	if created != 1 || runtimeObserved != 1 || terminated != 0 {
		t.Fatalf("durable lifecycle for live query %q has created=%d runtime_observed=%d terminated=%d, want exactly 1/1/0", queryID, created, runtimeObserved, terminated)
	}
}

func assertStateDBResponseLedger(t *testing.T, h *e2eHarness, sessionID string, accountings ...*frontendv1.TurnAccounting) {
	t.Helper()
	ledger, err := statedb.NewTokenUtilizations(h.stateDB)
	if err != nil {
		t.Fatalf("open durable token-utilization ledger: %v", err)
	}
	records, err := ledger.List(sessionID)
	if err != nil {
		t.Fatalf("list durable token-utilization ledger: %v", err)
	}
	want := map[string]bool{}
	for _, accounting := range accountings {
		for _, response := range accounting.GetResponses() {
			want[response.GetApiMessageId()] = true
		}
	}
	if len(records) != len(want) {
		t.Fatalf("durable token-utilization records = %d, want %d response-ledger records", len(records), len(want))
	}
	for _, record := range records {
		if !want[record.GetApiMessageId()] {
			t.Errorf("durable token-utilization ledger contains unexpected API message %q", record.GetApiMessageId())
		}
	}
}

// replayStoreEvents returns the persisted vendor-session event stream. A read
// deadline is a bounded observation of the standing subscription's idle tail,
// not an ordering delay: every event returned before timeout was durably
// committed by shim-store before delivery.
func replayStoreEvents(t *testing.T, vendorSessionID string) []*corev1.Event {
	t.Helper()
	conn, err := net.Dial("unix", storeSocket(t))
	if err != nil {
		t.Fatalf("dial store replay: %v", err)
	}
	defer conn.Close()
	if err := wire.WriteAny(conn, &corev1.Subscribe{SessionId: vendorSessionID}); err != nil {
		t.Fatalf("subscribe to store replay: %v", err)
	}
	if err := conn.SetReadDeadline(time.Now().Add(time.Second)); err != nil {
		t.Fatalf("set store replay deadline: %v", err)
	}
	var events []*corev1.Event
	for {
		message, err := wire.ReadAny(conn)
		if err != nil {
			t.Fatalf("read store replay readiness: %v", err)
		}
		switch frame := message.(type) {
		case *corev1.Event:
			// The store emits its persisted replay before the readiness marker.
			// Keep that replay private until the marker proves the live tail is
			// registered, so callers never observe a partial subscription.
			events = append(events, frame)
		case *corev1.Heartbeat:
			if frame.GetSentAtMs() <= 0 {
				t.Fatalf("store replay readiness heartbeat sent_at_ms = %d, want a positive timestamp", frame.GetSentAtMs())
			}
			goto subscribed
		default:
			t.Fatalf("store replay readiness message = %T, want *corev1.Event or *corev1.Heartbeat", message)
		}
	}

subscribed:
	for {
		message, err := wire.ReadAny(conn)
		if err != nil {
			var netErr net.Error
			if errors.As(err, &netErr) && netErr.Timeout() {
				return events
			}
			t.Fatalf("read store replay: %v", err)
		}
		event, ok := message.(*corev1.Event)
		if !ok {
			t.Fatalf("store replay message = %T, want *corev1.Event", message)
		}
		events = append(events, event)
	}
}
