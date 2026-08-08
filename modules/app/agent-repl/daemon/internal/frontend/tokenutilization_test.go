package frontend

import (
	"errors"
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/tokenutilization"
	"google.golang.org/protobuf/proto"
)

func ptr(v int64) *int64 { return &v }

func TestAggregateTokenUtilizationKeepsTimedCoverageSeparate(t *testing.T) {
	records := []*frontendv1.TokenUtilization{
		{Model: "fable", Actor: &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 3, OutputTokens: 5, CacheReadInputTokens: 7, CacheCreationInputTokens: 11, CacheCreation: &frontendv1.TokenCacheCreation{Ephemeral_5MInputTokens: 11}, ServerToolUse: &frontendv1.TokenServerToolUse{WebSearchRequests: 2}, OutputDetails: &frontendv1.TokenOutputDetails{ThinkingTokens: 4}}, ResponseTiming: &frontendv1.TokenResponseTiming{TimeToFirstTokenMs: ptr(10), OutputGenerationDurationMs: ptr(50)}},
		{Model: "opus", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "a"}}, Usage: &frontendv1.VendorTokenUsage{OutputTokens: 7}},
	}
	got := AggregateTokenUtilization(records)
	if got.GetAllAgents().GetOutputTokens() != 12 || got.GetAllAgents().GetTiming().GetOutputTokensWithGenerationDuration() != 5 || got.GetAllAgents().GetTiming().GetResponsesWithoutGenerationDuration() != 1 {
		t.Fatalf("all totals = %+v", got.GetAllAgents())
	}
	if got.GetMainAgent().GetOutputTokens() != 5 || len(got.GetSubagents()) != 1 || len(got.GetModels()) != 2 {
		t.Fatalf("aggregate = %+v", got)
	}
	for _, model := range got.GetModels() {
		if model.CanonicalModel != nil || model.Provider != nil || model.ContextWindow != nil || model.MaxOutputTokens != nil || model.CostUsd != nil {
			t.Fatalf("response-only model aggregate fabricated unavailable metadata: %+v", model)
		}
	}
	if got.GetAllAgents().GetCacheCreation().GetEphemeral_5MInputTokens() != 11 || got.GetAllAgents().GetServerToolUse().GetWebSearchRequests() != 2 || got.GetAllAgents().GetOutputDetails().GetThinkingTokens() != 4 || got.GetAllAgents().GetCacheRates().GetTotalPromptInputTokens() != 21 {
		t.Fatalf("full aggregate = %+v", got.GetAllAgents())
	}
}

func TestAggregateTokenUtilizationTreatsMeasuredZeroTimingAsPresent(t *testing.T) {
	record := &frontendv1.TokenUtilization{
		Model: "fable",
		Actor: &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}},
		Usage: &frontendv1.VendorTokenUsage{OutputTokens: 5},
		ResponseTiming: &frontendv1.TokenResponseTiming{
			TimeToFirstTokenMs:         ptr(0),
			OutputGenerationDurationMs: ptr(0),
		},
	}
	timing := AggregateTokenUtilization([]*frontendv1.TokenUtilization{record}).GetAllAgents().GetTiming()
	if timing.GetOutputTokensWithGenerationDuration() != 5 || timing.GetResponsesWithGenerationDuration() != 1 || timing.GetResponsesWithoutGenerationDuration() != 0 || timing.GetResponsesWithTimeToFirstToken() != 1 || timing.GetResponsesWithoutTimeToFirstToken() != 0 {
		t.Fatalf("zero-valued measured timing = %+v, want present timing coverage", timing)
	}
}

func TestSetTokenUtilizationActorPreservesAllFiveFields(t *testing.T) {
	record := &frontendv1.TokenUtilization{}
	want := &frontendv1.TokenUtilizationSubagent{AgentId: "agent", ParentToolUseId: "tool", ParentAgentId: "parent", SubagentType: "research", TaskDescription: "inspect"}
	SetTokenUtilizationActor(record, &datav1.AssistantMessage{AgentId: want.AgentId, ParentToolUseId: want.ParentToolUseId, ParentAgentId: want.ParentAgentId, SubagentType: want.SubagentType, TaskDescription: want.TaskDescription})
	if !proto.Equal(record.GetSubagent(), want) {
		t.Fatalf("subagent = %+v, want %+v", record.GetSubagent(), want)
	}
}

func TestAggregateTokenUtilizationPreservesTaskDescriptionOnlyResponsesWithoutGrouping(t *testing.T) {
	records := []*frontendv1.TokenUtilization{
		{ApiMessageId: "m1", Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{TaskDescription: "inspect cache evidence"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 3}},
		{ApiMessageId: "m2", Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{TaskDescription: "inspect cache evidence"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 4}},
	}
	got := AggregateTokenUtilization(records)
	if len(got.GetSubagents()) != 0 || len(got.GetUngroupedSubagentResponses()) != 2 || got.GetUngroupedSubagentResponses()[0].GetApiMessageId() != "m1" || got.GetUngroupedSubagentResponses()[1].GetApiMessageId() != "m2" || got.GetAllAgents().GetInputTokens() != 7 || got.GetMainAgent().GetInputTokens() != 0 {
		t.Fatalf("aggregate = %+v", got)
	}
}

func TestAggregateTokenUtilizationJoinsStableIdentityEnrichmentAndBuildsAgentModels(t *testing.T) {
	records := []*frontendv1.TokenUtilization{
		{ApiMessageId: "m1", Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{ParentToolUseId: "tool"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 3}},
		{ApiMessageId: "m2", Model: "opus", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent", ParentToolUseId: "tool", ParentAgentId: "parent", TaskDescription: "inspect"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 4}},
		{ApiMessageId: "m3", Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 5}},
	}
	got := AggregateTokenUtilization(records)
	if len(got.GetSubagents()) != 1 {
		t.Fatalf("subagents = %+v", got.GetSubagents())
	}
	agent := got.GetSubagents()[0]
	if agent.GetAgent().GetAgentId() != "agent" || agent.GetAgent().GetParentToolUseId() != "tool" || agent.GetAgent().GetParentAgentId() != "parent" || agent.GetTotals().GetInputTokens() != 12 {
		t.Fatalf("agent aggregate = %+v", agent)
	}
	if len(agent.GetModels()) != 2 || agent.GetModels()[0].GetModel() != "fable" || agent.GetModels()[0].GetTotals().GetInputTokens() != 8 || agent.GetModels()[1].GetModel() != "opus" || agent.GetModels()[1].GetTotals().GetInputTokens() != 4 {
		t.Fatalf("agent models = %+v", agent.GetModels())
	}
}

func TestAggregateTokenUtilizationBridgesPreviouslySeparateStableAliases(t *testing.T) {
	records := []*frontendv1.TokenUtilization{
		{ApiMessageId: "m1", Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 3}},
		{ApiMessageId: "m2", Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{ParentToolUseId: "tool"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 5}},
		{ApiMessageId: "m3", Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent", ParentToolUseId: "tool"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 7}},
	}
	got := AggregateTokenUtilization(records)
	if len(got.GetSubagents()) != 1 || got.GetSubagents()[0].GetTotals().GetInputTokens() != 15 || got.GetSubagents()[0].GetAgent().GetAgentId() != "agent" || got.GetSubagents()[0].GetAgent().GetParentToolUseId() != "tool" {
		t.Fatalf("bridge aggregate = %+v", got)
	}
}

func TestAggregateTokenUtilizationSortsStableSubagentIdentities(t *testing.T) {
	records := []*frontendv1.TokenUtilization{
		{Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{ParentToolUseId: "tool-z"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 1}},
		{Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent-a"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 1}},
	}
	got := AggregateTokenUtilization(records)
	if len(got.GetSubagents()) != 2 || got.GetSubagents()[0].GetAgent().GetAgentId() != "agent-a" || got.GetSubagents()[1].GetAgent().GetParentToolUseId() != "tool-z" {
		t.Fatalf("sorted subagents = %+v", got.GetSubagents())
	}
}

func TestAggregateTokenUtilizationDoesNotPanicOnCorruptStableIdentityCollision(t *testing.T) {
	got := AggregateTokenUtilization([]*frontendv1.TokenUtilization{
		{Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent-a", ParentToolUseId: "tool-a"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 1}},
		{Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent-b", ParentToolUseId: "tool-b"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 1}},
		{Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent-a", ParentToolUseId: "tool-b"}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 1}},
	})
	if got == nil {
		t.Fatal("corrupt topology produced nil aggregate")
	}
}

func TestAggregateTokenUtilizationPreservesEmptySubagentIdentityAsUngrouped(t *testing.T) {
	got := AggregateTokenUtilization([]*frontendv1.TokenUtilization{{Model: "fable", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{}}, Usage: &frontendv1.VendorTokenUsage{InputTokens: 1}}})
	if len(got.GetUngroupedSubagentResponses()) != 1 {
		t.Fatalf("ungrouped responses = %+v", got.GetUngroupedSubagentResponses())
	}
}

func TestAggregateTokenUtilizationRejectsBlankModelBeforeAllocatingAggregate(t *testing.T) {
	for name, model := range map[string]string{"empty": "", "whitespace": " \t "} {
		t.Run(name, func(t *testing.T) {
			record := &frontendv1.TokenUtilization{
				AgentReplSessionId: "daemon-session",
				ClaudeSessionId:    "claude-session",
				ApiMessageId:       "api-message",
				Model:              model,
				Usage:              &frontendv1.VendorTokenUsage{InputTokens: 1},
			}
			err := ValidateTokenUtilizationAggregation([]*frontendv1.TokenUtilization{record})
			var invariant *TokenUtilizationAggregationInvariantError
			if !errors.As(err, &invariant) {
				t.Fatalf("validation error = %v, want aggregation invariant", err)
			}
			var modelError *tokenutilization.ValidationError
			if !errors.As(err, &modelError) {
				t.Fatalf("validation error = %v, want shared model identity cause", err)
			}
			if invariant.RecordIndex != 0 || invariant.ModelError != modelError || modelError.FieldPath != "TokenUtilization.model" || modelError.AgentReplSessionID != "daemon-session" || modelError.ClaudeSessionID != "claude-session" || modelError.APIMessageID != "api-message" || modelError.Model != model {
				t.Fatalf("invariant = %+v", invariant)
			}
			defer func() {
				if recovered := recover(); recovered == nil {
					t.Fatal("aggregate accepted blank model")
				} else if aggregateErr, ok := recovered.(*TokenUtilizationAggregationInvariantError); !ok || aggregateErr.Error() != invariant.Error() {
					t.Fatalf("aggregate panic = %#v, want the same aggregation invariant", recovered)
				}
			}()
			AggregateTokenUtilization([]*frontendv1.TokenUtilization{record})
		})
	}
}

func TestCacheRatesFromCountersIsTheAggregateAuthority(t *testing.T) {
	got := CacheRatesFromCounters(10, 30, 60)
	if got.GetTotalPromptInputTokens() != 100 || got.GetCacheHitRate() != 0.3 || got.GetCacheWriteRate() != 0.6 || got.GetUncachedInputRate() != 0.1 {
		t.Fatalf("rates = %+v", got)
	}
	if CacheRatesFromCounters(0, 0, 0) != nil {
		t.Fatal("zero prompt total fabricated cache rates")
	}
}

// THE THREE RATES ARE A PARTITION, one per disjoint SDK bucket, so
// UncachedInputRate is the input_tokens bucket's share ALONE and the EXPENSIVE
// share is that plus CacheWriteRate. Pinning both facts is what stops a later
// "fix" from folding cache creation into UncachedInputRate, which would
// double-count it against CacheWriteRate and break every consumer that reads
// these as shares of one whole.
func TestCacheRatesPartitionTheBucketsAndLeaveTheExpensiveShareASum(t *testing.T) {
	// Arrange — every bucket nonzero, so no term can hide inside another.
	const input, read, creation int64 = 10, 50, 40

	// Act.
	got := CacheRatesFromCounters(input, read, creation)

	// Assert.
	if sum := got.GetUncachedInputRate() + got.GetCacheWriteRate() + got.GetCacheHitRate(); sum != 1 {
		t.Fatalf("the three bucket rates sum to %v, want exactly 1: they are a partition of the prompt input", sum)
	}
	if got.GetUncachedInputRate() != float64(input)/float64(input+read+creation) {
		t.Fatalf("uncached_input_rate = %v, want the input_tokens BUCKET's share %v", got.GetUncachedInputRate(), float64(input)/float64(input+read+creation))
	}
	wantExpensive := float64(input+creation) / float64(input+read+creation)
	if expensive := got.GetUncachedInputRate() + got.GetCacheWriteRate(); expensive != wantExpensive {
		t.Fatalf("uncached_input_rate + cache_write_rate = %v, want the uncached (expensive) share %v; reading uncached_input_rate alone as the cost understates it by the cache writes",
			expensive, wantExpensive)
	}
}
