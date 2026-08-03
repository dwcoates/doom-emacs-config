package frontend

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func ptr(v int64) *int64 { return &v }

func TestAggregateTokenUtilizationKeepsTimedCoverageSeparate(t *testing.T) {
	records := []*frontendv1.TokenUtilization{
		{Model: "fable", Actor: &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}}, Usage: &frontendv1.TokenUsage{OutputTokens: 5}, ResponseTiming: &frontendv1.TokenResponseTiming{TimeToFirstTokenMs: ptr(10), OutputGenerationDurationMs: ptr(50)}},
		{Model: "opus", Actor: &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "a"}}, Usage: &frontendv1.TokenUsage{OutputTokens: 7}},
	}
	got := AggregateTokenUtilization(records)
	if got.GetAllAgents().GetOutputTokens() != 12 || got.GetAllAgents().GetTiming().GetOutputTokensWithGenerationDuration() != 5 || got.GetAllAgents().GetTiming().GetResponsesWithoutGenerationDuration() != 1 {
		t.Fatalf("all totals = %+v", got.GetAllAgents())
	}
	if got.GetMainAgent().GetOutputTokens() != 5 || len(got.GetSubagents()) != 1 || len(got.GetModels()) != 2 {
		t.Fatalf("aggregate = %+v", got)
	}
}
