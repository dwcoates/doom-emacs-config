package frontend

import frontendv1 "agentrepl/proto/agentshim/frontend/v1"

// AggregateTokenUtilization folds completed response records into the session
// and per-actor/model totals consumed by frontend views.
func AggregateTokenUtilization(records []*frontendv1.TokenUtilization) *frontendv1.SessionTokenUtilization {
	out := &frontendv1.SessionTokenUtilization{AllAgents: &frontendv1.TokenUsageTotals{}, MainAgent: &frontendv1.TokenUsageTotals{}}
	subagents := map[string]*frontendv1.AgentTokenUtilization{}
	models := map[string]*frontendv1.ModelTokenUtilization{}
	for _, record := range records {
		if record == nil || record.GetUsage() == nil {
			continue
		}
		addTokenUsage(out.AllAgents, record)
		if agent := record.GetSubagent(); agent != nil {
			key := agent.GetAgentId()
			if key == "" {
				key = agent.GetParentToolUseId()
			}
			entry := subagents[key]
			if entry == nil {
				entry = &frontendv1.AgentTokenUtilization{Agent: agent, Totals: &frontendv1.TokenUsageTotals{}}
				subagents[key] = entry
			}
			addTokenUsage(entry.Totals, record)
		} else {
			addTokenUsage(out.MainAgent, record)
		}
		model := record.GetModel()
		entry := models[model]
		if entry == nil {
			entry = &frontendv1.ModelTokenUtilization{Model: model, Totals: &frontendv1.TokenUsageTotals{}}
			models[model] = entry
		}
		addTokenUsage(entry.Totals, record)
	}
	for _, entry := range subagents {
		out.Subagents = append(out.Subagents, entry)
	}
	for _, entry := range models {
		out.Models = append(out.Models, entry)
	}
	return out
}

func addTokenUsage(total *frontendv1.TokenUsageTotals, record *frontendv1.TokenUtilization) {
	u := record.GetUsage()
	total.InputTokens += u.GetInputTokens()
	total.OutputTokens += u.GetOutputTokens()
	total.CacheReadInputTokens += u.GetCacheReadInputTokens()
	total.CacheCreationInputTokens += u.GetCacheCreationInputTokens()
	if total.Timing == nil {
		total.Timing = &frontendv1.TokenTimingTotals{}
	}
	if timing := record.GetResponseTiming(); timing != nil && timing.GetOutputGenerationDurationMs() > 0 {
		total.Timing.OutputTokensWithGenerationDuration += u.GetOutputTokens()
		total.Timing.OutputGenerationDurationMs += timing.GetOutputGenerationDurationMs()
		total.Timing.ResponsesWithGenerationDuration++
	} else {
		total.Timing.ResponsesWithoutGenerationDuration++
	}
	if timing := record.GetResponseTiming(); timing != nil && timing.GetTimeToFirstTokenMs() > 0 {
		total.Timing.TotalTimeToFirstTokenMs += timing.GetTimeToFirstTokenMs()
		total.Timing.ResponsesWithTimeToFirstToken++
	} else {
		total.Timing.ResponsesWithoutTimeToFirstToken++
	}
}
