package frontend

import (
	"errors"
	"fmt"
	"sort"

	datav1 "agentrepl/proto/agentshim/data/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	statev1 "agentrepl/proto/state/v1"

	"claude-repld/internal/tokenusage"
	"claude-repld/internal/tokenutilization"
)

// TokenUtilizationAggregationInvariantError identifies a completed-response
// record which cannot safely be projected into a frontend model aggregate.
// The reducer reports original evidence verbatim so the owning frame boundary
// can diagnose persisted corruption without inventing an identity.
type TokenUtilizationAggregationInvariantError struct {
	RecordIndex int
	ModelError  *tokenutilization.ValidationError
}

func (e *TokenUtilizationAggregationInvariantError) Error() string {
	return fmt.Sprintf("token utilization aggregation invariant violated field_path=%s record_index=%d agent_repl_session_id=%q claude_session_id=%q api_message_id=%q model=%q", e.ModelError.FieldPath, e.RecordIndex, e.ModelError.AgentReplSessionID, e.ModelError.ClaudeSessionID, e.ModelError.APIMessageID, e.ModelError.Model)
}

func (e *TokenUtilizationAggregationInvariantError) Unwrap() error {
	return e.ModelError
}

// ValidateTokenUtilizationAggregation validates every response which would
// contribute model totals. It deliberately accepts neither an empty model nor
// whitespace-only model, because the strict frontend wire decoder requires
// each ModelTokenUtilization entry to carry an identity.
func ValidateTokenUtilizationAggregation(records []*statev1.TokenUtilization) error {
	for index, record := range records {
		if record == nil || record.GetUsage() == nil {
			continue
		}
		if err := tokenutilization.ValidateModelIdentity(record); err != nil {
			var modelError *tokenutilization.ValidationError
			if !errors.As(err, &modelError) {
				panic(fmt.Sprintf("token utilization model validator returned an untyped error: %v", err))
			}
			return &TokenUtilizationAggregationInvariantError{RecordIndex: index, ModelError: modelError}
		}
	}
	return nil
}

// SetTokenUtilizationActor preserves the SDK's complete agent provenance and
// classifies a response as main-agent usage only when every subagent field is
// absent. Keeping the five-field decision here prevents response mapping and
// aggregation from drifting into different definitions of a subagent.
func SetTokenUtilizationActor(record *statev1.TokenUtilization, assistant *datav1.AssistantMessage) {
	if record == nil || assistant == nil {
		panic("token utilization actor requires a record and assistant message")
	}
	agent := &statev1.TokenUtilizationSubagent{
		AgentId:         assistant.GetAgentId(),
		ParentToolUseId: assistant.GetParentToolUseId(),
		ParentAgentId:   assistant.GetParentAgentId(),
		SubagentType:    assistant.GetSubagentType(),
		TaskDescription: assistant.GetTaskDescription(),
	}
	if !hasSubagentProvenance(agent) {
		record.Actor = &statev1.TokenUtilization_MainAgent{MainAgent: &statev1.TokenUtilizationMainAgent{}}
		return
	}
	record.Actor = &statev1.TokenUtilization_Subagent{Subagent: agent}
}

func hasSubagentProvenance(agent *statev1.TokenUtilizationSubagent) bool {
	return agent != nil && (agent.GetAgentId() != "" || agent.GetParentToolUseId() != "" || agent.GetParentAgentId() != "" || agent.GetSubagentType() != "" || agent.GetTaskDescription() != "")
}

// CacheRatesFromCounters fills the DURABLE-LEGACY TokenCacheRates field, and
// nothing else.
//
// IT IS NOT THE SYSTEM'S RATE DERIVATION. Rates are derived from the canonical
// TokenUsage at the point of use (tokenusage.DeriveRates); this function exists
// because TokenCacheRates is populated inside the two persisted record types
// and a row written by an earlier build must keep replaying byte-identically.
// The quotients are therefore reproduced EXACTLY as they have always been
// computed — including by the shim's retired PromptCacheRates, which used the
// same three counters and the same divisor — so removing the shim's copy left
// every durable row unchanged.
//
// THESE ARE BUCKET SHARES, NOT A COST MEASURE. The three partition the prompt
// input and sum to 1, one per disjoint bucket, so UncachedInputRate is the
// fresh bucket's share ALONE and the expensive share is UncachedInputRate +
// CacheWriteRate. Nothing here may be changed to fold cache creation into
// UncachedInputRate: that would double-count against CacheWriteRate, break the
// partition, and — because these values are durable — make every persisted row
// irreproducible.
func CacheRatesFromCounters(input, read, creation int64) *statev1.TokenCacheRates {
	total := input + read + creation
	if total == 0 {
		return nil
	}
	return &statev1.TokenCacheRates{
		TotalPromptInputTokens: total,
		CacheHitRate:           float64(read) / float64(total),
		CacheWriteRate:         float64(creation) / float64(total),
		UncachedInputRate:      float64(input) / float64(total),
	}
}

// AggregateTokenUtilization folds completed response records into the session
// and per-actor/model totals consumed by frontend views.
func AggregateTokenUtilization(records []*statev1.TokenUtilization) *statev1.SessionTokenUtilization {
	// Validate before allocating or accumulating so corrupt durable evidence
	// cannot leave a caller with a partial aggregate that could enter a frame.
	if err := ValidateTokenUtilizationAggregation(records); err != nil {
		panic(err)
	}
	out := &statev1.SessionTokenUtilization{AllAgents: &statev1.TokenUsageTotals{}, MainAgent: &statev1.TokenUsageTotals{}}
	agentIDs := map[string]*subagentAggregateGroup{}
	parentToolUseIDs := map[string]*subagentAggregateGroup{}
	var groups []*subagentAggregateGroup
	models := map[string]*statev1.ModelTokenUtilization{}
	for _, record := range records {
		if record == nil || record.GetUsage() == nil {
			continue
		}
		addTokenUsage(out.AllAgents, record)
		if agent := record.GetSubagent(); agent != nil {
			group := resolveSubagentGroup(agentIDs, parentToolUseIDs, agent)
			if group == nil {
				out.UngroupedSubagentResponses = append(out.UngroupedSubagentResponses, record)
			} else {
				if err := mergeSubagentProvenanceSafe(group.agent, agent); err != nil {
					out.UngroupedSubagentResponses = append(out.UngroupedSubagentResponses, record)
				} else {
					group.records = append(group.records, record)
					bindSubagentGroup(agentIDs, parentToolUseIDs, group)
					if !group.listed {
						groups = append(groups, group)
						group.listed = true
					}
				}
			}
		} else {
			addTokenUsage(out.MainAgent, record)
		}
		addModelUsageToMap(models, record)
	}
	subagents := make([]*statev1.AgentTokenUtilization, 0, len(groups))
	for _, group := range groups {
		if group.mergedInto != nil {
			continue
		}
		entry := &statev1.AgentTokenUtilization{Agent: group.agent, Totals: &statev1.TokenUsageTotals{}}
		for _, record := range group.records {
			addTokenUsage(entry.Totals, record)
			entry.Models = addModelUsage(entry.Models, record)
		}
		entry.Tokens = resolveCanonicalTokens(entry.Totals, "subagent totals")
		subagents = append(subagents, entry)
	}
	sort.Slice(subagents, func(i, j int) bool {
		return stableSubagentSortKey(subagents[i].GetAgent()) < stableSubagentSortKey(subagents[j].GetAgent())
	})
	out.Subagents = subagents
	out.Models = sortedModelUsage(models)
	// THE ECONOMICS ARE RESOLVED HERE, ONCE, so a frontend never re-partitions
	// the vendor buckets to learn what the session paid. This aggregate is
	// rebuilt from the durable records on every read and is itself never
	// persisted, which is exactly what lets it carry the canonical shape at all.
	out.AllAgentsTokens = resolveCanonicalTokens(out.AllAgents, "all-agent totals")
	out.MainAgentTokens = resolveCanonicalTokens(out.MainAgent, "main-agent totals")
	return out
}

// resolveCanonicalTokens converts one vendor total at the read boundary.
//
// A NEGATIVE SUM IS CORRUPTION AND FAILS LOUDLY, in the same manner as the
// aggregation invariant this function's caller already panics on: every
// contributing record was validated non-negative before it was made durable, so
// a negative total means the durable evidence itself is wrong, and a view built
// from it would report a session costing nearly 2^64 tokens.
func resolveCanonicalTokens(totals *statev1.TokenUsageTotals, what string) *conversationv1.TokenUsage {
	canonical, err := tokenusage.FromTotals(totals)
	if err != nil {
		panic(fmt.Sprintf("token utilization %s cannot be made canonical: %v", what, err))
	}
	return canonical
}

type subagentAggregateGroup struct {
	agent      *statev1.TokenUtilizationSubagent
	records    []*statev1.TokenUtilization
	mergedInto *subagentAggregateGroup
	listed     bool
}

func resolveSubagentGroup(agentIDs, parentToolUseIDs map[string]*subagentAggregateGroup, agent *statev1.TokenUtilizationSubagent) *subagentAggregateGroup {
	if !hasSubagentProvenance(agent) {
		return nil
	}
	if agent.GetAgentId() == "" && agent.GetParentToolUseId() == "" {
		return nil
	}
	byAgent, byTool := agentIDs[agent.GetAgentId()], parentToolUseIDs[agent.GetParentToolUseId()]
	if agent.GetAgentId() == "" {
		byAgent = nil
	}
	if agent.GetParentToolUseId() == "" {
		byTool = nil
	}
	if byAgent != nil && byTool != nil && byAgent != byTool {
		byAgent.records = append(byAgent.records, byTool.records...)
		byTool.records = nil
		byTool.mergedInto = byAgent
		for id, group := range agentIDs {
			if group == byTool {
				agentIDs[id] = byAgent
			}
		}
		for id, group := range parentToolUseIDs {
			if group == byTool {
				parentToolUseIDs[id] = byAgent
			}
		}
	}
	if byAgent != nil {
		return byAgent
	}
	if byTool != nil {
		return byTool
	}
	return &subagentAggregateGroup{agent: &statev1.TokenUtilizationSubagent{}}
}

func bindSubagentGroup(agentIDs, parentToolUseIDs map[string]*subagentAggregateGroup, group *subagentAggregateGroup) {
	if id := group.agent.GetAgentId(); id != "" {
		agentIDs[id] = group
	}
	if id := group.agent.GetParentToolUseId(); id != "" {
		parentToolUseIDs[id] = group
	}
}

func mergeSubagentProvenanceSafe(dst, src *statev1.TokenUtilizationSubagent) error {
	merge := func(field string, current *string, incoming string) error {
		if incoming == "" {
			return nil
		}
		if *current != "" && *current != incoming {
			return fmt.Errorf("subagent provenance conflict field=%s prior=%q incoming=%q", field, *current, incoming)
		}
		*current = incoming
		return nil
	}
	if dst == nil || src == nil {
		return fmt.Errorf("subagent provenance merge requires both records")
	}
	if err := merge("agent_id", &dst.AgentId, src.GetAgentId()); err != nil {
		return err
	}
	if err := merge("parent_tool_use_id", &dst.ParentToolUseId, src.GetParentToolUseId()); err != nil {
		return err
	}
	if err := merge("parent_agent_id", &dst.ParentAgentId, src.GetParentAgentId()); err != nil {
		return err
	}
	if err := merge("subagent_type", &dst.SubagentType, src.GetSubagentType()); err != nil {
		return err
	}
	return merge("task_description", &dst.TaskDescription, src.GetTaskDescription())
}

// resolveSubagentAggregate selects an invocation only from the SDK's stable
// identifiers. Descriptive provenance without either identifier is retained
// per response by the caller instead of being promoted into a false identity.
func resolveSubagentAggregate(agentIDs, parentToolUseIDs map[string]*statev1.AgentTokenUtilization, agent *statev1.TokenUtilizationSubagent) *statev1.AgentTokenUtilization {
	if !hasSubagentProvenance(agent) {
		panic("subagent token utilization requires provenance")
	}
	byAgent := agentIDs[agent.GetAgentId()]
	byTool := parentToolUseIDs[agent.GetParentToolUseId()]
	if agent.GetAgentId() == "" {
		byAgent = nil
	}
	if agent.GetParentToolUseId() == "" {
		byTool = nil
	}
	if byAgent != nil && byTool != nil && byAgent != byTool {
		panic(fmt.Sprintf("subagent identity collision agent_id=%q parent_tool_use_id=%q", agent.GetAgentId(), agent.GetParentToolUseId()))
	}
	if byAgent != nil {
		return byAgent
	}
	if byTool != nil {
		return byTool
	}
	if agent.GetAgentId() == "" && agent.GetParentToolUseId() == "" {
		return nil
	}
	return &statev1.AgentTokenUtilization{Agent: &statev1.TokenUtilizationSubagent{}}
}

func bindSubagentIdentity(agentIDs, parentToolUseIDs map[string]*statev1.AgentTokenUtilization, entry *statev1.AgentTokenUtilization) {
	agent := entry.GetAgent()
	if id := agent.GetAgentId(); id != "" {
		if prior := agentIDs[id]; prior != nil && prior != entry {
			panic(fmt.Sprintf("duplicate subagent agent_id %q", id))
		}
		agentIDs[id] = entry
	}
	if id := agent.GetParentToolUseId(); id != "" {
		if prior := parentToolUseIDs[id]; prior != nil && prior != entry {
			panic(fmt.Sprintf("duplicate subagent parent_tool_use_id %q", id))
		}
		parentToolUseIDs[id] = entry
	}
}

func mergeSubagentProvenance(dst, src *statev1.TokenUtilizationSubagent) {
	if dst == nil || src == nil {
		panic("subagent provenance merge requires both records")
	}
	merge := func(field string, current *string, incoming string) {
		if incoming == "" {
			return
		}
		if *current != "" && *current != incoming {
			panic(fmt.Sprintf("subagent provenance conflict field=%s prior=%q incoming=%q", field, *current, incoming))
		}
		*current = incoming
	}
	merge("agent_id", &dst.AgentId, src.GetAgentId())
	merge("parent_tool_use_id", &dst.ParentToolUseId, src.GetParentToolUseId())
	merge("parent_agent_id", &dst.ParentAgentId, src.GetParentAgentId())
	merge("subagent_type", &dst.SubagentType, src.GetSubagentType())
	merge("task_description", &dst.TaskDescription, src.GetTaskDescription())
}

func stableSubagentSortKey(agent *statev1.TokenUtilizationSubagent) string {
	if agent.GetAgentId() != "" {
		return "agent:" + agent.GetAgentId()
	}
	return "tool:" + agent.GetParentToolUseId()
}

func addModelUsage(models []*statev1.ModelTokenUtilization, record *statev1.TokenUtilization) []*statev1.ModelTokenUtilization {
	indexed := make(map[string]*statev1.ModelTokenUtilization, len(models)+1)
	for _, model := range models {
		indexed[model.GetModel()] = model
	}
	addModelUsageToMap(indexed, record)
	return sortedModelUsage(indexed)
}

func addModelUsageToMap(models map[string]*statev1.ModelTokenUtilization, record *statev1.TokenUtilization) {
	model := record.GetModel()
	entry := models[model]
	if entry == nil {
		entry = &statev1.ModelTokenUtilization{Model: model, Totals: &statev1.TokenUsageTotals{}}
		models[model] = entry
	}
	addTokenUsage(entry.Totals, record)
}

func sortedModelUsage(models map[string]*statev1.ModelTokenUtilization) []*statev1.ModelTokenUtilization {
	keys := make([]string, 0, len(models))
	for key := range models {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	out := make([]*statev1.ModelTokenUtilization, 0, len(keys))
	for _, key := range keys {
		out = append(out, models[key])
	}
	return out
}

func addTokenUsage(total *statev1.TokenUsageTotals, record *statev1.TokenUtilization) {
	u := record.GetUsage()
	total.InputTokens += u.GetInputTokens()
	total.OutputTokens += u.GetOutputTokens()
	total.CacheReadInputTokens += u.GetCacheReadInputTokens()
	total.CacheCreationInputTokens += u.GetCacheCreationInputTokens()
	if total.CacheCreation == nil {
		total.CacheCreation = &statev1.TokenCacheCreation{}
	}
	total.CacheCreation.Ephemeral_5MInputTokens += u.GetCacheCreation().GetEphemeral_5MInputTokens()
	total.CacheCreation.Ephemeral_1HInputTokens += u.GetCacheCreation().GetEphemeral_1HInputTokens()
	if total.ServerToolUse == nil {
		total.ServerToolUse = &statev1.TokenServerToolUse{}
	}
	total.ServerToolUse.WebSearchRequests += u.GetServerToolUse().GetWebSearchRequests()
	total.ServerToolUse.WebFetchRequests += u.GetServerToolUse().GetWebFetchRequests()
	if total.OutputDetails == nil {
		total.OutputDetails = &statev1.TokenOutputDetails{}
	}
	total.OutputDetails.ThinkingTokens += u.GetOutputDetails().GetThinkingTokens()
	total.CacheRates = CacheRatesFromCounters(total.InputTokens, total.CacheReadInputTokens, total.CacheCreationInputTokens)
	if total.Timing == nil {
		total.Timing = &statev1.TokenTimingTotals{}
	}
	if timing := record.GetResponseTiming(); timing != nil && timing.OutputGenerationDurationMs != nil {
		total.Timing.OutputTokensWithGenerationDuration += u.GetOutputTokens()
		total.Timing.OutputGenerationDurationMs += timing.GetOutputGenerationDurationMs()
		total.Timing.ResponsesWithGenerationDuration++
	} else {
		total.Timing.ResponsesWithoutGenerationDuration++
	}
	if timing := record.GetResponseTiming(); timing != nil && timing.TimeToFirstTokenMs != nil {
		total.Timing.TotalTimeToFirstTokenMs += timing.GetTimeToFirstTokenMs()
		total.Timing.ResponsesWithTimeToFirstToken++
	} else {
		total.Timing.ResponsesWithoutTimeToFirstToken++
	}
}
