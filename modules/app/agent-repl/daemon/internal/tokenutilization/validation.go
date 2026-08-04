// Package tokenutilization owns the canonical completed-response evidence
// validation shared by the reducer and both durable accounting stores.
package tokenutilization

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
)

// Identity binds a completed vendor response to the one daemon session,
// vendor conversation, and admitted root turn that owns it.
type Identity struct {
	AgentReplSessionID string
	ClaudeSessionID    string
	RootTurnID         string
}

// ValidateHistoricalAgainstLive proves that a rootless transcript record is
// the partial view of one richer live response. Every fact the historical
// source carries must agree; only root-turn attribution, stream timing, an
// absent API request id, and missing subagent provenance may be enriched.
func ValidateHistoricalAgainstLive(historical, live *frontendv1.TokenUtilization) error {
	if err := ValidateHistorical(historical, Identity{}); err != nil {
		return fmt.Errorf("historical record: %w", err)
	}
	if err := Validate(live, Identity{
		AgentReplSessionID: historical.GetAgentReplSessionId(),
		ClaudeSessionID:    historical.GetClaudeSessionId(),
	}); err != nil {
		return fmt.Errorf("live record: %w", err)
	}
	if historical.GetApiMessageId() != live.GetApiMessageId() || historical.GetModel() != live.GetModel() || !proto.Equal(historical.GetUsage(), live.GetUsage()) {
		return fmt.Errorf("historical and live response payloads disagree")
	}
	if historical.ApiRequestId != nil && !SameOptionalAPIRequestID(historical, live) {
		return fmt.Errorf("historical and live api_request_id disagree")
	}
	if historical.GetMainAgent() != nil {
		if live.GetMainAgent() == nil {
			return fmt.Errorf("historical main-agent response became subagent usage")
		}
		return nil
	}
	historicalAgent, liveAgent := historical.GetSubagent(), live.GetSubagent()
	if liveAgent == nil {
		return fmt.Errorf("historical subagent response became main-agent usage")
	}
	for _, field := range []struct {
		name       string
		historical string
		live       string
	}{
		{name: "agent_id", historical: historicalAgent.GetAgentId(), live: liveAgent.GetAgentId()},
		{name: "parent_tool_use_id", historical: historicalAgent.GetParentToolUseId(), live: liveAgent.GetParentToolUseId()},
		{name: "parent_agent_id", historical: historicalAgent.GetParentAgentId(), live: liveAgent.GetParentAgentId()},
		{name: "subagent_type", historical: historicalAgent.GetSubagentType(), live: liveAgent.GetSubagentType()},
		{name: "task_description", historical: historicalAgent.GetTaskDescription(), live: liveAgent.GetTaskDescription()},
	} {
		if field.historical != "" && field.historical != field.live {
			return fmt.Errorf("historical and live %s disagree", field.name)
		}
	}
	return nil
}

// Validate rejects incomplete or misattributed billing evidence. API request
// presence is represented by the generated optional field and participates in
// exact duplicate identity.
func Validate(record *frontendv1.TokenUtilization, expected Identity) error {
	if err := validateCommon(record, expected); err != nil {
		return err
	}
	if record.GetRootTurnId() == "" {
		return fmt.Errorf("token utilization has blank root_turn_id")
	}
	return nil
}

// ValidateHistorical rejects incomplete file-plane response evidence without
// requiring an enclosing turn the transcript cannot prove. Historical records
// must keep both root-turn attribution and stream-derived timing absent.
func ValidateHistorical(record *frontendv1.TokenUtilization, expected Identity) error {
	if expected.RootTurnID != "" {
		return fmt.Errorf("historical token utilization expected identity has root_turn_id=%q", expected.RootTurnID)
	}
	if err := validateCommon(record, expected); err != nil {
		return err
	}
	if record.GetRootTurnId() != "" {
		return fmt.Errorf("historical token utilization has root_turn_id=%q", record.GetRootTurnId())
	}
	if record.GetResponseTiming() != nil {
		return fmt.Errorf("historical token utilization has response_timing")
	}
	return nil
}

func validateCommon(record *frontendv1.TokenUtilization, expected Identity) error {
	if record == nil {
		return fmt.Errorf("token utilization is nil")
	}
	if record.GetAgentReplSessionId() == "" {
		return fmt.Errorf("token utilization has blank agent_repl_session_id")
	}
	if record.GetClaudeSessionId() == "" {
		return fmt.Errorf("token utilization has blank claude_session_id")
	}
	if record.GetApiMessageId() == "" {
		return fmt.Errorf("token utilization has blank api_message_id")
	}
	if record.GetActor() == nil {
		return fmt.Errorf("token utilization has no actor")
	}
	if subagent := record.GetSubagent(); subagent != nil && !hasSubagentProvenance(subagent) {
		return fmt.Errorf("token utilization subagent actor has no provenance")
	}
	if record.GetUsage() == nil {
		return fmt.Errorf("token utilization has no usage")
	}
	if record.ApiRequestId != nil && *record.ApiRequestId == "" {
		return fmt.Errorf("token utilization has present but blank api_request_id")
	}
	if expected.AgentReplSessionID != "" && record.GetAgentReplSessionId() != expected.AgentReplSessionID {
		return fmt.Errorf("token utilization agent_repl_session_id=%q does not match expected %q", record.GetAgentReplSessionId(), expected.AgentReplSessionID)
	}
	if expected.ClaudeSessionID != "" && record.GetClaudeSessionId() != expected.ClaudeSessionID {
		return fmt.Errorf("token utilization claude_session_id=%q does not match expected %q", record.GetClaudeSessionId(), expected.ClaudeSessionID)
	}
	if expected.RootTurnID != "" && record.GetRootTurnId() != expected.RootTurnID {
		return fmt.Errorf("token utilization root_turn_id=%q does not match expected %q", record.GetRootTurnId(), expected.RootTurnID)
	}
	return nil
}

// SameOptionalAPIRequestID compares both protobuf presence and value. An
// omitted SDK request id is valid evidence and must not be conflated with a
// supplied id during replay.
func SameOptionalAPIRequestID(a, b *frontendv1.TokenUtilization) bool {
	if a == nil || b == nil {
		return a == b
	}
	if (a.ApiRequestId == nil) != (b.ApiRequestId == nil) {
		return false
	}
	return a.ApiRequestId == nil || *a.ApiRequestId == *b.ApiRequestId
}

// ValidateSubagentTopology rejects contradictory stable aliases before a
// response becomes durable. A record carrying both aliases is a bridge that
// joins prior agent-id-only and parent-tool-use-id-only observations.
func ValidateSubagentTopology(records []*frontendv1.TokenUtilization) error {
	aliases := map[string]*subagentTopology{}
	for _, record := range records {
		if record == nil || record.GetSubagent() == nil {
			continue
		}
		agent := record.GetSubagent()
		if agent.GetAgentId() == "" && agent.GetParentToolUseId() == "" {
			continue
		}
		var left, right *subagentTopology
		if id := agent.GetAgentId(); id != "" {
			left = findTopology(aliases["agent:"+id])
		}
		if id := agent.GetParentToolUseId(); id != "" {
			right = findTopology(aliases["tool:"+id])
		}
		node := left
		if node == nil {
			node = right
		}
		if node == nil {
			node = &subagentTopology{}
		}
		if left != nil && right != nil && left != right {
			if err := unionTopology(left, right); err != nil {
				return err
			}
			node = findTopology(left)
		}
		if err := mergeTopology(node, agent); err != nil {
			return err
		}
		if id := agent.GetAgentId(); id != "" {
			aliases["agent:"+id] = node
		}
		if id := agent.GetParentToolUseId(); id != "" {
			aliases["tool:"+id] = node
		}
	}
	return nil
}

type subagentTopology struct {
	parent                                  *subagentTopology
	agentID, parentToolUseID, parentAgentID string
	subagentType, taskDescription           string
}

func findTopology(node *subagentTopology) *subagentTopology {
	if node == nil || node.parent == nil {
		return node
	}
	node.parent = findTopology(node.parent)
	return node.parent
}

func unionTopology(left, right *subagentTopology) error {
	left, right = findTopology(left), findTopology(right)
	if left == right {
		return nil
	}
	if err := mergeTopology(left, &frontendv1.TokenUtilizationSubagent{AgentId: right.agentID, ParentToolUseId: right.parentToolUseID, ParentAgentId: right.parentAgentID, SubagentType: right.subagentType, TaskDescription: right.taskDescription}); err != nil {
		return err
	}
	right.parent = left
	return nil
}

func mergeTopology(node *subagentTopology, incoming *frontendv1.TokenUtilizationSubagent) error {
	node = findTopology(node)
	merge := func(field string, current *string, value string) error {
		if value == "" {
			return nil
		}
		if *current != "" && *current != value {
			return fmt.Errorf("inconsistent subagent topology field=%s prior=%q incoming=%q", field, *current, value)
		}
		*current = value
		return nil
	}
	if err := merge("agent_id", &node.agentID, incoming.GetAgentId()); err != nil {
		return err
	}
	if err := merge("parent_tool_use_id", &node.parentToolUseID, incoming.GetParentToolUseId()); err != nil {
		return err
	}
	if err := merge("parent_agent_id", &node.parentAgentID, incoming.GetParentAgentId()); err != nil {
		return err
	}
	if err := merge("subagent_type", &node.subagentType, incoming.GetSubagentType()); err != nil {
		return err
	}
	return merge("task_description", &node.taskDescription, incoming.GetTaskDescription())
}

func hasSubagentProvenance(agent *frontendv1.TokenUtilizationSubagent) bool {
	return agent != nil && (agent.GetAgentId() != "" || agent.GetParentToolUseId() != "" || agent.GetParentAgentId() != "" || agent.GetSubagentType() != "" || agent.GetTaskDescription() != "")
}
