package tokenutilization

import (
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func validationRecord(rootTurnID string) *frontendv1.TokenUtilization {
	return &frontendv1.TokenUtilization{
		AgentReplSessionId: "session",
		ClaudeSessionId:    "claude",
		RootTurnId:         rootTurnID,
		ApiMessageId:       "message",
		Model:              "model",
		Actor:              &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}},
		Usage:              &frontendv1.TokenUsage{InputTokens: 1},
	}
}

func TestValidateRequiresNonblankModelForLiveAndHistoricalEvidence(t *testing.T) {
	identity := Identity{AgentReplSessionID: "session", ClaudeSessionID: "claude"}
	for _, tc := range []struct {
		name     string
		validate func(*frontendv1.TokenUtilization, Identity) error
		rootTurn string
		model    string
	}{
		{name: "live blank", validate: Validate, rootTurn: "turn", model: ""},
		{name: "live whitespace", validate: Validate, rootTurn: "turn", model: " \t\n"},
		{name: "historical blank", validate: ValidateHistorical, model: ""},
		{name: "historical whitespace", validate: ValidateHistorical, model: " \t\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			record := validationRecord(tc.rootTurn)
			record.Model = tc.model
			err := tc.validate(record, identity)
			var validationErr *ValidationError
			if !strings.Contains(err.Error(), "blank model") || !errors.As(err, &validationErr) || validationErr.FieldPath != "TokenUtilization.model" || validationErr.Model != record.GetModel() || validationErr.APIMessageID != record.GetApiMessageId() || validationErr.AgentReplSessionID != record.GetAgentReplSessionId() || validationErr.ClaudeSessionID != record.GetClaudeSessionId() {
				t.Fatalf("validation error = %#v, want structured model rejection", err)
			}
		})
	}
	for _, tc := range []struct {
		name     string
		validate func(*frontendv1.TokenUtilization, Identity) error
		rootTurn string
	}{
		{name: "live synthetic", validate: Validate, rootTurn: "turn"},
		{name: "historical synthetic", validate: ValidateHistorical},
	} {
		t.Run(tc.name, func(t *testing.T) {
			record := validationRecord(tc.rootTurn)
			record.Model = SyntheticModelIdentity
			if err := tc.validate(record, identity); err != nil {
				t.Fatalf("Validate synthetic model: %v", err)
			}
		})
	}
}

func TestValidateRequiresLiveRootTurn(t *testing.T) {
	record := validationRecord("")
	if err := Validate(record, Identity{AgentReplSessionID: "session", ClaudeSessionID: "claude"}); err == nil || !strings.Contains(err.Error(), "blank root_turn_id") {
		t.Fatalf("Validate error = %v, want blank root_turn_id rejection", err)
	}
}

func TestValidateHistoricalRequiresAbsentRootTurnAndTiming(t *testing.T) {
	record := validationRecord("")
	identity := Identity{AgentReplSessionID: "session", ClaudeSessionID: "claude"}
	if err := ValidateHistorical(record, identity); err != nil {
		t.Fatalf("ValidateHistorical untimed rootless record: %v", err)
	}
	record.RootTurnId = "invented"
	if err := ValidateHistorical(record, identity); err == nil || !strings.Contains(err.Error(), "has root_turn_id") {
		t.Fatalf("ValidateHistorical rooted error = %v", err)
	}
	record.RootTurnId = ""
	record.ResponseTiming = &frontendv1.TokenResponseTiming{}
	if err := ValidateHistorical(record, identity); err == nil || !strings.Contains(err.Error(), "has response_timing") {
		t.Fatalf("ValidateHistorical timed error = %v", err)
	}
}

func TestValidateHistoricalAgainstLiveAllowsOnlyProvenanceAndTimingEnrichment(t *testing.T) {
	historical := validationRecord("")
	historical.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent"}}
	live := validationRecord("turn")
	live.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent", ParentToolUseId: "tool"}}
	live.ResponseTiming = &frontendv1.TokenResponseTiming{OutputGenerationDurationMs: int64Pointer(0)}
	if err := ValidateHistoricalAgainstLive(historical, live); err != nil {
		t.Fatalf("compatible enrichment: %v", err)
	}
	conflict := *live
	conflict.Usage = &frontendv1.TokenUsage{InputTokens: 2}
	if err := ValidateHistoricalAgainstLive(historical, &conflict); err == nil || !strings.Contains(err.Error(), "payloads disagree") {
		t.Fatalf("usage conflict error = %v", err)
	}
	conflict = *live
	conflict.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "other"}}
	if err := ValidateHistoricalAgainstLive(historical, &conflict); err == nil || !strings.Contains(err.Error(), "agent_id disagree") {
		t.Fatalf("actor conflict error = %v", err)
	}
}

func int64Pointer(value int64) *int64 { return &value }
