package tokenutilization

import (
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
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

func TestValidateModelIdentityOwnsTheSharedModelInvariant(t *testing.T) {
	for _, tc := range []struct {
		name    string
		record  *frontendv1.TokenUtilization
		wantErr bool
	}{
		{name: "nil", wantErr: true},
		{name: "blank", record: &frontendv1.TokenUtilization{Model: ""}, wantErr: true},
		{name: "whitespace", record: &frontendv1.TokenUtilization{Model: " \t\n"}, wantErr: true},
		{name: "vendor model", record: &frontendv1.TokenUtilization{Model: "claude-opus"}},
		{name: "synthetic model", record: &frontendv1.TokenUtilization{Model: SyntheticModelIdentity}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			err := ValidateModelIdentity(tc.record)
			if (err != nil) != tc.wantErr {
				t.Fatalf("ValidateModelIdentity() error = %v, wantErr=%t", err, tc.wantErr)
			}
			if tc.record != nil && tc.wantErr {
				var modelError *ValidationError
				if !errors.As(err, &modelError) || modelError.FieldPath != "TokenUtilization.model" || modelError.Model != tc.record.GetModel() {
					t.Fatalf("ValidateModelIdentity() error = %#v, want structured model rejection", err)
				}
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
	// proto.Clone, never a struct-value copy: TokenUtilization embeds a
	// protoimpl.MessageState (a sync.Mutex), which go vet's copylocks check
	// forbids copying by value.
	usageConflict := proto.Clone(live).(*frontendv1.TokenUtilization)
	usageConflict.Usage = &frontendv1.TokenUsage{InputTokens: 2}
	if err := ValidateHistoricalAgainstLive(historical, usageConflict); err == nil || !strings.Contains(err.Error(), "payloads disagree") {
		t.Fatalf("usage conflict error = %v", err)
	}
	actorConflict := proto.Clone(live).(*frontendv1.TokenUtilization)
	actorConflict.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "other"}}
	if err := ValidateHistoricalAgainstLive(historical, actorConflict); err == nil || !strings.Contains(err.Error(), "agent_id disagree") {
		t.Fatalf("actor conflict error = %v", err)
	}
}

func int64Pointer(value int64) *int64 { return &value }

// CarriesSubagentAlias IS THE VALIDATOR'S OWN ADMISSION RULE, exported so a
// caller validating an increment against an already-validated set can tell
// whether the increment can change the verdict. Its answer must therefore
// track exactly which records ValidateSubagentTopology's loop looks at.
func TestCarriesSubagentAlias(t *testing.T) {
	subagent := func(agentID, parentToolUseID string) *frontendv1.TokenUtilization {
		record := validationRecord("turn")
		record.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{
			AgentId: agentID, ParentToolUseId: parentToolUseID, SubagentType: "explore",
		}}
		return record
	}
	tests := []struct {
		name    string
		records []*frontendv1.TokenUtilization
		want    bool
	}{
		{name: "no records", records: nil, want: false},
		{name: "nil record", records: []*frontendv1.TokenUtilization{nil}, want: false},
		{name: "main agent actor", records: []*frontendv1.TokenUtilization{validationRecord("turn")}, want: false},
		{name: "subagent naming neither alias", records: []*frontendv1.TokenUtilization{subagent("", "")}, want: false},
		{name: "subagent naming only an agent id", records: []*frontendv1.TokenUtilization{subagent("agent-a", "")}, want: true},
		{name: "subagent naming only a parent tool use id", records: []*frontendv1.TokenUtilization{subagent("", "tool-a")}, want: true},
		{name: "subagent bridging both aliases", records: []*frontendv1.TokenUtilization{subagent("agent-a", "tool-a")}, want: true},
		{name: "an aliasing record among unaliased ones", records: []*frontendv1.TokenUtilization{
			validationRecord("turn"), subagent("", ""), subagent("agent-a", ""),
		}, want: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange is the table row.

			// Act.
			got := CarriesSubagentAlias(tc.records)

			// Assert.
			if got != tc.want {
				t.Fatalf("CarriesSubagentAlias = %v, want %v", got, tc.want)
			}
		})
	}
}

// THE PREDICATE AND THE VALIDATOR MUST AGREE. A set the predicate says carries
// no alias cannot be rejected by the validator, because the validator's loop
// skips every record in it — which is exactly what licenses skipping the read
// of the persisted set in statedb.validateSubagentTopologyTx.
func TestUnaliasedRecordsAreAlwaysTopologyClean(t *testing.T) {
	// Arrange — a subagent naming neither alias, beside a main-agent record.
	unaliased := validationRecord("turn")
	unaliased.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{
		SubagentType: "explore", TaskDescription: "look around",
	}}
	records := []*frontendv1.TokenUtilization{validationRecord("turn"), unaliased}
	if CarriesSubagentAlias(records) {
		t.Fatal("the arranged records carry an alias; the case under test needs ones that do not")
	}

	// Act.
	err := ValidateSubagentTopology(records)

	// Assert.
	if err != nil {
		t.Fatalf("ValidateSubagentTopology over unaliased records = %v, want nil", err)
	}
}
