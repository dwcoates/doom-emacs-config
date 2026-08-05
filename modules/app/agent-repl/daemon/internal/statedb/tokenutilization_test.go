package statedb

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"google.golang.org/protobuf/proto"
)

func int64p(v int64) *int64 { return &v }

func completeUtilization(sessionID, claudeSessionID, turnID, messageID string) *frontendv1.TokenUtilization {
	return &frontendv1.TokenUtilization{
		AgentReplSessionId: sessionID,
		ClaudeSessionId:    claudeSessionID,
		RootTurnId:         turnID,
		ApiMessageId:       messageID,
		Model:              "model",
		Actor:              &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}},
		Usage:              &frontendv1.TokenUsage{OutputTokens: 4},
	}
}

func TestTokenUtilizationRejectsBlankModelBeforeDurableMutation(t *testing.T) {
	store, _ := openReceipts(t)
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatalf("NewTokenUtilizations: %v", err)
	}
	for _, tc := range []struct {
		name   string
		record func() *frontendv1.TokenUtilization
		write  func(*frontendv1.TokenUtilization) (bool, error)
	}{
		{name: "live blank", record: func() *frontendv1.TokenUtilization { return completeUtilization("s", "claude", "turn", "live-blank") }, write: utilizations.Record},
		{name: "live whitespace", record: func() *frontendv1.TokenUtilization {
			return completeUtilization("s", "claude", "turn", "live-whitespace")
		}, write: utilizations.Record},
		{name: "historical blank", record: func() *frontendv1.TokenUtilization { return completeUtilization("s", "claude", "", "historical-blank") }, write: utilizations.RecordHistorical},
		{name: "historical whitespace", record: func() *frontendv1.TokenUtilization {
			return completeUtilization("s", "claude", "", "historical-whitespace")
		}, write: utilizations.RecordHistorical},
	} {
		t.Run(tc.name, func(t *testing.T) {
			record := tc.record()
			if strings.Contains(tc.name, "whitespace") {
				record.Model = " \t\n"
			} else {
				record.Model = ""
			}
			if inserted, err := tc.write(record); err == nil || inserted || !strings.Contains(err.Error(), "blank model") {
				t.Fatalf("record = %v, %v, want rejected blank model", inserted, err)
			}
		})
	}
	rows, err := utilizations.List("s")
	if err != nil || len(rows) != 0 {
		t.Fatalf("blank model evidence mutated durable store: rows=%+v err=%v", rows, err)
	}
}

func TestTokenUtilizationAcceptsExactReplayAndRejectsConflictingDuplicate(t *testing.T) {
	store, _ := openReceipts(t)
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatalf("NewTokenUtilizations: %v", err)
	}
	base := completeUtilization("s", "claude", "turn", "m")
	base.ResponseTiming = &frontendv1.TokenResponseTiming{TimeToFirstTokenMs: int64p(20)}
	if inserted, err := utilizations.Record(base); err != nil || !inserted {
		t.Fatalf("first Record = %v, %v", inserted, err)
	}
	if inserted, err := utilizations.Record(base); err != nil || inserted {
		t.Fatalf("exact replay Record = %v, %v", inserted, err)
	}
	conflict := completeUtilization("s", "claude", "turn", "m")
	conflict.ResponseTiming = &frontendv1.TokenResponseTiming{OutputGenerationDurationMs: int64p(80)}
	if _, err := utilizations.Record(conflict); err == nil {
		t.Fatal("conflicting duplicate was accepted")
	}
	got, err := utilizations.List("s")
	if err != nil || len(got) != 1 {
		t.Fatalf("List = %v, %v", got, err)
	}
	if got[0].GetResponseTiming().GetTimeToFirstTokenMs() != 20 || got[0].GetResponseTiming().GetOutputGenerationDurationMs() != 0 || got[0].GetMainAgent() == nil {
		t.Fatalf("merged record = %+v", got[0])
	}
}

func TestTokenUtilizationRejectsIncompleteEvidenceBeforeMutation(t *testing.T) {
	store, _ := openReceipts(t)
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatalf("NewTokenUtilizations: %v", err)
	}
	if _, err := utilizations.Record(nil); err == nil {
		t.Fatal("nil record was accepted")
	}
	missingUsage := completeUtilization("s", "claude", "turn", "m")
	missingUsage.Usage = nil
	if _, err := utilizations.Record(missingUsage); err == nil {
		t.Fatal("record without usage was accepted")
	}
	rows, err := utilizations.List("s")
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if len(rows) != 0 {
		t.Fatalf("incomplete evidence mutated durable store: %+v", rows)
	}
}

func TestHistoricalTokenUtilizationAcceptsExactReplayAndRejectsInventedLiveEvidence(t *testing.T) {
	store, _ := openReceipts(t)
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatalf("NewTokenUtilizations: %v", err)
	}
	historical := completeUtilization("s", "claude", "", "historical")
	if inserted, err := utilizations.RecordHistorical(historical); err != nil || !inserted {
		t.Fatalf("first RecordHistorical = %v, %v", inserted, err)
	}
	if inserted, err := utilizations.RecordHistorical(historical); err != nil || inserted {
		t.Fatalf("exact replay RecordHistorical = %v, %v", inserted, err)
	}
	rooted := completeUtilization("s", "claude", "invented", "rooted")
	if _, err := utilizations.RecordHistorical(rooted); err == nil {
		t.Fatal("historical record with invented root turn was accepted")
	}
	timed := completeUtilization("s", "claude", "", "timed")
	timed.ResponseTiming = &frontendv1.TokenResponseTiming{}
	if _, err := utilizations.RecordHistorical(timed); err == nil {
		t.Fatal("historical record with invented response timing was accepted")
	}
	got, err := utilizations.List("s")
	if err != nil || len(got) != 1 || got[0].GetApiMessageId() != "historical" {
		t.Fatalf("historical List = %+v, %v", got, err)
	}
}

func TestHistoricalObservationConvergesWithRicherLiveRow(t *testing.T) {
	store, _ := openReceipts(t)
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatal(err)
	}
	live := completeUtilization("s", "claude", "turn", "message")
	live.ResponseTiming = &frontendv1.TokenResponseTiming{OutputGenerationDurationMs: int64p(0)}
	if inserted, err := utilizations.Record(live); err != nil || !inserted {
		t.Fatalf("Record live = %v, %v", inserted, err)
	}
	historical := completeUtilization("s", "claude", "", "message")
	if inserted, err := utilizations.RecordHistorical(historical); err != nil || inserted {
		t.Fatalf("RecordHistorical after live = %v, %v", inserted, err)
	}
	conflict := completeUtilization("s", "claude", "", "message")
	conflict.Usage.InputTokens++
	if _, err := utilizations.RecordHistorical(conflict); err == nil {
		t.Fatal("conflicting historical observation converged with live row")
	}
	got, err := utilizations.List("s")
	if err != nil || len(got) != 1 || !proto.Equal(got[0], live) {
		t.Fatalf("canonical live row = %+v, %v", got, err)
	}
}

func TestTokenUtilizationRejectsInconsistentSubagentAliasTopologyBeforeMutation(t *testing.T) {
	store, _ := openReceipts(t)
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatalf("NewTokenUtilizations: %v", err)
	}
	first := completeUtilization("s", "claude", "turn", "m1")
	first.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent-a", ParentToolUseId: "tool-a"}}
	second := completeUtilization("s", "claude", "turn", "m2")
	second.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent-b", ParentToolUseId: "tool-b"}}
	for _, record := range []*frontendv1.TokenUtilization{first, second} {
		if _, err := utilizations.Record(record); err != nil {
			t.Fatalf("Record %q: %v", record.GetApiMessageId(), err)
		}
	}
	bridge := completeUtilization("s", "claude", "turn", "m3")
	bridge.Actor = &frontendv1.TokenUtilization_Subagent{Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "agent-a", ParentToolUseId: "tool-b"}}
	if _, err := utilizations.Record(bridge); err == nil {
		t.Fatal("inconsistent alias bridge was accepted")
	}
}
