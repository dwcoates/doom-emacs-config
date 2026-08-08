package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/statedb"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"google.golang.org/protobuf/proto"
)

func commandDB(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "state.db")
	db, err := statedb.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = db.Close() })
	if _, err := statedb.NewTokenUtilizations(db); err != nil {
		t.Fatal(err)
	}
	record := &frontendv1.TokenUtilization{AgentReplSessionId: "session", ClaudeSessionId: "claude", RootTurnId: "turn", ApiMessageId: "message", Model: " \t", Actor: &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}}, Usage: &frontendv1.VendorTokenUsage{OutputTokens: 1}}
	raw, err := proto.Marshal(record)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := db.Exec(`INSERT INTO token_utilization(agent_repl_session_id, api_message_id, record) VALUES (?,?,?)`, "session", "message", raw); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestRunAuditIsReadOnlyAndReportsEveryRequiredIdentity(t *testing.T) {
	path := commandDB(t)
	var output bytes.Buffer
	if err := run([]string{"-db", path}, &output, nil); err != nil {
		t.Fatalf("run audit: %v", err)
	}
	want := "TOKEN UTILIZATION MODEL AUDIT outcome=\"read-only-audit\" action=\"audit\" field_path=\"TokenUtilization.model\" source_plane=\"durable-store\" candidate_count=1 mutated_count=0\ninvalid model identity: agent_repl_session_id=\"session\" api_message_id=\"message\" claude_session_id=\"claude\" root_turn_id=\"turn\" raw_model=\" \\t\" field_path=\"TokenUtilization.model\" source_plane=\"durable-store\"\n"
	if output.String() != want {
		t.Fatalf("audit output = %q, want %q", output.String(), want)
	}
	db, err := statedb.OpenReadOnly(path)
	if err != nil {
		t.Fatal(err)
	}
	defer db.Close()
	rows, err := statedb.AuditBlankModelTokenUtilizations(db)
	if err != nil || len(rows) != 1 {
		t.Fatalf("audit after command = %+v, %v", rows, err)
	}
}

func TestRunRequiresExplicitMutationSelectionAndConfirmation(t *testing.T) {
	path := commandDB(t)
	for _, args := range [][]string{{"-db", path, "-action", "quarantine"}, {"-db", path, "-apply"}, {"-db", path, "-action", "delete"}} {
		if err := run(args, &bytes.Buffer{}, nil); err == nil {
			t.Fatalf("run %v allowed a mutation without exact action and confirmation", args)
		}
	}
	var output bytes.Buffer
	if err := run([]string{"-db", path, "-action", "delete", "-apply"}, &output, nil); err != nil {
		t.Fatalf("run explicit delete: %v", err)
	}
	if !strings.Contains(output.String(), "outcome=\"migration-committed\" action=\"delete\"") || !strings.Contains(output.String(), "mutated_count=1") {
		t.Fatalf("delete report = %q", output.String())
	}
}

func TestRunMutationOnMissingPathLeavesNoResidue(t *testing.T) {
	path := filepath.Join(t.TempDir(), "missing", "state.db")
	err := run([]string{"-db", path, "-action", "delete", "-apply"}, &bytes.Buffer{}, nil)
	if err == nil {
		t.Fatal("missing database mutation was accepted")
	}
	if _, err := os.Stat(filepath.Dir(path)); !os.IsNotExist(err) {
		t.Fatalf("missing database mutation created a directory: %v", err)
	}
}

func TestReportAndLogCarriesAuditIdentityAndOutcome(t *testing.T) {
	var reportOutput, logOutput bytes.Buffer
	logger := dlog.New(&logOutput, &bytes.Buffer{}, false)
	report := statedb.TokenUtilizationModelAuditReport{
		Invalid: []statedb.TokenUtilizationModelAuditRow{{
			AgentReplSessionID: "session",
			APIMessageID:       "message",
			ClaudeSessionID:    "claude",
			RootTurnID:         "turn",
			Model:              " ",
		}},
	}
	if err := reportAndLog(&reportOutput, logger, report, "read-only-audit"); err != nil {
		t.Fatalf("reportAndLog: %v", err)
	}
	for _, want := range []string{"\"action\":\"audit\"", "\"field_path\":\"TokenUtilization.model\"", "\"source_plane\":\"durable-store\"", "\"candidate_count\":1", "\"mutated_count\":0", "\"outcome\":\"read-only-audit\"", "\"agent_repl_session_id\":\"session\"", "\"request_id\":\"message\"", "\"raw_model\":\" \""} {
		if !strings.Contains(logOutput.String(), want) {
			t.Fatalf("structured log %q omitted %q", logOutput.String(), want)
		}
	}
}
