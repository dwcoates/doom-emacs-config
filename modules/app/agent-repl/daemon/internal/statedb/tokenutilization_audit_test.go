package statedb

import (
	"database/sql"
	"os"
	"path/filepath"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
)

func auditUtilization(sessionID, messageID, model string) *frontendv1.TokenUtilization {
	return &frontendv1.TokenUtilization{AgentReplSessionId: sessionID, ClaudeSessionId: "claude-" + sessionID, RootTurnId: "turn-" + messageID, ApiMessageId: messageID, Model: model, Actor: &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}}, Usage: &frontendv1.VendorTokenUsage{OutputTokens: 1}}
}

func insertAuditUtilization(t *testing.T, db *sql.DB, record *frontendv1.TokenUtilization) {
	t.Helper()
	raw, err := proto.Marshal(record)
	if err != nil {
		t.Fatalf("marshal audit record: %v", err)
	}
	if _, err := db.Exec(`INSERT INTO token_utilization(agent_repl_session_id, api_message_id, record) VALUES (?,?,?)`, record.GetAgentReplSessionId(), record.GetApiMessageId(), raw); err != nil {
		t.Fatalf("insert audit record: %v", err)
	}
}

func TestAuditBlankModelTokenUtilizationsIsReadOnlyAndDeterministic(t *testing.T) {
	store, _ := openReceipts(t)
	if _, err := NewTokenUtilizations(store.db); err != nil {
		t.Fatal(err)
	}
	insertAuditUtilization(t, store.db, auditUtilization("session-b", "message-b", " \t"))
	insertAuditUtilization(t, store.db, auditUtilization("session-a", "message-z", ""))
	insertAuditUtilization(t, store.db, auditUtilization("session-a", "message-a", "opus"))
	got, err := AuditBlankModelTokenUtilizations(store.db)
	if err != nil {
		t.Fatalf("AuditBlankModelTokenUtilizations: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("invalid rows = %+v, want two", got)
	}
	if got[0].AgentReplSessionID != "session-a" || got[0].APIMessageID != "message-z" || got[0].Model != "" || got[1].AgentReplSessionID != "session-b" || got[1].APIMessageID != "message-b" || got[1].Model != " \t" {
		t.Fatalf("deterministic audit rows = %+v", got)
	}
	var count int
	if err := store.db.QueryRow(`SELECT COUNT(*) FROM token_utilization`).Scan(&count); err != nil {
		t.Fatal(err)
	}
	if count != 3 {
		t.Fatalf("read-only audit changed token_utilization row count to %d", count)
	}
}

func TestMigrateBlankModelTokenUtilizationsQuarantinesOnlyProvenRows(t *testing.T) {
	store, _ := openReceipts(t)
	if _, err := NewTokenUtilizations(store.db); err != nil {
		t.Fatal(err)
	}
	insertAuditUtilization(t, store.db, auditUtilization("session", "invalid", " "))
	insertAuditUtilization(t, store.db, auditUtilization("session", "valid", "sonnet"))
	report, err := MigrateBlankModelTokenUtilizations(store.db, TokenUtilizationAuditQuarantine)
	if err != nil {
		t.Fatalf("MigrateBlankModelTokenUtilizations quarantine: %v", err)
	}
	if report.Mutated != 1 || len(report.Invalid) != 1 || report.Invalid[0].APIMessageID != "invalid" {
		t.Fatalf("quarantine report = %+v", report)
	}
	var live, quarantined int
	if err := store.db.QueryRow(`SELECT COUNT(*) FROM token_utilization`).Scan(&live); err != nil {
		t.Fatal(err)
	}
	if err := store.db.QueryRow(`SELECT COUNT(*) FROM token_utilization_quarantine`).Scan(&quarantined); err != nil {
		t.Fatal(err)
	}
	if live != 1 || quarantined != 1 {
		t.Fatalf("live=%d quarantined=%d, want valid live row and one invalid quarantined row", live, quarantined)
	}
}

func TestMigrateBlankModelTokenUtilizationsDeletesOnlyWithExplicitAction(t *testing.T) {
	store, _ := openReceipts(t)
	if _, err := NewTokenUtilizations(store.db); err != nil {
		t.Fatal(err)
	}
	insertAuditUtilization(t, store.db, auditUtilization("session", "invalid", ""))
	report, err := MigrateBlankModelTokenUtilizations(store.db, TokenUtilizationAuditDelete)
	if err != nil {
		t.Fatalf("MigrateBlankModelTokenUtilizations delete: %v", err)
	}
	if report.Mutated != 1 || report.Action != TokenUtilizationAuditDelete {
		t.Fatalf("delete report = %+v", report)
	}
	var count int
	if err := store.db.QueryRow(`SELECT COUNT(*) FROM token_utilization`).Scan(&count); err != nil {
		t.Fatal(err)
	}
	if count != 0 {
		t.Fatalf("delete migration left %d rows", count)
	}
}

func TestMigrateBlankModelTokenUtilizationsRejectsInvalidActionBeforeMutation(t *testing.T) {
	store, _ := openReceipts(t)
	if _, err := NewTokenUtilizations(store.db); err != nil {
		t.Fatal(err)
	}
	insertAuditUtilization(t, store.db, auditUtilization("session", "invalid", ""))
	if _, err := MigrateBlankModelTokenUtilizations(store.db, "rename"); err == nil {
		t.Fatal("invalid migration action was accepted")
	}
	var count int
	if err := store.db.QueryRow(`SELECT COUNT(*) FROM token_utilization`).Scan(&count); err != nil {
		t.Fatal(err)
	}
	if count != 1 {
		t.Fatalf("invalid action mutated durable rows: %d", count)
	}
}

func TestMigrateBlankModelTokenUtilizationsRollsBackOnQuarantineError(t *testing.T) {
	store, _ := openReceipts(t)
	if _, err := NewTokenUtilizations(store.db); err != nil {
		t.Fatal(err)
	}
	invalid := auditUtilization("session", "invalid", "")
	insertAuditUtilization(t, store.db, invalid)
	if _, err := store.db.Exec(`CREATE TABLE token_utilization_quarantine (agent_repl_session_id TEXT NOT NULL, api_message_id TEXT NOT NULL, record BLOB NOT NULL, reason TEXT NOT NULL, PRIMARY KEY (agent_repl_session_id, api_message_id))`); err != nil {
		t.Fatal(err)
	}
	if _, err := store.db.Exec(`INSERT INTO token_utilization_quarantine(agent_repl_session_id, api_message_id, record, reason) VALUES (?,?,?,?)`, "session", "invalid", []byte("prior"), "prior"); err != nil {
		t.Fatal(err)
	}
	if _, err := MigrateBlankModelTokenUtilizations(store.db, TokenUtilizationAuditQuarantine); err == nil || !strings.Contains(err.Error(), "quarantine token utilization") {
		t.Fatalf("quarantine collision error = %v", err)
	}
	var live int
	if err := store.db.QueryRow(`SELECT COUNT(*) FROM token_utilization`).Scan(&live); err != nil {
		t.Fatal(err)
	}
	if live != 1 {
		t.Fatalf("failed quarantine partially mutated live rows: %d", live)
	}
}

func TestOpenExistingAndReadOnlyRefuseMissingDatabaseWithoutCreatingIt(t *testing.T) {
	path := filepath.Join(t.TempDir(), "missing", "state.db")
	for _, open := range []func(string) (*sql.DB, error){OpenReadOnly, OpenExisting} {
		if _, err := open(path); err == nil {
			t.Fatal("missing database was accepted")
		}
	}
	if _, err := os.Stat(filepath.Dir(path)); !os.IsNotExist(err) {
		t.Fatalf("missing database opener created directory: %v", err)
	}
}
