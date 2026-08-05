package statedb

import (
	"bytes"
	"database/sql"
	"fmt"
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
)

// TokenUtilizationAuditAction is an operator-selected migration action.  The
// empty action is deliberately not valid for migration: audit is a separate,
// read-only operation.
type TokenUtilizationAuditAction string

const (
	TokenUtilizationAuditQuarantine TokenUtilizationAuditAction = "quarantine"
	TokenUtilizationAuditDelete     TokenUtilizationAuditAction = "delete"
)

// TokenUtilizationModelAuditRow identifies one durable record proven invalid
// because its model identity is blank after whitespace trimming.
type TokenUtilizationModelAuditRow struct {
	AgentReplSessionID string
	APIMessageID       string
	ClaudeSessionID    string
	RootTurnID         string
	Model              string
	raw                []byte
}

// TokenUtilizationModelAuditReport is the complete, deterministic operator
// result for one read-only audit or explicitly requested migration.
type TokenUtilizationModelAuditReport struct {
	Action  TokenUtilizationAuditAction
	Invalid []TokenUtilizationModelAuditRow
	Mutated int
}

// AuditBlankModelTokenUtilizations enumerates every stored response whose
// model is blank or whitespace-only.  It never starts a transaction and never
// creates schema, so callers can safely use a read-only SQLite connection.
func AuditBlankModelTokenUtilizations(db *sql.DB) ([]TokenUtilizationModelAuditRow, error) {
	if db == nil {
		return nil, fmt.Errorf("statedb: audit token utilization models needs an open state store")
	}
	return auditBlankModelTokenUtilizations(db)
}

// MigrateBlankModelTokenUtilizations performs exactly the named action for
// every row found invalid by the same audit.  It validates the complete target
// set before its first write and performs all writes in one transaction.
func MigrateBlankModelTokenUtilizations(db *sql.DB, action TokenUtilizationAuditAction) (TokenUtilizationModelAuditReport, error) {
	if db == nil {
		return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: migrate token utilization models needs an open state store")
	}
	if action != TokenUtilizationAuditQuarantine && action != TokenUtilizationAuditDelete {
		return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: token utilization migration action %q is invalid", action)
	}
	invalid, err := auditBlankModelTokenUtilizations(db)
	if err != nil {
		return TokenUtilizationModelAuditReport{}, err
	}
	report := TokenUtilizationModelAuditReport{Action: action, Invalid: invalid}
	if len(invalid) == 0 {
		return report, nil
	}
	tx, err := db.Begin()
	if err != nil {
		return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: begin token utilization %s migration: %w", action, err)
	}
	defer func() { _ = tx.Rollback() }()
	for _, row := range invalid {
		var current []byte
		err := tx.QueryRow(`SELECT record FROM token_utilization WHERE agent_repl_session_id=? AND api_message_id=?`, row.AgentReplSessionID, row.APIMessageID).Scan(&current)
		if err != nil {
			return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: revalidate token utilization %q/%q before %s: %w", row.AgentReplSessionID, row.APIMessageID, action, err)
		}
		if !bytes.Equal(current, row.raw) {
			return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: token utilization %q/%q changed after audit; aborting %s migration", row.AgentReplSessionID, row.APIMessageID, action)
		}
	}
	if action == TokenUtilizationAuditQuarantine {
		if _, err := tx.Exec(`
			CREATE TABLE IF NOT EXISTS token_utilization_quarantine (
				agent_repl_session_id TEXT NOT NULL,
				api_message_id        TEXT NOT NULL,
				record                BLOB NOT NULL,
				reason                TEXT NOT NULL,
				PRIMARY KEY (agent_repl_session_id, api_message_id)
			)`); err != nil {
			return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: create token utilization quarantine: %w", err)
		}
	}
	for _, row := range invalid {
		if action == TokenUtilizationAuditQuarantine {
			if _, err := tx.Exec(`INSERT INTO token_utilization_quarantine(agent_repl_session_id, api_message_id, record, reason) VALUES (?,?,?,?)`, row.AgentReplSessionID, row.APIMessageID, row.raw, "model identity is blank or whitespace-only"); err != nil {
				return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: quarantine token utilization %q/%q: %w", row.AgentReplSessionID, row.APIMessageID, err)
			}
		}
		result, err := tx.Exec(`DELETE FROM token_utilization WHERE agent_repl_session_id=? AND api_message_id=?`, row.AgentReplSessionID, row.APIMessageID)
		if err != nil {
			return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: delete token utilization %q/%q during %s: %w", row.AgentReplSessionID, row.APIMessageID, action, err)
		}
		count, err := result.RowsAffected()
		if err != nil || count != 1 {
			return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: delete token utilization %q/%q during %s affected %d rows: %w", row.AgentReplSessionID, row.APIMessageID, action, count, err)
		}
	}
	if err := tx.Commit(); err != nil {
		return TokenUtilizationModelAuditReport{}, fmt.Errorf("statedb: commit token utilization %s migration: %w", action, err)
	}
	report.Mutated = len(invalid)
	return report, nil
}

type queryer interface {
	Query(query string, args ...any) (*sql.Rows, error)
}

func auditBlankModelTokenUtilizations(db queryer) ([]TokenUtilizationModelAuditRow, error) {
	rows, err := db.Query(`SELECT agent_repl_session_id, api_message_id, record FROM token_utilization ORDER BY agent_repl_session_id, api_message_id`)
	if err != nil {
		return nil, fmt.Errorf("statedb: enumerate token utilization model audit: %w", err)
	}
	defer rows.Close()
	var invalid []TokenUtilizationModelAuditRow
	for rows.Next() {
		var row TokenUtilizationModelAuditRow
		if err := rows.Scan(&row.AgentReplSessionID, &row.APIMessageID, &row.raw); err != nil {
			return nil, fmt.Errorf("statedb: scan token utilization model audit: %w", err)
		}
		var record frontendv1.TokenUtilization
		if err := proto.Unmarshal(row.raw, &record); err != nil {
			return nil, fmt.Errorf("statedb: decode token utilization model audit %q/%q: %w", row.AgentReplSessionID, row.APIMessageID, err)
		}
		if strings.TrimSpace(record.GetModel()) != "" {
			continue
		}
		row.ClaudeSessionID = record.GetClaudeSessionId()
		row.RootTurnID = record.GetRootTurnId()
		row.Model = record.GetModel()
		invalid = append(invalid, row)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate token utilization model audit: %w", err)
	}
	return invalid, nil
}
