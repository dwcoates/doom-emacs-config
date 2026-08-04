package statedb

import (
	"database/sql"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/tokenutilization"

	"google.golang.org/protobuf/proto"
)

// TokenUtilizations durably owns normalized API-response accounting. Live rows
// carry a committed root turn and may carry stream timing; historical
// file-plane rows are explicitly rootless and untimed. The response id is the
// vendor's stable identity, so observations converge onto one row.
type TokenUtilizations struct{ db *sql.DB }

// NewTokenUtilizations installs the response-accounting table on the daemon
// state store.
func NewTokenUtilizations(db *sql.DB) (*TokenUtilizations, error) {
	if db == nil {
		return nil, fmt.Errorf("statedb: NewTokenUtilizations needs an open state store")
	}
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS token_utilization (
			agent_repl_session_id TEXT NOT NULL,
			api_message_id        TEXT NOT NULL,
			record                BLOB NOT NULL,
			PRIMARY KEY (agent_repl_session_id, api_message_id)
		);
		CREATE INDEX IF NOT EXISTS token_utilization_session
			ON token_utilization(agent_repl_session_id);
	`); err != nil {
		return nil, fmt.Errorf("statedb: create token_utilization schema: %w", err)
	}
	return &TokenUtilizations{db: db}, nil
}

// Record atomically persists one completed response. A response is immutable
// billing evidence: an exact replay is accepted while every conflicting
// duplicate is rejected before the durable row can change.
func (s *TokenUtilizations) Record(in *frontendv1.TokenUtilization) (inserted bool, err error) {
	return s.record(in, false)
}

// RecordHistorical atomically persists one normalized file-plane response.
// Exact replay converges on the same row while a fabricated root turn, timing,
// or conflicting duplicate is rejected before durable state changes.
func (s *TokenUtilizations) RecordHistorical(in *frontendv1.TokenUtilization) (inserted bool, err error) {
	return s.record(in, true)
}

func (s *TokenUtilizations) record(in *frontendv1.TokenUtilization, historical bool) (inserted bool, err error) {
	validate := tokenutilization.Validate
	kind := "token utilization"
	if historical {
		validate = tokenutilization.ValidateHistorical
		kind = "historical token utilization"
	}
	if err := validate(in, tokenutilization.Identity{}); err != nil {
		return false, fmt.Errorf("statedb: reject %s: %w", kind, err)
	}
	tx, err := s.db.Begin()
	if err != nil {
		return false, fmt.Errorf("statedb: begin token utilization record: %w", err)
	}
	defer func() { _ = tx.Rollback() }()
	if err := validateSubagentTopologyTx(tx, in.GetAgentReplSessionId(), []*frontendv1.TokenUtilization{in}); err != nil {
		return false, fmt.Errorf("statedb: reject token utilization topology: %w", err)
	}
	var raw []byte
	err = tx.QueryRow(`SELECT record FROM token_utilization WHERE agent_repl_session_id=? AND api_message_id=?`, in.GetAgentReplSessionId(), in.GetApiMessageId()).Scan(&raw)
	if err == sql.ErrNoRows {
		raw, err = proto.Marshal(in)
		if err == nil {
			_, err = tx.Exec(`INSERT INTO token_utilization(agent_repl_session_id, api_message_id, record) VALUES (?,?,?)`, in.GetAgentReplSessionId(), in.GetApiMessageId(), raw)
		}
		if err != nil {
			return false, fmt.Errorf("statedb: insert token utilization %q: %w", in.GetApiMessageId(), err)
		}
		if err = tx.Commit(); err != nil {
			return false, fmt.Errorf("statedb: commit token utilization %q: %w", in.GetApiMessageId(), err)
		}
		return true, nil
	}
	if err != nil {
		return false, fmt.Errorf("statedb: read token utilization %q: %w", in.GetApiMessageId(), err)
	}
	var prior frontendv1.TokenUtilization
	if err := proto.Unmarshal(raw, &prior); err != nil {
		return false, fmt.Errorf("statedb: decode token utilization %q: %w", in.GetApiMessageId(), err)
	}
	if historical && prior.GetRootTurnId() != "" {
		if err := tokenutilization.ValidateHistoricalAgainstLive(in, &prior); err != nil {
			return false, fmt.Errorf("statedb: conflicting historical observation for live token utilization %q: %w", in.GetApiMessageId(), err)
		}
		if err = tx.Commit(); err != nil {
			return false, fmt.Errorf("statedb: commit converged historical token utilization %q: %w", in.GetApiMessageId(), err)
		}
		return false, nil
	}
	if err := validate(&prior, tokenutilization.Identity{
		AgentReplSessionID: in.GetAgentReplSessionId(),
		ClaudeSessionID:    in.GetClaudeSessionId(),
		RootTurnID:         in.GetRootTurnId(),
	}); err != nil {
		return false, fmt.Errorf("statedb: corrupt prior token utilization %q: %w", in.GetApiMessageId(), err)
	}
	if !proto.Equal(&prior, in) || !tokenutilization.SameOptionalAPIRequestID(&prior, in) {
		return false, fmt.Errorf("statedb: conflicting duplicate token utilization %q", in.GetApiMessageId())
	}
	if err = tx.Commit(); err != nil {
		return false, fmt.Errorf("statedb: commit replayed token utilization %q: %w", in.GetApiMessageId(), err)
	}
	return false, nil
}

func validateSubagentTopologyTx(tx *sql.Tx, sessionID string, incoming []*frontendv1.TokenUtilization) error {
	rows, err := tx.Query(`SELECT record FROM token_utilization WHERE agent_repl_session_id=? ORDER BY api_message_id`, sessionID)
	if err != nil {
		return fmt.Errorf("list persisted response topology: %w", err)
	}
	defer rows.Close()
	records := make([]*frontendv1.TokenUtilization, 0, len(incoming)+1)
	for rows.Next() {
		var raw []byte
		if err := rows.Scan(&raw); err != nil {
			return fmt.Errorf("scan persisted response topology: %w", err)
		}
		var record frontendv1.TokenUtilization
		if err := proto.Unmarshal(raw, &record); err != nil {
			return fmt.Errorf("decode persisted response topology: %w", err)
		}
		records = append(records, &record)
	}
	if err := rows.Err(); err != nil {
		return fmt.Errorf("iterate persisted response topology: %w", err)
	}
	records = append(records, incoming...)
	return tokenutilization.ValidateSubagentTopology(records)
}

// List returns every durable response record for one daemon session.
func (s *TokenUtilizations) List(sessionID string) ([]*frontendv1.TokenUtilization, error) {
	rows, err := s.db.Query(`SELECT record FROM token_utilization WHERE agent_repl_session_id=? ORDER BY api_message_id`, sessionID)
	if err != nil {
		return nil, fmt.Errorf("statedb: list token utilizations for %q: %w", sessionID, err)
	}
	defer rows.Close()
	var out []*frontendv1.TokenUtilization
	for rows.Next() {
		var raw []byte
		if err := rows.Scan(&raw); err != nil {
			return nil, fmt.Errorf("statedb: scan token utilization for %q: %w", sessionID, err)
		}
		var u frontendv1.TokenUtilization
		if err := proto.Unmarshal(raw, &u); err != nil {
			return nil, fmt.Errorf("statedb: decode token utilization for %q: %w", sessionID, err)
		}
		validate := tokenutilization.Validate
		if u.GetRootTurnId() == "" {
			validate = tokenutilization.ValidateHistorical
		}
		if err := validate(&u, tokenutilization.Identity{AgentReplSessionID: sessionID}); err != nil {
			return nil, fmt.Errorf("statedb: invalid persisted token utilization for %q: %w", sessionID, err)
		}
		out = append(out, &u)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate token utilizations for %q: %w", sessionID, err)
	}
	return out, nil
}
