package statedb

import (
	"database/sql"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
)

// TokenUtilizations durably owns completed API-response accounting.  The
// response id is the vendor's stable identity, so observations from the stream
// and transcript planes converge onto one row.
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

// Record atomically persists one completed response.  A duplicate can only
// enrich an earlier observation: missing timing and actor fields are filled,
// while a record that already carries those facts is never erased by a twin.
func (s *TokenUtilizations) Record(in *frontendv1.TokenUtilization) (inserted bool, err error) {
	if in == nil {
		return false, fmt.Errorf("statedb: cannot record a nil token utilization")
	}
	if in.GetAgentReplSessionId() == "" || in.GetApiMessageId() == "" {
		return false, fmt.Errorf("statedb: token utilization needs agent-repl session id and api message id")
	}
	tx, err := s.db.Begin()
	if err != nil {
		return false, fmt.Errorf("statedb: begin token utilization record: %w", err)
	}
	defer func() { _ = tx.Rollback() }()
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
	merged := mergeTokenUtilization(&prior, in)
	raw, err = proto.Marshal(merged)
	if err != nil {
		return false, fmt.Errorf("statedb: encode token utilization %q: %w", in.GetApiMessageId(), err)
	}
	if _, err = tx.Exec(`UPDATE token_utilization SET record=? WHERE agent_repl_session_id=? AND api_message_id=?`, raw, in.GetAgentReplSessionId(), in.GetApiMessageId()); err != nil {
		return false, fmt.Errorf("statedb: enrich token utilization %q: %w", in.GetApiMessageId(), err)
	}
	if err = tx.Commit(); err != nil {
		return false, fmt.Errorf("statedb: commit enriched token utilization %q: %w", in.GetApiMessageId(), err)
	}
	return false, nil
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
		out = append(out, &u)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate token utilizations for %q: %w", sessionID, err)
	}
	return out, nil
}

func mergeTokenUtilization(prior, incoming *frontendv1.TokenUtilization) *frontendv1.TokenUtilization {
	merged := proto.Clone(prior).(*frontendv1.TokenUtilization)
	if incomingTiming := incoming.GetResponseTiming(); incomingTiming != nil {
		if merged.GetResponseTiming() == nil {
			merged.ResponseTiming = proto.Clone(incomingTiming).(*frontendv1.TokenResponseTiming)
		} else {
			if merged.ResponseTiming.TimeToFirstTokenMs == nil {
				merged.ResponseTiming.TimeToFirstTokenMs = incomingTiming.TimeToFirstTokenMs
			}
			if merged.ResponseTiming.OutputGenerationDurationMs == nil {
				merged.ResponseTiming.OutputGenerationDurationMs = incomingTiming.OutputGenerationDurationMs
			}
		}
	}
	if merged.GetActor() == nil && incoming.GetActor() != nil {
		merged.Actor = incoming.GetActor()
	}
	if merged.GetModel() == "" {
		merged.Model = incoming.GetModel()
	}
	if merged.GetClaudeSessionId() == "" {
		merged.ClaudeSessionId = incoming.GetClaudeSessionId()
	}
	if merged.GetRootTurnId() == "" {
		merged.RootTurnId = incoming.GetRootTurnId()
	}
	if merged.GetApiRequestId() == "" {
		merged.ApiRequestId = incoming.GetApiRequestId()
	}
	return merged
}
