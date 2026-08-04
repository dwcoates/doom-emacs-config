package statedb

import (
	"database/sql"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/tokenutilization"

	"google.golang.org/protobuf/proto"
)

// TurnAccountings durably owns the terminal accounting evidence for each turn.
// A single transaction writes response records and the corresponding terminal
// accounting so a visible terminal result never names partial evidence.
type TurnAccountings struct{ db *sql.DB }

// NewTurnAccountings installs the accounting tables in the daemon state store.
func NewTurnAccountings(db *sql.DB) (*TurnAccountings, error) {
	if db == nil {
		return nil, fmt.Errorf("statedb: NewTurnAccountings needs an open state store")
	}
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS token_utilization (
			agent_repl_session_id TEXT NOT NULL,
			api_message_id        TEXT NOT NULL,
			record                BLOB NOT NULL,
			PRIMARY KEY (agent_repl_session_id, api_message_id)
		);
		CREATE TABLE IF NOT EXISTS turn_accounting (
			agent_repl_session_id TEXT NOT NULL,
			turn_id TEXT NOT NULL,
			record BLOB NOT NULL,
			PRIMARY KEY (agent_repl_session_id, turn_id)
		);
		CREATE INDEX IF NOT EXISTS turn_accounting_session
			ON turn_accounting(agent_repl_session_id);
	`); err != nil {
		return nil, fmt.Errorf("statedb: create turn_accounting schema: %w", err)
	}
	return &TurnAccountings{db: db}, nil
}

// Record persists response records and their resolved terminal accounting as
// one SQLite transaction. Replays overwrite only byte-identical accounting;
// a divergent duplicate is an invariant violation and fails loudly.
func (s *TurnAccountings) Record(sessionID string, accounting *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error) {
	if sessionID == "" || accounting == nil || accounting.GetTurnId() == "" {
		return nil, fmt.Errorf("statedb: turn accounting needs session id and turn id")
	}
	if err := validateTurnAccountingResponses(sessionID, accounting); err != nil {
		return nil, fmt.Errorf("statedb: reject turn accounting %q: %w", accounting.GetTurnId(), err)
	}
	raw, err := proto.Marshal(accounting)
	if err != nil {
		return nil, fmt.Errorf("statedb: encode turn accounting %q: %w", accounting.GetTurnId(), err)
	}
	tx, err := s.db.Begin()
	if err != nil {
		return nil, fmt.Errorf("statedb: begin turn accounting %q: %w", accounting.GetTurnId(), err)
	}
	defer func() { _ = tx.Rollback() }()
	if err := validateSubagentTopologyTx(tx, sessionID, accounting.GetResponses()); err != nil {
		return nil, fmt.Errorf("statedb: reject turn accounting %q topology: %w", accounting.GetTurnId(), err)
	}
	for _, response := range accounting.GetResponses() {
		responseRaw, err := proto.Marshal(response)
		if err != nil {
			return nil, fmt.Errorf("statedb: encode response %q: %w", response.GetApiMessageId(), err)
		}
		var priorResponseRaw []byte
		err = tx.QueryRow(`SELECT record FROM token_utilization WHERE agent_repl_session_id=? AND api_message_id=?`, sessionID, response.GetApiMessageId()).Scan(&priorResponseRaw)
		switch err {
		case sql.ErrNoRows:
			if _, err = tx.Exec(`INSERT INTO token_utilization(agent_repl_session_id, api_message_id, record) VALUES (?,?,?)`, sessionID, response.GetApiMessageId(), responseRaw); err != nil {
				return nil, fmt.Errorf("statedb: record response %q for turn %q: %w", response.GetApiMessageId(), accounting.GetTurnId(), err)
			}
		case nil:
			var priorResponse frontendv1.TokenUtilization
			if err := proto.Unmarshal(priorResponseRaw, &priorResponse); err != nil {
				return nil, fmt.Errorf("statedb: decode response %q: %w", response.GetApiMessageId(), err)
			}
			if priorResponse.GetRootTurnId() == "" {
				if err := tokenutilization.ValidateHistoricalAgainstLive(&priorResponse, response); err != nil {
					return nil, fmt.Errorf("statedb: historical response %q cannot converge with terminal turn %q: %w", response.GetApiMessageId(), accounting.GetTurnId(), err)
				}
				if _, err := tx.Exec(`UPDATE token_utilization SET record=? WHERE agent_repl_session_id=? AND api_message_id=?`, responseRaw, sessionID, response.GetApiMessageId()); err != nil {
					return nil, fmt.Errorf("statedb: enrich historical response %q for turn %q: %w", response.GetApiMessageId(), accounting.GetTurnId(), err)
				}
			} else {
				if err := tokenutilization.Validate(&priorResponse, tokenutilization.Identity{
					AgentReplSessionID: sessionID,
					ClaudeSessionID:    response.GetClaudeSessionId(),
					RootTurnID:         accounting.GetTurnId(),
				}); err != nil {
					return nil, fmt.Errorf("statedb: corrupt persisted response %q: %w", response.GetApiMessageId(), err)
				}
				if !proto.Equal(&priorResponse, response) || !tokenutilization.SameOptionalAPIRequestID(&priorResponse, response) {
					return nil, fmt.Errorf("statedb: divergent replay for response %q", response.GetApiMessageId())
				}
			}
		default:
			return nil, fmt.Errorf("statedb: read response %q for turn %q: %w", response.GetApiMessageId(), accounting.GetTurnId(), err)
		}
	}
	var prior []byte
	err = tx.QueryRow(`SELECT record FROM turn_accounting WHERE agent_repl_session_id=? AND turn_id=?`, sessionID, accounting.GetTurnId()).Scan(&prior)
	switch err {
	case nil:
		persisted := mustUnmarshalTurnAccounting(prior)
		comparable := proto.Clone(accounting).(*frontendv1.TurnAccounting)
		if comparable.GetTiming() != nil && persisted.GetTiming() != nil {
			comparable.Timing.AccountingSettledAtMs = persisted.GetTiming().GetAccountingSettledAtMs()
			comparable.Timing.ResultToSettlementMs = persisted.GetTiming().GetResultToSettlementMs()
		}
		if !proto.Equal(persisted, comparable) {
			return nil, fmt.Errorf("statedb: divergent replay for turn accounting %q", accounting.GetTurnId())
		}
		accounting = persisted
	case sql.ErrNoRows:
		if _, err = tx.Exec(`INSERT INTO turn_accounting(agent_repl_session_id, turn_id, record) VALUES (?,?,?)`, sessionID, accounting.GetTurnId(), raw); err != nil {
			return nil, fmt.Errorf("statedb: insert turn accounting %q: %w", accounting.GetTurnId(), err)
		}
	default:
		return nil, fmt.Errorf("statedb: read turn accounting %q: %w", accounting.GetTurnId(), err)
	}
	if err = tx.Commit(); err != nil {
		return nil, fmt.Errorf("statedb: commit turn accounting %q: %w", accounting.GetTurnId(), err)
	}
	return accounting, nil
}

func validateTurnAccountingResponses(sessionID string, accounting *frontendv1.TurnAccounting) error {
	seenMessages := make(map[string]struct{}, len(accounting.GetResponses()))
	claudeSessionID := ""
	for _, response := range accounting.GetResponses() {
		if err := tokenutilization.Validate(response, tokenutilization.Identity{
			AgentReplSessionID: sessionID,
			RootTurnID:         accounting.GetTurnId(),
		}); err != nil {
			return err
		}
		if claudeSessionID == "" {
			claudeSessionID = response.GetClaudeSessionId()
		} else if response.GetClaudeSessionId() != claudeSessionID {
			return fmt.Errorf("responses disagree on claude_session_id: %q versus %q", claudeSessionID, response.GetClaudeSessionId())
		}
		if _, duplicate := seenMessages[response.GetApiMessageId()]; duplicate {
			return fmt.Errorf("duplicate api_message_id %q in terminal response ledger", response.GetApiMessageId())
		}
		seenMessages[response.GetApiMessageId()] = struct{}{}
	}
	return nil
}

func mustUnmarshalTurnAccounting(raw []byte) *frontendv1.TurnAccounting {
	var accounting frontendv1.TurnAccounting
	if err := proto.Unmarshal(raw, &accounting); err != nil {
		panic(fmt.Sprintf("statedb: corrupt turn accounting: %v", err))
	}
	return &accounting
}

// List returns terminal accounting records in turn-id order for replay.
func (s *TurnAccountings) List(sessionID string) ([]*frontendv1.TurnAccounting, error) {
	rows, err := s.db.Query(`SELECT record FROM turn_accounting WHERE agent_repl_session_id=? ORDER BY turn_id`, sessionID)
	if err != nil {
		return nil, fmt.Errorf("statedb: list turn accounting for %q: %w", sessionID, err)
	}
	defer rows.Close()
	var out []*frontendv1.TurnAccounting
	for rows.Next() {
		var raw []byte
		if err := rows.Scan(&raw); err != nil {
			return nil, fmt.Errorf("statedb: scan turn accounting for %q: %w", sessionID, err)
		}
		var accounting frontendv1.TurnAccounting
		if err := proto.Unmarshal(raw, &accounting); err != nil {
			return nil, fmt.Errorf("statedb: decode turn accounting for %q: %w", sessionID, err)
		}
		out = append(out, &accounting)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate turn accounting for %q: %w", sessionID, err)
	}
	return out, nil
}
