package statedb

import (
	"database/sql"
	"fmt"
)

// This file is the DURABLE half of a session's TERMINAL failure card (see the
// session controller's vanishedresume.go for the fence that writes it).
//
// WHAT THE ROW IS. A bring-up that is refused before any session controller
// exists — a resume whose transcript has been deleted — publishes ONE failure
// card and then refuses to spawn anything, forever. That card used to be a
// LIVE PUSH ONLY: it reached whichever webview happened to be connected at that
// instant and nothing else. Every later reader of the workspace resyncs from
// durable history (sessioncontroller/durablereplay.go), and durable history is
// the vendor conversation the store holds — which, for a session that never
// came up, carries no account of why. A reload therefore showed the whole
// conversation with no explanation of why nothing is driving it, and the only
// record of the terminal refusal was a log line.
//
// WHY THE ROW IS KEYED BY SESSION. The fence is one fact per session, so its
// card is one row per session: a re-fence — the fence is cleared by a hard
// restart and re-established when the re-check finds the transcript still gone
// — REPLACES the row rather than appending beside it. A history that
// accumulated one card per re-check would report a dozen failures for one
// broken session.
//
// WHY THE ROW IS DELETED ON CLEAR. Clearing the fence is an explicit user
// action (a hard restart), and it withdraws the standing claim: the session is
// being brought up again, and the next bring-up either succeeds — in which case
// the old card is a lie about a live session — or re-fences and rewrites the
// row. The card is durable evidence of a STANDING condition, not an archive of
// past ones.
//
// WHY IT LIVES IN THE SHARED STATE STORE rather than the shim-store, for the
// same reason the prompt receipts do: it is a DAEMON fact about a bring-up the
// daemon itself refused, not a line of the vendor's conversation.

// TerminalFailureCard is one session's standing terminal failure card.
type TerminalFailureCard struct {
	// SessionID keys the row: one standing terminal card per session.
	SessionID string
	// Workspace is the workspace the card is published onto.
	Workspace string
	// UUID is the card's conversation-item identity, the SAME identity the
	// live push uses, so a client that saw the live card and then resyncs
	// updates one item instead of drawing two.
	UUID string
	// Card is a marshaled frontend.v1.FailureCardView. It is stored opaque so
	// this package keeps no dependency on the frontend contract; the session
	// controller owns both the rendering and the parsing.
	Card []byte
	// AtMs is the instant the fence was established, which is the card's
	// timestamp in the conversation and the instant its provenance is
	// resolved against on replay.
	AtMs int64
}

// TerminalFailureCards is the terminal_failure_card table's owner.
type TerminalFailureCards struct{ db *sql.DB }

// NewTerminalFailureCards installs the terminal_failure_card table on the
// shared state store and returns its owner.
//
// The DDL is idempotent and purely ADDITIVE, exactly as NewPromptReceipts's is,
// so it carries no schema version of its own.
func NewTerminalFailureCards(db *sql.DB) (*TerminalFailureCards, error) {
	if db == nil {
		return nil, fmt.Errorf("statedb: NewTerminalFailureCards needs an open state store")
	}
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS terminal_failure_card (
			session_id TEXT    PRIMARY KEY,
			workspace  TEXT    NOT NULL,
			uuid       TEXT    NOT NULL,
			card       BLOB    NOT NULL,
			at_ms      INTEGER NOT NULL
		);
		CREATE INDEX IF NOT EXISTS terminal_failure_card_workspace
			ON terminal_failure_card(workspace);
	`); err != nil {
		return nil, fmt.Errorf("statedb: create terminal_failure_card schema: %w", err)
	}
	return &TerminalFailureCards{db: db}, nil
}

// Record writes the session's standing terminal card, REPLACING any prior one.
//
// The replace is what makes a re-fence idempotent in history: the fence's card
// is a standing claim about one session, and a second establishment of the same
// fence restates that claim rather than adding a new one.
func (s *TerminalFailureCards) Record(rec TerminalFailureCard) error {
	switch {
	case rec.SessionID == "":
		return fmt.Errorf("statedb: terminal failure card needs a session id")
	case rec.Workspace == "":
		return fmt.Errorf("statedb: terminal failure card for session %q needs a workspace", rec.SessionID)
	case rec.UUID == "":
		return fmt.Errorf("statedb: terminal failure card for session %q needs a card uuid", rec.SessionID)
	case len(rec.Card) == 0:
		return fmt.Errorf("statedb: terminal failure card for session %q carries no card", rec.SessionID)
	}
	if _, err := s.db.Exec(`
		INSERT INTO terminal_failure_card (session_id, workspace, uuid, card, at_ms)
		VALUES (?, ?, ?, ?, ?)
		ON CONFLICT(session_id) DO UPDATE SET
			workspace = excluded.workspace,
			uuid      = excluded.uuid,
			card      = excluded.card,
			at_ms     = excluded.at_ms
	`, rec.SessionID, rec.Workspace, rec.UUID, rec.Card, rec.AtMs); err != nil {
		return fmt.Errorf("statedb: record terminal failure card for session %q: %w", rec.SessionID, err)
	}
	return nil
}

// Standing returns the session's terminal card, if one stands.
func (s *TerminalFailureCards) Standing(sessionID string) (TerminalFailureCard, bool, error) {
	if sessionID == "" {
		return TerminalFailureCard{}, false, fmt.Errorf("statedb: terminal failure card lookup needs a session id")
	}
	rec := TerminalFailureCard{SessionID: sessionID}
	err := s.db.QueryRow(
		`SELECT workspace, uuid, card, at_ms FROM terminal_failure_card WHERE session_id = ?`,
		sessionID).Scan(&rec.Workspace, &rec.UUID, &rec.Card, &rec.AtMs)
	if err == sql.ErrNoRows {
		return TerminalFailureCard{}, false, nil
	}
	if err != nil {
		return TerminalFailureCard{}, false, fmt.Errorf("statedb: read terminal failure card for session %q: %w", sessionID, err)
	}
	return rec, true, nil
}

// Withdraw deletes the session's standing terminal card and reports whether one
// stood. A false with no error means there was nothing to withdraw, which is
// the ordinary outcome for every session that was never fenced.
func (s *TerminalFailureCards) Withdraw(sessionID string) (bool, error) {
	if sessionID == "" {
		return false, fmt.Errorf("statedb: terminal failure card withdrawal needs a session id")
	}
	res, err := s.db.Exec(`DELETE FROM terminal_failure_card WHERE session_id = ?`, sessionID)
	if err != nil {
		return false, fmt.Errorf("statedb: withdraw terminal failure card for session %q: %w", sessionID, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("statedb: withdraw terminal failure card for session %q: rows affected: %w", sessionID, err)
	}
	return n > 0, nil
}
