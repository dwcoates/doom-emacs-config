package statedb

import (
	"database/sql"
	"fmt"
)

// This file is the DURABLE half of the prompt receipt (see the session
// controller's promptecho.go for the in-memory half it backs).
//
// WHAT A RECEIPT IS. When a user submits a prompt, the daemon immediately
// pushes a ConversationDelta carrying the prompt text keyed by the frontend
// command's request id, so the user's own bubble appears without waiting for
// the vendor's durable transcript to echo it back. That receipt used to live
// ONLY in daemon memory.
//
// THE GAP IT CLOSES. A prompt accepted and not yet durable when the daemon dies
// vanished without trace: the shim-store never received the turn, so the
// durable replay had nothing to serve, and the in-memory receipt died with the
// process. The user had seen their prompt on screen and, after a reconnect, saw
// no evidence it was ever sent. A receipt the user saw must never be
// unrecoverable, which is what this table guarantees.
//
// WHY IT LIVES IN THE SHARED STATE STORE rather than the shim-store. The
// shim-store is the vendor conversation's record and its contract is
// deliberately tiny (schema, seq, dedup, fan-out). A receipt is a DAEMON fact
// about a submit the daemon itself made — the same kind of fact as the SSM's
// state log and the merge lease ledger, which share this database for exactly
// that reason.
//
// WHY A ROW IS DELETED RATHER THAN TOMBSTONED. A retired receipt has been
// superseded by the transcript's own copy of the prompt, which is durable, in
// its real place, at its real seq. Keeping the receipt past that point would
// serve a daemon-local duplicate of something the conversation already holds,
// and the row's whole purpose is "evidence not yet in the conversation".

// PromptReceipt is ONE accepted prompt the daemon durably recorded before
// pushing its receipt bubble.
type PromptReceipt struct {
	// RequestID is the frontend command's own request id — the identity the
	// frontend keys the prompt bubble on, and the identity the durable
	// transcript line is stamped with when it arrives.
	RequestID string
	// Workspace is the workspace whose session the prompt was submitted to.
	Workspace string
	// Text is the prompt as the user typed it.
	Text string
	// AcceptedAtMs is the instant the daemon committed to submitting the
	// prompt. It is the item timestamp the receipt is pushed under, live and
	// replayed alike, so a merge window that contains it reaches the same
	// provenance verdict both times.
	AcceptedAtMs int64
}

// PromptReceipts is the prompt-receipt table's owner.
type PromptReceipts struct{ db *sql.DB }

// NewPromptReceipts installs the prompt_receipt table on the shared state
// store and returns its owner.
//
// The DDL is idempotent and purely ADDITIVE, which is why it carries no schema
// version of its own: the shared store's `schema_meta` row is the session-state
// manager's, and a table that appears by CREATE TABLE IF NOT EXISTS cannot make
// an older binary's reading of the rest of the database wrong. This is the same
// discipline the out-of-band ALTERs in the session-state manager's migration
// follow.
func NewPromptReceipts(db *sql.DB) (*PromptReceipts, error) {
	if db == nil {
		return nil, fmt.Errorf("statedb: NewPromptReceipts needs an open state store")
	}
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS prompt_receipt (
			request_id     TEXT    PRIMARY KEY,
			workspace      TEXT    NOT NULL,
			text           TEXT    NOT NULL,
			accepted_at_ms INTEGER NOT NULL
		);
		CREATE INDEX IF NOT EXISTS prompt_receipt_workspace
			ON prompt_receipt(workspace, accepted_at_ms);
	`); err != nil {
		return nil, fmt.Errorf("statedb: create prompt_receipt schema: %w", err)
	}
	return &PromptReceipts{db: db}, nil
}

// Record persists one accepted prompt. It MUST complete before the receipt
// bubble is pushed, so a receipt the user saw always implies a durable record.
//
// A re-record under the same request id OVERWRITES rather than failing: the
// request id is the submit's identity, so a second write under it is the same
// submit being re-accepted (a queued prompt re-delivered, a retried command),
// and refusing it would fail a submit over bookkeeping the row already agrees
// with. Any OTHER database failure is returned to the caller, which fails the
// submit — an unwritable state store is not a condition to carry on through.
func (s *PromptReceipts) Record(r PromptReceipt) error {
	if r.RequestID == "" {
		return fmt.Errorf("statedb: prompt receipt for workspace %q has no request id to key it on", r.Workspace)
	}
	if r.Workspace == "" {
		return fmt.Errorf("statedb: prompt receipt %q has no workspace", r.RequestID)
	}
	_, err := s.db.Exec(
		`INSERT INTO prompt_receipt(request_id, workspace, text, accepted_at_ms) VALUES (?,?,?,?)
		 ON CONFLICT(request_id) DO UPDATE SET
		     workspace = excluded.workspace,
		     text = excluded.text,
		     accepted_at_ms = excluded.accepted_at_ms`,
		r.RequestID, r.Workspace, r.Text, r.AcceptedAtMs)
	if err != nil {
		return fmt.Errorf("statedb: record prompt receipt %q for workspace %q: %w", r.RequestID, r.Workspace, err)
	}
	return nil
}

// Retire discards the receipt for requestID, reporting whether one was
// outstanding. Retiring a receipt that is already gone is a no-op with a nil
// error: the retirement points are several (the durable line arriving live, a
// replay finding the prompt already in the store, a submit that failed after
// acceptance) and any of them may legitimately run second.
func (s *PromptReceipts) Retire(requestID string) (bool, error) {
	if requestID == "" {
		return false, fmt.Errorf("statedb: cannot retire a prompt receipt with no request id")
	}
	res, err := s.db.Exec(`DELETE FROM prompt_receipt WHERE request_id = ?`, requestID)
	if err != nil {
		return false, fmt.Errorf("statedb: retire prompt receipt %q: %w", requestID, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("statedb: retire prompt receipt %q: %w", requestID, err)
	}
	return n > 0, nil
}

// RetireWorkspace discards every receipt for a workspace accepted at or before
// throughMs, reporting how many went.
//
// It is the CONTEXT CUT's retirement: a clear or a compaction discards the
// history below it, and a receipt for a prompt from below that line has been
// discarded along with the prompt. Replaying it would put pre-cut text back
// above a floor that exists to hide exactly that.
func (s *PromptReceipts) RetireWorkspace(workspace string, throughMs int64) (int, error) {
	if workspace == "" {
		return 0, fmt.Errorf("statedb: cannot retire prompt receipts for an empty workspace")
	}
	res, err := s.db.Exec(
		`DELETE FROM prompt_receipt WHERE workspace = ? AND accepted_at_ms <= ?`, workspace, throughMs)
	if err != nil {
		return 0, fmt.Errorf("statedb: retire prompt receipts for workspace %q through %d: %w", workspace, throughMs, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("statedb: retire prompt receipts for workspace %q through %d: %w", workspace, throughMs, err)
	}
	return int(n), nil
}

// Outstanding lists a workspace's un-retired receipts, OLDEST FIRST — submit
// order, which is the order they are replayed in.
func (s *PromptReceipts) Outstanding(workspace string) ([]PromptReceipt, error) {
	rows, err := s.db.Query(
		`SELECT request_id, workspace, text, accepted_at_ms FROM prompt_receipt
		 WHERE workspace = ? ORDER BY accepted_at_ms, request_id`, workspace)
	if err != nil {
		return nil, fmt.Errorf("statedb: read prompt receipts for workspace %q: %w", workspace, err)
	}
	defer rows.Close()
	var out []PromptReceipt
	for rows.Next() {
		var r PromptReceipt
		if err := rows.Scan(&r.RequestID, &r.Workspace, &r.Text, &r.AcceptedAtMs); err != nil {
			return nil, fmt.Errorf("statedb: scan prompt receipt for workspace %q: %w", workspace, err)
		}
		out = append(out, r)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate prompt receipts for workspace %q: %w", workspace, err)
	}
	return out, nil
}
