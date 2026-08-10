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

// THE SECOND KIND OF ROW: A PENDING RESUMPTION.
//
// A planned daemon bounce that must take the shim with it (the shim bundle
// changed, or the shim is dead) interrupts whatever turn was in flight. The
// interrupt itself is honest — it is the only way that turn's end gets
// reported — but it must not be the END STATE: the user asked for work, and a
// deploy is not a reason to abandon it.
//
// So the teardown durably records the interrupted turn here, and the successor
// daemon re-drives it once the session is wired again. That record lives in
// THIS table rather than a parallel store because it is exactly the same kind
// of fact the table already holds: durable evidence of a submit whose work is
// not yet in the conversation.
//
// IT IS NOT A RECEIPT, AND THE SCHEMA IS WHAT KEEPS THEM APART. A receipt
// exists to be RENDERED — Outstanding serves it to the durable replay, which
// pushes it as the user's own prompt bubble. A resumption row must never be
// rendered anywhere: there is no second prompt from the user, and the re-drive
// text (when one is needed at all) is a daemon-internal instruction the user
// never wrote. Outstanding therefore serves only rows with an EMPTY
// resumption_state, which makes a resumption row unrenderable by construction
// rather than by every reader remembering to filter it out.
//
// EXACTLY-ONCE IS LEVEL-TRIGGERED OFF THIS ROW, never off an in-memory flag: a
// bounce during a resumption must not double-submit, and the only thing that
// survives a bounce is the row. It is cleared when the re-driven turn is
// ACCEPTED, so a daemon that dies mid-resumption leaves the row standing and
// its successor tries again.

// PromptReceiptResumption names what a prompt_receipt row IS.
type PromptReceiptResumption string

const (
	// ResumptionNone is an ordinary prompt receipt: durable evidence of a
	// prompt the user submitted, awaiting its own durable transcript line.
	// This is the empty string so every row written before resumptions existed
	// reads as one, which is what they are.
	ResumptionNone PromptReceiptResumption = ""
	// ResumptionPending is a turn interrupted by a teardown and owed a
	// re-drive by whichever daemon next wires the session.
	ResumptionPending PromptReceiptResumption = "pending"
)

// PendingResumption is one interrupted turn owed a re-drive.
type PendingResumption struct {
	// RequestID keys the row, exactly as it keys a receipt. It is the request
	// id of the RE-DRIVE the successor daemon will issue, minted at teardown so
	// the submit's identity is durable before the submit exists — which is what
	// makes the re-drive recognizable as this record's discharge rather than as
	// a fresh user prompt that happens to look similar.
	RequestID string
	// Workspace is the workspace whose turn was interrupted.
	Workspace string
	// TurnID is the interrupted turn's own identity, so the teardown's
	// interrupted-turn record can be resolved when the re-driven turn starts.
	// Empty when the turn was in flight but this daemon could not name it (an
	// adopted turn), which does not block the re-drive.
	TurnID string
	// Text is the re-drive instruction. It is DAEMON-INTERNAL and never
	// rendered; see the note above.
	Text string
	// InterruptedAtMs is when the teardown interrupted the turn.
	InterruptedAtMs int64
}

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
	// The resumption columns are additive over a table that predates them, and
	// their defaults are what make an old row read as the ordinary receipt it
	// is (see PromptReceiptResumption).
	if err := AddColumnsIfMissing(db, "prompt_receipt", []ColumnSpec{
		{Name: "resumption_state", DDL: `TEXT NOT NULL DEFAULT ''`},
		{Name: "interrupted_turn_id", DDL: `TEXT NOT NULL DEFAULT ''`},
		{Name: "interrupted_at_ms", DDL: `INTEGER NOT NULL DEFAULT 0`},
	}); err != nil {
		return nil, err
	}
	if _, err := db.Exec(`
		CREATE INDEX IF NOT EXISTS prompt_receipt_resumption
			ON prompt_receipt(workspace, resumption_state, interrupted_at_ms);
	`); err != nil {
		return nil, fmt.Errorf("statedb: index prompt_receipt resumptions: %w", err)
	}
	return &PromptReceipts{db: db}, nil
}

// RecordPendingResumption durably records one interrupted turn owed a re-drive.
//
// It MUST complete before the teardown's interrupt is delivered. A turn
// interrupted without this row is a turn nobody will ever pick up, which is the
// exact loss this record exists to prevent.
//
// Re-recording under the same request id OVERWRITES, for the same reason
// Record does: the request id is the re-drive's identity, so a second write
// under it is the same re-drive being re-recorded rather than a second one.
func (s *PromptReceipts) RecordPendingResumption(r PendingResumption) error {
	if r.RequestID == "" {
		return fmt.Errorf("statedb: pending resumption for workspace %q has no request id to key it on", r.Workspace)
	}
	if r.Workspace == "" {
		return fmt.Errorf("statedb: pending resumption %q has no workspace", r.RequestID)
	}
	_, err := s.db.Exec(
		`INSERT INTO prompt_receipt(
			request_id, workspace, text, accepted_at_ms,
			resumption_state, interrupted_turn_id, interrupted_at_ms
		) VALUES (?,?,?,?,?,?,?)
		 ON CONFLICT(request_id) DO UPDATE SET
		     workspace = excluded.workspace,
		     text = excluded.text,
		     accepted_at_ms = excluded.accepted_at_ms,
		     resumption_state = excluded.resumption_state,
		     interrupted_turn_id = excluded.interrupted_turn_id,
		     interrupted_at_ms = excluded.interrupted_at_ms`,
		r.RequestID, r.Workspace, r.Text, r.InterruptedAtMs,
		string(ResumptionPending), r.TurnID, r.InterruptedAtMs)
	if err != nil {
		return fmt.Errorf("statedb: record pending resumption %q for workspace %q: %w", r.RequestID, r.Workspace, err)
	}
	return nil
}

// PendingResumptions lists a workspace's un-discharged resumptions, OLDEST
// FIRST — the order the turns were interrupted in, which is the order they are
// re-driven in.
//
// This is the LEVEL the re-drive is triggered off. A caller asks the store what
// is owed rather than remembering what it queued, so a daemon that died between
// recording and re-driving is indistinguishable from one that never got round
// to it: both leave the row, and the next daemon to wire the session finds it.
func (s *PromptReceipts) PendingResumptions(workspace string) ([]PendingResumption, error) {
	if workspace == "" {
		return nil, fmt.Errorf("statedb: cannot read pending resumptions for an empty workspace")
	}
	rows, err := s.db.Query(
		`SELECT request_id, workspace, interrupted_turn_id, text, interrupted_at_ms
		 FROM prompt_receipt
		 WHERE workspace = ? AND resumption_state = ?
		 ORDER BY interrupted_at_ms, request_id`, workspace, string(ResumptionPending))
	if err != nil {
		return nil, fmt.Errorf("statedb: read pending resumptions for workspace %q: %w", workspace, err)
	}
	defer rows.Close()
	var out []PendingResumption
	for rows.Next() {
		var r PendingResumption
		if err := rows.Scan(&r.RequestID, &r.Workspace, &r.TurnID, &r.Text, &r.InterruptedAtMs); err != nil {
			return nil, fmt.Errorf("statedb: scan pending resumption for workspace %q: %w", workspace, err)
		}
		out = append(out, r)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate pending resumptions for workspace %q: %w", workspace, err)
	}
	return out, nil
}

// DischargeResumption discards one resumption row, reporting whether one was
// owed.
//
// TWO CALLERS, ONE EDGE. The re-drive discharges it when the re-driven turn is
// ACCEPTED (not when it is issued — an issued-but-refused re-drive is still
// owed), and the user preempting it discharges it when they submit or interrupt
// first. Both are "this resumption will not happen again", which is the only
// thing the row means.
//
// Discharging a resumption that is already gone is a no-op with a nil error:
// the two callers can legitimately race, and the loser is not a failure.
func (s *PromptReceipts) DischargeResumption(requestID string) (bool, error) {
	if requestID == "" {
		return false, fmt.Errorf("statedb: cannot discharge a resumption with no request id")
	}
	res, err := s.db.Exec(
		`DELETE FROM prompt_receipt WHERE request_id = ? AND resumption_state = ?`,
		requestID, string(ResumptionPending))
	if err != nil {
		return false, fmt.Errorf("statedb: discharge resumption %q: %w", requestID, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("statedb: discharge resumption %q: %w", requestID, err)
	}
	return n > 0, nil
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
//
// A PENDING RESUMPTION IS NOT A RECEIPT AND IS NEVER SERVED HERE. This is the
// filter that makes a re-drive invisible: the durable replay is the only path
// by which a row in this table becomes a rendered prompt bubble, so a row this
// query cannot return is a row no client can render — in a live push, a connect
// snapshot, a resync replay or a store re-pull alike. The invisibility is a
// property of the query rather than of every reader remembering to check.
func (s *PromptReceipts) Outstanding(workspace string) ([]PromptReceipt, error) {
	rows, err := s.db.Query(
		`SELECT request_id, workspace, text, accepted_at_ms FROM prompt_receipt
		 WHERE workspace = ? AND resumption_state = ?
		 ORDER BY accepted_at_ms, request_id`, workspace, string(ResumptionNone))
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
