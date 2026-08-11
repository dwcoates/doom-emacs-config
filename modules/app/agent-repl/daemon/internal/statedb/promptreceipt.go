package statedb

import (
	"database/sql"
	"fmt"
)

// This file owns the `prompt_receipt` table, which now holds exactly ONE kind
// of row: the interrupted-turn resumption.
//
// THE ROW THE TABLE IS NAMED AFTER IS GONE. It used to also carry a prompt
// RECEIPT — durable evidence of a prompt the daemon had accepted so the user's
// own words could be drawn before the vendor's transcript echoed them back. A
// prompt now renders when it round-trips through the SDK and not before, so
// there is no second identity for one prompt and nothing for a receipt to stand
// in for. The table keeps its name because the rows below still live in it and
// renaming a durable table buys nothing.
//
// WHY IT LIVES IN THE SHARED STATE STORE rather than the shim-store. The
// shim-store is the vendor conversation's record and its contract is
// deliberately tiny (schema, seq, dedup, fan-out). What this table holds is a
// DAEMON fact about work the daemon itself owes — the same kind of fact as the
// SSM's state log and the merge lease ledger, which share this database for
// exactly that reason.

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
// IT IS NOT A PROMPT, AND NO QUERY HERE CAN MAKE IT ONE. A resumption row must
// never be rendered anywhere: there is no second prompt from the user, and the
// re-drive text is a daemon-internal instruction the user never wrote. Every
// query below is scoped to the resumption states and returns the row to the
// re-drive machinery alone, so there is no path from this table into the feed
// at all — which is stronger than the filter that used to keep the two row
// kinds apart, because the renderable kind no longer exists.
//
// EXACTLY-ONCE IS LEVEL-TRIGGERED OFF THIS ROW, never off an in-memory flag: a
// bounce during a resumption must not double-submit, and the only thing that
// survives a bounce is the row.
//
// THE ROW IS ALSO THE SINGLE-DISCHARGE FENCE, and this is what a live incident
// taught. The re-drive used to be re-issued whenever a wire found the row still
// standing, and a submit was treated as failed the moment its control request
// timed out — so a bring-up that thrashed re-drove the same interrupted turn
// several times, and several copies of the daemon's instruction landed in the
// conversation. A submit whose control request timed out has an UNKNOWN fate:
// the shim may well have taken it, and re-issuing on that evidence is choosing
// the duplicate.
//
// So the row carries WHICH SIDE OF THE SUBMIT it is on. A re-drive claims the
// row (pending → delivering) BEFORE it submits, in one conditional update the
// database arbitrates, and only a row still `pending` is ever re-driven. A
// claim that is never seen through leaves a `delivering` row: no duplicate is
// possible, and the row stands as the durable evidence of the turn nobody
// picked up. The row is DELETED only on evidence the instruction reached the
// vendor conversation, or when the user preempts it.

// PromptReceiptResumption names what a prompt_receipt row IS.
type PromptReceiptResumption string

const (
	// ResumptionNone is a row that is no resumption at all. It is the empty
	// string because that is what the column defaults to, and it survives as a
	// named constant so the queries below can say which rows they exclude:
	// rows written by an older binary as prompt receipts, a kind nothing
	// produces any more.
	ResumptionNone PromptReceiptResumption = ""
	// ResumptionPending is a turn interrupted by a teardown and owed a
	// re-drive by whichever daemon next wires the session.
	ResumptionPending PromptReceiptResumption = "pending"
	// ResumptionDelivering is a re-drive that has been CLAIMED: some daemon
	// took this row and submitted for it. Whether the shim ran that submit may
	// be unknown, which is exactly why the state exists — no second daemon, and
	// no later wire of this one, may re-drive it.
	ResumptionDelivering PromptReceiptResumption = "delivering"
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
	// State is which side of the submit this row is on: owed and unclaimed
	// (ResumptionPending), or claimed by a re-drive whose delivery has not been
	// confirmed (ResumptionDelivering).
	State PromptReceiptResumption
	// DeliveryStartedAtMs is when a re-drive claimed this row, zero while it is
	// still pending. It is diagnostic: a `delivering` row that has stood since
	// long before the conversation's tail is a re-drive nobody ever confirmed.
	DeliveryStartedAtMs int64
}

// PromptReceipts is the prompt_receipt table's owner.
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
		{Name: "delivery_started_at_ms", DDL: `INTEGER NOT NULL DEFAULT 0`},
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
// Re-recording under the same request id OVERWRITES: the request id is the re-drive's identity, so a second write
// under it is the same re-drive being re-recorded rather than a second one.
//
// IT CANNOT UN-CLAIM A ROW A RE-DRIVE ALREADY TOOK. A retried teardown that
// found the row `delivering` would otherwise hand the same interrupted turn
// back to the next wire as owed, which is the duplicate the claim exists to
// make impossible. The conflict update is scoped to a still-`pending` row, so
// the re-record is a no-op over a claimed one.
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
		     interrupted_at_ms = excluded.interrupted_at_ms
		 WHERE prompt_receipt.resumption_state = ?`,
		r.RequestID, r.Workspace, r.Text, r.InterruptedAtMs,
		string(ResumptionPending), r.TurnID, r.InterruptedAtMs,
		string(ResumptionPending))
	if err != nil {
		return fmt.Errorf("statedb: record pending resumption %q for workspace %q: %w", r.RequestID, r.Workspace, err)
	}
	return nil
}

// PendingResumptions lists a workspace's UNCLAIMED resumptions, OLDEST FIRST —
// the order the turns were interrupted in, which is the order they are
// re-driven in.
//
// This is the LEVEL the re-drive is triggered off. A caller asks the store what
// is owed rather than remembering what it queued, so a daemon that died between
// recording and re-driving is indistinguishable from one that never got round
// to it: both leave the row, and the next daemon to wire the session finds it.
//
// A CLAIMED row is not among them, and that is the whole single-discharge
// fence: a re-drive already submitted for it, and whether the shim ran that
// submit is not something a second re-drive can find out by trying again. Use
// UndischargedResumptions for the sweeps that must see both.
func (s *PromptReceipts) PendingResumptions(workspace string) ([]PendingResumption, error) {
	return s.resumptionsInStates(workspace, "pending", ResumptionPending)
}

// UndischargedResumptions lists every resumption row a workspace still carries,
// claimed or not, OLDEST FIRST.
//
// It is the PREEMPTION's reading. A user who submits or interrupts has moved on
// from the interrupted turn whether or not a re-drive already claimed it, so
// the cancellation has to be able to reach a `delivering` row — otherwise the
// row nobody will ever re-drive would sit there forever.
func (s *PromptReceipts) UndischargedResumptions(workspace string) ([]PendingResumption, error) {
	return s.resumptionsInStates(workspace, "undischarged", ResumptionPending, ResumptionDelivering)
}

// resumptionsInStates is the ONE resumption query, parameterized by which
// states the caller means. Two hand-written near-copies of it would be two
// places for the row shape and the ordering to drift apart.
func (s *PromptReceipts) resumptionsInStates(workspace, what string, states ...PromptReceiptResumption) ([]PendingResumption, error) {
	if workspace == "" {
		return nil, fmt.Errorf("statedb: cannot read %s resumptions for an empty workspace", what)
	}
	if len(states) == 0 {
		return nil, fmt.Errorf("statedb: cannot read %s resumptions for workspace %q with no state to match", what, workspace)
	}
	args := []any{workspace}
	for _, st := range states {
		args = append(args, string(st))
	}
	rows, err := s.db.Query(
		`SELECT request_id, workspace, interrupted_turn_id, text, interrupted_at_ms,
		        resumption_state, delivery_started_at_ms
		 FROM prompt_receipt
		 WHERE workspace = ? AND resumption_state IN (`+Placeholders(len(states))+`)
		 ORDER BY interrupted_at_ms, request_id`, args...)
	if err != nil {
		return nil, fmt.Errorf("statedb: read %s resumptions for workspace %q: %w", what, workspace, err)
	}
	defer rows.Close()
	var out []PendingResumption
	for rows.Next() {
		var r PendingResumption
		var state string
		if err := rows.Scan(&r.RequestID, &r.Workspace, &r.TurnID, &r.Text, &r.InterruptedAtMs,
			&state, &r.DeliveryStartedAtMs); err != nil {
			return nil, fmt.Errorf("statedb: scan %s resumption for workspace %q: %w", what, workspace, err)
		}
		r.State = PromptReceiptResumption(state)
		out = append(out, r)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate %s resumptions for workspace %q: %w", what, workspace, err)
	}
	return out, nil
}

// ClaimResumptionForDelivery takes one owed resumption for a re-drive that is
// about to submit, reporting whether this caller got it.
//
// IT IS THE FENCE, AND THE DATABASE ARBITRATES IT. The state moves pending →
// delivering in ONE conditional update, so two daemons — or one daemon whose
// bring-up wired the session twice — cannot both come away holding the same
// interrupted turn. Whoever loses is told `false` and submits nothing.
//
// IT IS CLAIMED BEFORE THE SUBMIT, never after. A claim taken afterwards would
// leave the window the incident actually landed in: the submit reaches the shim,
// its control request times out, and the row is still `pending` for the next
// wire to re-drive — which is how one interrupted turn became several copies of
// the daemon's instruction in the conversation.
func (s *PromptReceipts) ClaimResumptionForDelivery(requestID string, atMs int64) (bool, error) {
	if requestID == "" {
		return false, fmt.Errorf("statedb: cannot claim a resumption with no request id")
	}
	res, err := s.db.Exec(
		`UPDATE prompt_receipt
		    SET resumption_state = ?, delivery_started_at_ms = ?
		  WHERE request_id = ? AND resumption_state = ?`,
		string(ResumptionDelivering), atMs, requestID, string(ResumptionPending))
	if err != nil {
		return false, fmt.Errorf("statedb: claim resumption %q for delivery: %w", requestID, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("statedb: claim resumption %q for delivery: %w", requestID, err)
	}
	return n > 0, nil
}

// DischargeResumption discards one resumption row, claimed or not, reporting
// whether one was there.
//
// TWO CALLERS, ONE EDGE. The re-drive discharges it on the evidence that its
// instruction reached the vendor conversation — the curator suppressing that
// instruction out of a conversation delta (frontend/internalresume.go) — and
// the user preempting it discharges it when they submit or interrupt first.
// Both are "this resumption will not happen again", which is the only thing the
// row means.
//
// IT IS NOT DISCHARGED ON THE SUBMIT'S OWN RETURN. A submit that returned
// successfully has still only been handed to a shim, and a submit that returned
// an error may have been run anyway; neither answers whether the instruction is
// in the conversation. The conversation does.
//
// THE DELETE IS SCOPED TO THE RESUMPTION STATES, so a row an older binary left
// behind as a prompt receipt is never swept away by a resumption's discharge.
//
// Discharging a resumption that is already gone is a no-op with a nil error:
// the two callers can legitimately race, and the loser is not a failure.
func (s *PromptReceipts) DischargeResumption(requestID string) (bool, error) {
	if requestID == "" {
		return false, fmt.Errorf("statedb: cannot discharge a resumption with no request id")
	}
	res, err := s.db.Exec(
		`DELETE FROM prompt_receipt WHERE request_id = ? AND resumption_state IN (?,?)`,
		requestID, string(ResumptionPending), string(ResumptionDelivering))
	if err != nil {
		return false, fmt.Errorf("statedb: discharge resumption %q: %w", requestID, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("statedb: discharge resumption %q: %w", requestID, err)
	}
	return n > 0, nil
}
