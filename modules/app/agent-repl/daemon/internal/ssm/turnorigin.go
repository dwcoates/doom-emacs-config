package ssm

import (
	"database/sql"
	"fmt"
)

// THE CLOSING EDGE EVERY MACHINE-ORIGINATED TURN WAS MISSING.
//
// A durable turn claim is retired by an OBSERVATION: the turn's own `TurnEnded`
// off the stream, a returning shim's hello contradicting it, or an interrupt ack
// that says there is nothing to stop. Every one of those requires something to
// still be watching the turn.
//
// The daemon submits turns that nothing watches. A merge run's conflict
// resolution, a workspace-create initial prompt and a cache keep-alive ping are
// all opened BY the daemon, on behalf of a machine, and each has a lifecycle of
// its own that ends whether or not the turn behind it ever reported anything.
// When the SDK behind one of them dies quietly, the observation never comes and
// the claim stands forever — a workspace latched `thinking`, hibernation
// refused, every later prompt queued behind a boundary that is not coming.
// Observed live: a `workspace-create:…:0` claim open from the moment the
// workspace was born, a `merge-resume:…` claim outliving a `merge_failed`, and a
// `ka_…` claim open for hours behind a ping that finishes in seconds.
//
// So each of those origins gets a CLOSING EDGE OF ITS OWN, taken from the fact
// it already has: the run reached a terminal outcome, the creation job settled
// or failed, the ping outlived any duration a ping can take. This file is the
// one door all three go through, so a fourth origin joins them rather than
// inventing a fourth way to end a turn.

// The ORIGIN-TERMINAL causes: the reasons a machine-originated turn's claim is
// ended by the origin that submitted it rather than by an observation of the
// turn.
//
// Each names the LIFECYCLE FACT that licensed the close, never merely "the
// daemon closed it". A reader of the ledger has to be able to tell a keep-alive
// ping whose end was never reported from a merge run that terminated over one,
// because the two are different defects with different fixes.
const (
	// TurnCloseMergeRunTerminal closes a claim opened by a merge run's own
	// resolution turns once that run reaches ANY terminal outcome. The run is
	// over; nothing is left to receive the turn's end, and the merge lease it
	// held is already released.
	TurnCloseMergeRunTerminal = "merge_run_terminal"
	// TurnCloseWorkspaceCreateSettled closes a claim opened by a
	// workspace-create initial prompt once the creation job reaches its own
	// terminal state (ready or failed).
	TurnCloseWorkspaceCreateSettled = "workspace_create_settled"
	// TurnCloseKeepAliveOverdue closes a claim opened by a cache keep-alive
	// ping that has stood open past any duration a ping can take. A ping is one
	// model call answering with a single character; a claim outliving a generous
	// multiple of that is not a slow ping, it is a turn whose end is not coming.
	TurnCloseKeepAliveOverdue = "keep_alive_overdue"
)

// CloseOriginTurns ends the NAMED turns' open claims, attributing each close to
// the origin lifecycle fact that licensed it, and reports the identities it
// closed.
//
// # It is scoped by TURN ID, not by claimant, and that is deliberate
//
// A named turn id is minted fresh per turn, so at most one claimant in a
// workspace ever holds a row for it (see recordTurnEnd's widening comment). The
// origins that call this — a merge run, a creation job, the keep-alive policy —
// know the id they submitted and do NOT reliably know which daemon session
// generation still holds its claim: hibernate, revive and reopen all re-mint the
// claimant under the same conversation, and the claim stays keyed to whichever
// generation opened it. Requiring the caller to name a claimant would therefore
// make the close silently retire nothing precisely when the workspace has been
// through the churn that produced the leak.
//
// # An unknown turn id is a NO-OP, not an error
//
// The ordinary outcome is that the turn's own `TurnEnded` already retired the
// claim. That is not a failure and nothing is stamped onto it: the run ending
// does not explain a close that happened honestly before it, and rewriting the
// attribution would put this cause on turns that ended perfectly well.
//
// # The durable end is written where a replay will see it
//
// Closing the claim closes it for its own claimant only, and a superseded
// workspace re-mints its claimant and replays the same store stream — which
// would reconstruct the killed turn as a fresh open claim under a session that
// never ran it. So each closed claim's START COORDINATE is recorded in
// `turn_interruption` in the SAME transaction, exactly as synthesizeTurnEnds
// does. A claim closed without its durable end is the orphan this exists to
// prevent, so the two writes can never be separated.
func (m *Manager) CloseOriginTurns(workspace string, turnIDs []string, cause string) ([]string, error) {
	if workspace == "" {
		return nil, fmt.Errorf("ssm: CloseOriginTurns got an empty workspace")
	}
	if cause == "" {
		return nil, fmt.Errorf("ssm: CloseOriginTurns for workspace %q got an empty cause; a close that cannot name the lifecycle fact behind it is indistinguishable from a lost one", workspace)
	}
	named := make([]string, 0, len(turnIDs))
	for _, id := range turnIDs {
		// A LEGACY, IDENTITY-LESS CLAIM IS REFUSED RATHER THAN GUESSED AT. This
		// close is keyed by turn id across the whole workspace, and an empty id
		// would match every legacy claim in it — including turns some other
		// origin is still running.
		if id != "" {
			named = append(named, id)
		}
	}
	if len(named) == 0 {
		return nil, nil
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	tx, err := m.db.Begin()
	if err != nil {
		return nil, fmt.Errorf("ssm: begin origin turn close transaction: %w", err)
	}
	defer tx.Rollback()

	starts, err := openTurnStartsByID(tx, workspace, named)
	if err != nil {
		m.logf("ssm: origin turn close decision=reject workspace=%s cause=%s requested=%d error=%v",
			workspace, cause, len(named), err)
		return nil, err
	}
	if len(starts) == 0 {
		m.logf("ssm: origin turn close workspace=%s cause=%s requested=%d decision=no_open_claim — every turn this origin submitted had already been retired by its own end, so nothing is synthesized",
			workspace, cause, len(named))
		return nil, nil
	}
	if _, _, err := recordTurnInterruptions(tx, workspace, starts, cause, m.nextAt()); err != nil {
		m.logf("ssm: origin turn close decision=reject workspace=%s cause=%s requested=%d error=%v",
			workspace, cause, len(named), err)
		return nil, err
	}
	closed := make([]string, 0, len(starts))
	for _, s := range starts {
		changed, execErr := closeOpenTurnClaimByID(tx, workspace, s.turnID, cause)
		if execErr != nil {
			m.logf("ssm: origin turn close decision=reject workspace=%s cause=%s turn_id=%q error=%v",
				workspace, cause, s.turnID, execErr)
			return nil, execErr
		}
		if changed != 1 {
			// The rows were selected inside this very transaction, so a count
			// other than one means two claimants hold the same named turn id —
			// an identity the ledger guarantees is unique. That is an invariant
			// violation, not a condition to absorb.
			return nil, fmt.Errorf("ssm: origin turn close for workspace %q turn %q updated %d claims, want exactly the one open claim it selected", workspace, s.turnID, changed)
		}
		closed = append(closed, s.turnID)
	}
	if err := tx.Commit(); err != nil {
		return nil, fmt.Errorf("ssm: commit origin turn close transaction: %w", err)
	}
	m.logf("ssm: turn claims CLOSED BY THEIR ORIGIN workspace=%s cause=%s requested=%d closed=%s — the origin that submitted these turns reached its own terminal, so no party is left to observe their ends and the daemon writes them; each killed start's store coordinate is recorded so a replay reconstructs a matched pair rather than a fresh open claim",
		workspace, cause, len(named), formatClosedTurnIDs(closed))
	return closed, nil
}

// openTurnStartsByID names the OPEN claims among turnIDs together with the store
// coordinate of the start that opened each, in claim order.
//
// It is the turn-id-scoped twin of activeInterruptedStarts, and it resolves the
// coordinate by the SAME rule: a bridge's session is the one a replay presents
// when a vendor uuid rotated mid-turn, so it wins over the retired start's.
func openTurnStartsByID(tx *sql.Tx, workspace string, turnIDs []string) ([]interruptedStart, error) {
	var out []interruptedStart
	for _, id := range turnIDs {
		var startEventSessionID, bridgeEventSessionID string
		err := tx.QueryRow(`SELECT start_event_session_id, bridge_event_session_id
			FROM turn_lifecycle_claim
			WHERE workspace=? AND turn_id=? AND end_seq IS NULL
			ORDER BY claim_id LIMIT 1`, workspace, id).Scan(&startEventSessionID, &bridgeEventSessionID)
		if err == sql.ErrNoRows {
			continue
		}
		if err != nil {
			return nil, fmt.Errorf("ssm: read open turn claim %q for workspace %q: %w", id, workspace, err)
		}
		coordinate := startEventSessionID
		if bridgeEventSessionID != "" {
			coordinate = bridgeEventSessionID
		}
		out = append(out, interruptedStart{turnID: id, eventSessionID: coordinate})
	}
	return out, nil
}

// closeOpenTurnClaimByID ends one named turn's open claim, writing end_seq=0
// exactly as synthesizeTurnEnds does: no event produced this close, and that is
// what distinguishes it in the ledger. `end_seq IS NULL` stays the ONE
// definition of an active claim.
func closeOpenTurnClaimByID(tx *sql.Tx, workspace, turnID, cause string) (int64, error) {
	result, err := tx.Exec(`UPDATE turn_lifecycle_claim
			SET end_seq=0, end_cause=?
			WHERE workspace=? AND turn_id=? AND end_seq IS NULL`,
		cause, workspace, turnID)
	if err != nil {
		return 0, fmt.Errorf("ssm: close origin turn claim %q for workspace %q: %w", turnID, workspace, err)
	}
	changed, err := result.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("ssm: inspect closed origin turn claim %q for workspace %q: %w", turnID, workspace, err)
	}
	return changed, nil
}
