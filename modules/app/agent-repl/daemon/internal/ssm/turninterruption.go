package ssm

import (
	"database/sql"
	"fmt"
)

// THE DURABLE END OF A TURN NOTHING WILL EVER END.
//
// WHY THE CLAIM LEDGER ALONE IS NOT ENOUGH. `turn_lifecycle_claim` rows are
// keyed by CLAIMANT SESSION, and a workspace re-mints its claimant on every
// CreateSession. So when a supersede kills a live turn, closing that turn's
// claim closes it FOR THE DEAD CLAIMANT ONLY. The replacement session then
// subscribes to the very same store stream, replays the very same
// `turn_started`, and — under its own fresh claimant — opens a NEW claim for a
// turn that was killed minutes ago. The observed wedge is exactly that: a start
// with no end, reconstructed out of the store by a session that never ran it,
// with no `TurnEnded` in the seq space that could ever close it.
//
// RETIRING THE CLAIM IS NECESSARY BUT NOT SUFFICIENT, because THE REPLAY IS
// WHAT RE-CREATES IT. The record that makes recurrence impossible has to be
// keyed by something the replayed event itself carries — its STORE COORDINATE,
// (event_session_id, turn_id) — rather than by whichever daemon session
// happened to be driving when the turn died.
//
// That is this table. A daemon-synthesized end writes one row here naming the
// store coordinate of the start it just killed; recordTurnStart consults it, and
// a start whose coordinate is listed is admitted AND CLOSED in the same
// statement. The replacement reconstructs a MATCHED START/END PAIR from the same
// stream that produced the orphan, so turn liveness derives `idle` rather than a
// turn nobody can finish.
//
// IT IS NOT A REPAIR PASS. Nothing scans for disagreement, nothing times a claim
// out, and nothing notices after the fact that the ledger and the axis have
// drifted. The record is written at the instant the daemon kills the turn, and
// it is read on the one edge that could otherwise resurrect it.

// TurnCloseShimStopped attributes a synthesized end to THIS daemon stopping the
// shim the turn was running in — a supersede, a teardown, a rotation. It is the
// third of the synthesized causes and the one the observed wedge needed: the
// turn was cut by a stop the daemon itself commanded, so the daemon is the only
// party that can write the end, and it says so.
const TurnCloseShimStopped = "interrupted_by_shim_stop"

// interruptedStart is one killed turn's STORE coordinate: the identity a later
// replay of the same stream will present, and therefore the only key by which a
// durable end can recognize it.
type interruptedStart struct {
	turnID         string
	eventSessionID string
}

// recordTurnInterruptions durably names the killed turns' store coordinates, so
// a replay of their starts reconstructs a matched pair instead of an orphan.
//
// A TURN WITH NO IDENTITY IS REPORTED, NOT SILENTLY DROPPED. A legacy start
// carries no turn id, so its coordinate cannot distinguish it from the next
// legacy start in the same conversation, and a durable end keyed on it would
// close the WRONG turn on replay. Those are counted and returned so the caller
// can say out loud that a claim was retired without a durable end behind it.
func recordTurnInterruptions(tx *sql.Tx, workspace string, starts []interruptedStart, cause string, at int64) (recorded int, unidentifiable int, err error) {
	if cause == "" {
		return 0, 0, fmt.Errorf("ssm: refusing to record a turn interruption for workspace %q with no cause; an unattributed durable end is indistinguishable from a lost one", workspace)
	}
	for _, s := range starts {
		if s.turnID == "" || s.eventSessionID == "" {
			unidentifiable++
			continue
		}
		if _, execErr := tx.Exec(`INSERT INTO turn_interruption(
				workspace, start_event_session_id, turn_id, cause, at
			) VALUES (?,?,?,?,?)
			ON CONFLICT(workspace, start_event_session_id, turn_id) DO NOTHING`,
			workspace, s.eventSessionID, s.turnID, cause, at); execErr != nil {
			return recorded, unidentifiable, fmt.Errorf("ssm: record turn interruption %q for workspace %q: %w", s.turnID, workspace, execErr)
		}
		recorded++
	}
	return recorded, unidentifiable, nil
}

// interruptedStartCause reports the durable end recorded for a start's store
// coordinate, and whether one exists.
//
// It is consulted on exactly one edge — recordTurnStart — because that is the
// only place a killed turn can come back: a replay of its own start under a
// session that never ran it.
func interruptedStartCause(tx *sql.Tx, workspace, eventSessionID, turnID string) (string, bool, error) {
	if turnID == "" || eventSessionID == "" {
		return "", false, nil
	}
	var cause string
	err := tx.QueryRow(`SELECT cause FROM turn_interruption
		WHERE workspace=? AND start_event_session_id=? AND turn_id=?`,
		workspace, eventSessionID, turnID).Scan(&cause)
	if err == sql.ErrNoRows {
		return "", false, nil
	}
	if err != nil {
		return "", false, fmt.Errorf("ssm: read turn interruption %q for workspace %q: %w", turnID, workspace, err)
	}
	return cause, true, nil
}

// activeInterruptedStarts names every OPEN claim held by claimantSessionID
// together with the store coordinate of the start that opened it. It is what
// synthesizeTurnEnds hands recordTurnInterruptions: the claim rows are being
// closed for one claimant, and the coordinates are what survives the claimant.
func activeInterruptedStarts(tx *sql.Tx, workspace, claimantSessionID string) ([]interruptedStart, error) {
	rows, err := tx.Query(`SELECT turn_id, start_event_session_id, bridge_event_session_id
		FROM turn_lifecycle_claim
		WHERE workspace=? AND claimant_session_id=? AND end_seq IS NULL
		ORDER BY claim_id`, workspace, claimantSessionID)
	if err != nil {
		return nil, fmt.Errorf("ssm: read active turn starts for workspace %q: %w", workspace, err)
	}
	defer rows.Close()
	var out []interruptedStart
	for rows.Next() {
		var turnID, startEventSessionID, bridgeEventSessionID string
		if err := rows.Scan(&turnID, &startEventSessionID, &bridgeEventSessionID); err != nil {
			return nil, fmt.Errorf("ssm: scan active turn start for workspace %q: %w", workspace, err)
		}
		// THE BRIDGE'S SESSION IS THE ONE A REPLAY WILL PRESENT. When a vendor
		// session uuid rotates mid-turn, the claim's start belongs to the
		// PREVIOUS uuid and the bridge names the one the stream continues
		// under. A durable end keyed on the retired uuid would never be found
		// by the replay that matters, so the live coordinate wins when there is
		// one.
		coordinate := startEventSessionID
		if bridgeEventSessionID != "" {
			coordinate = bridgeEventSessionID
		}
		out = append(out, interruptedStart{turnID: turnID, eventSessionID: coordinate})
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("ssm: iterate active turn starts for workspace %q: %w", workspace, err)
	}
	return out, nil
}
