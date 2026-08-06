package ssm

import (
	"database/sql"
	"fmt"
)

// THIS FILE IS THE DAEMON'S ONE ANSWER TO "IS A TURN IN FLIGHT".
//
// THE DEFECT IT CLOSES. That question used to have two answers, folded from
// the same store events by two different rules:
//
//   - the durable turn ledger (turnclaims.go) folded TurnStarted/TurnEnded into
//     `turn_lifecycle_claim`, keyed by CLAIMANT SESSION, and the prompt queue's
//     "turn in flight" test, the drain hold and the interrupt gate all read it;
//   - the session-status axis (ssm.go Apply) folded the SAME events into
//     `workspace_state`, deduplicated by (event_session_id, cause_seq), and the
//     workspace COLOR read that.
//
// A supersede mid-turn and a store replay under the replacement session hit the
// two rules differently: the ledger saw a fresh claim (new claimant), the axis
// saw a duplicate (same store coordinate) and wrote nothing. The sidebar stayed
// green while every prompt queued behind a turn that had been killed minutes
// earlier, and no TurnEnded was ever coming for it.
//
// THE SHAPE OF THE FIX. There is exactly ONE fold — deriveTurnLiveness — and it
// produces exactly one VALUE, TurnLiveness. The queue, the drain hold and the
// interrupt gate hold that value. The color is PAINTED FROM that value:
// applyTurnBoundaryLocked appends the session-status row the derivation implies,
// in the same transaction that moved the ledger, so the axis cannot carry a turn
// band the derivation does not hold.
//
// A NEW CONSUMER CANNOT REACH A SECOND ANSWER. TurnLiveness has no exported
// fields and no exported constructor, so the only way to obtain one is to ask
// this package. Manager.Apply — the one door the rest of the daemon pushes
// events through — REFUSES turn boundaries outright (see Apply), so folding the
// event stream into a private answer is not something a caller can do by
// accident or on purpose.

// TurnLiveness is THE answer to "is a turn in flight in this workspace", and
// the only value in the daemon that answers it.
//
// Its fields are unexported and it has no exported constructor on purpose: a
// value of this type can only come from deriveTurnLiveness, which is the single
// fold. `derived` is what separates a genuine answer from Go's zero value — an
// undeclared TurnLiveness is not "no turn in flight", it is NOT AN ANSWER, and
// every accessor refuses it rather than reporting the workspace idle on the
// strength of a struct nobody filled in.
type TurnLiveness struct {
	// workspace is the workspace the fold ran over. It travels with the
	// value so a consumer holding one cannot silently attribute it to
	// another workspace.
	workspace string
	// claims are the durable turn ledger's OPEN claims for the workspace, in
	// claim order. Scoped to the workspace rather than to a claimant: a
	// claim opened by a session that has since been superseded is still a
	// turn this workspace has not seen the end of, and claimant scoping is
	// precisely what let a replacement session read the workspace as idle.
	claims []string
	// submitting is the daemon's own commitment to a prompt whose TurnStarted
	// has not reached the store yet — the `submitting` row MarkPromptAccepted
	// appends. It is a turn in flight by every consumer's reckoning (a second
	// prompt must queue behind it, a drain must hold for it) and the ledger
	// cannot yet name it, so it is the second leg of the one fold rather than
	// a separate test somebody has to remember to run.
	submitting bool
	// derived is true only on a value this package's fold produced.
	derived bool
}

// ErrTurnLivenessUnderived reports that a TurnLiveness that nothing derived was
// used as an answer.
//
// It is an INVARIANT VIOLATION, not a runtime condition. The only way to hold a
// TurnLiveness is to receive one from ssm, so a zero value reaching a consumer
// means a caller declared its own and passed it off as a derivation — exactly
// the second authority this type exists to make unreachable. It fails hard
// rather than defaulting to "no turn in flight", because that default is the
// bug: it renders a workspace green and delivers prompts into a live turn.
var ErrTurnLivenessUnderived = fmt.Errorf("ssm: turn liveness value was never derived")

// Derived reports whether this value came from the fold. It is what a consumer
// asserts on before acting; see ErrTurnLivenessUnderived.
func (l TurnLiveness) Derived() bool { return l.derived }

// Workspace names the workspace the fold ran over.
func (l TurnLiveness) Workspace() string { return l.workspace }

// Active reports whether a turn is in flight — the single fact the color, the
// prompt queue, the drain hold and the interrupt gate all act on.
func (l TurnLiveness) Active() bool { return len(l.claims) > 0 || l.submitting }

// Claims returns the open durable claims, in claim order. The slice is a copy:
// a consumer cannot mutate the derivation out from under another.
func (l TurnLiveness) Claims() []string {
	out := make([]string, len(l.claims))
	copy(out, l.claims)
	return out
}

// Submitting reports whether the live leg is a daemon commitment the ledger
// cannot yet name (see the field).
func (l TurnLiveness) Submitting() bool { return l.submitting }

// Name reports the turn's id and whether the derivation can name it. A LEGACY
// start carries no turn id at all, so a workspace can hold claims and still
// name nothing; that is a phase, never an empty string a caller has to check.
func (l TurnLiveness) Name() (string, bool) {
	for _, id := range l.claims {
		if id != "" {
			return id, true
		}
	}
	return "", false
}

func (l TurnLiveness) String() string {
	if !l.derived {
		return "turn-liveness(UNDERIVED)"
	}
	return fmt.Sprintf("turn-liveness(ws=%s active=%v claims=%s submitting=%v)",
		l.workspace, l.Active(), formatClosedTurnIDs(l.claims), l.submitting)
}

// rowQuerier is the read half of a *sql.DB or a *sql.Tx, so the fold runs
// identically inside the ledger's own transaction and outside it. Deriving
// inside the transaction that just moved the ledger is what makes the axis row
// and the returned value the SAME derivation rather than two reads that agree.
type rowQuerier interface {
	Query(query string, args ...any) (*sql.Rows, error)
	QueryRow(query string, args ...any) *sql.Row
}

// deriveTurnLiveness is THE fold. Nothing else in the daemon may answer "is a
// turn in flight", and nothing else constructs a TurnLiveness.
func deriveTurnLiveness(q rowQuerier, workspace string) (TurnLiveness, error) {
	if workspace == "" {
		return TurnLiveness{}, fmt.Errorf("ssm: deriving turn liveness requires a workspace")
	}
	rows, err := q.Query(`SELECT turn_id FROM turn_lifecycle_claim
		WHERE workspace=? AND end_seq IS NULL ORDER BY claim_id`, workspace)
	if err != nil {
		return TurnLiveness{}, fmt.Errorf("ssm: derive turn liveness claims for workspace %q: %w", workspace, err)
	}
	defer rows.Close()
	l := TurnLiveness{workspace: workspace, derived: true}
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return TurnLiveness{}, fmt.Errorf("ssm: scan turn liveness claim for workspace %q: %w", workspace, err)
		}
		l.claims = append(l.claims, id)
	}
	if err := rows.Err(); err != nil {
		return TurnLiveness{}, fmt.Errorf("ssm: iterate turn liveness claims for workspace %q: %w", workspace, err)
	}
	if err := rows.Close(); err != nil {
		return TurnLiveness{}, fmt.Errorf("ssm: close turn liveness claims for workspace %q: %w", workspace, err)
	}
	// THE SUBMIT LEG. Read only when the ledger names nothing: a ledger claim
	// is the stronger statement, and a prompt-edge row underneath one is the
	// same turn seen a moment earlier.
	//
	// BOTH PROMPT EDGES COUNT, and omitting the second one is a bug this
	// derivation must not repeat. `prompt_accepted` is the daemon committing to
	// a submit; `prompt_delivered` is the shim acking it. The durable
	// TurnStarted arrives only after BOTH, so a leg that read `submitting`
	// alone would report the workspace idle for the whole span between the ack
	// and the stream — which is exactly when the agent is working hardest, and
	// exactly when a second prompt must queue rather than be delivered.
	if len(l.claims) == 0 {
		commitment, err := promptCommitmentStands(q, workspace)
		if err != nil {
			return TurnLiveness{}, err
		}
		l.submitting = commitment
	}
	return l, nil
}

// promptCommitmentStands reports whether the workspace's newest session-status
// row is a running turn THIS DAEMON committed to and the stream has not yet
// caught up with.
//
// It is scoped to the two prompt causes deliberately. A `thinking` row written
// by a turn boundary is already accounted for by the ledger leg above, so
// counting it here as well would let a closed claim's leftover row keep a
// workspace alive — the very latch this work removes.
func promptCommitmentStands(q rowQuerier, workspace string) (bool, error) {
	var state, causeKind string
	err := q.QueryRow(`SELECT state, cause_kind FROM workspace_state
		 WHERE workspace = ? AND state IN `+sessionStatusMembers+`
		 ORDER BY at DESC LIMIT 1`, workspace).Scan(&state, &causeKind)
	if err == sql.ErrNoRows {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: read prompt commitment for workspace %q: %w", workspace, err)
	}
	if !runningTurnTokens[state] {
		return false, nil
	}
	return causeKind == causePromptAccepted || causeKind == causePromptDelivered, nil
}

// TurnLiveness derives the workspace's turn liveness for a caller outside this
// package. It is the ONLY exported way to obtain the value.
func (m *Manager) TurnLiveness(workspace string) (TurnLiveness, error) {
	if workspace == "" {
		err := fmt.Errorf("ssm: TurnLiveness requires a workspace")
		m.logf("ssm: turn liveness decision=reject_validation workspace=%q error=%v", workspace, err)
		return TurnLiveness{}, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return deriveTurnLiveness(m.db, workspace)
}

// retireTurnsLocked is the ONE way a daemon-side reconciliation ends turns it
// can no longer receive an end for, and it is what keeps the three of them
// (a vendor-session rotation, a deleted session's claim, an ALREADY_COMPLETE
// ack) from each moving the color while leaving the ledger standing.
//
// Every such reconciliation used to append an `idle` row and stop there. With
// turn liveness derived from the ledger, that is now a contradiction rather
// than a partial fix: the axis would say idle while the derivation — which the
// prompt queue, the drain hold and the interrupt gate all read — still held the
// turn open. So the ledger is retired first, the durable end is recorded against
// each killed start's store coordinate (turninterruption.go), and the caller
// paints from the derivation that produces.
//
// An EMPTY claimantSessionID retires every claimant in the workspace. That is
// the rotation's case: the vendor minted a new conversation, so no claim in this
// workspace can ever be ended by the identity that opened it.
//
// Caller holds m.mu and owns tx.
func (m *Manager) retireTurnsLocked(tx *sql.Tx, workspace, claimantSessionID, cause string) ([]string, TurnLiveness, error) {
	claimants := []string{claimantSessionID}
	if claimantSessionID == "" {
		all, err := claimantsHoldingTurns(tx, workspace)
		if err != nil {
			return nil, TurnLiveness{}, err
		}
		claimants = all
	}
	var closed []string
	for _, claimant := range claimants {
		ended, err := synthesizeTurnEnds(tx, workspace, claimant, cause, m.nextAt())
		if err != nil {
			return nil, TurnLiveness{}, err
		}
		closed = append(closed, ended...)
	}
	l, err := deriveTurnLiveness(tx, workspace)
	if err != nil {
		return nil, TurnLiveness{}, err
	}
	return closed, l, nil
}

// claimantsHoldingTurns names every session that holds an OPEN claim in the
// workspace, in claim order and without repeats.
func claimantsHoldingTurns(tx *sql.Tx, workspace string) ([]string, error) {
	rows, err := tx.Query(`SELECT DISTINCT claimant_session_id FROM turn_lifecycle_claim
		WHERE workspace=? AND end_seq IS NULL`, workspace)
	if err != nil {
		return nil, fmt.Errorf("ssm: read turn claimants for workspace %q: %w", workspace, err)
	}
	defer rows.Close()
	var out []string
	for rows.Next() {
		var claimant string
		if err := rows.Scan(&claimant); err != nil {
			return nil, fmt.Errorf("ssm: scan turn claimant for workspace %q: %w", workspace, err)
		}
		out = append(out, claimant)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("ssm: iterate turn claimants for workspace %q: %w", workspace, err)
	}
	return out, nil
}

// runningTurnTokens are the session-status tokens that PAINT a turn in flight.
// They are the axis's rendering of the derivation, never an independent test of
// it: applyTurnBoundaryLocked chooses among them from the TurnLiveness value.
var runningTurnTokens = map[string]bool{
	sigThinking:   true,
	sigSubmitting: true,
}

// turnBandToken is the session-status token the derivation implies for a
// workspace whose turn is LIVE. A named or legacy ledger claim is `thinking`;
// a daemon commitment the ledger cannot name yet is `submitting`.
//
// It returns ok=false for a derivation that holds no live turn — the axis row
// for that case names HOW the turn ended, which is the boundary event's own
// business and not something liveness can answer.
func turnBandToken(l TurnLiveness) (string, bool) {
	switch {
	case len(l.claims) > 0:
		return sigThinking, true
	case l.submitting:
		return sigSubmitting, true
	default:
		return "", false
	}
}
