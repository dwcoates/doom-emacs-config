package ssm

import (
	"database/sql"
	"fmt"

	"claude-repld/internal/daemonturn"
)

// HEALING A REVIVE-COMPACT TURN WHOSE COMPACTION IS OVER AND WHOSE CLAIM IS NOT.
//
// THE DEFECT THIS CLEANS UP AFTER. A compact-first revival submits `/compact`
// as an ordinary prompt under its own turn id (sessioncontroller/revive.go,
// `revive-compact:<session>:<nonce>`), and the vendor's result for that turn is
// what closes the claim. The shim withheld that close whenever its pending
// `<task-notification>` queue was non-empty — and a revived conversation
// replays its whole transcript, so the queue refilled with entries from a
// process epoch that had already consumed them. The result was stored, the
// turn never ended, and the claim stayed open: the workspace rendered red,
// every submitted prompt queued behind a turn nothing was running, and the
// runtime restart guard refused the workspace by name for as long as the daemon
// lived. Observed live on 2026-08-08 at 14:10 in
// `model-selection-convergence-hwx`, whose compaction completed at 14:12:30 and
// whose claim was still open when the daemon was inspected.
//
// THE SHIM FIX DOES NOT HEAL WHAT IS ALREADY WRITTEN DOWN. It stops the next
// compaction from wedging; it cannot close a claim recorded before it shipped.
// That residue is durable, it survives every restart, and until it is retired
// the workspace stays unusable. So this reconciliation runs at Open, where the
// axis is warmed from the log.
//
// WHY IT IS SAFE HERE AND NOWHERE ELSE. Force-closing a claim is only honest
// when nothing could still be running behind it, and Open is exactly that
// moment: no shim is connected to a daemon that has just started, which is the
// same statement `hibernateEveryWorkspaceLocked` acts on two steps later. A
// LIVE daemon must never do this — an ordinary in-conversation `/compact`
// happens mid-turn and the turn legitimately continues after the compaction
// completes, so `compacted` is not a turn boundary and treating it as one would
// cut live turns short. See the note on ordinary compaction below.
//
// WHY IT IS SCOPED TO REVIVE-COMPACT TURNS. A revival's `/compact` turn exists
// to perform one compaction and nothing else; once the compacting axis has
// closed, that turn has done everything it was submitted to do. No other turn
// id carries that guarantee, so no other turn id is eligible.

// TurnCloseCompactionCompleteReconciled attributes an end the daemon wrote for
// a compact-first revival's turn whose compaction had demonstrably completed
// while its claim stayed open.
//
// It is deliberately DISTINCT from the ordinary synthesized causes: a claim
// carrying it is the residue of a past defect rather than an interruption, and
// the ledger should be able to say which.
const TurnCloseCompactionCompleteReconciled = "compaction_complete_reconciled"

// reviveCompactTurnPrefix is the identity a compact-first revival submits its
// compaction under. It is minted in sessioncontroller.newReviveCompactRequestID
// and is the only turn id whose completion this reconciliation may infer.
//
// It is the SHARED VOCABULARY'S CONSTANT rather than a second copy of it: the
// same prefix decides what the conversation curator withholds
// (sessioncontroller/contextcutexclude.go), and two strings that have to be
// kept equal for those two readings to agree is one string too many.
const reviveCompactTurnPrefix = daemonturn.ReviveCompactPrefix

// causeCompactionReconciled names the session-status row this reconciliation
// appends. It carries no store seq: the edge is the daemon's own reading of the
// log, exactly as the context-cut rows are.
const causeCompactionReconciled = "compaction_complete_reconciled"

// wedgedCompactionClaim is one open revive-compact claim together with the
// coordinates a durable end has to be recorded against.
type wedgedCompactionClaim struct {
	workspace      string
	claimant       string
	turnID         string
	startSeq       uint64
	eventSessionID string
}

// reconcileCompletedCompactionClaimsLocked closes every open revive-compact
// claim whose compaction the log shows completed. Caller holds m.mu.
//
// It never returns early on one workspace's failure: a claim it cannot heal is
// reported and the pass continues, because leaving the REMAINING wedged
// workspaces unusable over an unrelated read error is the larger harm. The
// first error is returned once the pass is done, so nothing is swallowed.
func (m *Manager) reconcileCompletedCompactionClaimsLocked() error {
	claims, err := openReviveCompactClaims(m.db)
	if err != nil {
		return err
	}
	if len(claims) == 0 {
		return nil
	}
	var firstErr error
	for _, c := range claims {
		completedAt, completed, err := compactionCompletedAfterStart(m.db, c.workspace, c.startSeq)
		if err != nil {
			m.warn("ssm: compaction claim reconciliation could not read the compacting evidence ws=%s claimant_session=%s turn_id=%q start_seq=%d: %v — the claim is left open rather than closed on an unread log",
				c.workspace, c.claimant, c.turnID, c.startSeq, err)
			if firstErr == nil {
				firstErr = err
			}
			continue
		}
		if !completed {
			// NOT A DEFECT AND NOT HEALED. The revival's compaction never
			// reported completion, so the claim names a turn whose outcome this
			// pass has no evidence about. It stays exactly as it is.
			m.logf("ssm: compaction claim reconciliation DECLINED ws=%s claimant_session=%s turn_id=%q start_seq=%d — the log holds no `%s` after this turn started, so there is no evidence the compaction ever completed and the claim is left open",
				c.workspace, c.claimant, c.turnID, c.startSeq, sigCompacted)
			continue
		}
		if err := m.closeCompletedCompactionClaimLocked(c, completedAt); err != nil {
			m.warn("ssm: compaction claim reconciliation FAILED ws=%s claimant_session=%s turn_id=%q: %v — the workspace stays latched behind a turn whose compaction is already over",
				c.workspace, c.claimant, c.turnID, err)
			if firstErr == nil {
				firstErr = err
			}
		}
	}
	return firstErr
}

// closeCompletedCompactionClaimLocked retires ONE wedged claim and settles the
// axis it latched, in a single transaction. Caller holds m.mu.
//
// The three writes are the same three closeStaleTurnLocked makes, and for the
// same reasons: the claim row (what turn liveness folds), the durable
// interruption keyed by the START'S STORE COORDINATE (what a later replay of
// the same stream consults, turninterruption.go), and the session-status row
// (what the workspace renders from). A claim retired without the second would
// come straight back the next time the stream replayed its start.
func (m *Manager) closeCompletedCompactionClaimLocked(c wedgedCompactionClaim, completedAt int64) error {
	tx, err := m.db.Begin()
	if err != nil {
		return fmt.Errorf("ssm: begin compaction claim reconciliation for workspace %q: %w", c.workspace, err)
	}
	defer tx.Rollback()
	if _, _, err := recordTurnInterruptions(tx, c.workspace,
		[]interruptedStart{{turnID: c.turnID, eventSessionID: c.eventSessionID}},
		TurnCloseCompactionCompleteReconciled, m.nextAt()); err != nil {
		return err
	}
	changed, err := supersedeOpenTurnClaim(tx, c.workspace, c.claimant, c.turnID, TurnCloseCompactionCompleteReconciled)
	if err != nil {
		return err
	}
	if changed != 1 {
		return fmt.Errorf("ssm: compaction claim reconciliation for workspace %q closed %d claims for turn %q, want exactly one", c.workspace, changed, c.turnID)
	}
	liveness, err := deriveTurnLiveness(tx, c.workspace)
	if err != nil {
		return err
	}
	top, _, err := sessionStatusTop(tx, c.workspace)
	if err != nil {
		return err
	}
	// THE ROW IS APPENDED ONLY WHEN THE AXIS STILL PAINTS A TURN THAT THE
	// DERIVATION NO LONGER HOLDS. Another claim standing in this workspace means
	// a turn really is in flight by the one fold that answers that question, and
	// the color belongs to it rather than to this repair.
	appended := false
	if runningTurnTokens[top] && !liveness.Active() {
		if err := appendRow(tx, c.workspace, c.claimant, sigIdle, causeCompactionReconciled, sql.NullInt64{}, m.nextAt(), ""); err != nil {
			return err
		}
		appended = true
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("ssm: commit compaction claim reconciliation for workspace %q: %w", c.workspace, err)
	}
	// WARN, because this is a past defect's residue surfacing rather than an
	// ordinary transition: a claim reaching this pass means a compaction
	// completed and its turn was never closed, and the workspace was unusable
	// for however long the daemon that recorded it ran.
	m.warn("ssm: revive-compact turn claim RECONCILED CLOSED ws=%s claimant_session=%s turn_id=%q start_seq=%d compacted_at=%d cause=%s appended_idle=%v liveness=%s — the compaction this turn was submitted to perform completed and no `TurnEnded` ever closed the claim, so the workspace stood behind a turn nothing was running; the end is written here so the turn band clears, queued prompts flow, and the restart guard releases",
		c.workspace, c.claimant, c.turnID, c.startSeq, completedAt,
		TurnCloseCompactionCompleteReconciled, appended, liveness)
	return nil
}

// openReviveCompactClaims names every OPEN claim whose turn id belongs to a
// compact-first revival, together with the store coordinate a durable end must
// be keyed on.
//
// The BRIDGE session wins over the start session when there is one, exactly as
// activeInterruptedStarts chooses: a mid-turn vendor uuid rotation means the
// replay that could resurrect this claim presents the bridged identity.
func openReviveCompactClaims(db *sql.DB) ([]wedgedCompactionClaim, error) {
	rows, err := db.Query(`SELECT workspace, claimant_session_id, turn_id, start_seq,
			start_event_session_id, bridge_event_session_id
		FROM turn_lifecycle_claim
		WHERE end_seq IS NULL AND turn_id LIKE ? ESCAPE '\'
		ORDER BY claim_id`, reviveCompactTurnPrefix+"%")
	if err != nil {
		return nil, fmt.Errorf("ssm: read open revive-compact turn claims: %w", err)
	}
	defer rows.Close()
	var out []wedgedCompactionClaim
	for rows.Next() {
		var c wedgedCompactionClaim
		var startSeq int64
		var startEventSessionID, bridgeEventSessionID string
		if err := rows.Scan(&c.workspace, &c.claimant, &c.turnID, &startSeq,
			&startEventSessionID, &bridgeEventSessionID); err != nil {
			return nil, fmt.Errorf("ssm: scan open revive-compact turn claim: %w", err)
		}
		c.startSeq = uint64(startSeq)
		c.eventSessionID = startEventSessionID
		if bridgeEventSessionID != "" {
			c.eventSessionID = bridgeEventSessionID
		}
		out = append(out, c)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("ssm: iterate open revive-compact turn claims: %w", err)
	}
	return out, nil
}

// compactionCompletedAfterStart reports whether the workspace's log holds a
// closed compacting window AFTER the turn that startSeq opened, and when.
//
// THE ORDERING IS WHAT MAKES THE EVIDENCE THIS TURN'S. A workspace accumulates
// compacting/compacted pairs over its whole life, and an OLDER one says nothing
// about a claim opened after it. So the turn's own `turn_started` row is located
// by its store seq first, and only a `compacted` appended after that row counts.
//
// A claim whose start carries no seq (a handshake-seeded claim the stream never
// caught up with) can be positioned against nothing, and reports no evidence
// rather than matching the first `compacted` in the log.
func compactionCompletedAfterStart(db *sql.DB, workspace string, startSeq uint64) (int64, bool, error) {
	if startSeq == 0 {
		return 0, false, nil
	}
	var startedAt sql.NullInt64
	if err := db.QueryRow(`SELECT MAX(at) FROM workspace_state
		WHERE workspace=? AND cause_kind=? AND cause_seq=?`,
		workspace, causeTurnStarted, int64(startSeq)).Scan(&startedAt); err != nil {
		return 0, false, fmt.Errorf("ssm: read turn start row for workspace %q seq=%d: %w", workspace, startSeq, err)
	}
	if !startedAt.Valid {
		return 0, false, nil
	}
	var compactedAt sql.NullInt64
	if err := db.QueryRow(`SELECT MIN(at) FROM workspace_state
		WHERE workspace=? AND state=? AND at>?`,
		workspace, sigCompacted, startedAt.Int64).Scan(&compactedAt); err != nil {
		return 0, false, fmt.Errorf("ssm: read compacted row for workspace %q after at=%d: %w", workspace, startedAt.Int64, err)
	}
	if !compactedAt.Valid {
		return 0, false, nil
	}
	return compactedAt.Int64, true, nil
}
