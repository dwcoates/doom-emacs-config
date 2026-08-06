package ssm

import (
	"database/sql"
	"fmt"
)

// CloseStaleTurn closes a workspace's standing `thinking` when the party that
// promised to report that turn's end can no longer do so, because the daemon
// has just STOPPED the shim behind it.
//
// THIS IS A TEARDOWN'S OWN OBLIGATION, not a fallback around a broken
// mechanism. The session-status lifecycle retires `thinking` on a `TurnEnded` and on nothing
// else. When the daemon kills a shim mid-turn that event can never arrive, so
// the latch is immortal: `verify-brilliant-fanout-uen` carried one from
// 2026-07-29 14:11:15 through every restart after it, with the shim answering
// INTERRUPT_OUTCOME_ALREADY_COMPLETE while the sidebar and footer both said
// "thinking". Two authorities and no reconciling edge. The daemon is the last
// party that can know the turn is over, and a ledger still claiming otherwise
// is corruption rather than honesty.
//
// It reports whether it WROTE the closing row. A false answer with a nil error
// is the good outcome: the shim's own `TurnEnded` beat the teardown to the axis
// and closed it honestly, so there was nothing stale left to close. The caller
// logs the two distinguishably.
//
// A PENDING PERMISSION IS RELEASED FIRST, exactly as ApplySessionRotated
// releases it and for the same reason: the row BURIES the `thinking` of the turn
// that asked the question, so left standing it hides the very latch this exists
// to clear — and the waiter behind it dies with the shim connection regardless.
// The release restores `thinking` only for a turn the log can still see, which
// is then closed here.
//
// soleSessionController says whether this stop is the workspace's OWN teardown. It decides
// one thing: whether an UNATTRIBUTED `thinking` — a row the SSM itself appended
// with no session id, such as the one a permission close restores — may be
// spent. It is exactly the row the observed bug wedges on, and it names no
// rival identity, so the workspace's own teardown closes it. A stop aimed at
// some OTHER record while a replacement session drives the workspace passes
// false: closing an unattributed claim there could blue out a live turn
// belonging to the replacement.
//
// A claim naming a DIFFERENT session is declined under either value. It is not
// this session's to spend, and that refusal is the same one InvalidateTurnClaim
// makes.
func (m *Manager) CloseStaleTurn(workspace, sessionID, reason string, soleSessionController bool) (bool, error) {
	if workspace == "" {
		return false, fmt.Errorf("ssm: CloseStaleTurn got an empty workspace")
	}
	if sessionID == "" {
		return false, fmt.Errorf("ssm: CloseStaleTurn for workspace %q got an empty session id; a turn claim can only be closed on behalf of the session whose shim was stopped", workspace)
	}
	if reason == "" {
		return false, fmt.Errorf("ssm: CloseStaleTurn for workspace %q session %q got an empty reason; the closing row must name the teardown that produced it", workspace, sessionID)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.closePermissionLocked(workspace, reason)
	return m.closeStaleTurnLocked(workspace, sessionID, reason, soleSessionController)
}

// closeStaleTurnLocked appends the closing session-status lifecycle row when the workspace's
// axis still tops out in a `thinking` this session may spend. Caller holds m.mu.
//
// IT APPENDS `idle` RATHER THAN `done`, the same choice ApplySessionRotated and
// InvalidateTurnClaim make: nothing is running, and claiming the turn COMPLETED
// would put a conclusion on the wire that no vendor message ever reported.
//
// THE ROW IS NOT SUBJECT TO THE READINESS NO-REGRESS GUARD, and that is what
// makes it able to cure the wedge. The `ready|session_started` row that would
// otherwise supersede a stale `thinking` is suppressed in Apply by the
// `active && (claimant == "" || claimant == sid)` arm, whose `active` is read
// off the stale row itself — the latch defends itself. This write goes straight
// to the log rather than through Apply, so the guard never sees it, and the
// readiness arriving afterwards reads a settled axis and is appended normally.
// IT RETIRES THE DURABLE CLAIM TOO, AND WRITES THE END WHERE A REPLAY WILL SEE
// IT. Closing only the status axis is what let the observed wedge happen: the
// axis went `idle`, the turn ledger kept the killed turn's claim, the workspace
// was superseded, and the replacement session replayed the same `turn_started`
// out of the store into a brand-new open claim. So the close now (a) ends the
// claims this session holds and (b) records each killed start's STORE coordinate
// in `turn_interruption`, which is the part a later replay actually consults.
// All three writes — claims, durable ends, axis row — are in one transaction.
func (m *Manager) closeStaleTurnLocked(workspace, sessionID, reason string, soleSessionController bool) (bool, error) {
	standing, claimant, err := turnClaim(m.db, workspace)
	if err != nil {
		return false, err
	}
	spendable := claimant == sessionID || (claimant == "" && soleSessionController)
	tx, err := m.db.Begin()
	if err != nil {
		return false, fmt.Errorf("ssm: begin stale turn close transaction: %w", err)
	}
	defer tx.Rollback()

	closed, err := synthesizeTurnEnds(tx, workspace, sessionID, TurnCloseShimStopped, m.nextAt())
	if err != nil {
		return false, err
	}
	// THE ONE FOLD, over the ledger this transaction has just retired.
	liveness, err := deriveTurnLiveness(tx, workspace)
	if err != nil {
		return false, err
	}
	cause := causeShimStopped + ":" + reason
	wrote := false
	switch {
	case !standing:
		m.logf("ssm: stale turn close ws=%s session=%s reason=%q sole_session_controller=%v — the session-status lifecycle holds no `thinking`, so the turn's own end already closed it honestly and nothing is appended",
			workspace, sessionID, reason, soleSessionController)
	case !spendable:
		m.logf("ssm: stale turn close ws=%s session=%s reason=%q sole_session_controller=%v DECLINED — the standing `thinking` is held by session=%q, which is not this stop's to spend",
			workspace, sessionID, reason, soleSessionController, claimant)
	case liveness.Active():
		// A turn is STILL live after this session's claims were retired — it
		// belongs to some other session driving this workspace. The derivation
		// is the authority on the color, and it says a turn runs.
		m.logf("ssm: stale turn close ws=%s session=%s reason=%q sole_session_controller=%v closed=%s liveness=%s — a turn remains in flight after this session's claims were retired, so the color is left exactly as the derivation holds it",
			workspace, sessionID, reason, soleSessionController, formatClosedTurnIDs(closed), liveness)
	default:
		if err := appendRow(tx, workspace, sessionID, sigIdle, cause, sql.NullInt64{}, m.nextAt(), ""); err != nil {
			return false, err
		}
		wrote = true
	}
	if err := tx.Commit(); err != nil {
		return false, fmt.Errorf("ssm: commit stale turn close transaction: %w", err)
	}
	if len(closed) > 0 {
		m.logf("ssm: turn claims CLOSED BY A SHIM STOP ws=%s session=%s reason=%q closed=%s cause=%s — the daemon stopped the shim these turns were running in, so their ends can never arrive; each killed start's store coordinate is recorded durably so a later replay of the same stream reconstructs a matched pair rather than a fresh open claim",
			workspace, sessionID, reason, formatClosedTurnIDs(closed), TurnCloseShimStopped)
	}
	if !wrote {
		return false, nil
	}
	m.logf("ssm: stale turn CLOSED ws=%s session=%s reason=%q sole_session_controller=%v claimant=%q — the shim behind the running turn is gone, so its end can never arrive and the session-status lifecycle is reconciled to `idle` rather than latched in `thinking`",
		workspace, sessionID, reason, soleSessionController, claimant)
	return true, m.reresolveLocked(workspace, cause, 0)
}

// topOrEmpty reads the workspace's newest session-status token inside tx,
// answering "" for a read failure so the caller's switch treats an unreadable
// axis as nothing to close. The error itself cannot be swallowed here; it
// resurfaces on the very next read the transaction makes.
func topOrEmpty(tx *sql.Tx, workspace string) string {
	top, _, err := sessionStatusTop(tx, workspace)
	if err != nil {
		return ""
	}
	return top
}
