package ssm

import (
	"database/sql"
	"fmt"
	"time"
)

// defaultClearingTimeout bounds the clearing axis.
//
// A `/clear` is dispatched by the daemon and closed by whichever of its two
// completion edges lands first: the vendor session rotation the clear itself
// causes (ApplySessionRotated, daemon-local and reliable), or a ContextCleared
// the SIDECAR produces off the vendor's transcript, several processes away.
//
// This bound backstops the case where NEITHER arrives: the vendor refused the
// command outright, so nothing rotated, and no transcript line was ever written
// for the sidecar to read. A red workspace that no arriving event can ever
// release is exactly the wedge it exists to prevent.
const defaultClearingTimeout = 60 * time.Second

// ApplyClearing opens or closes the CLEARING axis: the user asked for the
// conversation's context to be discarded and the agent has not yet reported
// doing it.
//
// THE DAEMON IS THE SOURCE OF TRUTH FOR THE OPENING EDGE. Nothing in the event
// stream announces a clear as it begins — the vendor's own report of it is the
// ContextCleared that arrives when it has already HAPPENED — so the only place
// that knows a clear is in flight is the dispatch site that sent it.
//
// THE AXIS CANNOT WEDGE. Opening arms a watchdog (ClearingTimeout); if the
// ContextCleared never lands the watchdog appends the closing row itself with
// a loud log, and the workspace falls back to whatever the other axes say. A
// clear that never completed is a real anomaly, so it is reported rather than
// absorbed.
//
// THE ROTATION CLOSES IT. A `/clear` retires the vendor session uuid and
// bounces the shim mid-window, and that rotation is the daemon's only
// GUARANTEED observation that the dispatched clear took effect. The axis used
// to be held open across it, on the reasoning that the ContextCleared belongs
// to the new identity and is precisely the event still expected — but that
// event has exactly one producer, the shim-sidecar reading the vendor's
// transcript off disk (shim-claude-sidecar clearcompact.go), and a workspace
// whose sidecar is not running, whose transcript line never landed, or whose
// new identity the sidecar has not discovered yet produces none. Held open,
// every such clear rode the full ClearingTimeout and reported itself as a
// wedge. The rotation is a narrower and earlier truth, and it is available on
// every clear the vendor actually performed.
//
// It is a CLOSING edge only, and only for an axis this daemon opened. Nothing
// else opens the axis (the dispatch site is the sole opener), so a rotation
// with no clear in flight — a resume, a vendor-side re-key — finds it shut and
// appends nothing.
//
// THE WATCHDOG IS NOT WEAKENED. It stays armed for every clear whose rotation
// has not landed, which is the case it was written for (a vendor that refused
// the command outright rotates nothing at all), and the ContextCleared still
// closes the axis when it arrives first.
//
// REASON is a short note recorded as the cause detail (the dispatched command,
// or the edge that closed it).
func (m *Manager) ApplyClearing(workspace string, clearing bool, reason string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyClearing got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if clearing {
		// Re-arm before the append: a second `/clear` dispatched while the
		// first is still open renews the deadline rather than inheriting the
		// old one, which would expire an in-flight clear early.
		m.armClearingWatchdogLocked(workspace)
	} else {
		m.disarmClearingWatchdogLocked(workspace)
	}
	return m.applyCutLocked(workspace, sigClearing, sigCleared, causeClearing, clearing, reason)
}

// ApplyCompacting opens or closes the COMPACTING axis: the agent is
// summarizing the conversation rather than answering.
//
// The opening edge is the vendor's own `status="compacting"` report (the same
// one the progress footer's compaction window is folded from); the closing
// edges are that status going empty, the first-class ContextCompacted, the
// turn ending, and a session rotation. FOUR closers rather than one because
// the vendor's status is a best-effort ticker, not a transaction: a compaction
// interrupted by the turn dying, or by a `/clear` rotating the identity out
// from under it, would otherwise leave the window open with nothing left that
// could close it.
func (m *Manager) ApplyCompacting(workspace string, compacting bool, reason string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyCompacting got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.applyCutLocked(workspace, sigCompacting, sigCompacted, causeCompacting, compacting, reason)
}

// applyCutLocked moves one context-cut axis, appending a row only when the
// axis actually changes state. Caller holds mu.
//
// A no-op edge is LOGGED rather than dropped silently: a ContextCompacted with
// no window open means the opening status never reached this daemon, and a
// second open means two cuts overlapped — both are worth seeing.
func (m *Manager) applyCutLocked(workspace, openToken, closeToken, causeKind string, open bool, reason string) error {
	was, err := cutOpen(m.db, workspace, openToken, closeToken)
	if err != nil {
		return err
	}
	if was == open {
		m.logf("ssm: %s axis unchanged ws=%s open=%t reason=%q — no row appended", openToken, workspace, open, reason)
		return nil
	}
	token := closeToken
	if open {
		token = openToken
	}
	cause := causeKind
	if reason != "" {
		cause = causeKind + ":" + reason
	}
	// Daemon-local: the edge is the daemon's own reading of the cut, so it
	// carries no store seq and no task id.
	if err := appendRow(m.db, workspace, "", token, cause, sql.NullInt64{}, m.nextAt(), ""); err != nil {
		return err
	}
	return m.reresolveLocked(workspace, cause, 0)
}

// closeCompactingLocked closes an open compacting window on a bounding edge
// (turn end, session rotation) that no compaction can outlive. Caller holds
// mu.
//
// A failure is loud-logged rather than returned: the caller is a turn end or a
// rotation whose OWN work must still complete, and losing that to a failed
// window close would be the larger harm. The error is never absorbed silently.
func (m *Manager) closeCompactingLocked(workspace, reason string) {
	open, err := cutOpen(m.db, workspace, sigCompacting, sigCompacted)
	if err != nil {
		m.logf("ssm: reading the compacting axis FAILED ws=%s reason=%s: %v — the window may stay open until the next compaction closes it", workspace, reason, err)
		return
	}
	if !open {
		return
	}
	m.logf("ssm: compacting window closed by %s ws=%s — a compaction cannot outlive it, and holding the phase word here would wedge the workspace red", reason, workspace)
	if err := m.applyCutLocked(workspace, sigCompacting, sigCompacted, causeCompacting, false, reason); err != nil {
		m.logf("ssm: closing the compacting axis FAILED ws=%s reason=%s: %v", workspace, reason, err)
	}
}

// closeClearingLocked closes an open clearing axis on the vendor session
// rotation the dispatched `/clear` itself caused. Caller holds mu.
//
// THE ROTATION IS THE PROOF. The vendor mints a fresh transcript identity for a
// recognized `/clear` and for nothing else this daemon dispatches, so a
// rotation arriving while the axis stands open is the clear reporting that it
// happened — earlier and more reliably than the sidecar's ContextCleared, which
// is produced several processes away off a file the sidecar may not yet be
// tailing under the new uuid.
//
// The deadline is disarmed with the axis, exactly as the ContextCleared path
// disarms it: a timer left live would later expire an axis some LATER clear had
// legitimately opened.
//
// A failure is loud-logged rather than returned, for the same reason
// closeCompactingLocked's is: the caller is a rotation whose own reconciliation
// must still complete. The error is never absorbed silently.
func (m *Manager) closeClearingLocked(workspace, reason string) {
	open, err := cutOpen(m.db, workspace, sigClearing, sigCleared)
	if err != nil {
		m.logf("ssm: reading the clearing axis FAILED ws=%s reason=%s: %v — the axis may stay open until its watchdog expires it", workspace, reason, err)
		return
	}
	if !open {
		return
	}
	m.disarmClearingWatchdogLocked(workspace)
	m.logf("ssm: clearing axis closed by %s ws=%s — the vendor retired the transcript identity, which is the dispatched /clear reporting that it took effect; waiting for a ContextCleared that has no guaranteed producer under the new identity is what left this axis to expire", reason, workspace)
	if err := m.applyCutLocked(workspace, sigClearing, sigCleared, causeClearing, false, reason); err != nil {
		m.logf("ssm: closing the clearing axis FAILED ws=%s reason=%s: %v", workspace, reason, err)
	}
}

// armClearingWatchdogLocked (re)starts the workspace's clearing deadline.
// Caller holds mu.
func (m *Manager) armClearingWatchdogLocked(workspace string) {
	m.disarmClearingWatchdogLocked(workspace)
	m.clearingTimers[workspace] = m.afterFunc(m.clearingTimeout, func() {
		m.expireClearing(workspace)
	})
}

// disarmClearingWatchdogLocked cancels a pending deadline. Caller holds mu.
func (m *Manager) disarmClearingWatchdogLocked(workspace string) {
	if t, ok := m.clearingTimers[workspace]; ok {
		t.Stop()
		delete(m.clearingTimers, workspace)
	}
}

// expireClearing closes a clearing axis whose ContextCleared never arrived.
//
// LOUD, always. This runs only when a clear the daemon dispatched produced no
// observable completion within the bound, which means either the sidecar is
// not reading the transcript or the vendor never performed the clear. Both are
// faults worth a line in the log; what must NOT happen is the workspace
// staying red forever because the closing event is not coming.
//
// It cannot return an error (it is a timer callback), so every failure inside
// it is logged at the same volume.
func (m *Manager) expireClearing(workspace string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	delete(m.clearingTimers, workspace)
	if m.closed {
		return
	}
	open, err := cutOpen(m.db, workspace, sigClearing, sigCleared)
	if err != nil {
		m.logf("ssm: CLEARING WATCHDOG could not read the axis ws=%s: %v — the workspace may stay in `clearing`", workspace, err)
		return
	}
	if !open {
		return
	}
	m.logf("ssm: CLEARING EXPIRED ws=%s after %s — the dispatched /clear never produced a ContextCleared; the axis is being released so the workspace resolves on its other axes, but the clear itself is unaccounted for",
		workspace, m.clearingTimeout)
	if err := m.applyCutLocked(workspace, sigClearing, sigCleared, causeClearing, false, "watchdog_expired"); err != nil {
		m.logf("ssm: CLEARING WATCHDOG failed to release the axis ws=%s: %v", workspace, err)
	}
}

// rearmClearingWatchdogsLocked re-arms a deadline for every workspace whose
// clearing axis is open in the persisted log. Caller holds mu (warm).
//
// Without it a daemon restart during a clear would leave a durable open axis
// with no timer behind it, which is the wedge the watchdog exists to prevent —
// reintroduced by the one event the watchdog cannot see.
func (m *Manager) rearmClearingWatchdogsLocked() error {
	open, err := openCutWorkspaces(m.db, sigClearing, sigCleared)
	if err != nil {
		return err
	}
	for _, ws := range open {
		m.logf("ssm: clearing watchdog re-armed ws=%s — the axis was open in the log at startup", ws)
		m.armClearingWatchdogLocked(ws)
	}
	return nil
}
