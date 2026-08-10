package sessioncontroller

import (
	"claude-repld/internal/ssm"
)

// keepalivedeadline.go — THE PING'S OWN CLOSING EDGE.
//
// # The wedge
//
// A keep-alive ping's claim is cleared by exactly one thing: the ping's own turn
// end (`noteKeepAliveTurnEndedLocked`). That is correct for every ping that
// ends. For a ping whose end never arrives it is the whole defect, because the
// claim is not a bookkeeping detail — it is load-bearing in four places at once:
//
//   - `keepAliveEligibleLocked` declines every later tick with
//     `keep_alive_in_flight`, so the feature stops for that session entirely;
//   - `keepAliveHoldTurnLocked` parks every real prompt behind a turn id nothing
//     will ever end, so the user's work never leaves the queue;
//   - the durable claim reads as a live turn to the hibernation lease, which
//     refuses to reclaim ~500MB from a session doing nothing; and
//   - the same claim reads as a turn in flight to the restart guards, which
//     refuse a deploy over work that finished hours ago.
//
// Observed live: a `ka_` claim open for hours on a session whose pings finish in
// seconds, holding all four of those.
//
// # Why the deadline is a comparison rather than a timer
//
// The bound itself lives in `internal/keepalive` beside every other keep-alive
// decision, and it is evaluated the way they all are — a time-since comparison
// against a remembered instant, at the sweep the policy already runs on. A timer
// would be a second, weaker representation of the same fact: it dies with the
// daemon, it does not advance across a laptop sleep, and it cannot tell "the
// ping is overdue" from "the deadline was missed while the machine was asleep".
//
// # Why the anomaly is reported rather than absorbed
//
// A ping that outlived its deadline means the turn's end was lost, and that is a
// defect somewhere below this code — the vendor, the shim, or the stream. The
// close makes the session usable again; it does not make the loss acceptable, so
// it is a WARN naming the ping and how long it stood open rather than a quiet
// tidy-up. Silencing it would turn a recurring lost boundary into a slow leak
// nobody ever sees.

// SweepOverdueKeepAlivePings closes the claim of every keep-alive ping that has
// stood open past the policy's deadline, and reports how many it closed.
//
// It is called from the same sweep that submits pings, so the feature's
// submission and its failure bound are evaluated at one cadence rather than two.
func (m *Manager) SweepOverdueKeepAlivePings() int {
	cfg := m.keepAliveConfig()
	nowMs := m.now()

	// THE SNAPSHOT IS TAKEN UNDER THE MUTEX AND ACTED ON WITHOUT IT. The close
	// reaches the SSM and the queue's own publication path, neither of which may
	// be entered under this mutex, and the ping claim cannot move underneath the
	// snapshot in a way that matters: every consumer below re-matches on the turn
	// id it was given, so a ping that ended in the gap is a no-op rather than a
	// close aimed at whatever replaced it.
	// THE PLANNED-BOUNCE GRACE, read ONCE for the whole sweep so every session
	// is judged against the same window rather than against a clock that could
	// advance between two map entries (restartepoch.go).
	epoch := m.restartEpochNow()
	type overduePing struct {
		d        *sessionController
		turnID   string
		openMs   int64
		deadline int64
	}
	var overdue []overduePing
	var extended []overduePing
	m.mu.Lock()
	for _, d := range m.byWS {
		ping := d.keepAlivePing
		if ping == nil || d.keepAliveTurnID == "" || ping.turnID != d.keepAliveTurnID {
			continue
		}
		open, late := cfg.PingOverdue(nowMs, ping.submittedAtMs)
		if !late {
			continue
		}
		// THE GAP IS NOT THIS PING'S FAULT. A bounce suspends the whole fleet
		// for as long as it takes, and a ping in flight across it accrues that
		// elapsed without anything having gone wrong — so the deadline is
		// EXTENDED by exactly the window this ping lived through, never
		// skipped. A genuinely wedged ping still trips the bound, one full
		// deadline after the gap, which is what keeps this a failure bound.
		grace := epoch.graceMs - ping.restartGraceAtSubmitMs
		if grace < 0 {
			grace = 0
		}
		entry := overduePing{
			d:        d,
			turnID:   ping.turnID,
			openMs:   open.Milliseconds(),
			deadline: cfg.PingDeadline().Milliseconds() + grace,
		}
		if entry.openMs < entry.deadline {
			extended = append(extended, entry)
			continue
		}
		overdue = append(overdue, entry)
	}
	m.mu.Unlock()

	for _, p := range extended {
		// SAID OUT LOUD, EVERY TIME. A deadline that silently does not fire is
		// indistinguishable from one that is broken, and the whole reason this
		// bound exists is that a claim nobody retires wedges four things at once.
		m.logf("session-controller: keep-alive ping deadline EXTENDED ws=%q session=%s turn_id=%s open_ms=%d extended_deadline_ms=%d base_deadline_ms=%d restart_epoch_open=%v restart_epoch_reason=%q — a planned daemon replacement spanned this ping, so the gap is granted rather than charged to it; the claim is retired if it is still open one full deadline past the window",
			p.d.workspace, p.d.sessionID, p.turnID, p.openMs, p.deadline, cfg.PingDeadline().Milliseconds(), epoch.open, epoch.reason)
	}

	closed := 0
	for _, p := range overdue {
		m.logf("session-controller: keep-alive ping OVERDUE ws=%q session=%s turn_id=%s open_ms=%d deadline_ms=%d — a ping is one model call answering with a single character, so a turn open this long has LOST ITS END; the claim is being retired because it declines every later ping, parks real prompts behind it, and reads as a live turn to hibernation and to every restart guard",
			p.d.workspace, p.d.sessionID, p.turnID, p.openMs, p.deadline)
		// THE DURABLE CLAIM GOES FIRST. Abandoning the in-memory claim below
		// releases held prompts and delivers the front one, and a prompt
		// delivered while the ledger still holds the ping's turn open would be
		// starting a turn the derivation says cannot start.
		retired, err := m.cfg.SSM.CloseOriginTurns(p.d.workspace, []string{p.turnID}, ssm.TurnCloseKeepAliveOverdue)
		if err != nil {
			// NEVER SWALLOWED, AND NEVER A REASON TO STOP. The in-memory claim
			// is released below regardless: leaving it standing on top of a
			// failed ledger write would keep all four consequences above in
			// force, which is strictly worse than holding only the durable half.
			m.logf("session-controller: keep-alive ping OVERDUE CLAIM CLOSE FAILED ws=%q session=%s turn_id=%s error=%v — the durable claim stands, so this workspace keeps reading as a turn in flight until another edge retires it",
				p.d.workspace, p.d.sessionID, p.turnID, err)
		}
		released := m.abandonKeepAlivePing(p.d, p.turnID)
		// The window's upper bound is NOW rather than the deadline instant: the
		// ping's records, whatever it wrote, belong to the interval it was
		// actually open for.
		m.closeKeepAliveWindow(p.d, p.turnID, nowMs)
		m.logf("session-controller: keep-alive ping OVERDUE CLAIM RETIRED ws=%q session=%s turn_id=%s open_ms=%d durable_closed=%v held_released=%d — the claim, the holds and the window are all released; the next sweep may ping this session again",
			p.d.workspace, p.d.sessionID, p.turnID, p.openMs, len(retired) > 0, released)
		closed++
	}
	return closed
}
