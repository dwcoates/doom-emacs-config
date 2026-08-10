package sessioncontroller

import (
	"sync"
	"time"
)

// restartepoch.go — THE WINDOW A PLANNED DAEMON REPLACEMENT OPENS, and the one
// place every deadline in this package asks about it.
//
// # What the gap is
//
// A daemon bounce is not a failure of anything. The old process takes a drain
// lease, stops accepting new turns, and exits; the replacement comes up,
// reattaches to the shims that outlived it, and settles. Throughout that window
// the shims are alive and working, the store is up, and nothing is broken.
//
// But every FAILURE BOUND in this package is a wall-clock comparison, and a
// wall clock does not know a bounce happened. A keep-alive ping submitted a
// moment before the lease was taken keeps accruing elapsed across the whole
// window and is then declared OVERDUE by the replacement — which retires its
// claim, logs the anomaly as a lost turn boundary, and closes a durable turn
// that was never lost. A teardown's interrupt is given ten seconds to be
// answered over a connection the bounce is in the middle of replacing. Each one
// mints a failure whose entire cause is the gap.
//
// # Suspend, and then EXTEND — never just suspend
//
// Suspending enforcement alone moves the spurious failure rather than removing
// it: the instant the epoch closes, every bound is measured against an elapsed
// that still contains the whole window, and the ping is declared overdue one
// tick later than it would have been. So the epoch also ACCUMULATES the time it
// spanned, and a bound asks for the grace accrued since its own start rather
// than for a flag. A ping submitted after the window sees no grace at all; one
// submitted before it sees exactly the window's length.
//
// # Two openers, and they are the same fact from either side of the bounce
//
// The window straddles two processes, so neither alone can hold it:
//
//   - THE OUTGOING DAEMON opens it when the drain lease is taken
//     (shutdownlease.go). That is the first instant a planned replacement is
//     known to be happening, and it is the daemon's own decision rather than an
//     inference from a symptom.
//   - THE REPLACEMENT opens it at boot and closes it when its boot sweep
//     settles (server.BootSweeper, NoteBootSettled). Until that sweep has
//     finished claiming the surviving shims, every session's state is still
//     being established, and a bound firing against a shim mid-redial is the
//     same spurious failure from the other end.
//
// Neither opener is a duration and neither is tuned: one is the lease's own
// lifetime and the other is the sweep's, so the window is exactly as long as
// the bounce is.

// restartEpoch is the accumulated grace a planned bounce is owed, plus the
// openers currently holding the window open.
type restartEpoch struct {
	mu sync.Mutex
	// booting is the replacement half's opener, held for exactly the extent of
	// DuringBootWindow and unreachable any other way.
	booting bool
	// openedAtMs is when the currently-open window began, or 0 when the epoch
	// is closed. It is re-stamped on each transition into open, so two bounces
	// accumulate two windows rather than one long one spanning the quiet
	// between them.
	openedAtMs int64
	// graceMs is the total elapsed every CLOSED window spanned. The open
	// window's own elapsed is added at read time, so a bound consulted mid-gap
	// already sees the gap it is living through.
	graceMs int64
}

// restartEpochState is one evaluation of the window, as a bound needs it.
type restartEpochState struct {
	// open reports whether a planned replacement is in progress right now.
	open bool
	// reason names the opener holding it, for the log line that says a deadline
	// was extended. Empty when closed.
	reason string
	// graceMs is the TOTAL elapsed every window has spanned, including the one
	// currently open. A bound subtracts the value it captured at its own start.
	graceMs int64
	// openElapsedMs is how long the CURRENTLY OPEN window has been open, or 0
	// when the epoch is closed.
	//
	// It is what a bound STARTING NOW extends by. A deadline comparison against
	// a remembered instant subtracts its own mark from graceMs and needs
	// nothing else; a plain duration bound has no earlier mark to subtract, so
	// the honest extension is the gap the exchange is already starting inside.
	openElapsedMs int64
}

// noteRestartEpoch folds one observation of the openers into the epoch and
// returns the resulting state. It is the ONLY writer, so the accumulated grace
// cannot be advanced from two places with two ideas of when a window began.
func (e *restartEpoch) note(nowMs int64, open bool, reason string) restartEpochState {
	e.mu.Lock()
	defer e.mu.Unlock()
	switch {
	case open && e.openedAtMs == 0:
		e.openedAtMs = nowMs
	case !open && e.openedAtMs != 0:
		// THE WINDOW IS BANKED AS IT CLOSES. Reading the elapsed here rather
		// than leaving the open stamp standing is what keeps the quiet between
		// two bounces out of the grace: the next window starts its own stamp.
		if nowMs > e.openedAtMs {
			e.graceMs += nowMs - e.openedAtMs
		}
		e.openedAtMs = 0
	}
	state := restartEpochState{open: open, reason: reason, graceMs: e.graceMs}
	if open && nowMs > e.openedAtMs {
		state.openElapsedMs = nowMs - e.openedAtMs
		state.graceMs += state.openElapsedMs
	}
	return state
}

// setBooting opens or closes the boot opener and reports whether the value
// actually moved, so a second open or a second close is a no-op rather than a
// second window.
func (e *restartEpoch) setBooting(booting bool) bool {
	e.mu.Lock()
	defer e.mu.Unlock()
	if e.booting == booting {
		return false
	}
	e.booting = booting
	return true
}

func (e *restartEpoch) isBooting() bool {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.booting
}

// restartEpochNow evaluates the window against the openers as they stand right
// now, folding the observation into the accumulated grace.
//
// It is called from the deadline choke points and nowhere else, which is what
// makes "a bound that forgot to ask" a visible omission rather than a subtle
// one: there is exactly one question and exactly one answer to it.
func (m *Manager) restartEpochNow() restartEpochState {
	nowMs := m.now()
	if scheduleID, held := m.heldSchedule(); held {
		return m.restart.note(nowMs, true, "drain_lease:"+scheduleID)
	}
	if m.restart.isBooting() {
		return m.restart.note(nowMs, true, "boot_settling")
	}
	return m.restart.note(nowMs, false, "")
}

// DuringBootWindow runs the daemon's boot reconciliation with the replacement
// half of the restart epoch OPEN, and closes it when that reconciliation
// returns — however it returns.
//
// IT IS A SCOPE, NOT A PAIR OF CALLS, and that is what makes the window
// impossible to get wrong. An opener and a closer as two exported methods can
// be half-wired in two directions, and each direction fails silently: a window
// opened and never closed extends every bound for the life of the process,
// while one closed without being opened restores exactly the spurious failures
// this exists to remove. Binding the window to the extent of the work means it
// is open for precisely as long as the boot sweep runs, including when that
// sweep is cut short by a shutdown or panics.
//
// A Manager that never calls it — every unit test, and any daemon with no boot
// reconciliation to do — has no boot window at all, which is the honest answer:
// nothing is being established, so nothing is owed grace for it.
func (m *Manager) DuringBootWindow(reconcile func()) {
	if !m.restart.setBooting(true) {
		m.errorf("session-controller: restart epoch BOOT WINDOW REFUSED — one is already open, and a second would either be closed early by the first or leave the first's own close unreachable; the boot reconciliation runs WITHOUT a window rather than under an ambiguous one")
		reconcile()
		return
	}
	state := m.restartEpochNow()
	m.logf("session-controller: restart epoch BOOT WINDOW OPEN reason=%q — every surviving shim is unclaimed or mid-redial until the boot sweep rules on it, so failure bounds are extended by exactly this window rather than charged for it",
		state.reason)
	defer func() {
		m.restart.setBooting(false)
		settled := m.restartEpochNow()
		m.logf("session-controller: restart epoch BOOT WINDOW CLOSED grace_ms=%d still_open=%v reason=%q — the boot sweep has claimed or ruled on every surviving shim, so deadline enforcement resumes with the window's elapsed banked as grace rather than charged to whatever was in flight across it",
			settled.graceMs, settled.open, settled.reason)
	}()
	reconcile()
}

// restartExtendedBound is a fixed failure bound for an exchange STARTING NOW,
// extended by the window it is starting inside.
//
// A DURATION BOUND HAS NO EARLIER MARK TO SUBTRACT, which is the whole reason
// this is not restartGraceSince. The exchange has not been running across the
// gap; it is beginning in the middle of one, over a transport that window has
// been disturbing for openElapsedMs already. Granting exactly that is the
// honest extension: it is the window's own length rather than a tuned constant,
// and it is zero the instant the window closes.
func (m *Manager) restartExtendedBound(base time.Duration) (time.Duration, restartEpochState) {
	state := m.restartEpochNow()
	return base + time.Duration(state.openElapsedMs)*time.Millisecond, state
}

// restartGraceMark is the value a bound captures at its own start, so it can
// later ask for the grace accrued SINCE then rather than for the total.
func (m *Manager) restartGraceMark() int64 {
	return m.restartEpochNow().graceMs
}
