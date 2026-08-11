// bringupretry.go — A FAILED BRING-UP WITH RETRIES LEFT MUST ACTUALLY BE RETRIED.
//
// # The wedge this closes
//
// At 22:20:39 on 2026-08-10 a daemon restart hibernated every workspace. Two of
// them — `marcos-pr-remediation` (s_9de8689040244f34) and
// `slack-cee-ceac-integration-shj` (s_9cebb553b0bf3924) — were brought back up
// at 22:20:53 and their bring-ups DIED at 22:22:46 with `awaiting shim
// connection for session <id>: context deadline exceeded`. The ladder in
// bringupescape.go resolved them exactly as designed: connectivity went
// `connecting -> unavailable` at 22:23:26 with
//
//	START FAILED ... consecutive_failures=1 bound=3 given_up=false
//
// and then NOTHING happened for over an hour. Every read since was served
// `durable resync STAMPED the unwired truth wiring=hibernated
// reason=durable_replay_unwired`.
//
// `consecutive_failures=1 bound=3 given_up=false` is the whole defect in one
// line: the daemon believed it had two attempts left and never spent them.
//
// # Why nothing retried
//
// Because a failed bring-up was EDGE-TRIGGERED. Every caller of the ladder is
// a user action or a one-shot boot walk:
//
//   - server.go, the open and prompt paths — a person opening the workspace;
//   - bootsweep.go — once, at daemon start.
//
// The ladder's own rung-2 retry is INSIDE one climb; once it resolves and tears
// its controller down (bringupescape.go, tearDownFailedBringUp), the manager
// holds no record of the workspace at all. The give-up park's cooldown
// (bringupescape.go) has the same shape: it "expires" only in the sense that
// the NEXT request is allowed through — and if no next request ever comes, the
// workspace sits unwired for as long as the daemon lives.
//
// So a failure budget with attempts remaining had no consumer. The bound was
// real; the retry was not.
//
// # The invariant
//
// A workspace whose bring-up failed is either (a) retried, on a backoff, until
// it wires or exhausts its budget, or (b) standing on a LOUD terminal card that
// says so. There is no third state, and in particular no silent forever-unwired
// workspace.
//
// # The shape, which is undriventurn.go's
//
// A provenance-keyed watch armed at the failure, a BOUNDED SWEEP that compares
// against the clock, and a typed resolution. A sweep rather than a timer for
// the reason undriventurn.go states: a timer dies with the daemon and does not
// advance across a laptop sleep, and this failure mode is BORN of a daemon
// restart. The watch is keyed by workspace because that is what a retry
// re-ensures, and the session id rides along only as evidence.
//
// The attempt itself runs off the sweep's goroutine: a bring-up costs a full
// bringUpTimeout and the idle sweeper's walk hibernates the fleet behind it.
// `inFlight` is the latch that makes the sweep idempotent — a second tick
// arriving while an attempt is still climbing selects nothing, so retries never
// stack.
package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/errclass"
)

// bringUpRetryBaseDelay is how long after a resolved bring-up failure the first
// automatic retry is due.
//
// It is a BACKOFF FLOOR, not a tuned delay. A failing bring-up has already cost
// a full bringUpTimeout, so the cadence is bounded by that far more than by
// this; the floor only stops a workspace whose spawn fails instantly (a missing
// binary, a lock held by a corpse) from being re-climbed in a tight loop.
const bringUpRetryBaseDelay = 15 * time.Second

// bringUpRetryMaxDelay caps the doubling below the give-up park's own first
// cooldown, so the budget is spent on the timescale a user is still watching
// rather than long after they have given up on the workspace themselves.
const bringUpRetryMaxDelay = 2 * time.Minute

// bringUpRetryAttemptCap bounds ONE automatic attempt. The ladder bounds itself
// (bringUpTimeout, then one same-conversation retry), and this is only the
// backstop that guarantees the inFlight latch is released: an attempt that
// somehow never returned would otherwise wedge the very sweep that exists to
// unwedge the workspace.
const bringUpRetryAttemptCap = 5 * time.Minute

// bringUpRetryWatch is one workspace's standing "this bring-up owes a retry".
//
// It is armed by resolveStartFailed and dropped by the edges that make it
// meaningless: a bring-up that WIRED, and a deliberate hibernation.
type bringUpRetryWatch struct {
	// sessionID is the session the failure was resolved against. Evidence for
	// the log; the retry re-ensures the WORKSPACE, which may legitimately
	// resolve to a different session.
	sessionID string
	// failures is the consecutive-failure count as of the arming failure.
	failures int
	// dueAtMs is when the next automatic attempt may run, on the Manager clock.
	dueAtMs int64
	// attempts counts automatic retries this watch has launched, for the log.
	attempts int
	// inFlight is the idempotence latch: an attempt is climbing right now.
	inFlight bool
	// givenUp records that the arming failure exhausted the budget, so the wait
	// in force is the give-up park's cooldown rather than the backoff.
	givenUp bool
	// terminalCarded latches the once-only terminal card.
	terminalCarded bool
}

// bringUpRetryDelay is the backoff for the nth consecutive failure: the base
// doubled once per failure beyond the first, capped.
func bringUpRetryDelay(failures int) time.Duration {
	d := bringUpRetryBaseDelay
	for i := 1; i < failures; i++ {
		d *= 2
		if d >= bringUpRetryMaxDelay {
			return bringUpRetryMaxDelay
		}
	}
	return d
}

// armBringUpRetry records that this workspace's bring-up failed and is owed an
// automatic attempt, and reports whether the caller must publish the once-only
// terminal card.
//
// It UPDATES an existing watch in place rather than replacing it, because the
// failure that arms it is frequently the failure of an attempt this watch
// launched: replacing would drop the inFlight latch the attempt still holds and
// re-card a terminal state already reported.
func (m *Manager) armBringUpRetry(workspace, sessionID string, failures int, givenUp bool, cooldown time.Duration) (cardTerminal bool) {
	wait := bringUpRetryDelay(failures)
	if givenUp && cooldown > wait {
		// The park is the authority once the budget is exhausted: retrying
		// inside it would only collect refusals (ErrBringUpGaveUp).
		wait = cooldown
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.bringUpRetries == nil {
		m.bringUpRetries = make(map[string]*bringUpRetryWatch)
	}
	w := m.bringUpRetries[workspace]
	if w == nil {
		w = &bringUpRetryWatch{}
		m.bringUpRetries[workspace] = w
	}
	w.sessionID = sessionID
	w.failures = failures
	w.givenUp = givenUp
	w.dueAtMs = m.now() + wait.Milliseconds()
	if givenUp && !w.terminalCarded {
		w.terminalCarded = true
		cardTerminal = true
	}
	m.logf("session-controller: bring-up retry ARMED ws=%q session=%s consecutive_failures=%d bound=%d given_up=%t retry_in=%s attempts_so_far=%d — the failure budget is not spent by a workspace nobody opens, so the idle sweep will climb the ladder again on its own",
		workspace, sessionID, failures, bringUpGiveUpAfter, givenUp, wait, w.attempts)
	return cardTerminal
}

// clearBringUpRetry drops the workspace's owed retry. Called from the edges
// that make it meaningless: a bring-up that wired, and a deliberate
// hibernation.
func (m *Manager) clearBringUpRetry(workspace, reason string) {
	m.mu.Lock()
	w := m.bringUpRetries[workspace]
	delete(m.bringUpRetries, workspace)
	m.mu.Unlock()
	if w != nil {
		m.logf("session-controller: bring-up retry CLEARED ws=%q session=%s reason=%s failures=%d attempts=%d",
			workspace, w.sessionID, reason, w.failures, w.attempts)
	}
}

// SweepFailedBringUps launches an automatic attempt for every workspace whose
// bring-up failed, is due, and has nothing already climbing for it. It reports
// how many attempts it launched.
//
// It is a COMPARISON AT A SWEEP rather than a timer, for undriventurn.go's
// reason: a timer dies with the daemon and does not advance across a laptop
// sleep, and this failure mode is born of a daemon restart.
func (m *Manager) SweepFailedBringUps() int {
	nowMs := m.now()
	var due []string
	m.mu.Lock()
	for ws, w := range m.bringUpRetries {
		if w.inFlight || nowMs < w.dueAtMs {
			continue
		}
		// A LIVE CONTROLLER RETIRES THE WATCH. Somebody opened the workspace
		// and it came up; the owed retry is answered, and re-ensuring would be
		// a no-op that only muddies the log.
		if _, live := m.byWS[ws]; live {
			delete(m.bringUpRetries, ws)
			continue
		}
		// LATCHED UNDER THE SAME ACQUISITION THAT SELECTED IT, so a second
		// sweep cannot select the same watch and stack a second climb on it.
		w.inFlight = true
		w.attempts++
		due = append(due, ws)
	}
	m.mu.Unlock()

	for _, ws := range due {
		go m.attemptBringUpRetry(ws)
	}
	return len(due)
}

// attemptBringUpRetry climbs the bring-up ladder once for a workspace that owes
// a retry, and always releases the latch.
//
// The failure path deliberately does NOT re-arm: a resolved failure runs
// through resolveStartFailed, which arms the watch itself with the fresh
// count's backoff. The only re-arm here is for the outcomes that never reach
// the ladder's resolution — chiefly a park refusal (ErrBringUpGaveUp) — which
// would otherwise leave the watch due forever and burn one refusal per tick.
func (m *Manager) attemptBringUpRetry(workspace string) {
	m.mu.Lock()
	w := m.bringUpRetries[workspace]
	sessionID, attempt, failures := "", 0, 0
	if w != nil {
		sessionID, attempt, failures = w.sessionID, w.attempts, w.failures
	}
	m.mu.Unlock()
	m.logf("session-controller: bring-up retry ATTEMPT ws=%q session=%s attempt=%d consecutive_failures=%d bound=%d — nothing opened this workspace, so the sweep is climbing the ladder on the user's behalf",
		workspace, sessionID, attempt, failures, bringUpGiveUpAfter)

	ctx, cancel := context.WithTimeout(context.Background(), bringUpRetryAttemptCap)
	defer cancel()
	_, err := m.ensure(ctx, workspace)

	if err == nil {
		// noteWired has already cleared both the streak and the watch; this is
		// the log line that says the sweep is what did it.
		m.logf("session-controller: bring-up retry WIRED ws=%q session=%s attempt=%d — the automatic retry brought the workspace back with no user action",
			workspace, sessionID, attempt)
		m.clearBringUpRetry(workspace, "retry_wired")
		return
	}

	nowMs := m.now()
	m.mu.Lock()
	w = m.bringUpRetries[workspace]
	rearmed := time.Duration(0)
	if w != nil {
		w.inFlight = false
		if w.dueAtMs <= nowMs {
			rearmed = bringUpRetryDelay(w.failures)
			w.dueAtMs = nowMs + rearmed.Milliseconds()
		}
	}
	m.mu.Unlock()
	m.warnf("session-controller: bring-up retry FAILED ws=%q session=%s attempt=%d parked=%t rearmed_in=%s cause=%v — the workspace stays unwired and the sweep will try again",
		workspace, sessionID, attempt, errors.Is(err, ErrBringUpGaveUp), rearmed, err)
}

// publishBringUpGaveUpCard is the LOUD terminal state a workspace lands in when
// its failure budget is spent.
//
// It rides its OWN stable uuid rather than the ladder's start-failed card,
// because it says a different thing: not "this attempt failed" but "nothing is
// attempting any more until the cooldown, and here is what you can do". Keyed
// by session so a workspace that exhausts its budget twice reports two accounts
// rather than one card that keeps changing its mind — and latched by
// terminalCarded so one exhaustion cards exactly once.
//
// It publishes through the throwaway consumer a durable resync serves an
// unwired workspace through (durablereplay.go), because the ladder has already
// torn its own controller down by the time the budget is judged spent.
func (m *Manager) publishBringUpGaveUpCard(workspace, sessionID string, failures int, cooldown time.Duration, cause error) {
	m.warnf("session-controller: bring-up budget EXHAUSTED ws=%q session=%s consecutive_failures=%d bound=%d cooldown=%s — the workspace is parked on a terminal failure card and will be retried automatically once when the cooldown expires; a hard restart ends the park now. Last failure: %v",
		workspace, sessionID, failures, bringUpGiveUpAfter, cooldown, cause)
	cons := m.durableConsumer(workspace, sessionID, m.publishedFence(workspace))
	cons.pushFailure(bringUpGaveUpUUID(sessionID), errclass.StartFailed(fmt.Sprintf(
		"bring-up failed %d times in a row (bound=%d) and is no longer being respawned immediately. The workspace is parked for %s, after which it is retried automatically once; a hard restart-session ends the park now. Last failure: %v",
		failures, bringUpGiveUpAfter, cooldown, cause)))
}

// bringUpGaveUpUUID is the stable card identity for ONE session's exhausted
// bring-up budget.
func bringUpGaveUpUUID(sessionID string) string {
	return "bring-up-gave-up:" + sessionID
}
