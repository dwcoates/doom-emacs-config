// survivingshim.go — the WORKSPACE-level spawn gate.
//
// # The duplicate this closes
//
// EnsureShim already refuses to spawn while a shim holds the SESSION lock, but
// a session id names one conversation attempt, not the workspace. When the
// daemon mints a second session id for a workspace whose shim is alive under
// the first, the two ids take two different session locks, both probes answer
// "free", and the daemon spawns a duplicate over the same vendor transcript.
// The duplicate never dials in, the bring-up waits out its whole timeout, the
// shim is killed, and the loop starts again — with the frontend read loop
// dispatching inline, every one of those waits makes the daemon deaf.
//
// The workspace lock (sessionlock.WorkspaceLockPath) is the claim keyed by the
// thing the invariant is about: a workspace and each resumed transcript keep
// exactly one live session at a time.
//
// # Why the answer is WAIT rather than fail or spawn
//
// A held lock with no controller behind it is the boot window: a surviving shim
// reconnects on its own schedule, so "do I have a connection for this session?"
// answers NO when the truth is NOT YET. Spawning is the duplicate itself.
// Failing immediately would refuse a workspace whose shim is about to dial in
// and is possibly mid-turn. So the gate waits for one of the two things that
// resolve the ambiguity — the survivor dials in and a controller appears, or
// the survivor dies and the lock frees — and treats running out of time as a
// loud typed failure rather than as permission to spawn anyway.
package sessioncontroller

import (
	"errors"
	"fmt"
	"time"
)

// ErrSurvivingShim reports that a shim holds a workspace's lock, this daemon
// drives no controller for that workspace, and the wait for the survivor to
// dial in ran out.
//
// It is a sentinel because it is categorically different from a bring-up
// failure: nothing was spawned and nothing failed to start. A live process owns
// the workspace and did not present itself, which a caller must be able to tell
// apart from "the shim could not be brought up" without matching message text.
var ErrSurvivingShim = errors.New("session-controller: a shim holds the workspace lock and did not dial in")

// survivingShimWaitTimeout bounds the wait for a lock-holding shim to dial in.
//
// It is deliberately NOT bringUpTimeout and deliberately much shorter. The two
// bound different things: bringUpTimeout bounds a shim THIS daemon just
// spawned, while this bounds one that already exists and only has to reconnect.
// Sharing a bound would hide which of the two a stall belongs to, and it is
// exactly that confusion — a bring-up wait charged for an absent duplicate —
// that made the daemon deaf for half a minute per respawn.
const survivingShimWaitTimeout = 10 * time.Second

// survivingShimPollInterval is how often the wait re-reads the two facts. Both
// are edge-free — a kernel lock and a map — so there is nothing to subscribe to
// and the wait samples them.
const survivingShimPollInterval = 100 * time.Millisecond

// awaitSurvivingShim is the gate itself, run before anything is spawned.
//
// It returns:
//
//   - (nil, nil) when the workspace is free to spawn into: either no shim holds
//     it, or the holder died while we waited;
//   - (d, nil) when the survivor dialled in and a controller now drives the
//     workspace, which the caller returns instead of spawning; and
//   - (nil, err) when the probe could not answer, or the wait ran out. Neither
//     is ever read as free: an unproved workspace is not spawned into.
func (m *Manager) awaitSurvivingShim(workspace string) (*sessionController, error) {
	held, err := m.workspaceLockHeld(workspace)
	if err != nil {
		return nil, fmt.Errorf("session-controller: workspace %q: cannot determine whether a shim already holds it: %w", workspace, err)
	}
	if !held {
		return nil, nil
	}

	timeout, interval := m.survivingShimBounds()
	m.logf("session-controller: SURVIVING SHIM WAIT ws=%q wait_bound=%s — a live shim holds this workspace's lock while the daemon drives no controller for it. This is NOT a bring-up wait: nothing has been spawned, and nothing will be until the holder dials in or dies.",
		workspace, timeout)

	started := time.Now()
	deadline := time.NewTimer(timeout)
	defer deadline.Stop()
	tick := time.NewTicker(interval)
	defer tick.Stop()
	for {
		select {
		case <-m.rootCtx.Done():
			return nil, fmt.Errorf("session-controller: workspace %q: the wait for its surviving shim was cancelled after %s: %w",
				workspace, time.Since(started).Round(time.Millisecond), m.rootCtx.Err())
		case <-deadline.C:
			m.logf("session-controller: SURVIVING SHIM WAIT EXPIRED ws=%q waited=%s decision=refuse_spawn — the lock holder never dialled in, so the workspace is owned by a process this daemon cannot drive. Spawning now would put two shims on one transcript.",
				workspace, time.Since(started).Round(time.Millisecond))
			return nil, fmt.Errorf("%w: workspace %q, after %s", ErrSurvivingShim, workspace, timeout)
		case <-tick.C:
			m.mu.Lock()
			d, wired := m.byWS[workspace]
			m.mu.Unlock()
			if wired {
				m.logf("session-controller: SURVIVING SHIM DIALLED IN ws=%q session=%s waited=%s decision=adopt_existing — no shim was spawned",
					workspace, d.sessionID, time.Since(started).Round(time.Millisecond))
				return d, nil
			}
			held, err := m.workspaceLockHeld(workspace)
			if err != nil {
				return nil, fmt.Errorf("session-controller: workspace %q: cannot determine whether a shim still holds it: %w", workspace, err)
			}
			if !held {
				m.logf("session-controller: SURVIVING SHIM GONE ws=%q waited=%s decision=spawn — the holder released the workspace lock, so the workspace is genuinely free",
					workspace, time.Since(started).Round(time.Millisecond))
				return nil, nil
			}
		}
	}
}

// survivingShimBounds resolves the wait's timing, honoring the test overrides.
func (m *Manager) survivingShimBounds() (timeout, interval time.Duration) {
	timeout = survivingShimWaitTimeout
	if m.survivingShimWaitOverride > 0 {
		timeout = m.survivingShimWaitOverride
	}
	interval = survivingShimPollInterval
	if m.survivingShimPollOverride > 0 {
		interval = m.survivingShimPollOverride
	}
	return timeout, interval
}
