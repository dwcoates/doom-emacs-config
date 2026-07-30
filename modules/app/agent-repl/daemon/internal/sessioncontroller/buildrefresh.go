// buildrefresh.go — keeping a surviving shim on CURRENT code, and the explicit
// hard restart of one session.
//
// # Why a surviving shim goes stale
//
// A shim outlives its daemon by design: it redials the daemon socket forever
// and the next daemon reattaches to it, which is what makes a bounce cheap.
// The cost of that design is that a deploy does not reach it. The bundle on
// disk moves; the running process keeps executing the code it was started with,
// forever, and nothing in the system could tell. A user who deployed a shim fix
// and then bounced the daemon got the OLD shim back, reattached, working
// exactly as wrongly as before.
//
// # How it is detected
//
// The bundle carries its build identity (the git revision it was built from,
// injected by build.mjs from the single value bin/build-frontend.sh also writes
// to `dist/.built-sha`), and the shim reports it on every ShimHello. The daemon
// compares that against the stamp beside the entrypoint it would spawn TODAY.
//
// A MISMATCH IS THE ONLY BOUNCE. Either side unknown — an older bundle with no
// field, a checkout with no stamp — is NOT a mismatch and never bounces: an
// unknown identity is a missing fact, and acting on a missing fact as though it
// were a difference is precisely how a refresh becomes a loop. It is logged
// once per session so the absence is visible rather than silent.
//
// The bounce itself is the ORDINARY path: stop the shim, then ensure the same
// session record again, which respawns with `--resume`. The conversation
// continues; only the process serving it changes. And it happens at most once
// per session, so a shim that comes back still reporting a mismatch (a wrong
// stamp, an unbuildable identity) is loud rather than restarted forever.
package sessioncontroller

import (
	"context"
	"fmt"
)

// noteShimPID records the pid a session's shim announced. Zero is recorded as
// "unknown" (the entry is dropped) rather than as pid 0.
func (m *Manager) noteShimPID(sessionID string, pid int32) {
	if sessionID == "" {
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if pid <= 0 {
		delete(m.shimPID, sessionID)
		return
	}
	m.shimPID[sessionID] = pid
}

// shimPIDFor returns the pid a session's shim announced, or 0 when none is
// known. 0 means the spawner must fall back on its own process handle, which
// is the ordinary case for a shim this daemon spawned.
func (m *Manager) shimPIDFor(sessionID string) int32 {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.shimPID[sessionID]
}

// currentShimBuild reports the build identity of the bundle this daemon would
// spawn today, or "" when it cannot be resolved.
func (m *Manager) currentShimBuild() string {
	if m.cfg.ShimBuildSHA == nil {
		return ""
	}
	return m.cfg.ShimBuildSHA()
}

// refreshStaleShim bounces a shim running a superseded bundle. Called from the
// gate-closed hook, where the connection is live and the announced pid is
// therefore trustworthy.
//
// It reports whether a bounce was started. The bounce runs on its own goroutine
// because the caller is the shimclient's read loop: tearing that connection
// down from inside its own dispatch would be stopping the thing calling us.
func (m *Manager) refreshStaleShim(workspace, sessionID, reported string) bool {
	want := m.currentShimBuild()
	if want == "" || reported == "" {
		m.noteUnknownBuild(workspace, sessionID, reported, want)
		return false
	}
	if reported == want {
		return false
	}
	m.mu.Lock()
	already := m.buildBounced[sessionID]
	if !already {
		m.buildBounced[sessionID] = true
	}
	m.mu.Unlock()
	if already {
		m.logf("session-controller: session %s (ws %q) STILL reports shim build %s against current %s after a refresh; NOT bouncing again — the bundle or the stamp is wrong, and a second bounce would loop",
			sessionID, workspace, reported, want)
		return false
	}
	m.logf("session-controller: STALE SHIM session=%s ws=%q build=%s current=%s — this shim survived a deploy and is running superseded code; bouncing it onto the current bundle",
		sessionID, workspace, reported, want)
	go func() {
		if err := m.RestartSession(m.rootCtx, workspace); err != nil {
			m.logf("session-controller: session %s (ws %q) stale-shim refresh FAILED; the workspace keeps running the superseded bundle: %v",
				sessionID, workspace, err)
			return
		}
		m.logf("session-controller: session %s (ws %q) refreshed onto shim build %s", sessionID, workspace, want)
	}()
	return true
}

// noteUnknownBuild logs an unresolvable comparison ONCE per session. It reuses
// the bounce latch deliberately: both are "this session's build question has
// been answered", and one line per session is the point.
func (m *Manager) noteUnknownBuild(workspace, sessionID, reported, want string) {
	m.mu.Lock()
	already := m.buildBounced[sessionID]
	m.buildBounced[sessionID] = true
	m.mu.Unlock()
	if already {
		return
	}
	m.logf("session-controller: session %s (ws %q) shim build identity is UNKNOWN (shim=%q current=%q); no staleness refresh is possible for it — an unknown identity is not a mismatch",
		sessionID, workspace, reported, want)
}

// RestartSession is the HARD RESTART of one workspace's session: stop whatever
// shim is serving it, then bring it up again along the ordinary path.
//
// THE SESSION RECORD IS UNCHANGED, so the respawn resumes the same vendor
// conversation and the user loses nothing. This is a process restart, not a new
// session: it is what to reach for when the shim is wedged, when it is running
// superseded code, or when the user simply wants the backend rebuilt under a
// conversation they are keeping.
//
// It works for a shim this daemon never spawned — the survivor of a previous
// daemon, which holds no process handle here — because the stop carries the pid
// that shim announced on its ShimHello.
//
// A workspace with NO live session controller is not an error: there is nothing to stop, so
// the restart is exactly the bring-up, and a user asking to restart an unwired
// workspace gets a running one.
func (m *Manager) RestartSession(ctx context.Context, workspace string) error {
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return fmt.Errorf("session-controller: workspace %q has no session to restart", workspace)
	}
	m.mu.Lock()
	_, live := m.byWS[workspace]
	m.mu.Unlock()
	if live {
		// The full teardown: it cancels the session controller, reports the workspace
		// unwired, and stops the shim (by handle, or by announced pid for a
		// survivor).
		if err := m.hibernate(workspace, ""); err != nil {
			return fmt.Errorf("session-controller: restarting session %s (ws %q): stopping the live shim: %w", sessionID, workspace, err)
		}
		m.logf("session-controller: hard restart ws=%q session=%s: live shim stopped", workspace, sessionID)
	} else if err := m.stopShimSettlingTurn(workspace, sessionID, "restart_session_orphan", true); err != nil {
		// NO DRAIN ON THIS BRANCH: there is no session controller, so there is no connection
		// an interrupt could travel over. The funnel still closes the axis, so a
		// parked orphan that died holding a `thinking` cannot survive the
		// restart as one.
		//
		// No controller, but a parked or orphaned process may still be out there
		// holding the session lock. Failing to stop it is fatal to the restart:
		// the bring-up below would refuse to spawn against that held lock, and
		// reporting success would leave the user with the very process they
		// asked to replace.
		return fmt.Errorf("session-controller: restarting session %s (ws %q): stopping the orphaned shim: %w", sessionID, workspace, err)
	} else {
		m.logf("session-controller: hard restart ws=%q session=%s: no live session controller; stopped any orphaned shim", workspace, sessionID)
	}
	if _, err := m.ensure(ctx, workspace); err != nil {
		return fmt.Errorf("session-controller: restarting session %s (ws %q): bringing it back up: %w", sessionID, workspace, err)
	}
	m.logf("session-controller: hard restart ws=%q session=%s COMPLETE (same conversation, fresh shim)", workspace, sessionID)
	return nil
}
