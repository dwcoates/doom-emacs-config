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

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
)

type buildRefreshState struct {
	source *sessionController
	done   chan struct{}
	err    error
}

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
	bounced := m.buildBounced[sessionID]
	inFlight := false
	if refresh := m.buildRefresh[sessionID]; refresh != nil {
		select {
		case <-refresh.done:
		default:
			inFlight = true
		}
	}
	source := m.byWS[workspace]
	if !bounced && !inFlight && (source == nil || source.sessionID != sessionID) {
		m.mu.Unlock()
		m.logf("session-controller: stale-shim refresh refused because its source controller is no longer live session=%s ws=%q", sessionID, workspace)
		return false
	}
	var refresh *buildRefreshState
	if !bounced && !inFlight {
		refresh = &buildRefreshState{source: source, done: make(chan struct{})}
		m.buildRefresh[sessionID] = refresh
		select {
		case <-source.buildRefreshStarted:
		default:
			close(source.buildRefreshStarted)
		}
	}
	m.mu.Unlock()
	switch {
	case bounced:
		m.logf("session-controller: session %s (ws %q) STILL reports shim build %s against current %s after a refresh; NOT bouncing again — the bundle or the stamp is wrong, and a second bounce would loop",
			sessionID, workspace, reported, want)
		return false
	case inFlight:
		m.logf("session-controller: session %s (ws %q) reports shim build %s against current %s while a refresh is already IN FLIGHT; NOT starting a second bounce",
			sessionID, workspace, reported, want)
		return false
	}
	generationID := m.currentControllerGeneration(workspace, sessionID)
	m.logf("session-controller: STALE SHIM session=%s ws=%q build=%s current=%s — this shim survived a deploy and is running superseded code; bouncing it onto the current bundle",
		sessionID, workspace, reported, want)
	go func() {
		// THE LATCH FOLLOWS THE OUTCOME, and that order is the whole point.
		//
		// It used to be set BEFORE the restart, on a goroutine whose error was
		// logged and dropped. A stop→respawn that failed therefore latched the
		// session as "already bounced" and returned to the ordinary path: the
		// second spawn never happened, no state edge said so, and every later
		// hello was answered with the "not bouncing again" line. The failure was
		// invisible to everything except a log nobody reads during a hang.
		//
		// Now the at-most-once latch records a refresh that ACTUALLY happened.
		// A failed one clears the in-flight marker without latching — so a later
		// hello may try again — and reports the failure as a real state edge.
		err := m.RestartSession(m.rootCtx, workspace)
		m.mu.Lock()
		refresh.err = err
		if err == nil {
			m.buildBounced[sessionID] = true
		}
		close(refresh.done)
		m.mu.Unlock()
		if err != nil {
			m.resolveStaleRefreshFailed(workspace, sessionID, generationID, reported, want, err)
			return
		}
		m.logf("session-controller: session %s (ws %q) refreshed onto shim build %s", sessionID, workspace, want)
	}()
	return true
}

// resolveStaleRefreshFailed is the close edge a dropped refresh error never
// had.
//
// A refresh that could not complete leaves the workspace running superseded
// code with no shim this daemon can account for — the stop may have landed and
// the respawn may not have — and that is EVIDENCE THAT SOMETHING BROKE, not a
// state to keep quiet about. It is reported exactly the way a failed bring-up
// is (resolveStartFailed): a failure card naming the error, and the
// connectivity axis closed to `unavailable`, so the workspace goes blue with a
// card instead of sitting on a color that claims a live session.
//
// The generation is the one that was live when the stale build was observed,
// falling back to whatever controller owns the workspace now; noteConnectivity
// reports an incomplete identity loudly rather than writing a nameless edge.
func (m *Manager) resolveStaleRefreshFailed(workspace, sessionID, generationID, reported, want string, cause error) {
	m.mu.Lock()
	d := m.byWS[workspace]
	m.mu.Unlock()
	if generationID == "" && d != nil && d.sessionID == sessionID {
		generationID = d.generationID
	}
	m.logf("session-controller: STALE SHIM REFRESH FAILED ws=%q session=%s generation=%s build=%s current=%s connectivity=unavailable branch=stale_shim_refresh_failed cause=%v — the workspace keeps running the superseded bundle",
		workspace, sessionID, generationID, reported, want, cause)
	if d != nil && d.consumer != nil {
		d.consumer.pushFailure(d.consumer.startFailedUUID(), errclass.StartFailed(cause.Error()))
	}
	m.noteConnectivity(workspace, sessionID, generationID, ssm.SessionConnectivityUnavailable, staleShimRefreshFailedCause)
}

// staleShimRefreshFailedCause names the closing edge above in the cause
// vocabulary the connectivity axis records.
const staleShimRefreshFailedCause = "stale_shim_refresh_failed"

// followBuildRefresh returns the replacement for exactly one controller that
// an intentional stale-build restart retired. A transport error without this
// source-generation record is returned to the caller unchanged.
func (m *Manager) followBuildRefresh(ctx context.Context, workspace, sessionID string, source *sessionController) (*sessionController, error) {
	m.mu.Lock()
	refresh := m.buildRefresh[sessionID]
	m.mu.Unlock()
	if refresh == nil || refresh.source != source {
		return nil, nil
	}
	select {
	case <-refresh.done:
	case <-ctx.Done():
		return nil, ctx.Err()
	}
	m.mu.Lock()
	err := refresh.err
	replacement := m.byWS[workspace]
	m.mu.Unlock()
	if err != nil {
		return nil, err
	}
	if replacement == nil || replacement == source || replacement.sessionID != sessionID {
		return nil, fmt.Errorf("session-controller: stale-build refresh completed without a replacement for session %s (ws %q)", sessionID, workspace)
	}
	return replacement, nil
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
		// A hard restart is an intentional process replacement, not hibernation:
		// it may stop a generation that has not reached operational and never
		// publishes the benign HIBERNATED state.
		if err := m.stopSessionController(workspace, sessionID, StopCauseHardRestartLive()); err != nil {
			return fmt.Errorf("session-controller: restarting session %s (ws %q): stopping the live shim: %w", sessionID, workspace, err)
		}
		m.logf("session-controller: hard restart ws=%q session=%s: live shim stopped", workspace, sessionID)
	} else if err := m.stopShimSettlingTurn(workspace, sessionID, StopCauseHardRestartOrphan(), true); err != nil {
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
