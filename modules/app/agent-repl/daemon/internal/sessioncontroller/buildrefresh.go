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

// noteShimBuild records the bundle identity a session's shim announced, so a
// decision about that shim can be taken when its connection is no longer in
// reach — which is exactly the shutdown drain's situation (ShimBundleStale).
//
// An empty identity DROPS the entry rather than recording "". An older bundle
// with no field carries no identity at all, and remembering an empty string as
// though it were one would let a comparison read a missing fact as a
// difference — the failure refreshStaleShim's unknown-is-never-a-mismatch rule
// exists to prevent.
func (m *Manager) noteShimBuild(sessionID, build string) {
	if sessionID == "" {
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if build == "" {
		delete(m.shimBuild, sessionID)
		return
	}
	m.shimBuild[sessionID] = build
}

// ShimStopWouldFixTheBundle reports whether stopping a session's shim is the
// only way to get it onto the current bundle, together with the two identities
// the verdict was taken from.
//
// IT IS THE ONLY THING THAT JUSTIFIES STOPPING A SURVIVING SHIM ON A BOUNCE. A
// shim is daemon-independent by design: it outlives the daemon, redials the
// socket, and the next daemon reattaches to it with the turn it was running
// still running. So a planned bounce has no reason to kill one — the process
// serving the user is fine, and stopping it converts a free reattach into an
// interrupted turn that has to be re-driven. The one thing a stop CAN fix and a
// reattach cannot is a shim executing code the deploy replaced.
//
// A PROVEN MATCH IS THE ONLY "NO", and the asymmetry is deliberate. This
// question is only ever asked because somebody EXPLICITLY asked for the shim
// bundle to be replaced, so an unreadable identity on either side leaves the
// daemon unable to prove the stop is pointless — and preserving on that would
// silently defeat the deploy the operator asked for. It is the opposite
// reading from refreshStaleShim's, which answers an AUTOMATIC bounce where an
// unknown treated as a difference is how a refresh becomes a loop; nothing
// here loops, because nothing here repeats.
func (m *Manager) ShimStopWouldFixTheBundle(sessionID string) (stop bool, reported, want string) {
	want = m.currentShimBuild()
	m.mu.Lock()
	reported = m.shimBuild[sessionID]
	m.mu.Unlock()
	if want != "" && reported != "" && reported == want {
		return false, reported, want
	}
	return true, reported, want
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
// It reports whether the SOURCE GENERATION IS RETIRING — whether this ShimReady
// must withhold readiness because a replacement is taking over. A bounce
// deferred to a turn boundary reports FALSE: the shim it found is still serving
// a live turn and must stay fully ready for it, which is the whole point of
// deferring.
//
// An immediate bounce runs on its own goroutine because the caller is the
// shimclient's read loop: tearing that connection down from inside its own
// dispatch would be stopping the thing calling us.
//
// activeTurnIDs and turnInFlight are the shim's OWN account of what it was
// doing when it reattached, straight off the hello. They are what separates the
// two paths, and they are read from the hello rather than from the daemon's
// turn latch on purpose: the latch describes what this daemon has observed, and
// a daemon that has just come back has observed nothing yet.
func (m *Manager) refreshStaleShim(workspace, sessionID, reported string, turnInFlight bool, activeTurnIDs []string) bool {
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
	armed := m.staleRefreshArms[workspace] != nil
	source := m.byWS[workspace]
	if !bounced && !inFlight && !armed && (source == nil || source.sessionID != sessionID) {
		m.mu.Unlock()
		m.logf("session-controller: stale-shim refresh refused because its source controller is no longer live session=%s ws=%q", sessionID, workspace)
		return false
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
	case armed:
		m.logf("session-controller: session %s (ws %q) reports shim build %s against current %s while a turn-boundary lease is already ARMED for it; NOT starting a second bounce — the armed lease owns this refresh and fires at the turn's own boundary",
			sessionID, workspace, reported, want)
		return false
	}
	generationID := m.currentControllerGeneration(workspace, sessionID)
	// THE MID-TURN FORK, and it is the whole reason this file is no longer the
	// only author of a stale-shim bounce.
	//
	// A shim that survived the daemon bounce is very often STILL WORKING on the
	// turn the user submitted before it — that is exactly what surviving means —
	// and an AUTOMATIC stop firing here would SIGTERM live work moments after
	// reattaching to it. The refresh is deferred to the turn's own boundary
	// instead (turnboundaryrefresh.go); nothing about the mismatch is forgotten,
	// and the at-most-once latch is untouched because no refresh has happened
	// yet.
	if turnInFlight || len(activeTurnIDs) > 0 {
		m.armStaleRefreshAtBoundary(workspace, sessionID, generationID, reported, want, activeTurnIDs)
		// FALSE, deliberately, whether or not an arm was taken: the source
		// generation is NOT retiring. This shim keeps its readiness because it
		// is still serving the turn the lease is waiting on, and withholding
		// readiness from a session that is actively driving a turn would strand
		// every caller of it.
		return false
	}
	m.logf("session-controller: STALE SHIM session=%s ws=%q build=%s current=%s — this shim survived a deploy and is running superseded code, and it reattached SETTLED; bouncing it onto the current bundle now",
		sessionID, workspace, reported, want)
	refresh := m.registerBuildRefresh(source, sessionID)
	// The error is not dropped here: runStaleRefresh reports it three ways
	// before it returns — the in-flight rendezvous (refresh.err, which
	// followBuildRefresh returns to a waiting health probe), the failure card,
	// and the connectivity edge. There is no caller left on this goroutine to
	// return it to.
	go func() { _ = m.runStaleRefresh(workspace, sessionID, generationID, reported, want, refresh) }()
	return true
}

// runStaleRefresh is the restart itself, shared by the IMMEDIATE path above and
// the TURN-BOUNDARY path (turnboundaryrefresh.go) so the two cannot disagree
// about the at-most-once latch, the in-flight rendezvous, or the failure card.
//
// THE LATCH FOLLOWS THE OUTCOME, and that order is the whole point.
//
// It used to be set BEFORE the restart, on a goroutine whose error was logged
// and dropped. A stop→respawn that failed therefore latched the session as
// "already bounced" and returned to the ordinary path: the second spawn never
// happened, no state edge said so, and every later hello was answered with the
// "not bouncing again" line. The failure was invisible to everything except a
// log nobody reads during a hang.
//
// Now the at-most-once latch records a refresh that ACTUALLY happened. A failed
// one clears the in-flight marker without latching — so a later hello may try
// again — and reports the failure as a real state edge.
//
// It must NOT run on the shim read-loop goroutine: it stops and respawns the
// shim, and the bring-up waits on a handshake only that loop can deliver.
func (m *Manager) runStaleRefresh(workspace, sessionID, generationID, reported, want string, refresh *buildRefreshState) error {
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
		return err
	}
	m.logf("session-controller: session %s (ws %q) refreshed onto shim build %s", sessionID, workspace, want)
	return nil
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
	// A HARD RESTART IS THE DELIBERATE WAY OUT OF A BRING-UP PARK. The park is a
	// cooldown on AUTOMATIC respawns; this is a user who has explicitly asked for
	// the process to be replaced, and refusing that with "it is resting" would
	// leave the one control that exists for a wedged session unable to reach the
	// session most in need of it (bringupescape.go).
	m.clearBringUpFailures(sessionID)
	// AND IT IS THE WAY OUT OF THE VANISHED-RESUME FENCE, for the same reason
	// and with one difference: the fence stands on a fact about the DISK, so
	// clearing it does not assert the session can now start — it only entitles
	// the bring-up below to look again. A transcript that has been restored
	// resumes; one that is still gone re-fences on the same card, from the same
	// gate, with no extra machinery (vanishedresume.go).
	m.clearVanishedResumeFence(workspace, sessionID)
	// AND IT IS EQUALLY THE WAY OUT OF A SLEEP, because it has already paid for
	// one. The restart resumes the SAME vendor conversation, so the whole cost the
	// revival gate exists to put to the user first is spent by the time this line
	// is reached; leaving the record saying `hibernated` bought nothing and cost
	// the user a session that comes up READY and then refuses every prompt they
	// send it, with no revival card standing to explain why.
	//
	// IT IS ALSO THE HOLE IN hibernation.go's STATED INVARIANT. "Hibernated but
	// still being pinged" is unrepresentable only because the two routes into a
	// session both refuse a sleeping record — and this one is a third route that
	// re-enters m.byWS without asking. Clearing here closes it, so the record and
	// the running process cannot disagree.
	//
	// CLEARED BEFORE THE BRING-UP, never after: ensure() evaluates the same
	// hibernation policy on its way through, so a clear that landed afterwards
	// would be a clear the bring-up it was meant to admit had already been
	// refused by. Only a record that actually claims a sleep is written, so an
	// ordinary restart does not disturb the idle clock clearHibernation stamps.
	m.mu.Lock()
	_, asleep := m.hibernatedLocked(sessionID)
	m.mu.Unlock()
	if asleep {
		m.logf("session-controller: hard restart ws=%q session=%s is CLEARING the revival gate — the restart resumes the same conversation, so the sleep's cost is already paid and a standing gate would only refuse the prompts the restart was asked for",
			workspace, sessionID)
		if err := m.clearHibernation(workspace, sessionID); err != nil {
			return fmt.Errorf("session-controller: restarting session %s (ws %q): clearing the hibernation record: %w", sessionID, workspace, err)
		}
	}
	if _, err := m.ensure(ctx, workspace); err != nil {
		return fmt.Errorf("session-controller: restarting session %s (ws %q): bringing it back up: %w", sessionID, workspace, err)
	}
	m.logf("session-controller: hard restart ws=%q session=%s COMPLETE (same conversation, fresh shim)", workspace, sessionID)
	return nil
}
