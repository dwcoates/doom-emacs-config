// turnboundaryrefresh.go — THE STALE-SHIM REFRESH'S TURN-BOUNDARY LEASE.
//
// # What was wrong
//
// A shim outlives its daemon by design, so a daemon bounce mid-turn reattaches
// to a shim that is STILL WORKING on the user's turn. buildrefresh.go compares
// that shim's bundle identity against the current one at ShimReady and, on a
// mismatch, bounced it there and then — moments after the reattach, with the
// user's turn in flight. The turn was SIGTERMed, its work thrown away, and the
// only account of it was a restart log line. An AUTOMATIC stop is the daemon's
// own housekeeping and has no business interrupting work nobody asked it to
// interrupt.
//
// # What happens instead
//
// A stale shim that reattaches MID-TURN ARMS a lease rather than firing. The
// refresh then happens at the TURN BOUNDARY: the moment the turn the reattach
// found running resolves — ended, interrupted, or cut, all of which arrive as
// the same inactive edge through onTurnBoundary — the lease acquires the
// settledness verdict, restarts the shim onto the current bundle with
// `--resume`, and releases.
//
// THERE IS NO TIMEOUT OF ITS OWN, and that is deliberate. A turn that never
// resolves is already somebody's problem — the turn-liveness watchdog owns turn
// death — and the boundary a watchdog produces is the same boundary an ordinary
// end produces. Adding a second bound here would be a second authority on when
// a turn is over.
//
// # What happens to prompts typed in the window
//
// They PARK, in the queue's existing hold machinery (queue.go,
// buildRefreshHoldSessionID), and they are delivered IN ORDER after the
// restarted shim's ShimReady. The entries themselves are carried across the
// bounce by the same ownership transfer the keep-alive rewind uses
// (takeQueueForRewind / returnQueueFromRewindLocked), so the retired
// controller's exit tail has nothing to drop and nothing to persist nil over.
//
// # What is NOT affected
//
// A user-commanded RestartSessionCmd is IMMEDIATE, and stays immediate. The
// user asking for the backend to be replaced IS the intent to interrupt; making
// them wait for a turn they have just decided to abandon would leave the one
// control that reaches a wedged session unable to reach it.
package sessioncontroller

import (
	"fmt"
)

// staleRefreshArm is one workspace's armed turn-boundary refresh: a stale-build
// verdict that has been TAKEN but not ACTED ON, because the shim that carried
// it was mid-turn when it reattached.
//
// It is keyed by workspace and pinned to the exact controller identity that
// armed it. A boundary reported by any other controller is not this arm's
// boundary: the generation that armed it may have been retired and replaced,
// and firing a restart off a successor's turn end would bounce a shim whose
// build was never compared.
type staleRefreshArm struct {
	workspace    string
	sessionID    string
	generationID string
	// reported and want are the two build identities that disagreed, carried so
	// the eventual restart's log lines and its failure card say the same thing
	// the ShimReady that armed it said.
	reported string
	want     string
	// activeTurnIDs is what the shim announced was running when it reattached.
	// Evidence for the log, never a gate: the arm fires on the next boundary
	// whatever the ending turn is called, because a turn this daemon did not
	// start may end under a name it never saw.
	activeTurnIDs []string
	// firing marks an arm a boundary has already claimed. It is what makes the
	// lease AT MOST ONCE across two boundaries arriving close together, and it
	// is also what keeps the restart's OWN teardown — which retires the very
	// controller the arm names — from disarming the lease it is executing.
	firing bool
}

// armStaleRefreshAtBoundary records the lease for a stale shim that reattached
// mid-turn, and reports whether it armed one.
//
// It refuses to arm a SECOND lease for a workspace that already has one: the
// arm is the workspace's single answer to "this shim is stale and is waiting
// for its turn to end", and two of them would race two restarts onto one
// session.
func (m *Manager) armStaleRefreshAtBoundary(workspace, sessionID, generationID, reported, want string, activeTurnIDs []string) bool {
	m.mu.Lock()
	if existing := m.staleRefreshArms[workspace]; existing != nil {
		m.mu.Unlock()
		m.logf("session-controller: stale-shim turn-boundary lease ALREADY ARMED ws=%q session=%s generation=%s armed_session=%s armed_generation=%s firing=%v build=%s current=%s branch=arm_exists — the workspace already has one lease waiting on its turn boundary, and a second would race two restarts onto one session",
			workspace, sessionID, generationID, existing.sessionID, existing.generationID, existing.firing, reported, want)
		return false
	}
	m.staleRefreshArms[workspace] = &staleRefreshArm{
		workspace: workspace, sessionID: sessionID, generationID: generationID,
		reported: reported, want: want, activeTurnIDs: activeTurnIDs,
	}
	m.mu.Unlock()
	m.logf("session-controller: STALE SHIM REATTACHED MID-TURN ws=%q session=%s generation=%s build=%s current=%s active_turn_ids=%v branch=arm_turn_boundary_lease — this shim survived a deploy and is running superseded code, but it is STILL WORKING; the refresh is armed and fires at the turn's own boundary, and prompts submitted meanwhile are parked rather than refused",
		workspace, sessionID, generationID, reported, want, activeTurnIDs)
	return true
}

// staleRefreshArmLocked returns the arm covering d, or nil. Caller holds m.mu.
//
// The identity match is workspace AND session: the generation is checked only
// where a boundary is being acted on, because a prompt arriving for the right
// session must park whichever generation is serving it.
func (m *Manager) staleRefreshArmLocked(d *sessionController) *staleRefreshArm {
	arm := m.staleRefreshArms[d.workspace]
	if arm == nil || arm.sessionID != d.sessionID {
		return nil
	}
	return arm
}

// buildRefreshHoldSessionLocked reports the session whose armed or in-flight
// stale-build refresh must park a prompt arriving for d right now, or "" when
// none does. Caller holds m.mu.
//
// IT SPANS THE RESTART TOO, not only the wait. The arm is cleared by the
// runner's own completion and nowhere else, so there is no instant between "the
// turn ended" and "the replacement shim is ready" in which a prompt is admitted
// into a session that is being SIGTERMed.
func (m *Manager) buildRefreshHoldSessionLocked(d *sessionController) string {
	arm := m.staleRefreshArmLocked(d)
	if arm == nil {
		return ""
	}
	return arm.sessionID
}

// claimStaleRefreshAtBoundaryLocked hands a turn boundary the arm it is owed,
// or nil when this boundary is not one an armed lease is waiting for. Caller
// holds m.mu.
//
// The claim is EXCLUSIVE: the arm is marked firing before it is returned, so a
// second boundary arriving while the restart runs finds nothing to claim.
func (m *Manager) claimStaleRefreshAtBoundaryLocked(d *sessionController) *staleRefreshArm {
	arm := m.staleRefreshArmLocked(d)
	if arm == nil || arm.firing {
		return nil
	}
	if arm.generationID != d.generationID {
		return nil
	}
	arm.firing = true
	return arm
}

// runStaleRefreshAtBoundary is the armed lease's whole aftermath: confirm the
// workspace really has settled, carry the parked prompts across the bounce,
// restart the shim onto the current bundle, and deliver what was parked.
//
// Runs on its own goroutine: it stops and respawns the shim, which cannot
// happen on the shim read-loop goroutine the boundary is reported on.
func (m *Manager) runStaleRefreshAtBoundary(d *sessionController, arm *staleRefreshArm) {
	workspace, sessionID := arm.workspace, arm.sessionID
	m.logf("session-controller: stale-shim turn-boundary lease FIRED ws=%q session=%s generation=%s build=%s current=%s — the turn the reattach found running has resolved, so the refusal that has been deferring this refresh no longer holds",
		workspace, sessionID, arm.generationID, arm.reported, arm.want)

	// THE SETTLEDNESS AUTHORITY IS THE HIBERNATION LEASE'S, not a second
	// opinion about the same question. A boundary is an OBSERVATION that one
	// turn ended; whether the workspace is now free of work is the SSM's to
	// answer, and it folds durable turn claims a stream edge cannot see.
	settled, err := m.workspaceSettledNow(workspace)
	if err != nil {
		m.releaseStaleRefreshArm(d, arm, "settledness_unknown")
		m.warnf("session-controller: stale-shim turn-boundary lease COULD NOT READ SETTLEDNESS ws=%q session=%s generation=%s error=%v branch=disarm_unknown_settledness — the refresh is NOT fired on an unknown verdict, and the lease is disarmed rather than left waiting on a boundary that may never come; the at-most-once latch is untouched, so the next hello for this session tries again",
			workspace, sessionID, arm.generationID, err)
		return
	}
	if !settled {
		// STILL WORKING. The boundary that woke this was one turn's end and
		// another is already running — a queued interject, a context cut, a
		// claim the ledger still holds open. The lease stays armed and the next
		// boundary claims it again; no new wait and no new bound are introduced.
		m.mu.Lock()
		arm.firing = false
		m.mu.Unlock()
		m.logf("session-controller: stale-shim turn-boundary lease STAYS ARMED ws=%q session=%s generation=%s branch=not_settled — the boundary arrived but the workspace is still working, so the restart would cut live work; the next boundary claims the lease again",
			workspace, sessionID, arm.generationID)
		return
	}

	// THE QUEUE IS TAKEN FIRST, before anything can stop the shim it belongs
	// to. From here to the re-park below these entries have exactly one owner,
	// so the exit tail this restart provokes has nothing to drop and nothing to
	// persist nil over (rewind.go states the whole argument).
	owned := m.takeQueueForRewind(d)

	// THE SOURCE GENERATION'S READINESS IS WITHHELD FROM HERE, exactly as the
	// immediate path withholds it: a health probe bound to the controller this
	// restart is retiring must transfer to the replacement rather than wait out
	// a transport that is going away (buildrefresh.go, followBuildRefresh).
	refresh := m.registerBuildRefresh(d, sessionID)
	restartErr := m.runStaleRefresh(workspace, sessionID, arm.generationID, arm.reported, arm.want, refresh)

	m.mu.Lock()
	// THE ARM IS CLEARED HERE AND NOWHERE ELSE ON THIS PATH, in the same
	// acquisition that re-parks the prompts and releases their holds: an arm
	// that outlived its restart would park every later prompt on this workspace
	// forever, and a hold released before its entry rejoined a queue would be a
	// prompt released into nothing.
	delete(m.staleRefreshArms, workspace)
	live, ok := m.byWS[workspace]
	if !ok {
		// NOTHING IS DISCARDED. The workspace has no controller to deliver
		// onto, so the entries go back to the retired one — which keeps them
		// renderable and, crucially, persists them, so the record still names
		// prompts the user typed and the daemon has not delivered.
		m.returnQueueFromRewindLocked(d, owned)
		released := d.queue.releaseBuildRefreshHold(sessionID)
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.warnf("session-controller: stale-shim turn-boundary refresh ABANDONED ws=%q session=%s generation=%s parked=%d released=%d error=%v — the workspace has no live session controller after the restart; the parked prompts stay queued, un-held, and are delivered by the next boundary",
			workspace, sessionID, arm.generationID, len(owned), released, restartErr)
		m.publish(sessionID, view, recs)
		return
	}
	// RE-PARK, THEN RELEASE, THEN DELIVER — the rewind's order, for the rewind's
	// reason: the entries rejoin a real queue before any of them is released or
	// delivered, so the view and the durable records are read from the one place
	// that holds the truth about each prompt.
	migrated := live != d
	m.returnQueueFromRewindLocked(live, owned)
	released := live.queue.releaseBuildRefreshHold(sessionID)
	next := live.queue.popFrontDeliverable()
	view, recs := m.publishQueueLocked(live)
	m.mu.Unlock()

	m.logf("session-controller: stale-shim turn-boundary refresh COMPLETE ws=%q session=%s generation=%s->%s migrated=%v parked=%d released=%d error=%v — the shim is on the current bundle and the prompts parked behind the lease are delivered in the order they were typed",
		workspace, sessionID, arm.generationID, live.generationID, migrated, len(owned), released, restartErr)
	m.publish(live.sessionID, view, recs)
	m.noteDrainActivity()
	if next != nil {
		go m.deliver(live, next)
	}
}

// releaseStaleRefreshArm drops an arm without restarting anything, and releases
// every prompt it parked so none is left held behind a lease that is gone.
//
// A dropped arm is never silent: a prompt parked behind it was promised a
// delay, and a delay whose end nobody announces is a drop.
func (m *Manager) releaseStaleRefreshArm(d *sessionController, arm *staleRefreshArm, branch string) {
	m.mu.Lock()
	if current := m.staleRefreshArms[arm.workspace]; current == arm {
		delete(m.staleRefreshArms, arm.workspace)
	}
	live, ok := m.byWS[arm.workspace]
	if !ok || live.sessionID != arm.sessionID {
		live = d
	}
	released := live.queue.releaseBuildRefreshHold(arm.sessionID)
	if released == 0 {
		m.mu.Unlock()
		return
	}
	next := live.queue.popFrontDeliverable()
	view, recs := m.publishQueueLocked(live)
	m.mu.Unlock()
	m.logf("session-controller: stale-shim turn-boundary lease DISARMED ws=%q session=%s branch=%s released=%d — the lease is gone, so the prompts it parked become ordinary queued prompts rather than waiting on a refresh that will never run",
		arm.workspace, arm.sessionID, branch, released)
	m.publish(live.sessionID, view, recs)
	m.noteDrainActivity()
	if next != nil {
		go m.deliver(live, next)
	}
}

// disarmStaleRefreshOnControllerExit drops the arm a DYING controller left
// behind, and reports whether it dropped one.
//
// A controller exit is the one way an armed lease can lose the boundary it is
// waiting for: the connection that would have reported the turn's end is gone.
// The arm is pinned to the exact identity that took it, so a superseded
// generation's exit cannot disarm the lease its successor is holding.
//
// AN ARM THAT IS FIRING IS LEFT ALONE, and that exception is load-bearing: the
// restart the lease is executing retires this very controller, so its own exit
// tail reaches here — and disarming there would delete the lease from
// underneath the runner that is mid-flight.
func (m *Manager) disarmStaleRefreshOnControllerExit(workspace, sessionID, generationID string) bool {
	m.mu.Lock()
	arm := m.staleRefreshArms[workspace]
	if arm == nil || arm.sessionID != sessionID || arm.generationID != generationID || arm.firing {
		m.mu.Unlock()
		return false
	}
	delete(m.staleRefreshArms, workspace)
	m.mu.Unlock()
	m.warnf("session-controller: stale-shim turn-boundary lease DISARMED BY A CONTROLLER EXIT ws=%q session=%s generation=%s build=%s current=%s — the connection that would have reported the turn's boundary is gone, so the lease can never fire; the at-most-once latch is untouched, so the next hello for this session compares the build again",
		workspace, sessionID, generationID, arm.reported, arm.want)
	return true
}

// registerBuildRefresh takes the in-flight marker and retires d's readiness for
// a restart that is about to run. It is the immediate path's registration
// (buildrefresh.go) hoisted to a function so the boundary path takes exactly
// the same one rather than a second one shaped like it.
func (m *Manager) registerBuildRefresh(source *sessionController, sessionID string) *buildRefreshState {
	m.mu.Lock()
	defer m.mu.Unlock()
	refresh := &buildRefreshState{source: source, done: make(chan struct{})}
	m.buildRefresh[sessionID] = refresh
	select {
	case <-source.buildRefreshStarted:
	default:
		close(source.buildRefreshStarted)
	}
	return refresh
}

// staleRefreshArmedFor reports whether workspace currently holds an armed
// turn-boundary refresh, and the session it names. It is the read every caller
// outside this file uses, so nothing else has to know the map exists.
func (m *Manager) staleRefreshArmedFor(workspace string) (string, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	arm := m.staleRefreshArms[workspace]
	if arm == nil {
		return "", false
	}
	return arm.sessionID, true
}

// staleRefreshArmParkRationale is what a prompt parked behind the lease says
// about itself. The queue entry carries ONE free-text account and the wire
// files it under the field its verdict owns, so a HOLD's rationale is where a
// parked prompt states what it is waiting for.
func staleRefreshArmParkRationale(sessionID string) string {
	return fmt.Sprintf("waiting for session %s to restart onto the current shim build", sessionID)
}
