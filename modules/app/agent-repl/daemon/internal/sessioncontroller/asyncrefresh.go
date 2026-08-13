// asyncrefresh.go — THE STALE-SHIM REFRESH'S ASYNC HALF.
//
// # What was wrong
//
// A full backend bounce promises not to interrupt anyone's work. It kept that
// promise only for work shaped like a TURN. `refreshStaleShim` deferred a roll
// on `turn_in_flight || active_turn_ids`, and both of those describe an SDK
// turn; a shim running a spawned agent or a long-lived background shell has
// neither, so it handshook looking perfectly IDLE at exactly the instant the
// daemon decided whether to SIGTERM it. The detached work died with it, and the
// only account was a restart log line.
//
// # What happens instead
//
// The hello now carries the shim's own live background-task set
// (core.proto ShimHello.live_task_set), so the decision can see async work at
// the moment it is taken. A stale shim with live tasks ARMS the same
// turn-boundary lease a mid-turn shim arms (turnboundaryrefresh.go) rather than
// firing, and the lease is claimed the moment that work drains.
//
// # Absence is not emptiness
//
// A hello with an EMPTY set says "nothing is running", and that is the daemon's
// licence to roll. A hello with NO set says only that this shim does not answer
// the question, and no licence follows from silence — the rule the phantom-task
// reconciler already lives by. Rolling on it would be exactly the bug this file
// exists to fix, merely moved to older bundles.
//
// But an unanswered question cannot be allowed to defer the roll FOREVER
// either: a shim too old to announce the set is precisely the shim a build
// refresh exists to replace, so a permanent deferral would wedge the mechanism
// on the population that needs it most. So silence is RESOLVED rather than
// guessed: the arm is taken first, and then the live connection — which exists
// by now, unlike at the moment of the decision — is asked outright. Only an ask
// that itself goes unanswered rolls, and it says so at `warn`.
//
// # Why the drain fires the lease rather than a sweep noticing
//
// The user measures the gap between "the work finished" and "the bounce
// happened". The idle sweeper's tick is seconds wide and is a poll; the task
// boundary is an EDGE the consumer already observes for every TaskEnded it
// folds. Firing from the edge makes the deferred bounce follow the work's end
// as immediately as the turn-boundary lease follows a turn's end, which is the
// behavior the turn path was already held to.
package sessioncontroller

import (
	"context"

	protocolv1 "agentrepl/proto/protocol/v1"
)

// asyncWorkVerdict is what a handshaking shim's announced background-task set
// says about rolling it.
type asyncWorkVerdict int

const (
	// asyncWorkNone — the shim ANSWERED and nothing is running. The only
	// verdict that licenses an immediate roll.
	asyncWorkNone asyncWorkVerdict = iota
	// asyncWorkLive — the shim answered and named work that is still running.
	asyncWorkLive
	// asyncWorkUnanswered — the hello carried no set at all. Not a session with
	// nothing running; a shim that does not answer the question.
	asyncWorkUnanswered
)

func (v asyncWorkVerdict) String() string {
	switch v {
	case asyncWorkNone:
		return "none"
	case asyncWorkLive:
		return "live"
	case asyncWorkUnanswered:
		return "unanswered"
	}
	return "invalid"
}

// announcedAsyncWork is one hello's answer about its detached work.
type announcedAsyncWork struct {
	verdict asyncWorkVerdict
	// taskIDs is what the shim named, for the log. Empty for every verdict but
	// asyncWorkLive.
	taskIDs []string
}

// classifyAnnouncedAsyncWork reads a hello's live-task set as the roll decision
// needs it: three answers, never two.
//
// The PRESENCE of the message is the assertion, which is the entire reason it
// is a message and not a bare repeated field on the hello — a repeated field
// cannot distinguish "I have nothing" from "I said nothing", and collapsing the
// two is how a roll comes to be taken on silence.
func classifyAnnouncedAsyncWork(hello *protocolv1.ShimHello) announcedAsyncWork {
	set := hello.GetLiveTaskSet()
	if set == nil {
		return announcedAsyncWork{verdict: asyncWorkUnanswered}
	}
	ids := set.GetTaskIds()
	if len(ids) == 0 {
		return announcedAsyncWork{verdict: asyncWorkNone}
	}
	return announcedAsyncWork{verdict: asyncWorkLive, taskIDs: ids}
}

// resolveAnnouncedAsyncSilence turns a hello that did not answer the async
// question into an answer, and acts on it.
//
// Runs on its OWN goroutine, always: it makes a control round-trip whose reply
// is delivered by the shim read loop, and the caller IS that read loop.
//
// Every exit is accounted for. An answer of "nothing running" claims the arm
// and rolls; an answer naming live work leaves the arm standing for the drain
// to claim; an ask that cannot be answered rolls too, loudly, because a
// deferral that can never be resolved would wedge the refresh on exactly the
// old bundles it exists to replace.
func (m *Manager) resolveAnnouncedAsyncSilence(workspace, sessionID, generationID string) {
	m.mu.Lock()
	d := m.byWS[workspace]
	// The controller may already have been retired between the arm and this
	// goroutine being scheduled. Its arm is not ours to fire.
	if d == nil || d.sessionID != sessionID || d.generationID != generationID {
		m.mu.Unlock()
		m.logf("session-controller: async-silence resolution ABANDONED ws=%q session=%s generation=%s branch=controller_retired — the controller that armed this lease is no longer current, so its arm belongs to whoever replaced it",
			workspace, sessionID, generationID)
		return
	}
	client := d.client
	m.mu.Unlock()
	if client == nil {
		m.warnf("session-controller: async-silence resolution CANNOT ASK ws=%q session=%s generation=%s branch=no_client — the controller has no shim client, so whether async work is running cannot be established; the lease stays armed rather than rolling on an unasked question",
			workspace, sessionID, generationID)
		return
	}

	ctx, cancel := context.WithTimeout(m.rootCtx, phantomTaskQueryTimeout)
	defer cancel()
	live, err := client.QueryLiveTasks(ctx)
	if err != nil {
		// THE ONE ROLL TAKEN WITHOUT AN ANSWER, and it is loud. The shim neither
		// announces the set nor answers when asked, so nothing can establish
		// safety — but leaving the lease armed forever would strand a
		// superseded bundle permanently, which is the worse of the two.
		m.warnf("session-controller: async-silence resolution UNANSWERED ws=%q session=%s generation=%s branch=roll_without_answer: %v — this shim neither announced its live-task set nor answered a probe for it, so the roll proceeds WITHOUT having established that no async work is running; a detached task may be lost with it",
			workspace, sessionID, generationID, err)
		m.fireArmedRefreshNow(workspace, sessionID, generationID, "async_unanswerable")
		return
	}
	if len(live) > 0 {
		m.logf("session-controller: async-silence resolution found LIVE WORK ws=%q session=%s generation=%s live_task_ids=%v branch=stay_armed — the shim does not announce its set but answers when asked, and it is running detached work; the lease stays armed and the drain claims it",
			workspace, sessionID, generationID, live)
		return
	}
	m.logf("session-controller: async-silence resolution found NOTHING RUNNING ws=%q session=%s generation=%s branch=roll_now — the silence is resolved into a real answer and the roll is safe",
		workspace, sessionID, generationID)
	m.fireArmedRefreshNow(workspace, sessionID, generationID, "async_resolved_empty")
}

// onAsyncWorkDrained is the ASYNC ANALOGUE OF A TURN BOUNDARY: the consumer's
// live background-task set has just gone empty, so a lease that was deferring
// on that work has nothing left to wait for.
//
// Called from the consumer's task-end fold with m.mu RELEASED.
//
// It is deliberately the same claim-and-run the turn boundary uses rather than
// a second path to the same restart: the lease's exclusivity, its settledness
// re-check, and the parked-prompt handover are all properties of that path, and
// a second author of the restart would have to restate every one of them.
func (m *Manager) onAsyncWorkDrained(d *sessionController) {
	m.mu.Lock()
	arm := m.claimStaleRefreshAtBoundaryLocked(d)
	m.mu.Unlock()
	if arm == nil {
		return
	}
	m.logf("session-controller: async work DRAINED with a stale-shim lease armed ws=%q session=%s generation=%s build=%s current=%s — the detached work this refresh was deferring on has finished, so the lease fires now rather than waiting for a sweep to notice",
		d.workspace, d.sessionID, arm.generationID, arm.reported, arm.want)
	go m.runStaleRefreshAtBoundary(d, arm)
}

// fireArmedRefreshNow claims a workspace's armed lease and runs it immediately,
// naming the branch that decided to.
//
// It exists so the async paths reach the restart through the SAME claim the
// turn boundary uses — exclusive, generation-pinned, and settledness-checked by
// runStaleRefreshAtBoundary — rather than by calling the restart directly and
// racing a boundary that claims the same arm.
func (m *Manager) fireArmedRefreshNow(workspace, sessionID, generationID, branch string) {
	m.mu.Lock()
	d := m.byWS[workspace]
	if d == nil || d.sessionID != sessionID || d.generationID != generationID {
		m.mu.Unlock()
		m.logf("session-controller: armed refresh NOT FIRED ws=%q session=%s generation=%s branch=%s reason=controller_retired — the controller that armed this lease is no longer current",
			workspace, sessionID, generationID, branch)
		return
	}
	arm := m.claimStaleRefreshAtBoundaryLocked(d)
	m.mu.Unlock()
	if arm == nil {
		m.logf("session-controller: armed refresh NOT FIRED ws=%q session=%s generation=%s branch=%s reason=no_claimable_arm — the lease was already claimed, already firing, or disarmed",
			workspace, sessionID, generationID, branch)
		return
	}
	m.logf("session-controller: armed refresh FIRING ws=%q session=%s generation=%s branch=%s build=%s current=%s",
		workspace, sessionID, generationID, branch, arm.reported, arm.want)
	go m.runStaleRefreshAtBoundary(d, arm)
}
