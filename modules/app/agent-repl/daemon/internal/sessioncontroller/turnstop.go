package sessioncontroller

import (
	"context"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/shim"
)

// shimStopGate HOLDS THE STOP HALF OF THE SPAWNER, and it is the only thing
// that does. The Manager retains a spawner whose StopShim cannot stop anything
// (sealedSpawner, below), so `m.cfg.Spawner.StopShim` — the shape a new
// teardown would reach for by habit — can never reach a shim, and the live stop
// func is unreachable except through this gate's unexported method, which
// stopShimSettlingTurn is the sole caller of.
//
// Go cannot make an intra-package method literally uncallable, so the seal is
// stated where it CAN be enforced: the capability itself is held in an
// unexported field of an unexported type, handed to nothing else, and the
// obvious bypass fails loudly rather than quietly stopping a shim off-funnel.
type shimStopGate struct {
	stop func(sessionID string, hintPID int32, by shim.Stop) error
}

// newShimStopGate takes the stop half off the wired spawner. It is called once,
// in New, and the spawner the Manager keeps afterwards is sealed.
func newShimStopGate(spawner Spawner) *shimStopGate {
	return &shimStopGate{stop: spawner.StopShim}
}

// sealedSpawner is what the Manager retains: the spawner's bring-up half
// verbatim, and a stop half that REFUSES.
//
// A stop that arrives here bypassed stopShimSettlingTurn, which means it also
// bypassed the session-status close that funnel guarantees. Refusing loudly is
// the only answer that keeps the guarantee true: quietly forwarding would
// restore the very hole the funnel closes, and quietly dropping it would leave
// a shim running while its caller believed it stopped.
type sealedSpawner struct {
	ensure Spawner
	logf   func(string, ...any)
}

func (s sealedSpawner) EnsureShim(ctx context.Context, sessionID string) (SpawnResult, error) {
	return s.ensure.EnsureShim(ctx, sessionID)
}

func (s sealedSpawner) StopShim(sessionID string, hintPID int32, by shim.Stop) error {
	err := fmt.Errorf("session-controller: session %s: refusing an off-funnel shim stop (hint_pid=%d initiator=%q reason=%q); every stop in this package must go through stopShimSettlingTurn, which is what closes the session-status turn the stop would otherwise strand",
		sessionID, hintPID, by.Initiator, by.Reason)
	s.logf("session-controller: SHIM STOP REFUSED session=%s hint_pid=%d initiator=%q reason=%q — the stop did not come through the teardown funnel", sessionID, hintPID, by.Initiator, by.Reason)
	return err
}

// turnstop.go — THE INVARIANT: a session whose shim THIS DAEMON stops cannot
// leave a live turn standing on the SSM's session-status lifecycle.
//
// # The defect it closes
//
// The session-status lifecycle retires `thinking` on a `TurnEnded` and on nothing else. A
// daemon-initiated stop kills the only process that could ever emit that event,
// so a stop landing mid-turn latches the axis forever: the shim, asked to
// interrupt, answers INTERRUPT_OUTCOME_ALREADY_COMPLETE from its own honest
// in-memory view while the sidebar and footer keep reading `thinking` off the
// ledger. Two authorities, no reconciling edge, and every restart re-reads the
// poisoned row rather than curing it.
//
// # Where the invariant is enforced
//
// `stopShimSettlingTurn` is the ONLY route from this package to
// `Spawner.StopShim`, and it closes the axis as its FINAL ACT on every path it
// has — after a successful stop, after a failed one, and after a stop that
// found nothing running. That is what makes the guarantee structural rather
// than a rule each teardown has to remember: there is no way to stop a shim
// from here that does not pass through the close.
//
// The close is deliberately AFTER the stop rather than before it. By the time
// `StopShim` returns the shim is provably gone — it waits on the session lock,
// which the kernel releases only on process death — so the closing row can
// never land over a turn that is still genuinely running. No grace window and
// no retry is involved, because there is no race left to cover.
//
// # The graceful path, and why it is not the guarantee
//
// `drainLiveTurnForStop` runs BEFORE the session controller context is cancelled, while the
// shim connection is still live, and asks the shim to interrupt. When it works
// the shim produces a real `TurnEnded` and the axis closes honestly through the
// ordinary lifecycle, so the teardown's own close finds nothing left to do and
// says so. When it times out, errors, or has no connection to speak over, the
// teardown's close writes the row itself. The two outcomes are logged
// distinguishably, and only the first is an honest shim-sourced close.
//
// Missing the drain costs honesty. It cannot cost the invariant, which is why
// the drain and the guarantee are separate calls with only one of them
// load-bearing.

// interruptDrainTimeout bounds how long a teardown waits for the shim's verdict
// on the stop it asked for.
//
// It is a FAILURE bound, not a tuned delay: the wait ends the instant the ack
// arrives, and this only decides how long a teardown blocks on a shim that has
// stopped answering before giving up and closing the axis itself. It matches
// the shimclient's own ack timeout, which is the same question asked one layer
// down.
const interruptDrainTimeout = 10 * time.Second

// drainTimeout is the bound this manager actually applies. It exists so the
// TIMEOUT BRANCH is exercisable in a unit test without a ten-second wait: an
// unset field resolves to the production constant, and only a test ever assigns
// one. Read under mu, because the tests that set it do so before any teardown
// runs while production teardowns run concurrently.
func (m *Manager) drainTimeout() time.Duration {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.interruptDrain > 0 {
		return m.interruptDrain
	}
	return interruptDrainTimeout
}

// turnInterrupter is the slice of a live shim connection a teardown needs. It
// is an interface rather than *sessionController so the timeout and the already-dead-shim
// branches are exercisable deterministically.
type turnInterrupter interface {
	Interrupt(ctx context.Context, originRequestID string) (corev1.InterruptOutcome, error)
}

// drainLiveTurnForStop asks a shim about to be stopped to interrupt the turn it
// is running, so the turn's end is reported by the shim itself rather than
// synthesized by the teardown.
//
// IT MUST BE CALLED BEFORE THE SESSION CONTROLLER CONTEXT IS CANCELLED. The cancel ends
// `client.Run`, and an interrupt sent after it has nothing to travel over.
//
// EVERY OUTCOME IS BENIGN HERE. A missing connection, a nack, an ack timeout
// and a shim that was already idle all lead to the same place: the stop
// proceeds and `stopShimSettlingTurn` closes the axis. Nothing is returned
// because there is nothing a caller could decide differently — a teardown that
// could not interrupt still tears down — and each is logged where it happens.
func (m *Manager) drainLiveTurnForStop(workspace, sessionID string, cause StopCause, turnID string, cl turnInterrupter, cons *consumer) {
	path := cause.path()
	m.logf("session-controller: teardown turn drain ENTRY ws=%q session=%s path=%s client_present=%v",
		workspace, sessionID, path, cl != nil)
	if cl == nil {
		m.logf("session-controller: teardown turn drain SKIPPED ws=%q session=%s path=%s — there is no live shim connection to interrupt over, so the teardown's own axis close is the only edge available",
			workspace, sessionID, path)
		return
	}
	// The state read is ADVISORY: it exists to spare an idle workspace a
	// pointless round trip, never to authorize the stop. A read failure
	// therefore interrupts anyway rather than skipping — asking a shim we are
	// about to kill to stop costs at most the ack timeout, while skipping on a
	// guess forfeits the honest close this whole path exists for.
	st, found, err := m.cfg.SSM.Current(workspace)
	switch {
	case err != nil:
		m.logf("session-controller: teardown turn drain state read FAILED ws=%q session=%s path=%s: %v — interrupting anyway, because the round trip is cheap and skipping it on an unreadable log would forfeit the shim's own turn end",
			workspace, sessionID, path, err)
	case !found:
		m.logf("session-controller: teardown turn drain SKIPPED ws=%q session=%s path=%s — the log knows nothing about this workspace, so it has no turn to interrupt",
			workspace, sessionID, path)
		return
	case !st.GetTurnActive():
		m.logf("session-controller: teardown turn drain SKIPPED ws=%q session=%s path=%s state=%s — no turn is in flight, so there is nothing for the stop to interrupt",
			workspace, sessionID, path, st.GetState())
		return
	default:
		m.logf("session-controller: teardown turn drain INTERRUPTING ws=%q session=%s path=%s state=%s — the shim is about to be stopped over a live turn, and its own interrupt is the only way that turn's end gets reported honestly",
			workspace, sessionID, path, st.GetState())
	}
	// BEFORE the interrupt, never after. An interrupt delivered without this
	// record is a turn nobody will ever pick up, and the window between the two
	// is a window in which a crash loses the user's work — so the durable
	// record is what the interrupt is allowed to proceed FROM.
	m.recordInterruptedTurnResumption(workspace, sessionID, cause, turnID)
	// ALSO BEFORE THE INTERRUPT. The result the stop provokes can reach the
	// consumer before Interrupt returns, and a turn this teardown displaced
	// must not be settled from it as a turn that finished on its own
	// (terminalsettlement.go).
	if cons != nil {
		cons.noteTurnStopInFlight(turnID)
	}
	// Deliberately NOT the manager's root context. A daemon shutdown cancels
	// that root, and the shutdown teardown is exactly the one whose turns most
	// need stopping; inheriting it would make the interrupt a guaranteed no-op
	// on the path it matters most.
	bound := m.drainTimeout()
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	// The teardown has no request id of its own, so the stop is named by the
	// cause that ordered it, which is the vocabulary the whole funnel logs in.
	outcome, err := cl.Interrupt(ctx, "teardown:"+path)
	if err != nil {
		m.logf("session-controller: teardown turn drain interrupt FAILED ws=%q session=%s path=%s outcome=%s timeout=%s: %v — the shim will not report this turn's end, so the teardown closes the session-status lifecycle itself",
			workspace, sessionID, path, outcome, bound, err)
		return
	}
	m.logf("session-controller: teardown turn drain interrupt DELIVERED ws=%q session=%s path=%s outcome=%s",
		workspace, sessionID, path, outcome)
}

// releaseHeldTerminalResults publishes every terminal result a session
// controller's consumer still holds, so a teardown carries the same release a
// real `TurnEnded` does (sinks.go, settleTurnAccounting).
//
// THE SET IS EVERY TURN THE CONSUMER STILL HOLDS, and nothing narrower is
// correct. A hold is entered only while a turn is active and is discharged the
// instant that turn's accounting settles, so a turn still held when a teardown
// reaches here is one whose end boundary has no remaining way to arrive: the
// shim is about to be stopped and the stream that boundary would travel on is
// about to end with it. The set cannot over-reach either — the map is private
// to this session's own consumer, so it names no turn belonging to any other
// session or to a replacement controller.
//
// IT MUST RUN BEFORE THE SESSION CONTROLLER CONTEXT IS CANCELLED. The release
// publishes through the consumer's conversation path, and the cancel is what
// stands the consumer down.
func (m *Manager) releaseHeldTerminalResults(d *sessionController, cause StopCause) {
	// A controller torn down before its consumer was bound has no stream behind
	// it and therefore nothing held. This is the absence of a hold, not the
	// suppression of one.
	if d.consumer == nil {
		return
	}
	held := d.consumer.heldTerminalTurnIDs()
	if len(held) == 0 {
		return
	}
	m.logf("session-controller: teardown releasing %d retained terminal result(s) ws=%q session=%s path=%s turns=%s — no TurnEnded can reach these turns once this teardown completes, so the teardown is what publishes their results",
		len(held), d.workspace, d.sessionID, cause.path(), formatTurnIDs(held))
	// Per-turn failures are reported and never abort the rest: one turn whose
	// accounting will not persist must not strand the results of the others.
	d.consumer.ReleaseSynthesizedTurnClose(held, cause.path())
}

// drainAndCancelSessionController is the teardown prologue every path that
// holds a live session controller shares, and it exists so the three steps
// cannot drift apart across the paths that need them:
//
//  1. interrupt the turn the shim is running, while the connection an interrupt
//     travels over is still up;
//  2. release the terminal results the consumer holds, while the consumer is
//     still consuming; and
//  3. cancel the session controller context, which ends client.Run.
//
// STEP 3 FORECLOSES BOTH STEPS BEFORE IT, which is also why neither of them can
// live in stopShimSettlingTurn: by the time the axis close runs the controller
// has been evicted from byWS, the cancel has landed, and neither the shim nor
// the consumer is reachable from what that close is handed.
func (m *Manager) drainAndCancelSessionController(workspace string, d *sessionController, cause StopCause) {
	m.drainLiveTurnForStop(workspace, d.sessionID, cause, d.namedTurnForResumption(), d.client, d.consumer)
	m.releaseHeldTerminalResults(d, cause)
	if d.cancel != nil {
		d.cancel()
	}
}

// stopShimSettlingTurn stops a session's shim and then GUARANTEES the workspace's
// session-status lifecycle carries no live turn. It is the only route from this package to
// Spawner.StopShim.
//
// soleSessionController says whether this stop is the workspace's OWN teardown, and it
// travels to the SSM unchanged: it decides whether an UNATTRIBUTED `thinking` —
// one the SSM appended itself, with no session id, such as the row a permission
// close restores — may be spent. A stop aimed at some other record while a
// replacement session drives the same workspace passes false, so it can never
// close a turn that belongs to the replacement.
//
// THE STOP'S ERROR IS THE RETURNED ONE, unchanged from before this funnel
// existed: callers distinguish "no live shim" from a real failure by it. A
// failure to close the axis cannot be folded into it without turning a
// successful hibernation into a failed one, so it is logged loudly at the layer
// that owns it — the same treatment closeCompactingLocked and the rotation
// reconciliation get — and never silently absorbed.
// THE STOP'S PROVENANCE IS STATED IN FULL, on every path. This is the ONE
// funnel from this package to Spawner.StopShim, so the line it writes is the
// complete answer to "why did this shim stop" — who asked (path), what the
// session was doing at the time, and whether a scheduled shutdown's drain lease
// was standing over it, named by schedule and cause. A stop that leaves that
// question to be reconstructed from surrounding lines is the failure mode this
// exists to end.
// THE CAUSE IS THE ONLY VOCABULARY. `path` used to be a free string each
// teardown chose; it is now rendered from the cause (stopcause.go) together
// with the shim.Stop the process itself is handed, so the daemon log and the
// stop record cannot tell two different stories about one death. An unminted
// cause is refused before the spawner is touched — and the axis is still closed
// below, because a refused stop leaves the turn exactly as unreportable as a
// failed one.
func (m *Manager) stopShimSettlingTurn(workspace, sessionID string, cause StopCause, soleSessionController bool) error {
	path := cause.path()
	by := cause.stop()
	m.logf("session-controller: SHIM STOP ENTRY ws=%q session=%s initiator=%s cause=%s stop_initiator=%q stop_reason=%q sole_session_controller=%v %s",
		workspace, sessionID, path, cause, by.Initiator, by.Reason, soleSessionController, m.shimStopProvenance())
	if err := by.Validate(); err != nil {
		stopErr := fmt.Errorf("session-controller: session %s (ws %q): refusing an unattributed shim stop: %w", sessionID, workspace, err)
		m.logf("session-controller: SHIM STOP REFUSED ws=%q session=%s cause=%s: %v — a stop with no attribution would leave the daemon log unable to say who ordered this shim's death, so nothing is signalled",
			workspace, sessionID, cause, err)
		m.settleTurnAfterStop(workspace, sessionID, path, soleSessionController)
		return stopErr
	}
	stopErr := m.shimStops.stop(sessionID, m.shimPIDFor(sessionID), by)
	if stopErr != nil {
		m.logf("session-controller: teardown shim stop FAILED ws=%q session=%s path=%s: %v — the session-status lifecycle is still closed below, because a stop that failed leaves the turn no more reportable than one that succeeded",
			workspace, sessionID, path, stopErr)
	}
	m.settleTurnAfterStop(workspace, sessionID, path, soleSessionController)
	return stopErr
}

// settleTurnAfterStop is the teardown's final act: the workspace's session-status lifecycle
// carries no live turn once it returns.
//
// A workspace it cannot name is the one case with nothing to settle — there is
// no axis to close — and it is loud rather than silent, because a session-scoped
// stop with no workspace behind it means the caller lost the binding.
//
// IT SETTLES THE STATUS AXIS AND NOTHING ELSE. A turn's retained terminal
// result is released by the teardown prologue instead
// (drainAndCancelSessionController), and it has to be: this runs after the
// controller has been evicted from byWS and its context cancelled, and the four
// strings it is handed cannot reach the consumer that holds the result.
func (m *Manager) settleTurnAfterStop(workspace, sessionID, path string, soleSessionController bool) {
	if workspace == "" {
		m.logf("session-controller: teardown axis close SKIPPED session=%s path=%s — the stop named no workspace, so there is no session-status lifecycle to close",
			sessionID, path)
		return
	}
	closed, err := m.cfg.SSM.CloseStaleTurn(workspace, sessionID, path, soleSessionController)
	if err != nil {
		m.logf("session-controller: teardown axis close FAILED ws=%q session=%s path=%s sole_session_controller=%v: %v — the workspace may stay latched in THINKING until another edge supersedes it",
			workspace, sessionID, path, soleSessionController, err)
		return
	}
	if closed {
		m.logf("session-controller: teardown axis close SYNTHESIZED ws=%q session=%s path=%s — no `TurnEnded` reached the log before the shim died, so the daemon wrote the closing row itself as the last party that could know the turn is over",
			workspace, sessionID, path)
		return
	}
	m.logf("session-controller: teardown axis close NOT NEEDED ws=%q session=%s path=%s — the axis already carried no live turn, so the shim's own end closed it honestly",
		workspace, sessionID, path)
}
