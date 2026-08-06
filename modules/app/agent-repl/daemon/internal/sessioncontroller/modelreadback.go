package sessioncontroller

import (
	"claude-repld/internal/errclass"
)

// THE BARE `/model`, which is the one model change the daemon cannot perform
// itself.
//
// `/model <name>` names its operation, so the daemon runs it through the
// picker's own SetModel and the shim's confirmation writes the record at the
// moment of the command (promptdispatch.go). The ARGUMENT-LESS form names
// nothing: it opens the CLI's own interactive picker, the user chooses inside
// it, and no layer above ever sees the choice.
//
// What the daemon used to do about that was nothing. The record — and
// therefore the pushed SessionView, and therefore the topbar picker — went on
// naming the previous model until some later prompt's SystemInit happened to
// re-announce the new one, and a hibernation before that respawned the session
// on the stale value.
//
// So the daemon ASKS. The read is anchored to the command's OWN turn boundary
// rather than to a delay or to the user's next prompt: the CLI resolves
// `/model` inside a turn, so that turn's end is the instant the answer exists,
// and waiting for it is a rendezvous rather than a guess. Nothing here sleeps,
// retries, or hopes an interval was long enough.
//
// A SHIM THAT CANNOT ANSWER IS A FAILURE THE USER SEES. The whole point of the
// read is that the committed value was verified, so an unanswerable read
// surfaces through the failure channel and leaves the record alone, rather
// than leaving a picker naming a model nobody confirmed.

// noteModelReadbackPending records that one submitted turn is a bare `/model`
// whose result must be read back when it ends.
//
// Keyed by the request id, which the shim adopts as the turn_id of the
// boundaries the submit produces — so the turn that ends is nameable as the
// command that started it, with no correlation to invent.
//
// Must be called with m.mu RELEASED.
func (m *Manager) noteModelReadbackPending(d *sessionController, requestID string) {
	if requestID == "" {
		return
	}
	m.mu.Lock()
	if d.modelReadbacks == nil {
		d.modelReadbacks = map[string]struct{}{}
	}
	d.modelReadbacks[requestID] = struct{}{}
	pending := len(d.modelReadbacks)
	m.mu.Unlock()
	m.logf("session-controller: bare /model dispatched ws=%q session=%s request_id=%s pending_model_readbacks=%d — the CLI owns the choice, so the live model is read back when this turn ends",
		d.workspace, d.sessionID, requestID, pending)
}

// takeModelReadback claims the pending read-back for one ended turn, reporting
// whether this call is the one that owns it.
//
// CLAIMED UNDER THE MUTEX AND EXACTLY ONCE. A turn's end can be observed more
// than once (a durable boundary behind a re-delivered event), and a read-back
// per observation would ask the shim repeatedly and write the record from each
// answer.
func (m *Manager) takeModelReadback(d *sessionController, turnID string) bool {
	if turnID == "" {
		return false
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if _, pending := d.modelReadbacks[turnID]; !pending {
		return false
	}
	delete(d.modelReadbacks, turnID)
	return true
}

// readBackObservedModel asks the shim which model the session is now running
// and commits the answer, for a bare `/model` whose turn has just ended.
//
// Runs on its own goroutine: it is a shim round-trip, and the caller is the
// turn-boundary path that every other consumer of that edge is queued behind.
func (m *Manager) readBackObservedModel(d *sessionController, requestID string) {
	// Counted BEFORE the goroutine starts, so a waiter that arrives between
	// these two lines still sees the read as in flight.
	m.modelReadbacks.Add(1)
	go func() {
		defer m.modelReadbacks.Done()
		selected, err := d.client.QuerySelectedModel(m.rootCtx)
		if err != nil {
			// LOUD AND VISIBLE, never a silent fallback to the record's
			// standing value. The user just changed the model; a picker that
			// keeps naming the old one with nothing said about it is precisely
			// the failure this read exists to end, so the failure card says the
			// selection could not be confirmed.
			m.logf("session-controller: live model read-back FAILED ws=%q session=%s request_id=%s: %v — the record is unchanged and the picker may name a model this session is no longer running",
				d.workspace, d.sessionID, requestID, err)
			d.consumer.pushFailure("model-readback-"+requestID, errclass.Command(nil, err))
			return
		}
		m.logf("session-controller: live model read-back CONFIRMED ws=%q session=%s request_id=%s selected=%q",
			d.workspace, d.sessionID, requestID, selected)
		m.persistObservedModel(d.sessionID, selected, d.modelObservationNow())
	}()
}
