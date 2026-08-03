package sessioncontroller

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"sort"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// queueEntry is one prompt the daemon is holding because the session's turn
// was already running when it was submitted (E4).
//
// It is NOT a conversation item and never becomes one: if the entry is
// delivered it is submitted as an ordinary prompt and the turn it starts
// produces the conversation item, exactly as an un-queued prompt would. If it
// is cancelled it leaves no trace in the conversation at all.
type queueEntry struct {
	id string
	// requestID is the frontend submit this entry is holding, carried so the
	// prompt RECEIPT can be pushed when the entry is finally DELIVERED — the
	// moment the prompt actually enters the conversation. Held rather than
	// echoed at submit because a queued prompt is a chip, not a bubble.
	requestID      string
	text           string
	permissionMode string
	queuedAtMs     int64

	classification frontendv1.QueueClassification
	rationale      string
	accepted       bool

	// headJump marks an entry submitted while the queue was PAUSED by a user
	// interrupt (I1). It jumps the retained entries and runs ALONE: the user
	// stopped the agent and then typed something, so what they typed is the
	// only thing they want run.
	headJump bool

	// interjecting marks an entry that is waiting for the running turn to
	// actually END so it can be delivered immediately after (an INTERJECT
	// verdict, or a user force). It is the flag the turn-end handler looks for
	// before falling back to the ordinary FIFO drain.
	interjecting bool

	// shutdownHoldScheduleID names the scheduled shutdown whose DRAIN LEASE is
	// parking this entry, and is empty for every ordinary entry.
	//
	// It is a different KIND of hold from a classification, which is why it is
	// a field of its own rather than a fifth QueueClassification: a classified
	// entry is waiting on the turn in front of it, and a drain-held entry is
	// waiting on the whole daemon. The classifier NEVER runs on one of these —
	// there is nothing to interject into and nothing to decide — so the entry
	// carries the HOLD stamp newParkedEntry gives it for its whole parked life
	// (PENDING would claim a classifier is running that never will), and the
	// frontend renders the lease bubble off this field instead of the
	// classifier bubble.
	//
	// The three exits are a user force (delivered now, further delaying the
	// bounce), a user cancel, and the schedule ending — by cancel, which sheds
	// the hold in place, or by the bounce, after which the daemon that comes
	// back restores the entry un-held.
	shutdownHoldScheduleID string

	// drainRowPending marks an entry that still has a durable parking row
	// behind it. It stays true after the hold itself is shed — a prompt
	// restored after the bounce is UN-HELD but its row must not go until the
	// prompt is actually delivered or cancelled, or a second crash in the
	// window would lose the very prompt the row exists to save.
	drainRowPending bool
}

// drainHeld reports whether a scheduled shutdown's lease is parking this entry.
func (e *queueEntry) drainHeld() bool { return e.shutdownHoldScheduleID != "" }

// promptQueue is one session's ordered FIFO of held prompts. It is not
// goroutine-safe; the Manager serializes every access under its own mutex.
type promptQueue struct {
	entries []*queueEntry
}

// add appends an entry to the back.
func (q *promptQueue) add(e *queueEntry) { q.entries = append(q.entries, e) }

// get returns the entry with id, or nil when the queue no longer holds it
// (already delivered or cancelled).
func (q *promptQueue) get(id string) *queueEntry {
	for _, e := range q.entries {
		if e.id == id {
			return e
		}
	}
	return nil
}

// remove deletes the entry with id and returns it, or nil when it is gone.
func (q *promptQueue) remove(id string) *queueEntry {
	for i, e := range q.entries {
		if e.id == id {
			q.entries = append(q.entries[:i], q.entries[i+1:]...)
			return e
		}
	}
	return nil
}

// popFront removes and returns the front entry, or nil when empty. This is the
// turn-end drain's pick: strictly FIFO, so a classification never reorders
// delivery. Only an interject jumps the queue, and it does so by being
// delivered out of band rather than by being moved.
func (q *promptQueue) popFront() *queueEntry {
	if len(q.entries) == 0 {
		return nil
	}
	e := q.entries[0]
	q.entries = q.entries[1:]
	return e
}

// popFrontDeliverable removes and returns the frontmost entry the ORDINARY
// turn-end drain may deliver, skipping every entry parked by a shutdown drain
// lease. Nil when the queue holds nothing deliverable.
//
// While a lease stands nothing is skipped in practice: taking the lease parks
// the entries already queued as well as every later one, so a deliverable entry
// and a parked one never coexist. The skip is the STRUCTURAL guarantee behind
// that rather than a second mechanism — a future path that parks one entry
// without parking the rest still cannot deliver a parked one, and the relative
// order of whatever is deliverable is untouched.
func (q *promptQueue) popFrontDeliverable() *queueEntry {
	for i, e := range q.entries {
		if e.drainHeld() {
			continue
		}
		q.entries = append(q.entries[:i], q.entries[i+1:]...)
		return e
	}
	return nil
}

// drainHeldCount reports how many entries the drain lease is parking.
func (q *promptQueue) drainHeldCount() int {
	n := 0
	for _, e := range q.entries {
		if e.drainHeld() {
			n++
		}
	}
	return n
}

// pushFront puts an entry back at the head of the queue: the position for one
// that was taken for delivery and could not be delivered after all, so it keeps
// its claim on the next delivery slot rather than going to the back.
func (q *promptQueue) pushFront(e *queueEntry) {
	q.entries = append([]*queueEntry{e}, q.entries...)
}

// addHeadJump inserts an entry AHEAD of every retained entry but BEHIND any
// head-jump already waiting (I1).
//
// Ahead of the retained ones because that is the whole point: the user
// stopped the agent and typed something new, and the queue they paused must
// not run first. Behind the earlier head-jumps because two prompts typed
// during one pause are still two prompts typed in an order, and reversing
// them would be a second surprise on top of the first.
func (q *promptQueue) addHeadJump(e *queueEntry) {
	e.headJump = true
	at := 0
	for at < len(q.entries) && q.entries[at].headJump {
		at++
	}
	q.entries = append(q.entries, nil)
	copy(q.entries[at+1:], q.entries[at:])
	q.entries[at] = e
}

// takeHeadJump removes and returns the first head-jump entry, or nil when
// none is waiting. It is the ONLY delivery a paused queue makes.
// A DRAIN-HELD head jump is skipped, not taken: the drain lease outranks the
// pause, and the entry keeps its head-jump claim for whenever the lease ends.
func (q *promptQueue) takeHeadJump() *queueEntry {
	for i, e := range q.entries {
		if e.headJump && !e.drainHeld() {
			q.entries = append(q.entries[:i], q.entries[i+1:]...)
			return e
		}
	}
	return nil
}

// takeInterjecting removes and returns the first entry flagged for interjection,
// or nil when none is. Front-to-back so two forces in quick succession still
// deliver in the order they were requested.
// A DRAIN-HELD entry is skipped whatever its verdict: a classification decided
// before the lease was taken cannot authorize starting a turn the lease exists
// to prevent. The flag is kept, so a cancelled schedule leaves the entry's
// claim on the next boundary exactly as it found it.
func (q *promptQueue) takeInterjecting() *queueEntry {
	for i, e := range q.entries {
		if e.interjecting && !e.drainHeld() {
			q.entries = append(q.entries[:i], q.entries[i+1:]...)
			return e
		}
	}
	return nil
}

// drainAll empties the queue and returns everything it held, front to back
// (session teardown).
func (q *promptQueue) drainAll() []*queueEntry {
	out := q.entries
	q.entries = nil
	return out
}

// view renders the queue as the pushed frontend frame, front to back. An empty
// queue renders as a QueueView with no entries rather than as no frame at all:
// "the queue is now empty" is exactly the state a frontend needs told.
func (q *promptQueue) view(workspace, sessionID string) *frontendv1.QueueView {
	v := &frontendv1.QueueView{Workspace: workspace, SessionId: sessionID}
	for _, e := range q.entries {
		entry := &frontendv1.QueueEntry{
			Id:             e.id,
			Text:           e.text,
			QueuedAtMs:     e.queuedAtMs,
			Classification: e.classification,
			Rationale:      e.rationale,
			Accepted:       e.accepted,
		}
		if e.drainHeld() {
			entry.ShutdownHold = &frontendv1.QueueEntryShutdownHold{ScheduleId: e.shutdownHoldScheduleID}
		}
		v.Entries = append(v.Entries, entry)
	}
	return v
}

// newQueueEntryID mints a queue entry id.
func newQueueEntryID() string {
	var b [12]byte
	if _, err := rand.Read(b[:]); err != nil {
		// crypto/rand failing is not a condition to paper over with a weaker
		// id: a colliding id would silently misroute a force or a cancel.
		panic(fmt.Sprintf("session-controller: crypto/rand failed: %v", err))
	}
	return "q_" + hex.EncodeToString(b[:])
}

// ---------------------------------------------------------------------------
// Manager integration: interception, classification, delivery.
// ---------------------------------------------------------------------------

// Classifier judges whether a prompt queued during a running turn should
// interject (be delivered now, by interrupting) or hold (be delivered when the
// turn ends on its own).
//
// It is an INTERFACE, and the only thing in this package that ever talks to a
// model, so every test drives the queue with a deterministic fake and nothing
// in the suite can invoke a real one.
type Classifier interface {
	Classify(ctx context.Context, req ClassifyRequest) (ClassifyResult, error)
}

// ClassifyRequest is one classification's inputs. Both prompts are DATA to the
// classifier, never instructions to it.
type ClassifyRequest struct {
	// RunningPrompt is the prompt that started the turn now in flight, as far
	// as the daemon saw it. Empty when the turn began before this daemon did.
	RunningPrompt string
	// QueuedPrompt is the newly submitted prompt being judged.
	QueuedPrompt string
	// ConfigDir is the session's CLAUDE_CONFIG_DIR, so the classification runs
	// under the same account as the session it is about. Empty inherits the
	// daemon's own environment.
	ConfigDir string
}

// ClassifyResult is a believed verdict. A classifier that cannot produce one
// returns an error instead of guessing; the caller surfaces that as the ERROR
// classification rather than resolving it to a real verdict.
type ClassifyResult struct {
	Classification frontendv1.QueueClassification
	Rationale      string
}

// queueSubmitLocked decides what to do with a prompt submitted for d, and
// reports whether it was QUEUED (true) or should be forwarded now (false). A
// non-nil error is a REFUSED submit: nothing was queued and nothing is to be
// forwarded. Caller holds m.mu.
//
// The queue forms ONLY while a turn is running. With the session idle there is
// nothing to hold the prompt behind, so it goes straight through and no queue
// entry is ever created — which is why an idle session never shows chips.
//
// A PAUSED queue does not change that rule, only what the prompt means. With
// no turn running the prompt still goes straight through, and it becomes the
// LONE RUNNER whose clean end resumes the drain; with a turn running it is
// queued as a HEAD JUMP, ahead of everything the pause retained.
//
// THE DRAIN LEASE OVERRIDES ALL OF IT. While a scheduled shutdown holds the
// lease, no new turn may start anywhere, so EVERY submitted prompt is parked —
// from any source, on an idle session as readily as a busy one — and the
// classifier never runs on it. That is the one condition under which an idle
// session does show a chip, and it is the honest one: the prompt genuinely is
// not going to run until the bounce is over.
//
// leaseScheduleID is the drain lease as the CALLER read it, before taking the
// manager mutex. It is passed in rather than read here on purpose: the lease
// engine calls back into this package to recompute its holds, so a read of the
// engine underneath the manager mutex would invert the two locks.
func (m *Manager) queueSubmitLocked(d *sessionController, requestID, text, permissionMode, leaseScheduleID string) (*queueEntry, bool, error) {
	if scheduleID := leaseScheduleID; scheduleID != "" {
		e := newParkedEntry(newQueueEntryID(), requestID, text, permissionMode, m.now())
		if err := m.parkForDrain(d, e, scheduleID); err != nil {
			// THE SUBMIT IS REFUSED, NOT SILENTLY DEGRADED. The lease's whole
			// promise to this prompt is that it is delayed rather than dropped,
			// and the durable row is what carries that promise across the
			// bounce. Without one the daemon can only keep the prompt in memory
			// until the very bounce it is waiting for eats it — so the entry
			// never joins the queue, and the submitter is told now, while they
			// can still retype it, instead of being handed a success and
			// discovering the loss after the deploy.
			m.logf("session-controller: prompt submit REFUSED by the drain lease entry=%s ws=%q session=%s schedule=%s error=%v — the prompt could not be parked durably, so it is neither queued nor forwarded",
				e.id, d.workspace, d.sessionID, scheduleID, err)
			return nil, false, err
		}
		// Appended at the BACK even against a paused queue: a head jump is the
		// paused queue's one deliverable, and a drain-held entry is by
		// definition not deliverable, so claiming that position would be a lie
		// about when it runs.
		d.queue.add(e)
		m.logf("session-controller: prompt PARKED by the drain lease entry=%s ws=%q session=%s schedule=%s turn_active=%v — a scheduled shutdown holds the lease, so this prompt is delayed until the bounce completes; it is not classified and not refused",
			e.id, d.workspace, d.sessionID, scheduleID, d.turnActive)
		return e, true, nil
	}
	if !d.turnActive {
		d.runningText = text
		if d.paused {
			// It runs ALONE: the pause still stands, so the turn-end handler
			// will deliver nothing behind it until this turn ends cleanly.
			d.pausedRunner = true
		}
		return nil, false, nil
	}
	e := &queueEntry{
		id:             newQueueEntryID(),
		requestID:      requestID,
		text:           text,
		permissionMode: permissionMode,
		queuedAtMs:     m.now(),
		classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_PENDING,
	}
	if d.paused {
		d.queue.addHeadJump(e)
		return e, true, nil
	}
	d.queue.add(e)
	return e, true, nil
}

// publishQueueLocked snapshots the queue for publication. Caller holds m.mu.
// The caller pushes and persists AFTER unlocking, so neither the frontend write
// nor the registry write happens under the manager mutex.
func (m *Manager) publishQueueLocked(d *sessionController) (*frontendv1.QueueView, []registry.QueuedPrompt) {
	view := d.queue.view(d.workspace, d.sessionID)
	recs := make([]registry.QueuedPrompt, 0, len(d.queue.entries))
	for _, e := range d.queue.entries {
		recs = append(recs, registry.QueuedPrompt{
			ID:             e.id,
			Text:           e.text,
			PermissionMode: e.permissionMode,
			QueuedAtMs:     e.queuedAtMs,
		})
	}
	return view, recs
}

// publish pushes the view and persists the records produced by
// publishQueueLocked. Must be called with m.mu RELEASED.
func (m *Manager) publish(sessionID string, view *frontendv1.QueueView, recs []registry.QueuedPrompt) {
	m.cfg.Push.PushQueueView(view)
	m.persistQueue(sessionID, recs)
	// Every queue change is also a change to the footer's queue-depth badge, and
	// this is the one place every queue change funnels through.
	m.noteProgressCounts(view.GetWorkspace(), int64(len(view.GetEntries())))
}

// persistQueue writes the session's held prompts through to the durable
// registry record, so a daemon that dies holding prompts leaves evidence of
// what it was holding rather than losing them silently. No-op without a
// registrar.
func (m *Manager) persistQueue(sessionID string, recs []registry.QueuedPrompt) {
	if m.cfg.Registrar == nil {
		return
	}
	m.cfg.Registrar.QueuedPromptsChanged(sessionID, recs)
}

// onTurnBoundary records an observed turn boundary and, on a turn END, delivers
// the next held prompt if there is one.
//
// This is the whole reason the interject sequence is evented. An interject
// sends an Interrupt and then must NOT submit until the turn has actually
// ended — submitting into a turn that is still tearing down races the teardown.
// So the interrupt and the submit are separated by exactly this callback: the
// TurnEnded the shim really reported.
//
// Called on the shim read-loop goroutine, so delivery is dispatched to its own
// goroutine: SubmitPrompt awaits an Ack that only the read loop can deliver, and
// calling it from inside the read loop would deadlock the session.
func (m *Manager) onTurnBoundary(d *sessionController, active bool) {
	m.mu.Lock()
	d.turnActive = active
	if active {
		m.mu.Unlock()
		// A turn STARTING is a new drain hold. Told after the mutex is released
		// (the engine re-reads DrainHolds, which takes it).
		m.noteDrainActivity()
		return
	}
	// The turn that just ended: was it one a user-commanded stop was delivered
	// to, and was it the lone prompt running against a paused queue? Both are
	// consumed here, whatever the boundary goes on to decide.
	wasInterrupted, wasLoneRunner := d.interruptedTurn, d.pausedRunner
	d.interruptedTurn, d.pausedRunner = false, false

	// THE PAUSE RESUMES on the clean end of a prompt that ran alone. The user
	// stopped the agent, ran one thing, and that thing finished — which is the
	// signal that the work they stopped may continue. An interrupted lone
	// runner is the opposite signal, so the pause stands.
	if d.paused && wasLoneRunner && !wasInterrupted {
		d.paused = false
		m.logf("session-controller: queue RESUMED session=%s ws=%q — the prompt that ran alone finished cleanly; draining %d retained entr(ies) in their original order",
			d.sessionID, d.workspace, len(d.queue.entries))
	}

	var e *queueEntry
	var reason string
	switch {
	case d.paused:
		// A PAUSED queue delivers exactly one kind of entry: a head jump, the
		// prompt the user typed after stopping the agent. Everything else is
		// retained — including an entry mid-interject, whose stop was
		// machinery the user's own stop has since overruled.
		e = d.queue.takeHeadJump()
		reason = "paused head jump"
	default:
		// An entry that has been waiting to interject goes first — it asked for
		// exactly this moment. Otherwise the ordinary FIFO drain applies.
		e = d.queue.takeInterjecting()
		reason = "interject"
		if e == nil {
			// THE ORDINARY DRAIN SKIPS DRAIN-HELD ENTRIES. This turn ending is
			// exactly the event the scheduled bounce is waiting for, so
			// delivering the prompt it parked would start the very turn the
			// lease exists to prevent and the drain would never finish.
			e = d.queue.popFrontDeliverable()
			reason = "turn-end drain"
		}
	}
	if e == nil {
		if d.paused {
			m.logf("session-controller: queue PAUSED at a turn boundary session=%s ws=%q — %d entr(ies) retained, none delivered",
				d.sessionID, d.workspace, len(d.queue.entries))
		}
		if held := d.queue.drainHeldCount(); held > 0 {
			m.logf("session-controller: queue DRAIN-HELD at a turn boundary session=%s ws=%q — %d entr(ies) parked by a scheduled shutdown, none delivered; they run once the bounce completes",
				d.sessionID, d.workspace, held)
		}
		m.mu.Unlock()
		// The turn that just ended may have been the last thing holding the
		// drain open. Told AFTER the mutex is released: the engine re-reads the
		// holds through DrainHolds, which takes it.
		m.noteDrainActivity()
		return
	}
	if e.headJump {
		// It runs ALONE against the still-paused queue, and its clean end is
		// what will resume the drain.
		d.pausedRunner = true
	}
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()

	m.logf("session-controller: queue delivering entry=%s (%s) session=%s ws=%q",
		e.id, reason, d.sessionID, d.workspace)
	m.publish(d.sessionID, view, recs)
	m.noteDrainActivity()
	go m.deliver(d, e)
}

// deliver submits a held prompt as an ordinary prompt. Must run OFF the shim
// read-loop goroutine.
//
// On failure the entry is put BACK at the front of the queue, marked ERROR with
// the delivery failure as its rationale. Dropping it would lose something the
// user typed; retrying in place would spin. Instead it is visible, keeps its
// place, and gets another chance at the next turn end or on a user force.
// THE HELD PROMPT'S RECEIPT is pushed from HERE (through forwardPrompt), at the
// one delivery funnel every path reaches — the turn-end drain, the paused
// queue's head jump, and an interject alike (onTurnBoundary and beginInterject
// both end in `go m.deliver`). This is the moment the prompt stops being a chip
// and enters the conversation, which is exactly when a bubble states the truth
// about the order things ran in.
//
// Routing through forwardPrompt is also what gives a HELD `/clear` the same
// reading an immediate one gets: it is recognized, echoed to nobody, and opens
// the clearing axis. This path used to recognize nothing at all.
func (m *Manager) deliver(d *sessionController, e *queueEntry) {
	// THE DRAIN LEASE'S BACKSTOP, at the one funnel every delivery reaches.
	// Every selection path already refuses a parked entry; this is what makes a
	// path that FORGETS to unable to start a turn during a drain, rather than
	// merely unlikely to. It is a loud requeue, never a silent drop: the prompt
	// is still the user's and still owed.
	if e.drainHeld() {
		m.mu.Lock()
		d.queue.pushFront(e)
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.logf("session-controller: queue delivery REFUSED entry=%s session=%s ws=%q schedule=%s — the entry is parked by a scheduled shutdown's drain lease and a delivery path selected it anyway; requeued at the head, nothing was submitted",
			e.id, d.sessionID, d.workspace, e.shutdownHoldScheduleID)
		m.publish(d.sessionID, view, recs)
		return
	}
	err := m.forwardPrompt(m.rootCtx, d, e.requestID, e.text, "frontend", e.permissionMode, submitterUser)
	if err == nil {
		m.mu.Lock()
		d.runningText = e.text
		m.mu.Unlock()
		// The durable parking row outlived the hold on purpose (see
		// drainRowPending). The prompt has now reached the shim, so the row has
		// nothing left to protect and goes.
		if e.drainRowPending {
			e.drainRowPending = false
			m.releaseDrainRow(e.id, "delivered")
		}
		return
	}
	m.logf("session-controller: queue delivery FAILED entry=%s session=%s ws=%q: %v (prompt requeued)",
		e.id, d.sessionID, d.workspace, err)

	m.mu.Lock()
	e.interjecting = false
	e.classification = frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR
	e.rationale = fmt.Sprintf("delivery failed: %v", err)
	// No turn started, so nothing is running alone against the paused queue.
	// Leaving the flag set would make the pause wait on a turn end that can
	// never arrive. The entry keeps its head-jump claim and gets another
	// chance at the next boundary.
	d.pausedRunner = false
	d.queue.pushFront(e)
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.publish(d.sessionID, view, recs)
}

// classify runs the injected classifier for one entry and applies the verdict.
// Runs on its own goroutine (the classifier may spawn a subprocess).
func (m *Manager) classify(d *sessionController, entryID, runningPrompt, queuedPrompt string) {
	res, err := m.runClassifier(d, runningPrompt, queuedPrompt)

	m.mu.Lock()
	e := d.queue.get(entryID)
	if e == nil {
		// Delivered or cancelled while the classifier ran. A verdict about an
		// entry that is already gone is MOOT, not an error: the prompt reached
		// the agent (or the user withdrew it) either way.
		m.mu.Unlock()
		m.logf("session-controller: queue verdict for a gone entry=%s session=%s (moot)", entryID, d.sessionID)
		return
	}
	if err != nil {
		// NEVER silently defaulted to a real verdict: a frontend has to be able
		// to see that nothing decided this. The entry stays queued and the
		// ordinary turn-end drain delivers it.
		e.classification = frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR
		e.rationale = err.Error()
		m.logf("session-controller: queue classify FAILED entry=%s session=%s: %v", entryID, d.sessionID, err)
	} else {
		e.classification = res.Classification
		e.rationale = res.Rationale
		m.logf("session-controller: queue classified entry=%s session=%s verdict=%s",
			entryID, d.sessionID, res.Classification)
	}
	interject := e.classification == frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT
	m.mu.Unlock()

	if interject {
		m.beginInterject(d, entryID, "classifier")
		return
	}
	m.mu.Lock()
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.publish(d.sessionID, view, recs)
}

// runClassifier invokes the injected classifier, or reports its absence.
func (m *Manager) runClassifier(d *sessionController, runningPrompt, queuedPrompt string) (ClassifyResult, error) {
	if m.cfg.Classifier == nil {
		// Not an outage to hide: with no classifier wired NOTHING decided this
		// entry, which is exactly what ERROR means. Delivery is unaffected —
		// the turn-end drain still runs — so the queue degrades to plain FIFO.
		return ClassifyResult{}, fmt.Errorf("no classifier configured; this prompt will be delivered when the turn ends")
	}
	var configDir string
	if m.cfg.SessionConfigDir != nil {
		configDir = m.cfg.SessionConfigDir(d.sessionID)
	}
	return m.cfg.Classifier.Classify(m.rootCtx, ClassifyRequest{
		RunningPrompt: runningPrompt,
		QueuedPrompt:  queuedPrompt,
		ConfigDir:     configDir,
	})
}

// beginInterject starts the interject sequence for an entry: mark it, push the
// state, interrupt the running turn, and then WAIT. The submit itself happens
// in onTurnBoundary when the real TurnEnded arrives — never here.
//
// A verdict that lands after the turn already ended is MOOT: there is nothing
// to interrupt, and the ordinary drain either already delivered the entry or is
// about to. Interrupting an unrelated later turn to "honor" a stale verdict
// would be actively wrong.
func (m *Manager) beginInterject(d *sessionController, entryID, source string) {
	m.mu.Lock()
	e := d.queue.get(entryID)
	if e == nil {
		m.mu.Unlock()
		return
	}
	if !d.turnActive {
		m.logf("session-controller: queue interject entry=%s (%s) is moot — the turn already ended; delivering normally",
			entryID, source)
		e.interjecting = true
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.publish(d.sessionID, view, recs)
		// No turn is running, so no TurnEnded is coming to trigger delivery.
		// Deliver directly rather than stranding the entry.
		m.mu.Lock()
		taken := d.queue.takeInterjecting()
		// The lock was RELEASED across the publish above, so a TurnStarted can
		// have landed in that window and d.turnActive can be true again. The
		// "moot" reasoning that got us here — there is no turn, so delivering
		// now is safe — no longer holds, and submitting would put the prompt
		// into a turn that IS running, which is precisely what the queue
		// exists to prevent. So the turn state is re-verified under the
		// RE-TAKEN lock before the entry is committed to delivery.
		if taken != nil && d.paused && !d.turnActive {
			// The queue is PAUSED, so this delivery is the one prompt running
			// alone against it and its clean end is what resumes the drain.
			// Marked as a head jump too, so a boundary arriving before it
			// lands still treats it as the paused queue's one deliverable.
			taken.headJump = true
			d.pausedRunner = true
		}
		if taken != nil && d.turnActive {
			// Back to the head, keeping its classification and its interjecting
			// flag: the entry still wants to go first, and the turn that just
			// started will deliver it at its TurnEnded via the ordinary
			// interject path. Loud, because a delivery decision was reversed.
			d.queue.pushFront(taken)
			view2, recs2 := m.publishQueueLocked(d)
			m.mu.Unlock()
			m.logf("session-controller: queue interject entry=%s (%s) session=%s — a turn STARTED while the moot path was publishing; requeued at the head instead of submitting into it",
				taken.id, source, d.sessionID)
			m.publish(d.sessionID, view2, recs2)
			return
		}
		view2, recs2 := m.publishQueueLocked(d)
		m.mu.Unlock()
		if taken != nil {
			m.publish(d.sessionID, view2, recs2)
			go m.deliver(d, taken)
		}
		return
	}
	e.interjecting = true
	e.classification = frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT
	if source == "user" {
		e.rationale = "run now, requested by the user"
	}
	if d.paused {
		// The user has stopped this session, and the turn now running is the
		// one prompt they allowed through. An interject's stop is MACHINERY —
		// it interrupts on a held prompt's behalf — and it must not overrule
		// the stop the user commanded, so it becomes a head jump and waits for
		// the boundary instead of sending an Interrupt of its own.
		e.headJump = true
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.logf("session-controller: queue interject entry=%s (%s) session=%s NOT interrupting — the queue is paused by a user interrupt; the entry jumps the head and runs at the next boundary",
			entryID, source, d.sessionID)
		m.publish(d.sessionID, view, recs)
		return
	}
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()

	// Push the INTERJECT state BEFORE interrupting, so the user sees why the
	// turn is about to stop rather than seeing it stop for no stated reason.
	m.publish(d.sessionID, view, recs)

	m.logf("session-controller: queue interjecting entry=%s (%s) session=%s — interrupting, will submit on TurnEnded",
		entryID, source, d.sessionID)
	go func() {
		// An interject's stop is only a failure if the shim says it could not
		// deliver it. ALREADY_COMPLETE means the turn we were racing had
		// already ended, which is the outcome the interject wanted.
		outcome, err := d.client.Interrupt(m.rootCtx)
		if err == nil {
			err = errclass.InterruptError(outcome)
		}
		if err != nil {
			m.logf("session-controller: queue interject interrupt FAILED entry=%s session=%s outcome=%s: %v",
				entryID, d.sessionID, outcome, err)
			m.mu.Lock()
			if cur := d.queue.get(entryID); cur != nil {
				cur.interjecting = false
				cur.classification = frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR
				cur.rationale = fmt.Sprintf("interrupt failed: %v", err)
			}
			view, recs := m.publishQueueLocked(d)
			m.mu.Unlock()
			m.publish(d.sessionID, view, recs)
			return
		}
		if outcome == corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE {
			// THE ACK IS THE BOUNDARY. This interject is waiting for a TurnEnded
			// before it submits, and the shim has just answered that there is no
			// foreground turn to end — so the wait it is holding can never be
			// satisfied by the stream. Both authorities are reconciled to the
			// Ack and the boundary is delivered here (phantomturn.go), which is
			// what actually submits the prompt the user typed.
			m.settleInterjectAlreadyComplete(d, entryID)
		}
	}()
}

// ---------------------------------------------------------------------------
// The frontend command surface.
// ---------------------------------------------------------------------------

// ForceQueueEntry delivers a held prompt NOW: the same interject sequence an
// INTERJECT verdict runs, user-initiated. An unknown id is a loud error — the
// user asked for something specific and it is not there.
func (m *Manager) ForceQueueEntry(workspace, entryID string) error {
	d, err := m.existing(workspace)
	if err != nil {
		return err
	}
	// A FORCE OVERRIDES THE DRAIN LEASE, and that is the point of the control.
	// The user is looking at a bubble that says their prompt is waiting for a
	// scheduled bounce and is telling the daemon to run it anyway. The turn it
	// starts then becomes a drain hold of its own and delays the bounce further
	// — which is exactly what they asked for, so it is honored rather than
	// second-guessed, and loud-logged so the delay has a stated author.
	m.mu.Lock()
	e := d.queue.get(entryID)
	if e == nil {
		m.mu.Unlock()
		return fmt.Errorf("session-controller: no queued prompt %q on workspace %q", entryID, workspace)
	}
	forcedSchedule := e.shutdownHoldScheduleID
	e.shutdownHoldScheduleID = ""
	hadRow := e.drainRowPending
	e.drainRowPending = false
	m.mu.Unlock()
	if forcedSchedule != "" {
		m.logf("session-controller: drain hold FORCED entry=%s ws=%q session=%s schedule=%s initiator=user — the user asked for a prompt parked by a scheduled shutdown to run now; the turn it starts will hold the drain open until it ends",
			entryID, workspace, d.sessionID, forcedSchedule)
	}
	if hadRow {
		m.releaseDrainRow(entryID, "forced_by_user")
	}
	m.beginInterject(d, entryID, "user")
	return nil
}

// AcceptQueueEntry confirms a held prompt's classification. VIEW STATE ONLY: it
// records that the user saw it and changes nothing about when the prompt is
// delivered, so the control cannot imply a power it does not have.
func (m *Manager) AcceptQueueEntry(workspace, entryID string) error {
	d, err := m.existing(workspace)
	if err != nil {
		return err
	}
	m.mu.Lock()
	e := d.queue.get(entryID)
	if e == nil {
		m.mu.Unlock()
		return fmt.Errorf("session-controller: no queued prompt %q on workspace %q", entryID, workspace)
	}
	e.accepted = true
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.logf("session-controller: queue entry=%s accepted session=%s", entryID, d.sessionID)
	m.publish(d.sessionID, view, recs)
	return nil
}

// CancelQueueEntry drops a held prompt. It is never delivered.
func (m *Manager) CancelQueueEntry(workspace, entryID string) error {
	d, err := m.existing(workspace)
	if err != nil {
		return err
	}
	m.mu.Lock()
	e := d.queue.remove(entryID)
	if e == nil {
		m.mu.Unlock()
		return fmt.Errorf("session-controller: no queued prompt %q on workspace %q", entryID, workspace)
	}
	heldBy, hadRow := e.shutdownHoldScheduleID, e.drainRowPending
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.logf("session-controller: queue entry=%s cancelled session=%s drain_schedule=%q initiator=user", entryID, d.sessionID, heldBy)
	if hadRow {
		m.releaseDrainRow(entryID, "cancelled_by_user")
	}
	m.publish(d.sessionID, view, recs)
	return nil
}

// QueueViews returns every live session's queue, sorted by workspace, for the
// connect/resync StateSnapshot. A session with an empty queue still contributes
// its (empty) view, so a reconnecting frontend is told the queue is empty rather
// than being left to assume it.
func (m *Manager) QueueViews() []*frontendv1.QueueView {
	m.mu.Lock()
	defer m.mu.Unlock()
	out := make([]*frontendv1.QueueView, 0, len(m.byWS))
	for _, d := range m.byWS {
		out = append(out, d.queue.view(d.workspace, d.sessionID))
	}
	sort.Slice(out, func(i, j int) bool { return out[i].GetWorkspace() < out[j].GetWorkspace() })
	return out
}
