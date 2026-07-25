package sessiondrv

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"sort"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

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
	id             string
	text           string
	permissionMode string
	queuedAtMs     int64

	classification frontendv1.QueueClassification
	rationale      string
	accepted       bool

	// interjecting marks an entry that is waiting for the running turn to
	// actually END so it can be delivered immediately after (an INTERJECT
	// verdict, or a user force). It is the flag the turn-end handler looks for
	// before falling back to the ordinary FIFO drain.
	interjecting bool
}

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

// takeInterjecting removes and returns the first entry flagged for interjection,
// or nil when none is. Front-to-back so two forces in quick succession still
// deliver in the order they were requested.
func (q *promptQueue) takeInterjecting() *queueEntry {
	for i, e := range q.entries {
		if e.interjecting {
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

// empty reports whether the queue holds nothing.
func (q *promptQueue) empty() bool { return len(q.entries) == 0 }

// view renders the queue as the pushed frontend frame, front to back. An empty
// queue renders as a QueueView with no entries rather than as no frame at all:
// "the queue is now empty" is exactly the state a frontend needs told.
func (q *promptQueue) view(workspace, sessionID string) *frontendv1.QueueView {
	v := &frontendv1.QueueView{Workspace: workspace, SessionId: sessionID}
	for _, e := range q.entries {
		v.Entries = append(v.Entries, &frontendv1.QueueEntry{
			Id:             e.id,
			Text:           e.text,
			QueuedAtMs:     e.queuedAtMs,
			Classification: e.classification,
			Rationale:      e.rationale,
			Accepted:       e.accepted,
		})
	}
	return v
}

// newQueueEntryID mints a queue entry id.
func newQueueEntryID() string {
	var b [12]byte
	if _, err := rand.Read(b[:]); err != nil {
		// crypto/rand failing is not a condition to paper over with a weaker
		// id: a colliding id would silently misroute a force or a cancel.
		panic(fmt.Sprintf("sessiondrv: crypto/rand failed: %v", err))
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
// reports whether it was QUEUED (true) or should be forwarded now (false).
// Caller holds m.mu.
//
// The queue forms ONLY while a turn is running. With the session idle there is
// nothing to hold the prompt behind, so it goes straight through and no queue
// entry is ever created — which is why an idle session never shows chips.
func (m *Manager) queueSubmitLocked(d *driven, text, permissionMode string) (*queueEntry, bool) {
	if !d.turnActive {
		d.runningText = text
		return nil, false
	}
	e := &queueEntry{
		id:             newQueueEntryID(),
		text:           text,
		permissionMode: permissionMode,
		queuedAtMs:     m.now(),
		classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_PENDING,
	}
	d.queue.add(e)
	return e, true
}

// publishQueueLocked snapshots the queue for publication while the lock is
// held. The caller pushes and persists AFTER unlocking, so neither the frontend
// write nor the registry write happens under the manager mutex.
func (m *Manager) publishQueueLocked(d *driven) (*frontendv1.QueueView, []registry.QueuedPrompt) {
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
func (m *Manager) onTurnBoundary(d *driven, active bool) {
	m.mu.Lock()
	d.turnActive = active
	if active {
		m.mu.Unlock()
		return
	}
	// An entry that has been waiting to interject goes first — it asked for
	// exactly this moment. Otherwise the ordinary FIFO drain applies.
	e := d.queue.takeInterjecting()
	reason := "interject"
	if e == nil {
		e = d.queue.popFront()
		reason = "turn-end drain"
	}
	if e == nil {
		m.mu.Unlock()
		return
	}
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()

	m.logf("sessiondrv: queue delivering entry=%s (%s) session=%s ws=%q",
		e.id, reason, d.sessionID, d.workspace)
	m.publish(d.sessionID, view, recs)
	go m.deliver(d, e)
}

// deliver submits a held prompt as an ordinary prompt. Must run OFF the shim
// read-loop goroutine.
//
// On failure the entry is put BACK at the front of the queue, marked ERROR with
// the delivery failure as its rationale. Dropping it would lose something the
// user typed; retrying in place would spin. Instead it is visible, keeps its
// place, and gets another chance at the next turn end or on a user force.
func (m *Manager) deliver(d *driven, e *queueEntry) {
	text := m.applyMetapromptLocked(d, e.text)
	err := d.client.SubmitPrompt(m.rootCtx, text, "frontend", e.permissionMode)
	if err == nil {
		m.mu.Lock()
		d.runningText = e.text
		m.mu.Unlock()
		return
	}
	m.logf("sessiondrv: queue delivery FAILED entry=%s session=%s ws=%q: %v (prompt requeued)",
		e.id, d.sessionID, d.workspace, err)

	m.mu.Lock()
	e.interjecting = false
	e.classification = frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR
	e.rationale = fmt.Sprintf("delivery failed: %v", err)
	d.queue.entries = append([]*queueEntry{e}, d.queue.entries...)
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.publish(d.sessionID, view, recs)
}

// classify runs the injected classifier for one entry and applies the verdict.
// Runs on its own goroutine (the classifier may spawn a subprocess).
func (m *Manager) classify(d *driven, entryID, runningPrompt, queuedPrompt string) {
	res, err := m.runClassifier(d, runningPrompt, queuedPrompt)

	m.mu.Lock()
	e := d.queue.get(entryID)
	if e == nil {
		// Delivered or cancelled while the classifier ran. A verdict about an
		// entry that is already gone is MOOT, not an error: the prompt reached
		// the agent (or the user withdrew it) either way.
		m.mu.Unlock()
		m.logf("sessiondrv: queue verdict for a gone entry=%s session=%s (moot)", entryID, d.sessionID)
		return
	}
	if err != nil {
		// NEVER silently defaulted to a real verdict: a frontend has to be able
		// to see that nothing decided this. The entry stays queued and the
		// ordinary turn-end drain delivers it.
		e.classification = frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR
		e.rationale = err.Error()
		m.logf("sessiondrv: queue classify FAILED entry=%s session=%s: %v", entryID, d.sessionID, err)
	} else {
		e.classification = res.Classification
		e.rationale = res.Rationale
		m.logf("sessiondrv: queue classified entry=%s session=%s verdict=%s",
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
func (m *Manager) runClassifier(d *driven, runningPrompt, queuedPrompt string) (ClassifyResult, error) {
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
func (m *Manager) beginInterject(d *driven, entryID, source string) {
	m.mu.Lock()
	e := d.queue.get(entryID)
	if e == nil {
		m.mu.Unlock()
		return
	}
	if !d.turnActive {
		m.logf("sessiondrv: queue interject entry=%s (%s) is moot — the turn already ended; delivering normally",
			entryID, source)
		e.interjecting = true
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.publish(d.sessionID, view, recs)
		// No turn is running, so no TurnEnded is coming to trigger delivery.
		// Deliver directly rather than stranding the entry.
		m.mu.Lock()
		taken := d.queue.takeInterjecting()
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
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()

	// Push the INTERJECT state BEFORE interrupting, so the user sees why the
	// turn is about to stop rather than seeing it stop for no stated reason.
	m.publish(d.sessionID, view, recs)

	m.logf("sessiondrv: queue interjecting entry=%s (%s) session=%s — interrupting, will submit on TurnEnded",
		entryID, source, d.sessionID)
	go func() {
		if err := d.client.Interrupt(m.rootCtx, true); err != nil {
			m.logf("sessiondrv: queue interject interrupt FAILED entry=%s session=%s: %v", entryID, d.sessionID, err)
			m.mu.Lock()
			if cur := d.queue.get(entryID); cur != nil {
				cur.interjecting = false
				cur.classification = frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR
				cur.rationale = fmt.Sprintf("interrupt failed: %v", err)
			}
			view, recs := m.publishQueueLocked(d)
			m.mu.Unlock()
			m.publish(d.sessionID, view, recs)
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
	m.mu.Lock()
	known := d.queue.get(entryID) != nil
	m.mu.Unlock()
	if !known {
		return fmt.Errorf("sessiondrv: no queued prompt %q on workspace %q", entryID, workspace)
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
		return fmt.Errorf("sessiondrv: no queued prompt %q on workspace %q", entryID, workspace)
	}
	e.accepted = true
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.logf("sessiondrv: queue entry=%s accepted session=%s", entryID, d.sessionID)
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
		return fmt.Errorf("sessiondrv: no queued prompt %q on workspace %q", entryID, workspace)
	}
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()
	m.logf("sessiondrv: queue entry=%s cancelled session=%s", entryID, d.sessionID)
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
