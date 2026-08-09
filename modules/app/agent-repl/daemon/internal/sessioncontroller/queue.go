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
	promptOrigin   corev1.PromptOrigin
	queuedAtMs     int64

	classification Verdict
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
	// a field of its own rather than a fifth verdict: a classified
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

	// keepAliveHoldTurnID names the in-flight cache keep-alive turn holding
	// this entry, and is empty for every ordinary entry.
	//
	// It is the drain hold's SHAPE with a different reason and a different exit
	// set. Like a drain hold it is not a classification — the classifier never
	// runs on one of these, because the turn in front of the entry is a
	// machine-generated ping there is nothing to interject into — so the entry
	// keeps the HOLD stamp for its whole held life rather than claiming a
	// classifier is running that never will.
	//
	// Unlike a drain hold there is NO FORCE-THROUGH. A drain-held prompt can be
	// forced because delivering it merely delays a bounce; a keep-alive-held
	// prompt cannot, because the ping must COMPLETE before the daemon can
	// rewind the transcript that the ping is about to pollute, and delivering
	// the prompt first would submit it on top of the keep-alive turns the
	// rewind exists to discard. The two exits are delivery when the ping's turn
	// ends — the daemon rewinds, then submits — and QueueCancelCmd.
	keepAliveHoldTurnID string

	// revivalHoldSessionID names the session whose IN-FLIGHT COMPACT-FIRST
	// REVIVAL is parking this entry, and is empty for every ordinary entry.
	//
	// It is the third hold of the same shape, and it exists because the gate
	// alone was the wrong answer for this one window. A hibernated session with
	// no revival decision behind it refuses prompts outright — that is the whole
	// feature, and it is unchanged. But once the user HAS chosen compact-first,
	// the record stays hibernated on purpose while the compaction runs
	// (revive.go), and every prompt typed in that window was being nacked with
	// no queue entry made at all. The contract for that window is
	// DELAYED-NEVER-DROPPED: the prompt must not RUN before the compaction
	// lands, and it must be DELIVERED after it does.
	//
	// The classifier NEVER runs on one of these, exactly as it never runs on a
	// drain- or keep-alive-held entry: the turn in front of it is the daemon's
	// own `/compact`, and asking a model whether the user's prompt should
	// interrupt it could only produce a verdict that defeats the mode the user
	// chose. So the entry carries the HOLD stamp for its whole held life rather
	// than claiming a classifier is running that never will.
	//
	// The exits are delivery when the compaction lands and the gate is cleared,
	// a QueueCancelCmd, and the revival ending WITHOUT clearing the gate — the
	// bound expiry, the abandoned wait, a failed clear — which DROPS the entry
	// loudly (revive.go, dropRevivalHolds). There is no force-through: forcing
	// a prompt past the compaction would spend the whole uncompacted history on
	// it, which is the one cost compact-first exists to avoid.
	revivalHoldSessionID string

	// buildRefreshHoldSessionID names the session whose ARMED OR RUNNING
	// stale-build refresh is parking this entry, and is empty for every
	// ordinary entry.
	//
	// It is the fourth hold of the same shape and it covers the window a
	// deferred shim bounce opens: a shim running superseded code reattached
	// mid-turn, so the refresh waits for that turn's boundary and then restarts
	// the process (turnboundaryrefresh.go). A prompt typed anywhere in that
	// window — while the lease waits, and while the restart runs — must not be
	// submitted to a shim that is about to be SIGTERMed, and must not be
	// refused either: the contract is DELAYED-NEVER-DROPPED, exactly as it is
	// for the revival hold.
	//
	// The classifier NEVER runs on one of these. The turn in front of it is the
	// turn the lease is waiting on, and a verdict of INTERJECT could only
	// interrupt the very turn the deferral exists to protect — so the entry
	// carries the HOLD stamp for its whole held life rather than claiming a
	// classifier is running that never will.
	//
	// THE EXITS ARE DELIVERY AND CANCEL, and there is no third. The restart
	// releases the hold and delivers in order; a lease that is disarmed without
	// restarting (an unknown settledness verdict, a controller exit) releases
	// the hold in place so the entry becomes an ordinary queued prompt. Nothing
	// drops one: unlike a revival, the session is not left asleep, so an
	// un-held entry always has a boundary that can deliver it.
	//
	// A FORCE IS NOT REFUSED, which is what distinguishes it from the
	// keep-alive and revival holds. Forcing one cannot do the harm those
	// refusals exist to prevent — the entry is still held, so every delivery
	// path still refuses to submit it — and the interrupt a force sends ENDS
	// the turn the lease is waiting on, which is the user bringing the refresh
	// forward rather than defeating it.
	buildRefreshHoldSessionID string
}

// drainHeld reports whether a scheduled shutdown's lease is parking this entry.
func (e *queueEntry) drainHeld() bool { return e.shutdownHoldScheduleID != "" }

// keepAliveHeld reports whether an in-flight cache keep-alive turn is holding
// this entry.
func (e *queueEntry) keepAliveHeld() bool { return e.keepAliveHoldTurnID != "" }

// revivalHeld reports whether an in-flight compact-first revival is parking
// this entry.
func (e *queueEntry) revivalHeld() bool { return e.revivalHoldSessionID != "" }

// buildRefreshHeld reports whether an armed or running stale-build refresh is
// parking this entry.
func (e *queueEntry) buildRefreshHeld() bool { return e.buildRefreshHoldSessionID != "" }

// held reports whether ANY hold is parking this entry, whatever its kind. It is
// the predicate every delivery-selection path asks, so a hold added later is
// honored by all of them without each having to learn its name.
func (e *queueEntry) held() bool {
	return e.drainHeld() || e.keepAliveHeld() || e.revivalHeld() || e.buildRefreshHeld()
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
		if e.held() {
			continue
		}
		q.entries = append(q.entries[:i], q.entries[i+1:]...)
		return e
	}
	return nil
}

// keepAliveHeldIDs reports the ids of every entry held behind turnID, front to
// back. Ids rather than pointers because the caller acts on them after
// releasing the mutex, and an entry can be cancelled in between — a stale
// pointer would be a prompt the user took back, delivered anyway.
func (q *promptQueue) keepAliveHeldIDs(turnID string) []string {
	var out []string
	for _, e := range q.entries {
		if e.keepAliveHoldTurnID == turnID {
			out = append(out, e.id)
		}
	}
	return out
}

// releaseKeepAliveHold clears turnID's hold from every entry carrying it and
// reports how many were released.
func (q *promptQueue) releaseKeepAliveHold(turnID string) int {
	n := 0
	for _, e := range q.entries {
		if e.keepAliveHoldTurnID == turnID {
			e.keepAliveHoldTurnID = ""
			// The HOLD stamp was standing in for a classification that never
			// ran. With the hold gone the entry is an ordinary queued prompt
			// about to be delivered, and leaving it stamped HOLD would render a
			// chip claiming it is still waiting on something.
			e.classification = VerdictPending
			n++
		}
	}
	return n
}

// releaseRevivalHold clears sessionID's revival hold from every entry carrying
// it and reports how many were released.
//
// It is releaseKeepAliveHold's shape and re-stamps for the same reason: the
// HOLD stamp was standing in for a classification that never ran, and an entry
// about to be delivered must not render a chip claiming it is still waiting on
// a compaction that has landed.
func (q *promptQueue) releaseRevivalHold(sessionID string) int {
	n := 0
	for _, e := range q.entries {
		if e.revivalHoldSessionID == sessionID {
			e.revivalHoldSessionID = ""
			e.classification = VerdictPending
			n++
		}
	}
	return n
}

// releaseBuildRefreshHold clears sessionID's stale-build refresh hold from
// every entry carrying it and reports how many were released.
//
// It is releaseRevivalHold's shape and re-stamps for the same reason: the HOLD
// stamp was standing in for a classification that never ran, and an entry about
// to be delivered must not render a chip claiming it is still waiting on a
// restart that has already happened. The rationale goes with it, because the
// account it carried — "waiting for the shim to restart" — stops being true in
// the same instant.
func (q *promptQueue) releaseBuildRefreshHold(sessionID string) int {
	n := 0
	for _, e := range q.entries {
		if e.buildRefreshHoldSessionID == sessionID {
			e.buildRefreshHoldSessionID = ""
			e.classification = VerdictPending
			e.rationale = ""
			n++
		}
	}
	return n
}

// dropRevivalHeld removes every entry sessionID's revival is parking and
// returns them, front to back.
//
// IT IS THE PARKED ENTRY'S BOUND. A revival that ends without clearing the gate
// leaves a session that is still asleep, and an entry held behind it can never
// be selected by any delivery path — so keeping it would be a prompt rendered
// as pending forever, on a queue nothing will ever drain. The entries are
// removed here and the caller states the loss out loud (revive.go).
func (q *promptQueue) dropRevivalHeld(sessionID string) []*queueEntry {
	var dropped []*queueEntry
	kept := q.entries[:0]
	for _, e := range q.entries {
		if e.revivalHoldSessionID == sessionID {
			dropped = append(dropped, e)
			continue
		}
		kept = append(kept, e)
	}
	q.entries = kept
	return dropped
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
		if e.headJump && !e.held() {
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
		if e.interjecting && !e.held() {
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

// pushFrontAll puts entries back at the head of the queue, keeping their order
// among themselves and ahead of whatever arrived while they were away.
//
// AHEAD, not behind: these are the prompts the rewind carried across the
// bounce, and anything now sitting in this queue was typed after them. Putting
// them at the back would reorder the user's own prompts as the price of tidying
// the transcript.
func (q *promptQueue) pushFrontAll(entries []*queueEntry) {
	if len(entries) == 0 {
		return
	}
	q.entries = append(append(make([]*queueEntry, 0, len(entries)+len(q.entries)), entries...), q.entries...)
}

// view renders the queue as the pushed frontend frame, front to back. An empty
// queue renders as a QueueView with no entries rather than as no frame at all:
// "the queue is now empty" is exactly the state a frontend needs told.
func (q *promptQueue) view(workspace, fence string) *frontendv1.QueueView {
	v := &frontendv1.QueueView{Workspace: workspace, Fence: fence}
	for _, e := range q.entries {
		entry := &frontendv1.QueueEntry{
			Id:         e.id,
			Text:       e.text,
			QueuedAtMs: e.queuedAtMs,
		}
		// The entry holds ONE free-text account, which the wire files under the
		// field its verdict owns: a rationale on interject/hold, a detail on
		// error. Passing it for both is what keeps the daemon from carrying two
		// strings that can disagree about the same thing.
		setClassification(entry, e.classification, e.rationale, e.rationale, e.accepted)
		if e.drainHeld() {
			entry.Hold = &frontendv1.QueueEntry_Shutdown{
				Shutdown: &frontendv1.QueueEntryShutdownHold{ScheduleId: e.shutdownHoldScheduleID},
			}
		}
		// The keep-alive hold is projected beside the drain hold and never
		// instead of a classification: the webapp renders a dedicated "waiting
		// on a keep-alive response" bubble from this field, which is the honest
		// account of a prompt waiting on a turn nobody asked for.
		if e.keepAliveHeld() {
			entry.Hold = &frontendv1.QueueEntry_KeepAlive{
				KeepAlive: &frontendv1.QueueEntryKeepAliveHold{TurnId: e.keepAliveHoldTurnID},
			}
		}
		// The revival hold is projected the same way, and for the sharper reason:
		// without this field a compact-first-parked entry reaches the frontend as
		// a bare HOLD with no account of what it is waiting on, and the webapp
		// cannot tell the user that their prompt is queued behind the daemon's own
		// `/compact` rather than behind a classifier that will never run on it.
		if e.revivalHeld() {
			// The arm's PRESENCE is the whole fact. It used to name the session
			// being revived; a revival is a workspace-level event and the entry
			// already rides its workspace's queue, so the id joined the bubble to
			// nothing the client could not already reach.
			entry.Hold = &frontendv1.QueueEntry_Revival{
				Revival: &frontendv1.QueueEntryRevivalHold{},
			}
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
	Classification Verdict
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
func (m *Manager) queueSubmitLocked(d *sessionController, requestID, text, permissionMode string, promptOrigin corev1.PromptOrigin, leaseScheduleID string) (*queueEntry, bool, error) {
	if scheduleID := leaseScheduleID; scheduleID != "" {
		e := newParkedEntry(newQueueEntryID(), requestID, text, permissionMode, promptOrigin, m.now())
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
			e.id, d.workspace, d.sessionID, scheduleID, d.turn.active())
		return e, true, nil
	}
	// A PROMPT ARRIVING MID-REVIVAL IS PARKED, not nacked. The user has already
	// made the revival choice, the session is up, and the record is still
	// hibernated only because that is how compact-first keeps the gate standing
	// while its `/compact` runs — so the honest answer to a prompt typed in that
	// window is "delayed", not "refused". The classifier never runs on it, for
	// the same reason it never runs on a drain- or keep-alive-held entry.
	//
	// It is checked BEFORE the turn-active test on purpose, exactly as the
	// keep-alive hold is: the compaction's turn is normally active here, so the
	// ordinary queueing path would catch the prompt anyway — but it would stamp
	// it PENDING and spend a model call asking whether the user's prompt should
	// interrupt the daemon's own compaction, whose interruption is precisely
	// what the mode exists to prevent.
	if revivalSessionID := m.revivalHoldSessionLocked(d); revivalSessionID != "" {
		e := &queueEntry{
			id:                   newQueueEntryID(),
			requestID:            requestID,
			text:                 text,
			permissionMode:       permissionMode,
			promptOrigin:         promptOrigin,
			queuedAtMs:           m.now(),
			classification:       VerdictHold,
			revivalHoldSessionID: revivalSessionID,
		}
		// Appended at the BACK even against a paused queue, exactly as the other
		// two holds are: a head jump is the paused queue's one deliverable, and a
		// held entry is by definition not deliverable, so claiming that position
		// would be a lie about when it runs.
		d.queue.add(e)
		m.logf("session-controller: prompt PARKED by the revival gate entry=%s ws=%q session=%s turn_active=%v — a compact-first revival's compaction is still pending, so this prompt is DELAYED until it lands; it is not classified and not refused",
			e.id, d.workspace, d.sessionID, d.turn.active())
		return e, true, nil
	}
	// A REAL PROMPT ARRIVING MID-PING IS HELD, not raced against the ping and
	// not allowed to interrupt it. The ping's turn must COMPLETE before the
	// daemon can rewind the transcript it is polluting, and only after that
	// rewind may this prompt be submitted — so the hold is what makes the
	// ordering "ping ends, rewind, submit" rather than a race.
	//
	// It is taken BEFORE the turn-active test on purpose. The ping's turn is
	// normally active here, so the ordinary queueing path would catch the
	// prompt anyway — but it would stamp it PENDING and run the classifier on
	// it, asking a model whether the user's prompt should interrupt a
	// machine-generated ping. Checking first is what makes the hold, not the
	// classification, the thing that describes this entry.
	//
	// THE HOLD SPANS THE AFTERMATH TOO. keepAliveHoldTurnLocked answers with the
	// ping's own claim while its turn runs and with the workspace's rewind claim
	// afterwards, so there is no instant between "the ping ended" and "the
	// rewound session is back up" in which this test says no.
	if holdTurnID := m.keepAliveHoldTurnLocked(d); holdTurnID != "" {
		e := &queueEntry{
			id:                  newQueueEntryID(),
			requestID:           requestID,
			text:                text,
			permissionMode:      permissionMode,
			promptOrigin:        promptOrigin,
			queuedAtMs:          m.now(),
			classification:      VerdictHold,
			keepAliveHoldTurnID: holdTurnID,
		}
		// Appended at the BACK even against a paused queue, exactly as a
		// drain-parked entry is: a head jump is the paused queue's one
		// deliverable, and a held entry is by definition not deliverable, so
		// claiming that position would be a lie about when it runs.
		d.queue.add(e)
		m.logf("session-controller: prompt HELD behind a cache keep-alive entry=%s ws=%q session=%s keep_alive_turn=%s turn_active=%v — the ping must finish so the daemon can rewind it out of the transcript before this prompt is submitted; it is not classified and not refused",
			e.id, d.workspace, d.sessionID, holdTurnID, d.turn.active())
		return e, true, nil
	}
	// A PROMPT ARRIVING BEHIND AN ARMED OR RUNNING STALE-BUILD REFRESH IS
	// PARKED, not forwarded and not refused. The shim serving this session is
	// running superseded code and is going to be restarted the moment its turn
	// resolves, so submitting into it would either be thrown away by the
	// SIGTERM or answered by the very bundle the refresh exists to retire.
	//
	// It is checked BEFORE the turn-active test for the reason the other holds
	// are: while the lease is ARMED a turn is normally running, so the ordinary
	// queueing path would catch the prompt anyway — but it would stamp it
	// PENDING and spend a model call asking whether it should INTERJECT, whose
	// only possible effect is to interrupt the turn the deferral exists to
	// protect. And while the RESTART runs no turn is active at all, which is
	// exactly the window in which the ordinary path would forward it straight
	// into a dying shim.
	if refreshSessionID := m.buildRefreshHoldSessionLocked(d); refreshSessionID != "" {
		e := &queueEntry{
			id:                        newQueueEntryID(),
			requestID:                 requestID,
			text:                      text,
			permissionMode:            permissionMode,
			promptOrigin:              promptOrigin,
			queuedAtMs:                m.now(),
			classification:            VerdictHold,
			rationale:                 staleRefreshArmParkRationale(refreshSessionID),
			buildRefreshHoldSessionID: refreshSessionID,
		}
		// Appended at the BACK even against a paused queue, exactly as the other
		// three holds are: a head jump is the paused queue's one deliverable, and
		// a held entry is by definition not deliverable, so claiming that
		// position would be a lie about when it runs.
		d.queue.add(e)
		m.logf("session-controller: prompt PARKED behind a stale-build refresh entry=%s ws=%q session=%s turn_active=%v — the shim serving this session is running superseded code and is restarted at the turn's boundary; this prompt is DELAYED until the replacement is ready, and is delivered in the order it was typed",
			e.id, d.workspace, d.sessionID, d.turn.active())
		return e, true, nil
	}
	if !d.turn.active() {
		d.runningText = text
		d.runningPermissionMode = permissionMode
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
		promptOrigin:   promptOrigin,
		queuedAtMs:     m.now(),
		classification: VerdictPending,
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
	return d.queue.view(d.workspace, d.sessionID), d.queue.records()
}

// records renders the queue as the durable registry evidence of what this
// daemon is holding. Shared with the materialized parked ledger, which persists
// the same evidence for a session that has not wired: two renderings of "what
// is queued" would be two answers to it.
func (q *promptQueue) records() []registry.QueuedPrompt {
	recs := make([]registry.QueuedPrompt, 0, len(q.entries))
	for _, e := range q.entries {
		recs = append(recs, registry.QueuedPrompt{
			ID:             e.id,
			Text:           e.text,
			PermissionMode: e.permissionMode,
			QueuedAtMs:     e.queuedAtMs,
		})
	}
	return recs
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

// onTurnBoundary acts on an observed turn boundary's EDGE and, on a turn END,
// delivers the next held prompt if there is one.
//
// This is the whole reason the interject sequence is evented. An interject
// sends an Interrupt and then must NOT submit until the turn has actually
// ended — submitting into a turn that is still tearing down races the teardown.
// So the interrupt and the submit are separated by exactly this callback: the
// TurnEnded the shim really reported.
//
// THE RECORD IS NOT WRITTEN ON THE START EDGE HERE. A start binds the turn
// record earlier, off the durable claim set the SSM's turn ledger just accepted
// (noteTurnClaims), so nothing user-visible — not the SSM apply, not the
// WorkspaceState it publishes — can move between "a turn is active" and "this is
// which turn". A start reaching this function therefore finds the record already
// bound, and the only thing left for it is the drain notification.
//
// The END is the other direction and stays HERE, after the SSM has applied the
// boundary: releasing the record early would let a scheduled bounce stop
// holding for a turn the SSM has not finished accounting for, and a bounce that
// cuts live work is the one failure this lease exists to prevent.
//
// Called on the shim read-loop goroutine, so delivery is dispatched to its own
// goroutine: SubmitPrompt awaits an Ack that only the read loop can deliver, and
// calling it from inside the read loop would deadlock the session.
// endedAtMs is the BOUNDARY'S OWN INSTANT, carried from the event rather than
// re-read here. The keep-alive window's closing edge is stamped from it, and
// that edge is compared against vendor-authored record timestamps: a clock read
// taken at handling time would put the window's end wherever the daemon's clock
// happened to be, not where the turn actually ended.
func (m *Manager) onTurnBoundary(d *sessionController, active bool, endedAtMs int64) {
	if active {
		// The record is normally already bound by the claim projection, and this
		// adopt is then a NO-OP: an active record keeps the name it has. It is
		// applied all the same so that an active edge reaching the queue with
		// nothing bound behind it still leaves a record that says a turn is
		// running — as `adopted`, the one phase honest about not being able to
		// name it, never as an active flag with an empty id beside it.
		m.mu.Lock()
		before, changed := d.noteTurnAdoptedLocked(true)
		m.mu.Unlock()
		if changed {
			m.logf("session-controller: turn record ADOPTED at an active edge ws=%q session=%s before=%s after=adopted edge=turn_start — the boundary reached the queue with no durable claim bound, so the turn holds the drain and names nothing",
				d.workspace, d.sessionID, before)
		}
		// A turn STARTING is a new drain hold. Told with the mutex RELEASED (the
		// engine re-reads DrainHolds, which takes it).
		m.noteDrainActivity()
		return
	}
	m.mu.Lock()
	// THE ENDING TURN'S NAME, read BEFORE the record is released: releasing it
	// first would leave nothing to match the keep-alive claim against, and an
	// unconditional release would let a late end for some OTHER turn free a
	// hold the ping still owns.
	endingTurnID, _ := d.turn.name()
	before, changed := d.noteTurnIdleLocked()
	if changed {
		m.logf("session-controller: turn record RELEASED ws=%q session=%s before=%s after=idle edge=turn_end — the durable ledger holds no further claim for this session",
			d.workspace, d.sessionID, before)
	}
	// The turn that just ended: was it one a user-commanded stop was delivered
	// to, and was it the lone prompt running against a paused queue? Both are
	// consumed here, whatever the boundary goes on to decide.
	wasInterrupted, wasLoneRunner := d.interruptedTurn, d.pausedRunner
	d.interruptedTurn, d.pausedRunner = false, false

	// A DAEMON COMPACTION'S CLAIM IS RETIRED AT ITS OWN TURN'S END, matched on
	// the ending turn's name for the keep-alive claim's reason: a late end for
	// some other turn must not retire a claim this compaction still owns.
	//
	// NOTHING IS DECIDED HERE. The cold-read alarm is a REPORT, not a lifecycle
	// transition, so it is taken at the observation itself (compactioncold.go) —
	// which by then has already happened, because the terminal result reaches
	// the consumer on this same goroutine before the boundary that ends the
	// turn. This is only the release, and it is placed ahead of the keep-alive
	// branch below because that branch returns.
	if m.releaseDaemonCompactionLocked(d, endingTurnID) {
		m.logf("session-controller: daemon compaction turn ENDED ws=%q session=%s turn_id=%s — the cold-read claim is retired; its verdict was already taken when the terminal result was reduced",
			d.workspace, d.sessionID, endingTurnID)
	}

	// THE KEEP-ALIVE PING'S END is the event the whole rewind sequence hangs
	// off. It is taken here, at the same boundary the ordinary drain uses, so
	// there is one place that decides what a turn ending means.
	//
	// The release is dispatched to its own goroutine AND this boundary returns:
	// the rewind stops and respawns the shim, which cannot happen on the shim
	// read-loop goroutine this runs on, and the ordinary drain must not deliver
	// anything into a session that is about to be bounced.
	pingTurn := d.keepAliveTurnID
	// THE PING'S MEASUREMENT LEAVES WITH ITS CLAIM, in this same acquisition.
	// The ping's terminal result filled it earlier on this very goroutine, so it
	// is complete by the time the boundary that ended that turn can read it —
	// which is what makes the cold-ping hibernation ordered after the turn end
	// structurally rather than by timing (keepalivecold.go).
	pingMeasurement, pingEnded := d.noteKeepAliveTurnEndedLocked(endingTurnID)
	if pingEnded {
		heldIDs := d.queue.keepAliveHeldIDs(pingTurn)
		// THE CLAIM IS TRANSFERRED, NOT DROPPED, whenever an aftermath is going
		// to run. noteKeepAliveTurnEndedLocked has just cleared the ping's own
		// claim; taking the rewind's claim in the SAME acquisition is what
		// leaves no instant in which a fresh prompt is admitted, starts a real
		// turn, and is then SIGTERMed and truncated out by the rewind that was
		// already on its way.
		// THE DEBT IS RECORDED IN THE SAME ACQUISITION THAT CLEARS THE CLAIM.
		// The ping's turns are now standing in the vendor transcript, and they
		// stay there until something rewinds them out — which, when nothing was
		// waiting on this ping, is some LATER path's job. Recording it here is
		// what makes that job exist at all; without it, the pings simply became
		// permanent context (keepaliveresidue.go).
		m.noteKeepAliveResidueLocked(d.workspace, pingTurn)
		rewinding := len(heldIDs) > 0
		if rewinding {
			m.claimKeepAliveRewindLocked(d.workspace, pingTurn)
		}
		m.mu.Unlock()
		m.logf("session-controller: keep-alive turn ENDED ws=%q session=%s turn_id=%s held_prompts=%d",
			d.workspace, d.sessionID, pingTurn, len(heldIDs))
		// THE WINDOW CLOSES HERE. An open window has no upper bound, so leaving
		// it open would exclude every later item on this workspace forever —
		// the whole conversation, silently.
		//
		// IT CLOSES AT THE TURN'S OWN END INSTANT, not at a fresh clock read.
		// The verdict this bound decides is against the timestamps the VENDOR
		// wrote on its transcript records, so a daemon clock running ahead of
		// the CLI's would keep excluding after the ping was over and swallow
		// the leading edge of the user's next real turn. The boundary's
		// produced_at_ms is the same instant the rest of the daemon agrees the
		// turn ended at, and it is durable evidence rather than an observation
		// made at the moment of writing.
		m.closeKeepAliveWindow(d, pingTurn, endedAtMs)
		m.noteDrainActivity()
		// THE PING'S OWN VERDICT ON THE CACHE, taken after the window is closed
		// and the drain is told, so a hibernation it decides on finds this ping
		// fully accounted for rather than half-retired (keepalivecold.go).
		m.actOnColdKeepAlivePing(d, pingMeasurement, rewinding)
		if rewinding {
			go m.releaseKeepAliveHolds(d, pingTurn, heldIDs)
			return
		}
		// Nothing was waiting on the ping, so no rewind runs HERE: there is no
		// prompt to deliver behind it, and bouncing the shim for nobody would
		// pay a bring-up to tidy a transcript nothing is about to read. The
		// turns stay in the transcript and the debt recorded above says so; the
		// next user-facing submission or session resume settles it BEFORE
		// putting anything on top of them (keepaliveresidue.go). This is the
		// case that used to leave the pings as permanent model context.
		return
	}

	// THE PAUSE RESUMES on the clean end of a prompt that ran alone. The user
	// stopped the agent, ran one thing, and that thing finished — which is the
	// signal that the work they stopped may continue. An interrupted lone
	// runner is the opposite signal, so the pause stands.
	if d.paused && wasLoneRunner && !wasInterrupted {
		d.paused = false
		m.logf("session-controller: queue RESUMED session=%s ws=%q — the prompt that ran alone finished cleanly; draining %d retained entr(ies) in their original order",
			d.sessionID, d.workspace, len(d.queue.entries))
	}

	// THE ARMED STALE-BUILD REFRESH CLAIMS THIS BOUNDARY, and it claims it
	// BEFORE any delivery is selected.
	//
	// This is the event the lease has been waiting for: the turn a stale shim
	// was mid-way through when it reattached has resolved — ended, interrupted,
	// or cut, all of which arrive here as the same inactive edge — so the
	// refresh may finally stop that shim without throwing work away.
	//
	// NOTHING IS DELIVERED ON THIS BOUNDARY. The prompts the lease parked are
	// held and no selection path would take them anyway, but an entry that
	// PREDATES the lease (one a previous daemon parked and this session
	// restored) is not held, and submitting it here would start a turn into a
	// shim that is about to be SIGTERMed. The restart delivers everything in
	// order once the replacement is ready.
	//
	// A KEEP-ALIVE PING'S END NEVER REACHES HERE — that branch returns above —
	// and it should not: the ping's aftermath bounces the shim on its own terms,
	// and the lease simply stays armed for the next boundary.
	if arm := m.claimStaleRefreshAtBoundaryLocked(d); arm != nil {
		queued := len(d.queue.entries)
		m.mu.Unlock()
		m.logf("session-controller: turn boundary CLAIMED by an armed stale-build refresh ws=%q session=%s generation=%s queued=%d — the turn the stale shim was running has resolved, so the deferred restart runs now and nothing is delivered into the shim it is about to replace",
			d.workspace, d.sessionID, d.generationID, queued)
		m.noteDrainActivity()
		go m.runStaleRefreshAtBoundary(d, arm)
		return
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
	// THE KEEP-ALIVE HOLD'S BACKSTOP, at the same funnel and for the same
	// reason. Delivering a keep-alive-held prompt would submit it on top of the
	// very keep-alive turns the rewind exists to discard, which is worse than
	// delaying it: the ping would become permanent context.
	if e.keepAliveHeld() {
		m.mu.Lock()
		d.queue.pushFront(e)
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.logf("session-controller: queue delivery REFUSED entry=%s session=%s ws=%q keep_alive_turn=%s — the entry is held behind an in-flight cache keep-alive turn and a delivery path selected it anyway; requeued at the head, nothing was submitted",
			e.id, d.sessionID, d.workspace, e.keepAliveHoldTurnID)
		m.publish(d.sessionID, view, recs)
		return
	}
	// THE REVIVAL HOLD'S BACKSTOP, at the same funnel and for the same reason.
	// Delivering a revival-held prompt would submit it against the whole
	// uncompacted conversation the user chose compact-first precisely to avoid
	// paying for — and forwardPrompt's own gate would nack it anyway, marking a
	// prompt ERROR that nothing was wrong with. Requeued at the head, loudly.
	if e.revivalHeld() {
		m.mu.Lock()
		d.queue.pushFront(e)
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.logf("session-controller: queue delivery REFUSED entry=%s session=%s ws=%q revival_session=%s — the entry is parked by an in-flight compact-first revival and a delivery path selected it anyway; requeued at the head, nothing was submitted",
			e.id, d.sessionID, d.workspace, e.revivalHoldSessionID)
		m.publish(d.sessionID, view, recs)
		return
	}
	// THE STALE-BUILD REFRESH HOLD'S BACKSTOP, at the same funnel and for the
	// same reason. Delivering one would submit the prompt into a shim the daemon
	// is about to SIGTERM onto a new bundle: the turn it started would be cut,
	// and the prompt would be lost in the very restart that was supposed to
	// deliver it. Requeued at the head, loudly.
	if e.buildRefreshHeld() {
		m.mu.Lock()
		d.queue.pushFront(e)
		view, recs := m.publishQueueLocked(d)
		m.mu.Unlock()
		m.logf("session-controller: queue delivery REFUSED entry=%s session=%s ws=%q build_refresh_session=%s — the entry is parked behind an armed or running stale-build refresh and a delivery path selected it anyway; requeued at the head, nothing was submitted",
			e.id, d.sessionID, d.workspace, e.buildRefreshHoldSessionID)
		m.publish(d.sessionID, view, recs)
		return
	}
	err := m.forwardPrompt(m.rootCtx, d, e.requestID, e.text, e.promptOrigin.String(), e.permissionMode, e.promptOrigin, submitterUser)
	if err == nil {
		m.mu.Lock()
		d.runningText = e.text
		d.runningPermissionMode = e.permissionMode
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
	e.classification = VerdictError
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
		e.classification = VerdictError
		e.rationale = err.Error()
		m.logf("session-controller: queue classify FAILED entry=%s session=%s: %v", entryID, d.sessionID, err)
	} else {
		e.classification = res.Classification
		e.rationale = res.Rationale
		m.logf("session-controller: queue classified entry=%s session=%s verdict=%s",
			entryID, d.sessionID, res.Classification)
	}
	interject := e.classification == VerdictInterject
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
	if !d.turn.active() {
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
		// have landed in that window and d.turn.active() can be true again. The
		// "moot" reasoning that got us here — there is no turn, so delivering
		// now is safe — no longer holds, and submitting would put the prompt
		// into a turn that IS running, which is precisely what the queue
		// exists to prevent. So the turn state is re-verified under the
		// RE-TAKEN lock before the entry is committed to delivery.
		if taken != nil && d.paused && !d.turn.active() {
			// The queue is PAUSED, so this delivery is the one prompt running
			// alone against it and its clean end is what resumes the drain.
			// Marked as a head jump too, so a boundary arriving before it
			// lands still treats it as the paused queue's one deliverable.
			taken.headJump = true
			d.pausedRunner = true
		}
		if taken != nil && d.turn.active() {
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
	e.classification = VerdictInterject
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
		// THE INTERJECT'S OWN ENTRY, not a user request. This stop is pure
		// machinery -- the user asked for a prompt to run sooner, not for the
		// turn to end -- and naming the entry is what keeps it distinguishable
		// in the log from the user-commanded stop.
		outcome, err := d.client.Interrupt(m.rootCtx, "interject:"+entryID)
		if err == nil {
			err = errclass.InterruptError(outcome)
		}
		if err != nil {
			m.logf("session-controller: queue interject interrupt FAILED entry=%s session=%s outcome=%s: %v",
				entryID, d.sessionID, outcome, err)
			m.mu.Lock()
			if cur := d.queue.get(entryID); cur != nil {
				cur.interjecting = false
				cur.classification = VerdictError
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
	// THE MATERIALIZED LEDGER IS CONSULTED FIRST, through the resolver every
	// queue command shares, because a client can see and aim at a prompt whose
	// session has not wired. A force can only refuse it, and it refuses with the
	// session named and the failure TYPED (parkedledger.go).
	//
	// ONE ACQUISITION SPANS RESOLVE, DROP AND COMMIT. The drain lease's own
	// parking write (parkForDrain) creates the durable row under m.mu; taking the
	// drop under the SAME mutex is what makes one lock arbitrate both the row's
	// creation and its destruction. Snapshotting drainRowPending, releasing, and
	// then skipping the drop left a window in which AcquireShutdownHolds wrote a
	// row for this very entry — so the force committed, delivered the prompt, and
	// left a row standing that the next boot replays as a SECOND run of a prompt
	// the user forced exactly once.
	m.mu.Lock()
	owner, err := m.resolveQueueEntryLocked(workspace, entryID)
	if err != nil {
		m.mu.Unlock()
		return err
	}
	if owner.parked != nil {
		sessionID, wired := owner.sessionID(), owner.wired
		m.mu.Unlock()
		return m.refuseParkedForce(workspace, entryID, sessionID, wired)
	}
	d := owner.live
	e := owner.entryLocked(entryID)
	// A KEEP-ALIVE HOLD HAS NO FORCE-THROUGH, and this is the one place that
	// has to say so. A drain-held prompt can be forced because delivering it
	// only delays a bounce the user is choosing to delay. A keep-alive-held
	// prompt cannot: the ping's turn has to COMPLETE before the daemon can
	// rewind it out of the transcript, and forcing the prompt through would
	// submit it on top of the keep-alive turns the rewind exists to discard —
	// making the ping permanent context instead of temporary plumbing. The
	// wait is short and bounded by a turn that is already running, so the
	// refusal costs the user seconds and preserves the guarantee.
	if e.keepAliveHeld() {
		keepAliveTurn := e.keepAliveHoldTurnID
		m.mu.Unlock()
		return m.refuseKeepAliveForce(workspace, entryID, d.sessionID, keepAliveTurn)
	}
	// A REVIVAL HOLD HAS NO FORCE-THROUGH EITHER, and its argument is the
	// strongest of the three. The user chose compact-first to pay the
	// whole-conversation cost exactly once; forcing a prompt past the pending
	// compaction would answer it against the uncompacted history and spend that
	// cost anyway — the one outcome the mode exists to prevent. The wait is
	// bounded by the compaction, and the prompt stays cancellable throughout.
	if e.revivalHeld() {
		revivalSession := e.revivalHoldSessionID
		m.mu.Unlock()
		return m.refuseRevivalForce(workspace, entryID, revivalSession)
	}
	forcedSchedule, hadRow := e.shutdownHoldScheduleID, e.drainRowPending

	// THE DURABLE ROW GOES FIRST, before the entry becomes deliverable. A force
	// ends in a submitted prompt, and an entry whose row outlives that submit is
	// a prompt the daemon that comes back from the bounce re-materializes and
	// delivers a SECOND time — the user forced one prompt and got two. So the
	// drop's failure refuses the force instead of being swallowed: the entry is
	// left exactly as it was, still parked, still forceable once the store is
	// answering again.
	if hadRow {
		if err := m.dropDrainRow(entryID, "forced_by_user"); err != nil {
			m.mu.Unlock()
			m.logf("session-controller: force REFUSED entry=%s ws=%q session=%s schedule=%s error=%v — the durable parking row could not be dropped, so the entry is left parked rather than delivered; delivering it with its row standing would let the daemon that comes back run the same prompt again",
				entryID, workspace, d.sessionID, forcedSchedule, err)
			return fmt.Errorf("session-controller: cannot force queued prompt %q on workspace %q: %w", entryID, workspace, err)
		}
	}

	// A FORCE OVERRIDES THE DRAIN LEASE, and that is the point of the control.
	// The user is looking at a bubble that says their prompt is waiting for a
	// scheduled bounce and is telling the daemon to run it anyway. The turn it
	// starts then becomes a drain hold of its own and delays the bounce further
	// — which is exactly what they asked for, so it is honored rather than
	// second-guessed, and loud-logged so the delay has a stated author.
	//
	// The re-fetch is kept as an assertion rather than as a recovery: nothing can
	// take the entry out from under a mutex this path never released, so a miss
	// here is a broken invariant and is reported as the loud failure it is.
	if e = d.queue.get(entryID); e == nil {
		m.mu.Unlock()
		m.logf("session-controller: force found NOTHING to deliver entry=%s ws=%q session=%s — the entry left the queue while this force held m.mu, which nothing is supposed to be able to do; its durable row is already gone, so nothing will re-materialize it",
			entryID, workspace, d.sessionID)
		return fmt.Errorf("session-controller: no queued prompt %q on workspace %q", entryID, workspace)
	}
	e.shutdownHoldScheduleID = ""
	e.drainRowPending = false
	// The row drop and the tombstone are ONE step, exactly as they are for a
	// cancel. A force that dropped a row is the other half of the resurrection
	// the tombstone set exists to stop: the interject below takes the entry out
	// of the queue, so the apply loop's `d.queue.get` dedupe no longer covers it,
	// and a restore holding a snapshot from before the drop would re-queue a
	// prompt that has already been delivered (shutdownlease.go).
	if hadRow {
		m.noteRowDroppedTombstoneLocked(workspace, entryID, "forced_by_user")
	}
	m.mu.Unlock()
	if forcedSchedule != "" {
		m.logf("session-controller: drain hold FORCED entry=%s ws=%q session=%s schedule=%s initiator=user — the user asked for a prompt parked by a scheduled shutdown to run now; the turn it starts will hold the drain open until it ends",
			entryID, workspace, d.sessionID, forcedSchedule)
	}
	m.beginInterject(d, entryID, "user")
	return nil
}

// AcceptQueueEntry confirms a held prompt's classification. VIEW STATE ONLY: it
// records that the user saw it and changes nothing about when the prompt is
// delivered, so the control cannot imply a power it does not have.
//
// IT IS HONORED ON A MATERIALIZED PROMPT TOO, through the shared resolver. That
// is the same argument cancel already makes: accept changes view state and
// needs no shim, so refusing it because the session has not wired would be the
// daemon withholding a control that costs nothing to honor — and it used to
// refuse it with "no queued prompt", a sentence about a prompt the client was
// looking at.
func (m *Manager) AcceptQueueEntry(workspace, entryID string) error {
	m.mu.Lock()
	owner, err := m.resolveQueueEntryLocked(workspace, entryID)
	if err != nil {
		m.mu.Unlock()
		return err
	}
	owner.entryLocked(entryID).accepted = true
	sessionID := owner.sessionID()
	var view *frontendv1.QueueView
	var recs []registry.QueuedPrompt
	if pk := owner.parked; pk != nil {
		view, recs = pk.queue.view(pk.workspace, pk.sessionID), pk.queue.records()
	} else {
		view, recs = m.publishQueueLocked(owner.live)
	}
	m.mu.Unlock()
	m.logf("session-controller: queue entry=%s accepted session=%s ws=%q ledger=%s — accept is view state, so it is honored wherever the prompt lives",
		entryID, sessionID, workspace, ledgerName(owner))
	m.publish(sessionID, view, recs)
	return nil
}

// ledgerName renders which ledger a resolved queue command landed on, for the
// log lines that state it.
func ledgerName(o queueEntryOwner) string {
	if o.parked != nil {
		return "materialized"
	}
	return "live"
}

// CancelQueueEntry drops a held prompt. It is never delivered.
//
// A materialized prompt is cancellable with NO SESSION at all: nothing has to
// run to take a prompt back, and making the user wait for a shim to come back
// would be the daemon holding a prompt hostage to the very bounce that delayed
// it (parkedledger.go).
//
// THE DURABLE ROW IS DROPPED BEFORE THE ENTRY IS REMOVED, and a drop that fails
// REFUSES the cancel (dropCancelledRow). The other order tells the user their
// prompt is gone while the ledger still says it is parked, and the daemon that
// comes back from the bounce then re-materializes and delivers it.
//
// ONE ACQUISITION SPANS RESOLVE, DROP AND COMMIT, for the same reason it does in
// ForceQueueEntry. The drain lease creates the durable row under m.mu
// (parkForDrain), so the drop must be taken under m.mu too or the two are not
// arbitrated by anything: a cancel that decided "no row" and then released could
// have AcquireShutdownHolds write one for that entry before the removal
// committed, and the next boot resurrects a prompt the user took back.
func (m *Manager) CancelQueueEntry(workspace, entryID string) error {
	return m.cancelQueueEntry(workspace, entryID, "user", "cancelled_by_user")
}

// cancelQueueEntry is CancelQueueEntry with the INITIATOR named, so a cancel the
// daemon performs on its own behalf is not recorded as one the user asked for.
//
// The merge gate is the second initiator: when a merge resolution prompt is
// parked instead of forwarded, the gate fails the merge immediately, and the
// parked prompt must go with it. Leaving it queued would hand the agent an
// instruction to repair a merge that has already been failed and rolled back,
// delivered whenever the turn in front of it finally ends.
func (m *Manager) cancelQueueEntry(workspace, entryID, initiator, tombstoneReason string) error {
	m.mu.Lock()
	owner, err := m.resolveQueueEntryLocked(workspace, entryID)
	if err != nil {
		m.mu.Unlock()
		return err
	}
	e := owner.entryLocked(entryID)
	heldBy, hadRow, sessionID := e.shutdownHoldScheduleID, e.drainRowPending, owner.sessionID()

	if err := m.dropCancelledRow(workspace, entryID, sessionID, hadRow); err != nil {
		m.mu.Unlock()
		return err
	}

	// The re-resolve is kept as an assertion rather than as a recovery: nothing
	// can move the entry between ledgers under a mutex this path never released,
	// so a miss here is a broken invariant and is reported as the loud failure it
	// is.
	sessionID, view, recs, materialized, ok := m.removeCancelledEntryLocked(workspace, entryID)
	if !ok {
		m.mu.Unlock()
		m.logf("session-controller: queue cancel found NOTHING to remove entry=%s ws=%q — the entry left both ledgers while this cancel held m.mu, which nothing is supposed to be able to do; its durable row is already gone, so nothing can re-materialize it",
			entryID, workspace)
		return fmt.Errorf("session-controller: no queued prompt %q on workspace %q", entryID, workspace)
	}
	// The removal and the tombstone are ONE step, under one acquisition: a
	// restore mid-flight is holding a row snapshot that predates this cancel,
	// and its apply loop consults exactly this set before it re-adds anything
	// (shutdownlease.go).
	m.noteRowDroppedTombstoneLocked(workspace, entryID, tombstoneReason)
	m.mu.Unlock()

	ledger := "live"
	if materialized {
		ledger = "materialized"
	}
	m.logf("session-controller: queue entry=%s cancelled session=%s ws=%q ledger=%s drain_schedule=%q initiator=%s — its durable parking row was dropped first, so nothing can bring it back",
		entryID, sessionID, workspace, ledger, heldBy, initiator)
	m.publish(sessionID, view, recs)
	return nil
}

// QueueViews returns every session's queue, sorted by workspace, for the
// connect/resync StateSnapshot. A session with an empty queue still contributes
// its (empty) view, so a reconnecting frontend is told the queue is empty rather
// than being left to assume it.
//
// IT SPANS BOTH LEDGERS, and that is not a convenience. A successor daemon
// holds prompts a previous one parked for sessions that have not wired yet
// (parkedledger.go); reading only the live fleet made every one of them
// invisible to every client until its session happened to come back, which is
// precisely the promise the drain lease exists to keep.
func (m *Manager) QueueViews() []*frontendv1.QueueView {
	m.mu.Lock()
	defer m.mu.Unlock()
	out := make([]*frontendv1.QueueView, 0, len(m.byWS)+len(m.parked))
	for _, d := range m.byWS {
		out = append(out, d.queue.view(d.workspace, d.sessionID))
	}
	out = append(out, m.parkedViewsLocked()...)
	sort.Slice(out, func(i, j int) bool { return out[i].GetWorkspace() < out[j].GetWorkspace() })
	return out
}
