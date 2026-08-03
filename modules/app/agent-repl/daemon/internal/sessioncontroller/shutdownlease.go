package sessioncontroller

import (
	"fmt"
	"sort"
	"sync"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/statedb"
)

// This file is the session controller's half of the SCHEDULED-SHUTDOWN DRAIN
// LEASE. The engine that owns the lease lives in internal/server
// (shutdownschedule.go); everything here is the two things the fleet owes it.
//
// THE FIRST IS THE HOLDS. A scheduled shutdown waits for the daemon to go
// quiet, and "quiet" is a fact only this package and the progress resolver
// know: a workspace holds the drain while a turn is in flight in it, or while
// it has live background tasks. DrainHolds reports that, per live session, off
// the SAME observed turn boundary the prompt queue already acts on rather than
// off a re-derivation of it.
//
// THE SECOND IS THE PARKING. While the lease stands, no new turn may start
// anywhere — so a submitted prompt is neither refused nor classified, it is
// PARKED on the session's ordinary prompt queue with the schedule stamped on
// it. That is a deliberate difference from the merge lease next door, which
// REFUSES: a merge is a foreground operation the user just asked for and can
// retry in a moment, while a scheduled bounce may wait on someone else's turn
// for as long as that turn runs, and refusing for the duration would make the
// daemon eat prompts on the deployer's behalf.
//
// THE LEASE NEVER REFUSES AND NEVER DROPS. A parked prompt is delayed; the
// durable row behind it (statedb.ShutdownSchedules) is what makes that promise
// survive the very bounce that caused the delay.

// ShutdownLease is the drain lease as this package needs to read it. It is
// satisfied by *server.ShutdownScheduler, which binds itself here at
// construction so a daemon cannot come up with an engine holding leases and a
// fleet that never heard of them.
type ShutdownLease interface {
	// HeldSchedule reports the live schedule's id, and whether the lease is
	// held at all. It is consulted on the submit path, so it must not block on
	// anything this package owns.
	HeldSchedule() (scheduleID string, held bool)
	// NoteDrainActivity tells the engine that a fact its holds are derived from
	// has changed (a turn started or ended, a session went away). The engine
	// re-reads DrainHolds and broadcasts; it never trusts a delta.
	//
	// MUST NOT be called while the manager mutex is held: the engine calls back
	// into DrainHolds, which takes it.
	NoteDrainActivity()
	// LeaseProvenance is the lease as a LOG LINE needs it: the schedule and the
	// human cause behind it. Separate from HeldSchedule because that one is on
	// the prompt submit path and must stay a single field read, while this is
	// only ever called where a shim is about to stop and the record of WHY
	// matters more than the cost.
	LeaseProvenance() (scheduleID, cause string, held bool)
}

// DrainHold is one workspace's reason for holding the drain open. A hold with
// neither a turn nor live tasks is not a hold and is never reported.
//
// TurnActive and TurnID are SEPARATE because they answer different questions. A
// turn this daemon adopted rather than started — a shim that outlived the
// previous daemon and reattached mid-turn — is unambiguously in flight and
// unambiguously holds the drain, but no id for it was ever seen by this
// process. Collapsing the two would make that turn read as no turn at all,
// which is the one reading that lets a bounce cut live work.
type DrainHold struct {
	Workspace  string
	SessionID  string
	TurnActive bool
	TurnID     string
	LiveTasks  int64
}

// LiveTaskCounter reports a workspace's live background-task count and whether
// the workspace is known at all. Satisfied by *progress.Manager, the authority
// that already folds the count for the progress footer — so the drain and the
// footer answer with the same number instead of two derivations of it.
type LiveTaskCounter interface {
	LiveTasks(workspace string) (int64, bool)
}

// ShutdownHoldStore is the durable ledger of prompts parked by the lease.
// Satisfied by *statedb.ShutdownSchedules. Optional: a nil store is a daemon
// with no durable parking, which is loud-logged at every parking site rather
// than silently tolerated.
type ShutdownHoldStore interface {
	RecordHeldPrompt(p statedb.HeldPrompt) error
	DropHeldPrompt(entryID string) (bool, error)
	DropHeldPromptsForSchedule(scheduleID string) (int, error)
	HeldPrompts(workspace string) ([]statedb.HeldPrompt, error)
}

// shutdownLeaseBinding late-binds the engine. The engine is constructed with
// this fleet as a dependency, so it cannot be a Config field; binding after
// construction is the same shape server.SessionCommandBinding already uses for
// the other direction of this cycle.
type shutdownLeaseBinding struct {
	mu    sync.RWMutex
	lease ShutdownLease
}

func (b *shutdownLeaseBinding) set(l ShutdownLease) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.lease = l
}

func (b *shutdownLeaseBinding) get() ShutdownLease {
	b.mu.RLock()
	defer b.mu.RUnlock()
	return b.lease
}

// BindShutdownLease binds the drain-lease engine to this fleet. Called exactly
// once, by the engine's own constructor, which is what makes "an engine exists
// but the queue never heard of it" unrepresentable rather than merely unlikely.
func (m *Manager) BindShutdownLease(l ShutdownLease) error {
	if l == nil {
		return fmt.Errorf("session-controller: BindShutdownLease needs a lease engine; a nil one would leave every prompt unparked while a shutdown drains")
	}
	m.shutdownLease.set(l)
	m.logf("session-controller: drain lease engine BOUND — submitted prompts will be parked, not classified, while a shutdown schedule holds the lease")
	return nil
}

// heldSchedule reports the live drain lease, or "" and false when none is held
// (including a daemon with no engine bound at all, which is the honest answer:
// a fleet nothing can schedule a shutdown on holds no lease).
func (m *Manager) heldSchedule() (string, bool) {
	l := m.shutdownLease.get()
	if l == nil {
		return "", false
	}
	return l.HeldSchedule()
}

// shimStopProvenance renders the drain lease for a shim-stop log line, or the
// explicit "no scheduled shutdown" statement when none is held.
//
// STATED, NEVER OMITTED. A stop line that simply lacks a schedule is
// indistinguishable from one whose logging forgot to look, and the whole point
// of these lines is that a log trace alone answers "why did this shim stop".
func (m *Manager) shimStopProvenance() string {
	l := m.shutdownLease.get()
	if l == nil {
		return "drain_lease=unwired"
	}
	scheduleID, cause, held := l.LeaseProvenance()
	if !held {
		return "drain_lease=none"
	}
	return fmt.Sprintf("drain_lease=held schedule_id=%s schedule_cause=%q", scheduleID, cause)
}

// noteDrainActivity tells the engine a hold fact moved. MUST be called with
// m.mu RELEASED.
func (m *Manager) noteDrainActivity() {
	l := m.shutdownLease.get()
	if l == nil {
		return
	}
	l.NoteDrainActivity()
}

// DrainHolds reports every live session currently holding the drain open,
// sorted by workspace so two reads of an unchanged fleet compare equal.
//
// A session with neither a turn nor live tasks contributes NOTHING: the holds
// list is the complete answer to "what is the bounce waiting on", and a session
// that is waiting on nothing is not an answer to it.
func (m *Manager) DrainHolds(tasks LiveTaskCounter) []DrainHold {
	type live struct {
		workspace, sessionID, turnID string
		turnActive                   bool
	}
	m.mu.Lock()
	sessions := make([]live, 0, len(m.byWS))
	for _, d := range m.byWS {
		s := live{workspace: d.workspace, sessionID: d.sessionID, turnActive: d.turnActive}
		if d.turnActive {
			// A turn this daemon adopted rather than started has no id this
			// process ever learned. It still HOLDS — the hold is the fact that
			// a turn is running, not that we can name it — and the wire carries
			// the empty id rather than inventing one.
			s.turnID = d.activeTurnID
		}
		sessions = append(sessions, s)
	}
	m.mu.Unlock()

	out := make([]DrainHold, 0, len(sessions))
	for _, s := range sessions {
		var liveTasks int64
		if tasks != nil {
			if n, known := tasks.LiveTasks(s.workspace); known {
				liveTasks = n
			}
		}
		if !s.turnActive && liveTasks <= 0 {
			continue
		}
		out = append(out, DrainHold{
			Workspace:  s.workspace,
			SessionID:  s.sessionID,
			TurnActive: s.turnActive,
			TurnID:     s.turnID,
			LiveTasks:  liveTasks,
		})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Workspace < out[j].Workspace })
	return out
}

// WiredSessions reports the session id of every session this fleet currently
// holds a controller for, sorted so two reads of an unchanged fleet compare
// equal.
//
// It is the AFFIRMATIVE RESOLUTION half of the restored lease's unresolved set.
// DrainHolds answers "who is holding the drain open", and a session that has
// wired and is simply idle is absent from it — which is indistinguishable, from
// the outside, from a session that has not wired at all. This says which
// sessions the fleet can actually answer for, so "not in DrainHolds" can be read
// as "genuinely holding nothing" instead of "not asked yet".
func (m *Manager) WiredSessions() []string {
	m.mu.Lock()
	out := make([]string, 0, len(m.byWS))
	for _, d := range m.byWS {
		if d.sessionID == "" {
			continue
		}
		out = append(out, d.sessionID)
	}
	m.mu.Unlock()
	sort.Strings(out)
	return out
}

// noteActiveTurnID records the turn now in flight, or clears it at its end. It
// rides the SAME correlated boundary the merge waiters bind on (onTurnEvent),
// which is the only place this package is told a turn's own id.
//
// Caller holds m.mu.
func (d *sessionController) noteActiveTurnID(started bool, turnID string) {
	if started {
		d.activeTurnID = turnID
		return
	}
	if d.activeTurnID == turnID {
		d.activeTurnID = ""
	}
}

// newParkedEntry builds the queue entry that a drain lease parks, and is the
// ONE place the parked entry's classification is decided.
//
// Both parking sites — the live submit that lands under a standing lease and
// the restore that replays a previous daemon's parking ledger — construct the
// same thing, so they construct it here. A second literal elsewhere would be a
// second answer to "what is a parked prompt", and the two would drift.
//
// The stamp is HOLD, not PENDING. PENDING's frozen meaning is "the classifier
// is running", and the classifier NEVER runs on a parked entry — there is no
// turn to interject into and nothing to decide — so PENDING would describe a
// state this code makes unreachable. HOLD's frozen meaning is "deliver it
// later, never interrupt for it", which is exactly the promise the lease makes.
// The rationale stays empty because no classifier produced one; the frontend
// renders the lease bubble off shutdownHoldScheduleID instead.
func newParkedEntry(id, requestID, text, permissionMode string, queuedAtMs int64) *queueEntry {
	return &queueEntry{
		id:             id,
		requestID:      requestID,
		text:           text,
		permissionMode: permissionMode,
		queuedAtMs:     queuedAtMs,
		classification: frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD,
		rationale:      "",
	}
}

// parkForDrain stamps a queue entry as held by the drain lease and records the
// durable row behind it. Caller holds m.mu; the durable write happens under it
// deliberately — the parked entry must not be publishable before the record
// that makes it recoverable exists.
func (m *Manager) parkForDrain(d *sessionController, e *queueEntry, scheduleID string) {
	e.shutdownHoldScheduleID = scheduleID
	if m.cfg.ShutdownHolds == nil {
		m.logf("session-controller: drain-held prompt NOT recorded durably entry=%s ws=%q session=%s schedule=%s — no ShutdownHoldStore is wired, so this prompt will be LOST if the scheduled bounce happens before it is delivered",
			e.id, d.workspace, d.sessionID, scheduleID)
		return
	}
	if err := m.cfg.ShutdownHolds.RecordHeldPrompt(statedb.HeldPrompt{
		EntryID:        e.id,
		ScheduleID:     scheduleID,
		Workspace:      d.workspace,
		SessionID:      d.sessionID,
		RequestID:      e.requestID,
		Text:           e.text,
		PermissionMode: e.permissionMode,
		QueuedAtMs:     e.queuedAtMs,
	}); err != nil {
		m.logf("session-controller: drain-held prompt durable record FAILED entry=%s ws=%q session=%s schedule=%s error=%v — the prompt is parked in memory only and will not survive the scheduled bounce",
			e.id, d.workspace, d.sessionID, scheduleID, err)
		return
	}
	e.drainRowPending = true
}

// AcquireShutdownHolds parks EVERY prompt already queued across the fleet under
// a newly taken drain lease, and reports how many it parked.
//
// The entries queued a moment before the lease was taken are in exactly the
// position the lease exists to control: their turn ends, the ordinary drain
// delivers them, and a new turn starts in the middle of a drain that was
// supposed to be finishing. Parking them is what makes "no NEW turn may start
// anywhere" true of the whole queue rather than only of what arrives next.
//
// Nothing is dropped and no flag is cleared. A parked entry keeps its
// classification, its head-jump claim, and its interject claim; the takers all
// skip it while the lease stands, so a cancel returns it to exactly the
// position it held.
func (m *Manager) AcquireShutdownHolds(scheduleID string) int {
	if scheduleID == "" {
		m.logf("session-controller: drain hold acquisition REFUSED — no schedule id was named, and an unnamed hold could never be released by a cancel")
		return 0
	}
	type parked struct {
		d       *sessionController
		view    *frontendv1.QueueView
		recs    []registry.QueuedPrompt
		entries []string
	}
	var work []parked

	m.mu.Lock()
	for _, d := range m.byWS {
		var ids []string
		for _, e := range d.queue.entries {
			if e.drainHeld() {
				continue
			}
			m.parkForDrain(d, e, scheduleID)
			ids = append(ids, e.id)
		}
		if len(ids) == 0 {
			continue
		}
		view, recs := m.publishQueueLocked(d)
		work = append(work, parked{d: d, view: view, recs: recs, entries: ids})
	}
	m.mu.Unlock()

	total := 0
	for _, w := range work {
		total += len(w.entries)
		m.logf("session-controller: drain holds ACQUIRED ws=%q session=%s schedule=%s entries=%v — these prompts were queued before the lease was taken and are parked with it, so the drain cannot be restarted by its own backlog",
			w.d.workspace, w.d.sessionID, scheduleID, w.entries)
		m.publish(w.d.sessionID, w.view, w.recs)
	}
	return total
}

// releaseDrainRow drops one entry's durable parking row, for an entry that has
// stopped being drain-held for any reason. A failure is loud-logged and not
// returned: the entry has already left the hold in memory, and a stale row is
// re-reconciled at restore (an entry whose schedule no longer exists is
// un-held on the way in).
func (m *Manager) releaseDrainRow(entryID, reason string) {
	if m.cfg.ShutdownHolds == nil {
		return
	}
	dropped, err := m.cfg.ShutdownHolds.DropHeldPrompt(entryID)
	if err != nil {
		m.logf("session-controller: drain-held prompt durable drop FAILED entry=%s reason=%s error=%v — the row is stale and will be reconciled at the next restore",
			entryID, reason, err)
		return
	}
	m.logf("session-controller: drain-held prompt durable row released entry=%s reason=%s row_existed=%v",
		entryID, reason, dropped)
}

// ReleaseShutdownHolds sheds the drain hold from every parked entry of the
// named schedule and returns those entries to ORDINARY delivery flow.
//
// This is the CANCEL path, and the ruling it implements is deliberate: a
// cancelled schedule leaves nothing behind. The parked entries were never
// classified — the classifier never runs on a lease-held entry — so they rejoin
// the queue exactly as unclassified FIFO entries, which is the same state an
// unclassifiable prompt already occupies, and the turn-end drain delivers them.
// A session with no turn running gets an immediate delivery kick, because with
// no turn there is no boundary coming to trigger one.
func (m *Manager) ReleaseShutdownHolds(scheduleID string) {
	if scheduleID == "" {
		m.logf("session-controller: drain hold release REFUSED — no schedule id was named, and releasing every hold regardless of schedule could free entries a newer schedule owns")
		return
	}
	type released struct {
		d     *sessionController
		view  *frontendv1.QueueView
		recs  []registry.QueuedPrompt
		freed []string
		kick  *queueEntry
	}
	var work []released

	m.mu.Lock()
	for _, d := range m.byWS {
		var freed []string
		for _, e := range d.queue.entries {
			if e.shutdownHoldScheduleID == scheduleID {
				e.shutdownHoldScheduleID = ""
				// The durable rows go together, below, in one statement per
				// schedule rather than one per entry.
				e.drainRowPending = false
				freed = append(freed, e.id)
			}
		}
		if len(freed) == 0 {
			continue
		}
		var kick *queueEntry
		if !d.turnActive && !d.paused {
			kick = d.queue.popFrontDeliverable()
		}
		view, recs := m.publishQueueLocked(d)
		work = append(work, released{d: d, view: view, recs: recs, freed: freed, kick: kick})
	}
	m.mu.Unlock()

	if m.cfg.ShutdownHolds != nil {
		dropped, err := m.cfg.ShutdownHolds.DropHeldPromptsForSchedule(scheduleID)
		if err != nil {
			m.logf("session-controller: drain hold durable release FAILED schedule=%s error=%v — the rows are stale and will be reconciled at the next restore", scheduleID, err)
		} else {
			m.logf("session-controller: drain hold durable release schedule=%s rows_dropped=%d", scheduleID, dropped)
		}
	}

	for _, w := range work {
		m.logf("session-controller: drain holds RELEASED ws=%q session=%s schedule=%s entries=%v — the schedule was cancelled, so these prompts rejoin ordinary delivery unclassified",
			w.d.workspace, w.d.sessionID, scheduleID, w.freed)
		m.publish(w.d.sessionID, w.view, w.recs)
		if w.kick != nil {
			m.logf("session-controller: drain hold release delivering entry=%s ws=%q session=%s — no turn is running, so no boundary is coming to drain it",
				w.kick.id, w.d.workspace, w.d.sessionID)
			go m.deliver(w.d, w.kick)
		}
	}
}

// restoreShutdownHolds seeds one bringing-up session's queue from the durable
// parking ledger.
//
// THIS IS THE OTHER HALF OF THE PROMISE. The daemon told the user their prompt
// was delayed rather than dropped, and then exited; this is the daemon that
// came back honoring it. Ordinary queue entries are deliberately NOT restored —
// the registry has always persisted them as evidence rather than as recovery,
// and a daemon that resurrected every queue would replay prompts nobody was
// waiting on. Drain-held entries are the exception because the daemon's OWN
// scheduled bounce is what delayed them.
//
// An entry whose schedule is no longer the live one comes back UN-HELD: the
// bounce it was waiting for has happened, which is exactly when it should run.
//
// Caller must hold neither m.mu nor the shim read loop.
func (m *Manager) restoreShutdownHolds(d *sessionController) {
	if m.cfg.ShutdownHolds == nil {
		return
	}
	rows, err := m.cfg.ShutdownHolds.HeldPrompts(d.workspace)
	if err != nil {
		m.logf("session-controller: drain-held prompt restore FAILED ws=%q session=%s error=%v — prompts parked by a previous daemon's scheduled bounce are not being replayed",
			d.workspace, d.sessionID, err)
		return
	}
	if len(rows) == 0 {
		return
	}
	liveSchedule, held := m.heldSchedule()

	m.mu.Lock()
	restored := make([]string, 0, len(rows))
	stillHeld := 0
	for _, row := range rows {
		if d.queue.get(row.EntryID) != nil {
			continue
		}
		e := newParkedEntry(row.EntryID, row.RequestID, row.Text, row.PermissionMode, row.QueuedAtMs)
		e.drainRowPending = true
		if held && row.ScheduleID == liveSchedule {
			e.shutdownHoldScheduleID = row.ScheduleID
			stillHeld++
		}
		d.queue.add(e)
		restored = append(restored, e.id)
	}
	var kick *queueEntry
	if !d.turnActive && !d.paused {
		kick = d.queue.popFrontDeliverable()
	}
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()

	if len(restored) == 0 {
		return
	}
	m.logf("session-controller: drain-held prompts RESTORED ws=%q session=%s restored=%d still_held=%d live_schedule=%q entries=%v — these prompts were parked by a scheduled bounce and are being honored by the daemon that came back",
		d.workspace, d.sessionID, len(restored), stillHeld, liveSchedule, restored)
	m.publish(d.sessionID, view, recs)
	if kick != nil {
		m.logf("session-controller: restored prompt delivering entry=%s ws=%q session=%s — the bounce that delayed it is over and no turn is running",
			kick.id, d.workspace, d.sessionID)
		go m.deliver(d, kick)
	}
}
