package sessioncontroller

import (
	"fmt"
	"sort"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
)

// This file is the successor daemon's answer to a question the previous one
// left open: WHERE DOES A PARKED PROMPT LIVE BEFORE ITS SESSION COMES BACK?
//
// The drain lease's promise is that a prompt parked by a scheduled bounce is
// delayed, not dropped. Until this file existed that promise was kept only from
// the moment the session WIRED — restoreShutdownHolds runs on ShimReady, and
// every queue view before it was derived from m.byWS alone. So on a successor
// daemon a parked prompt was invisible to every client (absent from the connect
// snapshot and from every push) and unreachable by force and cancel, for as long
// as the session stayed down — which after a bounce that stopped the shims is
// until somebody happens to open the workspace. The user was told their prompt
// was waiting and then shown a daemon holding nothing.
//
// So the ledger is MATERIALIZED at boot instead, into a Manager-owned parked
// queue keyed by workspace, and every reader of "what is this workspace's queue"
// reads it: QueueViews for the snapshot, the pushes, and the force/cancel
// resolution. It is deliberately NOT a second derivation of a queue — the
// entries are built by newParkedEntry, the same constructor the ShimReady replay
// uses, and when the session finally wires the controller ADOPTS these very
// entries rather than rebuilding them.
//
// A materialized workspace is NOT a wired session and must never read as one:
// WiredSessions stays keyed on m.byWS, because the restored lease treats a wired
// session as affirmative resolution of its unresolved set, and a materialized
// record answering yes would let a mid-drain lease execute over a shim that is
// still reattaching mid-turn.

// parkedSession is one workspace's parked prompts with no live controller
// behind them. It carries the session id the durable row named, because that id
// is what the frontend keys a QueueView by and what a client needs to route a
// force or a cancel back.
type parkedSession struct {
	workspace string
	sessionID string
	queue     promptQueue
}

// MaterializeShutdownHolds seeds the parked-prompt ledger into queue state at
// successor boot, and reports how many prompts it materialized.
//
// Called ONCE, from the daemon's boot sequence, immediately after the drain
// lease is restored and before the frontend serves its first snapshot: the
// lease decides which rows are still HELD, and a snapshot served before this
// would be the very "daemon holding nothing" the materialization exists to
// prevent.
//
// A workspace whose session has somehow already wired is skipped and said so.
// Its controller owns its queue, and restoreShutdownHolds is the path that
// seeds it; materializing a second copy beside it would be two answers to one
// workspace's queue.
func (m *Manager) MaterializeShutdownHolds() (int, error) {
	if m.cfg.ShutdownHolds == nil {
		m.logf("session-controller: drain-held prompt materialization SKIPPED — no ShutdownHoldStore is wired, so this daemon has no parking ledger to recover; any prompt a previous daemon parked is already unrecoverable")
		return 0, nil
	}
	rows, err := m.cfg.ShutdownHolds.AllHeldPrompts()
	if err != nil {
		m.logf("session-controller: drain-held prompt materialization FAILED error=%v — prompts parked by a previous daemon's scheduled bounce stay invisible to every client until their sessions wire",
			err)
		return 0, fmt.Errorf("session-controller: materializing the drain-held prompt ledger at boot: %w", err)
	}
	if len(rows) == 0 {
		m.logf("session-controller: drain-held prompt materialization found no parked prompts — this daemon boots with an empty parking ledger")
		return 0, nil
	}
	liveSchedule, held := m.heldSchedule()

	type materialized struct {
		view *frontendv1.QueueView
		recs []registry.QueuedPrompt
		ws   string
		sess string
		ids  []string
	}
	var work []materialized
	total := 0

	m.mu.Lock()
	for _, row := range rows {
		if _, wired := m.byWS[row.Workspace]; wired {
			m.logf("session-controller: drain-held prompt materialization SKIPPED entry=%s ws=%q session=%s — a controller is already live for this workspace, so its own restore owns this row rather than the boot ledger",
				row.EntryID, row.Workspace, row.SessionID)
			continue
		}
		pk, ok := m.parked[row.Workspace]
		if !ok {
			pk = &parkedSession{workspace: row.Workspace, sessionID: row.SessionID}
			m.parked[row.Workspace] = pk
		}
		if pk.queue.get(row.EntryID) != nil {
			continue
		}
		// THE SAME CONSTRUCTOR THE REPLAY USES. A parked entry's stamp and its
		// empty rationale are decided in newParkedEntry and nowhere else, so
		// the boot ledger and the ShimReady replay cannot disagree about what a
		// parked prompt is.
		e := newParkedEntry(row.EntryID, row.RequestID, row.Text, row.PermissionMode, row.QueuedAtMs)
		e.drainRowPending = true
		if held && row.ScheduleID == liveSchedule {
			e.shutdownHoldScheduleID = row.ScheduleID
		}
		pk.queue.add(e)
		total++
	}
	for _, pk := range m.parked {
		ids := make([]string, 0, len(pk.queue.entries))
		for _, e := range pk.queue.entries {
			ids = append(ids, e.id)
		}
		work = append(work, materialized{
			view: pk.queue.view(pk.workspace, pk.sessionID),
			recs: pk.queue.records(),
			ws:   pk.workspace, sess: pk.sessionID, ids: ids,
		})
	}
	m.mu.Unlock()

	for _, w := range work {
		m.logf("session-controller: drain-held prompts MATERIALIZED ws=%q session=%s live_schedule=%q entries=%v — a previous daemon parked these and their session has not wired; they are visible in the snapshot and reachable by cancel from now, not from whenever the session comes back",
			w.ws, w.sess, liveSchedule, w.ids)
		m.publish(w.sess, w.view, w.recs)
	}
	return total, nil
}

// parkedViews renders every materialized workspace's queue. Caller holds m.mu.
func (m *Manager) parkedViewsLocked() []*frontendv1.QueueView {
	out := make([]*frontendv1.QueueView, 0, len(m.parked))
	for _, pk := range m.parked {
		out = append(out, pk.queue.view(pk.workspace, pk.sessionID))
	}
	return out
}

// adoptParkedLocked hands a wiring session the entries the boot materialization
// has been holding for its workspace, and reports how many moved.
//
// THE ENTRIES MOVE, THEY ARE NOT REBUILT. Adoption is what makes the ShimReady
// replay's existing `d.queue.get(row.EntryID)` dedupe do its job against the
// early materialization: the rows are already in the controller's queue under
// their own ids by the time the replay walks them, so the replay adds nothing
// and the prompt exists exactly once.
//
// Caller holds m.mu.
func (m *Manager) adoptParkedLocked(d *sessionController) []string {
	pk, ok := m.parked[d.workspace]
	if !ok {
		return nil
	}
	delete(m.parked, d.workspace)
	adopted := make([]string, 0, len(pk.queue.entries))
	for _, e := range pk.queue.entries {
		if d.queue.get(e.id) != nil {
			continue
		}
		d.queue.add(e)
		adopted = append(adopted, e.id)
	}
	return adopted
}

// parkedEntryOwner returns the materialized workspace holding entryID, or nil.
// Caller holds m.mu.
func (m *Manager) parkedEntryOwner(workspace, entryID string) *parkedSession {
	pk, ok := m.parked[workspace]
	if !ok {
		return nil
	}
	if pk.queue.get(entryID) == nil {
		return nil
	}
	return pk
}

// cancelParkedEntry drops a materialized prompt: the durable row goes, the
// entry goes, and the shrunken view is pushed.
//
// A cancel needs NO session. The user is discarding a prompt that has not run
// and cannot run yet, and making them wait for a shim to come back before they
// may take it back would be the daemon holding a prompt hostage to the very
// bounce that delayed it. Reports false when this workspace holds no such
// materialized entry, which is the caller's signal to look at the live fleet.
func (m *Manager) cancelParkedEntry(workspace, entryID string) bool {
	m.mu.Lock()
	pk := m.parkedEntryOwner(workspace, entryID)
	if pk == nil {
		m.mu.Unlock()
		return false
	}
	e := pk.queue.remove(entryID)
	heldBy, hadRow := e.shutdownHoldScheduleID, e.drainRowPending
	sessionID := pk.sessionID
	if len(pk.queue.entries) == 0 {
		delete(m.parked, workspace)
	}
	view, recs := pk.queue.view(workspace, sessionID), pk.queue.records()
	m.mu.Unlock()

	m.logf("session-controller: materialized queue entry=%s cancelled ws=%q session=%s drain_schedule=%q initiator=user — the prompt was parked by a previous daemon and its session has not wired; cancelling needs no shim",
		entryID, workspace, sessionID, heldBy)
	if hadRow {
		m.releaseDrainRow(entryID, "cancelled_by_user")
	}
	m.publish(sessionID, view, recs)
	return true
}

// forceParkedEntry answers a force aimed at a materialized prompt, which it can
// only ever REFUSE, loudly and with the unwired session named.
//
// A FORCE IS A DELIVERY, AND A DELIVERY NEEDS A SESSION. The control's whole
// meaning is "run this prompt now", and running it means submitting it to a
// shim this daemon does not have. The two alternatives were both worse:
//
//   - Silently doing nothing would be the exact failure this whole change
//     exists to end — a control that acknowledges and achieves nothing.
//   - Bringing the session up from here would have a queue control spawn a
//     shim, which is a power the queue surface has never had (ForceQueueEntry
//     resolves through `existing`, the deliberately no-lazy-bring-up path, for
//     the same reason interrupt and resync do), and it would do it while a
//     drain lease may still stand — starting the very turn the lease exists to
//     prevent, on a session the lease has not yet resolved.
//
// So the refusal is the answer, and it names the session so the user is told
// what to do about it rather than merely told no. Reports false when this
// workspace holds no such materialized entry.
func (m *Manager) forceParkedEntry(workspace, entryID string) (bool, error) {
	m.mu.Lock()
	pk := m.parkedEntryOwner(workspace, entryID)
	if pk == nil {
		m.mu.Unlock()
		return false, nil
	}
	sessionID := pk.sessionID
	m.mu.Unlock()

	m.logf("session-controller: force REFUSED for materialized queue entry=%s ws=%q session=%s — the prompt was parked by a previous daemon and its session has not wired to this one, so there is no shim to deliver it to; open the workspace to bring the session up, or cancel the prompt",
		entryID, workspace, sessionID)
	return true, fmt.Errorf("session-controller: cannot force queued prompt %q on workspace %q: session %s has not wired to this daemon, so there is no shim to deliver it to; open the workspace to bring the session up, or cancel the prompt", entryID, workspace, sessionID)
}

// releaseParkedHolds sheds a cancelled schedule's hold from every materialized
// entry, so a workspace whose session never wired does not keep rendering a
// lease bubble for a schedule that no longer exists. The durable rows are
// dropped by the caller, per schedule, exactly as they are for live sessions.
func (m *Manager) releaseParkedHolds(scheduleID string) {
	type freed struct {
		view *frontendv1.QueueView
		recs []registry.QueuedPrompt
		ws   string
		sess string
		ids  []string
	}
	var work []freed

	m.mu.Lock()
	for _, pk := range m.parked {
		var ids []string
		for _, e := range pk.queue.entries {
			if e.shutdownHoldScheduleID != scheduleID {
				continue
			}
			e.shutdownHoldScheduleID = ""
			e.drainRowPending = false
			ids = append(ids, e.id)
		}
		if len(ids) == 0 {
			continue
		}
		work = append(work, freed{
			view: pk.queue.view(pk.workspace, pk.sessionID),
			recs: pk.queue.records(),
			ws:   pk.workspace, sess: pk.sessionID, ids: ids,
		})
	}
	m.mu.Unlock()

	sort.Slice(work, func(i, j int) bool { return work[i].ws < work[j].ws })
	for _, w := range work {
		m.logf("session-controller: materialized drain holds RELEASED ws=%q session=%s schedule=%s entries=%v — the schedule was cancelled, so these prompts wait un-held for their session to wire",
			w.ws, w.sess, scheduleID, w.ids)
		m.publish(w.sess, w.view, w.recs)
	}
}
