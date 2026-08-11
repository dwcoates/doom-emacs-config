package sessioncontroller

// engagementturn.go — WHICH TURNS COUNT AS SOMEBODY USING THE WORKSPACE.
//
// The idle cutoff hibernates a session nobody has touched for six hours, and
// "touched" has to mean something the daemon can measure. It used to mean the
// last turn END of any kind, which included the daemon's own cache keep-alive
// ping — so a session pinged every fifty-five minutes had its idle clock reset
// every fifty-five minutes, never reached six hours, and NEVER HIBERNATED AT
// ALL. That is the exact inverse of the defect that made a cold cache sleep at
// one hour, and both come from one field answering two questions.
//
// There are now two durable instants (registry.Record.LastEngagementMs):
//
//   - the CACHE clock moves on EVERY turn end, ping included, because a ping
//     refreshes the prompt cache and the next one is due a cache lifetime later;
//   - the ENGAGEMENT clock moves only for a turn somebody asked for, and it is
//     the only thing the idle cutoff measures.
//
// # The fact is DECLARED, never recognized
//
// A turn's kind is recorded at the SUBMIT — `forwardPrompt`, the one funnel
// every prompt path reaches, where the submitter is known exactly — and read
// back by turn id at that same turn's end. Nothing anywhere reconstructs it from
// prompt text, turn duration, cost, or timing. An invariant that depended on
// recognizing machinery after the fact would break the first time the machinery
// changed shape, and the shape here is one string of prompt text.
//
// # One machine turn at a time, and that is enforced elsewhere
//
// A ping declines while a compaction is in flight and vice versa
// (`keep_alive_in_flight`, `compaction_in_flight`), and both decline while any
// turn is active. So a single field suffices, exactly as the keep-alive claim
// itself is a single field. A second machine turn arriving on top of a standing
// one is that exclusivity having failed, and it says so rather than being
// absorbed into a set that would hide it.
//
// # THE MARK HAS ITS OWN MUTEX, AND THAT IS NOT AN OPTIMIZATION
//
// The reader is the turn-end hook, which runs on the SHIM READ-LOOP goroutine
// inside the consumer's event dispatch. That path is deliberately free of the
// manager mutex — the mutex every submit, teardown and sweep holds — and taking
// it there serializes every turn boundary behind whatever the fleet is doing.
// The first version of this file did exactly that, and the e2e suite found the
// cost: a vendor terminal result arrived late enough that the turn it settled
// had already been closed by an interrupt, the shim's own TurnEnded then named
// an unpinned accounting turn, and the replay-cursor invariant killed the
// session's shim link. A dedicated lock, taken for one field assignment and
// never held across a call, has no such reach.
//
// # A mark for a turn that never ran is harmless
//
// The mark is taken BEFORE the submit, so no boundary can arrive before it. A
// submit that then fails retracts it beside the drive record; a retraction
// missed anyway leaves an id nothing will ever match, because turn ids are
// unique.

// noteMachineTurn records that turnID belongs to the daemon's own machinery and
// therefore must NOT move the engagement clock when it ends.
func (m *Manager) noteMachineTurn(d *sessionController, turnID string, who submitter) {
	if d == nil || turnID == "" {
		return
	}
	d.machineTurnMu.Lock()
	previous := d.machineTurnID
	d.machineTurnID = turnID
	d.machineTurnMu.Unlock()
	if previous != "" && previous != turnID {
		// NOT ABSORBED. Two machine turns in flight at once means the claims
		// that make them mutually exclusive have a hole, and the visible symptom
		// would otherwise be nothing at all: the newer mark simply replaces the
		// older, and the older turn's end then moves the engagement clock as
		// though a person had done it — quietly making a workspace nobody
		// touched look busy.
		m.logf("session-controller: INVARIANT VIOLATION — machine turn %s (%s) was submitted for ws=%q session=%s while machine turn %s was still unresolved; the keep-alive and warm-compaction claims are supposed to make that impossible, and the older turn's end will now move the engagement clock as if somebody had asked for it",
			turnID, who, d.workspace, d.sessionID, previous)
	}
}

// forgetMachineTurn drops the mark for a submit that FAILED, so a turn the shim
// never took cannot be waiting to answer for an id some later path reuses.
func (m *Manager) forgetMachineTurn(d *sessionController, turnID string) {
	if d == nil || turnID == "" {
		return
	}
	d.machineTurnMu.Lock()
	if d.machineTurnID == turnID {
		d.machineTurnID = ""
	}
	d.machineTurnMu.Unlock()
}

// engagementTurn reports whether the turn that just ended was somebody using the
// workspace, and consumes the mark if it was not.
//
// AN UNKNOWN TURN ANSWERS YES, and the asymmetry is deliberate. Only the two
// machine submitters ever leave a mark, so an unmarked turn is a real one by
// construction — and if that construction ever failed, the two ways to be wrong
// are not symmetric: counting a ping as engagement DELAYS a teardown by one
// cache lifetime, while counting a real turn as machinery would reap a workspace
// somebody was working in. An untraceable turn end (empty id) answers yes for
// the same reason.
func (m *Manager) engagementTurn(d *sessionController, turnID string) bool {
	if d == nil {
		return true
	}
	d.machineTurnMu.Lock()
	defer d.machineTurnMu.Unlock()
	if turnID == "" || d.machineTurnID != turnID {
		return true
	}
	d.machineTurnID = ""
	return false
}
