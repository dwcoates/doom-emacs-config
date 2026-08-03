package sessioncontroller

import "fmt"

// This file holds the session controller's ONE answer to "is a turn in flight,
// and which turn is it".
//
// THE DEFECT IT CLOSES. The two halves of that answer used to be two fields
// written at two different edges: `turnActive` latched when the daemon COMMITTED
// to submitting a prompt, and `activeTurnID` bound only when the durable
// TurnStarted was later consumed. Every schedule in between produced a torn pair
// — a drain hold that said "a turn is running" and could not say which — and the
// drain lease broadcasts exactly that pair. Two more writers (the vendor-session
// rotation and the handshake reconcile) moved one half without touching the
// other, so a fresh latch could stand over a retired turn's id.
//
// A TAGGED RECORD MAKES THE TORN PAIR UNREPRESENTABLE. There is one field, it is
// written only by the validating transitions below, and each phase carries
// exactly the provenance that phase can honestly claim:
//
//   - idle      — no turn. Carries nothing.
//   - accepted  — this daemon committed to submitting requestID and no
//     TurnStarted has been observed for it yet. It HOLDS the drain (the prompt is
//     on its way to a shim), and its id is resolved from the durable turn ledger
//     at derivation time (DrainHolds), never guessed here.
//   - named     — the durable turn ledger has accepted a start for turnID, which
//     is always non-empty. This is the only phase that can name a turn out of
//     process memory.
//   - adopted   — a turn is in flight that this process never saw begin (a shim
//     that outlived the previous daemon and reattached mid-turn, or a start the
//     stream carried with no id at all). It holds the drain and deliberately
//     names nothing: the hold is the fact that a turn runs, not that we can name
//     it. Provenance-keyed, never string-keyed — "no id" is a phase, not an empty
//     string somebody has to remember to check.

// turnPhase names WHAT this daemon knows about the turn now in flight.
type turnPhase uint8

const (
	turnPhaseIdle turnPhase = iota
	turnPhaseAccepted
	turnPhaseNamed
	turnPhaseAdopted
)

func (p turnPhase) String() string {
	switch p {
	case turnPhaseIdle:
		return "idle"
	case turnPhaseAccepted:
		return "accepted"
	case turnPhaseNamed:
		return "named"
	case turnPhaseAdopted:
		return "adopted"
	default:
		return fmt.Sprintf("turnPhase(%d)", uint8(p))
	}
}

// turnRecord is the session controller's turn-in-flight state. Guarded by the
// manager mutex. The zero value is idle, which is what a controller that has
// never seen a turn honestly holds.
//
// requestID is set ONLY in the accepted phase and turnID ONLY in the named
// phase: a field that could outlive its phase is exactly the stale-id half of
// the pair this type exists to end.
type turnRecord struct {
	phase     turnPhase
	requestID string
	turnID    string
}

// active reports whether a turn is in flight at all — the fact the prompt queue,
// the interrupt gate and the drain hold all act on.
func (t turnRecord) active() bool { return t.phase != turnPhaseIdle }

// name reports the turn's id and whether this process can name it. An adopted or
// accepted turn answers ("", false): both are unambiguously in flight and
// neither can be named from process memory.
func (t turnRecord) name() (string, bool) {
	if t.phase == turnPhaseNamed {
		return t.turnID, true
	}
	return "", false
}

func (t turnRecord) String() string {
	switch t.phase {
	case turnPhaseAccepted:
		return fmt.Sprintf("accepted(request_id=%q)", t.requestID)
	case turnPhaseNamed:
		return fmt.Sprintf("named(turn_id=%q)", t.turnID)
	default:
		return t.phase.String()
	}
}

// firstNamedClaim picks the id a claim set can be named by, and reports whether
// one exists. A LEGACY start carries no turn id at all (turnlifecycle.go's
// accept_legacy_stream_start), so a claim set can be non-empty and still name
// nothing; that is the adopted phase's business, not an empty turnID.
func firstNamedClaim(ids []string) (string, bool) {
	for _, id := range ids {
		if id != "" {
			return id, true
		}
	}
	return "", false
}

// --- the transitions. Every one of them is the ONLY way its edge may write the
// record, and every one of them holds the manager mutex through the write. ---

// noteTurnAcceptedLocked moves the record on the ACCEPT edge — the daemon has
// committed to submitting requestID — and returns the record as it was found, so
// a failed submit can restore exactly what it displaced (noteTurnRestoreLocked).
//
// An ALREADY ACTIVE record is left alone rather than overwritten. A named turn
// keeps its name: the accept edge learns nothing about a turn already in flight,
// and downgrading `named` to `accepted` would throw away the one provenance this
// process has.
//
// Caller holds m.mu.
func (d *sessionController) noteTurnAcceptedLocked(requestID string) (before turnRecord) {
	before = d.turn
	if before.active() {
		return before
	}
	d.turn = turnRecord{phase: turnPhaseAccepted, requestID: requestID}
	return before
}

// noteTurnRestoreLocked puts the record back where the accept edge found it,
// for a submit the shim refused. It is the accept edge's exact inverse and the
// only unvalidated write in the file, which is why it takes a record the accept
// edge itself produced rather than arbitrary phase/id parts.
//
// Caller holds m.mu.
func (d *sessionController) noteTurnRestoreLocked(before turnRecord) { d.turn = before }

// turnClaimProjection is what a durable claim set says the record should be, and
// what actually happened when it was applied.
type turnClaimProjection struct {
	before   turnRecord
	after    turnRecord
	changed  bool
	unnamed  bool // the claim set is non-empty and names nothing (legacy start)
	released bool // the claim set is empty: this projection RELEASES the hold
}

// noteTurnClaimsLocked projects the SSM's durable active-turn claim set onto the
// record. The ledger is the authority on which turns are in flight, so the
// record is derived from it rather than accumulated beside it — which is what
// makes "active without provenance" unreachable instead of merely unlikely.
//
// An UNNAMEABLE claim set (a legacy start, which carries no turn id) resolves to
// adopted, never to named with an empty string. The caller reports it loudly:
// the turn is real and holds the drain, and the record says so, but nothing
// pretends to have an id for it.
//
// Caller holds m.mu.
func (d *sessionController) noteTurnClaimsLocked(activeIDs []string) turnClaimProjection {
	p := turnClaimProjection{before: d.turn}
	switch id, named := firstNamedClaim(activeIDs); {
	case len(activeIDs) == 0:
		p.after = turnRecord{}
		p.released = true
	case named:
		p.after = turnRecord{phase: turnPhaseNamed, turnID: id}
	default:
		p.after = turnRecord{phase: turnPhaseAdopted}
		p.unnamed = true
	}
	p.changed = p.after != p.before
	d.turn = p.after
	return p
}

// noteTurnAdoptedLocked moves the record on the HANDSHAKE reconcile, where the
// shim reports whether a turn is in flight but this process still has no
// standing of its own to name one.
//
// An already-active record is left alone: a named turn that reconnects is still
// that named turn, and replacing it with `adopted` would lose the id for no
// reason. active=false is a genuine release and clears the record whole.
//
// Caller holds m.mu.
func (d *sessionController) noteTurnAdoptedLocked(active bool) (before turnRecord, changed bool) {
	before = d.turn
	switch {
	case !active:
		d.turn = turnRecord{}
	case before.active():
		return before, false
	default:
		d.turn = turnRecord{phase: turnPhaseAdopted}
	}
	return before, d.turn != before
}

// noteTurnIdleLocked releases the record whole, in ONE assignment: the phase and
// every id it carried go together, so no writer can leave a stale id standing
// under a later latch. It is the turn end, the ALREADY_COMPLETE reconciliation
// and the vendor-session rotation alike — all three are "this daemon no longer
// observes a turn", and they differ only in the cause the caller logs.
//
// Caller holds m.mu.
func (d *sessionController) noteTurnIdleLocked() (before turnRecord, changed bool) {
	before = d.turn
	d.turn = turnRecord{}
	return before, before.active()
}
