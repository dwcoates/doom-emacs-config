// Package ladder is THE WORKSPACE STATUS LADDER: the one precedence every
// surface's status is projected from, stated once.
//
// A workspace's status is drawn on three surfaces — the footer strip, the
// roster row (the webapp rail) and the Emacs tab bar, which paints the roster
// row's arm. The owner's ruling (2026-09-28) is that all three ALWAYS show the
// same status, as a structural invariant rather than by coincidence: a footer
// reading `merging` beside a rail reading `merge failed` beside an uncolored
// tab was the report that produced this package.
//
// So the COARSE CLAIM — which of the rungs below a workspace stands on — is
// resolved ONCE per surface by Resolve, over this one Order, and each surface
// only maps the winning claim onto its own proto arm. The surfaces may differ
// in DETAIL (the footer's substatus and activity line, the roster's finer arms
// and its viewed marker) but never in the coarse claim: FooterClaim and
// RosterClaim below project every arm each surface can emit back onto the
// claim it makes, each resolver asserts its own output against the claim it
// resolved, and the invariant test in resolve/sidebar walks the fact
// combinations through BOTH real resolvers and requires the two projections
// to agree.
//
// THE LADDER, strongest claim first:
//
//  1. merging        a merge is IN FLIGHT: enqueuing, queued or running a
//     phase. It outranks the link because it is the DAEMON's own fact,
//     knowable whatever the route to the shim is doing, and "the merge
//     pipeline owns the row while it runs" (the roster's standing ruling).
//  2. merge_conflict the merge STOPPED awaiting the user — on a conflict, or
//     parked. The run still holds its lease, so it ranks with the merge in
//     flight, above the link: what the user types goes to the merge's agent,
//     not through the session's route.
//  3. disconnected   the route to the session is not serving, or a turn the
//     daemon accepted awaits the bring-up of a route never seen
//     (AwaitingBringUp). SKIPPED WHOLE while the session is PARKED: the idle
//     sweep put the route down itself and a prompt brings it back, so a
//     parked session is idle, not broken.
//  4. closing        a close was refused. The roster observes no close
//     refusal, so only the footer ever stands here.
//  5. merge_failed   the merge failed. TERMINAL: the merge no longer holds the
//     session, so a broken route (which is live evidence about what the user
//     cannot do right now) outranks it, while it outranks everything the
//     session itself is doing.
//  6. merged         the merge landed. Terminal, ranked as merge_failed is.
//  7. blocked        the vendor or the account refuses the session.
//  8. waiting        the session waits on the user: a permission ask, and on
//     the footer also an interrupt landing, a question or a cold gate.
//  9. thinking       a turn is in flight, a context cut included.
//  10. idle          the foreground is free: a turn end (read or not),
//     detached work running, a wakeup pending, or nothing at all.
//
// Momentary footer statuses sit INSIDE the rung their fact belongs to rather
// than above it: `loading` is a turn taking on context (thinking) and the
// momentary `interrupted` is a turn end (idle).
//
// WHERE THE TWO RESOLVERS' OLD ORDERS DISAGREED, this is what was chosen:
//   - the footer ranked `disconnected` above every merge state; the roster
//     ranked every merge state above the link. A merge in flight or stopped on
//     a conflict now outranks the link (the daemon owns it and it is knowable
//     regardless), and a terminal merge ranks below it (it is over, and the
//     roster's own ruling scoped its precedence to a merge that "runs");
//   - the footer ranked `blocked` above `merging`; the roster ranked every
//     merge state above `vendor_blocked`. The roster's order stands;
//   - the footer's momentary `interrupted` and `loading` ranked above
//     `blocked` and the merge; they now sit in the idle and thinking rungs;
//   - the roster skipped the merge rungs for a parked session; parking now
//     skips only the link rung, which is all its ruling ever spoke about.
//
// FACTS ONLY ONE RESOLVER OBSERVES are the limit of the guarantee. The ladder
// makes the two surfaces agree on every fact both are fed; a fact only one of
// them is fed can still move that one alone. Today those are:
//   - the footer's alone: a refused close (closing), a client stream down (a
//     disconnected hop), a standing daemon fault (disconnected or blocked by
//     fault), an open question, a cold gate and its answer, a pending wakeup,
//     and a momentary loading;
//   - the roster's alone: a durable session record before any link state has
//     been seen, which it reads as `init` (a link rung) while the footer, with
//     no link state, reads idle.
//
// Closing one means feeding the other resolver the fact, never a second
// ladder.
package ladder

// Claim is one coarse workspace status: a rung of the ladder.
type Claim string

// The rungs, named as the arms that draw them are.
const (
	// Merging is a merge in flight.
	Merging Claim = "merging"
	// MergeConflict is a merge stopped awaiting the user.
	MergeConflict Claim = "merge_conflict"
	// Disconnected is a route to the session that is not serving.
	Disconnected Claim = "disconnected"
	// Closing is a refused close.
	Closing Claim = "closing"
	// MergeFailed is a failed merge.
	MergeFailed Claim = "merge_failed"
	// Merged is a landed merge.
	Merged Claim = "merged"
	// Blocked is the vendor or the account refusing the session.
	Blocked Claim = "blocked"
	// Waiting is the session waiting on the user.
	Waiting Claim = "waiting"
	// Thinking is a turn in flight.
	Thinking Claim = "thinking"
	// Idle is a free foreground.
	Idle Claim = "idle"
)

// Inactive is NOT a rung. It is the roster's `inactive` row — registered, with
// no open perspective and nothing live behind it — which dominates the whole
// ladder on the roster, and which no footer is ever drawn for: a workspace
// with no perspective has no strip to disagree with.
const Inactive Claim = "inactive"

// Order is the ladder, strongest claim first. It is the ONE statement of the
// precedence; the package comment explains every position.
var Order = []Claim{
	Merging, MergeConflict, Disconnected, Closing, MergeFailed, Merged,
	Blocked, Waiting, Thinking, Idle,
}

// AwaitingBringUp reports a turn the daemon ACCEPTED on a workspace whose route
// has never been seen at all: the prompt is held for a session that is still
// to be brought up. It stands on the DISCONNECTED rung — the route is coming
// up, and that is the truest claim about it — so both surfaces walk a cold
// submit monotonically (idle, then the route coming up, then the turn) instead
// of drawing the turn, then the bring-up, then the turn again.
func AwaitingBringUp(linkSeen, turnInFlight bool) bool {
	return !linkSeen && turnInFlight
}

// MergeClaim names the rung a merge state stands on, and the empty claim for
// no merge ("" and "none" both mean the orchestrator has said nothing).
//
// A state this build does not name is a merge the orchestrator REPORTS, so it
// is taken as one in flight: it holds the session until the orchestrator says
// otherwise, which is what both surfaces have always drawn for it.
func MergeClaim(state string) Claim {
	switch state {
	case "", "none":
		return ""
	case "conflict", "parked":
		return MergeConflict
	case "failed":
		return MergeFailed
	case "merged":
		return Merged
	default:
		return Merging
	}
}

// Resolve walks the ladder for one workspace and returns the winning claim
// with the surface's own drawing of it.
//
// merge is the merge orchestrator's state and parked reports a session the
// idle sweep stood down; together they decide which rungs are open at all.
// probe draws a rung the surface can claim, answering false when its facts
// make no claim there; idle draws the bottom rung, which always answers.
func Resolve[T any](merge string, parked bool, probe func(Claim) (T, bool), idle func() T) (Claim, T) {
	standing := MergeClaim(merge)
	for _, claim := range Order {
		switch claim {
		case Merging, MergeConflict, MergeFailed, Merged:
			if claim != standing {
				continue
			}
		case Disconnected:
			if parked {
				continue
			}
		case Idle:
			return Idle, idle()
		}
		if drawn, ok := probe(claim); ok {
			return claim, drawn
		}
	}
	// Order ends at Idle, which returns above, so the walk cannot fall off.
	return Idle, idle()
}
