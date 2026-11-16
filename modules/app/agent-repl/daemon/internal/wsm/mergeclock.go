package wsm

import (
	"fmt"
	"time"
)

// MergeClockPhase is the kind of stretch a merge queue entry's clock counts
// (owner rule, 2026-11-16): a merge is ACTIVE while it rebases, runs its
// tests, fixes its tests or resolves its conflicts, and WAITING otherwise.
//
// THE PHASE IS DERIVED, NEVER CHOSEN (mergeClockPhaseOf), from the entry's
// state, its slot and its restart hold, and stored with the clock it moved in
// the same write that changed them (restampMergeSlots), so a restart or a
// handover reads the clock the last write left.
type MergeClockPhase int

// The clock phases.
const (
	// MergeClockNone is a merge only requested: in no line, and counting
	// nothing.
	MergeClockNone MergeClockPhase = 0
	// MergeClockWaiting is a merge waiting: in line behind the head, at the
	// head before it is admitted, or awaiting the fixing-tests slot.
	MergeClockWaiting MergeClockPhase = 1
	// MergeClockActive is a merge at work: the admitted head (or the head a
	// restart returned to its place, MergeQueueEntry.Clock.Held), the merge
	// fixing tests, and every merge resolving conflicts.
	MergeClockActive MergeClockPhase = 2
)

// String names the phase in log records and errors.
func (p MergeClockPhase) String() string {
	switch p {
	case MergeClockNone:
		return "none"
	case MergeClockWaiting:
		return "waiting"
	case MergeClockActive:
		return "active"
	}
	return fmt.Sprintf("clock_phase(%d)", int(p))
}

func (p MergeClockPhase) valid() bool { return p >= MergeClockNone && p <= MergeClockActive }

// MergeClock is a merge queue entry's clock, durable on its row.
type MergeClock struct {
	// Phase is the kind of stretch the merge is in.
	Phase MergeClockPhase
	// Since is when the current stretch began. Zero for MergeClockNone.
	Since time.Time
	// Waited is the waiting the merge did before its current stretch: every
	// earlier waiting stretch since it was put in line, summed. An active
	// stretch never adds to it.
	Waited time.Duration
	// Held reports that the active stretch is held across a restart: the
	// admitted head a restart or a handover returned to its place before it
	// took a step (ReturnAdmittedMerge, RequeuePausedMerge) stays active
	// until it is admitted again, so a deploy never resets its clock. It lapses
	// at the first write that leaves the entry anywhere but the unadmitted
	// head.
	Held bool
}

// mergeClockPhaseOf answers the phase of an entry in STATE holding SLOT,
// HELD saying whether a restart holds its active stretch. It is the ONE
// definition of active and waiting, so the store's stamp and the roster's
// clock can never disagree.
func mergeClockPhaseOf(state MergeQueueState, slot MergeSlot, held bool) (MergeClockPhase, error) {
	switch slot {
	case MergeSlotNone:
		return MergeClockNone, nil
	case MergeSlotWaiting, MergeSlotAwaitingFixingTests:
		return MergeClockWaiting, nil
	case MergeSlotFixingTests, MergeSlotResolvingConflicts:
		return MergeClockActive, nil
	case MergeSlotHead:
		if state == MergeAdmitted || held {
			return MergeClockActive, nil
		}
		return MergeClockWaiting, nil
	}
	return 0, fmt.Errorf("wsm: no clock phase for the undeclared merge slot %s", slot)
}

// heldAt reports whether a restart's hold on the active stretch still stands
// for an entry in STATE holding SLOT: only while it is the head waiting to be
// admitted again.
func heldAt(state MergeQueueState, slot MergeSlot, held bool) bool {
	return held && slot == MergeSlotHead && state == MergeQueued
}

// nextMergeClock answers the clock PREV becomes as the entry moves into
// PHASE at AT, holding HELD:
//
//   - into no line: the clock stops, and the waiting is forgotten;
//   - from no line into any stretch: it starts at AT with nothing waited;
//   - within one kind of stretch: it runs on unchanged;
//   - from waiting to active: the waiting stretch is banked into Waited and
//     the active stretch starts at AT, from 0;
//   - from active to waiting: the waiting stretch starts at AT, and Waited
//     (the waiting before the active stretch) is given back unchanged.
func nextMergeClock(prev MergeClock, phase MergeClockPhase, held bool, at time.Time) MergeClock {
	switch {
	case phase == MergeClockNone:
		return MergeClock{Phase: MergeClockNone}
	case prev.Phase == MergeClockNone:
		return MergeClock{Phase: phase, Since: at, Held: held}
	case prev.Phase == phase:
		prev.Held = held
		return prev
	case phase == MergeClockActive:
		return MergeClock{Phase: phase, Since: at, Waited: prev.Waited + at.Sub(prev.Since), Held: held}
	default:
		return MergeClock{Phase: phase, Since: at, Waited: prev.Waited, Held: held}
	}
}

// StampClock answers the clock PREV becomes as its entry stands in STATE
// holding SLOT at AT, and whether it moved: the restart hold lapses unless
// the entry is still the unadmitted head (heldAt), the phase is derived
// (mergeClockPhaseOf), and the clock moves only when its phase or its hold
// changed (nextMergeClock). It is the ONE clock transition: the store's
// restamp applies it, and so does a fake store, so neither can count a
// stretch differently.
func StampClock(prev MergeClock, state MergeQueueState, slot MergeSlot, at time.Time) (MergeClock, bool, error) {
	held := heldAt(state, slot, prev.Held)
	phase, err := mergeClockPhaseOf(state, slot, held)
	if err != nil {
		return MergeClock{}, false, err
	}
	if phase == prev.Phase && held == prev.Held {
		return prev, false, nil
	}
	return nextMergeClock(prev, phase, held, at), true, nil
}

// mergeQueueClockDDL is the layout-180 addition: each merge queue entry's
// clock (MergeClock). Rows already queued take the clock their state and
// slot give them from what the file knows: an active row's stretch from its
// slot moment, a waiting row's from when it was put in line, with no waiting
// before it, since no earlier stretch was ever recorded. A fresh file and
// the 179 -> 180 migration write the one text.
//
// IT IS ADDITIVE: new columns with defaults the previous build never names.
const mergeQueueClockDDL = `
ALTER TABLE merge_queue ADD COLUMN clock_phase INTEGER NOT NULL DEFAULT 0;
ALTER TABLE merge_queue ADD COLUMN clock_since INTEGER NOT NULL DEFAULT 0;
ALTER TABLE merge_queue ADD COLUMN clock_waited INTEGER NOT NULL DEFAULT 0;
ALTER TABLE merge_queue ADD COLUMN clock_held INTEGER NOT NULL DEFAULT 0;
UPDATE merge_queue SET
  clock_phase = CASE
    WHEN slot IN (4, 5) THEN 2
    WHEN slot = 2 AND state = 1 THEN 2
    WHEN slot IN (1, 2, 6) THEN 1
    ELSE 0
  END,
  clock_since = CASE
    WHEN slot IN (4, 5) THEN slot_since
    WHEN slot = 2 AND state = 1 THEN slot_since
    WHEN slot IN (1, 2, 6) THEN enqueued_at
    ELSE 0
  END;
`
