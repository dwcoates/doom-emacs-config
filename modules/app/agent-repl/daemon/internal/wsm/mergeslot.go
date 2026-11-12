package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// MergeSlot is the place a merge queue entry holds, as the roster draws it.
// A queue has THREE ACTIVE SLOTS, each held by at most one merge (owner
// rulings, 2026-11-11): the HEAD, the one merge that runs the test gate and
// lands; FIXING TESTS, the one merge fixing its failing tests out of line;
// and RESOLVING CONFLICTS, the one merge resolving its rebase's conflicts out
// of line. A merge that steps aside while its repair slot is taken AWAITS
// that slot, in order, and is granted it by the holder's release.
//
// THE SLOT IS DERIVED, NEVER CHOSEN (mergeSlotsOf), from the entry's state,
// its aside reason and its order, and stored beside the moment the entry took
// it (MergeQueueEntry.SlotSince) in the same write that moved it there. The
// store's partial unique index (mergeQueueOneHolderDDL) refuses a second
// holder of any active slot, so a derivation that ever produced one fails
// the write that recorded it, loudly, rather than standing.
type MergeSlot int

// The merge slots. The zero value is the slot of a merge only requested, in
// nobody's line. Slot 3 is retired: it was the slot of a merge racing behind
// the head, which no queue has any more, and layout 126 restamped every row.
const (
	// MergeSlotNone is a merge only requested: in no line.
	MergeSlotNone MergeSlot = 0
	// MergeSlotWaiting is a merge waiting its turn behind the head.
	MergeSlotWaiting MergeSlot = 1
	// MergeSlotHead is the front of the line, admitted or waiting to be: the
	// queue's testing slot.
	MergeSlotHead MergeSlot = 2
	// MergeSlotFixingTests is the merge fixing its failing tests out of line.
	MergeSlotFixingTests MergeSlot = 4
	// MergeSlotResolvingConflicts is the merge resolving its rebase's
	// conflicts out of line.
	MergeSlotResolvingConflicts MergeSlot = 5
	// MergeSlotAwaitingFixingTests is a merge out of line waiting for the
	// fixing-tests slot.
	MergeSlotAwaitingFixingTests MergeSlot = 6
	// MergeSlotAwaitingResolvingConflicts is a merge out of line waiting for
	// the resolving-conflicts slot.
	MergeSlotAwaitingResolvingConflicts MergeSlot = 7
)

// String names the slot in log records and errors.
func (s MergeSlot) String() string {
	switch s {
	case MergeSlotNone:
		return "none"
	case MergeSlotWaiting:
		return "waiting"
	case MergeSlotHead:
		return "head"
	case MergeSlotFixingTests:
		return "fixing_tests"
	case MergeSlotResolvingConflicts:
		return "resolving_conflicts"
	case MergeSlotAwaitingFixingTests:
		return "awaiting_fixing_tests"
	case MergeSlotAwaitingResolvingConflicts:
		return "awaiting_resolving_conflicts"
	}
	return fmt.Sprintf("slot(%d)", int(s))
}

func (s MergeSlot) valid() bool {
	switch s {
	case MergeSlotNone, MergeSlotWaiting, MergeSlotHead, MergeSlotFixingTests, MergeSlotResolvingConflicts,
		MergeSlotAwaitingFixingTests, MergeSlotAwaitingResolvingConflicts:
		return true
	}
	return false
}

// Active reports whether the slot is one of the queue's three active slots:
// the head, fixing tests, or resolving conflicts. The roster draws a slot
// clock for each.
func (s MergeSlot) Active() bool {
	return s == MergeSlotHead || s == MergeSlotFixingTests || s == MergeSlotResolvingConflicts
}

// Awaiting reports whether the slot is a merge out of line waiting for a
// repair slot another merge holds.
func (s MergeSlot) Awaiting() bool {
	return s == MergeSlotAwaitingFixingTests || s == MergeSlotAwaitingResolvingConflicts
}

// MergeAside is why a remediating entry is out of line: the repair it waits
// on, and so the repair slot it holds or awaits. It is durable on the entry
// (merge_queue.aside) because the slot derivation reads it in the store's own
// transaction. The zero value is no aside: every entry not remediating.
type MergeAside int

// The asides.
const (
	// MergeAsideNone is an entry that is not remediating.
	MergeAsideNone MergeAside = 0
	// MergeAsideTests is a merge whose whole suite failed at the head: it
	// fixes its tests out of line.
	MergeAsideTests MergeAside = 1
	// MergeAsideConflicts is a merge whose rebase conflicted at the head: its
	// session resolves the conflicts out of line.
	MergeAsideConflicts MergeAside = 2
	// MergeAsideWaitingConflicts is a merge waiting in line whose background
	// rebase met a conflict: it holds no lease and no progress record, and the
	// holder of the resolving-conflicts slot runs its resolving rebase.
	MergeAsideWaitingConflicts MergeAside = 3
)

// String names the aside in log records and errors.
func (a MergeAside) String() string {
	switch a {
	case MergeAsideNone:
		return "none"
	case MergeAsideTests:
		return "tests"
	case MergeAsideConflicts:
		return "conflicts"
	case MergeAsideWaitingConflicts:
		return "waiting_conflicts"
	}
	return fmt.Sprintf("aside(%d)", int(a))
}

func (a MergeAside) valid() bool { return a >= MergeAsideNone && a <= MergeAsideWaitingConflicts }

// slots answers the repair slot a remediating entry of this aside holds, and
// the one it awaits while another holds that.
func (a MergeAside) slots() (holds, awaits MergeSlot, err error) {
	switch a {
	case MergeAsideTests:
		return MergeSlotFixingTests, MergeSlotAwaitingFixingTests, nil
	case MergeAsideConflicts, MergeAsideWaitingConflicts:
		return MergeSlotResolvingConflicts, MergeSlotAwaitingResolvingConflicts, nil
	}
	return 0, 0, fmt.Errorf("wsm: a remediating merge queue entry carries the aside %s, which names no repair slot", a)
}

// mergeQueueSlotDDL is the layout-101 addition: the slot each merge queue
// entry holds and the moment it took it, epoch ns. Rows already queued take
// the slot their state and order give them, at the moment of the migration:
// no earlier moment was ever recorded durably. A fresh file and the 100 -> 101
// migration write the one text, as feedTextScaleDDL's reason gives.
const mergeQueueSlotDDL = `
ALTER TABLE merge_queue ADD COLUMN slot INTEGER NOT NULL DEFAULT 0;
ALTER TABLE merge_queue ADD COLUMN slot_since INTEGER NOT NULL DEFAULT 0;
` + mergeQueueSlotBackfillDDL

// mergeQueueSlotBackfillDDL gives every row the slot layout 101's derivation
// gave it (states: 0 queued, 1 admitted, 2 requested, 3 remediating; slots:
// 0 none, 1 waiting, 2 head, 3 racing, 4 remediating) and the moment the
// statement runs, epoch ns. Layout 126 restamps every row with the three-slot
// derivation (mergeQueueThreeSlotsDDL).
const mergeQueueSlotBackfillDDL = `
UPDATE merge_queue SET
  slot = CASE
    WHEN state = 2 THEN 0
    WHEN state = 3 THEN 4
    WHEN seq = (SELECT min(q.seq) FROM merge_queue AS q WHERE q.repo_key = merge_queue.repo_key AND q.state IN (0, 1)) THEN 2
    WHEN state = 1 THEN 3
    ELSE 1
  END,
  slot_since = CAST(round((julianday('now') - 2440587.5) * 86400000.0) AS INTEGER) * 1000000;
`

// mergeQueueOneHolderDDL is the store's own guarantee that each of a queue's
// three active slots (head 2, fixing tests 4, resolving conflicts 5) has at
// most one holder: a write that would record a second fails its transaction.
const mergeQueueOneHolderDDL = `
CREATE UNIQUE INDEX merge_queue_one_slot_holder ON merge_queue (repo_key, slot) WHERE slot IN (2, 4, 5);
`

// mergeQueueReturnedDDL is the layout-125 addition, kept for the migration
// chain: the mark of a merge back in line from a repair out of line. Layout
// 126 drops the column again (mergeQueueThreeSlotsDDL): a returned merge
// waits like any other, drawn by its slot. IT IS ADDITIVE: a new column the
// previous build never names.
const mergeQueueReturnedDDL = `
ALTER TABLE merge_queue ADD COLUMN returned INTEGER NOT NULL DEFAULT 0;
`

// mergeQueueAsideDDL adds the aside reason each remediating entry is out of
// line for (MergeAside).
const mergeQueueAsideDDL = `
ALTER TABLE merge_queue ADD COLUMN aside INTEGER NOT NULL DEFAULT 0;
`

// mergeQueueThreeSlotsDDL is the layout-126 step: the queue holds three
// slots and races no more (owner rulings, 2026-11-11). Every remediating
// entry takes the aside its progress record's step names (fixes 1, conflicts
// 2; a record at any other step is left at 0, which the next write refuses
// loudly, as the defect it is), every row is restamped with the three-slot
// derivation (mergeSlotsOf) -- the first remediating entry of each repair
// holds its slot and the rest await it, the first in line heads, and an
// admitted entry behind the head, once racing, waits -- the one-holder index
// is created, the returned mark's column is dropped (a merge back from a
// repair out of line waits like any other, drawn by its slot), and the racing
// switch's table is dropped.
//
// IT IS BREAKING: the build before it stamps slot 3 for a racing merge, which
// the index's absence let stand and this build cannot read, and it reads and
// writes merge_racing_disabled.
const mergeQueueThreeSlotsDDL = mergeQueueAsideDDL + `
UPDATE merge_queue SET aside = CASE (
    SELECT json_extract(p.document, '$.step') FROM merge_progress AS p WHERE p.workspace_id = merge_queue.workspace_id)
  WHEN 'fixes' THEN 1
  WHEN 'conflicts' THEN 2
  ELSE 0
END WHERE state = 3;
UPDATE merge_queue SET
  slot = CASE
    WHEN state = 2 THEN 0
    WHEN state = 3 AND aside = 1 THEN CASE
      WHEN seq = (SELECT min(q.seq) FROM merge_queue AS q WHERE q.repo_key = merge_queue.repo_key AND q.state = 3 AND q.aside = 1) THEN 4
      ELSE 6 END
    WHEN state = 3 AND aside IN (2, 3) THEN CASE
      WHEN seq = (SELECT min(q.seq) FROM merge_queue AS q WHERE q.repo_key = merge_queue.repo_key AND q.state = 3 AND q.aside IN (2, 3)) THEN 5
      ELSE 7 END
    WHEN state = 3 THEN 0
    WHEN seq = (SELECT min(q.seq) FROM merge_queue AS q WHERE q.repo_key = merge_queue.repo_key AND q.state IN (0, 1)) THEN 2
    ELSE 1
  END,
  slot_since = CAST(round((julianday('now') - 2440587.5) * 86400000.0) AS INTEGER) * 1000000;
` + mergeQueueOneHolderDDL + `
DROP TABLE merge_racing_disabled;
ALTER TABLE merge_queue DROP COLUMN returned;
`

// mergeQueueThreeSlotsFreshDDL is what a fresh file carries of layout 126:
// the aside column and the one-holder index. It has no rows to restamp and
// no racing table to drop (schemaDDL never creates it).
const mergeQueueThreeSlotsFreshDDL = mergeQueueAsideDDL + mergeQueueOneHolderDDL

// slotRow is one entry as the slot derivation reads it, in sequence order.
type slotRow struct {
	workspace WorkspaceID
	state     MergeQueueState
	aside     MergeAside
	slot      MergeSlot
}

// mergeSlotsOf answers the slot each of ROWS (one queue, in sequence order)
// holds: the first entry in line is the head and every other one in line
// waits; the first remediating entry of each repair holds that repair's slot
// and every later one awaits it, in sequence order; a requested one holds
// none. It is the ONE definition of a slot, so the store's stamp, the
// roster's line and the grant of a released repair slot can never disagree.
//
// THE GRANT IS FIFO BY CONSTRUCTION: a merge steps aside to the back of the
// sequence (StepAsideMerge) and nothing moves a remediating entry's place, so
// the earliest remediating entry of a repair is always the one that stepped
// aside first, and a release grants the slot to the next in that order.
//
// A remediating entry whose aside names no repair is a defect, answered as an
// error that fails the write it was read in.
func mergeSlotsOf(rows []slotRow) ([]MergeSlot, error) {
	out := make([]MergeSlot, len(rows))
	headed := false
	held := map[MergeSlot]bool{}
	for i, row := range rows {
		switch row.state {
		case MergeRequested:
			out[i] = MergeSlotNone
		case MergeRemediating:
			holds, awaits, err := row.aside.slots()
			if err != nil {
				return nil, fmt.Errorf("%w (workspace %s)", err, row.workspace)
			}
			if held[holds] {
				out[i] = awaits
				continue
			}
			held[holds] = true
			out[i] = holds
		case MergeQueued, MergeAdmitted:
			if headed {
				out[i] = MergeSlotWaiting
				continue
			}
			out[i] = MergeSlotHead
			headed = true
		default:
			return nil, fmt.Errorf("wsm: merge queue entry of workspace %s stands in the undeclared state %d", row.workspace, int(row.state))
		}
	}
	return out, nil
}

// SlotsOf answers the slot each of ENTRIES (one queue, in sequence order)
// holds, by the one derivation the store stamps (mergeSlotsOf): what a fake
// store stands its entries on, so it can never derive a slot differently.
func SlotsOf(entries []MergeQueueEntry) ([]MergeSlot, error) {
	rows := make([]slotRow, len(entries))
	for i, entry := range entries {
		rows[i] = slotRow{workspace: entry.Workspace, state: entry.State, aside: entry.Aside, slot: entry.Slot}
	}
	return mergeSlotsOf(rows)
}

// restampMergeSlots writes, for every entry of KEY's queue whose slot changed
// in the transaction TX, its new slot and AT as the moment it took it. An
// entry whose slot is unchanged keeps its moment, so a restart or a handover
// that leaves the queue's shape alone changes no clock. Every write that can
// change a queue's shape runs it last, in the same transaction (writeQueue).
//
// EVERY CHANGED ROW IS CLEARED BEFORE ANY IS SET: a head that stepped aside
// and the merge behind it that heads now trade the head's slot in one write,
// and setting the new head first would meet the old head's stamp in the
// one-holder index (mergeQueueOneHolderDDL) for an instant that is no defect.
// Cleared first, the index meets only the stamps the derivation answers, so
// a refusal from it is a true second holder.
func restampMergeSlots(ctx context.Context, tx *sql.Tx, key RepoKey, at time.Time) error {
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, state, aside, slot FROM merge_queue WHERE repo_key = ? ORDER BY seq`, key)
	if err != nil {
		return err
	}
	var entries []slotRow
	for rows.Next() {
		var (
			row   slotRow
			state int64
			aside int64
			slot  int64
		)
		if err := rows.Scan(&row.workspace, &state, &aside, &slot); err != nil {
			rows.Close()
			return err
		}
		row.state, row.aside, row.slot = MergeQueueState(state), MergeAside(aside), MergeSlot(slot)
		entries = append(entries, row)
	}
	if err := rows.Close(); err != nil {
		return err
	}
	if err := rows.Err(); err != nil {
		return err
	}
	slots, err := mergeSlotsOf(entries)
	if err != nil {
		return err
	}
	var changed []int
	for i, slot := range slots {
		if slot != entries[i].slot {
			changed = append(changed, i)
		}
	}
	for _, i := range changed {
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET slot = ? WHERE repo_key = ? AND workspace_id = ?`,
			int(MergeSlotNone), key, entries[i].workspace); err != nil {
			return err
		}
	}
	for _, i := range changed {
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET slot = ?, slot_since = ? WHERE repo_key = ? AND workspace_id = ?`,
			int(slots[i]), nanos(at), key, entries[i].workspace); err != nil {
			return fmt.Errorf("wsm: recording workspace %s in repo %q in the %s slot: %w", entries[i].workspace, key, slots[i], err)
		}
	}
	return nil
}

// writeQueue is the one write path for anything that can change the shape
// of KEY's queue: FN runs, then the slots it changed are stamped with the
// store's present moment in the same transaction (restampMergeSlots), so no
// such write can move a merge into a slot without the moment it took it.
// THEN, when given, read what the write left once the slots are stamped, in
// the same transaction: the slot a step aside took (StepAsideMerge).
func (s *store) writeQueue(ctx context.Context, op string, fields dlog.Context, key RepoKey, fn func(context.Context, *sql.Tx) error, then ...func(context.Context, *sql.Tx) error) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := fn(ctx, tx); err != nil {
			return err
		}
		if err := restampMergeSlots(ctx, tx, key, s.now()); err != nil {
			return err
		}
		for _, read := range then {
			if err := read(ctx, tx); err != nil {
				return err
			}
		}
		return nil
	})
}

// queueKeysOf answers the queues workspace ID has an entry on, so a write
// that removes its rows indirectly (a cascade) can restamp each.
func queueKeysOf(ctx context.Context, tx *sql.Tx, id WorkspaceID) ([]RepoKey, error) {
	rows, err := tx.QueryContext(ctx, `SELECT repo_key FROM merge_queue WHERE workspace_id = ?`, id)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var keys []RepoKey
	for rows.Next() {
		var key RepoKey
		if err := rows.Scan(&key); err != nil {
			return nil, err
		}
		keys = append(keys, key)
	}
	return keys, rows.Err()
}

// racingDisabledDDL is the layout-53 addition: the operator's daemon-wide
// switch that disabled the merge queue's racing. The queue races no more
// (owner rulings, 2026-11-11), and layout 126 drops the table again
// (mergeQueueThreeSlotsDDL); the text stays only because the migration chain
// from a file older than layout 53 still runs it.
const racingDisabledDDL = `
CREATE TABLE merge_racing_disabled (
  id          INTEGER PRIMARY KEY CHECK (id = 1),
  disabled_at INTEGER NOT NULL
);
`
