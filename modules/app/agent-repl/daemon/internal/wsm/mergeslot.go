package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// MergeSlot is the place a merge queue entry holds, as the roster draws it:
// the head of its line, racing behind it, remediating out of line, waiting,
// or in no line at all. It is DERIVED from the entry's state and order
// (mergeSlotsOf) and stored beside the moment the entry took it
// (MergeQueueEntry.SlotSince), so a change of slot is a fact the store
// records in the same write that made it.
type MergeSlot int

// The merge slots. The zero value is the slot of a merge only requested, in
// nobody's line, and of a row an earlier build wrote.
const (
	// MergeSlotNone is a merge only requested: in no line.
	MergeSlotNone MergeSlot = iota
	// MergeSlotWaiting is a merge waiting its turn behind the head.
	MergeSlotWaiting
	// MergeSlotHead is the front of the line, admitted or waiting to be.
	MergeSlotHead
	// MergeSlotRacing is a merge being worked on behind the head.
	MergeSlotRacing
	// MergeSlotRemediating is a merge remediating out of line.
	MergeSlotRemediating
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
	case MergeSlotRacing:
		return "racing"
	case MergeSlotRemediating:
		return "remediating"
	}
	return fmt.Sprintf("slot(%d)", int(s))
}

func (s MergeSlot) valid() bool { return s >= MergeSlotNone && s <= MergeSlotRemediating }

// Active reports whether the slot is one the roster draws a slot clock for:
// the head, a racing merge, and a remediating one.
func (s MergeSlot) Active() bool {
	return s == MergeSlotHead || s == MergeSlotRacing || s == MergeSlotRemediating
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

// mergeQueueSlotBackfillDDL gives every row the slot mergeSlotsOf derives
// for it (states: 0 queued, 1 admitted, 2 requested, 3 remediating; slots as
// MergeSlot numbers them) and the moment the statement runs, epoch ns.
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

// slotRow is one entry as the slot derivation reads it, in sequence order.
type slotRow struct {
	workspace WorkspaceID
	state     MergeQueueState
	slot      MergeSlot
}

// mergeSlotsOf answers the slot each of ROWS (one queue, in sequence order)
// holds: the first entry in line is the head, an admitted entry behind it
// races, a queued one waits, a remediating one remediates, and a requested
// one holds none. It is the ONE definition of a slot, so the store's stamp
// and the roster's line can never disagree on which merge heads.
func mergeSlotsOf(rows []slotRow) []MergeSlot {
	out := make([]MergeSlot, len(rows))
	headed := false
	for i, row := range rows {
		switch row.state {
		case MergeRequested:
			out[i] = MergeSlotNone
		case MergeRemediating:
			out[i] = MergeSlotRemediating
		case MergeQueued, MergeAdmitted:
			switch {
			case !headed:
				out[i] = MergeSlotHead
				headed = true
			case row.state == MergeAdmitted:
				out[i] = MergeSlotRacing
			default:
				out[i] = MergeSlotWaiting
			}
		}
	}
	return out
}

// restampMergeSlots writes, for every entry of KEY's queue whose slot changed
// in the transaction TX, its new slot and AT as the moment it took it. An
// entry whose slot is unchanged keeps its moment, so a restart or a handover
// that leaves the queue's shape alone changes no clock. Every write that can
// change a queue's shape runs it last, in the same transaction (writeQueue).
func restampMergeSlots(ctx context.Context, tx *sql.Tx, key RepoKey, at time.Time) error {
	rows, err := tx.QueryContext(ctx, `SELECT workspace_id, state, slot FROM merge_queue WHERE repo_key = ? ORDER BY seq`, key)
	if err != nil {
		return err
	}
	var entries []slotRow
	for rows.Next() {
		var (
			row   slotRow
			state int64
			slot  int64
		)
		if err := rows.Scan(&row.workspace, &state, &slot); err != nil {
			rows.Close()
			return err
		}
		row.state, row.slot = MergeQueueState(state), MergeSlot(slot)
		entries = append(entries, row)
	}
	if err := rows.Close(); err != nil {
		return err
	}
	if err := rows.Err(); err != nil {
		return err
	}
	for i, slot := range mergeSlotsOf(entries) {
		if slot == entries[i].slot {
			continue
		}
		if _, err := tx.ExecContext(ctx, `UPDATE merge_queue SET slot = ?, slot_since = ? WHERE repo_key = ? AND workspace_id = ?`,
			int(slot), nanos(at), key, entries[i].workspace); err != nil {
			return err
		}
	}
	return nil
}

// writeQueue is the one write path for anything that can change the shape
// of KEY's queue: FN runs, then the slots it changed are stamped with the
// store's present moment in the same transaction (restampMergeSlots), so no
// such write can move a merge into a slot without the moment it took it.
func (s *store) writeQueue(ctx context.Context, op string, fields dlog.Context, key RepoKey, fn func(context.Context, *sql.Tx) error) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := fn(ctx, tx); err != nil {
			return err
		}
		return restampMergeSlots(ctx, tx, key, s.now())
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
