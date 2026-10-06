package db

// hooksweep.go — THE HOOK RECORDS STORED BEFORE THE STORE STOPPED KEEPING THEM
// ARE DROPPED BY THE STORE ITSELF, never by a hand-run statement.
//
// Owner ruling 2026-10-06: "we should stop storing hook records, they are just
// bloat." A write that carries a hook line keeps only its identity from then
// on (kindHookDropped). The rows written before the rule are page lines, and a
// SessionStart:resume firing per resume filled history pages with rows that
// drew nothing (the feed's `history_loaded drew_rows=false`). This sweep turns
// each into the row the rule would have written: kind hook_dropped, frame
// reduced to its stamps.
//
// NO WRITE_SEQ IS BUMPED, so no standing watch is told and no replay reads
// the row: every hook line stored before the rule was a succeeded firing or a
// start, which draws nothing, so nothing drawn has to be withdrawn. The
// position stays a valid pointer (pointerInBookSQL), so a reader whose mark
// was one of these rows walks on from it.
//
// ONLY STREAM-PLANE ACTIVITY ROWS ARE READ. The stream plane is the one that
// ever wrote a hook line (the file plane's hook attachments are residue, which
// the sidecar never persists), and every hook line is keyed `activity:<id>`.
// OPTIMIZATION: that predicate keeps the sweep from decoding the file plane's
// tens of thousands of activity frames (80 MB on the owner's store, 2026-10-06)
// at every boot; the stream plane held 776 activity rows.
//
// IT RUNS ONCE PER STORE BOOT. Once the rows are dropped the sweep finds
// nothing, and nothing writes a hook page line again, because classify gives
// every hook line its own kind whichever producer build wrote it.

import (
	"context"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// hookSweepBatch is how many candidate rows one sweep batch reads and judges.
//
// THE SWEEP SHARES THE WRITE SLOT, so a batch is bounded the way the ledger
// sweep's is: a producer's write waits at most one batch.
const hookSweepBatch = 500

// hookSweepCandidatesSQL binds (the position after which to read, the
// page-line kind, the stream plane, the batch size). The position range is the
// primary key's, so a batch is a seek and never a rescan of what was read. At package scope so the
// suite EXPLAINs the production text.
const hookSweepCandidatesSQL = `SELECT position, frame FROM entry
  WHERE position > ? AND kind = ? AND plane = ? AND upsert_key LIKE 'activity:%'
  ORDER BY position LIMIT ?`

// hookSweepDropSQL binds (the hook kind, the stamps frame, the position, the
// page-line kind). The kind guard keeps a row a producer superseded between
// the read and this write from being judged on content it no longer holds.
const hookSweepDropSQL = `UPDATE entry SET kind = ?, frame = ? WHERE position = ? AND kind = ?`

// HookSweepResult reports what one sweep dropped.
type HookSweepResult struct {
	// Dropped is the hook page lines turned into hook_dropped rows.
	Dropped int64
	// Scanned is the stream-plane activity rows the sweep judged.
	Scanned int64
	// Batches is the write transactions it took.
	Batches int
}

// SweepHookLines drops every hook record stored as a page line, in bounded
// batches, and reports what it dropped. A sweep cut short by shutdown keeps
// what it committed and returns the caller's cancellation.
func (d *DB) SweepHookLines(ctx context.Context) (HookSweepResult, error) {
	var result HookSweepResult
	base := logging.Fields{Operation: "store.db.hook-sweep", Table: "entry"}
	started := d.mono()
	after := int64(0)
	for {
		if err := ctx.Err(); err != nil {
			return result, d.refuse(base, err)
		}
		hooks, scanned, last, err := d.readHookCandidates(ctx, base, after)
		if err != nil {
			return result, err
		}
		result.Scanned += scanned
		if len(hooks) > 0 {
			dropped, err := d.dropHookLines(ctx, base, hooks)
			if err != nil {
				return result, err
			}
			result.Dropped += dropped
			result.Batches++
		}
		if scanned < hookSweepBatch {
			break
		}
		after = last
	}
	elapsed := d.mono().Sub(started)
	if result.Dropped == 0 {
		d.log.LogVerbose(base, "hook sweep found no stored hook record scanned=%d duration_ms=%d",
			result.Scanned, elapsed.Milliseconds())
		return result, nil
	}
	// A SWEEP THAT DROPPED SOMETHING IS A STATE CHANGE, so it is a normal-level
	// record; one that dropped nothing is the steady state and is verbose.
	d.log.Log(base, "hook sweep dropped %d stored hook records; their rows keep only their identity scanned=%d batches=%d duration_ms=%d",
		result.Dropped, result.Scanned, result.Batches, elapsed.Milliseconds())
	return result, nil
}

// hookRow is one stored hook page line the sweep will drop: its position and
// the stamps frame it will keep.
type hookRow struct {
	position int64
	stamps   []byte
}

// readHookCandidates reads one batch of stream-plane activity page lines after
// `after` and answers the hook lines among them, how many rows it judged, and
// the last position it read.
func (d *DB) readHookCandidates(ctx context.Context, base logging.Fields, after int64) ([]hookRow, int64, int64, error) {
	tx, err := d.beginRead(ctx)
	if err != nil {
		return nil, 0, 0, d.refuse(base, storagef(err, "begin hook sweep read"))
	}
	defer d.endTx(tx, base)
	rows, err := tx.QueryContext(ctx, hookSweepCandidatesSQL, after, kindPageLine, planeStream, hookSweepBatch)
	if err != nil {
		return nil, 0, 0, d.refuse(base, storagef(err, "reading the hook sweep's candidates"))
	}
	defer rows.Close() //nolint:errcheck // read-only; rows.Err is checked below
	var (
		hooks   []hookRow
		scanned int64
		last    = after
	)
	for rows.Next() {
		var position int64
		var frame []byte
		if err := rows.Scan(&position, &frame); err != nil {
			return nil, 0, 0, d.refuse(base, storagef(err, "scanning a hook sweep candidate"))
		}
		scanned++
		last = position
		entry := &storev1.StoreEntry{}
		if err := proto.Unmarshal(frame, entry); err != nil {
			return nil, 0, 0, d.refuse(base, storagef(err, "the stored frame at position %d cannot be decoded to judge whether it is a hook", position))
		}
		if !isHookFrame(entry.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame()) {
			continue
		}
		stamps, err := hookStampsFrame(entry)
		if err != nil {
			return nil, 0, 0, d.refuse(base, storagef(err, "serializing the stamps of the hook row at position %d", position))
		}
		hooks = append(hooks, hookRow{position: position, stamps: stamps})
	}
	if err := rows.Err(); err != nil {
		return nil, 0, 0, d.refuse(base, storagef(err, "reading the hook sweep's candidates"))
	}
	return hooks, scanned, last, nil
}

// dropHookLines turns one batch of hook page lines into hook_dropped rows, in
// one bulk transaction through the shared write slot.
func (d *DB) dropHookLines(ctx context.Context, base logging.Fields, hooks []hookRow) (int64, error) {
	tx, release, err := d.beginWrite(ctx, WriteBulk)
	if err != nil {
		if isContextError(err) {
			return 0, d.refuse(base, err)
		}
		return 0, d.refuse(base, storagef(err, "begin hook sweep transaction"))
	}
	defer release()
	defer d.endTx(tx, base)
	var dropped int64
	for _, hook := range hooks {
		res, err := tx.ExecContext(ctx, hookSweepDropSQL, kindHookDropped, hook.stamps, hook.position, kindPageLine)
		if err != nil {
			return 0, d.refuse(base, storagef(err, "dropping the hook row at position %d", hook.position))
		}
		n, err := res.RowsAffected()
		if err != nil {
			return 0, d.refuse(base, storagef(err, "counting the dropped hook row at position %d", hook.position))
		}
		dropped += n
	}
	if err := tx.Commit(); err != nil {
		return 0, d.refuse(base, storagef(err, "committing the hook sweep transaction"))
	}
	verbose := base
	verbose.Position = encodePointer(hooks[len(hooks)-1].position).GetValue()
	d.log.LogVerbose(verbose, "hook sweep transaction committed dropped=%d", dropped)
	return dropped, nil
}
