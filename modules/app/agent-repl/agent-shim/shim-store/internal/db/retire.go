package db

// retire.go — THE FILE PLANE'S RE-DERIVATION RETIRES THE ROWS A CONVERSION NO
// LONGER PRODUCES.
//
// When the sidecar's conversion changes, a transcript whose rows predate it is
// re-read from its start, and for each record it names the rows that record
// alone could ever have produced and no longer does (store.v1
// StoreRetirement). Nothing else in the store ever removes a page line, and
// re-reading alone cannot: the new conversion writes different keys, so the
// stale rows would stand forever.
//
// A RETIRED ROW IS A TOMBSTONE, NOT A DELETE. Its kind becomes kindRetired and
// its write_seq is bumped; its book, position and last frame stay. So:
//
//   - every page stops serving it, because pages read kindPageLine alone;
//   - a standing watch is told once, live, and a watch opened after the fact
//     replays the retirement by write_seq exactly as it replays an upsert —
//     a delete would leave a watcher whose page was read before the
//     retirement and whose subscription came after it holding a line nobody
//     would ever withdraw;
//   - its position stays a valid pointer into its book, so a reader whose
//     high-water mark was that row is not sent to repaint;
//   - the write ledger is untouched: the writes that produced the row were
//     applied and stay absorbable, so a replay of the old conversion's bytes
//     can never resurrect it.
//
// ONLY WHAT THE STORE CAN UNDO WHOLE IS RETIRED. A prompt, a peer message, or
// an agent frame carrying a non-activity update touches no lifecycle table
// beyond ensuring its agent is known, so taking the line away leaves the agent,
// detached-work and workflow tables exactly as consistent as before. A
// retirement naming a row whose content DID drive those tables (an activity, a
// terminal, a detached announcement) is not applied, and the store says so at
// ERROR: the producer asked for something the store cannot do without leaving
// its own tables disagreeing.

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// validateRetirements refuses a retirement that names no row or no version,
// and retirements that ride no cursor advance.
//
// THE CURSOR IS REQUIRED because it is what makes a retirement safe to have
// happened: the re-read advances past a record in the same transaction that
// retires what the record no longer converts to, and a batch with no position
// could commit a retirement the producer will never know it made.
func validateRetirements(retirements []*storev1.StoreRetirement, cursor *storev1.CursorState) error {
	if len(retirements) == 0 {
		return nil
	}
	if cursor == nil {
		return invalidSitef(SiteRetirementInvalid, "retirements",
			"the batch carries %d retirement(s) and no cursor_advance — a retirement commits beside the position of the re-read that made it", len(retirements))
	}
	for i, retirement := range retirements {
		field := fmt.Sprintf("retirements[%d]", i)
		switch {
		case retirement == nil:
			return invalidSitef(SiteRetirementInvalid, field, "%s is unset", field)
		case retirement.GetUpsertKey() == "":
			return invalidSitef(SiteRetirementInvalid, field+".upsert_key", "%s.upsert_key is empty", field)
		case retirement.GetConversionVersion() == 0:
			return invalidSitef(SiteRetirementInvalid, field+".conversion_version",
				"%s (upsert_key=%q) has conversion_version 0, which names no conversion", field, retirement.GetUpsertKey())
		}
	}
	return nil
}

// applyRetirements applies one batch's retirements inside its final write
// transaction, publishing each retired line through `result`.
func (d *DB) applyRetirements(ctx context.Context, tx *sql.Tx, base logging.Fields, retirements []*storev1.StoreRetirement, nextSeq *uint64, now int64, result *WriteResult) error {
	for i, retirement := range retirements {
		fields := base
		fields.Operation = "store.db.retire"
		fields.UpsertKey = retirement.GetUpsertKey()
		if err := d.applyRetirement(ctx, tx, fields, i, retirement, nextSeq, now, result); err != nil {
			return err
		}
	}
	return nil
}

// applyRetirement retires one row, or states why it left it.
func (d *DB) applyRetirement(ctx context.Context, tx *sql.Tx, fields logging.Fields, index int, retirement *storev1.StoreRetirement,
	nextSeq *uint64, now int64, result *WriteResult) error {
	row := &storedRow{}
	switch err := tx.QueryRowContext(ctx, retireProbeSQL, retirement.GetUpsertKey()).
		Scan(&row.position, &row.writeSeq, &row.plane, &row.book, &row.kind, &row.frame); {
	case errors.Is(err, sql.ErrNoRows):
		d.log.LogVerbose(fields, "retirement names no row; nothing to retire retirements_index=%d", index)
		return nil
	case err != nil:
		return d.refuse(fields, storagef(err, "reading row %q to retire it", retirement.GetUpsertKey()))
	}
	fields.Position = encodePointer(row.position).GetValue()
	if row.book.Valid {
		fields.BookAgentID = row.book.String
	}
	if row.kind == kindRetired {
		d.log.LogVerbose(fields, "row is already retired retirements_index=%d", index)
		return nil
	}
	if row.kind != kindPageLine {
		d.log.LogVerbose(fields, "row is not a page line (kind=%s); only a page line is retired retirements_index=%d", row.kind, index)
		return nil
	}
	if row.plane != planeFile {
		d.log.LogVerbose(fields, "row was last written by the stream plane; the file plane's re-derivation leaves it retirements_index=%d", index)
		return nil
	}
	stored := &storev1.StoreEntry{}
	if err := proto.Unmarshal(row.frame, stored); err != nil {
		return d.refuse(fields, storagef(err, "the stored frame of row %q cannot be decoded to read its conversion version", retirement.GetUpsertKey()))
	}
	if stored.GetConversionVersion() >= retirement.GetConversionVersion() {
		d.log.LogVerbose(fields, "row was produced by conversion_version=%d, not below the re-read's %d; it is left retirements_index=%d",
			stored.GetConversionVersion(), retirement.GetConversionVersion(), index)
		return nil
	}
	line := stored.GetAgentUpdate().GetServeableFrame()
	if line == nil {
		return d.refuse(fields, storagef(errNotAPageLine, "row %q is indexed as a page line but its frame carries none", retirement.GetUpsertKey()))
	}
	if what, ok := retirable(line); !ok {
		failed := fields
		failed.Level = "error"
		d.log.Log(failed, "a retirement named a row whose %s drove the store's lifecycle tables, which retiring the line alone would leave disagreeing with the books; the row is left and the producer's conversion must not name it retirements_index=%d",
			what, index)
		return nil
	}

	*nextSeq++
	seq := *nextSeq
	if _, err := tx.ExecContext(ctx, retireRowSQL, kindRetired, seq, now, row.position); err != nil {
		return d.refuse(fields, storagef(err, "retiring row %q", retirement.GetUpsertKey()))
	}
	result.Retired++
	result.Lines = append(result.Lines, LineWritten{
		AgentID:  row.book.String,
		Line:     &storev1.StoreLineAt{At: encodePointer(row.position), Line: line, Turn: stored.GetTurn()},
		WriteSeq: seq,
		Retired:  true,
	})
	info := fields
	info.Level = "info"
	info.WriteSeq = seq
	d.log.Log(info, "retired a page line conversion %d produced and conversion %d no longer does; no page serves it and its watchers are told retirements_index=%d",
		stored.GetConversionVersion(), retirement.GetConversionVersion(), index)
	return nil
}

// retirable reports whether retiring a line leaves every lifecycle table as
// consistent as it found it, and names what the line is when it does not.
func retirable(line *storev1.StorePageLine) (string, bool) {
	switch item := line.GetAgentItem().GetItem().(type) {
	case *storev1.StoreAgentItem_AgentPrompt, *storev1.StoreAgentItem_PeerMessage:
		return "", true
	case *storev1.StoreAgentItem_AgentFrame:
		update, ok := item.AgentFrame.GetResult().(*conversationv1.AgentFrame_Update)
		if !ok {
			return "terminal or detached-work announcement", false
		}
		if _, activity := update.Update.GetUpdate().(*conversationv1.AgentUpdate_Activity); activity {
			return "activity", false
		}
		return "", true
	default:
		return "unrecognized item", false
	}
}

// THE RETIREMENT STATEMENTS ARE AT PACKAGE SCOPE so the suite EXPLAINs the
// production text itself.
const (
	// retireProbeSQL binds (upsert_key).
	retireProbeSQL = `SELECT position, write_seq, plane, book_agent_id, kind, frame FROM entry WHERE upsert_key = ?`
	// retireRowSQL binds (the retired kind, the new write_seq, now, position).
	retireRowSQL = `UPDATE entry SET kind = ?, write_seq = ?, last_written_at_ms = ? WHERE position = ?`
)
