package db

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// BashRunReplay is the answer to opening a WatchBashRun: every row the run has
// so far, and the write ordinal the tail that follows must begin after.
//
// PinSeq IS TAKEN INSIDE THE REPLAY'S OWN TRANSACTION, for the same reason
// OpenedPage.PinSeq is: taken before, the watcher re-receives rows the replay
// already carried; taken after, rows written in between are lost with no way
// for the caller to know it lost them.
//
// AN EMPTY Rows IS THE UNKNOWN RUN. A run the store has never been written a
// single row for is not an empty stream, it is a run this store cannot serve —
// and that is a REFUSED OPEN at the transport, the store's convention for every
// watch. The distinction needs no sentinel because it is structural: a run
// exists exactly when it has a row.
type BashRunReplay struct {
	Rows   []BashRowWritten
	PinSeq uint64
}

// bashRunRowsSQL binds (run, the bash kind). It is at package scope so the
// suite EXPLAINs the production text itself (bashrun_test.go).
const bashRunRowsSQL = `SELECT position, write_seq, frame FROM entry
	  WHERE run_id = ? AND kind = ?
	  ORDER BY position ASC`

// BashRun reads every stored row of one detached shell run, in FIRST-INSERT
// order, plus the pin the live tail begins after.
//
// THE ORDER IS `position`, NOT `write_seq`. A run's rows are its start, its
// one rendered tail, and its terminal. What a consumer needs is the run's own
// sequence, which is the order the rows were first inserted in — the tail,
// superseded by every write, must appear where it always was, not at the end. (WatchAgentSession replays by write_seq for the
// opposite reason: a book's upsert is NEW INFORMATION about a line the caller
// has already read past.)
func (d *DB) BashRun(ctx context.Context, runID string) (BashRunReplay, error) {
	base := logging.Fields{Operation: "store.db.bash-run", Table: "entry", TaskID: runID}
	if runID == "" {
		return BashRunReplay{}, d.refuse(base, invalidFieldf("run", "run id value is empty"))
	}
	started := d.mono()

	tx, err := d.beginRead(ctx)
	if err != nil {
		return BashRunReplay{}, d.refuse(base, storagef(err, "begin read transaction"))
	}
	defer tx.Rollback() //nolint:errcheck // a read transaction commits nothing

	rows, err := tx.QueryContext(ctx, bashRunRowsSQL, runID, kindBash)
	if err != nil {
		return BashRunReplay{}, d.refuse(base, storagef(err, "reading the rows of run %q", runID))
	}
	var out []BashRowWritten
	outmoded := 0
	for rows.Next() {
		var position int64
		var seq uint64
		var frame []byte
		if err := rows.Scan(&position, &seq, &frame); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return BashRunReplay{}, d.refuse(base, storagef(err, "scanning a row of run %q", runID))
		}
		row, err := decodeBashRow(frame, position)
		if err != nil {
			rows.Close() //nolint:errcheck // the decode already failed
			return BashRunReplay{}, d.refuse(base, err)
		}
		if row.GetFrame().GetResult() == nil {
			// AN OUTMODED ROW, NOT DAMAGE. Every write is refused without a
			// result arm, so a stored row that decodes to none was written
			// under an arm this build no longer carries — the retired
			// contiguous-delta `update` (`bash:<run>:<from_offset>`). It is
			// left in the store untouched and served to nobody: a consumer
			// could only draw nothing from it.
			outmoded++
			continue
		}
		out = append(out, BashRowWritten{RunID: runID, Row: row, WriteSeq: seq})
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return BashRunReplay{}, d.refuse(base, storagef(err, "iterating the rows of run %q", runID))
	}
	if err := rows.Close(); err != nil {
		return BashRunReplay{}, d.refuse(base, storagef(err, "closing the rows of run %q", runID))
	}

	var pinSeq uint64
	if err := tx.QueryRowContext(ctx, `SELECT COALESCE(MAX(write_seq), 0) FROM entry`).Scan(&pinSeq); err != nil {
		return BashRunReplay{}, d.refuse(base, storagef(err, "reading the watch pin"))
	}
	if outmoded > 0 {
		// ONCE PER REPLAY, AT INFO: old data is accepted as outmoded, and a
		// run holding a hundred of those rows is one fact, not a hundred.
		info := base
		info.Level = "info"
		d.log.Log(info, "bash run replay skipped %d outmoded row(s) written under a retired arm; they are left in place and drawn by nothing", outmoded)
	}
	d.observeQuery(StatementBashRun, "entry", base, started, int64(len(out)))
	d.traceStatement(ctx, StatementBashRun, "entry", base, int64(len(out)))
	verbose := base
	verbose.WriteSeq = pinSeq
	d.log.LogVerbose(verbose, "bash run read rows=%d", len(out))
	return BashRunReplay{Rows: out, PinSeq: pinSeq}, nil
}

// decodeBashRow recovers a run's row from a stored frame.
//
// A ROW THAT CANNOT BE DECODED IS A LOUD FAILURE, never a skipped row: the blob
// was written by this package from a message it had already validated, so
// failing to read one back means the file is damaged — and serving the run with
// a hole in it would report the damage as a shell that produced less than it
// did.
func decodeBashRow(frame []byte, position int64) (*storev1.StoreAgentBash, error) {
	entry := &storev1.StoreEntry{}
	if err := proto.Unmarshal(frame, entry); err != nil {
		return nil, storagef(err, "stored frame at position %d cannot be decoded", position)
	}
	row := entry.GetAgentUpdate().GetBash()
	if row == nil {
		return nil, storagef(errNotABashRow, "stored frame at position %d is indexed as a bash row but carries none", position)
	}
	return row, nil
}

// BashRowIsTerminal reports whether one run row CONCLUDES the run, which is
// what gives WatchBashRun its natural end.
//
// THE ARM IS THE ANSWER. A shell run's vocabulary spells its conclusion as
// success or failure, and nothing else in AgentBash ends it — so a stream may
// stop after this row without the caller wondering whether more is coming.
func BashRowIsTerminal(row *storev1.StoreAgentBash) bool {
	switch row.GetFrame().GetResult().(type) {
	case *conversationv1.AgentBash_Success, *conversationv1.AgentBash_Failure:
		return true
	default:
		return false
	}
}
