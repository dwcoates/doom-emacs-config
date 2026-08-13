package db

import (
	"database/sql"
	"errors"
	"fmt"
	"time"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// Plane column values. The observing plane is a shim-side fact and the store is
// entitled to it, so it is extracted for attribution in the store's own logs —
// never returned to a reader, which is why it has no place in any query result.
const (
	planeUnset  = 0
	planeStream = 1
	planeFile   = 2
)

// Result reports the outcome of one Ingest call.
//
// NOTHING CARRIES IT BACK TO THE PRODUCER. `StoreWriteAck` was retired with the
// `Event` layer and `StoreEntryWrite` has no reply message at all, so these
// counters reach the store's log and stop there. A producer therefore cannot
// currently learn that its write landed — which is what write_id's
// replay-idempotency contract was written to depend on. Recorded as a gap; no
// replacement message is invented here.
type Result struct {
	// Accepted is the number of records persisted with a freshly assigned seq.
	Accepted uint64
	// Replayed is the number of records the producer re-delivered under a
	// write_id this store had already written. It is what a batch whose
	// (now nonexistent) ack was lost to a store restart looks like from here:
	// the unique (session_id, write_id) index rejects the repeat, it consumes
	// no seq, and it is neither written again nor fanned out again.
	//
	// It no longer has a sibling `Deduped`. Cross-plane dedup is gone with
	// `dedup_key`: the shim stopped writing conversation content, so the twins
	// the key existed to collapse no longer occur, and a duplicate arriving now
	// can only be one producer re-delivering one record.
	Replayed uint64
	// Unconverted is the number of records stored with no external half. They
	// are durable and unreachable by every read the store serves.
	Unconverted uint64
	// Deduplicated-away records consume no seq, so LastSeq is the highest seq
	// assigned in this batch (0 if none).
	LastSeq uint64
	// Deliveries are the accepted records, in arrival order, already wrapped in
	// the envelope a subscriber receives.
	//
	// RETURNED RATHER THAN STAMPED IN PLACE. The old Event carried its own seq
	// field, so ingest wrote the assigned position back onto the caller's
	// message and the caller re-read it to decide what to fan out. `Entry` has
	// no seq field — position is the store's addressing and lives on the
	// delivery envelope — so the fan-out set is stated here instead of being
	// recovered from a mutation.
	Deliveries []*protocolv1.EntryDelivery
}

// Ingest persists a batch of records and, atomically in the SAME transaction,
// advances the file cursor when one is supplied.
//
// Per-session seq is assigned gapless in arrival order over the records that
// HAVE a session: a replayed write consumes no seq, and a record with no
// external half is not positioned at all.
//
// IDEMPOTENT BY IDENTITY. A record carrying a write_id can be delivered any
// number of times and lands exactly once: the unique (session_id, write_id)
// index rejects every repeat, the repeat consumes no seq, and it is reported as
// Replayed rather than written again. A record with an empty write_id is not
// replay-idempotent, and the store enforces uniqueness only over non-empty
// values — exactly as the field's contract states.
func (d *DB) Ingest(producer string, batch *agentshimv1.EntryBatch) (res Result, resultErr error) {
	entries := batch.GetEntries()
	cursor := batch.GetCursorAdvance()
	d.log.LogVerbose(logging.Fields{
		Operation: "ingest", Producer: producer, Table: "entry", Transaction: "BEGIN IMMEDIATE",
	}, "starting transaction entries=%d cursor_advance=%t", len(entries), cursor != nil)
	// The whole transaction is one timed unit: it is a read-then-write under
	// BEGIN IMMEDIATE, so what an operator needs to see is the interval the
	// writer lock was held, not one statement inside it. Rows are the records
	// the batch actually resolved.
	started := time.Now()
	defer func() {
		d.observeQuery(StatementIngest, "entry", "", started, int64(res.Accepted+res.Replayed+res.Unconverted))
	}()
	defer func() {
		if resultErr != nil {
			d.log.Log(logging.Fields{
				Operation: "ingest", Producer: producer, Table: "entry",
				Transaction: "BEGIN IMMEDIATE", Level: "error",
			}, "transaction rejected: %v", resultErr)
		}
	}()

	tx, err := d.sql.Begin()
	if err != nil {
		return res, fmt.Errorf("shim-store ingest: begin tx (producer=%q): %w", producer, err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit

	// Lazily loaded per-session high-water seq, so multi-record single-session
	// batches read MAX(seq) exactly once.
	sessionSeq := make(map[string]uint64)
	loadSeq := func(sid string) (uint64, error) {
		if v, ok := sessionSeq[sid]; ok {
			return v, nil
		}
		var v uint64
		row := tx.QueryRow(`SELECT COALESCE(MAX(seq), 0) FROM entry WHERE session_id = ?`, sid)
		if err := row.Scan(&v); err != nil {
			return 0, fmt.Errorf("shim-store ingest: reading max seq for session %q: %w", sid, err)
		}
		sessionSeq[sid] = v
		return v, nil
	}

	// OR IGNORE covers the one remaining unique index, entry_write_id
	// (session_id, write_id), which makes a REPLAYED write a no-op. There is no
	// second index to disambiguate against any more: event_dedup went with
	// dedup_key, so a rejection here has exactly one cause.
	const insertSQL = `INSERT OR IGNORE INTO entry
	  (session_id, seq, plane, kind, write_id, top_level_message_id, produced_at, payload)
	  VALUES (?, ?, ?, ?, ?, ?, ?, ?)`
	const insertUnconvertedSQL = `INSERT OR IGNORE INTO unconverted
	  (producer, write_id, kind, stored_at, payload) VALUES (?, ?, ?, ?, ?)`
	const writeIDSeqSQL = `SELECT seq FROM entry WHERE session_id = ? AND write_id = ?`

	for i, entry := range entries {
		plane, err := planeOf(entry)
		if err != nil {
			return res, fmt.Errorf("shim-store ingest: %w (producer=%q index=%d)", err, producer, i)
		}
		kind := kindOf(entry)
		blob, err := proto.Marshal(entry)
		if err != nil {
			return res, fmt.Errorf("shim-store ingest: marshaling entry (producer=%q index=%d kind=%q): %w", producer, i, kind, err)
		}
		writeID := entry.GetInternal().GetWriteId()

		external := entry.GetExternal()
		if external == nil {
			// UNRENDERABLE. Nothing to hand the daemon, no session to be
			// positioned in, so it is stored whole and unpositioned.
			if entry.GetInternal().GetUnconverted() == nil {
				return res, fmt.Errorf("shim-store ingest: entry has neither an external half nor an unconverted arm — it says nothing and can never be read back (producer=%q index=%d)", producer, i)
			}
			out, err := tx.Exec(insertUnconvertedSQL, producer, nullStr(writeID), kind, nowMillis(), blob)
			if err != nil {
				return res, fmt.Errorf("shim-store ingest: inserting unconverted record (producer=%q index=%d kind=%q): %w", producer, i, kind, err)
			}
			n, err := out.RowsAffected()
			if err != nil {
				return res, fmt.Errorf("shim-store ingest: rows-affected for unconverted record (producer=%q index=%d): %w", producer, i, err)
			}
			if n == 1 {
				res.Unconverted++
				d.log.Log(logging.Fields{
					Operation: "ingest-unconverted", Producer: producer, Table: "unconverted",
				}, "stored a record the producer could not convert kind=%q write_id=%q — it is durable and has no path to any reader", kind, writeID)
			} else {
				res.Replayed++
			}
			continue
		}

		sid := external.GetSessionId()
		if sid == "" {
			return res, fmt.Errorf("shim-store ingest: entry with empty session_id (producer=%q index=%d kind=%q)", producer, i, kind)
		}

		cur, err := loadSeq(sid)
		if err != nil {
			return res, err
		}
		candidate := cur + 1

		out, err := tx.Exec(insertSQL,
			sid, candidate, plane, kind, nullStr(writeID),
			nullStr(external.GetMessage().GetTopLevelMessageId()),
			external.GetProducedAtMs(), blob)
		if err != nil {
			return res, fmt.Errorf("shim-store ingest: inserting entry (session=%q seq=%d kind=%q): %w", sid, candidate, kind, err)
		}
		n, err := out.RowsAffected()
		if err != nil {
			return res, fmt.Errorf("shim-store ingest: rows-affected (session=%q seq=%d): %w", sid, candidate, err)
		}
		if n == 1 {
			res.Accepted++
			sessionSeq[sid] = candidate
			if candidate > res.LastSeq {
				res.LastSeq = candidate
			}
			res.Deliveries = append(res.Deliveries, &protocolv1.EntryDelivery{
				Delivery: &protocolv1.EntryDelivery_Stored{Stored: &protocolv1.StoredEntryDelivery{
					Seq:   candidate,
					Entry: external,
				}},
			})
			continue
		}

		// The write_id index rejected it: one producer delivered one record
		// twice, which is the idempotent-replay guarantee firing. It consumes
		// no seq and produces NO delivery — the original write already fanned
		// this record out, and re-fanning it would turn an idempotent store
		// write into a duplicate DELIVERY, the same defect one layer up.
		res.Replayed++
		if writeID == "" {
			// Unreachable via entry_write_id, which is partial on a non-null
			// write_id, so a rejection with no write identity means some other
			// constraint fired. Surfaced rather than counted as a replay we
			// cannot substantiate.
			return res, fmt.Errorf("shim-store ingest: entry rejected by a constraint with no write identity to explain it (session=%q seq=%d kind=%q)", sid, candidate, kind)
		}
		var prior uint64
		switch err := tx.QueryRow(writeIDSeqSQL, sid, writeID).Scan(&prior); {
		case err == nil:
			d.log.LogVerbose(logging.Fields{
				Operation: "ingest", Producer: producer, Table: "entry",
			}, "replayed write is a no-op session=%q write_id=%q existing_seq=%d", sid, writeID, prior)
		case errors.Is(err, sql.ErrNoRows):
			return res, fmt.Errorf("shim-store ingest: entry rejected but no row holds its write identity (session=%q write_id=%q seq=%d)", sid, writeID, candidate)
		default:
			return res, fmt.Errorf("shim-store ingest: resolving duplicate cause (session=%q write_id=%q): %w", sid, writeID, err)
		}
	}

	if cursor != nil {
		if err := upsertCursor(tx, cursor); err != nil {
			return res, err
		}
	}

	if err := tx.Commit(); err != nil {
		return res, fmt.Errorf("shim-store ingest: commit (producer=%q): %w", producer, err)
	}
	d.log.LogVerbose(logging.Fields{
		Operation: "ingest", Producer: producer, Table: "entry", Transaction: "BEGIN IMMEDIATE",
	}, "transaction committed accepted=%d replayed=%d unconverted=%d last_seq=%d cursor_advance=%t", res.Accepted, res.Replayed, res.Unconverted, res.LastSeq, cursor != nil)
	return res, nil
}

func upsertCursor(tx *sql.Tx, c *agentshimv1.CursorState) error {
	const upsertSQL = `INSERT INTO cursor (file_id, path, offset, carry, updated_at)
	  VALUES (?, ?, ?, ?, ?)
	  ON CONFLICT(file_id) DO UPDATE SET
	    path = excluded.path, offset = excluded.offset,
	    carry = excluded.carry, updated_at = excluded.updated_at`
	if _, err := tx.Exec(upsertSQL, c.GetFileId(), c.GetPath(), c.GetOffset(), c.GetCarry(), nowMillis()); err != nil {
		return fmt.Errorf("shim-store ingest: upserting cursor (file_id=%q): %w", c.GetFileId(), err)
	}
	return nil
}

// planeOf returns the `plane` column, refusing a record that does not say which
// producer observed it.
//
// THIS IS WHERE THE EPHEMERAL INVARIANT WENT. Ingest used to reject an
// EVENT_CLASS_EPHEMERAL event reaching persistence, because such an event was
// supposed to bypass the database entirely. `EventClass` is retired and no
// message on the write surface can say "do not store this", so that particular
// violation is no longer expressible. What replaced it is the invariant the new
// record DOES carry: every stored record has an internal half, and at minimum
// that half names the observing plane. A record without one cannot be
// attributed, so it rejects the whole batch exactly as the old violation did.
func planeOf(entry *agentshimv1.Entry) (int, error) {
	internal := entry.GetInternal()
	if internal == nil {
		return planeUnset, errors.New("entry has no internal half — every stored record has one, at minimum naming the producer that observed it")
	}
	switch internal.GetPlane().GetPlane().(type) {
	case *agentshimv1.Plane_Stream:
		return planeStream, nil
	case *agentshimv1.Plane_File:
		return planeFile, nil
	default:
		return planeUnset, errors.New("entry does not name the plane that observed it")
	}
}

// kindOf returns the `kind` column: which arm of the record is set, in
// `<half>.<arm>` form ("message.user_said", "bookkeeping.turn_began",
// "unconverted.unparsed").
//
// READ REFLECTIVELY RATHER THAN BY TYPE SWITCH. The old spelling was a switch
// over every payload arm, which meant a schema that grew an arm silently
// reported "Unknown" for it until somebody noticed. The oneof descriptor
// already knows every arm's name, so this cannot fall behind the schema.
//
// It is diagnostic only: nothing indexes it and no query selects on it. The
// column exists so the store's own logs and error messages can say WHAT was
// rejected without opening the opaque payload.
func kindOf(entry *agentshimv1.Entry) string {
	if external := entry.GetExternal(); external != nil {
		switch arm := external.GetEntry().(type) {
		case *protocolv1.ExternalEntry_Message:
			return "message." + oneofArm(arm.Message.ProtoReflect(), "payload")
		case *protocolv1.ExternalEntry_Bookkeeping:
			return "bookkeeping." + oneofArm(arm.Bookkeeping.ProtoReflect(), "kind")
		default:
			return "external.unset"
		}
	}
	return "unconverted." + oneofArm(entry.GetInternal().ProtoReflect(), "unconverted")
}

// oneofArm names the set arm of a oneof, or "unset" when none is.
func oneofArm(m protoreflect.Message, oneof string) string {
	if m == nil || !m.IsValid() {
		return "unset"
	}
	od := m.Descriptor().Oneofs().ByName(protoreflect.Name(oneof))
	if od == nil {
		// The schema no longer declares the oneof this column is derived from.
		// Loud in the data rather than silently indistinguishable from "unset",
		// because the two mean opposite things about the record.
		return "no-such-oneof:" + oneof
	}
	fd := m.WhichOneof(od)
	if fd == nil {
		return "unset"
	}
	return string(fd.Name())
}

// nullStr maps "" to a SQL NULL so the partial indexes (WHERE ... IS NOT NULL)
// skip empty extracted columns.
func nullStr(s string) any {
	if s == "" {
		return nil
	}
	return s
}
