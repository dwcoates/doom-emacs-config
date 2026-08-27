package db

import (
	"database/sql"
	"errors"
	"fmt"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// Plane column values. The observing plane is a producer-side fact and the
// store is entitled to it, so it is extracted for attribution in the store's
// own logs — never returned to a reader.
const (
	planeUnset  = 0
	planeStream = 1
	planeFile   = 2
)

// Result reports the outcome of one Ingest call.
//
// It no longer carries a `Deliveries` slice: `protocol.v1.EntryDelivery` was
// deleted with the UDS entry model, and the store.v1 replacement
// (WatchAgentSessionResponse, carrying a StoreLineAt) is addressed by a
// store-minted StoreItemPointer rather than by the per-session `seq` this
// layer assigns. Wiring one to the other is a design decision, not a rename,
// so nothing is invented here.
type Result struct {
	// Accepted is the number of records persisted with a freshly assigned seq.
	Accepted uint64
	// Replayed is the number of records the producer re-delivered under a
	// write_id this store had already written.
	Replayed uint64
	// Unconverted is the number of records stored with no serveable half.
	Unconverted uint64
	// LastSeq is the highest seq assigned in this batch (0 if none).
	LastSeq uint64
}

// ErrRecordPersistenceUnreconciled is returned for every batch that carries
// records.
//
// WHY THE PATH IS STUBBED RATHER THAN ADAPTED. The `entry` table addresses a
// record by (session_id, seq) and pages it by top_level_message_id. All three
// columns were extracted off `protocol.v1.ExternalEntry`, which the store.v1
// redesign deleted: `store.v1.StoreEntry` names no session at all, carries no
// position, and states its book as a `conversation.v1.AgentId` inside
// StorePageLine. Choosing what those columns become — and what a StoreItemPointer
// is minted from — is a schema decision this reconciliation is not entitled to
// make, so the write is REFUSED LOUDLY instead of being written under a guess
// that would land unreadable rows.
var ErrRecordPersistenceUnreconciled = errors.New(
	"shim-store ingest: record persistence is not reconciled with store.v1 — StoreEntry carries no session_id, no position and no top_level_message_id, so the entry table's addressing has no source; refusing the batch rather than persisting rows no read can address")

// Ingest persists a batch and, atomically in the SAME transaction, advances the
// file cursor when one is supplied.
//
// The cursor half is intact: `store.v1.CursorState` is a pure rename of the
// retired `agentshim.v1.CursorState` and its columns are unchanged. The record
// half is refused — see ErrRecordPersistenceUnreconciled.
func (d *DB) Ingest(producer string, batch *storev1.EntryBatch) (res Result, resultErr error) {
	entries := batch.GetEntries()
	cursor := batch.GetCursorAdvance()
	d.log.LogVerbose(logging.Fields{
		Operation: "ingest", Producer: producer, Table: "entry", Transaction: "BEGIN IMMEDIATE",
	}, "starting transaction entries=%d cursor_advance=%t", len(entries), cursor != nil)
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

	// The per-record envelope checks that survived the redesign still run, and
	// they still reject the whole batch, so a producer sending an unattributable
	// record learns that before it learns the persistence gap.
	for i, entry := range entries {
		if _, err := planeOf(entry); err != nil {
			return res, fmt.Errorf("shim-store ingest: %w (producer=%q index=%d)", err, producer, i)
		}
	}
	if len(entries) > 0 {
		return res, fmt.Errorf("%w (producer=%q entries=%d first_kind=%q)",
			ErrRecordPersistenceUnreconciled, producer, len(entries), kindOf(entries[0]))
	}

	tx, err := d.sql.Begin()
	if err != nil {
		return res, fmt.Errorf("shim-store ingest: begin tx (producer=%q): %w", producer, err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit

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

func upsertCursor(tx *sql.Tx, c *storev1.CursorState) error {
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
// producer observed it. The plane moved from the retired record's internal half
// onto StoreEntry itself; the invariant is unchanged.
func planeOf(entry *storev1.StoreEntry) (int, error) {
	switch entry.GetPlane().GetPlane().(type) {
	case *storev1.Plane_Stream:
		return planeStream, nil
	case *storev1.Plane_File:
		return planeFile, nil
	default:
		return planeUnset, errors.New("entry does not name the plane that observed it")
	}
}

// kindOf names which arm of the record is set, in `<oneof>.<arm>` form
// ("entry.agent_update", "agent_info.serveable_frame").
//
// READ REFLECTIVELY RATHER THAN BY TYPE SWITCH, so a schema that grows an arm
// cannot silently report it as unknown. Diagnostic only: nothing indexes it.
func kindOf(entry *storev1.StoreEntry) string {
	arm := oneofArm(entry.ProtoReflect(), "entry")
	if update := entry.GetAgentUpdate(); update != nil {
		return arm + "." + oneofArm(update.ProtoReflect(), "agent_info")
	}
	return arm
}

// oneofArm names the set arm of a oneof, or "unset" when none is.
func oneofArm(m protoreflect.Message, oneof string) string {
	if m == nil || !m.IsValid() {
		return "unset"
	}
	od := m.Descriptor().Oneofs().ByName(protoreflect.Name(oneof))
	if od == nil {
		// The schema no longer declares the oneof this name is derived from.
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
