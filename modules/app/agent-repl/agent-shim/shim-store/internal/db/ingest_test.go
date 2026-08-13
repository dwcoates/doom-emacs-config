package db

import (
	"bytes"
	"database/sql"
	"io"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/shim-store/internal/logging"
)

// --- seq assignment --------------------------------------------------------

func TestIngestAssignsGaplessSeq(t *testing.T) {
	// Arrange
	d := openTemp(t)

	// Act
	res, err := d.Ingest("p", batch(bookkeeping("s1"), bookkeeping("s1"), bookkeeping("s1")))

	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.Accepted != 3 || res.LastSeq != 3 {
		t.Fatalf("accepted=%d last_seq=%d, want 3 and 3", res.Accepted, res.LastSeq)
	}
	for i, delivery := range collectReplay(t, d, "s1", 0) {
		if want := uint64(i + 1); delivery.GetStored().GetSeq() != want {
			t.Fatalf("record %d has seq %d, want %d", i, delivery.GetStored().GetSeq(), want)
		}
	}
}

func TestIngestSeparateSessionsSeqIndependently(t *testing.T) {
	// Arrange
	d := openTemp(t)

	// Act
	if _, err := d.Ingest("p", batch(bookkeeping("s1"), bookkeeping("s2"), bookkeeping("s1"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Assert: each session's sequence starts at 1 and counts only its own.
	if got := len(collectReplay(t, d, "s2", 0)); got != 1 {
		t.Fatalf("session s2 holds %d records, want 1", got)
	}
	s2, err := d.MaxSeq("s2")
	if err != nil {
		t.Fatalf("MaxSeq: %v", err)
	}
	if s2 != 1 {
		t.Fatalf("s2 max seq = %d, want 1 — sessions do not share a sequence", s2)
	}
}

// --- replay idempotency ----------------------------------------------------

func TestIngestReplayedWriteIdIsANoOp(t *testing.T) {
	// Arrange: a record the producer already delivered once.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(withWriteID(bookkeeping("s1"), "w-1"))); err != nil {
		t.Fatalf("first Ingest: %v", err)
	}

	// Act: the producer re-sends it, which is what an unlearned outcome looks
	// like from its side.
	res, err := d.Ingest("p", batch(withWriteID(bookkeeping("s1"), "w-1")))

	// Assert
	if err != nil {
		t.Fatalf("replayed Ingest: %v", err)
	}
	if res.Accepted != 0 || res.Replayed != 1 {
		t.Fatalf("accepted=%d replayed=%d, want 0 and 1", res.Accepted, res.Replayed)
	}
	if got := len(collectReplay(t, d, "s1", 0)); got != 1 {
		t.Fatalf("session holds %d records after a replay, want 1", got)
	}
}

func TestIngestReplayedWriteConsumesNoSeq(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(withWriteID(bookkeeping("s1"), "w-1"))); err != nil {
		t.Fatalf("first Ingest: %v", err)
	}
	if _, err := d.Ingest("p", batch(withWriteID(bookkeeping("s1"), "w-1"))); err != nil {
		t.Fatalf("replayed Ingest: %v", err)
	}

	// Act
	res, err := d.Ingest("p", batch(bookkeeping("s1")))

	// Assert: the next record takes seq 2, so the replay left no gap.
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.LastSeq != 2 {
		t.Fatalf("last_seq after a replay = %d, want 2 — the replay consumed a seq", res.LastSeq)
	}
}

func TestIngestReplayedWriteIsNotDeliveredAgain(t *testing.T) {
	// Arrange: the first write already fanned this record out.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(withWriteID(bookkeeping("s1"), "w-1"))); err != nil {
		t.Fatalf("first Ingest: %v", err)
	}

	// Act
	res, err := d.Ingest("p", batch(withWriteID(bookkeeping("s1"), "w-1")))

	// Assert: an idempotent store write must not become a duplicate DELIVERY.
	if err != nil {
		t.Fatalf("replayed Ingest: %v", err)
	}
	if len(res.Deliveries) != 0 {
		t.Fatalf("a replayed write produced %d deliveries, want 0", len(res.Deliveries))
	}
}

func TestIngestDistinctWriteIdsBothLand(t *testing.T) {
	// Arrange / Act
	d := openTemp(t)
	res, err := d.Ingest("p", batch(
		withWriteID(bookkeeping("s1"), "w-1"),
		withWriteID(bookkeeping("s1"), "w-2"),
	))

	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.Accepted != 2 || res.Replayed != 0 {
		t.Fatalf("accepted=%d replayed=%d, want 2 and 0", res.Accepted, res.Replayed)
	}
}

func TestIngestSameWriteIdInDifferentSessionsBothLand(t *testing.T) {
	// Arrange / Act: uniqueness is scoped to (session_id, write_id), so two
	// sessions minting the same identity are not each other's replay.
	d := openTemp(t)
	res, err := d.Ingest("p", batch(
		withWriteID(bookkeeping("s1"), "w-1"),
		withWriteID(bookkeeping("s2"), "w-1"),
	))

	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.Accepted != 2 {
		t.Fatalf("accepted=%d, want 2", res.Accepted)
	}
}

func TestIngestWithoutAWriteIdIsNotReplayIdempotent(t *testing.T) {
	// Arrange: the field's contract says the store enforces uniqueness only
	// over non-empty values, so an unidentified record delivered twice lands
	// twice. That is a documented cost of omitting the identity, not a bug.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(bookkeeping("s1"))); err != nil {
		t.Fatalf("first Ingest: %v", err)
	}

	// Act
	res, err := d.Ingest("p", batch(bookkeeping("s1")))

	// Assert
	if err != nil {
		t.Fatalf("second Ingest: %v", err)
	}
	if res.Accepted != 1 {
		t.Fatalf("accepted=%d, want 1 — an unidentified record cannot be recognized as a repeat", res.Accepted)
	}
	if got := len(collectReplay(t, d, "s1", 0)); got != 2 {
		t.Fatalf("session holds %d records, want 2", got)
	}
}

// --- cursor atomicity ------------------------------------------------------

func TestIngestCommitsCursorAtomically(t *testing.T) {
	// Arrange
	d := openTemp(t)
	entries := batch(bookkeeping("s1"))
	entries.CursorAdvance = &agentshimv1.CursorState{FileId: "dev:1", Path: "/t.jsonl", Offset: 42}

	// Act
	if _, err := d.Ingest("p", entries); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Assert
	c, err := d.Cursor("dev:1")
	if err != nil {
		t.Fatalf("Cursor: %v", err)
	}
	if c.GetOffset() != 42 {
		t.Fatalf("cursor offset = %d, want 42 — it did not commit with the records", c.GetOffset())
	}
}

func TestIngestAdvancesACursorWithNoRecords(t *testing.T) {
	// Arrange: a reader that consumed only records it could skip still has to
	// persist where it got to.
	d := openTemp(t)
	entries := &agentshimv1.EntryBatch{CursorAdvance: &agentshimv1.CursorState{FileId: "dev:1", Path: "/t.jsonl", Offset: 7}}

	// Act
	res, err := d.Ingest("p", entries)

	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.Accepted != 0 {
		t.Fatalf("accepted=%d, want 0", res.Accepted)
	}
	c, err := d.Cursor("dev:1")
	if err != nil {
		t.Fatalf("Cursor: %v", err)
	}
	if c.GetOffset() != 7 {
		t.Fatalf("cursor offset = %d, want 7", c.GetOffset())
	}
}

// --- unconverted records ---------------------------------------------------

func TestIngestStoresAnUnconvertedRecordWhole(t *testing.T) {
	// Arrange / Act: agentshim.v1's unconverted arm is durable on purpose, so
	// the decision not to model something stays reversible from stored data.
	d := openTemp(t)
	res, err := d.Ingest("p", batch(unconverted("bad json")))

	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.Unconverted != 1 {
		t.Fatalf("unconverted=%d, want 1", res.Unconverted)
	}
	var stored int
	if err := d.sql.QueryRow(`SELECT COUNT(*) FROM unconverted`).Scan(&stored); err != nil {
		t.Fatalf("counting unconverted rows: %v", err)
	}
	if stored != 1 {
		t.Fatalf("unconverted table holds %d rows, want 1", stored)
	}
}

func TestIngestGivesAnUnconvertedRecordNoPosition(t *testing.T) {
	// Arrange: seq is per-session addressing and an unconverted record carries
	// no session anywhere in the schema, so it must consume none.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(unconverted("bad json"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Act
	res, err := d.Ingest("p", batch(bookkeeping("s1")))

	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.LastSeq != 1 {
		t.Fatalf("last_seq = %d, want 1 — an unconverted record consumed a position", res.LastSeq)
	}
}

func TestIngestNeverDeliversAnUnconvertedRecord(t *testing.T) {
	// Arrange / Act: it has no external half, so there is nothing to hand over.
	d := openTemp(t)
	res, err := d.Ingest("p", batch(unconverted("bad json")))

	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if len(res.Deliveries) != 0 {
		t.Fatalf("an unconverted record produced %d deliveries, want 0", len(res.Deliveries))
	}
}

func TestIngestKeepsAnUnconvertedRecordOutOfReplay(t *testing.T) {
	// Arrange: it is written to a different table, so replay cannot reach it —
	// a fact about the schema rather than a filter the query applies.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(unconverted("bad json"), bookkeeping("s1"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Act
	replayed := collectReplay(t, d, "s1", 0)

	// Assert
	if len(replayed) != 1 {
		t.Fatalf("replay returned %d records, want 1", len(replayed))
	}
}

func TestIngestReplayedUnconvertedWriteIsANoOp(t *testing.T) {
	// Arrange: an unconverted record has no session, so its write identity is
	// scoped to the producer instead.
	d := openTemp(t)
	first := unconverted("bad json")
	first.Internal.WriteId = "w-1"
	if _, err := d.Ingest("p", batch(first)); err != nil {
		t.Fatalf("first Ingest: %v", err)
	}
	repeat := unconverted("bad json")
	repeat.Internal.WriteId = "w-1"

	// Act
	res, err := d.Ingest("p", batch(repeat))

	// Assert
	if err != nil {
		t.Fatalf("replayed Ingest: %v", err)
	}
	if res.Unconverted != 0 || res.Replayed != 1 {
		t.Fatalf("unconverted=%d replayed=%d, want 0 and 1", res.Unconverted, res.Replayed)
	}
}

// --- invariant rejections --------------------------------------------------

func TestIngestRejectsARecordWithNoInternalHalf(t *testing.T) {
	// Arrange: every stored record has an internal half, at minimum naming the
	// producer that observed it.
	d := openTemp(t)
	entry := bookkeeping("s1")
	entry.Internal = nil

	// Act
	_, err := d.Ingest("p", batch(entry))

	// Assert
	if err == nil {
		t.Fatal("Ingest accepted a record with no internal half")
	}
	if !strings.Contains(err.Error(), "no internal half") {
		t.Fatalf("error = %v, want it to name the missing internal half", err)
	}
}

func TestIngestRejectsARecordThatNamesNoPlane(t *testing.T) {
	// Arrange: this is where the retired EPHEMERAL-reached-persistence check
	// went. EventClass can no longer express a violation, so the invariant the
	// NEW record carries is enforced instead: a record nobody can attribute.
	d := openTemp(t)
	entry := bookkeeping("s1")
	entry.Internal.Plane = nil

	// Act
	_, err := d.Ingest("p", batch(entry))

	// Assert
	if err == nil {
		t.Fatal("Ingest accepted a record that names no observing plane")
	}
	if !strings.Contains(err.Error(), "does not name the plane") {
		t.Fatalf("error = %v, want it to name the missing plane", err)
	}
}

func TestIngestRejectsAnEmptySession(t *testing.T) {
	// Arrange: the session id is the seq scope and the fan-out routing key, so
	// a record without one cannot be positioned or delivered.
	d := openTemp(t)
	entry := bookkeeping("")

	// Act
	_, err := d.Ingest("p", batch(entry))

	// Assert
	if err == nil {
		t.Fatal("Ingest accepted a record with an empty session_id")
	}
	if !strings.Contains(err.Error(), "empty session_id") {
		t.Fatalf("error = %v, want it to name the empty session", err)
	}
}

func TestIngestRejectsARecordThatSaysNothing(t *testing.T) {
	// Arrange: no external half AND no unconverted arm. Such a record can never
	// be read back by anything, so storing it would be a silent discard.
	d := openTemp(t)
	entry := &agentshimv1.Entry{Internal: streamPlane()}

	// Act
	_, err := d.Ingest("p", batch(entry))

	// Assert
	if err == nil {
		t.Fatal("Ingest accepted a record with neither an external half nor an unconverted arm")
	}
	if !strings.Contains(err.Error(), "can never be read back") {
		t.Fatalf("error = %v, want it to say the record is unreadable", err)
	}
}

func TestIngestRejectionRollsBackTheWholeBatch(t *testing.T) {
	// Arrange: a batch is one transaction, so a record rejected halfway must
	// take the records before it with it.
	d := openTemp(t)
	bad := bookkeeping("s1")
	bad.Internal.Plane = nil

	// Act
	if _, err := d.Ingest("p", batch(bookkeeping("s1"), bad)); err == nil {
		t.Fatal("Ingest accepted a batch containing an invalid record")
	}

	// Assert
	if got := len(collectReplay(t, d, "s1", 0)); got != 0 {
		t.Fatalf("session holds %d records after a rejected batch, want 0", got)
	}
}

func TestIngestLogsARejectedTransactionWithStoreContext(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "entries.db")
	var logs bytes.Buffer
	log := logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "db", DatabasePath: path})
	d, err := Open(path, log)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer d.Close()
	logs.Reset()

	// Act
	if _, err := d.Ingest("shim-claude-sidecar", batch(bookkeeping(""))); err == nil {
		t.Fatal("Ingest accepted a record with an empty session_id")
	}

	// Assert
	record, found := findRecord(t, &logs, "ingest", "error")
	if !found {
		t.Fatalf("rejected-transaction record missing: %s", logs.String())
	}
	if record.Context["producer"] != "shim-claude-sidecar" || record.Context["table"] != "entry" ||
		record.Context["transaction"] != "BEGIN IMMEDIATE" || record.Context["db"] != path {
		t.Fatalf("rejection lacks canonical store context: %#v", record)
	}
	if !strings.Contains(record.Message, "transaction rejected") {
		t.Fatalf("rejection message = %q, want it to say the transaction was rejected", record.Message)
	}
}

// --- extracted columns -----------------------------------------------------

func TestIngestExtractsTheOwningMessage(t *testing.T) {
	// Arrange / Act
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(message("s1", "m-child", "m-top"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Assert
	var owner string
	if err := d.sql.QueryRow(`SELECT top_level_message_id FROM entry WHERE session_id = 's1'`).Scan(&owner); err != nil {
		t.Fatalf("reading ownership column: %v", err)
	}
	if owner != "m-top" {
		t.Fatalf("top_level_message_id = %q, want %q", owner, "m-top")
	}
}

func TestIngestLeavesABookkeepingRecordUnowned(t *testing.T) {
	// Arrange: BookkeepingEntry has no field capable of naming a message, so the
	// ownership column is NULL by construction — which is what keeps a turn
	// boundary out of the partial index a page reads.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(bookkeeping("s1"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Act
	var owner sql.NullString
	if err := d.sql.QueryRow(`SELECT top_level_message_id FROM entry WHERE session_id = 's1'`).Scan(&owner); err != nil {
		t.Fatalf("reading ownership column: %v", err)
	}

	// Assert
	if owner.Valid {
		t.Fatalf("a bookkeeping record carries ownership %q, want SQL NULL", owner.String)
	}
}

func TestIngestExtractsTheObservingPlane(t *testing.T) {
	// Arrange: the file plane is the sidecar reading what the vendor wrote.
	d := openTemp(t)
	entry := bookkeeping("s1")
	entry.Internal.Plane = &agentshimv1.Plane{Plane: &agentshimv1.Plane_File{File: &agentshimv1.PlaneFile{}}}

	// Act
	if _, err := d.Ingest("p", batch(entry)); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Assert
	var plane int
	if err := d.sql.QueryRow(`SELECT plane FROM entry WHERE session_id = 's1'`).Scan(&plane); err != nil {
		t.Fatalf("reading plane column: %v", err)
	}
	if plane != planeFile {
		t.Fatalf("plane = %d, want %d", plane, planeFile)
	}
}

func TestIngestExtractsTheProducerClock(t *testing.T) {
	// Arrange
	d := openTemp(t)
	entry := bookkeeping("s1")
	entry.External.ProducedAtMs = 1755000000123

	// Act
	if _, err := d.Ingest("p", batch(entry)); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Assert
	var producedAt int64
	if err := d.sql.QueryRow(`SELECT produced_at FROM entry WHERE session_id = 's1'`).Scan(&producedAt); err != nil {
		t.Fatalf("reading produced_at column: %v", err)
	}
	if producedAt != 1755000000123 {
		t.Fatalf("produced_at = %d, want 1755000000123", producedAt)
	}
}

// --- kind derivation -------------------------------------------------------

func TestKindOfNamesAMessageArm(t *testing.T) {
	// Arrange / Act / Assert
	if got := kindOf(message("s1", "m-1", "m-1")); got != "message.user_said" {
		t.Fatalf("kindOf = %q, want %q", got, "message.user_said")
	}
}

func TestKindOfNamesABookkeepingArm(t *testing.T) {
	// Arrange / Act / Assert
	if got := kindOf(bookkeeping("s1")); got != "bookkeeping.turn_began" {
		t.Fatalf("kindOf = %q, want %q", got, "bookkeeping.turn_began")
	}
}

func TestKindOfNamesAnUnconvertedArm(t *testing.T) {
	// Arrange / Act / Assert
	if got := kindOf(unconverted("bad json")); got != "unconverted.unparsed" {
		t.Fatalf("kindOf = %q, want %q", got, "unconverted.unparsed")
	}
}

func TestKindOfReportsAnExternalHalfWithNoArmSet(t *testing.T) {
	// Arrange: a record that says it is deliverable but names nothing. The
	// column must say so rather than borrowing another arm's name.
	entry := &agentshimv1.Entry{Internal: streamPlane(), External: &protocolv1.ExternalEntry{SessionId: "s1"}}

	// Act / Assert
	if got := kindOf(entry); got != "external.unset" {
		t.Fatalf("kindOf = %q, want %q", got, "external.unset")
	}
}

func TestKindOfTracksAnArmTheSwitchNeverNamed(t *testing.T) {
	// Arrange: the derivation reads the oneof descriptor rather than a switch,
	// so an arm nobody wrote a case for is still named correctly. This is the
	// drift the old type switch produced by reporting "Unknown".
	entry := &agentshimv1.Entry{
		Internal: streamPlane(),
		External: &protocolv1.ExternalEntry{
			SessionId: "s1",
			Entry: &protocolv1.ExternalEntry_Message{Message: &conversationv1.MessageEntry{
				Payload: &conversationv1.MessageEntry_SkillBodyResolved{SkillBodyResolved: &conversationv1.SkillBodyResolved{}},
			}},
		},
	}

	// Act / Assert
	if got := kindOf(entry); got != "message.skill_body_resolved" {
		t.Fatalf("kindOf = %q, want %q", got, "message.skill_body_resolved")
	}
}

// --- concurrency -----------------------------------------------------------

func TestConcurrentIngestNeverRejectsABatch(t *testing.T) {
	// Arrange: a rejected batch is PERMANENT loss (the producer's store client
	// drops it — no spill, no retry), which is why the DSN carries
	// _txlock=immediate. Under the DEFERRED default these writers would take a
	// read snapshot and then fail to upgrade with SQLITE_BUSY_SNAPSHOT.
	const writers = 8
	const perWriter = 10
	d := openTemp(t)

	// Act
	var ready, done sync.WaitGroup
	ready.Add(writers)
	done.Add(writers)
	start := make(chan struct{})
	errs := make([]error, writers)
	for i := range writers {
		go func() {
			defer done.Done()
			ready.Done()
			<-start
			for range perWriter {
				if _, err := d.Ingest("p", batch(bookkeeping("s1"))); err != nil {
					errs[i] = err
					return
				}
			}
		}()
	}
	ready.Wait()
	close(start)
	done.Wait()

	// Assert
	for i, err := range errs {
		if err != nil {
			t.Fatalf("writer %d lost a batch permanently: %v", i, err)
		}
	}
	if got := len(collectReplay(t, d, "s1", 0)); got != writers*perWriter {
		t.Fatalf("persisted %d records, want %d", got, writers*perWriter)
	}
}

func TestConcurrentIngestKeepsSeqGapless(t *testing.T) {
	// Arrange
	const writers = 8
	const perWriter = 10
	d := openTemp(t)

	// Act
	var ready, done sync.WaitGroup
	ready.Add(writers)
	done.Add(writers)
	start := make(chan struct{})
	for range writers {
		go func() {
			defer done.Done()
			ready.Done()
			<-start
			for range perWriter {
				if _, err := d.Ingest("p", batch(bookkeeping("s1"))); err != nil {
					return
				}
			}
		}()
	}
	ready.Wait()
	close(start)
	done.Wait()

	// Assert
	for i, delivery := range collectReplay(t, d, "s1", 0) {
		if want := uint64(i + 1); delivery.GetStored().GetSeq() != want {
			t.Fatalf("record %d has seq %d, want %d — the sequence gapped", i, delivery.GetStored().GetSeq(), want)
		}
	}
}
