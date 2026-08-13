package db

import (
	"context"
	"testing"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
	"google.golang.org/protobuf/proto"
)

func seqs(deliveries []*protocolv1.EntryDelivery) []uint64 {
	out := make([]uint64, len(deliveries))
	for i, d := range deliveries {
		out[i] = d.GetStored().GetSeq()
	}
	return out
}

// manyBookkeeping builds n records for one session.
func manyBookkeeping(session string, n int) *agentshimv1.EntryBatch {
	entries := make([]*agentshimv1.Entry, 0, n)
	for range n {
		entries = append(entries, bookkeeping(session))
	}
	return &agentshimv1.EntryBatch{Entries: entries}
}

func TestReplayFromStreamsLargeHistoryInOrder(t *testing.T) {
	// Arrange: match the observed incident scale closely enough that a future
	// slice-based implementation would again be an obvious architectural
	// regression rather than an optimization hidden by tiny fixtures.
	const recordCount = 3702
	d := openTemp(t)
	if _, err := d.Ingest("p", manyBookkeeping("s1", recordCount)); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Act: each row is observed through the callback while the query is open.
	var delivered uint64
	stats, err := d.ReplayFrom(context.Background(), "s1", 0, func(delivery *protocolv1.EntryDelivery) error {
		delivered++
		if delivery.GetStored().GetSeq() != delivered {
			t.Fatalf("streamed seq=%d at position=%d", delivery.GetStored().GetSeq(), delivered)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	if delivered != recordCount || stats.Entries != recordCount || stats.FirstSeq != 1 || stats.LastSeq != recordCount {
		t.Fatalf("delivered=%d stats=%+v, want %d records spanning [1,%d]", delivered, stats, recordCount, recordCount)
	}
}

func TestReplayFromDeliversEarlierRowsBeforeALaterDecodeFailure(t *testing.T) {
	// Arrange: seq 1 is valid and seq 2 is deliberately malformed. A
	// materializing implementation would decode the later row before the
	// caller saw seq 1; row-to-callback streaming must expose seq 1 first.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(bookkeeping("s1"), bookkeeping("s1"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if _, err := d.sql.Exec(`UPDATE entry SET payload = X'00' WHERE session_id = 's1' AND seq = 2`); err != nil {
		t.Fatalf("corrupting later fixture row: %v", err)
	}

	// Act
	var delivered []uint64
	stats, err := d.ReplayFrom(context.Background(), "s1", 0, func(delivery *protocolv1.EntryDelivery) error {
		delivered = append(delivered, delivery.GetStored().GetSeq())
		return nil
	})

	// Assert: the decode error remains loud, after the prior row was delivered.
	if err == nil {
		t.Fatal("ReplayFrom unexpectedly accepted a malformed persisted payload")
	}
	if len(delivered) != 1 || delivered[0] != 1 || stats.Entries != 1 || stats.FirstSeq != 1 || stats.LastSeq != 1 {
		t.Fatalf("delivered seqs=%v stats=%+v before failure, want only seq 1", delivered, stats)
	}
}

func TestReplayFromSurfacesARowWithNoExternalHalf(t *testing.T) {
	// Arrange: only records WITH an external half are written to `entry`, so
	// one without is corruption. It must surface rather than be delivered as an
	// envelope carrying nothing.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(bookkeeping("s1"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	internalOnly := &agentshimv1.Entry{Internal: streamPlane()}
	blob, err := proto.Marshal(internalOnly)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	if _, err := d.sql.Exec(`UPDATE entry SET payload = ? WHERE session_id = 's1' AND seq = 1`, blob); err != nil {
		t.Fatalf("seeding a half-less row: %v", err)
	}

	// Act
	_, err = d.ReplayFrom(context.Background(), "s1", 0, func(*protocolv1.EntryDelivery) error { return nil })

	// Assert
	if err == nil {
		t.Fatal("ReplayFrom delivered a row with no external half")
	}
}

func TestReplayFromZeroReturnsAll(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("p", manyBookkeeping("s1", 3)); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Act
	got := collectReplay(t, d, "s1", 0)
	// Assert
	if len(got) != 3 {
		t.Fatalf("replayed %d records, want 3", len(got))
	}
	for i, delivery := range got {
		if delivery.GetStored().GetSeq() != uint64(i+1) {
			t.Fatalf("replayed[%d] seq = %d, want %d", i, delivery.GetStored().GetSeq(), i+1)
		}
	}
}

func TestReplayFromMidSeqIsExclusive(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("p", manyBookkeeping("s1", 3)); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Act: from_seq is EXCLUSIVE, so from_seq=1 yields seqs 2,3.
	got := collectReplay(t, d, "s1", 1)
	// Assert
	if len(got) != 2 || got[0].GetStored().GetSeq() != 2 || got[1].GetStored().GetSeq() != 3 {
		t.Fatalf("replay from_seq=1 gave seqs %v, want [2 3]", seqs(got))
	}
}

func TestReplayIsSessionScoped(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(bookkeeping("a"), bookkeeping("b"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Act
	got := collectReplay(t, d, "a", 0)
	// Assert
	if len(got) != 1 || got[0].GetStored().GetEntry().GetSessionId() != "a" {
		t.Fatalf("replay for session a returned %d records (want 1 for 'a')", len(got))
	}
}

func TestReplayDeliversTheExternalHalfVerbatim(t *testing.T) {
	// Arrange: what a subscriber receives is a field access on the stored
	// record, so the arm and its contents must survive the round trip whole.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(message("s1", "m-1", "m-top"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Act
	got := collectReplay(t, d, "s1", 0)

	// Assert
	if len(got) != 1 {
		t.Fatalf("replayed %d records, want 1", len(got))
	}
	entry := got[0].GetStored().GetEntry().GetMessage()
	if entry.GetMessageId() != "m-1" || entry.GetTopLevelMessageId() != "m-top" {
		t.Fatalf("replayed message = %+v, want m-1 owned by m-top", entry)
	}
}

func TestReplayNeverCarriesTheInternalHalf(t *testing.T) {
	// Arrange: the store persists both halves and serves one. There is no field
	// on the delivery envelope an internal half could occupy, so this asserts
	// what the type already guarantees — and would fail loudly if a future
	// change smuggled the whole record onto the wire.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(withWriteID(bookkeeping("s1"), "w-1"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Act
	got := collectReplay(t, d, "s1", 0)

	// Assert
	stored := got[0].GetStored()
	if stored.GetEntry() == nil {
		t.Fatal("delivery carries no external half")
	}
	if stored.GetSeq() != 1 {
		t.Fatalf("delivery seq = %d, want 1 — position rides the envelope, not the record", stored.GetSeq())
	}
}

func TestReplayNeverUsesTheLiveArm(t *testing.T) {
	// Arrange: a stored record has a position, so it arrives on the `stored`
	// arm. The `live` arm has no seq field at all, which is what stops a
	// consumer advancing a resume cursor onto a record with no durable place.
	d := openTemp(t)
	if _, err := d.Ingest("p", batch(bookkeeping("s1"))); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Act
	got := collectReplay(t, d, "s1", 0)

	// Assert
	if got[0].GetLive() != nil {
		t.Fatal("a replayed record arrived on the live arm, which has no position to resume from")
	}
}

func TestMaxSeqOnAnEmptySessionIsZero(t *testing.T) {
	// Arrange / Act
	d := openTemp(t)
	got, err := d.MaxSeq("nobody")
	// Assert
	if err != nil {
		t.Fatalf("MaxSeq: %v", err)
	}
	if got != 0 {
		t.Fatalf("MaxSeq on an empty session = %d, want 0", got)
	}
}

// --- cursors ---------------------------------------------------------------

func cursorBatch(c *agentshimv1.CursorState) *agentshimv1.EntryBatch {
	return &agentshimv1.EntryBatch{CursorAdvance: c}
}

func TestCursorRecovery(t *testing.T) {
	// Arrange
	d := openTemp(t)
	c1 := &agentshimv1.CursorState{FileId: "1:2", Path: "/a.jsonl", Offset: 10}
	c2 := &agentshimv1.CursorState{FileId: "3:4", Path: "/b.jsonl", Offset: 20, Carry: []byte("x")}
	if _, err := d.Ingest("sidecar", cursorBatch(c1)); err != nil {
		t.Fatalf("Ingest c1: %v", err)
	}
	if _, err := d.Ingest("sidecar", cursorBatch(c2)); err != nil {
		t.Fatalf("Ingest c2: %v", err)
	}
	// Act
	all, err := d.Cursors()
	// Assert
	if err != nil {
		t.Fatalf("Cursors: %v", err)
	}
	if len(all) != 2 {
		t.Fatalf("recovered %d cursors, want 2", len(all))
	}
}

func TestCursorAbsentReturnsNil(t *testing.T) {
	// Arrange
	d := openTemp(t)
	// Act
	got, err := d.Cursor("nope")
	// Assert
	if err != nil {
		t.Fatalf("Cursor: %v", err)
	}
	if got != nil {
		t.Fatalf("Cursor(absent) = %+v, want nil", got)
	}
}

func TestCursorUpsertOverwrites(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("sidecar", cursorBatch(&agentshimv1.CursorState{FileId: "1:2", Path: "/a", Offset: 10})); err != nil {
		t.Fatalf("Ingest v1: %v", err)
	}
	// Act: same file_id, advanced offset.
	if _, err := d.Ingest("sidecar", cursorBatch(&agentshimv1.CursorState{FileId: "1:2", Path: "/a", Offset: 99})); err != nil {
		t.Fatalf("Ingest v2: %v", err)
	}
	// Assert
	got, err := d.Cursor("1:2")
	if err != nil {
		t.Fatalf("Cursor: %v", err)
	}
	if got.GetOffset() != 99 {
		t.Fatalf("offset after upsert = %d, want 99", got.GetOffset())
	}
}

func TestCursorPreservesThePartialLineCarry(t *testing.T) {
	// Arrange: the carry is what makes a resumed read pick up mid-line.
	d := openTemp(t)
	if _, err := d.Ingest("sidecar", cursorBatch(&agentshimv1.CursorState{FileId: "1:2", Path: "/a", Offset: 5, Carry: []byte(`{"par`)})); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Act
	got, err := d.Cursor("1:2")
	// Assert
	if err != nil {
		t.Fatalf("Cursor: %v", err)
	}
	if string(got.GetCarry()) != `{"par` {
		t.Fatalf("carry = %q, want %q", got.GetCarry(), `{"par`)
	}
}
