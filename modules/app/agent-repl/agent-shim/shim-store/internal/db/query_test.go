package db

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

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

func cursorBatch(c *storev1.CursorState) *storev1.EntryBatch {
	return &storev1.EntryBatch{CursorAdvance: c}
}

func TestCursorRecovery(t *testing.T) {
	// Arrange
	d := openTemp(t)
	c1 := &storev1.CursorState{FileId: "1:2", Path: "/a.jsonl", Offset: 10}
	c2 := &storev1.CursorState{FileId: "3:4", Path: "/b.jsonl", Offset: 20, Carry: []byte("x")}
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
	if _, err := d.Ingest("sidecar", cursorBatch(&storev1.CursorState{FileId: "1:2", Path: "/a", Offset: 10})); err != nil {
		t.Fatalf("Ingest v1: %v", err)
	}
	// Act: same file_id, advanced offset.
	if _, err := d.Ingest("sidecar", cursorBatch(&storev1.CursorState{FileId: "1:2", Path: "/a", Offset: 99})); err != nil {
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
	if _, err := d.Ingest("sidecar", cursorBatch(&storev1.CursorState{FileId: "1:2", Path: "/a", Offset: 5, Carry: []byte(`{"par`)})); err != nil {
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
