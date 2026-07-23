package db

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func TestReplayFromZeroReturnsAll(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("p", []*corev1.Event{persistentCore("s1"), persistentCore("s1"), persistentCore("s1")}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Act
	got, err := d.ReplayFrom("s1", 0)
	// Assert
	if err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	if len(got) != 3 {
		t.Fatalf("replayed %d events, want 3", len(got))
	}
	for i, ev := range got {
		if ev.GetSeq() != uint64(i+1) {
			t.Fatalf("replayed[%d] seq = %d, want %d", i, ev.GetSeq(), i+1)
		}
	}
}

func TestReplayFromMidSeqIsExclusive(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("p", []*corev1.Event{persistentCore("s1"), persistentCore("s1"), persistentCore("s1")}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Act: from_seq is EXCLUSIVE, so from_seq=1 yields seqs 2,3.
	got, err := d.ReplayFrom("s1", 1)
	// Assert
	if err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	if len(got) != 2 || got[0].GetSeq() != 2 || got[1].GetSeq() != 3 {
		t.Fatalf("replay from_seq=1 gave seqs %v, want [2 3]", seqs(got))
	}
}

func TestReplayIsSessionScoped(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("p", []*corev1.Event{persistentCore("a"), persistentCore("b")}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Act
	got, err := d.ReplayFrom("a", 0)
	// Assert
	if err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	if len(got) != 1 || got[0].GetSessionId() != "a" {
		t.Fatalf("replay for session a returned %d events (want 1 for 'a')", len(got))
	}
}

func TestCursorRecovery(t *testing.T) {
	// Arrange
	d := openTemp(t)
	c1 := &corev1.CursorState{FileId: "1:2", Path: "/a.jsonl", Offset: 10}
	c2 := &corev1.CursorState{FileId: "3:4", Path: "/b.jsonl", Offset: 20, Carry: []byte("x")}
	if _, err := d.Ingest("sidecar", nil, c1); err != nil {
		t.Fatalf("Ingest c1: %v", err)
	}
	if _, err := d.Ingest("sidecar", nil, c2); err != nil {
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
	if _, err := d.Ingest("sidecar", nil, &corev1.CursorState{FileId: "1:2", Path: "/a", Offset: 10}); err != nil {
		t.Fatalf("Ingest v1: %v", err)
	}
	// Act: same file_id, advanced offset.
	if _, err := d.Ingest("sidecar", nil, &corev1.CursorState{FileId: "1:2", Path: "/a", Offset: 99}); err != nil {
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

func seqs(evs []*corev1.Event) []uint64 {
	out := make([]uint64, len(evs))
	for i, e := range evs {
		out[i] = e.GetSeq()
	}
	return out
}
