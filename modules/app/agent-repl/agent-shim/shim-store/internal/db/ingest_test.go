package db

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func TestIngestAssignsGaplessSeq(t *testing.T) {
	// Arrange
	d := openTemp(t)
	events := []*corev1.Event{
		persistentCore("s1"), persistentCore("s1"), persistentCore("s1"),
	}
	// Act
	res, err := d.Ingest("p", events, nil)
	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.Accepted != 3 || res.Deduped != 0 || res.LastSeq != 3 {
		t.Fatalf("result = %+v, want accepted=3 deduped=0 last_seq=3", res)
	}
	for i, ev := range events {
		if ev.GetSeq() != uint64(i+1) {
			t.Fatalf("event[%d] seq = %d, want %d", i, ev.GetSeq(), i+1)
		}
	}
}

func TestIngestSeparateSessionsSeqIndependently(t *testing.T) {
	// Arrange
	d := openTemp(t)
	events := []*corev1.Event{persistentCore("a"), persistentCore("b"), persistentCore("a")}
	// Act
	if _, err := d.Ingest("p", events, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Assert
	if got := []uint64{events[0].GetSeq(), events[1].GetSeq(), events[2].GetSeq()}; got[0] != 1 || got[1] != 1 || got[2] != 2 {
		t.Fatalf("seqs = %v, want [1 1 2] (per-session)", got)
	}
}

func TestIngestDedupsWithinBatch(t *testing.T) {
	// Arrange: the same uuid twin twice in one batch.
	d := openTemp(t)
	events := []*corev1.Event{streamAssistant(t, "s1", "U"), diskAssistant(t, "s1", "U")}
	// Act
	res, err := d.Ingest("p", events, nil)
	// Assert
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if res.Accepted != 1 || res.Deduped != 1 {
		t.Fatalf("result = %+v, want accepted=1 deduped=1", res)
	}
	if events[0].GetSeq() != 1 {
		t.Fatalf("first (winner) seq = %d, want 1", events[0].GetSeq())
	}
	if events[1].GetSeq() != 0 {
		t.Fatalf("second (loser) seq = %d, want 0 (unpersisted)", events[1].GetSeq())
	}
}

func TestIngestDedupIsIdempotentAcrossBatches(t *testing.T) {
	// Arrange: crash-replay — the identical batch written twice.
	d := openTemp(t)
	batch := func() []*corev1.Event {
		return []*corev1.Event{streamAssistant(t, "s1", "A"), streamAssistant(t, "s1", "B")}
	}
	// Act
	r1, err := d.Ingest("p", batch(), nil)
	if err != nil {
		t.Fatalf("first Ingest: %v", err)
	}
	r2, err := d.Ingest("p", batch(), nil)
	if err != nil {
		t.Fatalf("second Ingest: %v", err)
	}
	// Assert
	if r1.Accepted != 2 || r1.Deduped != 0 {
		t.Fatalf("first result = %+v, want accepted=2 deduped=0", r1)
	}
	if r2.Accepted != 0 || r2.Deduped != 2 {
		t.Fatalf("second result = %+v, want accepted=0 deduped=2 (fully deduped)", r2)
	}
	if max, _ := d.MaxSeq("s1"); max != 2 {
		t.Fatalf("MaxSeq after replay = %d, want 2 (no gap growth)", max)
	}
}

func TestIngestCommitsCursorAtomically(t *testing.T) {
	// Arrange
	d := openTemp(t)
	cur := &corev1.CursorState{FileId: "1:2", Path: "/x.jsonl", Offset: 42, Carry: []byte("tail")}
	// Act
	if _, err := d.Ingest("sidecar", []*corev1.Event{persistentCore("s1")}, cur); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Assert: both the event and the cursor landed.
	got, err := d.Cursor("1:2")
	if err != nil {
		t.Fatalf("Cursor: %v", err)
	}
	if got == nil || got.GetOffset() != 42 || string(got.GetCarry()) != "tail" {
		t.Fatalf("cursor = %+v, want offset=42 carry=tail", got)
	}
}

func TestIngestRejectsEphemeral(t *testing.T) {
	// Arrange
	d := openTemp(t)
	eph := &corev1.Event{
		SessionId: "s1",
		Class:     corev1.EventClass_EVENT_CLASS_EPHEMERAL,
		Payload:   &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "x"}},
	}
	// Act
	_, err := d.Ingest("p", []*corev1.Event{eph}, nil)
	// Assert
	if err == nil {
		t.Fatal("expected Ingest to reject an EPHEMERAL event, got nil")
	}
}

func TestIngestRejectsEmptySession(t *testing.T) {
	// Arrange
	d := openTemp(t)
	// Act
	_, err := d.Ingest("p", []*corev1.Event{persistentCore("")}, nil)
	// Assert
	if err == nil {
		t.Fatal("expected Ingest to reject an empty session_id, got nil")
	}
}

func TestIngestExtractsColumns(t *testing.T) {
	// Arrange
	d := openTemp(t)
	// Act
	if _, err := d.Ingest("p", []*corev1.Event{taskEnded("s1", "a1234"), streamAssistant(t, "s1", "U7")}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Assert: task_id and uuid columns were extracted and are queryable.
	byTask, err := d.EventsByTask("s1", "a1234")
	if err != nil {
		t.Fatalf("EventsByTask: %v", err)
	}
	if len(byTask) != 1 {
		t.Fatalf("EventsByTask returned %d rows, want 1", len(byTask))
	}
	var uuidCount int
	if err := d.sql.QueryRow(`SELECT COUNT(*) FROM event WHERE session_id='s1' AND uuid='U7'`).Scan(&uuidCount); err != nil {
		t.Fatalf("querying uuid column: %v", err)
	}
	if uuidCount != 1 {
		t.Fatalf("uuid column rows = %d, want 1", uuidCount)
	}
}
