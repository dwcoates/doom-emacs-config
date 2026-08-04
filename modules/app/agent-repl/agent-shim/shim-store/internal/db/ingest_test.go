package db

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"path/filepath"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/logging"
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

func TestSyntheticLostDedupsWithoutSuppressingARealTerminal(t *testing.T) {
	d := openTemp(t)
	lost := func() *corev1.Event {
		return terminalTaskEnded("s1", "task-1", corev1.TerminalStatus_TERMINAL_STATUS_LOST, "task-lost:task-1")
	}
	first, err := d.Ingest("sidecar", []*corev1.Event{lost()}, nil)
	if err != nil {
		t.Fatalf("first LOST Ingest: %v", err)
	}
	repeated, err := d.Ingest("sidecar", []*corev1.Event{lost()}, nil)
	if err != nil {
		t.Fatalf("repeated LOST Ingest: %v", err)
	}
	real, err := d.Ingest("stream", []*corev1.Event{
		terminalTaskEnded("s1", "task-1", corev1.TerminalStatus_TERMINAL_STATUS_DONE, ""),
	}, nil)
	if err != nil {
		t.Fatalf("real terminal Ingest: %v", err)
	}
	if first.Accepted != 1 || repeated.Deduped != 1 || real.Accepted != 1 {
		t.Fatalf("results first=%+v repeated=%+v real=%+v; want accepted, deduped, accepted", first, repeated, real)
	}
	if max, err := d.MaxSeq("s1"); err != nil || max != 2 {
		t.Fatalf("MaxSeq = %d, err=%v; want 2", max, err)
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

func TestIngestLogsRejectedTransactionWithStoreContext(t *testing.T) {
	path := filepath.Join(t.TempDir(), "events.db")
	var logs bytes.Buffer
	d, err := Open(path, logging.New(&logs, io.Discard, false).With(logging.Fields{
		Component: "db", DatabasePath: path, Table: "event",
	}))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { d.Close() })

	_, err = d.Ingest("sidecar", []*corev1.Event{{
		SessionId: "s1",
		Class:     corev1.EventClass_EVENT_CLASS_EPHEMERAL,
	}}, nil)
	if err == nil {
		t.Fatal("expected ephemeral event rejection")
	}

	lines := bytes.Split(bytes.TrimSpace(logs.Bytes()), []byte("\n"))
	var record struct {
		Level     string         `json:"level"`
		Operation string         `json:"operation"`
		Context   map[string]any `json:"context"`
	}
	if err := json.Unmarshal(lines[len(lines)-1], &record); err != nil {
		t.Fatalf("rejection record is not JSON: %v", err)
	}
	if record.Level != "error" || record.Operation != "ingest" || record.Context["db"] != path || record.Context["table"] != "event" || record.Context["producer"] != "sidecar" || record.Context["transaction"] != "BEGIN IMMEDIATE" {
		t.Fatalf("rejection record lacks canonical store context: %#v", record)
	}
}

func TestKindOfNamesFilePlaneDiagnostic(t *testing.T) {
	ev := &corev1.Event{
		Payload: &corev1.Event_FilePlaneDiagnostic{
			FilePlaneDiagnostic: &corev1.FilePlaneDiagnostic{},
		},
	}
	if got := kindOf(ev); got != "FilePlaneDiagnostic" {
		t.Fatalf("kindOf(FilePlaneDiagnostic) = %q, want FilePlaneDiagnostic", got)
	}
}

func TestKindOfNamesTurnClaimBridge(t *testing.T) {
	ev := &corev1.Event{
		Payload: &corev1.Event_TurnClaimBridge{
			TurnClaimBridge: &corev1.TurnClaimBridge{},
		},
	}
	if got := kindOf(ev); got != "TurnClaimBridge" {
		t.Fatalf("kindOf(TurnClaimBridge) = %q, want TurnClaimBridge", got)
	}
}

func TestKindOfNamesEveryObservabilityPayload(t *testing.T) {
	tests := []struct {
		name  string
		event *corev1.Event
	}{
		{"MessageLatency", &corev1.Event{Payload: &corev1.Event_MessageLatency{MessageLatency: &corev1.MessageLatency{}}}},
		{"ContextCleared", &corev1.Event{Payload: &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}}}},
		{"ContextCompacted", &corev1.Event{Payload: &corev1.Event_ContextCompacted{ContextCompacted: &corev1.ContextCompacted{}}}},
		{"QueryLifecycle", &corev1.Event{Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{}}}},
		{"AccountUsageObservation", &corev1.Event{Payload: &corev1.Event_AccountUsageObservation{AccountUsageObservation: &corev1.AccountUsageObservation{}}}},
		{"SessionRewound", &corev1.Event{Payload: &corev1.Event_SessionRewound{SessionRewound: &corev1.SessionRewound{}}}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			if got := kindOf(test.event); got != test.name {
				t.Fatalf("kindOf(%s) = %q, want %q", test.name, got, test.name)
			}
		})
	}
}

// A rewind is found by an envelope query on the `kind` column, so persisting it
// as "Unknown" would hide the vendor-session lineage the store is meant to keep
// reconstructable on its own.
func TestIngestPersistsSessionRewoundKindColumn(t *testing.T) {
	// Arrange
	d := openTemp(t)
	ev := &corev1.Event{
		SessionId: "s-new",
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Plane:     corev1.Plane_PLANE_STREAM,
		Payload: &corev1.Event_SessionRewound{SessionRewound: &corev1.SessionRewound{
			PreviousVendorSessionId: "s-old",
			NewVendorSessionId:      "s-new",
			RetainedLeafUuid:        "L1",
		}},
	}
	// Act
	if _, err := d.Ingest("shim", []*corev1.Event{ev}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Assert
	var kind string
	if err := d.sql.QueryRow(`SELECT kind FROM event WHERE session_id='s-new'`).Scan(&kind); err != nil {
		t.Fatalf("querying kind column: %v", err)
	}
	if kind != "SessionRewound" {
		t.Fatalf("kind column = %q, want SessionRewound", kind)
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

// --- concurrent writers -----------------------------------------------------

// One shim-store process serves every live shim, and each connection's writes
// run on their own goroutine over their own pooled SQL connection — so batches
// from different sessions genuinely ingest concurrently. A rejected batch is
// PERMANENT data loss (the shim's store-client drops it: no spill, no retry),
// so a transient lock conflict must never surface as an ingest error.
func TestConcurrentIngestNeverRejectsABatch(t *testing.T) {
	// Arrange: 8 writers × 20 batches, the shape of a daemon restart where
	// every reattached shim replays at once.
	d := openTemp(t)
	const writers, batches = 8, 20

	// Act
	errs := make(chan error, writers*batches)
	var wg sync.WaitGroup
	for w := 0; w < writers; w++ {
		wg.Add(1)
		go func(w int) {
			defer wg.Done()
			session := fmt.Sprintf("s%d", w)
			for b := 0; b < batches; b++ {
				if _, err := d.Ingest("p", []*corev1.Event{persistentCore(session)}, nil); err != nil {
					errs <- err
				}
			}
		}(w)
	}
	wg.Wait()
	close(errs)

	// Assert: not one batch lost.
	var got []error
	for err := range errs {
		got = append(got, err)
	}
	if len(got) > 0 {
		t.Fatalf("%d/%d batches rejected under concurrency, first: %v", len(got), writers*batches, got[0])
	}
}

// A writer must WAIT for a busy database rather than fail: busy_timeout only
// applies when the write lock is taken at BEGIN, which is what the DSN's
// immediate txlock buys. A deferred transaction that reads first and upgrades
// later gets SQLITE_BUSY_SNAPSHOT with the busy handler never invoked.
func TestConcurrentIngestKeepsSeqGapless(t *testing.T) {
	// Arrange: many writers on ONE session, so every batch contends for the
	// same MAX(seq) read and the same insert.
	d := openTemp(t)
	const writers, batches = 8, 20

	// Act
	var wg sync.WaitGroup
	for w := 0; w < writers; w++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for b := 0; b < batches; b++ {
				if _, err := d.Ingest("p", []*corev1.Event{persistentCore("shared")}, nil); err != nil {
					t.Errorf("Ingest: %v", err)
					return
				}
			}
		}()
	}
	wg.Wait()

	// Assert: every event landed, with a gapless 1..N sequence.
	var count, maxSeq uint64
	row := d.sql.QueryRow(`SELECT COUNT(*), COALESCE(MAX(seq), 0) FROM event WHERE session_id = 'shared'`)
	if err := row.Scan(&count, &maxSeq); err != nil {
		t.Fatalf("scan: %v", err)
	}
	if want := uint64(writers * batches); count != want || maxSeq != want {
		t.Fatalf("count=%d max_seq=%d, want both %d (gapless, nothing lost)", count, maxSeq, want)
	}
}
