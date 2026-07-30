package db

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/types/known/anypb"
)

// --- shared test helpers ---------------------------------------------------

// openTemp opens a fresh WAL database in a temp dir (exercises real WAL, unlike
// :memory:) and registers cleanup.
func openTemp(t *testing.T) *DB {
	t.Helper()
	path := filepath.Join(t.TempDir(), "events.db")
	d, err := Open(path, logging.New(io.Discard, io.Discard, false))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { d.Close() })
	return d
}

// persistentCore builds a PERSISTENT core-lifecycle event with no dedup
// identity (SessionStarted) — useful for testing gapless seq without dedup.
func persistentCore(session string) *corev1.Event {
	return &corev1.Event{
		SessionId: session,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Plane:     corev1.Plane_PLANE_STREAM,
		Payload:   &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{}},
	}
}

// collectReplay materializes a streamed replay only inside tests that need to
// inspect the complete result. Production has no slice-returning replay API.
func collectReplay(t *testing.T, d *DB, session string, fromSeq uint64) []*corev1.Event {
	t.Helper()
	var events []*corev1.Event
	if _, err := d.ReplayFrom(context.Background(), session, fromSeq, func(ev *corev1.Event) error {
		events = append(events, ev)
		return nil
	}); err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	return events
}

// streamAssistant builds a PERSISTENT stream-plane event whose derived dedup
// key is "uuid:<uuid>".
func streamAssistant(t *testing.T, session, uuid string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Uuid: uuid}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId: session,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Plane:     corev1.Plane_PLANE_STREAM,
		Payload:   &corev1.Event_Vendor{Vendor: a},
	}
}

// diskAssistant is the file-plane twin of streamAssistant (same uuid → same
// derived key → dedup collision).
func diskAssistant(t *testing.T, session, uuid string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_Assistant{Assistant: &datav1.AssistantLine{Envelope: &datav1.LineEnvelope{Uuid: uuid}}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId: session,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Plane:     corev1.Plane_PLANE_FILE,
		Payload:   &corev1.Event_Vendor{Vendor: a},
	}
}

// taskEnded builds a PERSISTENT core TaskEnded event (task_id column extracted).
func taskEnded(session, taskID string) *corev1.Event {
	return &corev1.Event{
		SessionId: session,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Plane:     corev1.Plane_PLANE_STREAM,
		Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{
			TaskId: taskID, Kind: corev1.TaskKind_TASK_KIND_AGENT,
		}},
	}
}

// --- write serialization ---------------------------------------------------

func TestConcurrentIngestsAssignEverySeqExactlyOnce(t *testing.T) {
	// Arrange: BEGIN IMMEDIATE (`_txlock=immediate`, see Open) is what makes
	// concurrent writers mutually exclusive. Ingest reads MAX(seq) and only then
	// inserts, so if that serialization did NOT hold, two transactions would read
	// the same high-water and derive the same candidate — and the loser's
	// `INSERT OR IGNORE` against PRIMARY KEY (session_id, seq) would be silently
	// ignored and then miscounted as a dedup. Every failure mode is therefore
	// observable from outside: a duplicate seq, a gap, a lost event, or an error.
	//
	// SessionStarted carries no dedup identity, so a genuine dedup can never be
	// confused with a seq collision here.
	const writers = 12
	d := openTemp(t)

	// Act: release every writer at once from a channel barrier — no sleeps.
	var ready, done sync.WaitGroup
	ready.Add(writers)
	done.Add(writers)
	start := make(chan struct{})
	results := make([]Result, writers)
	errs := make([]error, writers)
	for i := range writers {
		go func() {
			defer done.Done()
			ready.Done()
			<-start
			results[i], errs[i] = d.Ingest("p", []*corev1.Event{persistentCore("s1")}, nil)
		}()
	}
	ready.Wait()
	close(start)
	done.Wait()

	// Assert: every writer succeeded with its one event, and the assigned seqs are
	// exactly 1..writers with no duplicate and no gap.
	seen := make(map[uint64]int, writers)
	for i := range writers {
		if errs[i] != nil {
			t.Fatalf("writer %d: Ingest failed (write serialization did not hold): %v", i, errs[i])
		}
		if results[i].Accepted != 1 || results[i].Deduped != 0 {
			t.Fatalf("writer %d: accepted=%d deduped=%d, want accepted=1 deduped=0 — a seq collision was miscounted as a dedup",
				i, results[i].Accepted, results[i].Deduped)
		}
		seen[results[i].LastSeq]++
	}
	for seq := uint64(1); seq <= writers; seq++ {
		switch n := seen[seq]; {
		case n == 0:
			t.Fatalf("seq %d was never assigned; assigned set = %v", seq, seen)
		case n > 1:
			t.Fatalf("seq %d was assigned to %d writers; assigned set = %v", seq, n, seen)
		}
	}

	// Assert: the durable rows agree with the acks.
	replayed := collectReplay(t, d, "s1", 0)
	if len(replayed) != writers {
		t.Fatalf("persisted %d events, want %d", len(replayed), writers)
	}
	for i, ev := range replayed {
		if want := uint64(i + 1); ev.GetSeq() != want {
			t.Fatalf("persisted event %d has seq %d, want %d", i, ev.GetSeq(), want)
		}
	}
}

// --- schema / migration tests ----------------------------------------------

func TestOpenSeedsSchemaMeta(t *testing.T) {
	// Arrange / Act
	d := openTemp(t)
	// Assert
	var version int
	if err := d.sql.QueryRow(`SELECT version FROM schema_meta`).Scan(&version); err != nil {
		t.Fatalf("reading schema_meta: %v", err)
	}
	if version != SchemaVersion {
		t.Fatalf("schema_meta version = %d, want %d", version, SchemaVersion)
	}
}

func TestReopenIsIdempotent(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "events.db")
	d1, err := Open(path, logging.New(io.Discard, io.Discard, false))
	if err != nil {
		t.Fatalf("first Open: %v", err)
	}
	d1.Close()
	// Act
	d2, err := Open(path, logging.New(io.Discard, io.Discard, false))
	// Assert
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer d2.Close()
	var version int
	if err := d2.sql.QueryRow(`SELECT version FROM schema_meta`).Scan(&version); err != nil {
		t.Fatalf("reading schema_meta: %v", err)
	}
	if version != SchemaVersion {
		t.Fatalf("version after reopen = %d, want %d", version, SchemaVersion)
	}
}

func TestOpenRejectsNewerSchema(t *testing.T) {
	// Arrange: a database stamped with a future version.
	path := filepath.Join(t.TempDir(), "events.db")
	d, err := Open(path, logging.New(io.Discard, io.Discard, false))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if _, err := d.sql.Exec(`UPDATE schema_meta SET version = ?`, SchemaVersion+1); err != nil {
		t.Fatalf("bumping version: %v", err)
	}
	d.Close()
	// Act
	var logs bytes.Buffer
	_, err = Open(path, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "db", DatabasePath: path}))
	// Assert
	if err == nil {
		t.Fatal("expected Open to reject a newer on-disk schema, got nil")
	}
	var record struct {
		Operation string         `json:"operation"`
		Level     string         `json:"level"`
		Message   string         `json:"message"`
		Context   map[string]any `json:"context"`
	}
	found := false
	for _, line := range bytes.Split(bytes.TrimSpace(logs.Bytes()), []byte("\n")) {
		var candidate struct {
			Operation string         `json:"operation"`
			Level     string         `json:"level"`
			Message   string         `json:"message"`
			Context   map[string]any `json:"context"`
		}
		if decodeErr := json.Unmarshal(line, &candidate); decodeErr != nil {
			t.Fatalf("newer-schema record is not JSON: %v", decodeErr)
		}
		if candidate.Operation == "migrate" && candidate.Level == "error" {
			record = candidate
			found = true
		}
	}
	if !found {
		t.Fatalf("newer-schema error record missing: %s", logs.String())
	}
	if record.Operation != "migrate" || record.Level != "error" || !strings.Contains(record.Message, "schema migration failed") || record.Context["db"] != path || record.Context["table"] != "schema_meta" {
		t.Fatalf("newer-schema error was not canonically logged with context: %#v", record)
	}
}

func TestOpenTasksFailureUsesCanonicalQueryLogger(t *testing.T) {
	path := filepath.Join(t.TempDir(), "events.db")
	var logs bytes.Buffer
	log := logging.New(&logs, io.Discard, false).With(logging.Fields{
		Component:    "db",
		DatabasePath: path,
	})
	d, err := Open(path, log)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := d.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	logs.Reset()

	if _, err := d.OpenTasks(); err == nil {
		t.Fatal("OpenTasks on closed database returned nil error")
	}

	var record struct {
		Operation string         `json:"operation"`
		Level     string         `json:"level"`
		Message   string         `json:"message"`
		Context   map[string]any `json:"context"`
	}
	found := false
	for _, line := range bytes.Split(bytes.TrimSpace(logs.Bytes()), []byte("\n")) {
		if err := json.Unmarshal(line, &record); err != nil {
			t.Fatalf("OpenTasks failure record is not JSON: %v", err)
		}
		if record.Operation == "open-tasks" && record.Level == "error" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("OpenTasks canonical error record missing: %s", logs.String())
	}
	if record.Context["component"] != "db" || record.Context["db"] != path || record.Context["table"] != "event" ||
		!strings.Contains(record.Message, "database query failed") {
		t.Fatalf("OpenTasks error lacks canonical query context: %#v", record)
	}
}
