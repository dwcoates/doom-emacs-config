package db

import (
	"bytes"
	"encoding/json"
	"io"
	"path/filepath"
	"strings"
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
	if decodeErr := json.Unmarshal(logs.Bytes(), &record); decodeErr != nil {
		t.Fatalf("newer-schema record is not JSON: %v", decodeErr)
	}
	if record.Operation != "migrate" || record.Level != "error" || !strings.Contains(record.Message, "schema migration failed") || record.Context["db"] != path || record.Context["table"] != "schema_meta" {
		t.Fatalf("newer-schema error was not canonically logged with context: %#v", record)
	}
}
