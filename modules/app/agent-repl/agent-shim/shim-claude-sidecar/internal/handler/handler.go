// Package handler is the sidecar's Layer-2 record→event logic (design §7.2):
// pure functions with ZERO IO that turn decoded file records into core.Event
// values. Each handler owns a converter and emits the file-plane twins of the
// records it reads plus the vendor-neutral lifecycle events the design assigns
// to that plane (TaskStarted / TurnEnded / TaskEnded / TaskProgress).
package handler

import (
	"fmt"
	"path/filepath"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/tail"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
	"google.golang.org/protobuf/types/known/structpb"
)

// Producer is the fixed StoreWrite producer identity for the sidecar (§5.2).
const Producer = "shim-claude-sidecar"

// Logf is the loud-logging sink (§12).
type Logf = func(format string, args ...any)

// Kind classifies a watched file (mirrors discover classification).
type Kind int

const (
	KindSessionTranscript Kind = iota // projects/*/<session>.jsonl
	KindAgentTranscript               // .../subagents/agent-*.jsonl (+ a*.output spool)
	KindWorkflowJournal               // .../workflows/wf_*/journal.jsonl (+ w*.output spool)
	KindShellSpool                    // /tmp .../tasks/b*.output
)

// Context carries per-file attribution and the tailer-owned cumulative counters
// a handler needs. It is populated fresh for each batch (counters reflect totals
// THROUGH the current batch).
type Context struct {
	SessionID string // attribution from the file PATH
	Path      string // absolute file path (UnparsedEvent evidence + logging)
	Kind      Kind

	// TaskID identifies the detached task for agent/shell/workflow files.
	TaskID string
	// SpoolDir is the session's /tmp task spool directory, used to construct a
	// background shell task's output_path from its backgroundTaskId.
	SpoolDir string
	// RunID is the workflow run id (from the journal PATH), the wf dedup-key root.
	RunID string

	// RecordsObserved / BytesObserved are cumulative totals through this batch.
	RecordsObserved int64
	BytesObserved   int64
}

// Handler converts a batch of framed records into events (pure; no IO).
type Handler interface {
	Handle(frames []tail.Frame, ctx *Context) []*corev1.Event
}

// nowMillis is the producer wall clock in unix millis (Event.produced_at_ms).
var nowMillis = func() int64 { return time.Now().UnixMilli() }

// vendorEvent wraps a data.v1 vendor message as a PERSISTENT file-plane Event.
// dedupKey is set only where the store cannot derive it (journal/turn); "" lets
// the store derive uuid:/tur: keys itself.
func vendorEvent(sessionID string, vendor proto.Message, extras *structpb.Struct, dedupKey string, log Logf) *corev1.Event {
	a, err := anypb.New(vendor)
	if err != nil {
		// A generated data.v1 message always marshals into Any; a failure is a
		// build-time impossibility, surfaced loudly rather than dropped.
		log("handler: wrapping %T in Any failed: %v", vendor, err)
		return nil
	}
	return &corev1.Event{
		SessionId:    sessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: nowMillis(),
		DedupKey:     dedupKey,
		Extras:       extras,
		Payload:      &corev1.Event_Vendor{Vendor: a},
	}
}

// base builds the common PERSISTENT lifecycle Event scaffold on a plane.
func base(sessionID string, plane corev1.Plane) *corev1.Event {
	return &corev1.Event{
		SessionId:    sessionID,
		Plane:        plane,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: nowMillis(),
	}
}

func taskStartedEvent(sessionID string, plane corev1.Plane, ts *corev1.TaskStarted) *corev1.Event {
	e := base(sessionID, plane)
	e.Payload = &corev1.Event_TaskStarted{TaskStarted: ts}
	return e
}

func taskProgressEvent(sessionID string, plane corev1.Plane, tp *corev1.TaskProgress) *corev1.Event {
	e := base(sessionID, plane)
	e.Payload = &corev1.Event_TaskProgress{TaskProgress: tp}
	return e
}

func taskEndedEvent(sessionID string, plane corev1.Plane, te *corev1.TaskEnded) *corev1.Event {
	e := base(sessionID, plane)
	e.Payload = &corev1.Event_TaskEnded{TaskEnded: te}
	return e
}

func turnEndedEvent(sessionID, dedupKey string, te *corev1.TurnEnded) *corev1.Event {
	e := base(sessionID, corev1.Plane_PLANE_FILE)
	e.DedupKey = dedupKey
	e.Payload = &corev1.Event_TurnEnded{TurnEnded: te}
	return e
}

// unparsedEvent records a record that failed conversion (§5.1). raw is bounded to
// 64KiB per the core.UnparsedEvent contract.
func unparsedEvent(sessionID, path string, offset int64, raw []byte, convErr error) *corev1.Event {
	const cap64 = 64 << 10
	if len(raw) > cap64 {
		raw = raw[:cap64]
	}
	return &corev1.Event{
		SessionId:    sessionID,
		Plane:        corev1.Plane_PLANE_FILE,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: nowMillis(),
		Payload: &corev1.Event_Unparsed{Unparsed: &corev1.UnparsedEvent{
			SourcePath: path,
			ByteOffset: offset,
			Raw:        append([]byte(nil), raw...),
			Error:      convErr.Error(),
			Producer:   Producer,
		}},
	}
}

// shellOutputPath constructs a background shell task's spool output path from its
// backgroundTaskId and the session's spool dir (design §7.2). Returns "" when the
// spool dir is unknown.
func shellOutputPath(spoolDir, backgroundTaskID string) string {
	if spoolDir == "" || backgroundTaskID == "" {
		return ""
	}
	return filepath.Join(spoolDir, backgroundTaskID+".output")
}

// turnDedupKey is the producer-supplied dedup key for a disk turn-end twin
// (§6.4): the store cannot derive it because core.TurnEnded has no stable id.
func turnDedupKey(sessionID, envelopeUUID string) string {
	if envelopeUUID == "" {
		return ""
	}
	return fmt.Sprintf("turn:%s:%s", sessionID, envelopeUUID)
}

// journalDedupKey is the producer-supplied dedup key for a journal record (§6.4):
// wf:<run_id>:<key>:<type>. The run_id comes from the journal PATH.
func journalDedupKey(runID, key, recType string) string {
	return fmt.Sprintf("wf:%s:%s:%s", runID, key, recType)
}
