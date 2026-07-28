package main

// A one-off, opt-in repair: close ONE stale open task in the LIVE shim-store
// through the store's own producer wire, so seq assignment, dedup and fan-out
// all apply exactly as they do for any producer. No DB surgery.
//
// WHY THIS EXISTS. stale.Tracker.Restore treats a task_id that is open under
// two session ids as a fatal recovery error, which takes the whole sidecar
// file plane down (it dial-loops, reading nothing, so no transcript reaches
// the GUI). One conversation observed under two session ids can produce
// exactly that, and the store has no other way to retract the duplicate.
//
// WHY A TEST. It needs the store module's resolved deps and its wire package;
// a scratch module cannot build offline (no go.sum). It is skipped unless
// REPAIR_TASK_ID and REPAIR_SESSION are set, so `go test ./...` never runs it:
//
//	REPAIR_TASK_ID=b1pi0nmip REPAIR_SESSION=<uuid> go test -run TestManualRepair -v .
//
// The event mirrors stale.Tracker.lost, including its "task-lost:<id>" dedup
// key — safe because dedup is scoped (session_id, dedup_key), so this cannot
// mask a later genuine sweep of another session's copy.

import (
	"net"
	"os"
	"path/filepath"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

func TestManualRepairClosesDuplicateOpenTask(t *testing.T) {
	taskID := os.Getenv("REPAIR_TASK_ID")
	session := os.Getenv("REPAIR_SESSION")
	if taskID == "" || session == "" {
		t.Skip("set REPAIR_TASK_ID and REPAIR_SESSION to run the live-store repair")
	}
	sock := os.Getenv("REPAIR_SOCKET")
	if sock == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			t.Fatalf("resolving home: %v", err)
		}
		sock = filepath.Join(home, ".cache", "agent-repl", "sock", "store.sock")
	}

	conn, err := net.Dial("unix", sock)
	if err != nil {
		t.Fatalf("dialing store at %s: %v", sock, err)
	}
	defer conn.Close()

	ev := &corev1.Event{
		SessionId:    session,
		Plane:        corev1.Plane_PLANE_SYNTHETIC,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		DedupKey:     "task-lost:" + taskID,
		Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{
			TaskId:    taskID,
			Kind:      corev1.TaskKind_TASK_KIND_AGENT,
			Status:    corev1.TerminalStatus_TERMINAL_STATUS_LOST,
			Inference: "manual-duplicate-repair",
		}},
	}
	if err := wire.WriteAny(conn, &corev1.StoreWrite{
		Producer: "store-repair",
		Batch:    &corev1.EventBatch{Events: []*corev1.Event{ev}},
	}); err != nil {
		t.Fatalf("writing repair event: %v", err)
	}

	msg, err := wire.ReadAny(conn)
	if err != nil {
		t.Fatalf("reading ack: %v", err)
	}
	ack, ok := msg.(*corev1.StoreWriteAck)
	if !ok {
		t.Fatalf("ack = %T, want *corev1.StoreWriteAck", msg)
	}
	if ack.GetError() != "" {
		t.Fatalf("store rejected the repair: %s", ack.GetError())
	}
	t.Logf("repair accepted: task=%s session=%s accepted=%d deduped=%d last_seq=%d",
		taskID, session, ack.GetAccepted(), ack.GetDeduped(), ack.GetLastSeq())
}
