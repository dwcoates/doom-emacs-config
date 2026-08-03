package db

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"google.golang.org/protobuf/types/known/anypb"
)

func TestReplayFromStreamsLargeHistoryInOrder(t *testing.T) {
	// Arrange: match the observed incident scale closely enough that a future
	// slice-based implementation would again be an obvious architectural
	// regression rather than an optimization hidden by tiny fixtures.
	const eventCount = 3702
	d := openTemp(t)
	events := make([]*corev1.Event, 0, eventCount)
	for range eventCount {
		events = append(events, persistentCore("s1"))
	}
	if _, err := d.Ingest("p", events, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Act: each row is observed through the callback while the query is open.
	var delivered uint64
	stats, err := d.ReplayFrom(context.Background(), "s1", 0, func(ev *corev1.Event) error {
		delivered++
		if ev.GetSeq() != delivered {
			t.Fatalf("streamed seq=%d at position=%d", ev.GetSeq(), delivered)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("ReplayFrom: %v", err)
	}
	if delivered != eventCount || stats.Events != eventCount || stats.FirstSeq != 1 || stats.LastSeq != eventCount {
		t.Fatalf("delivered=%d stats=%+v, want %d events spanning [1,%d]", delivered, stats, eventCount, eventCount)
	}
}

func TestReplayFromDeliversEarlierRowsBeforeALaterDecodeFailure(t *testing.T) {
	// Arrange: seq 1 is valid and seq 2 is deliberately malformed. A
	// materializing implementation would decode the later row before the
	// caller saw seq 1; row-to-callback streaming must expose seq 1 first.
	d := openTemp(t)
	if _, err := d.Ingest("p", []*corev1.Event{persistentCore("s1"), persistentCore("s1")}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if _, err := d.sql.Exec(`UPDATE event SET payload = X'00' WHERE session_id = 's1' AND seq = 2`); err != nil {
		t.Fatalf("corrupting later fixture row: %v", err)
	}

	var seqs []uint64
	stats, err := d.ReplayFrom(context.Background(), "s1", 0, func(ev *corev1.Event) error {
		seqs = append(seqs, ev.GetSeq())
		return nil
	})

	// Assert: the decode error remains loud, after the prior row was delivered.
	if err == nil {
		t.Fatal("ReplayFrom unexpectedly accepted a malformed persisted payload")
	}
	if len(seqs) != 1 || seqs[0] != 1 || stats.Events != 1 || stats.FirstSeq != 1 || stats.LastSeq != 1 {
		t.Fatalf("delivered seqs=%v stats=%+v before failure, want only seq 1", seqs, stats)
	}
}

func TestReplayFromZeroReturnsAll(t *testing.T) {
	// Arrange
	d := openTemp(t)
	if _, err := d.Ingest("p", []*corev1.Event{persistentCore("s1"), persistentCore("s1"), persistentCore("s1")}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	// Act
	got := collectReplay(t, d, "s1", 0)
	// Assert
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
	got := collectReplay(t, d, "s1", 1)
	// Assert
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
	got := collectReplay(t, d, "a", 0)
	// Assert
	if len(got) != 1 || got[0].GetSessionId() != "a" {
		t.Fatalf("replay for session a returned %d events (want 1 for 'a')", len(got))
	}
}

func TestReplayPreservesResultCacheUsage(t *testing.T) {
	// Arrange: cache usage lives inside the opaque vendor payload rather than
	// indexed store columns. Exercise the complete protobuf -> SQLite ->
	// protobuf path so persistence cannot silently shed either top-level or
	// whole-tree model counters.
	d := openTemp(t)
	stream := &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{
			Usage: &datav1.Usage{
				InputTokens:              11,
				OutputTokens:             13,
				CacheCreationInputTokens: 17,
				CacheReadInputTokens:     19,
			},
			ModelUsage: map[string]*datav1.ModelUsage{
				"claude-opus": {
					InputTokens:              23,
					OutputTokens:             29,
					CacheCreationInputTokens: 31,
					CacheReadInputTokens:     37,
					CostUsd:                  0.42,
				},
			},
		}},
	}
	vendor, err := anypb.New(stream)
	if err != nil {
		t.Fatalf("packing cache-bearing result: %v", err)
	}
	event := &corev1.Event{
		SessionId: "cache-session",
		Plane:     corev1.Plane_PLANE_STREAM,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Payload:   &corev1.Event_Vendor{Vendor: vendor},
	}
	if _, err := d.Ingest("claude-shim", []*corev1.Event{event}, nil); err != nil {
		t.Fatalf("persisting cache-bearing result: %v", err)
	}

	// Act
	got := collectReplay(t, d, "cache-session", 0)

	// Assert
	if len(got) != 1 {
		t.Fatalf("replayed %d cache-bearing events, want 1", len(got))
	}
	replayed := new(datav1.ClaudeStreamMessage)
	if err := got[0].GetVendor().UnmarshalTo(replayed); err != nil {
		t.Fatalf("unpacking replayed result: %v", err)
	}
	result := replayed.GetResult()
	usage := result.GetUsage()
	if usage.GetInputTokens() != 11 || usage.GetOutputTokens() != 13 ||
		usage.GetCacheCreationInputTokens() != 17 || usage.GetCacheReadInputTokens() != 19 {
		t.Fatalf("replayed top-level usage = %+v, want all cache and token counters", usage)
	}
	model := result.GetModelUsage()["claude-opus"]
	if model.GetInputTokens() != 23 || model.GetOutputTokens() != 29 ||
		model.GetCacheCreationInputTokens() != 31 || model.GetCacheReadInputTokens() != 37 ||
		model.GetCostUsd() != 0.42 {
		t.Fatalf("replayed model usage = %+v, want all cache, token, and cost counters", model)
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

func TestOpenTaskStartsReturnsOnlyPersistedStartsWithoutAnyEnd(t *testing.T) {
	d := openTemp(t)
	events := []*corev1.Event{
		taskStarted("s1", "live", 100),
		taskStarted("s1", "closed", 110),
		terminalTaskEnded("s1", "closed", corev1.TerminalStatus_TERMINAL_STATUS_DONE, ""),
		terminalTaskEnded("s1", "orphan", corev1.TerminalStatus_TERMINAL_STATUS_LOST, "task-lost:orphan"),
		taskStarted("s1", "live", 120), // duplicate start; recovery returns one.
		taskProgress("s1", "live", 125),
		taskStarted("s2", "other", 130),
	}
	if _, err := d.Ingest("p", events, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	got, err := d.OpenTasks()
	if err != nil {
		t.Fatalf("OpenTasks: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("open task starts = %d, want 2: %+v", len(got), got)
	}
	first := got[0].GetStarted()
	if first.GetSessionId() != "s1" || first.GetTaskStarted().GetTaskId() != "live" || first.GetProducedAtMs() != 100 {
		t.Fatalf("first recovered open task = %+v, want earliest s1/live start", got[0])
	}
	if got[0].GetLastActivityAtMs() != 125 {
		t.Fatalf("live last_activity_at_ms = %d, want task progress at 125", got[0].GetLastActivityAtMs())
	}
	second := got[1].GetStarted()
	if second.GetSessionId() != "s2" || second.GetTaskStarted().GetTaskId() != "other" {
		t.Fatalf("second recovered open task = %+v, want s2/other", got[1])
	}
}

func taskStarted(session, taskID string, at int64) *corev1.Event {
	return &corev1.Event{
		SessionId: session, Plane: corev1.Plane_PLANE_FILE,
		Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, ProducedAtMs: at,
		Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
			TaskId: taskID, Kind: corev1.TaskKind_TASK_KIND_SHELL, OutputPath: "/tmp/" + taskID,
		}},
	}
}

func taskProgress(session, taskID string, at int64) *corev1.Event {
	return &corev1.Event{
		SessionId: session, Plane: corev1.Plane_PLANE_FILE,
		Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, ProducedAtMs: at,
		Payload: &corev1.Event_TaskProgress{TaskProgress: &corev1.TaskProgress{
			TaskId: taskID, Kind: corev1.TaskKind_TASK_KIND_SHELL,
		}},
	}
}

func terminalTaskEnded(session, taskID string, status corev1.TerminalStatus, dedupKey string) *corev1.Event {
	return &corev1.Event{
		SessionId: session, Plane: corev1.Plane_PLANE_SYNTHETIC,
		Class: corev1.EventClass_EVENT_CLASS_PERSISTENT, ProducedAtMs: 200,
		DedupKey: dedupKey,
		Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{
			TaskId: taskID, Kind: corev1.TaskKind_TASK_KIND_SHELL, Status: status,
		}},
	}
}

func seqs(evs []*corev1.Event) []uint64 {
	out := make([]uint64, len(evs))
	for i, e := range evs {
		out[i] = e.GetSeq()
	}
	return out
}
