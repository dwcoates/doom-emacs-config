package handler

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func corpusRoot(t *testing.T) string {
	t.Helper()
	dir, _ := os.Getwd()
	for {
		cand := filepath.Join(dir, "testdata", "corpus")
		if fi, err := os.Stat(cand); err == nil && fi.IsDir() {
			return cand
		}
		p := filepath.Dir(dir)
		if p == dir {
			t.Fatalf("corpus not found")
		}
		dir = p
	}
}

// frameFor decodes the first line of a corpus fixture into a JSONL frame.
func frameFor(t *testing.T, rel string) tail.Frame {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(corpusRoot(t), rel))
	if err != nil {
		t.Fatalf("read %s: %v", rel, err)
	}
	for _, ln := range strings.Split(string(data), "\n") {
		ln = strings.TrimSpace(ln)
		if ln == "" {
			continue
		}
		var obj map[string]any
		if err := json.Unmarshal([]byte(ln), &obj); err != nil {
			continue
		}
		return tail.Frame{Obj: obj, Raw: []byte(ln)}
	}
	t.Fatalf("no decodable line in %s", rel)
	return tail.Frame{}
}

func quietLog(string, ...any) {}

// findTaskStarted returns the first TaskStarted payload in events, or nil.
func findTaskStarted(evs []*corev1.Event) *corev1.TaskStarted {
	for _, e := range evs {
		if ts := e.GetTaskStarted(); ts != nil {
			return ts
		}
	}
	return nil
}

func TestSessionHandlerAgentLaunchTwin(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	f := frameFor(t, "tool-results/agent_async_launch.jsonl")
	// Act
	evs := h.Handle([]tail.Frame{f}, &Context{SessionID: "s1", Kind: tail.KindSessionTranscript})
	// Assert: a vendor line twin AND an AGENT TaskStarted.
	ts := findTaskStarted(evs)
	if ts == nil {
		t.Fatalf("no TaskStarted emitted; got %d events", len(evs))
	}
	if ts.GetKind() != corev1.TaskKind_TASK_KIND_AGENT {
		t.Fatalf("kind = %v, want AGENT", ts.GetKind())
	}
	if ts.GetTaskId() != "a15b5267244c1360e" {
		t.Fatalf("task_id = %q", ts.GetTaskId())
	}
}

func TestSessionHandlerWorkflowLaunchTwin(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	f := frameFor(t, "tool-results/workflow_launch.jsonl")
	// Act
	evs := h.Handle([]tail.Frame{f}, &Context{SessionID: "s1"})
	// Assert
	ts := findTaskStarted(evs)
	if ts == nil || ts.GetKind() != corev1.TaskKind_TASK_KIND_WORKFLOW {
		t.Fatalf("want WORKFLOW TaskStarted, got %+v", ts)
	}
	if !strings.HasSuffix(ts.GetOutputPath(), "/journal.jsonl") {
		t.Fatalf("output_path = %q, want journal.jsonl suffix", ts.GetOutputPath())
	}
}

func TestSessionHandlerShellLaunchConstructsPath(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	f := frameFor(t, "tool-results/bash-background.jsonl")
	// Act
	evs := h.Handle([]tail.Frame{f}, &Context{SessionID: "s1", SpoolDir: "/tmp/claude-501/slug/s1/tasks"})
	// Assert: SHELL TaskStarted with output_path built from backgroundTaskId.
	ts := findTaskStarted(evs)
	if ts == nil || ts.GetKind() != corev1.TaskKind_TASK_KIND_SHELL {
		t.Fatalf("want SHELL TaskStarted, got %+v", ts)
	}
	want := "/tmp/claude-501/slug/s1/tasks/" + ts.GetTaskId() + ".output"
	if ts.GetOutputPath() != want {
		t.Fatalf("output_path = %q, want %q", ts.GetOutputPath(), want)
	}
}

func TestSessionHandlerTaskStopTwin(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	f := frameFor(t, "tool-results/task_stop.jsonl")
	// Act
	evs := h.Handle([]tail.Frame{f}, &Context{SessionID: "s1"})
	// Assert
	var te *corev1.TaskEnded
	for _, e := range evs {
		if x := e.GetTaskEnded(); x != nil {
			te = x
		}
	}
	if te == nil || te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_STOPPED {
		t.Fatalf("want STOPPED TaskEnded, got %+v", te)
	}
	if te.GetTaskId() != "abaa339795d28ab75" {
		t.Fatalf("task_id = %q", te.GetTaskId())
	}
}

func TestSessionHandlerTurnEndedFromStopHook(t *testing.T) {
	// Arrange
	h := NewSessionTranscriptHandler(quietLog)
	f := frameFor(t, "transcript-lines/system-stop_hook_summary.jsonl")
	// Act
	evs := h.Handle([]tail.Frame{f}, &Context{SessionID: "sess"})
	// Assert: a TurnEnded twin carrying the producer-supplied turn dedup key.
	var turn *corev1.Event
	for _, e := range evs {
		if e.GetTurnEnded() != nil {
			turn = e
		}
	}
	if turn == nil {
		t.Fatalf("no TurnEnded emitted")
	}
	wantKey := "turn:sess:877b7c91-44c1-4dda-95a2-3eeeef4b7fba"
	if turn.GetDedupKey() != wantKey {
		t.Fatalf("dedup_key = %q, want %q", turn.GetDedupKey(), wantKey)
	}
}

func TestSessionHandlerParseFailureBecomesUnparsed(t *testing.T) {
	// Arrange: a frame the codec flagged as a parse error.
	h := NewSessionTranscriptHandler(quietLog)
	bad := tail.Frame{Raw: []byte("not json"), Offset: 128, ParseErr: errors.New("boom")}
	// Act
	evs := h.Handle([]tail.Frame{bad}, &Context{SessionID: "s1", Path: "/x/y.jsonl"})
	// Assert
	if len(evs) != 1 || evs[0].GetUnparsed() == nil {
		t.Fatalf("want one UnparsedEvent, got %+v", evs)
	}
	if evs[0].GetUnparsed().GetByteOffset() != 128 || evs[0].GetUnparsed().GetProducer() != Producer {
		t.Fatalf("unparsed = %+v", evs[0].GetUnparsed())
	}
}

func TestSessionHandlerVendorTwinHasNoDedupKey(t *testing.T) {
	// Arrange: a plain assistant line (store derives its uuid: key itself).
	h := NewSessionTranscriptHandler(quietLog)
	f := frameFor(t, "transcript-lines/assistant.jsonl")
	// Act
	evs := h.Handle([]tail.Frame{f}, &Context{SessionID: "s1"})
	// Assert: the vendor event carries no producer dedup key.
	if len(evs) == 0 || evs[0].GetVendor() == nil {
		t.Fatalf("want a vendor event, got %+v", evs)
	}
	if evs[0].GetDedupKey() != "" {
		t.Fatalf("vendor twin dedup_key = %q, want empty (store derives)", evs[0].GetDedupKey())
	}
}

func TestAgentHandlerEmitsProgress(t *testing.T) {
	// Arrange
	h := NewAgentTranscriptHandler(quietLog)
	f := frameFor(t, "sidechain/agent-aef975b7bc3422d4b.jsonl")
	// Act
	evs := h.Handle([]tail.Frame{f}, &Context{SessionID: "s1", TaskID: "agent-x", RecordsObserved: 7, Kind: tail.KindAgentTranscript})
	// Assert: at least one AGENT TaskProgress carrying the running record count.
	var tp *corev1.TaskProgress
	for _, e := range evs {
		if x := e.GetTaskProgress(); x != nil {
			tp = x
		}
	}
	if tp == nil || tp.GetKind() != corev1.TaskKind_TASK_KIND_AGENT {
		t.Fatalf("want AGENT TaskProgress, got %+v", tp)
	}
	if tp.GetRecordsObserved() != 7 || tp.GetTaskId() != "agent-x" {
		t.Fatalf("progress = %+v", tp)
	}
}

func TestJournalHandlerDedupKeys(t *testing.T) {
	// Arrange: the complete started+result pair.
	h := NewWorkflowJournalHandler(quietLog)
	data, _ := os.ReadFile(filepath.Join(corpusRoot(t), "journals", "complete-journal.jsonl"))
	var frames []tail.Frame
	for _, ln := range strings.Split(string(data), "\n") {
		ln = strings.TrimSpace(ln)
		if ln == "" {
			continue
		}
		var obj map[string]any
		if err := json.Unmarshal([]byte(ln), &obj); err != nil {
			t.Fatalf("json: %v", err)
		}
		frames = append(frames, tail.Frame{Obj: obj})
	}
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1", RunID: "wf_abc", Kind: tail.KindWorkflowJournal})
	// Assert: two vendor journal events with wf:<run>:<key>:<type> dedup keys.
	if len(evs) != 2 {
		t.Fatalf("events = %d, want 2", len(evs))
	}
	if !strings.HasPrefix(evs[0].GetDedupKey(), "wf:wf_abc:v2:") || !strings.HasSuffix(evs[0].GetDedupKey(), ":started") {
		t.Fatalf("started dedup_key = %q", evs[0].GetDedupKey())
	}
	if !strings.HasSuffix(evs[1].GetDedupKey(), ":result") {
		t.Fatalf("result dedup_key = %q", evs[1].GetDedupKey())
	}
}

func TestShellHandlerByteProgress(t *testing.T) {
	// Arrange
	h := NewShellOutputHandler(quietLog)
	frames := []tail.Frame{{Raw: []byte("some output bytes")}}
	// Act
	evs := h.Handle(frames, &Context{SessionID: "s1", TaskID: "b123", BytesObserved: 4096, Kind: tail.KindShellSpool})
	// Assert
	if len(evs) != 1 {
		t.Fatalf("events = %d, want 1", len(evs))
	}
	tp := evs[0].GetTaskProgress()
	if tp == nil || tp.GetKind() != corev1.TaskKind_TASK_KIND_SHELL {
		t.Fatalf("want SHELL TaskProgress, got %+v", tp)
	}
	if tp.GetBytesObserved() != 4096 || tp.GetTaskId() != "b123" {
		t.Fatalf("progress = %+v", tp)
	}
}

func TestShellHandlerNoFramesNoEvents(t *testing.T) {
	// Arrange / Act
	h := NewShellOutputHandler(quietLog)
	evs := h.Handle(nil, &Context{SessionID: "s1"})
	// Assert
	if len(evs) != 0 {
		t.Fatalf("events = %d, want 0", len(evs))
	}
}
