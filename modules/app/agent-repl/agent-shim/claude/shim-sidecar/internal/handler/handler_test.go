package handler

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
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

var quietLog = logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})

func init() { quietLog.SetDiagnosticSink(func(logging.Diagnostic) {}) }

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

func TestSessionHandlerStopHookRemainsVendorEvidenceOnly(t *testing.T) {
	// Arrange
	var logs []string
	var sink bytes.Buffer
	log := logging.New(&sink, &sink).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(d logging.Diagnostic) { logs = append(logs, d.Message) })
	h := NewSessionTranscriptHandler(log)
	f := frameFor(t, "transcript-lines/system-stop_hook_summary.jsonl")
	// Act
	evs := h.Handle([]tail.Frame{f}, &Context{SessionID: "sess"})
	// Assert: total ingestion keeps the transcript line, while no file-plane
	// lifecycle fact can close a live stream turn.
	if len(evs) != 1 || evs[0].GetVendor() == nil {
		t.Fatalf("events = %+v, want one vendor transcript event", evs)
	}
	for _, e := range evs {
		if e.GetTurnEnded() != nil {
			t.Fatalf("file-plane stop_hook_summary emitted TurnEnded: %+v", e)
		}
	}
	joined := strings.Join(logs, "\n")
	if !strings.Contains(joined, "plane=file") ||
		!strings.Contains(joined, "identity=877b7c91-44c1-4dda-95a2-3eeeef4b7fba") ||
		!strings.Contains(joined, "decision=vendor_only") {
		t.Fatalf("lifecycle decision log = %q", joined)
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

func TestSessionHandlerParseFailureIsWarnBecauseTheBubbleLosesItsStructure(t *testing.T) {
	// Arrange — a parse failure persists only as an UnparsedEvent.
	var seen []logging.Diagnostic
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(d logging.Diagnostic) { seen = append(seen, d) })
	h := NewSessionTranscriptHandler(log)
	bad := tail.Frame{Raw: []byte("not json"), Offset: 128, ParseErr: errors.New("boom")}
	// Act
	h.Handle([]tail.Frame{bad}, &Context{SessionID: "s1", Path: "/x/y.jsonl"})
	// Assert
	var levels []string
	for _, d := range seen {
		if d.Operation == "parse" {
			levels = append(levels, d.Level)
		}
	}
	if len(levels) != 1 || levels[0] != "warn" {
		t.Fatalf("parse-failure levels = %v, want exactly one warn", levels)
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

// --- shell EXIT= marker ------------------------------------------------------

// shellHandle runs the shell handler over one raw batch at a byte offset.
func shellHandle(raw string, offset int64) []*corev1.Event {
	h := NewShellOutputHandler(quietLog)
	return h.Handle([]tail.Frame{{Raw: []byte(raw), Offset: offset}},
		&Context{SessionID: "s1", TaskID: "b123", Path: "/tmp/t/b123.output", Kind: tail.KindShellSpool})
}

// shellTaskEnded returns the TaskEnded a shell batch produced, or nil.
func shellTaskEnded(raw string, offset int64) *corev1.TaskEnded {
	for _, e := range shellHandle(raw, offset) {
		if te := e.GetTaskEnded(); te != nil {
			return te
		}
	}
	return nil
}

func TestShellHandlerExitZeroEndsTheTaskDone(t *testing.T) {
	// Arrange / Act
	te := shellTaskEnded("output line\nEXIT=0\n", 0)
	// Assert
	if te == nil || te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_DONE {
		t.Fatalf("TaskEnded = %+v, want DONE", te)
	}
}

func TestShellHandlerNonZeroExitEndsTheTaskError(t *testing.T) {
	// Arrange / Act
	te := shellTaskEnded("output line\nEXIT=1\n", 0)
	// Assert
	if te == nil || te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_ERROR {
		t.Fatalf("TaskEnded = %+v, want ERROR", te)
	}
}

func TestShellHandlerMarkerNamesItsInference(t *testing.T) {
	// Arrange / Act — so a frontend can tell this from a LOST sweep.
	te := shellTaskEnded("x\nEXIT=0\n", 0)
	// Assert
	if te.GetInference() != "exit-marker" {
		t.Fatalf("inference = %q, want exit-marker", te.GetInference())
	}
}

func TestShellHandlerMarkerCarriesTheShellKind(t *testing.T) {
	// Arrange / Act
	te := shellTaskEnded("x\nEXIT=0\n", 0)
	// Assert
	if te.GetKind() != corev1.TaskKind_TASK_KIND_SHELL {
		t.Fatalf("kind = %v, want SHELL", te.GetKind())
	}
}

func TestShellHandlerMarkerCarriesTheOutputPath(t *testing.T) {
	// Arrange / Act
	te := shellTaskEnded("x\nEXIT=0\n", 0)
	// Assert
	if te.GetOutputPath() != "/tmp/t/b123.output" {
		t.Fatalf("output path = %q", te.GetOutputPath())
	}
}

func TestShellHandlerStillEmitsProgressAlongsideTheMarker(t *testing.T) {
	// Arrange / Act — the terminating batch's bytes still count.
	evs := shellHandle("x\nEXIT=0\n", 0)
	// Assert
	if len(evs) != 2 || evs[0].GetTaskProgress() == nil {
		t.Fatalf("events = %d, want progress then ended", len(evs))
	}
}

func TestShellHandlerMarkerAloneAtFileStartEnds(t *testing.T) {
	// Arrange / Act — a command with NO output at all: a real observed spool
	// is exactly the 7 bytes "EXIT=0\n".
	te := shellTaskEnded("EXIT=0\n", 0)
	// Assert
	if te == nil {
		t.Fatal("a spool that is only the marker must end the task")
	}
}

func TestShellHandlerIgnoresASuffixedExitAssignment(t *testing.T) {
	// Arrange / Act — the common false positive: script output like
	// `BUILD_EXIT=0`, which is 109 of the 128 EXIT=-bearing spools on disk.
	te := shellTaskEnded("running\nBUILD_EXIT=0\n", 0)
	// Assert
	if te != nil {
		t.Fatalf("BUILD_EXIT=0 must not end the task, got %+v", te)
	}
}

func TestShellHandlerIgnoresAMarkerThatIsNotLast(t *testing.T) {
	// Arrange / Act — the marker terminates the spool; mid-batch it is output.
	te := shellTaskEnded("EXIT=0\nmore output\n", 0)
	// Assert
	if te != nil {
		t.Fatalf("a non-final EXIT= must not end the task, got %+v", te)
	}
}

func TestShellHandlerIgnoresANonNumericExitValue(t *testing.T) {
	// Arrange / Act
	te := shellTaskEnded("x\nEXIT=abc\n", 0)
	// Assert
	if te != nil {
		t.Fatalf("EXIT=abc must not end the task, got %+v", te)
	}
}

func TestShellHandlerIgnoresAnEmptyExitValue(t *testing.T) {
	// Arrange / Act — `WEBAPP_TC_EXIT=` shapes appear in real spools.
	te := shellTaskEnded("x\nEXIT=\n", 0)
	// Assert
	if te != nil {
		t.Fatalf("EXIT= with no code must not end the task, got %+v", te)
	}
}

func TestShellHandlerIgnoresAnOverlongExitValue(t *testing.T) {
	// Arrange / Act — an exit code is 0-255; 4+ digits is not the marker.
	te := shellTaskEnded("x\nEXIT=1234\n", 0)
	// Assert
	if te != nil {
		t.Fatalf("EXIT=1234 must not end the task, got %+v", te)
	}
}

func TestShellHandlerIgnoresAnUnterminatedMarker(t *testing.T) {
	// Arrange / Act — no trailing newline means the line may still be growing.
	te := shellTaskEnded("x\nEXIT=0", 0)
	// Assert
	if te != nil {
		t.Fatalf("an unterminated marker must not end the task, got %+v", te)
	}
}

func TestShellHandlerIgnoresAMarkerStartingAMidFileBatch(t *testing.T) {
	// Arrange / Act — mid-file with no newline in the batch, the text may be
	// the tail of a line that began in an earlier batch (so `...FOO_EXIT=0`).
	te := shellTaskEnded("EXIT=0\n", 4096)
	// Assert
	if te != nil {
		t.Fatalf("an unprovable line start must not end the task, got %+v", te)
	}
}

func TestShellHandlerEndsOnAThreeDigitExitCode(t *testing.T) {
	// Arrange / Act — 130 (SIGINT) is a real, in-range code.
	te := shellTaskEnded("x\nEXIT=130\n", 0)
	// Assert
	if te == nil || te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_ERROR {
		t.Fatalf("TaskEnded = %+v, want ERROR", te)
	}
}

// --- shell EXIT= marker, against the real corpus fixtures --------------------

// spoolCorpusRoot walks up to testdata/corpus, the golden fixture set.
func spoolCorpusRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for {
		cand := filepath.Join(dir, "testdata", "corpus")
		if fi, err := os.Stat(cand); err == nil && fi.IsDir() {
			return cand
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("could not locate testdata/corpus above %s", dir)
		}
		dir = parent
	}
}

// handleCorpusSpool feeds a corpus spool file through the shell handler as one
// whole-file batch, the way the tailer's first poll of a finished spool does.
func handleCorpusSpool(t *testing.T, name string) *corev1.TaskEnded {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(spoolCorpusRoot(t), "spools", name))
	if err != nil {
		t.Fatalf("read corpus spool %s: %v", name, err)
	}
	h := NewShellOutputHandler(quietLog)
	evs := h.Handle([]tail.Frame{{Raw: raw, Offset: 0}},
		&Context{SessionID: "s1", TaskID: "b1", Kind: tail.KindShellSpool})
	for _, e := range evs {
		if te := e.GetTaskEnded(); te != nil {
			return te
		}
	}
	return nil
}

func TestShellHandlerCorpusCleanSpoolEndsWithItsRealExitCode(t *testing.T) {
	// Arrange / Act — bash-clean.output really ends "...\n\nEXIT=1\n".
	te := handleCorpusSpool(t, "bash-clean.output")
	// Assert
	if te == nil || te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_ERROR {
		t.Fatalf("TaskEnded = %+v, want ERROR from the fixture's EXIT=1", te)
	}
}

func TestShellHandlerCorpusUnterminatedSpoolDoesNotEnd(t *testing.T) {
	// Arrange / Act — bash-midoutput.output has no terminator at all, which is
	// the shape of the great majority of real spools.
	te := handleCorpusSpool(t, "bash-midoutput.output")
	// Assert
	if te != nil {
		t.Fatalf("an unterminated spool must not end the task, got %+v", te)
	}
}
