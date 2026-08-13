package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

type captureWriter struct {
	mu    sync.Mutex
	lines []string
}

func (w *captureWriter) Write(p []byte) (int, error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.lines = append(w.lines, string(p))
	return len(p), nil
}

// capturingLog returns a logger that records every JSON line, plus a reader
// for what it has seen. It is mutex-guarded because the sidecar's logger is.
func capturingLog() (*logging.Bound, func() []string) {
	writer := &captureWriter{}
	logf := logging.New(writer, io.Discard).With(logging.Context{Component: "test"})
	logf.SetDiagnosticSink(func(d logging.Diagnostic) {
		writer.Write([]byte(d.Message))
	})
	return logf, func() []string {
		writer.mu.Lock()
		defer writer.mu.Unlock()
		return append([]string(nil), writer.lines...)
	}
}

// linesContaining filters captured log lines by substring.
func linesContaining(lines []string, sub string) []string {
	var out []string
	for _, l := range lines {
		if strings.Contains(l, sub) {
			out = append(out, l)
		}
	}
	return out
}

// --- spool ownership: one identifier, resolved by task id --------------------
//
// A /tmp spool path carries a session-SHAPED segment that is the harness's
// runtime id, NOT the transcript's. These pin the replacement: a spool is
// attributed from the launching session recorded against its task id, and an
// unattributed spool is held and reported rather than guessed.

// spoolTarget builds an unattributed spool target, exactly as discover now
// classifies one (no SessionID).
func spoolTarget(taskID string) discover.Target {
	return discover.Target{
		Path:   "/tmp/claude-501/slug/runtime-id/tasks/" + taskID + ".output",
		Kind:   tail.KindShellSpool,
		TaskID: taskID,
		Raw:    true,
	}
}

// ownerSidecar is a sidecar with no store, sufficient for the pure attribution
// logic (resolveOwnerResult/noteTaskOwner/seedOwners touch no connection).
func ownerSidecar(t *testing.T) (*sidecar, func() []string) {
	t.Helper()
	logf, read := capturingLog()
	return newSidecar("/nonexistent.sock", nil, t.TempDir(), logf), read
}

func TestResolveOwnerAnswersConfigTargetFromItsOwnPath(t *testing.T) {
	// Arrange — a transcript names its own session; nothing to look up.
	s, _ := ownerSidecar(t)
	tgt := discover.Target{Path: "/x/S1.jsonl", Kind: tail.KindSessionTranscript, SessionID: "S1"}

	// Act
	got := s.resolveOwnerResult(tgt)

	// Assert
	if !got.Resolved() || got.SessionID != "S1" {
		t.Fatalf("resolveOwnerResult = %+v, want resolved S1", got)
	}
}

func TestResolveOwnerAttributesSpoolToItsLaunchingSession(t *testing.T) {
	// Arrange — the transcript's TaskStarted established the mapping.
	s, _ := ownerSidecar(t)
	s.noteTaskOwner("b1pi0nmip", "9b6a4f2d-transcript-id", "", OwnerSourceLiveLaunch)

	// Act
	got := s.resolveOwnerResult(spoolTarget("b1pi0nmip"))

	// Assert — the TRANSCRIPT id, never the path's runtime id.
	if !got.Resolved() || got.SessionID != "9b6a4f2d-transcript-id" {
		t.Fatalf("resolveOwnerResult = %+v, want transcript owner", got)
	}
}

func TestNoteOwnerKeepsTheFirstSessionAndReportsAConflict(t *testing.T) {
	// Arrange — the corruption this change removes upstream, seen again.
	s, read := ownerSidecar(t)
	s.noteTaskOwner("b1pi0nmip", "9b6a4f2d", "", OwnerSourceLiveLaunch)

	// Act
	s.noteTaskOwner("b1pi0nmip", "a4f52dc5", "", OwnerSourceLiveLaunch)

	// Assert — first wins (no flapping), and it is loud.
	if got := s.owners["b1pi0nmip"]; got != "9b6a4f2d" {
		t.Fatalf("owner = %q, want the first-recorded 9b6a4f2d", got)
	}
	if got := linesContaining(read(), "CONFLICTING owner"); len(got) != 1 {
		t.Fatalf("conflict lines = %v, want exactly 1", got)
	}
}

// The seed USED TO let a restart attribute a spool whose launch line sits
// behind the resumed cursor and will never be re-read. `OpenTaskState.started`
// carried the task id, the session and the output path that made that possible,
// and it was retired with no successor.
//
// This pins the honest consequence: the snapshot seeds nothing, and no owner is
// invented from it. The spool is held and reported instead — see
// TestAttributionBacklogRestartPastLaunchHoldsWithoutADurableOwner.
func TestSeedOwnersCannotAttributeFromAPersistedOpenTask(t *testing.T) {
	// Arrange — what the store hands back on every connection, all it can say.
	s, _ := ownerSidecar(t)
	states := []*agentshimv1.OpenTaskState{{LastActivityAtMs: 1}}

	// Act
	n := s.seedOwners(states)

	// Assert
	if n != 0 || len(s.owners) != 0 {
		t.Fatalf("seeded %d owner(s) = %v; OpenTaskState names no task to seed from", n, s.owners)
	}
}

func TestSeedOwnersOfAnEmptySnapshotSeedsNothing(t *testing.T) {
	// Arrange
	s, _ := ownerSidecar(t)

	// Act
	n := s.seedOwners(nil)

	// Assert
	if n != 0 || len(s.owners) != 0 {
		t.Fatalf("seeded %d, owners = %v, want none", n, s.owners)
	}
}

func TestKindLabel(t *testing.T) {
	cases := []struct {
		in   tail.Kind
		want string
	}{
		{tail.KindSessionTranscript, "session"},
		{tail.KindAgentTranscript, "agent"},
		{tail.KindWorkflowJournal, "workflow"},
		{tail.KindShellSpool, "shell"},
		{tail.Kind(42), "kind(42)"},
	}
	for _, tc := range cases {
		if got := kindLabel(tc.in); got != tc.want {
			t.Fatalf("kindLabel(%d) = %q, want %q", int(tc.in), got, tc.want)
		}
	}
}

func TestParseRootsSplitsAndExpandsHome(t *testing.T) {
	// Arrange
	home, _ := os.UserHomeDir()
	// Act
	got := parseRoots(" ~/.claude , ~/.claude-chesscom ,, /abs/root ")
	// Assert: trimmed, blanks dropped, ~ expanded, absolute preserved.
	want := []string{filepath.Join(home, ".claude"), filepath.Join(home, ".claude-chesscom"), "/abs/root"}
	if len(got) != len(want) {
		t.Fatalf("got %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("root[%d] = %q, want %q", i, got[i], want[i])
		}
	}
}

func TestParseRootsEmpty(t *testing.T) {
	// Arrange / Act / Assert
	if got := parseRoots("   "); len(got) != 0 {
		t.Fatalf("got %v, want empty", got)
	}
}

func TestIndexCursorsByPath(t *testing.T) {
	// Arrange
	cs := []*agentshimv1.CursorState{
		{FileId: "1:1", Path: "/a.jsonl", Offset: 10},
		{FileId: "2:2", Path: "/b.jsonl", Offset: 20},
		{FileId: "3:3", Path: ""}, // no path → dropped
	}
	// Act
	m := indexCursorsByPath(cs)
	// Assert
	if len(m) != 2 {
		t.Fatalf("index size = %d, want 2", len(m))
	}
	if m["/a.jsonl"].GetOffset() != 10 || m["/b.jsonl"].GetOffset() != 20 {
		t.Fatalf("index = %+v", m)
	}
}

// What kind of work detached decides which silence window the staleness policy
// applies before calling it LOST. A kind this reader does not recognize gets the
// LONGEST window, because a premature LOST is a wrong verdict the user reads.
func TestDetachedKindToTail(t *testing.T) {
	shell := &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Shell{Shell: &conversationv1.DetachedShell{}}}
	workflow := &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Workflow{Workflow: &conversationv1.DetachedWorkflow{}}}
	agent := &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Agent{Agent: &conversationv1.DetachedAgent{}}}
	skill := &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Skill{Skill: &conversationv1.DetachedSkill{}}}

	cases := []struct {
		name string
		in   *conversationv1.DetachedWorkKind
		want tail.Kind
	}{
		{"shell", shell, tail.KindShellSpool},
		{"workflow", workflow, tail.KindWorkflowJournal},
		{"agent", agent, tail.KindAgentTranscript},
		{"skill falls back to the longest window", skill, tail.KindAgentTranscript},
		{"unstated kind falls back to the longest window", nil, tail.KindAgentTranscript},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := detachedKindToTail(tc.in); got != tc.want {
				t.Fatalf("detachedKindToTail(%v) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}

// The card's message id is derived from the task id, so recovering the task from
// the id is a pure inverse — and a message that is not a card must yield nothing
// rather than a task name taken from a conversation id.
func TestTaskIDFromMessageID(t *testing.T) {
	cases := []struct {
		name      string
		messageID string
		want      string
	}{
		{"a detached-work card", "dw:agent-1", "agent-1"},
		{"an ordinary message", "b7e3-uuid", ""},
		{"an empty id", "", ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := taskIDFromMessageID(tc.messageID); got != tc.want {
				t.Fatalf("taskIDFromMessageID(%q) = %q, want %q", tc.messageID, got, tc.want)
			}
		})
	}
}

func TestBootTimeMillisIsPast(t *testing.T) {
	// Act
	boot := bootTimeMillis()
	// Assert: either unavailable (0) or a plausible past instant.
	if boot != 0 && boot > time.Now().UnixMilli() {
		t.Fatalf("boot time %d is in the future", boot)
	}
}

func TestExpandHomeLeavesAbsolute(t *testing.T) {
	if got := expandHome("/absolute/path"); got != "/absolute/path" {
		t.Fatalf("expandHome mangled an absolute path: %q", got)
	}
}

func TestOpenLoggerReturnsBootstrapErrorBeforePersistentSinkExists(t *testing.T) {
	parent := t.TempDir()
	blocked := filepath.Join(parent, "blocked")
	if err := os.WriteFile(blocked, []byte("not a directory"), 0o600); err != nil {
		t.Fatal(err)
	}

	_, _, err := openLogger("/tmp/store.sock", filepath.Join(blocked, "sidecar.log"))
	if err == nil {
		t.Fatal("openLogger succeeded with a non-directory parent")
	}
	if !isBootstrapError(err) {
		t.Fatalf("error %T = %v, want bootstrap error", err, err)
	}
	var stderr bytes.Buffer
	reportFatal(err, &stderr)
	var record map[string]any
	if decodeErr := json.Unmarshal(stderr.Bytes(), &record); decodeErr != nil {
		t.Fatalf("bootstrap failure is not JSON: %v: %q", decodeErr, stderr.String())
	}
	if record["operation"] != "sidecar.bootstrap" || record["level"] != "error" {
		t.Fatalf("bootstrap failure record = %#v", record)
	}
	stderr.Reset()
	reportFatal(errors.New("runtime failure"), &stderr)
	if stderr.Len() != 0 {
		t.Fatalf("post-bootstrap failure bypassed canonical logger: %q", stderr.String())
	}
}

func TestRunLoggedRecordsPostBootstrapErrorExactlyOnce(t *testing.T) {
	var stderr, file bytes.Buffer
	log := logging.New(&stderr, &file).With(logging.Context{Component: "sidecar", StoreSocket: "/tmp/store.sock"})
	want := errors.New("close failed")

	err := runLogged(log, func() error { return want })
	if !errors.Is(err, want) {
		t.Fatalf("runLogged error = %v, want %v", err, want)
	}
	for sink, got := range map[string]string{"stderr": stderr.String(), "file": file.String()} {
		if count := strings.Count(got, "sidecar stopped with error: close failed"); count != 1 {
			t.Fatalf("%s error record count = %d, output=%q", sink, count, got)
		}
		var record struct {
			Operation string         `json:"operation"`
			Context   map[string]any `json:"context"`
		}
		if err := json.Unmarshal([]byte(got), &record); err != nil {
			t.Fatalf("%s record is not JSON: %v", sink, err)
		}
		if record.Operation != "run" || record.Context["store_socket"] != "/tmp/store.sock" {
			t.Fatalf("%s missing canonical runtime context: %#v", sink, record)
		}
	}
}

func TestLogProcessExitNamesCleanOrErrorExit(t *testing.T) {
	// Arrange
	for _, tc := range []struct {
		name        string
		err         error
		wantLevel   string
		wantMessage string
	}{
		{name: "clean", err: nil, wantLevel: "info", wantMessage: "sidecar exiting cleanly"},
		{name: "error", err: errors.New("run failed"), wantLevel: "error", wantMessage: "sidecar exiting: run failed"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var stderr, file bytes.Buffer
			log := logging.New(&stderr, &file).With(logging.Context{Component: "sidecar"})

			// Act
			logProcessExit(log, &tc.err)

			// Assert
			var record struct {
				Level     string `json:"level"`
				Operation string `json:"operation"`
				Message   string `json:"message"`
			}
			if err := json.Unmarshal(file.Bytes(), &record); err != nil {
				t.Fatalf("exit trace is not JSON: %v: %q", err, file.String())
			}
			if record.Operation != "exit" || record.Level != tc.wantLevel || record.Message != tc.wantMessage {
				t.Fatalf("exit trace = %#v, want operation=exit level=%q message=%q", record, tc.wantLevel, tc.wantMessage)
			}
		})
	}
}

// TestLogProcessExitLogsThenRepanics proves the exit trace narrates a panic
// without recovering it: logProcessExit must remain deferred directly (not
// wrapped) for its own recover() to observe the panic, so this drives it
// through a real deferred panic rather than calling it as a plain function.
func TestLogProcessExitLogsThenRepanics(t *testing.T) {
	// Arrange
	var stderr, file bytes.Buffer
	log := logging.New(&stderr, &file).With(logging.Context{Component: "sidecar"})
	var recovered any

	// Act
	func() {
		defer func() { recovered = recover() }()
		func() {
			var err error
			defer logProcessExit(log, &err)
			panic("invariant violated")
		}()
	}()

	// Assert
	if recovered != "invariant violated" {
		t.Fatalf("re-panicked value = %v, want the original panic to survive the trace", recovered)
	}
	var record struct {
		Level   string `json:"level"`
		Message string `json:"message"`
	}
	if err := json.Unmarshal(file.Bytes(), &record); err != nil {
		t.Fatalf("panic exit trace is not JSON: %v: %q", err, file.String())
	}
	if record.Level != "error" || record.Message != "sidecar exiting: panic: invariant violated" {
		t.Fatalf("panic exit trace = %#v", record)
	}
}

// TestRunReturnsOnSignalAndNamesWhichOne drives Run's stop branch with a real
// (never-dialed) storeclient — Close() on one is a documented no-op success —
// to prove the shutdown cause and the received signal both reach the log
// before the one teardown step (the store connection) runs.
func TestRunReturnsOnSignalAndNamesWhichOne(t *testing.T) {
	// Arrange
	logf, lines := capturingLog()
	sc := newSidecar("/tmp/agent-repl-test-nonexistent.sock", nil, t.TempDir(), logf)
	stop := make(chan os.Signal, 1)
	stop <- syscall.SIGTERM

	// Act
	err := sc.Run(stop)

	// Assert
	if err != nil {
		t.Fatalf("Run() error = %v, want nil (a never-dialed store close succeeds)", err)
	}
	want := "received signal=" + syscall.SIGTERM.String() + "; beginning sidecar shutdown"
	if got := linesContaining(lines(), want); len(got) != 1 {
		t.Fatalf("shutdown-cause line count = %d, want 1 containing %q; lines=%v", len(got), want, lines())
	}
}
