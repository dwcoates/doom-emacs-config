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
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
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

// pickupSidecar wires a live store, a 5-line transcript, and a log-capturing
// sidecar with its link established — the arrangement both pickup tests share.
func pickupSidecar(t *testing.T) (*sidecar, string, func() []string) {
	t.Helper()
	h := newStoreHarness(t)
	h.start()
	root, path := writeHistory(t, 5)
	logf, read := capturingLog()
	s := newSidecar(h.sock, []string{root}, t.TempDir(), logf)
	t.Cleanup(func() { s.store.Close() })
	if err := s.establish(); err != nil {
		t.Fatalf("establish: %v", err)
	}
	return s, path, read
}

func TestPollLogsOnePickupLinePerChangedFile(t *testing.T) {
	// Arrange
	s, path, read := pickupSidecar(t)

	// Act
	s.pollAll()

	// Assert — exactly one line, carrying path, count, kind and write latency.
	got := linesContaining(read(), "picked up")
	if len(got) != 1 {
		t.Fatalf("pickup lines = %v, want exactly 1", got)
	}
	for _, want := range []string{path, "5 event(s)", "kind=session", "store_write_ms="} {
		if !strings.Contains(got[0], want) {
			t.Fatalf("pickup line %q missing %q", got[0], want)
		}
	}
}

func TestPollLogsNothingWhenNothingChanged(t *testing.T) {
	// Arrange — the file's events are already picked up and cursored.
	s, _, read := pickupSidecar(t)
	s.pollAll()
	before := len(read())

	// Act — a second pass over an unchanged file.
	s.pollAll()

	// Assert — steady state is silent.
	if after := read(); len(after) != before {
		t.Fatalf("unchanged poll logged %v", after[before:])
	}
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
// logic (resolveOwner/holdUnowned/noteTaskOwner/seedOwners touch no connection).
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
	got, ok := s.resolveOwner(tgt, 1000)

	// Assert
	if !ok || got != "S1" {
		t.Fatalf("resolveOwner = (%q, %v), want (S1, true)", got, ok)
	}
}

func TestResolveOwnerHoldsSpoolWhoseLaunchWasNeverSeen(t *testing.T) {
	// Arrange
	s, _ := ownerSidecar(t)

	// Act
	got, ok := s.resolveOwner(spoolTarget("b1pi0nmip"), 1000)

	// Assert — unresolved, and recorded as held rather than tailed.
	if ok || got != "" {
		t.Fatalf("resolveOwner = (%q, %v), want (\"\", false)", got, ok)
	}
	if _, held := s.held[spoolTarget("b1pi0nmip").Path]; !held {
		t.Fatal("spool was left unresolved without being recorded as held")
	}
}

func TestResolveOwnerAttributesSpoolToItsLaunchingSession(t *testing.T) {
	// Arrange — the transcript's TaskStarted established the mapping.
	s, _ := ownerSidecar(t)
	s.noteTaskOwner("b1pi0nmip", "9b6a4f2d-transcript-id", "", OwnerSourceLiveLaunch)

	// Act
	got, ok := s.resolveOwner(spoolTarget("b1pi0nmip"), 1000)

	// Assert — the TRANSCRIPT id, never the path's runtime id.
	if !ok || got != "9b6a4f2d-transcript-id" {
		t.Fatalf("resolveOwner = (%q, %v), want (9b6a4f2d-transcript-id, true)", got, ok)
	}
}

func TestResolveOwnerClearsTheHoldOnceTheOwnerArrives(t *testing.T) {
	// Arrange — held first, attributed after.
	s, read := ownerSidecar(t)
	s.resolveOwner(spoolTarget("b1pi0nmip"), 1000)
	s.noteTaskOwner("b1pi0nmip", "S1", "", OwnerSourceLiveLaunch)

	// Act
	if _, ok := s.resolveOwner(spoolTarget("b1pi0nmip"), 1500); !ok {
		t.Fatal("resolveOwner did not resolve after the owner was noted")
	}

	// Assert — the hold is released, not merely ignored.
	if len(s.held) != 0 {
		t.Fatalf("held = %v, want empty", s.held)
	}
	if got := linesContaining(read(), "owner resolved"); len(got) != 1 {
		t.Fatalf("resolution lines = %v, want exactly 1", got)
	}
}

func TestReportHeldSummarizesTheBacklogInOneLine(t *testing.T) {
	// Arrange — three spools nothing can attribute. /tmp really does hold
	// thousands of these, so the report must not scale with them.
	s, read := ownerSidecar(t)
	for _, id := range []string{"b1", "b2", "b3"} {
		s.resolveOwner(spoolTarget(id), 1000)
	}

	// Act
	s.reportHeld(1000)

	// Assert
	got := linesContaining(read(), "held awaiting attribution")
	if len(got) != 1 {
		t.Fatalf("summary lines = %v, want exactly 1", got)
	}
	if !strings.Contains(got[0], "3 held") {
		t.Fatalf("summary %q does not carry the held count", got[0])
	}
}

func TestReportHeldStaysSilentWhileTheBacklogIsUnchanged(t *testing.T) {
	// Arrange — reported once already.
	s, read := ownerSidecar(t)
	s.resolveOwner(spoolTarget("b1"), 1000)
	s.reportHeld(1000)
	before := len(read())

	// Act — the rescan timer keeps firing over the same backlog.
	s.reportHeld(1000)

	// Assert — a permanent backlog must not be re-reported every pass.
	if after := read(); len(after) != before {
		t.Fatalf("unchanged backlog logged %v", after[before:])
	}
}

func TestReportHeldCountsAHoldPastTheWindowAsStale(t *testing.T) {
	// Arrange — one spool held long enough to be an anomaly, not a race.
	s, read := ownerSidecar(t)
	s.resolveOwner(spoolTarget("b1"), 1000)

	// Act
	s.reportHeld(1000 + UnownedSpoolWindow.Milliseconds())

	// Assert
	got := linesContaining(read(), "held awaiting attribution")
	if len(got) != 1 || !strings.Contains(got[0], "(1 past ") {
		t.Fatalf("summary = %v, want one line counting 1 past the window", got)
	}
}

func TestReportHeldAnnouncesTheBacklogClearing(t *testing.T) {
	// Arrange — a backlog that then gets attributed.
	s, read := ownerSidecar(t)
	s.resolveOwner(spoolTarget("b1"), 1000)
	s.reportHeld(1000)
	s.noteTaskOwner("b1", "S1", "", OwnerSourceLiveLaunch)
	s.resolveOwner(spoolTarget("b1"), 1100)

	// Act
	s.reportHeld(1100)

	// Assert — the all-clear is reported, not just silently reached.
	if got := linesContaining(read(), "no unattributed spools remain"); len(got) != 1 {
		t.Fatalf("all-clear lines = %v, want exactly 1", got)
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

func TestSeedOwnersRestoresAttributionFromPersistedOpenTasks(t *testing.T) {
	// Arrange — what the store hands back on every connection.
	s, _ := ownerSidecar(t)
	states := []*corev1.OpenTaskState{{
		Started: &corev1.Event{
			SessionId: "9b6a4f2d",
			Payload:   &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "b1pi0nmip"}},
		},
		LastActivityAtMs: 1,
	}}

	// Act
	n := s.seedOwners(states)

	// Assert — a restart can attribute a spool whose launch line sits behind
	// the resumed cursor and will never be re-read.
	if n != 1 || s.owners["b1pi0nmip"] != "9b6a4f2d" {
		t.Fatalf("seeded %d, owners = %v", n, s.owners)
	}
}

func TestSeedOwnersIgnoresAStateCarryingNoTaskStarted(t *testing.T) {
	// Arrange
	s, _ := ownerSidecar(t)
	states := []*corev1.OpenTaskState{{Started: &corev1.Event{SessionId: "S1"}}}

	// Act
	n := s.seedOwners(states)

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
	cs := []*corev1.CursorState{
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

func TestTaskKindToTail(t *testing.T) {
	cases := []struct {
		in   corev1.TaskKind
		want tail.Kind
	}{
		{corev1.TaskKind_TASK_KIND_SHELL, tail.KindShellSpool},
		{corev1.TaskKind_TASK_KIND_WORKFLOW, tail.KindWorkflowJournal},
		{corev1.TaskKind_TASK_KIND_AGENT, tail.KindAgentTranscript},
		{corev1.TaskKind_TASK_KIND_UNSPECIFIED, tail.KindAgentTranscript},
	}
	for _, tc := range cases {
		if got := taskKindToTail(tc.in); got != tc.want {
			t.Fatalf("taskKindToTail(%v) = %v, want %v", tc.in, got, tc.want)
		}
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
