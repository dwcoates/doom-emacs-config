package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/handler"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// capturingLog returns a Logf that records every formatted line, plus a reader
// for what it has seen. It is mutex-guarded because the sidecar's logger is.
func capturingLog() (handler.Logf, func() []string) {
	var (
		mu    sync.Mutex
		lines []string
	)
	logf := func(format string, args ...any) {
		msg := fmt.Sprintf(format, args...)
		mu.Lock()
		lines = append(lines, msg)
		mu.Unlock()
	}
	return logf, func() []string {
		mu.Lock()
		defer mu.Unlock()
		return append([]string(nil), lines...)
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
