package main

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

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
