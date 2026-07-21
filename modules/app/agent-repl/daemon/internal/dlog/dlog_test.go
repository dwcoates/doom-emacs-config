package dlog

import (
	"errors"
	"fmt"
	"strings"
	"testing"
)

// capture returns a Logf that records rendered lines.
func capture() (Logf, *[]string) {
	var lines []string
	logf := func(format string, args ...any) {
		lines = append(lines, fmt.Sprintf(format, args...))
	}
	return logf, &lines
}

func TestClampShortStringUnchanged(t *testing.T) {
	// Arrange
	s := "short"
	// Act + Assert
	if got := Clamp(s, 10); got != "short" {
		t.Fatalf("Clamp = %q, want unchanged", got)
	}
}

func TestClampLongStringTruncatedWithMarker(t *testing.T) {
	// Arrange
	s := strings.Repeat("x", 20)
	// Act
	got := Clamp(s, 10)
	// Assert
	if got != strings.Repeat("x", 10)+" …[clamped]" {
		t.Fatalf("Clamp = %q, want 10 bytes plus marker", got)
	}
}

func TestTagAppendsContextBlock(t *testing.T) {
	// Arrange
	logf, lines := capture()
	tagged := Tag(logf, "session", "s1", "cwd", "/w")
	// Act
	tagged("hello %d", 7)
	// Assert
	if len(*lines) != 1 || (*lines)[0] != "hello 7 {session=s1 cwd=/w}" {
		t.Fatalf("lines = %v", *lines)
	}
}

func TestTagWithNoPairsReturnsLogfUnchanged(t *testing.T) {
	// Arrange
	logf, lines := capture()
	tagged := Tag(logf)
	// Act
	tagged("plain")
	// Assert
	if len(*lines) != 1 || (*lines)[0] != "plain" {
		t.Fatalf("lines = %v", *lines)
	}
}

func TestTagMarksAnOddTrailingKey(t *testing.T) {
	// Arrange
	logf, lines := capture()
	tagged := Tag(logf, "session", "s1", "dangling")
	// Act
	tagged("x")
	// Assert
	if (*lines)[0] != "x {session=s1 dangling=!MISSING}" {
		t.Fatalf("lines = %v", *lines)
	}
}

func TestTagFuncResolvesPairsAtCallTime(t *testing.T) {
	// Arrange — the cwd becomes known between construction and use.
	logf, lines := capture()
	cwd := ""
	tagged := TagFunc(logf, func() []any { return []any{"cwd", cwd} })
	// Act
	tagged("before")
	cwd = "/w"
	tagged("after")
	// Assert
	if (*lines)[0] != "before {cwd=}" || (*lines)[1] != "after {cwd=/w}" {
		t.Fatalf("lines = %v", *lines)
	}
}

func TestCallLogsStartWithContext(t *testing.T) {
	// Arrange
	logf, lines := capture()
	// Act
	Call(logf, "classifier", "session", "s1", "model", "haiku")
	// Assert
	if len(*lines) != 1 || (*lines)[0] != "classifier: call start {session=s1 model=haiku}" {
		t.Fatalf("lines = %v", *lines)
	}
}

func TestCallCompletionLogsSuccessWithClampedOutput(t *testing.T) {
	// Arrange
	logf, lines := capture()
	done := Call(logf, "summarizer", "session", "s1")
	// Act
	done(strings.Repeat("y", OutputClampLen+50), nil)
	// Assert
	got := (*lines)[1]
	if !strings.HasPrefix(got, "summarizer: call ok in ") {
		t.Fatalf("completion line = %q", got)
	}
	if !strings.Contains(got, "{session=s1}") || !strings.Contains(got, "…[clamped]") {
		t.Fatalf("completion line missing context or clamp marker: %q", got)
	}
}

func TestCallCompletionLogsFailureWithErrorAndOutputTail(t *testing.T) {
	// Arrange
	logf, lines := capture()
	done := Call(logf, "classifier", "session", "s1")
	// Act
	done("partial stdout", errors.New("signal: killed"))
	// Assert
	got := (*lines)[1]
	if !strings.HasPrefix(got, "classifier: call FAILED after ") {
		t.Fatalf("failure line = %q", got)
	}
	if !strings.Contains(got, "signal: killed") || !strings.Contains(got, "partial stdout") {
		t.Fatalf("failure line missing error or output: %q", got)
	}
}
