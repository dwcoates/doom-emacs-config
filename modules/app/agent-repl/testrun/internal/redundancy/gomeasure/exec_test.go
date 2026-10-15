package gomeasure

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"
)

func TestOSExecCapturesMergedLinesAndTheExitStatus(t *testing.T) {
	// Arrange
	c := Cmd{Path: "/bin/sh", Args: []string{"-c", "echo out; echo err >&2; exit 3"}, Timeout: 5 * time.Second}

	// Act
	res, err := OSExec(context.Background(), c)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if res.Exit != 3 || res.TimedOut {
		t.Fatalf("exit %d, timed out %t; want 3, false", res.Exit, res.TimedOut)
	}
	if got := res.Text(); got != "out\nerr\n" && got != "err\nout\n" {
		t.Fatalf("text = %q, want both lines", got)
	}
}

func TestOSExecPassesTheAddedEnvironment(t *testing.T) {
	// Arrange
	c := Cmd{Path: "/bin/sh", Args: []string{"-c", `echo "$GOMEASURE_PROBE"`}, Env: []string{"GOMEASURE_PROBE=yes"}, Timeout: 5 * time.Second}

	// Act
	res, err := OSExec(context.Background(), c)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if res.Text() != "yes\n" {
		t.Fatalf("text = %q", res.Text())
	}
}

func TestOSExecEndsTheWholeGroupAtItsTimeout(t *testing.T) {
	// Arrange: the shell leaves a grandchild that would hold the pipe open.
	pidFile := filepath.Join(t.TempDir(), "pid")
	script := "sleep 30 & echo $! > " + pidFile + "; wait"
	c := Cmd{Path: "/bin/sh", Args: []string{"-c", script}, Timeout: 300 * time.Millisecond}

	// Act
	res, err := OSExec(context.Background(), c)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !res.TimedOut {
		t.Fatal("TimedOut = false")
	}
	b, err := os.ReadFile(pidFile)
	if err != nil {
		t.Fatal(err)
	}
	pid := strings.TrimSpace(string(b))
	var n int
	for _, ch := range pid {
		n = n*10 + int(ch-'0')
	}
	if err := syscall.Kill(n, 0); err == nil {
		t.Fatalf("the grandchild %d outlived the timeout", n)
	}
}

func TestOSExecRefusesAnUnboundedCommand(t *testing.T) {
	// Act
	_, err := OSExec(context.Background(), Cmd{Path: "/bin/true"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "has no timeout") {
		t.Fatalf("err = %v, want the missing bound refused", err)
	}
}

func TestOSExecReportsACommandThatCannotStart(t *testing.T) {
	// Act
	_, err := OSExec(context.Background(), Cmd{Path: filepath.Join(t.TempDir(), "absent"), Timeout: time.Second})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "start") {
		t.Fatalf("err = %v, want the start failure", err)
	}
}
