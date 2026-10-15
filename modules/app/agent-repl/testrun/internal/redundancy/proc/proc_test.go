package proc

import (
	"context"
	"path/filepath"
	"strings"
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
	// Arrange: the shell leaves a grandchild that holds the output pipe open
	// for 30s. OSExec reads that pipe to its end, so it can return before
	// then only if the timeout ended the grandchild too. (Its pid cannot be
	// probed instead: a killed orphan answers kill(pid, 0) until init reaps
	// it.)
	c := Cmd{Path: "/bin/sh", Args: []string{"-c", "sleep 30 & wait"}, Timeout: 300 * time.Millisecond}

	start := time.Now()

	// Act
	res, err := OSExec(context.Background(), c)

	// Assert
	returned := time.Since(start)
	if err != nil {
		t.Fatal(err)
	}
	if !res.TimedOut {
		t.Fatal("TimedOut = false")
	}
	if returned >= 30*time.Second {
		t.Fatalf("OSExec returned after %v: the grandchild held the pipe to its end", returned)
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
