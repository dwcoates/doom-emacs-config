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

func TestCounterCountsEveryProcessAndItsCPU(t *testing.T) {
	// Arrange
	c := &Counter{Name: "x", Procs: 1, Exec: func(context.Context, Cmd) (Result, error) { return Result{CPU: time.Second}, nil }}

	// Act
	for i := 0; i < 3; i++ {
		if _, err := c.Run(context.Background(), Cmd{Path: "/bin/true"}); err != nil {
			t.Fatal(err)
		}
	}

	// Assert
	if c.Processes != 3 || c.CPU != 3*time.Second {
		t.Fatalf("processes %d, cpu %s", c.Processes, c.CPU)
	}
}

func TestCounterEndsAtAnInterrupt(t *testing.T) {
	tests := []struct {
		name string
		// cancelDuring cancels while the process runs, else before it.
		cancelDuring bool
		want         string
	}{
		{name: "before a process starts", want: "x: interrupted: context canceled"},
		{name: "while a process runs", cancelDuring: true, want: "x: interrupted during tool: context canceled"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx, cancel := context.WithCancel(context.Background())
			ran := false
			c := &Counter{Name: "x", Procs: 1, Exec: func(context.Context, Cmd) (Result, error) {
				ran = true
				cancel()
				return Result{TimedOut: true}, nil
			}}
			if !tt.cancelDuring {
				cancel()
			}

			// Act
			_, err := c.Run(ctx, Cmd{Path: "/usr/bin/tool"})

			// Assert
			if err == nil || err.Error() != tt.want {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
			if ran != tt.cancelDuring {
				t.Fatalf("ran = %t", ran)
			}
		})
	}
}

func TestCounterCapsEveryProcessItRuns(t *testing.T) {
	// Arrange: a command carrying a GOMAXPROCS of its own.
	var env []string
	c := &Counter{Name: "x", Procs: 2, Exec: func(_ context.Context, cmd Cmd) (Result, error) {
		env = cmd.Env
		return Result{}, nil
	}}
	cmd := Cmd{Path: "/bin/true", Env: []string{"A=1", "GOMAXPROCS=16"}}

	// Act
	if _, err := c.Run(context.Background(), cmd); err != nil {
		t.Fatal(err)
	}

	// Assert: the cap comes last, so it wins, and the caller's slice is
	// left alone.
	if len(env) != 3 || env[2] != "GOMAXPROCS=2" {
		t.Fatalf("env = %v, want the cap last", env)
	}
	if len(cmd.Env) != 2 {
		t.Fatalf("the caller's env became %v", cmd.Env)
	}
}

func TestCounterRefusesNoCap(t *testing.T) {
	// Arrange
	ran := false
	c := &Counter{Name: "x", Exec: func(context.Context, Cmd) (Result, error) {
		ran = true
		return Result{}, nil
	}}

	// Act
	_, err := c.Run(context.Background(), Cmd{Path: "/bin/true"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "x: the process cap must be at least 1, got 0") || ran {
		t.Fatalf("err = %v, ran %t", err, ran)
	}
}
