package kernelevent

import (
	"context"
	"errors"
	"io"
	"os/exec"
	"testing"
)

// startShell starts sh running script with its stdin a pipe, so the test
// says when the script moves on by writing a line.
func startShell(t *testing.T, script string) (*exec.Cmd, io.WriteCloser) {
	t.Helper()
	cmd := exec.Command("/bin/sh", "-c", script)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatalf("stdin: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { stdin.Close(); _ = cmd.Process.Kill(); _ = cmd.Wait() })
	return cmd, stdin
}

func TestSpawnsReportsAStart(t *testing.T) {
	cases := []struct {
		name string
		// script reads one line, then starts something.
		script string
		follow func(s *Spawns, pid int) (bool, error)
	}{
		{name: "a fork by a process followed for forks", script: "read x; /usr/bin/true; read y",
			follow: (*Spawns).FollowForks},
		{name: "an exec by a process followed for its exec", script: "read x; exec /bin/cat",
			follow: (*Spawns).FollowExec},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd, stdin := startShell(t, tc.script)
			s, err := NewSpawns()
			if err != nil {
				t.Fatalf("NewSpawns: %v", err)
			}
			defer s.Close()
			if ok, err := tc.follow(s, cmd.Process.Pid); !ok || err != nil {
				t.Fatalf("follow = %v, %v; want registered", ok, err)
			}
			ctx, cancel := context.WithTimeout(context.Background(), waitBound)
			defer cancel()

			// Act
			if _, err := io.WriteString(stdin, "go\n"); err != nil {
				t.Fatalf("write: %v", err)
			}
			err = s.Wait(ctx)

			// Assert
			if err != nil {
				t.Fatalf("Wait = %v, want the start reported", err)
			}
		})
	}
}

func TestSpawnsAnswersFalseForAProcessGone(t *testing.T) {
	cases := []struct {
		name   string
		follow func(s *Spawns, pid int) (bool, error)
	}{
		{name: "forks", follow: (*Spawns).FollowForks},
		{name: "exec", follow: (*Spawns).FollowExec},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd := exec.Command("/usr/bin/true")
			if err := cmd.Run(); err != nil {
				t.Fatalf("run: %v", err)
			}
			s, err := NewSpawns()
			if err != nil {
				t.Fatalf("NewSpawns: %v", err)
			}
			defer s.Close()

			// Act
			ok, err := tc.follow(s, cmd.Process.Pid)

			// Assert
			if ok || err != nil {
				t.Fatalf("follow = %v, %v; want false and no error", ok, err)
			}
		})
	}
}

func TestSpawnsWaitEndsWithItsContext(t *testing.T) {
	// Arrange
	s, err := NewSpawns()
	if err != nil {
		t.Fatalf("NewSpawns: %v", err)
	}
	defer s.Close()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err = s.Wait(ctx)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Wait = %v, want context.Canceled", err)
	}
}
