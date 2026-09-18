package harness

import (
	"context"
	"errors"
	"os/exec"
	"testing"
	"time"
)

func TestWaitProcessExit(t *testing.T) {
	cases := []struct {
		name string
		// end ends the process (or not) once the wait is armed; nil leaves it
		// running.
		end     func(cmd *exec.Cmd) error
		reaped  bool
		wantErr error
	}{
		{name: "a process that exits is reported", end: func(cmd *exec.Cmd) error { return cmd.Process.Kill() }},
		{name: "a process already gone is reported", reaped: true},
		{name: "a process that outlives the bound is refused", wantErr: context.DeadlineExceeded},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd := exec.Command("cat")
			stdin, err := cmd.StdinPipe()
			if err != nil {
				t.Fatalf("stdin: %v", err)
			}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
			pid := cmd.Process.Pid
			if tc.reaped {
				stdin.Close()
				_ = cmd.Wait()
			}
			bound := 5 * time.Second
			if tc.wantErr != nil {
				bound = 50 * time.Millisecond
			}
			ctx, cancel := context.WithTimeout(context.Background(), bound)
			defer cancel()
			done := make(chan error, 1)

			// Act
			go func() { done <- WaitProcessExit(ctx, pid) }()
			if tc.end != nil {
				if err := tc.end(cmd); err != nil {
					t.Fatalf("end: %v", err)
				}
			}
			err = <-done

			// Assert
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("WaitProcessExit = %v, want %v", err, tc.wantErr)
			}
		})
	}
}
