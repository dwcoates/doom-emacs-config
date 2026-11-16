package integration

import (
	"context"
	"errors"
	"os/exec"
	"strings"
	"sync/atomic"
	"testing"
)

func TestTestContextEndsWithItsLatchsCause(t *testing.T) {
	// Arrange
	ctx, cancel := testContext(t)
	defer cancel()
	cause := errors.New("the sidecar exited unasked")

	// Act
	faultFor(t).close(t, cause)

	// Assert
	<-ctx.Done()
	if got := context.Cause(ctx); !errors.Is(got, cause) {
		t.Fatalf("context.Cause = %v, want the latch's cause", got)
	}
}

func TestWaitLatched(t *testing.T) {
	cases := []struct {
		name      string
		stopped   bool
		wantFault bool
	}{
		{name: "an exit nobody asked for closes the latch", wantFault: true},
		{name: "an exit the test asked for leaves it open", stopped: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd := exec.Command("/bin/sh", "-c", "echo dying; exit 3")
			out := &childOutput{}
			cmd.Stdout = out
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			var stopped atomic.Bool
			stopped.Store(tc.stopped)
			ctx, cancel := testContext(t)
			defer cancel()
			done := make(chan error, 1)

			// Act
			waitLatched(t, "the stand-in", cmd, &stopped, out, done)

			// Assert
			<-done
			cause := context.Cause(faultFor(t).ctx)
			if tc.wantFault != (cause != nil) {
				t.Fatalf("latch cause = %v, want a fault: %v", cause, tc.wantFault)
			}
			if tc.wantFault && (!strings.Contains(cause.Error(), "the stand-in exited unasked") || !strings.Contains(cause.Error(), "dying")) {
				t.Fatalf("latch cause = %v, want the exit and the output", cause)
			}
			if tc.wantFault && ctx.Err() == nil {
				t.Fatal("the test context outlived the latch")
			}
		})
	}
}
