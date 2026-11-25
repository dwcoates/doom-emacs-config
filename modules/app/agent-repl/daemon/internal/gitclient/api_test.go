package gitclient

import (
	"context"
	"errors"
	"fmt"
	"testing"
)

func TestStoppedBy(t *testing.T) {
	cancelledGit := &Cancelled{Args: []string{"show-ref"}, Dir: "/repo", Cause: context.Canceled}
	tests := []struct {
		name      string
		cancelCtx bool
		err       error
		want      bool
	}{
		{name: "a git the daemon cancelled under a live context is stopped", err: cancelledGit, want: true},
		{name: "a wrapped cancelled git is stopped", err: fmt.Errorf("retire: %w", cancelledGit), want: true},
		{name: "any error under a done context is stopped", cancelCtx: true, err: errors.New("boom"), want: true},
		{name: "a failure under a live context is not stopped", err: errors.New("boom"), want: false},
		{name: "a failed git under a live context is not stopped", err: &Error{Args: []string{"show-ref"}, ExitCode: 128}, want: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if test.cancelCtx {
				cancel()
			}

			// Act.
			got := StoppedBy(ctx, test.err)

			// Assert.
			if got != test.want {
				t.Fatalf("StoppedBy(%v) = %t, want %t", test.err, got, test.want)
			}
		})
	}
}
