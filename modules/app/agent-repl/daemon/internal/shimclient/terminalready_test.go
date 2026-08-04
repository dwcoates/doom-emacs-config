package shimclient

import (
	"context"
	"errors"
	"fmt"
	"testing"
)

type typedTerminalTestError struct{ marker string }

func (e *typedTerminalTestError) Error() string { return e.marker }

func TestAwaitReadyReturnsExactTerminalRunCause(t *testing.T) {
	c := New(Config{SessionID: "session", Logf: t.Logf})
	want := &typedTerminalTestError{marker: "resume identity mismatch"}
	c.finishTerminal(fmt.Errorf("frame rejected: %w", want))

	err := c.AwaitReady(context.Background())
	var got *typedTerminalTestError
	if !errors.As(err, &got) || got != want {
		t.Fatalf("AwaitReady error = %v, want exact typed terminal cause", err)
	}
}
