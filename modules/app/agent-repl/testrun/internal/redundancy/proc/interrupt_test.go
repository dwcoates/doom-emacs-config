package proc

import (
	"context"
	"errors"
	"os"
	"strings"
	"syscall"
	"testing"
)

func TestInterruptedByNamesTheSignalAsItsCause(t *testing.T) {
	// Arrange
	signals := make(chan os.Signal, 1)
	ctx, stop := interruptedBy(context.Background(), signals)
	defer stop()

	// Act
	signals <- syscall.SIGTERM
	<-ctx.Done()

	// Assert
	if cause := context.Cause(ctx); cause == nil || !strings.Contains(cause.Error(), "terminated received") {
		t.Fatalf("cause = %v, want the signal named", cause)
	}
}

func TestInterruptedByStoppedIsPlainlyCanceled(t *testing.T) {
	// Arrange
	ctx, stop := interruptedBy(context.Background(), make(chan os.Signal))

	// Act
	stop()

	// Assert
	if cause := context.Cause(ctx); !errors.Is(cause, context.Canceled) {
		t.Fatalf("cause = %v, want context.Canceled", cause)
	}
}

func TestCounterNamesTheSignalThatInterruptedIt(t *testing.T) {
	// Arrange
	signals := make(chan os.Signal, 1)
	ctx, stop := interruptedBy(context.Background(), signals)
	defer stop()
	signals <- syscall.SIGINT
	<-ctx.Done()
	c := &Counter{Name: "gomeasure", Exec: func(context.Context, Cmd) (Result, error) { return Result{}, nil }}

	// Act
	_, err := c.Run(ctx, Cmd{Path: "/x/cover-1.test"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "gomeasure: interrupted: interrupt received") {
		t.Fatalf("err = %v, want the signal named", err)
	}
}
