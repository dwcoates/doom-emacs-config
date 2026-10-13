package harness

import (
	"context"
	"errors"
	"io"
	"net"
	"strings"
	"testing"

	"agent-repld/internal/workspace"
)

// dialHoldLikeTheDaemon connects to a hold and reads until it is released,
// as the daemon's watcher-start hold does, answering the read's end.
func dialHoldLikeTheDaemon(t *testing.T, socket string) <-chan error {
	t.Helper()
	conn, err := net.Dial("unix", socket)
	if err != nil {
		t.Fatalf("dial the hold: %v", err)
	}
	done := make(chan error, 1)
	go func() {
		defer conn.Close()
		_, err := io.Copy(io.Discard, conn)
		done <- err
	}()
	return done
}

func TestWatcherStartHoldEnvNamesItsSocket(t *testing.T) {
	// Arrange
	h := HoldFirstWatcherStart(t)

	// Act
	got := h.Env()

	// Assert
	if want := workspace.EnvTestHoldFirstWatcherStart + "=" + h.socket; got != want {
		t.Fatalf("Env() = %q, want %q", got, want)
	}
}

func TestWatcherStartHoldKeepsTheDaemonHeldUntilRelease(t *testing.T) {
	// Arrange
	h := HoldFirstWatcherStart(t)
	read := dialHoldLikeTheDaemon(t, h.socket)
	h.AwaitHeld(context.Background())

	// Act
	h.Release()

	// Assert: the daemon's read ends in a clean EOF.
	if err := <-read; err != nil {
		t.Fatalf("the held read ended with %v, want the release's EOF", err)
	}
}

func TestWatcherStartHoldReleaseIsIdempotent(t *testing.T) {
	// Arrange
	h := HoldFirstWatcherStart(t)
	read := dialHoldLikeTheDaemon(t, h.socket)
	h.AwaitHeld(context.Background())
	h.Release()

	// Act
	h.Release()

	// Assert
	if err := <-read; err != nil {
		t.Fatalf("the held read ended with %v, want the release's EOF", err)
	}
}

func TestWatcherStartHoldThatNoDaemonReachesFails(t *testing.T) {
	// Arrange: the wait has already ended, and nothing dialed.
	h := HoldFirstWatcherStart(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	conn, err := h.accept(ctx)

	// Assert
	if conn != nil || !errors.Is(err, context.Canceled) || !strings.Contains(err.Error(), h.socket) {
		t.Fatalf("accept = (%v, %v), want no connection and the ended wait naming %s", conn, err, h.socket)
	}
}

func TestWatcherStartHoldWhoseListenerIsGoneFails(t *testing.T) {
	// Arrange
	h := HoldFirstWatcherStart(t)
	_ = h.ln.Close()

	// Act
	conn, err := h.accept(context.Background())

	// Assert
	if conn != nil || !errors.Is(err, net.ErrClosed) || !strings.Contains(err.Error(), "accept the watcher-start hold") {
		t.Fatalf("accept = (%v, %v), want no connection and the closed listener's error", conn, err)
	}
}
