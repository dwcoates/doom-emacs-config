package main

import (
	"strings"
	"testing"
	"time"
)

// mustAwaitSubscribers is awaitSubscribers under its bound, failing T with
// the wait's own account when the subscription never lands.
func (s *fakeServer) mustAwaitSubscribers(t testing.TB, stream, workspaceID string, n int) {
	t.Helper()
	if err := s.awaitSubscribers(stream, workspaceID, n, awaitSubscribersBound); err != nil {
		t.Fatal(err)
	}
}

func TestAwaitSubscribersFailsLoudlyWhenNoneLands(t *testing.T) {
	// Arrange
	server := newFakeServer()

	// Act
	err := server.awaitSubscribers(streamDaemon, "", 1, 10*time.Millisecond)

	// Assert
	if err == nil || !strings.Contains(err.Error(), `stream "daemon"`) || !strings.Contains(err.Error(), "0 registered") {
		t.Fatalf("awaitSubscribers = %v, want a failure naming the stream and what was registered", err)
	}
}
