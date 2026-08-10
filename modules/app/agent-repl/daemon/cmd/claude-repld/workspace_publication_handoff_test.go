package main

import (
	"context"
	"fmt"
	"runtime"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/server"
	workspacecreate "claude-repld/internal/workspace/create"
)

// THE RELEASE HAND-OFF MUST NOT HOLD THE BRIDGE MUTEX.
//
// The subscriber's buffer is finite, so a send can wait — and the only
// goroutine that drains it is the one running the release, which reaches back
// into this bridge through Open and through the publication gate, both of which
// take the bridge mutex. Sending under that mutex therefore turns a full buffer
// into a deadlock: the sender holding the lock the receiver needs to make room.
//
// This is the same lock-order defect as the frontend's publication release, on
// the other side of the same hand-off.

// releaseSubscriberBuffer is the capacity SubscribeSessionPublicationReleases
// gives a subscriber. The test fills it, so it has to know it.
const releaseSubscriberBuffer = 32

// awaitBufferFull spins until the subscriber's buffer holds want frames. It is
// a spin rather than a wait because the senders are blocked INSIDE a channel
// send, which offers nothing to wait on; Gosched yields instead of burning the
// scheduler.
func awaitBufferFull(t *testing.T, releases <-chan server.SessionPublicationRelease, want int) {
	t.Helper()
	deadline := time.Now().Add(30 * time.Second)
	for len(releases) < want {
		if time.Now().After(deadline) {
			t.Fatalf("the release buffer reached %d of %d frames", len(releases), want)
		}
		runtime.Gosched()
	}
}

func TestTheBridgeStaysAnswerableWhileAReleaseHandOffWaitsForBufferSpace(t *testing.T) {
	// Arrange — enough concurrent releases to fill the subscriber's buffer and
	// leave one sender blocked inside the send.
	bridge := gateFixture(t, heldJob())
	releases, cancel := bridge.SubscribeSessionPublicationReleases()
	defer cancel()
	senders := releaseSubscriberBuffer + 1
	var sending sync.WaitGroup
	sending.Add(senders)
	for i := 0; i < senders; i++ {
		go func(i int) {
			defer sending.Done()
			_ = bridge.ReleaseSessionPublication(context.Background(), workspacecreate.PublicationDecision{
				JobID:        fmt.Sprintf("job-%d", i),
				WorktreePath: fmt.Sprintf("/worktrees/w-%d", i),
				SessionID:    fmt.Sprintf("s-%d", i),
				Materialized: true,
			})
		}(i)
	}
	awaitBufferFull(t, releases, releaseSubscriberBuffer)

	// Act — ask the gate a question, exactly as a broadcasting frontend does
	// while the release is in flight.
	answered := make(chan error, 1)
	go func() {
		_, err := bridge.SessionPublicationDecision("/worktrees/held", "")
		answered <- err
	}()

	// Assert.
	select {
	case err := <-answered:
		if err != nil {
			t.Fatalf("SessionPublicationDecision: %v", err)
		}
	case <-time.After(30 * time.Second):
		t.Fatal("the gate never answered — a release hand-off is holding the bridge mutex while it waits for buffer space")
	}

	// Drain, so the senders finish before the test does.
	go func() {
		for release := range releases {
			release.Completion <- release.Open()
		}
	}()
	sending.Wait()
}
