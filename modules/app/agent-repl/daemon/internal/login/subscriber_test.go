package login

import (
	"context"
	"testing"
)

func TestSubscriberDeliversEveryFrameInOrder(t *testing.T) {
	// Arrange: a viewer must never lose a frame — a dropped chunk silently
	// corrupts the screen it is trying to draw.
	sub := newSubscriber()
	want := []string{"a", "b", "c", "d", "e"}
	for _, s := range want {
		sub.push(Output{Bytes: []byte(s)})
	}
	sub.end()

	// Act.
	out := sub.start(context.Background(), func() {})

	// Assert.
	var got []string
	for frame := range out {
		got = append(got, string(frame.Bytes))
	}
	if len(got) != len(want) {
		t.Fatalf("frames = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("frame %d = %q, want %q", i, got[i], want[i])
		}
	}
}

func TestSubscriberDeliversFramesQueuedBeforeEnd(t *testing.T) {
	// Arrange: end means "no MORE frames", never "discard what is queued".
	sub := newSubscriber()
	sub.push(Output{Bytes: []byte("screen")})
	sub.push(Output{Closed: true})
	sub.end()

	// Act.
	out := sub.start(context.Background(), func() {})

	// Assert.
	var frames []Output
	for frame := range out {
		frames = append(frames, frame)
	}
	if len(frames) != 2 || !frames[1].Closed {
		t.Fatalf("frames = %+v, want the bytes then the terminal frame", frames)
	}
}

func TestSubscriberClosesTheChannelWhenTheContextIsCancelled(t *testing.T) {
	// Arrange.
	sub := newSubscriber()
	ctx, cancel := context.WithCancel(context.Background())

	// Act.
	out := sub.start(ctx, func() {})
	cancel()

	// Assert.
	if _, ok := <-out; ok {
		t.Fatal("the channel yielded a frame, want it closed on cancellation")
	}
}

func TestSubscriberRunsOnDoneWhenItStops(t *testing.T) {
	// Arrange: the session unregisters a detached viewer through this hook.
	sub := newSubscriber()
	done := make(chan struct{})
	ctx, cancel := context.WithCancel(context.Background())

	// Act.
	sub.start(ctx, func() { close(done) })
	cancel()

	// Assert.
	<-done
}

func TestSubscriberPushNeverBlocksTheProducer(t *testing.T) {
	// Arrange: the pty reader holds the session lock while it broadcasts, so a
	// producer that could block would stall every other viewer.
	sub := newSubscriber()

	// Act: far more frames than any channel buffer, with no consumer at all.
	for i := 0; i < 10_000; i++ {
		sub.push(Output{Bytes: []byte("x")})
	}
	sub.end()

	// Assert.
	sub.mu.Lock()
	queued := len(sub.queue)
	sub.mu.Unlock()
	if queued != 10_000 {
		t.Fatalf("queued = %d, want 10000", queued)
	}
}
