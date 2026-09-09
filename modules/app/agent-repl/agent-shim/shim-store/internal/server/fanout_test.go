package server

import (
	"fmt"
	"testing"
)

func TestNewFanoutFallsBackToTheDefaultBuffer(t *testing.T) {
	// Arrange.

	// Act.
	f := newFanout(0, lineKey)

	// Assert.
	if f.buffer != DefaultWatchBuffer {
		t.Fatalf("buffer = %d, want %d", f.buffer, DefaultWatchBuffer)
	}
}

func TestPublishDeliversToTheSubscribedBook(t *testing.T) {
	// Arrange.
	f := newFanout(4, lineKey)
	sub := f.subscribe("a1", "hash")

	// Act.
	overflowed := f.publish([]LineWritten{line("a1", "p1", 1)})

	// Assert.
	if len(overflowed) != 0 {
		t.Fatalf("overflowed = %d, want 0", len(overflowed))
	}
	got := <-sub.items
	if got.Line.GetAt().GetValue() != "p1" {
		t.Fatalf("delivered %q, want %q", got.Line.GetAt().GetValue(), "p1")
	}
}

func TestPublishSkipsAnotherBooksLines(t *testing.T) {
	// Arrange. Fan-out is an exact match on the line's book.
	f := newFanout(4, lineKey)
	sub := f.subscribe("a1", "hash")

	// Act.
	f.publish([]LineWritten{line("other", "px", 1), line("a1", "p2", 2)})

	// Assert.
	got := <-sub.items
	if got.Line.GetAt().GetValue() != "p2" {
		t.Fatalf("delivered %q, want only this book's %q", got.Line.GetAt().GetValue(), "p2")
	}
	if len(sub.items) != 0 {
		t.Fatalf("buffered %d more lines, want none", len(sub.items))
	}
}

func TestPublishDropsAnOverflowedSubscriberFromTheRegistry(t *testing.T) {
	// Arrange. A watcher that fell behind recovers by re-opening, never by
	// being silently thinned.
	f := newFanout(1, lineKey)
	f.subscribe("a1", "hash")

	// Act.
	overflowed := f.publish([]LineWritten{line("a1", "p1", 1), line("a1", "p2", 2)})

	// Assert.
	if len(overflowed) != 1 {
		t.Fatalf("overflowed = %d, want 1", len(overflowed))
	}
	if f.subscribers() != 0 {
		t.Fatalf("subscribers = %d, want 0 after an overflow", f.subscribers())
	}
}

func TestPublishSignalsAnOverflowedSubscriber(t *testing.T) {
	// Arrange.
	f := newFanout(1, lineKey)
	sub := f.subscribe("a1", "hash")

	// Act.
	f.publish([]LineWritten{line("a1", "p1", 1), line("a1", "p2", 2)})

	// Assert. The signal is a CLOSED channel, so it cannot be missed.
	<-sub.overflow
}

func TestPublishCountsTheLinesLostToAnOverflow(t *testing.T) {
	// Arrange. The warning must say how much was dropped.
	f := newFanout(1, lineKey)
	sub := f.subscribe("a1", "hash")

	// Act.
	f.publish([]LineWritten{line("a1", "p1", 1), line("a1", "p2", 2), line("a1", "p3", 3)})

	// Assert.
	<-sub.overflow
	if sub.dropped != 2 {
		t.Fatalf("dropped = %d, want 2", sub.dropped)
	}
}

func TestPublishOfNoLinesTouchesNoSubscriber(t *testing.T) {
	// Arrange. A batch of unserveable rows produces no page lines.
	f := newFanout(1, lineKey)
	sub := f.subscribe("a1", "hash")

	// Act.
	overflowed := f.publish(nil)

	// Assert.
	if len(overflowed) != 0 || len(sub.items) != 0 {
		t.Fatalf("overflowed = %d, buffered = %d, want 0 and 0", len(overflowed), len(sub.items))
	}
}

func TestUnsubscribeIsIdempotent(t *testing.T) {
	// Arrange. The watch loop's defer runs even after an overflow removed it.
	f := newFanout(4, lineKey)
	sub := f.subscribe("a1", "hash")

	// Act.
	f.unsubscribe(sub)
	f.unsubscribe(sub)

	// Assert.
	if f.subscribers() != 0 {
		t.Fatalf("subscribers = %d, want 0", f.subscribers())
	}
}

// TestPublishAbsorbsABurstOfTheWholeBuffer is the load-independent statement of
// what the integration suite's default-buffer test claims: a burst up to the
// buffer's capacity is absorbed, and no subscriber is ended. The integration
// test can only observe this through a real store on a shared box; here the
// claim is decided by the fan-out itself.
func TestPublishAbsorbsABurstOfTheWholeBuffer(t *testing.T) {
	// Arrange.
	const buffer = 4096
	f := newFanout(buffer, lineKey)
	sub := f.subscribe("a1", "hash")
	burst := make([]LineWritten, 0, buffer)
	for i := 0; i < buffer; i++ {
		burst = append(burst, line("a1", fmt.Sprintf("p%d", i), uint64(i+1)))
	}

	// Act.
	overflowed := f.publish(burst)

	// Assert.
	if len(overflowed) != 0 {
		t.Fatalf("overflowed = %d, want 0 on a burst of exactly the buffer", len(overflowed))
	}
	if got := len(sub.items); got != buffer {
		t.Fatalf("buffered = %d, want %d", got, buffer)
	}
	if f.subscribers() != 1 {
		t.Fatalf("subscribers = %d, want 1", f.subscribers())
	}
}

// TestTheDefaultBufferExceedsADaemonBounceBurst pins the shipped default above
// the burst the integration suite declares absorbable, so shrinking the default
// fails here rather than as a timing-shaped flake in an integration run.
func TestTheDefaultBufferExceedsADaemonBounceBurst(t *testing.T) {
	// Arrange. 4096 is the burst the store's default-buffer integration test
	// writes, itself SUBSTANTIALLY above the ~1k a daemon bounce can burst.
	const declaredAbsorbableBurst = 4096

	// Act.
	got := DefaultWatchBuffer

	// Assert.
	if got < declaredAbsorbableBurst {
		t.Fatalf("DefaultWatchBuffer = %d, want at least %d", got, declaredAbsorbableBurst)
	}
}
