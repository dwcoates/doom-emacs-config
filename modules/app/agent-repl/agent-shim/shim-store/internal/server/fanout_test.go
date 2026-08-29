package server

import "testing"

func TestNewFanoutFallsBackToTheDefaultBuffer(t *testing.T) {
	// Arrange.

	// Act.
	f := newFanout(0)

	// Assert.
	if f.buffer != DefaultWatchBuffer {
		t.Fatalf("buffer = %d, want %d", f.buffer, DefaultWatchBuffer)
	}
}

func TestPublishDeliversToTheSubscribedBook(t *testing.T) {
	// Arrange.
	f := newFanout(4)
	sub := f.subscribe("a1", "hash")

	// Act.
	overflowed := f.publish([]LineWritten{line("a1", "p1", 1)})

	// Assert.
	if len(overflowed) != 0 {
		t.Fatalf("overflowed = %d, want 0", len(overflowed))
	}
	got := <-sub.lines
	if got.Line.GetAt().GetValue() != "p1" {
		t.Fatalf("delivered %q, want %q", got.Line.GetAt().GetValue(), "p1")
	}
}

func TestPublishSkipsAnotherBooksLines(t *testing.T) {
	// Arrange. Fan-out is an exact match on the line's book.
	f := newFanout(4)
	sub := f.subscribe("a1", "hash")

	// Act.
	f.publish([]LineWritten{line("other", "px", 1), line("a1", "p2", 2)})

	// Assert.
	got := <-sub.lines
	if got.Line.GetAt().GetValue() != "p2" {
		t.Fatalf("delivered %q, want only this book's %q", got.Line.GetAt().GetValue(), "p2")
	}
	if len(sub.lines) != 0 {
		t.Fatalf("buffered %d more lines, want none", len(sub.lines))
	}
}

func TestPublishDropsAnOverflowedSubscriberFromTheRegistry(t *testing.T) {
	// Arrange. A watcher that fell behind recovers by re-opening, never by
	// being silently thinned.
	f := newFanout(1)
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
	f := newFanout(1)
	sub := f.subscribe("a1", "hash")

	// Act.
	f.publish([]LineWritten{line("a1", "p1", 1), line("a1", "p2", 2)})

	// Assert. The signal is a CLOSED channel, so it cannot be missed.
	<-sub.overflow
}

func TestPublishCountsTheLinesLostToAnOverflow(t *testing.T) {
	// Arrange. The warning must say how much was dropped.
	f := newFanout(1)
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
	f := newFanout(1)
	sub := f.subscribe("a1", "hash")

	// Act.
	overflowed := f.publish(nil)

	// Assert.
	if len(overflowed) != 0 || len(sub.lines) != 0 {
		t.Fatalf("overflowed = %d, buffered = %d, want 0 and 0", len(overflowed), len(sub.lines))
	}
}

func TestUnsubscribeIsIdempotent(t *testing.T) {
	// Arrange. The watch loop's defer runs even after an overflow removed it.
	f := newFanout(4)
	sub := f.subscribe("a1", "hash")

	// Act.
	f.unsubscribe(sub)
	f.unsubscribe(sub)

	// Assert.
	if f.subscribers() != 0 {
		t.Fatalf("subscribers = %d, want 0", f.subscribers())
	}
}
