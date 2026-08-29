package dlog

import (
	"errors"
	"sync"
	"testing"
)

// blockingWriter blocks each Write until the test releases it, which is how a
// wedged terminal is modeled without a sleep.
type blockingWriter struct {
	entered chan []byte
	release chan struct{}
}

func newBlockingWriter() *blockingWriter {
	return &blockingWriter{entered: make(chan []byte, 64), release: make(chan struct{})}
}

func (w *blockingWriter) Write(p []byte) (int, error) {
	line := make([]byte, len(p))
	copy(line, p)
	w.entered <- line
	<-w.release
	return len(p), nil
}

// failingWriter fails every write and announces each attempt.
type failingWriter struct {
	attempted chan struct{}
	err       error
}

func (w *failingWriter) Write(p []byte) (int, error) {
	w.attempted <- struct{}{}
	return 0, w.err
}

// collectingWriter records every line it is given.
type collectingWriter struct {
	mu      sync.Mutex
	lines   [][]byte
	written chan struct{}
}

func newCollectingWriter() *collectingWriter {
	return &collectingWriter{written: make(chan struct{}, 64)}
}

func (w *collectingWriter) Write(p []byte) (int, error) {
	line := make([]byte, len(p))
	copy(line, p)
	w.mu.Lock()
	w.lines = append(w.lines, line)
	w.mu.Unlock()
	w.written <- struct{}{}
	return len(p), nil
}

func (w *collectingWriter) all() [][]byte {
	w.mu.Lock()
	defer w.mu.Unlock()
	out := make([][]byte, len(w.lines))
	copy(out, w.lines)
	return out
}

func TestMirrorDeliversWhileHealthy(t *testing.T) {
	// Arrange.
	w := newCollectingWriter()
	m := newMirror(w, 8)
	defer m.close()

	// Act.
	if status := m.enqueue([]byte("one\n")); !status.ok() {
		t.Fatalf("status = %+v, want nothing owed", status)
	}
	<-w.written

	// Assert.
	lines := w.all()
	if len(lines) != 1 || string(lines[0]) != "one\n" {
		t.Fatalf("terminal = %q, want the record", lines)
	}
}

func TestMirrorDropsRatherThanBlocksWhenWedged(t *testing.T) {
	// Arrange: depth 1, and the writer is stuck inside the first Write.
	w := newBlockingWriter()
	m := newMirror(w, 1)
	defer func() { close(w.release); m.close() }()
	m.enqueue([]byte("first\n"))
	<-w.entered                   // the drain goroutine is now blocked
	m.enqueue([]byte("queued\n")) // fills the depth-1 queue

	// Act: this one has nowhere to go.
	status := m.enqueue([]byte("dropped\n"))

	// Assert.
	if status.Dropped != 1 {
		t.Fatalf("Dropped = %d, want 1 — a wedged terminal drops rather than blocks", status.Dropped)
	}
}

func TestMirrorReportsADropExactlyOnce(t *testing.T) {
	// Arrange.
	w := newBlockingWriter()
	m := newMirror(w, 1)
	defer func() { close(w.release); m.close() }()
	m.enqueue([]byte("first\n"))
	<-w.entered
	m.enqueue([]byte("queued\n"))
	if status := m.enqueue([]byte("dropped\n")); status.Dropped != 1 {
		t.Fatalf("first report Dropped = %d, want 1", status.Dropped)
	}

	// Act: the queue is still full, so this one drops too, but the previous
	// report has already been taken.
	second := m.enqueue([]byte("dropped again\n"))

	// Assert.
	if second.Dropped != 1 {
		t.Fatalf("Dropped = %d, want exactly the one new drop", second.Dropped)
	}
}

func TestMirrorReportsAWriteFailureToTheNextEmitter(t *testing.T) {
	// Arrange.
	boom := errors.New("terminal gone")
	w := &failingWriter{attempted: make(chan struct{}, 4), err: boom}
	m := newMirror(w, 8)
	defer m.close()
	m.enqueue([]byte("first\n"))
	<-w.attempted // the failure is now recorded

	// Act.
	status := m.enqueue([]byte("second\n"))

	// Assert.
	if status.Failure == nil || !errors.Is(status.Failure, boom) {
		t.Fatalf("Failure = %v, want the write failure handed to the next emitter", status.Failure)
	}
}

func TestMirrorStopsWritingAfterAFailure(t *testing.T) {
	// Arrange.
	w := &failingWriter{attempted: make(chan struct{}, 4), err: errors.New("terminal gone")}
	m := newMirror(w, 8)
	defer m.close()
	m.enqueue([]byte("first\n"))
	<-w.attempted

	// Act.
	m.enqueue([]byte("second\n"))
	status := m.enqueue([]byte("third\n"))

	// Assert: no second attempt was made, and nothing is reported twice.
	if len(w.attempted) != 0 {
		t.Fatalf("the mirror kept writing to a failed terminal (%d further attempts)", len(w.attempted))
	}
	if status.Failure != nil {
		t.Fatalf("Failure = %v, want the failure reported exactly once", status.Failure)
	}
}
