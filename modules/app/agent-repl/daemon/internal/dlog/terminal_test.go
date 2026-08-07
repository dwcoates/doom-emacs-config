package dlog

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"strings"
	"sync"
	"testing"
	"time"
)

// blockingWriter is a terminal that consumes nothing until it is released. It
// stands in for the production pty: Emacs owns the other end and stops reading
// while it is busy, and the kernel buffer's writer blocks.
type blockingWriter struct {
	release chan struct{}
	mu      sync.Mutex
	written [][]byte
	entered chan struct{}
}

func newBlockingWriter() *blockingWriter {
	return &blockingWriter{release: make(chan struct{}), entered: make(chan struct{}, 64)}
}

func (w *blockingWriter) Write(p []byte) (int, error) {
	select {
	case w.entered <- struct{}{}:
	default:
	}
	<-w.release
	w.mu.Lock()
	defer w.mu.Unlock()
	line := make([]byte, len(p))
	copy(line, p)
	w.written = append(w.written, line)
	return len(p), nil
}

func (w *blockingWriter) lines() []string {
	w.mu.Lock()
	defer w.mu.Unlock()
	out := make([]string, 0, len(w.written))
	for _, l := range w.written {
		out = append(out, strings.TrimRight(string(l), "\n"))
	}
	return out
}

// syncWriter is a trivially cooperative terminal that records what it got.
type syncWriter struct {
	mu    sync.Mutex
	lines []string
}

func (w *syncWriter) Write(p []byte) (int, error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.lines = append(w.lines, strings.TrimRight(string(p), "\n"))
	return len(p), nil
}

func (w *syncWriter) snapshot() []string {
	w.mu.Lock()
	defer w.mu.Unlock()
	return append([]string(nil), w.lines...)
}

// THE POINT OF THE TYPE: an emitter never waits on the terminal's reader.
func TestATerminalSinkWriteDoesNotWaitForABlockedTerminal(t *testing.T) {
	// Arrange.
	terminal := newBlockingWriter()
	sink := NewTerminalSink(terminal, 0)
	// Park the drain goroutine inside the blocked write so the next line can
	// only be accepted by the buffer, never by the terminal.
	if _, err := sink.Write([]byte("first\n")); err != nil {
		t.Fatalf("first write: %v", err)
	}
	<-terminal.entered

	// Act.
	done := make(chan error, 1)
	go func() {
		_, err := sink.Write([]byte("second\n"))
		done <- err
	}()

	// Assert.
	select {
	case err := <-done:
		if err != nil {
			t.Fatalf("write while the terminal is blocked: %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("a write blocked behind an unresponsive terminal; the decoupling buffer is not decoupling")
	}
	close(terminal.release)
	if err := sink.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
}

// The mirror is FIFO, across every concurrent emitter sharing the sink.
func TestATerminalSinkPreservesTheOrderLinesWereWritten(t *testing.T) {
	// Arrange.
	terminal := &syncWriter{}
	sink := NewTerminalSink(terminal, 0)
	want := make([]string, 0, 200)
	for i := 0; i < 200; i++ {
		want = append(want, fmt.Sprintf("line-%03d", i))
	}

	// Act.
	for _, line := range want {
		if _, err := sink.Write([]byte(line + "\n")); err != nil {
			t.Fatalf("write %s: %v", line, err)
		}
	}
	if err := sink.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Assert.
	got := terminal.snapshot()
	if len(got) != len(want) {
		t.Fatalf("terminal got %d lines, want %d", len(got), len(want))
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("terminal line %d = %q, want %q", i, got[i], want[i])
		}
	}
}

// NOTHING IS DROPPED. Close flushes what is queued before it returns.
func TestATerminalSinkFlushesEveryQueuedLineOnClose(t *testing.T) {
	// Arrange.
	terminal := newBlockingWriter()
	sink := NewTerminalSink(terminal, 0)
	for i := 0; i < 25; i++ {
		if _, err := sink.Write([]byte(fmt.Sprintf("queued-%02d\n", i))); err != nil {
			t.Fatalf("write %d: %v", i, err)
		}
	}
	<-terminal.entered

	// Act.
	close(terminal.release)
	if err := sink.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Assert.
	got := terminal.lines()
	if len(got) != 25 {
		t.Fatalf("terminal received %d lines, want all 25 queued lines", len(got))
	}
}

// A FULL BUFFER BLOCKS, it does not discard: backpressure of last resort.
func TestATerminalSinkBlocksRatherThanDroppingWhenItsBufferIsFull(t *testing.T) {
	// Arrange. One line's worth of capacity, with the drain parked in a
	// blocked terminal write.
	terminal := newBlockingWriter()
	sink := NewTerminalSink(terminal, 8)
	if _, err := sink.Write([]byte("aaaaaaaa\n")); err != nil { // taken by the drain
		t.Fatalf("first write: %v", err)
	}
	<-terminal.entered
	if _, err := sink.Write([]byte("bbbbbbbb\n")); err != nil { // fills the buffer
		t.Fatalf("second write: %v", err)
	}

	// Act.
	blocked := make(chan error, 1)
	go func() {
		_, err := sink.Write([]byte("cccccccc\n"))
		blocked <- err
	}()
	select {
	case err := <-blocked:
		t.Fatalf("a write over a full buffer returned early (%v); it must wait for space rather than drop the line", err)
	case <-time.After(50 * time.Millisecond):
	}
	close(terminal.release)

	// Assert.
	select {
	case err := <-blocked:
		if err != nil {
			t.Fatalf("write over a full buffer: %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("a write over a full buffer never completed after the terminal drained")
	}
	if err := sink.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	if got := len(terminal.lines()); got != 3 {
		t.Fatalf("terminal received %d lines, want 3 with none dropped", got)
	}
}

// A FAILURE IS REPORTED TO AN EMITTER, never swallowed in the drain goroutine.
func TestATerminalSinkReportsAWriteFailureToTheNextWriter(t *testing.T) {
	// Arrange.
	boom := errors.New("terminal is gone")
	sink := NewTerminalSink(failingWriter{err: boom}, 0)
	if _, err := sink.Write([]byte("doomed\n")); err != nil {
		t.Fatalf("first write: %v", err)
	}

	// Act.
	var err error
	deadline := time.Now().Add(2 * time.Second)
	for time.Now().Before(deadline) {
		if _, err = sink.Write([]byte("next\n")); err != nil {
			break
		}
	}

	// Assert.
	if !errors.Is(err, boom) {
		t.Fatalf("write after a failed terminal write = %v, want the latched %v", err, boom)
	}
	if closeErr := sink.Close(); !errors.Is(closeErr, boom) {
		t.Fatalf("close = %v, want the latched %v", closeErr, boom)
	}
}

// A write after Close is a lifecycle violation, surfaced rather than ignored.
func TestATerminalSinkRefusesAWriteAfterClose(t *testing.T) {
	// Arrange.
	sink := NewTerminalSink(&syncWriter{}, 0)
	if err := sink.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act.
	_, err := sink.Write([]byte("late\n"))

	// Assert.
	if !errors.Is(err, ErrTerminalSinkClosed) {
		t.Fatalf("write after close = %v, want ErrTerminalSinkClosed", err)
	}
}

// A record larger than the whole cap is still accepted; the cap bounds the
// QUEUE, and waiting for space a single line can never get would deadlock.
func TestATerminalSinkAcceptsALineLargerThanItsCap(t *testing.T) {
	// Arrange.
	terminal := &syncWriter{}
	sink := NewTerminalSink(terminal, 4)
	huge := strings.Repeat("x", 64)

	// Act.
	n, err := sink.Write([]byte(huge + "\n"))

	// Assert.
	if err != nil {
		t.Fatalf("write of an oversized line: %v", err)
	}
	if n != len(huge)+1 {
		t.Fatalf("write reported %d bytes, want %d", n, len(huge)+1)
	}
	if err := sink.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	if got := terminal.snapshot(); len(got) != 1 || got[0] != huge {
		t.Fatalf("terminal got %v, want the oversized line", got)
	}
}

// The sink borrows nothing: dlog reuses its encode buffer, so the queued line
// must be the sink's own copy.
func TestATerminalSinkCopiesTheCallersBuffer(t *testing.T) {
	// Arrange.
	terminal := newBlockingWriter()
	sink := NewTerminalSink(terminal, 0)
	buffer := []byte("original\n")
	if _, err := sink.Write(buffer); err != nil {
		t.Fatalf("first write: %v", err)
	}
	<-terminal.entered
	queued := []byte("queued-original\n")
	if _, err := sink.Write(queued); err != nil {
		t.Fatalf("second write: %v", err)
	}

	// Act.
	copy(queued, []byte("queued-MUTATED!"))
	close(terminal.release)
	if err := sink.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Assert.
	got := terminal.lines()
	if len(got) != 2 || got[1] != "queued-original" {
		t.Fatalf("terminal got %v, want the line as it was when written", got)
	}
}

// THE REGRESSION THIS EXISTS FOR: one Logger's blocked terminal write used to
// hold the durable sink's mutex, so an unrelated emitter's record — including a
// VERBOSE one, which never reaches the terminal at all — waited out the
// terminal's reader. It must not.
func TestABlockedTerminalDoesNotStallAnUnrelatedVerbosePersist(t *testing.T) {
	// Arrange.
	terminal := newBlockingWriter()
	sink := NewTerminalSink(terminal, 0)
	var durable bytes.Buffer
	logger := New(&lockedBuffer{buf: &durable}, sink, false)
	if err := logger.EmitNormal(GlobalScope(), Event{
		Runtime: RuntimeDaemon, Level: LevelInfo, Operation: "daemon.test", Message: "normal record", Context: map[string]any{},
	}); err != nil {
		t.Fatalf("normal emit: %v", err)
	}
	<-terminal.entered

	// Act.
	done := make(chan error, 1)
	go func() {
		done <- logger.EmitVerbose(GlobalScope(), Event{
			Runtime: RuntimeDaemon, Level: LevelInfo, Operation: "daemon.test", Message: "verbose record", Context: map[string]any{},
		})
	}()

	// Assert.
	select {
	case err := <-done:
		if err != nil {
			t.Fatalf("verbose emit behind a blocked terminal: %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("a verbose emit waited on a blocked terminal it never writes to; the durable sink is still coupled to the terminal")
	}
	close(terminal.release)
	if err := sink.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	if !strings.Contains(durable.String(), "verbose record") {
		t.Fatal("the verbose record never reached the durable sink")
	}
}

// lockedBuffer is a concurrency-safe durable sink for the test above.
type lockedBuffer struct {
	mu  sync.Mutex
	buf *bytes.Buffer
}

func (b *lockedBuffer) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.Write(p)
}

var _ io.Writer = (*TerminalSink)(nil)
