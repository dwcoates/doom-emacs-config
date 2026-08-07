package dlog

import (
	"errors"
	"fmt"
	"io"
	"sync"
)

// ---------------------------------------------------------------------------
// The terminal mirror's decoupling buffer
// ---------------------------------------------------------------------------
//
// Every canonical record has TWO destinations: the durable sink, which is
// authoritative, and the terminal, which is a MIRROR for whoever is watching.
// Both used to be written synchronously, inside `persist`, under the durable
// sink's mutex. That made the daemon's own progress a hostage of whoever owned
// the terminal.
//
// In production the daemon is a child of Emacs and its stderr is a pty Emacs
// reads from its process filter. A pty's kernel buffer is small, so once Emacs
// stops draining — which it does for seconds at a time during its own startup,
// exactly when the daemon logs hardest — the daemon's next terminal write
// BLOCKS in the kernel. It blocks holding the durable sink's mutex, so every
// other goroutine's logging blocks behind it, and since the daemon logs on the
// critical path of every frontend command, the command's ack blocks too. The
// measured shape of this is unmistakable: normal-record inter-arrival times in
// a production boot log cluster at 16-17ms and its multiples — Emacs's
// redisplay cadence, imposed on a daemon that has nothing to do with it. At
// boot the same coupling cost one roster publish 6957ms of ack latency, 100% of
// which was spent inside a single verbose log call that never touches the
// terminal at all: it was waiting for the sink mutex held by a normal record's
// blocked terminal write.
//
// TerminalSink breaks that coupling and nothing else. It is an io.Writer that
// takes the line, queues it, and returns; one goroutine drains the queue into
// the real terminal in FIFO order. What it deliberately does NOT do:
//
//   - It never drops a record. A full buffer BLOCKS the writer (backpressure of
//     last resort) rather than discarding a line, because a mirror that
//     silently omits records is worse than a slow one.
//   - It never swallows a write failure. A failed terminal write is LATCHED and
//     returned from the next Write, so the emitter still sees the error on its
//     own error channel and dlog's callers still treat it exactly as they treat
//     one today. The report is delayed by one record; it is never lost.
//   - It never reorders. One queue, one drain goroutine, FIFO — so the mirror's
//     order is the order records were emitted, across every Logger sharing the
//     sink, which is a stronger guarantee than the unsynchronized concurrent
//     writes to a shared os.Stderr it replaces.
//
// The durable sink is untouched: it is still written synchronously, in order,
// before the terminal ever sees the line. A crash can therefore lose queued
// MIRROR lines and never a durable record.

// DefaultTerminalBufferBytes bounds the queued mirror. It is generous on
// purpose — a whole Emacs startup's chatter is a few hundred kilobytes — so
// that reaching it means the terminal has stopped consuming entirely, which is
// the one case where blocking the emitter is the honest answer.
const DefaultTerminalBufferBytes = 8 << 20

// ErrTerminalSinkClosed is returned by a Write that arrives after Close. It is
// an error rather than a silent no-op: a record emitted after the sink was
// closed is a lifecycle violation the caller must be able to see.
var ErrTerminalSinkClosed = errors.New("dlog: terminal sink is closed")

// TerminalSink is the asynchronous mirror described above. The zero value is
// not usable; construct one with NewTerminalSink.
type TerminalSink struct {
	dest     io.Writer
	capBytes int

	mu       sync.Mutex
	space    *sync.Cond // woken when the drain frees buffer space
	work     *sync.Cond // woken when a line is queued or the sink is closed
	pending  [][]byte
	buffered int
	// failure latches the first terminal write error. Once set, the drain
	// stops and every subsequent Write reports it, so the failure reaches an
	// emitter's error channel instead of dying in a background goroutine.
	failure error
	closed  bool

	done chan struct{}
}

// NewTerminalSink wraps dest with the decoupling buffer and starts its drain
// goroutine. A nonpositive capBytes uses DefaultTerminalBufferBytes. A nil dest
// is a programmer error, surfaced the way dlog surfaces its other missing
// dependencies.
func NewTerminalSink(dest io.Writer, capBytes int) *TerminalSink {
	if dest == nil {
		panic("dlog: terminal sink destination is required")
	}
	if capBytes <= 0 {
		capBytes = DefaultTerminalBufferBytes
	}
	t := &TerminalSink{dest: dest, capBytes: capBytes, done: make(chan struct{})}
	t.space = sync.NewCond(&t.mu)
	t.work = sync.NewCond(&t.mu)
	go t.drain()
	return t
}

// Write queues one line for the terminal. It reports the full length written
// once the line is queued: the mirror owns the line from that moment, and the
// only outcomes are that it reaches the terminal or that its failure is
// reported to a later Write.
//
// The line is COPIED. dlog hands persist's own buffer over, and the drain
// touches it after Write returns, so borrowing it would be a data race.
func (t *TerminalSink) Write(p []byte) (int, error) {
	if len(p) == 0 {
		return 0, nil
	}
	line := make([]byte, len(p))
	copy(line, p)

	t.mu.Lock()
	defer t.mu.Unlock()
	if t.failure != nil {
		return 0, fmt.Errorf("dlog: terminal sink write failed: %w", t.failure)
	}
	if t.closed {
		return 0, ErrTerminalSinkClosed
	}
	// Backpressure, never loss. A single line larger than the whole cap is
	// still accepted (waiting for space it can never get would deadlock); the
	// cap bounds the QUEUE, not one record.
	for t.buffered > 0 && t.buffered+len(line) > t.capBytes {
		t.space.Wait()
		if t.failure != nil {
			return 0, fmt.Errorf("dlog: terminal sink write failed: %w", t.failure)
		}
		if t.closed {
			return 0, ErrTerminalSinkClosed
		}
	}
	t.pending = append(t.pending, line)
	t.buffered += len(line)
	t.work.Signal()
	return len(p), nil
}

// Close stops accepting lines and waits for the queued ones to reach the
// terminal. It returns the latched write failure, if any, so a caller that
// checks its shutdown errors still learns the mirror failed.
func (t *TerminalSink) Close() error {
	t.mu.Lock()
	if t.closed {
		failure := t.failure
		t.mu.Unlock()
		<-t.done
		return failure
	}
	t.closed = true
	t.work.Broadcast()
	t.space.Broadcast()
	t.mu.Unlock()
	<-t.done
	t.mu.Lock()
	defer t.mu.Unlock()
	return t.failure
}

// drain writes queued lines to the real terminal in FIFO order until the sink
// is closed and empty, or until a write fails.
func (t *TerminalSink) drain() {
	defer close(t.done)
	for {
		t.mu.Lock()
		for len(t.pending) == 0 && !t.closed && t.failure == nil {
			t.work.Wait()
		}
		if t.failure != nil || (len(t.pending) == 0 && t.closed) {
			t.mu.Unlock()
			return
		}
		line := t.pending[0]
		t.pending = t.pending[1:]
		t.buffered -= len(line)
		t.space.Broadcast()
		t.mu.Unlock()

		if err := writeFull(t.dest, line); err != nil {
			t.mu.Lock()
			t.failure = err
			// Everything still queued is unreachable now; releasing it wakes
			// the blocked writers so they observe the latched failure instead
			// of waiting for a drain that has stopped.
			t.pending = nil
			t.buffered = 0
			t.space.Broadcast()
			t.mu.Unlock()
			return
		}
	}
}
