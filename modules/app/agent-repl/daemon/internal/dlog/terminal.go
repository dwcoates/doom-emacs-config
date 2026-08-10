package dlog

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"sync"
	"time"
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
//   - It never BLOCKS the emitter. A full buffer drops the OLDEST queued
//     mirror lines to make room for the newest, because the mirror is a tail
//     for whoever is watching and the newest line is the one being watched
//     for. Blocking here was the second half of the production stall: the
//     producer waited for mirror space WHILE HOLDING the sink lock, so a slow
//     mirror consumer parked every logging goroutine in the daemon.
//   - It never drops SILENTLY. Every dropped line is counted, and once space
//     frees the sink reports the count through itself as a canonical record,
//     rate limited so a persistently full mirror cannot itself become the
//     flood.
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
// the one case where the oldest queued mirror lines are worth losing.
const DefaultTerminalBufferBytes = 8 << 20

// TerminalDropReportInterval rate limits the sink's self-report about dropped
// mirror lines. One report per interval, carrying the number dropped since the
// previous one, so a mirror that stays full reports a running toll instead of
// doubling the flood it is already failing to keep up with.
const TerminalDropReportInterval = time.Second

// TerminalDropOperation is the canonical operation of the sink's self-report.
const TerminalDropOperation = "daemon.logging.terminal-mirror-dropped"

// ErrTerminalSinkClosed is returned by a Write that arrives after Close. It is
// an error rather than a silent no-op: a record emitted after the sink was
// closed is a lifecycle violation the caller must be able to see.
var ErrTerminalSinkClosed = errors.New("dlog: terminal sink is closed")

// pendingLine is one queued mirror line. covers is zero for an ordinary record
// and, for the sink's own drop report, the number of drops that report accounts
// for — so if the report is itself evicted its toll goes back on the counter
// instead of vanishing with it.
type pendingLine struct {
	bytes  []byte
	covers int
}

// TerminalSink is the asynchronous mirror described above. The zero value is
// not usable; construct one with NewTerminalSink.
type TerminalSink struct {
	dest     io.Writer
	capBytes int

	mu       sync.Mutex
	work     *sync.Cond // woken when a line is queued or the sink is closed
	pending  []pendingLine
	buffered int
	// dropped counts mirror lines discarded since the last self-report, and
	// lastReport is when that report went out. Together they make the loss
	// visible in the mirror itself rather than inferable only from a gap.
	dropped    int
	lastReport time.Time
	// now is the clock the rate limiter reads. Tests replace it; production
	// never does.
	now func() time.Time
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
	t := &TerminalSink{dest: dest, capBytes: capBytes, done: make(chan struct{}), now: time.Now}
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
	// Loss, never blocking. The producer must not wait here: it holds the
	// emitter's whole logging call, and in production that call is on the
	// critical path of every frontend command. A single line larger than the
	// whole cap is still accepted; the cap bounds the QUEUE, not one record.
	t.makeRoom(len(line))
	t.reportDropsLocked(len(line))
	t.pending = append(t.pending, pendingLine{bytes: line})
	t.buffered += len(line)
	t.work.Signal()
	return len(p), nil
}

// makeRoom discards the OLDEST queued mirror lines until incoming fits, and
// counts every one it discards. The caller holds t.mu.
func (t *TerminalSink) makeRoom(incoming int) {
	for len(t.pending) > 0 && t.buffered+incoming > t.capBytes {
		oldest := t.pending[0]
		t.pending = t.pending[1:]
		t.buffered -= len(oldest.bytes)
		if oldest.covers > 0 {
			// Evicting the report would erase the only account of the lines it
			// covers. Put the toll back and let it be reported again.
			t.dropped += oldest.covers
			t.lastReport = time.Time{}
			continue
		}
		t.dropped++
	}
}

// reportDropsLocked queues the sink's own account of what it discarded, at most
// once per TerminalDropReportInterval and only when the queue has room for the
// report. The caller holds t.mu.
func (t *TerminalSink) reportDropsLocked(reserve int) {
	if t.dropped == 0 {
		return
	}
	at := t.now()
	if !t.lastReport.IsZero() && at.Sub(t.lastReport) < TerminalDropReportInterval {
		return
	}
	record := Record{
		Timestamp: NewStamp(at),
		Runtime:   RuntimeDaemon,
		Level:     LevelWarn,
		Verbosity: Normal,
		Operation: TerminalDropOperation,
		Message:   "terminal mirror buffer full; oldest mirror lines dropped",
		Context: map[string]any{
			"dropped_lines":     t.dropped,
			"buffer_cap_bytes":  t.capBytes,
			"report_interval_s": TerminalDropReportInterval.Seconds(),
		},
		PID: os.Getpid(),
	}
	line, err := json.Marshal(record)
	if err != nil {
		// The report is the only channel this failure has; there is no caller
		// to return it to, so it goes into the mirror as plain text rather
		// than being swallowed.
		line = []byte(fmt.Sprintf("dlog: encode terminal mirror drop report failed: %v (dropped_lines=%d)", err, t.dropped))
	}
	line = append(line, '\n')
	if len(t.pending) > 0 && t.buffered+len(line)+reserve > t.capBytes {
		return // still no room; keep counting and report later
	}
	t.pending = append(t.pending, pendingLine{bytes: line, covers: t.dropped})
	t.buffered += len(line)
	t.dropped = 0
	t.lastReport = at
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
		line := t.pending[0].bytes
		t.pending = t.pending[1:]
		t.buffered -= len(line)
		t.mu.Unlock()

		if err := writeFull(t.dest, line); err != nil {
			t.mu.Lock()
			t.failure = err
			// Everything still queued is unreachable now; releasing it keeps
			// the stopped drain from pinning the buffer, and the next Write
			// observes the latched failure.
			t.pending = nil
			t.buffered = 0
			t.mu.Unlock()
			return
		}
	}
}
