package dlog

import (
	"fmt"
	"io"
	"sync"
	"time"
)

// mirrorDepth is the terminal mirror's queue depth. It is generous because the
// mirror must drop nothing while the terminal keeps up, and bounded because a
// wedged terminal must cost memory rather than the daemon.
const mirrorDepth = 4096

// mirrorCloseGrace bounds how long Close waits for the mirror to finish. A
// terminal wedged inside a write must not hold up an orderly exit; the durable
// records are already on disk.
const mirrorCloseGrace = 2 * time.Second

// mirror is the terminal side of every record, DECOUPLED from the durable
// sink. Enqueueing is non-blocking and the drain runs on its own goroutine, so
// a stalled reader on the other end of the terminal cannot add a single
// microsecond to a durable write (a stalled pty reader once added seconds to
// boot by being on the write path).
type mirror struct {
	out  io.Writer
	q    chan []byte
	done chan struct{}
	// finished closes when the drain goroutine has returned.
	finished chan struct{}

	mu      sync.Mutex
	failure error
	dropped int

	closeOnce sync.Once
}

// mirrorStatus is what the mirror hands back to whichever emitter comes next:
// the write failure it could not report itself, and how many lines it dropped
// while wedged. Both are reported once and then cleared.
type mirrorStatus struct {
	Failure error
	Dropped int
}

// ok reports whether there is nothing to tell.
func (s mirrorStatus) ok() bool { return s.Failure == nil && s.Dropped == 0 }

// newMirror starts the drain goroutine for out.
func newMirror(out io.Writer, depth int) *mirror {
	m := &mirror{
		out:      out,
		q:        make(chan []byte, depth),
		done:     make(chan struct{}),
		finished: make(chan struct{}),
	}
	go m.run()
	return m
}

// enqueue offers one line to the terminal and returns whatever the mirror owes
// the caller. It never blocks: a full queue means the terminal is wedged, and
// the line is dropped and counted.
func (m *mirror) enqueue(line []byte) mirrorStatus {
	select {
	case m.q <- line:
	default:
		m.mu.Lock()
		m.dropped++
		m.mu.Unlock()
	}
	return m.take()
}

// take removes and returns the pending failure and drop count.
func (m *mirror) take() mirrorStatus {
	m.mu.Lock()
	defer m.mu.Unlock()
	status := mirrorStatus{Failure: m.failure, Dropped: m.dropped}
	m.failure = nil
	m.dropped = 0
	return status
}

// run drains the queue. A write failure is remembered for the next emitter and
// the mirror stops writing; it keeps draining so enqueue stays non-blocking.
func (m *mirror) run() {
	defer close(m.finished)
	failed := false
	for {
		select {
		case line := <-m.q:
			if failed {
				continue
			}
			if _, err := m.out.Write(line); err != nil {
				failed = true
				m.mu.Lock()
				if m.failure == nil {
					m.failure = fmt.Errorf("write to the terminal mirror: %w", err)
				}
				m.mu.Unlock()
			}
		case <-m.done:
			// Drain what is already queued, then stop.
			for {
				select {
				case line := <-m.q:
					if !failed {
						if _, err := m.out.Write(line); err != nil {
							failed = true
						}
					}
				default:
					return
				}
			}
		}
	}
}

// close stops the drain goroutine, waiting only briefly for it.
func (m *mirror) close() {
	m.closeOnce.Do(func() { close(m.done) })
	select {
	case <-m.finished:
	case <-time.After(mirrorCloseGrace):
	}
}
