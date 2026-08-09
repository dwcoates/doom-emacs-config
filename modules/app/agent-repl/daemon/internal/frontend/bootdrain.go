package frontend

import (
	"sync"
	"time"
)

// BootSnapshotDrainDecision names the ONE structural window in which a slow ack
// delivery is expected rather than alarming: the interval a connection spends
// draining its own connect StateSnapshot.
//
// It is carried on the sample and printed as `decision=boot_snapshot_drain`, so
// a record an operator reads says WHY it was not a warning, in the same
// vocabulary the rest of the daemon classifies decisions with.
const BootSnapshotDrainDecision = "boot_snapshot_drain"

// snapshotDrain is one connection's CONNECT-SNAPSHOT DRAIN WINDOW: the interval
// from the moment its initial StateSnapshot was handed to the outbound queue
// until that snapshot's bytes left for the socket.
//
// WHY THE WINDOW EXISTS. The host's connect snapshot carries every workspace the
// daemon knows about — 104 in the observed boot — and a snapshot that large
// takes seconds to marshal onto a socket an Emacs host reads single-threaded.
// A command answered during that interval has its ack delivered behind one
// in-flight write it cannot preempt: the control lane bounds how many frames sit
// AHEAD of the ack, and there is still exactly one writer goroutine and one
// socket. The delay is therefore inherent to bringing a connection up, not a
// fault in the daemon's command path, and the client's own 10s ack deadline is
// the meaningful bound during it.
//
// WHY IT IS STRUCTURAL RATHER THAN A TIMER. The window's two edges are the two
// events that actually define it — the snapshot's enqueue and the outbox's
// terminal disposition for that same frame — so it cannot be open while the
// snapshot is already delivered, nor closed while it is still draining. A
// duration guess ("the first N seconds of a connection") would drift from the
// real drain in both directions on every boot whose size or host differed.
//
// A drain that never opened (a connection with no snapshot, which serveClient
// makes impossible) reports no overlap at all: an unexplained slow ack keeps
// warning exactly as it did before this type existed.
type snapshotDrain struct {
	mu sync.Mutex
	// opened is set once the connect snapshot has been handed to the outbound
	// queue. Only the FIRST open counts: a connection has exactly one connect
	// snapshot, and the GUI stream's later lease snapshots are ordinary bulk
	// traffic rather than a second bring-up.
	opened   bool
	openedAt time.Time
	// closed is set once that snapshot's frame reached its terminal disposition
	// — its bytes on the socket, or the reason they never will be. Both end the
	// drain: a snapshot that will never be written is not still draining.
	closed   bool
	closedAt time.Time
}

// open marks the instant the connect snapshot was handed to the outbound queue.
// A second call is ignored, so the window's start is the bring-up's start.
func (d *snapshotDrain) open(at time.Time) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.opened {
		return
	}
	d.opened, d.openedAt = true, at
}

// closeAt marks the instant the connect snapshot reached its terminal
// disposition. A second call is ignored: the window closes once, at the first
// disposition, and nothing reopens it.
func (d *snapshotDrain) closeAt(at time.Time) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.closed {
		return
	}
	d.closed, d.closedAt = true, at
}

// snapshotDisposed is the outbox notify hook for the connect snapshot frame. It
// closes the window at the moment the writer reported the frame's disposition,
// which is exactly the write completion the window is defined by.
//
// The error is deliberately not consulted: a snapshot that will never reach the
// socket ends the drain just as a written one does, and the frame's own error
// handling — the write failure log and the disconnect — is untouched by this
// observer.
func (d *snapshotDrain) snapshotDisposed(error) { d.closeAt(time.Now()) }

// overlaps reports whether the interval [start, end] intersects this drain
// window, which is [openedAt, closedAt) once closed and [openedAt, ∞) while the
// snapshot is still draining.
//
// The closing edge is EXCLUSIVE: a command whose wait began at or after the
// snapshot's write completion was not waiting on that write, which is what
// "the window closes exactly at write completion" has to mean for the record
// after it to be a plain warning again.
func (d *snapshotDrain) overlaps(start, end time.Time) bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	if !d.opened {
		return false
	}
	if end.Before(d.openedAt) {
		return false
	}
	if d.closed && !start.Before(d.closedAt) {
		return false
	}
	return true
}
