package frontend

import (
	"sync"
	"time"
)

// BootSnapshotDrainDecision names the ONE structural window in which a slow ack
// delivery is expected rather than alarming: the interval a connection spends
// draining the bring-up backlog its connect StateSnapshot opens.
//
// It is carried on the sample and printed as `decision=boot_snapshot_drain`, so
// a record an operator reads says WHY it was not a warning, in the same
// vocabulary the rest of the daemon classifies decisions with.
const BootSnapshotDrainDecision = "boot_snapshot_drain"

// snapshotDrain is one connection's INITIAL-BACKLOG DRAIN WINDOW: the interval
// from the moment its connect StateSnapshot was handed to the outbound queue
// until the first moment that outbox is empty again.
//
// WHY THE WINDOW EXISTS. The host's connect snapshot carries every workspace the
// daemon knows about — 104 in the observed boot — and it is only the first frame
// of the bring-up storm: the retained roster, the task catalogs, the progress
// views and the session inits follow it immediately, all on the bulk lane. A
// command answered during that storm has its ack delivered behind a write it
// cannot preempt. The control lane bounds how many frames sit AHEAD of the ack,
// and that is all it can bound: there is still one writer goroutine and one
// socket, and an Emacs host reads it single-threaded. The delay is inherent to
// bringing a connection up, not a fault in the daemon's command path, and the
// client's own 10s ack deadline is the meaningful bound during it.
//
// WHY THE CLOSING EDGE IS "EMPTY", NOT "THE SNAPSHOT WAS WRITTEN". It was the
// snapshot's own write completion first, and the evidence refuted it: a
// publish_workspace_roster ack still took 4343ms and warned with no
// classification, because the ack drained behind the frames the snapshot was
// merely the first of. The snapshot's write is the first frame of the storm, not
// the end of it. The outbox running dry is the end of it — that is the moment
// the connection has nothing left owed from its bring-up, and the next ack's
// wait is the command path's own.
//
// WHY IT IS STRUCTURAL RATHER THAN A TIMER. Both edges are events the transport
// already performs: the snapshot's push, and the writer's own drain loop finding
// the queue exhausted. Nothing is estimated, so the window cannot be open while
// the connection is idle nor closed while frames are still queued, and it does
// not drift with the storm's size or the host's speed the way any duration guess
// would.
//
// A connection whose outbox NEVER runs dry keeps its window open, and that is
// honest rather than lenient: the storm never ended. A drain that never opened
// (a connection with no snapshot, which serveClient makes impossible) reports no
// overlap at all, so an unexplained slow ack keeps warning exactly as it did
// before this type existed.
type snapshotDrain struct {
	mu sync.Mutex
	// opened is set once the connect snapshot has been handed to the outbound
	// queue. Only the FIRST open counts: a connection has exactly one connect
	// snapshot, and the GUI stream's later lease snapshots are ordinary bulk
	// traffic rather than a second bring-up.
	opened   bool
	openedAt time.Time
	// closed is set at the first moment the outbox held nothing after that
	// push. Only the FIRST such moment counts: every later quiet period is
	// ordinary running, not a bring-up.
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

// closeAt marks the instant the outbox first ran dry after the connect snapshot
// was pushed. It is called from the writer's drain loop every time the queue is
// exhausted, so it must be idempotent: the window closes once, at the FIRST such
// moment, and nothing reopens it.
//
// A drain that never opened is left alone. Closing one would leave a window that
// is closed before it starts, and the later open would then produce an
// [openedAt, closedAt) interval running backwards.
func (d *snapshotDrain) closeAt(at time.Time) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if !d.opened || d.closed {
		return
	}
	d.closed, d.closedAt = true, at
}

// overlaps reports whether the interval [start, end] intersects this drain
// window, which is [openedAt, closedAt) once the backlog has drained and
// [openedAt, ∞) while frames are still owed.
//
// The closing edge is EXCLUSIVE: a command whose wait began at or after the
// outbox ran dry was not waiting on the bring-up, which is what "the window
// closes when the initial backlog is gone" has to mean for the record after it
// to be a plain warning again.
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
