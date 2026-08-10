package frontend

import (
	"sync"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
)

// ---------------------------------------------------------------------------
// client_log is telemetry, not a command lane's work
// ---------------------------------------------------------------------------
//
// A client_log used to ride the per-workspace command lane (lanes.go) that
// carries that workspace's opens, prompts and resyncs. It was moved to a
// low-priority sublane first, which bounded the damage without removing the
// coupling: the lane's single executor still ran every record one at a time,
// still owed each one an ack, and still counted each one in the connection's
// in-flight accounting. A webview flooding client_log built a 6,142-deep
// backlog on its workspace's lane and 3,696 records blew the client's 10s ack
// deadline, because the ack a client_log waits for is the WRITE completing.
//
// Nothing waits on that write. A client_log is evidence: no user-visible state
// changes when it lands, no later command's meaning depends on it having
// landed, and no client reads its ack for anything but "stop tracking this
// request". So it does not belong on a lane at all.
//
// The shape is therefore: ACK AT INGRESS, then write asynchronously.
//
//   - The ack means ACCEPTED FOR WRITING, and it is handed to the outbound
//     queue from the read loop with no queueing wait in front of it.
//   - The record goes to this writer, which owns a BOUNDED queue and ONE
//     writer goroutine. One goroutine keeps every workspace's records in the
//     order they were received, which is the property the daemon's log is read
//     with; bounding the queue keeps a flood from growing daemon memory.
//   - Overflow drops the OLDEST record and COUNTS the drop. Telemetry may be
//     lossy under a flood — that is the one thing it is allowed to be — but a
//     loss is never silent: the drop is reported at warn, naming the workspace
//     and the count, rate-limited so the report cannot itself become the flood.
//
// The ingress path never blocks: submit takes the queue lock, appends, and
// returns. That is the invariant the whole change exists for.

// clientLogQueueCapacity bounds how many unwritten client_log records the
// daemon holds. It is a memory bound on a lossy stream, not a promise: the
// observed flood was thousands deep, and holding thousands of console lines
// serves nobody.
const clientLogQueueCapacity = 512

// clientLogDropReportInterval rate-limits the drop warning. The first drop of
// an episode reports immediately; further drops accumulate and are reported at
// most this often, so the counts stay exact while the log stays readable.
const clientLogDropReportInterval = 5 * time.Second

// clientLogWriteWarnThreshold is the always-on attribution instrumentation for
// the one serial stage the write still shares with everything else the daemon
// logs — see telemetryWriter.serve.
const clientLogWriteWarnThreshold = 1 * time.Second

// telemetryRecord is one accepted client_log and the connection it arrived on.
// The connection rides along because the write is dispatched exactly as the
// lane dispatched it, including the client-kind authority checks.
type telemetryRecord struct {
	cl  *client
	cmd *frontendv1.FrontendCommand
}

// telemetryWriter is the bounded queue plus single writer goroutine described
// above. One is created per connection by its read loop, and closed by it.
type telemetryWriter struct {
	// write performs one record. It is called only from the writer goroutine,
	// so it is never concurrent with itself.
	write func(telemetryRecord)
	// warnf carries the drop accounting and the slow-write attribution. Both
	// are operator-facing: a dropped record is lost evidence and a slow write
	// is the shared stage that couples unrelated workspaces.
	warnf    dlog.Logf
	capacity int
	// reportEvery is clientLogDropReportInterval, injectable for tests.
	reportEvery time.Duration
	// warnAfter is clientLogWriteWarnThreshold, injectable for tests.
	warnAfter time.Duration

	mu    sync.Mutex
	queue []telemetryRecord
	// pending counts drops per workspace since the last report, and total
	// counts them for the writer's lifetime. Both are reported, because "12
	// since the last line" and "3,704 so far" answer different questions.
	pending    map[string]int
	total      map[string]int
	lastReport time.Time
	started    bool
	closed     bool
	// ready is a coalescing wakeup (capacity 1), never the queue itself.
	ready chan struct{}
	done  chan struct{}
}

// newTelemetryWriter builds a writer with the shipped bounds. A nil write or
// warnf is a wiring error, surfaced the way the lanes surface theirs.
func newTelemetryWriter(warnf dlog.Logf, write func(telemetryRecord)) *telemetryWriter {
	if write == nil {
		panic("frontend: telemetry writer requires a write func")
	}
	if warnf == nil {
		panic("frontend: telemetry writer requires a warn logger")
	}
	return &telemetryWriter{
		write:       write,
		warnf:       warnf,
		capacity:    clientLogQueueCapacity,
		reportEvery: clientLogDropReportInterval,
		warnAfter:   clientLogWriteWarnThreshold,
		ready:       make(chan struct{}, 1),
		done:        make(chan struct{}),
	}
}

// submit accepts one record for writing. It NEVER blocks and it never returns
// an error to the ingress path: the only two outcomes are "queued" and
// "dropped and reported".
//
// A submit after close is a drop like any other, and it is reported like any
// other. Unlike the lanes it is not a violated invariant: the writer is closed
// while a record may still be in the ingress path's hands, and a panic there
// would take down a connection over a console line.
func (w *telemetryWriter) submit(rec telemetryRecord) {
	w.mu.Lock()
	if w.closed {
		w.noteDropLocked(rec)
		report := w.dropReportLocked(true)
		w.mu.Unlock()
		w.emit(report)
		return
	}
	var dropped bool
	if len(w.queue) >= w.capacity {
		// DROP-OLDEST: the newest record describes what the frontend is doing
		// NOW, which is what an operator reading a flood needs.
		w.noteDropLocked(w.queue[0])
		w.queue = w.queue[1:]
		dropped = true
	}
	w.queue = append(w.queue, rec)
	if !w.started {
		w.started = true
		go w.serve()
	}
	report := ""
	if dropped {
		report = w.dropReportLocked(false)
	}
	w.mu.Unlock()
	w.signal()
	w.emit(report)
}

// close stops accepting records and waits for the queued ones to be written.
// The queue is DRAINED rather than discarded: those records were accepted, and
// the accepting ack already told the client so.
func (w *telemetryWriter) close() {
	w.mu.Lock()
	if w.closed {
		w.mu.Unlock()
		return
	}
	w.closed = true
	started := w.started
	report := w.dropReportLocked(true)
	w.mu.Unlock()
	w.signal()
	w.emit(report)
	if started {
		<-w.done
	}
}

// serve writes records one at a time, in receipt order, until the writer is
// closed AND drained.
//
// THE ALWAYS-ON ATTRIBUTION LIVES HERE. The write itself still passes through
// stages the whole daemon shares — dlog's TargetManager mutex and its single
// terminal mirror, whose Write applies backpressure to every emitter at once —
// so a flood can still couple unrelated workspaces THROUGH THE LOGGER even
// with the command lanes out of the picture. Restructuring that is a separate
// change; timing it is not, and a record naming the stage and the elapsed is
// what makes the next incident attributable instead of inferred.
func (w *telemetryWriter) serve() {
	defer close(w.done)
	for {
		rec, ok, done := w.next()
		switch {
		case ok:
			started := time.Now()
			w.write(rec)
			if elapsed := time.Since(started); elapsed >= w.warnAfter {
				w.warnf("frontend: client_log telemetry write SLOW stage=dlog_workspace_target ws=%q request_id=%q elapsed_ms=%d queue_depth=%d; this stage is daemon-global (dlog target manager + terminal mirror), so a stall here delays every workspace's logging",
					rec.cmd.GetWorkspace(), rec.cmd.GetRequestId(), elapsed.Milliseconds(), w.depth())
			}
		case done:
			return
		default:
			<-w.ready
		}
	}
}

// next reports the head record, or whether the writer is finished. The three
// results are exclusive: a record, or "closed and drained", or "wait".
func (w *telemetryWriter) next() (rec telemetryRecord, ok bool, done bool) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if len(w.queue) > 0 {
		head := w.queue[0]
		w.queue = w.queue[1:]
		return head, true, false
	}
	return telemetryRecord{}, false, w.closed
}

// depth reports the current queue occupancy for the slow-write record.
func (w *telemetryWriter) depth() int {
	w.mu.Lock()
	defer w.mu.Unlock()
	return len(w.queue)
}

// noteDropLocked counts one lost record against its workspace. Caller holds mu.
func (w *telemetryWriter) noteDropLocked(rec telemetryRecord) {
	ws := rec.cmd.GetWorkspace()
	if w.pending == nil {
		w.pending, w.total = map[string]int{}, map[string]int{}
	}
	w.pending[ws]++
	w.total[ws]++
}

// dropReportLocked returns the warn line owed right now, or "" when the counts
// are empty or the last report is still too recent. force ignores the rate
// limit, which is how a final report is guaranteed on close. Caller holds mu.
func (w *telemetryWriter) dropReportLocked(force bool) string {
	if len(w.pending) == 0 {
		return ""
	}
	now := time.Now()
	if !force && !w.lastReport.IsZero() && now.Sub(w.lastReport) < w.reportEvery {
		return ""
	}
	w.lastReport = now
	line := "frontend: client_log telemetry DROPPED records (queue full, oldest dropped first)"
	for ws, n := range w.pending {
		line += " ws=" + ws + " dropped=" + itoa(n) + " dropped_total=" + itoa(w.total[ws])
	}
	w.pending = map[string]int{}
	return line
}

// emit writes a report line, if there is one. It is always called with no lock
// held: the warn path may itself write to the terminal mirror, and holding the
// queue lock across that would put the ingress path behind the logger.
func (w *telemetryWriter) emit(report string) {
	if report == "" {
		return
	}
	w.warnf("%s", report)
}

// signal posts a coalescing wakeup. A wakeup already pending is the same
// wakeup: the writer re-reads the queue under the lock either way.
func (w *telemetryWriter) signal() {
	select {
	case w.ready <- struct{}{}:
	default:
	}
}

// itoa keeps the drop line free of a fmt dependency in a path that runs during
// a flood.
func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	neg := n < 0
	if neg {
		n = -n
	}
	var buf [20]byte
	i := len(buf)
	for n > 0 {
		i--
		buf[i] = byte('0' + n%10)
		n /= 10
	}
	if neg {
		i--
		buf[i] = '-'
	}
	return string(buf[i:])
}
