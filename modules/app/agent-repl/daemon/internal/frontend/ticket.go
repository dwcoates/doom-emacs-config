package frontend

import (
	"sync"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// Command tickets: one accounting object per received command
// ---------------------------------------------------------------------------
//
// A command's in-flight accounting used to live as two loose statements around
// the dispatch — `inflight.Add(1)` in the read loop, `inflight.Add(-1)` near
// the bottom of processCommand — with the latency record written after the
// second one. That arrangement had two holes, and both of them show up as a
// queue_depth an operator cannot explain:
//
//   - ANY non-local exit from processCommand skipped both the decrement and the
//     record. A panic unwinding out of a handler released nothing, so every
//     later command reported a queue_depth inflated by the leak for as long as
//     the daemon lived.
//   - A command that has not finished YET produces no evidence at all. A real
//     open_workspace bring-up has been observed taking 83s and 123s — more than
//     eight and twelve times the client's ack deadline — and for that whole
//     interval the only visible symptom was other commands reporting a high
//     queue_depth with nothing to attribute it to. If the daemon exited or its
//     log rotated first, the record was never written at all.
//
// A commandTicket closes both. It is created at receipt, carried through the
// lane queue, and settled EXACTLY ONCE by a deferred call in processCommand, so
// the decrement and the completion record happen on every path out — success,
// nack, teardown drain, or panic. It also arms an ack-deadline watchdog, so a
// command still in flight past the deadline announces itself at warn while it
// is still running rather than only in retrospect.

// commandTicket is one received command's in-flight accounting.
//
// The completion record and the gauge release are the same event, guarded by
// one mutex: a ticket that has recorded its completion has released the gauge,
// and a ticket that has released the gauge has recorded its completion. Nothing
// can observe one without the other.
type commandTicket struct {
	server   *Server
	cl       *client
	cmd      *frontendv1.FrontendCommand
	received time.Time
	depth    int64

	// mu guards settled AND serializes the two emitters against each other, so
	// the watchdog can never announce a command that had already completed.
	mu sync.Mutex
	// settled is true once the completion record was written and the in-flight
	// gauge released.
	settled bool
	// watchdog fires once at the ack deadline. It is stopped on settle; a fire
	// that races the settle finds settled true and emits nothing.
	watchdog *time.Timer
}

// newCommandTicket registers one received command and arms its ack-deadline
// watchdog. The caller has ALREADY incremented the in-flight gauge and passes
// the resulting depth, because the depth a record reports is the one observed
// at receipt, before any of this command's own work ran.
func (s *Server) newCommandTicket(cl *client, cmd *frontendv1.FrontendCommand, received time.Time, depth int64) *commandTicket {
	t := &commandTicket{server: s, cl: cl, cmd: cmd, received: received, depth: depth}
	t.watchdog = time.AfterFunc(s.ackDeadline, t.reportOverdue)
	return t
}

// finish releases the in-flight gauge and writes this command's one completion
// record. It is called from a defer, so it runs on every exit from the dispatch
// including a panic unwinding through it — the panic still propagates, and the
// gauge still comes back.
//
// A second call is a no-op rather than a second decrement: the gauge may never
// go below the number of commands genuinely in flight either.
func (t *commandTicket) finish(ack *frontendv1.CommandAck, processing time.Duration) {
	elapsed := time.Since(t.received)
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.settled {
		return
	}
	t.settled = true
	t.watchdog.Stop()
	t.server.inflight.Add(-1)
	t.server.recordCommandLatency(commandLatencyRecord{
		ticket: t, ack: ack, elapsed: elapsed, processing: processing,
	})
}

// reportOverdue announces a command that is STILL RUNNING past the client's ack
// deadline. It deliberately does not release the gauge: the command has not
// finished, the depth it contributes is real, and its completion record still
// comes from finish. The record is therefore an in-flight observation, not a
// second completion record — it carries its own operation name so a census of
// completions stays exact.
func (t *commandTicket) reportOverdue() {
	elapsed := time.Since(t.received)
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.settled {
		return
	}
	t.server.recordCommandLatency(commandLatencyRecord{
		ticket: t, elapsed: elapsed, overdue: true,
	})
}

// commandLatencyRecord is one emission's inputs. Grouping them keeps the two
// call sites above from drifting in argument order as the sample grows.
type commandLatencyRecord struct {
	ticket     *commandTicket
	ack        *frontendv1.CommandAck
	elapsed    time.Duration
	processing time.Duration
	overdue    bool
}
