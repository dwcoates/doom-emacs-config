package frontend

import (
	"errors"
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

// A command's life has TWO ends, and the record waits for both.
//
// Dispatch finishing is when the daemon is done with the command; the ack
// reaching the socket is when the CLIENT's wait ends. Those used to be treated
// as one moment, because the ack was handed to a FIFO shared with bulk pushes
// and the record was written at the hand-off. With a 104-workspace connect
// snapshot queued ahead of it, a command processed in ~12ms had its ack
// DELIVERED ~15s later — past the client's 10s deadline — and the daemon's own
// slow-ack alarm saw only the 12ms, because it stopped at the enqueue. The
// dominant term was invisible to the exact alarm that existed to catch it.
//
// So a ticket now carries both numbers and emits once, from whichever half
// completes second: Enqueue (receipt → ack handed to the outbound queue, the
// daemon's own share) and Delivery (receipt → those bytes on the socket, what
// the client actually waits out). The threshold warn judges Delivery.

// commandTicket is one received command's in-flight accounting.
//
// The gauge release is still tied to dispatch finishing, and is still exactly
// once. The completion RECORD is tied to both halves being known, and is also
// exactly once: recorded is set under the same mutex both halves take, so no
// schedule can produce two records or none.
type commandTicket struct {
	server   *Server
	cl       *client
	cmd      *frontendv1.FrontendCommand
	received time.Time
	depth    int64

	// mu guards every field below AND serializes the emitters against each
	// other, so the watchdog can never announce a command already recorded.
	mu sync.Mutex
	// settled is true once dispatch finished and the in-flight gauge was
	// released. It is the guard on the gauge, not on the record.
	settled bool
	// recorded is true once this command's ONE completion record was written.
	recorded bool
	// awaitingAck is set the moment an ack frame is handed to the outbound
	// queue, and it is what makes the record wait for a delivery rather than
	// hang on one that is never coming. A dispatch that never got that far —
	// it panicked, or its ack would not marshal — leaves it false, and the
	// record is written from finish alone.
	awaitingAck bool
	// completion holds dispatch's facts until the ack's disposition is known.
	completion *commandCompletion
	// disposition holds the ack's delivery facts until dispatch has finished.
	disposition *ackDisposition
	// watchdog fires once at the client's ack deadline. It is stopped when the
	// record is written — NOT when dispatch finishes — because a command whose
	// ack is still stuck behind a backlog is precisely the case it exists to
	// announce.
	watchdog *time.Timer
}

// commandCompletion is dispatch's half of a completion record.
type commandCompletion struct {
	ack        *frontendv1.CommandAck
	enqueue    time.Duration
	processing time.Duration
}

// ackDisposition is the wire's half: when the ack's bytes reached the socket,
// or the reason they never will.
type ackDisposition struct {
	elapsed time.Duration
	err     error
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
	enqueue := time.Since(t.received)
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.settled {
		return
	}
	t.settled = true
	t.server.inflight.Add(-1)
	t.completion = &commandCompletion{ack: ack, enqueue: enqueue, processing: processing}
	t.emitLocked()
}

// ackDisposed is the ack frame's terminal disposition, reported by whoever
// consumed or abandoned it: err is nil once its bytes reached the socket, and
// names the reason otherwise. It is the outbox notify hook, so it runs with no
// queue lock held.
//
// It is called exactly once per pushed ack frame, and ackUndeliverable stands in
// for it when no frame was ever pushed at all.
func (t *commandTicket) ackDisposed(err error) {
	elapsed := time.Since(t.received)
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.disposition != nil {
		return
	}
	t.disposition = &ackDisposition{elapsed: elapsed, err: err}
	t.emitLocked()
}

// ackUndeliverable records that this command's ack will never reach the wire at
// all — it could not even be marshalled. The record is still owed: a command
// that ran and whose answer vanished is the loudest case there is, not a silent
// one.
func (t *commandTicket) ackUndeliverable(err error) { t.ackDisposed(err) }

// expectAckDelivery declares that an ack frame has been handed to the outbound
// queue and that its disposition is therefore coming. It is called BEFORE the
// push, so no writer can report a delivery this ticket is not yet expecting.
func (t *commandTicket) expectAckDelivery() {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.awaitingAck = true
}

// errNoAckFrame is the delivery verdict for a command whose dispatch never
// produced an ack frame at all — it panicked on the way to one. The client is
// left waiting on an answer that does not exist, so the record says so rather
// than reporting a delivery that never happened as though it had.
var errNoAckFrame = errors.New("frontend: the dispatch produced no ack frame to deliver")

// emitLocked writes this command's ONE completion record once everything it can
// still learn is known, and disarms the overdue watchdog at the same moment.
//
// It waits for the ack's disposition only when one is actually owed. Caller
// holds mu.
func (t *commandTicket) emitLocked() {
	if t.recorded || t.completion == nil {
		return
	}
	if t.awaitingAck && t.disposition == nil {
		return
	}
	t.recorded = true
	t.watchdog.Stop()
	rec := commandLatencyRecord{
		ticket:     t,
		ack:        t.completion.ack,
		enqueue:    t.completion.enqueue,
		processing: t.completion.processing,
		// No ack frame exists, so the client's wait has no end to report. The
		// enqueue elapsed is all this command ever produced.
		delivery:    t.completion.enqueue,
		deliveryErr: errNoAckFrame,
	}
	if t.disposition != nil {
		rec.delivery, rec.deliveryErr = t.disposition.elapsed, t.disposition.err
	}
	t.server.recordCommandLatency(rec)
}

// reportOverdue announces a command that is STILL RUNNING past the client's ack
// deadline. It deliberately does not release the gauge: the command has not
// finished, the depth it contributes is real, and its completion record still
// comes from finish. The record is therefore an in-flight observation, not a
// second completion record — it carries its own operation name so a census of
// completions stays exact.
// It is gated on the RECORD rather than on dispatch finishing, which is the
// second half of this alarm's reach: a command whose handler returned in
// milliseconds but whose ack is still stuck behind a bulk backlog is exactly
// the failure this class is made of, and the client is still waiting on it.
func (t *commandTicket) reportOverdue() {
	elapsed := time.Since(t.received)
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.recorded {
		return
	}
	t.server.recordCommandLatency(commandLatencyRecord{
		ticket: t, enqueue: elapsed, delivery: elapsed, overdue: true,
	})
}

// commandLatencyRecord is one emission's inputs. Grouping them keeps the call
// sites above from drifting in argument order as the sample grows.
type commandLatencyRecord struct {
	ticket *commandTicket
	ack    *frontendv1.CommandAck
	// enqueue is receipt through the ack being handed to the outbound queue.
	enqueue time.Duration
	// delivery is receipt through the ack's bytes reaching the socket. On an
	// overdue observation it is how long the command has been waiting so far —
	// a lower bound on the delivery that has not happened yet.
	delivery time.Duration
	// deliveryErr names why an ack never reached the socket, nil when it did.
	deliveryErr error
	processing  time.Duration
	overdue     bool
}
