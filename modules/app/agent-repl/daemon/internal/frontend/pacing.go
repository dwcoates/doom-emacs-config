package frontend

import (
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// Producer pacing — backpressure instead of congestion collapse
// ---------------------------------------------------------------------------
//
// THE FAILURE THIS EXISTS FOR. A gui_stream reconnects, asks for a resync, and
// the daemon answers by pushing the workspace's whole retained conversation —
// thousands of ConversationDeltas — as fast as it can compose them. Every one
// of those frames is IRREPLACEABLE (append semantics: see coalesceKey), so
// compaction can remove none of them, and the queue's ceiling is 256 frames. A
// replay longer than the ceiling therefore does not merely risk an eviction, it
// GUARANTEES one the moment the browser consumes a hair slower than the daemon
// writes:
//
//	slow consumer (gui_stream), outbound buffer full (256 frames, soft 256,
//	hard 256) after compacting 0 superseded frames
//	closing connection ... cause=outbound_overflow: limit=hard_ceiling
//
// The client reconnects, asks for a resync, and the replay starts over from
// zero. That is a per-client congestion collapse — the user watches
// "synchronizing" forever — and the consumer was never the problem: it was
// reading the whole time, just slower than an unbounded producer.
//
// THE FIX IS BACKPRESSURE, NOT A BIGGER BUFFER. A bigger buffer moves the
// cliff; pacing removes it. Before a bulk frame is offered to a connection, the
// producer WAITS for that connection's queue to drain to its low watermark. The
// replay then proceeds at exactly the rate the client consumes, and the queue
// depth stays bounded by construction no matter how long the history is.
//
// THE EVICTION IS NOT WEAKENED, IT IS RE-AIMED, and the aim is copied verbatim
// from the host connection's byte-progress stall verdict (see outbox.progress
// and writeFrameWatched): a consumer is dead when it makes NO OBSERVABLE
// PROGRESS — no frame popped and no byte accepted by the socket — for the whole
// grace period. A client that is genuinely gone still gets hard-disconnected,
// loudly, through the same recordOverflow record and the same closeCause. What
// no longer happens is disconnecting a client that IS draining, merely slower
// than the producer composes.
//
// ORDERING IS UNTOUCHED. Pacing waits BEFORE any delivery decision is made: it
// takes no server lock, parks nothing, and reorders nothing. The push that
// follows it is the same push, into the same lane, in the same order, and
// compaction still rewrites the bulk lane exactly as before — pacing simply
// means the queue is rarely deep enough for compaction to have anything to do.
// Live pushes that arrive mid-replay interleave with the replay exactly as they
// always did: whichever Broadcast reaches the outbox first is queued first.

// paceStallGrace is how long a producer waits on a connection that has made NO
// observable drain progress before giving up on it. It matches hostStallGrace
// deliberately: "wedged" means the same thing for a producer waiting on a queue
// as it does for a queue judging its own consumer, and two different definitions
// of a dead client is exactly how one of them ends up wrong.
const paceStallGrace = 30 * time.Second

// paceRecordThreshold is how long a single pacing wait has to last before it is
// worth a line. See paceClient for why a per-wait record would be a flood.
const paceRecordThreshold = time.Second

// roomVerdict is the outcome of waiting for space in a connection's outbox.
type roomVerdict int

const (
	// roomAvailable — the queue has room; the caller may push.
	roomAvailable roomVerdict = iota
	// roomClosed — the connection went away while waiting. Not a slow consumer:
	// the push that follows reports the frame undelivered through the normal
	// closed-queue path, and there is nothing left to tear down.
	roomClosed
	// roomStalled — the consumer made no observable progress for the whole
	// grace period. This is the load-shedding verdict, preserved intact.
	roomStalled
)

// pacingHighLocked is the depth at which a paced producer stops producing, and
// it is HALF the soft bound rather than the soft bound itself. The lower half
// is a RESERVE, and the reserve is load-bearing: not every push is paced. The
// frames that go out under the delivery lock — WorkspaceState, the snapshot
// lease — are pushed without waiting by design (they are absolute, coalescable
// and their contract is a hard bound), so a paced producer that filled the
// queue right up to the soft bound would hand a live WorkspaceState a queue
// with one slot and evict the connection on the very next one. Producing only
// into the top half leaves the unpaced traffic room to land. Caller holds mu.
func (o *outbox) pacingHighLocked() int {
	if o.soft <= 2 {
		return 1
	}
	return o.soft / 2
}

// pacingLowLocked is the depth a paced producer waits back down to before it
// resumes. Resuming at the same depth that started the wait would wake the
// producer once per drained frame; the gap between the two marks is what lets
// the consumer clear a batch between wakes. Caller holds mu.
func (o *outbox) pacingLowLocked() int {
	if o.soft <= 4 {
		return 1
	}
	return o.soft / 4
}

// awaitRoom blocks until this queue has room for a bulk frame, the connection
// dies, or the consumer is judged wedged.
//
// It returns immediately for a queue below its HIGH watermark, which is every
// ordinary push: the cost on the uncongested path is one mutex acquisition.
// Once it does wait, it waits until the queue is at or below its LOW watermark,
// so a producer that has been paced hands back a queue with real headroom
// rather than one frame's worth.
//
// The stall verdict is a PROGRESS verdict, not a clock one. o.progress counts
// every frame the writer popped AND every chunk of bytes the socket accepted
// mid-write, so a consumer working through one enormous frame is visibly alive
// and a consumer that died holding it is visibly not. Any advance re-arms the
// grace period; only a consumer that advances it not at all for the whole
// period is given up on.
//
// The pushResult it returns carries the same statistics a refused push carries,
// so a stalled connection is reported and closed through recordOverflow and
// causeOverflow unchanged — one vocabulary for giving up on a connection, not
// two.
func (o *outbox) awaitRoom(grace time.Duration) (roomVerdict, pushResult) {
	var (
		waiting        bool
		markProgress   uint64
		waitStart      time.Time
		lastProgressAt time.Time
	)
	for {
		o.mu.Lock()
		res := pushResult{soft: o.soft, hard: o.hard, depth: o.depthLocked()}
		if o.closed {
			res.closed = true
			o.mu.Unlock()
			return roomClosed, res
		}
		depth, high, low := o.depthLocked(), o.pacingHighLocked(), o.pacingLowLocked()
		if (!waiting && depth < high) || (waiting && depth <= low) {
			// entered reports that this producer ACTUALLY waited, so the record
			// downstream fires once per pacing episode rather than once per
			// frame — the same discipline notePressure follows.
			res.overSoft, res.entered = waiting, waiting
			if waiting {
				res.stalledFor = o.now().Sub(waitStart)
			}
			o.mu.Unlock()
			return roomAvailable, res
		}
		if !waiting {
			waiting, markProgress = true, o.progress
			waitStart, lastProgressAt = o.now(), o.now()
		}
		if o.progress != markProgress {
			// The consumer drained a frame or the socket took more bytes. That
			// is life, and it re-arms the whole grace period.
			markProgress, lastProgressAt = o.progress, o.now()
		}
		remaining := grace - o.now().Sub(lastProgressAt)
		if remaining <= 0 {
			res.overSoft = true
			res.reason, res.stalledFor = overflowStalled, o.now().Sub(lastProgressAt)
			o.mu.Unlock()
			return roomStalled, res
		}
		room := o.roomLocked()
		o.mu.Unlock()

		timer := time.NewTimer(remaining)
		select {
		case <-room:
		case <-timer.C:
		}
		timer.Stop()
	}
}

// roomLocked hands back the channel a waiter parks on, creating it on demand so
// a queue built by any route still has one. It is closed and replaced whenever
// the queue drains to its low watermark or dies, which is a BROADCAST: every
// waiter wakes, not one of them. Caller holds mu.
func (o *outbox) roomLocked() chan struct{} {
	if o.room == nil {
		o.room = make(chan struct{})
	}
	return o.room
}

// signalRoomLocked wakes every producer waiting on this queue. Caller holds mu.
func (o *outbox) signalRoomLocked() {
	if o.room == nil {
		return
	}
	close(o.room)
	o.room = nil
}

// ---------------------------------------------------------------------------
// The server side of pacing
// ---------------------------------------------------------------------------

// paceBulkFrame paces a bulk broadcast against every connection that could
// receive it, BEFORE any delivery lock is taken and before the publication gate
// is consulted, so a paced wait holds nothing anyone else needs.
//
// Control frames never come through here. A correlated reply is one frame, it
// is drained ahead of the bulk lane by construction, and making a command's ack
// wait on a slow browser's history replay is precisely the head-of-line
// coupling the two-lane outbox exists to prevent.
//
// The clients are waited on one at a time, which is not the same as waiting for
// the sum of their delays: every other connection keeps draining while this
// one is waited on, so the cost is the slowest connection's, not the total.
func (s *Server) paceBulkFrame(frame *frontendv1.FrontendFrame) {
	s.mu.Lock()
	clients := make([]*client, 0, len(s.clients))
	for cl := range s.clients {
		clients = append(clients, cl)
	}
	s.mu.Unlock()
	for _, cl := range clients {
		if isHostOnlyFrame(frame) && !cl.kind.isHost() {
			continue
		}
		s.paceClient(cl)
	}
}

// paceClient blocks until cl has room for a bulk frame, disconnecting it — for
// the same recorded cause a refused push would have used — when it is wedged.
// A connection that died while being waited on is left alone: the push that
// follows reports its own frame undelivered, and disconnecting a gone client a
// second time records a cause for a teardown that already has one.
func (s *Server) paceClient(cl *client) {
	verdict, res := cl.out.awaitRoom(s.paceGrace)
	switch verdict {
	case roomStalled:
		s.recordOverflow(cl, res, pacingPhase)
		s.disconnect(cl, causeOverflow(res, pacingPhase))
	case roomAvailable:
		// ONE LINE PER SUSTAINED WAIT, not one per frame. Pacing a slow browser
		// through a long replay is thousands of short waits and each of them is
		// the mechanism WORKING; recording every one would reproduce, in the
		// daemon log, exactly the flood this change removes from the client's.
		// A wait long enough to be worth an operator's attention is not short.
		if res.entered && res.stalledFor >= paceRecordThreshold {
			s.logVerbosef("frontend: producer paced client_id=%d kind=%s phase=%s waited_ms=%d depth=%d soft=%d hard=%d; waited for the consumer to drain rather than overflowing it",
				cl.id, cl.kind, pacingPhase, res.stalledFor.Milliseconds(), res.depth, res.soft, res.hard)
		}
	case roomClosed:
	}
}

// pacingPhase names the pacing wait in every record it produces, so an eviction
// that happened while a producer was waiting is never confused with one that
// happened at a push.
const pacingPhase = "pacing"
