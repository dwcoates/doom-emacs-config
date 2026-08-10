package frontend

import (
	"fmt"
	"sync"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
)

// ---------------------------------------------------------------------------
// Per-workspace command lanes
// ---------------------------------------------------------------------------
//
// A connection's inbound commands used to run one at a time, INLINE in its
// read loop: the daemon read a command, performed the whole command, answered
// it, and only then read the next one. That made a connection's command
// throughput the sum of every command's cost, and a workspace bring-up costs
// seconds. At Emacs startup the host opens every restored workspace at once,
// so sixteen open_workspace commands serialized behind each other blew the
// client's ack deadline and opened a `client.command_unacked' failure card per
// starved command — the daemon had not refused anything, it simply had not
// read the command yet.
//
// The ACK CONTRACT is why the fix is not "ack on receipt". The Emacs client
// treats a successful CommandAck as COMPLETION, not receipt: openWorkspace's
// async bridge resolves ACCEPTED on the ack, createSession unblocks its await
// loop on it, restartSession reports "session restarted" from it. Acking early
// would tell the user their workspace was open before any shim existed. So the
// ack still means exactly what it meant — the command ran, and this is its
// outcome — and what changes is only HOW MANY commands may be running.
//
// Commands are therefore routed onto lanes keyed by workspace. One lane runs
// its commands strictly in arrival order, so a workspace's open_workspace
// still precedes every session command that followed it on the wire and no
// per-workspace ordering assumption anywhere downstream is weakened. Lanes run
// concurrently with each other, so a startup costs max(bring-up) rather than
// sum(bring-ups), and a workspace wedged in bring-up can no longer delay
// another workspace's ack. Commands with no workspace (the roster publish, the
// daemon-global controls) share one global lane, which is likewise never
// behind any workspace's bring-up.
//
// Concurrency across workspaces is not a new demand on the CommandHandler: a
// UDS host connection and every webview connection already ran their read
// loops in parallel, so the handler was always called concurrently. What is
// new is that ONE connection may have several commands in flight, and they are
// always for different workspaces.

// globalLaneKey names the lane every workspace-less command runs on. It is not
// a valid workspace directory, so it can never collide with a real lane.
const globalLaneKey = "\x00global"

// ---------------------------------------------------------------------------
// Resync coalescing
// ---------------------------------------------------------------------------
//
// A RESYNC IS SUPERSEDING, and it is the one command class whose backlog is
// self-sustaining. Its meaning is "make me current AS OF NOW", so a newer
// resync answers everything an older queued one asked for and more: replaying
// the older one first can only re-send state the newer replay is about to send
// again. That is harmless when a lane holds one of them and ruinous when it
// holds thousands.
//
// It went ruinous the moment webview recovery stopped being visibility-gated.
// A backgrounded page used to re-arm its resync at the throttled rate the
// browser allowed it, which bounded the flood by accident; retrying at full
// speed removed the bound, and the observed queue reached 5,069 commands with
// resyncs completing 420-550 SECONDS after they were read off the socket. The
// flood then FEEDS ITSELF: a resync unanswered past the client's ack deadline
// is exactly what makes that client re-arm another one.
//
// So resyncs coalesce at intake. When one arrives for a lane that already
// holds a QUEUED resync, the older queued entry is dropped and answered as
// superseded — the newer entry ahead of every command behind it discharges
// what the older one asked for. Two properties make that safe rather than a
// silent loss:
//
//   - Only QUEUED entries are eligible. next() removes an entry from the queue
//     before its command runs, so an EXECUTING resync is not in the queue and
//     can never be dropped by this.
//   - The superseding entry is on the SAME lane's FIFO, so it is already
//     guaranteed to run — including during close(), which still drains what a
//     lane holds — before anything queued behind it.
//
// Nothing else coalesces. Every other command is a distinct instruction whose
// effect a later one does not contain.

// maxQueuedResyncPerLane bounds how many resyncs one lane may hold queued at
// once. Superseding coalescing already keeps a lane at one queued resync, so
// this is a BACKSTOP rather than the mechanism: if the coalescing predicate is
// ever narrowed (per-fence, say) so two queued resyncs can coexist, the depth a
// lane can reach stays bounded by construction instead of by that predicate's
// continued breadth.
const maxQueuedResyncPerLane = 2

// isResync reports whether a command is a conversation resync.
func isResync(cmd *frontendv1.FrontendCommand) bool { return cmd.GetResync() != nil }

// ---------------------------------------------------------------------------
// Low-priority sublane
// ---------------------------------------------------------------------------
//
// A client_log is EVIDENCE, not an operation: nothing waits on its outcome and
// no user-visible state changes when it runs. It nevertheless rides the same
// per-workspace lane as that workspace's opens, prompts and resyncs, because it
// carries the workspace it came from. Sixteen refreshed webviews echoing their
// console records drove one connection's queue to 2,355 commands, and every
// interactive command that arrived behind that burst waited the whole burst out
// — a lane FIFO is a FIFO.
//
// So each lane keeps TWO queues and serves the interactive one first. The
// resulting guarantees are exactly the three that matter:
//
//   - Interactive commands keep their arrival order among themselves. They all
//     sit in one queue, untouched; only client_log moved out of it.
//   - client_log keeps its own relative order. The low queue is a FIFO too, so
//     the daemon's log still reads in the order the frontend emitted.
//   - An EXECUTING client_log is never preempted. next() removes an entry from
//     its queue before the worker runs it, so priority is a question asked at
//     dequeue time and never a question asked about the command in progress.
//
// Nothing else is low priority. The class is deliberately one command whose
// definition is "the daemon writes it and does not act on it"; a command whose
// completion something waits for must never be deferred behind another's.

// isLowPriority reports whether a command may yield its lane to an interactive
// command that arrived later.
func isLowPriority(cmd *frontendv1.FrontendCommand) bool { return cmd.GetClientLog() != nil }

// laneKey names the serialization domain a command belongs to. Ordering is
// promised per workspace, and the wire field the daemon routes every command
// by is the workspace, so the workspace IS the lane.
func laneKey(cmd *frontendv1.FrontendCommand) string {
	if ws := cmd.GetWorkspace(); ws != "" {
		return ws
	}
	return globalLaneKey
}

// commandLanes owns one connection's set of lanes. It is created by the read
// loop, fed only by that read loop, and closed when the read loop ends.
type commandLanes struct {
	logf dlog.Logf
	// run performs one command to completion, including answering it. It is
	// called from a lane goroutine and may be called concurrently for
	// different lanes. It is handed the command's TICKET rather than the bare
	// command: the receipt instant and in-flight depth the read loop captured
	// ride along on it, so command-latency telemetry keeps timing the interval
	// the CLIENT waits out — receipt through ack — with the lane's own queue
	// wait included, not just the handler's processing time, and the ticket's
	// deferred settle is what releases the in-flight gauge.
	run func(t *commandTicket)
	// debugf carries the per-coalescing accounting. It is the verbose channel
	// on purpose: one line per superseded resync is diagnostic detail during a
	// flood, not an operator alarm.
	debugf dlog.Logf
	// supersede ANSWERS a queued command this lane dropped. It is not optional
	// and it is not "best effort": the entry was already read off the socket,
	// so the client is waiting on it, and a drop with no answer would produce
	// exactly the unacked-command failure the coalescing exists to end.
	supersede func(t *commandTicket)

	mu     sync.Mutex
	lanes  map[string]*commandLane
	closed bool
	wg     sync.WaitGroup
}

// laneItem is one queued command's ticket, which carries both the command and
// the receipt-side facts its eventual execution must report against.
type laneItem struct {
	ticket *commandTicket
}

// commandLane is one serialization domain's FIFO plus its worker's wakeup.
// The queue is unbounded on purpose: every frame in it has already been read
// off the socket and OWES the client an answer, so dropping one would swallow
// a command the client is waiting on. The backlog that used to sit in the
// socket's receive buffer simply sits here instead.
type commandLane struct {
	key string

	mu    sync.Mutex
	queue []laneItem
	// lowQueue holds the lane's low-priority commands (see isLowPriority). It
	// is served only when queue is empty, and it is a FIFO of its own so the
	// deferred commands keep their emission order relative to each other.
	lowQueue []laneItem
	closed   bool
	// ready is a coalescing wakeup (capacity 1), never the queue itself.
	ready chan struct{}
}

func newCommandLanes(logf, debugf dlog.Logf, run, supersede func(t *commandTicket)) *commandLanes {
	if supersede == nil {
		panic("frontend: command lanes require a supersede answerer")
	}
	return &commandLanes{
		logf:      logf,
		debugf:    debugf,
		run:       run,
		supersede: supersede,
		lanes:     map[string]*commandLane{},
	}
}

// submit hands a command to its lane, starting that lane's worker on first
// use. It never blocks on command execution, which is the whole point: the
// read loop returns to the socket immediately.
//
// A submit after close is a VIOLATED INVARIANT, not a runtime condition: the
// read loop is the only submitter and it closes the lanes only after it has
// stopped reading. Coping with it would mean either dropping a command that
// was already taken off the socket or running it outside the ordering the
// lanes exist to guarantee, so it panics with the offending identity instead
// (the same loudness New uses for a missing dependency).
func (l *commandLanes) submit(t *commandTicket) {
	cmd := t.cmd
	key := laneKey(cmd)
	l.mu.Lock()
	if l.closed {
		l.mu.Unlock()
		l.logf("frontend: command lane %q received request_id=%s AFTER the connection's lanes closed; the read loop is the only submitter and it closes the lanes only once it has stopped reading",
			key, cmd.GetRequestId())
		panic(fmt.Sprintf("frontend: command submitted to closed lanes (lane=%q request_id=%q)", key, cmd.GetRequestId()))
	}
	lane, ok := l.lanes[key]
	if !ok {
		lane = &commandLane{key: key, ready: make(chan struct{}, 1)}
		l.lanes[key] = lane
		l.wg.Add(1)
		go func() {
			defer l.wg.Done()
			l.serve(lane)
		}()
	}
	l.mu.Unlock()
	superseded, queued := lane.push(laneItem{ticket: t})
	// Answered OUTSIDE the lane's lock: the answer marshals a frame and hands
	// it to the client's outbound queue, and none of that may run with a lane's
	// intake blocked behind it.
	for _, old := range superseded {
		l.debugf("frontend: resync coalesced lane=%q superseded_request_id=%q by_request_id=%q superseded=%d queued_resyncs=%d lane_depth=%d",
			key, old.ticket.cmd.GetRequestId(), cmd.GetRequestId(), len(superseded), queued.resyncs, queued.depth)
		l.supersede(old.ticket)
	}
}

// laneDepth is one lane's post-push occupancy, reported for the coalescing log.
type laneDepth struct {
	// depth is every queued entry, of any command.
	depth int
	// resyncs is how many of those are resyncs.
	resyncs int
	// low is how many of those are on the low-priority sublane.
	low int
}

// serve runs one lane's commands in arrival order until the lane is closed AND
// drained.
func (l *commandLanes) serve(lane *commandLane) {
	for {
		item, ok, done := lane.next()
		switch {
		case ok:
			l.run(item.ticket)
		case done:
			return
		default:
			<-lane.ready
		}
	}
}

// close stops accepting new commands and waits for every lane to finish what
// it already holds. Commands read but not yet run are still performed and
// answered: they were taken off the socket, so abandoning them would be a
// silent loss of work the client asked for.
func (l *commandLanes) close() {
	l.mu.Lock()
	l.closed = true
	lanes := make([]*commandLane, 0, len(l.lanes))
	for _, lane := range l.lanes {
		lanes = append(lanes, lane)
	}
	l.mu.Unlock()
	for _, lane := range lanes {
		if pending := lane.close(); pending > 0 {
			l.logf("frontend: command lane %q still holds %d read-but-unrun command(s) as the connection ends; each still runs and is answered",
				lane.key, pending)
		}
	}
	l.wg.Wait()
}

// push appends a command and wakes the lane's worker. It returns the queued
// entries this push SUPERSEDED — always resyncs, always still queued, never the
// one executing — together with the lane's occupancy after the push, and the
// caller owes every returned entry an answer.
func (lane *commandLane) push(item laneItem) (superseded []laneItem, queued laneDepth) {
	lane.mu.Lock()
	if isLowPriority(item.ticket.cmd) {
		lane.lowQueue = append(lane.lowQueue, item)
		queued = lane.depthLocked()
		lane.mu.Unlock()
		lane.signal()
		return nil, queued
	}
	if isResync(item.ticket.cmd) {
		superseded = lane.takeQueuedResyncsLocked()
	}
	lane.queue = append(lane.queue, item)
	superseded = append(superseded, lane.trimQueuedResyncsLocked()...)
	queued = lane.depthLocked()
	lane.mu.Unlock()
	lane.signal()
	return superseded, queued
}

// takeQueuedResyncsLocked removes every queued resync and returns it. This is
// the coalescing proper: the arriving resync is newer than all of them, and a
// lane is one workspace, so each removed entry asked for a replay the arriving
// one contains. Caller holds lane.mu.
func (lane *commandLane) takeQueuedResyncsLocked() []laneItem {
	var taken []laneItem
	kept := lane.queue[:0]
	for _, q := range lane.queue {
		if isResync(q.ticket.cmd) {
			taken = append(taken, q)
			continue
		}
		kept = append(kept, q)
	}
	lane.queue = kept
	return taken
}

// trimQueuedResyncsLocked enforces maxQueuedResyncPerLane by removing the
// OLDEST queued resyncs beyond the bound, returning them for their answer.
// Coalescing normally leaves nothing for it to do; it is the backstop that
// keeps the bound a property of the lane rather than of the coalescing
// predicate. Caller holds lane.mu.
func (lane *commandLane) trimQueuedResyncsLocked() []laneItem {
	over := lane.depthLocked().resyncs - maxQueuedResyncPerLane
	if over <= 0 {
		return nil
	}
	var trimmed []laneItem
	kept := lane.queue[:0]
	for _, q := range lane.queue {
		if over > 0 && isResync(q.ticket.cmd) {
			over--
			trimmed = append(trimmed, q)
			continue
		}
		kept = append(kept, q)
	}
	lane.queue = kept
	return trimmed
}

// depthLocked reports the lane's occupancy. Caller holds lane.mu.
func (lane *commandLane) depthLocked() laneDepth {
	d := laneDepth{depth: len(lane.queue) + len(lane.lowQueue), low: len(lane.lowQueue)}
	for _, q := range lane.queue {
		if isResync(q.ticket.cmd) {
			d.resyncs++
		}
	}
	return d
}

// next reports the head command, or whether the lane is finished. The three
// results are exclusive: a command, or "closed and drained", or "wait".
func (lane *commandLane) next() (item laneItem, ok bool, done bool) {
	lane.mu.Lock()
	defer lane.mu.Unlock()
	if len(lane.queue) > 0 {
		head := lane.queue[0]
		lane.queue = lane.queue[1:]
		return head, true, false
	}
	// Only once the lane owes no interactive work does it spend itself on
	// evidence. A closing lane still drains this queue: those commands were
	// read off the socket and are owed an answer like any other.
	if len(lane.lowQueue) > 0 {
		head := lane.lowQueue[0]
		lane.lowQueue = lane.lowQueue[1:]
		return head, true, false
	}
	return laneItem{}, false, lane.closed
}

// close marks the lane closed and returns how much it still holds. The wakeup
// is signalled unconditionally so a worker parked on an empty queue observes
// the closure.
func (lane *commandLane) close() int {
	lane.mu.Lock()
	lane.closed = true
	pending := len(lane.queue) + len(lane.lowQueue)
	lane.mu.Unlock()
	lane.signal()
	return pending
}

// signal posts a coalescing wakeup. A wakeup already pending is the same
// wakeup: the worker re-reads the queue under the lock either way.
func (lane *commandLane) signal() {
	select {
	case lane.ready <- struct{}{}:
	default:
	}
}
