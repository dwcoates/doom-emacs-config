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

	mu     sync.Mutex
	queue  []laneItem
	closed bool
	// ready is a coalescing wakeup (capacity 1), never the queue itself.
	ready chan struct{}
}

func newCommandLanes(logf dlog.Logf, run func(t *commandTicket)) *commandLanes {
	return &commandLanes{logf: logf, run: run, lanes: map[string]*commandLane{}}
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
	lane.push(laneItem{ticket: t})
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

// push appends a command and wakes the lane's worker.
func (lane *commandLane) push(item laneItem) {
	lane.mu.Lock()
	lane.queue = append(lane.queue, item)
	lane.mu.Unlock()
	lane.signal()
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
	return laneItem{}, false, lane.closed
}

// close marks the lane closed and returns how much it still holds. The wakeup
// is signalled unconditionally so a worker parked on an empty queue observes
// the closure.
func (lane *commandLane) close() int {
	lane.mu.Lock()
	lane.closed = true
	pending := len(lane.queue)
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
