package frontend

import (
	"sync"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// Frame supersession
// ---------------------------------------------------------------------------

// coalesceKey names the piece of frontend state a frame REPLACES WHOLESALE.
// Two queued frames sharing a non-empty key are redundant with each other: the
// newer one alone leaves the consumer in exactly the state both would have. An
// empty key means the frame is irreplaceable — it either carries append
// semantics (the consumer GROWS something with it) or it is a one-shot event
// whose delivery is the whole point — and it is never dropped for any reason.
//
// The classification is evidence-based, taken from how the two frontends APPLY
// each frame (webapp/src/store.ts, webapp/src/state-adapter.ts) rather than
// from the frame's name:
//
//   - WorkspaceState — ABSOLUTE. store.applyWorkspaceState assigns every
//     projected field (render state, connectivity, session status, controller
//     generation, faults, merge status) from the frame; Emacs adopts it the
//     same way. It is a resolved state stamped with at_ms, not a transition.
//     Keyed by workspace: it is THE per-workspace state, and a session
//     rebinding is itself just a newer value of it.
//
//   - ProgressView — ABSOLUTE. store.applyProgress does `this.progress = p`.
//     The proto calls its tickers "latest-wins, coalesced daemon-side", so a
//     second coalescing point downstream is the same operation, not a new
//     policy. Keyed by workspace+session.
//
//   - QueueView — ABSOLUTE. store.applyQueue does
//     `this.state.queued = q.entries.map(...)`, and the proto states the queue
//     is "pushed on EVERY change" with an empty list a meaningful value. The
//     frame carries the whole queue, never a mutation of it. Keyed by
//     workspace+session.
//
//   - HeartbeatView — ABSOLUTE per running tool. store.applyToolProgress does
//     `item.progressElapsedS = p.elapsedSeconds`, looked up by tool_use_id, so
//     a later heartbeat for the same tool overwrites the earlier one's only
//     effect. Keyed by workspace+session+tool_use_id, because two tools running
//     concurrently do NOT supersede each other.
//
//   - WorkspaceRoster — ABSOLUTE, and keyed GLOBALLY with no discriminator at
//     all. The proto states the roster is "always whole, never a delta", the
//     frontends replace their entire sidebar model with it, and there is
//     exactly one roster for the whole editor. So any two queued rosters are
//     redundant with each other and the newer one alone is the whole truth.
//     The daemon already refuses a publication that does not advance the
//     revision, so the survivor of a compaction is always the newest revision.
//
// Everything else is irreplaceable, and the two that most look coalescable are
// deliberately not:
//
//   - TypingDelta — INCREMENTAL, NOT absolute. It carries a core.v1
//     ContentDelta (a text/thinking/input_json CHUNK), and
//     store.applyTyping feeds it to applyStreamDelta, which GROWS the open
//     block. Dropping one deletes prose from the transcript, so typing deltas
//     are never coalesced.
//
//   - StateSnapshot — absolute, but excluded on purpose. The GUI stream's
//     snapshot lease is the browser's bounded freshness proof, and this
//     package's contract is that a full lease queue is a hard disconnect,
//     never a silent skip (see AGENTS.md). Collapsing queued snapshots would
//     turn that proof into an unbounded one.
//
// ConversationDelta, SessionView, SessionInitView, TaskCatalog, CommandAck,
// DaemonView, health views, WorkspaceAvailable and HostAction are all either
// append-semantic, correlated one-shot replies, or durable host work that must
// be consumed exactly once. None are coalescable.
func coalesceKey(frame *frontendv1.FrontendFrame) string {
	switch f := frame.GetFrame().(type) {
	case *frontendv1.FrontendFrame_WorkspaceState:
		return "workspace_state\x00" + f.WorkspaceState.GetWorkspace()
	// Keyed by WORKSPACE alone. These pushes carry a fence rather than a session
	// id now, and two queued absolute views of one workspace are redundant with
	// each other whether or not the fence rotated between them — the newer is
	// the current truth either way, which is the whole premise of coalescing.
	case *frontendv1.FrontendFrame_Progress:
		return "progress\x00" + f.Progress.GetWorkspace()
	case *frontendv1.FrontendFrame_Queue:
		return "queue\x00" + f.Queue.GetWorkspace()
	// The three RESOLVED-VIEW families, each ABSOLUTE for its workspace: a
	// topbar carries the whole topbar, a breakdown the whole section tree, a
	// gate the whole gate. Every one of them is published only when the
	// RENDERED facts change (internal/server/workspaceviews.go), so a queued
	// pair is a consumer that fell behind two genuine changes and the newer
	// alone is what both would have left it in.
	case *frontendv1.FrontendFrame_Topbar:
		return "topbar\x00" + f.Topbar.GetWorkspace()
	case *frontendv1.FrontendFrame_TokenBreakdown:
		return "token_breakdown\x00" + f.TokenBreakdown.GetWorkspace()
	case *frontendv1.FrontendFrame_WorkspaceGate:
		return "workspace_gate\x00" + f.WorkspaceGate.GetWorkspace()
	case *frontendv1.FrontendFrame_Heartbeat:
		return "heartbeat\x00" + f.Heartbeat.GetWorkspace() + "\x00" +
			f.Heartbeat.GetProgress().GetToolUseId()
	case *frontendv1.FrontendFrame_WorkspaceRoster:
		return "workspace_roster"
	case *frontendv1.FrontendFrame_ShutdownSchedule:
		// ABSOLUTE and keyed GLOBALLY, exactly as the roster is. The view is
		// always the WHOLE lease — schedule plus its complete holds list — so
		// any two queued ones are redundant with each other and the newer alone
		// is the whole truth. Nothing incremental is lost by dropping the older.
		return "shutdown_schedule"
	default:
		return ""
	}
}

// ---------------------------------------------------------------------------
// The per-client outbound queue
// ---------------------------------------------------------------------------

// notifyFunc reports one queued frame's TERMINAL disposition, exactly once: nil
// once its bytes reached the socket, non-nil when the frame will never get
// there (the write failed, the connection was torn down, or the push itself was
// refused). It is nil for the frames nobody is waiting on.
//
// It is called with no queue lock held, by whoever consumed or abandoned the
// frame, so a callback may take any lock it likes — including one a producer
// holds while pushing.
type notifyFunc func(error)

// lane names which of the outbox's two queues a frame leaves by. It is a named
// type rather than a bare bool at the CALL sites, because "which ordering
// guarantee does this frame need" is the whole decision and `true` does not say
// it.
type lane bool

const (
	// bulkLane is the state-carrying FIFO: snapshots, deltas, resolved views.
	// Its order is load-bearing and nothing overtakes anything in it.
	bulkLane lane = false
	// controlLane is for correlated, request-keyed replies, which carry no
	// state-ordering obligation and are drained ahead of the bulk lane.
	controlLane lane = true
)

// outFrame is one queued wire payload plus the state it supersedes. key is
// empty for a frame nothing may replace.
type outFrame struct {
	key  string
	data []byte
	// control marks a frame as belonging to the CONTROL LANE: a correlated,
	// request-keyed reply that carries no state-ordering obligation. See
	// outbox.control for why that lane may overtake the bulk one.
	control bool
	// notify, when non-nil, is told this frame's terminal disposition.
	notify notifyFunc
}

// overflowReason names WHY a push was refused. It is empty for a queued push.
type overflowReason string

const (
	// overflowCeiling — the queue reached its absolute frame ceiling. This is
	// the memory bound: it holds regardless of how recently the consumer drained.
	overflowCeiling overflowReason = "hard_ceiling"
	// overflowStalled — the queue sat above its soft bound for longer than the
	// grace period without a single frame being drained. That is a consumer
	// which is not merely busy but WEDGED: transient busyness always drains
	// something eventually, and this one drained nothing at all.
	overflowStalled overflowReason = "stalled"
)

// pushResult reports one push's outcome plus the queue statistics a caller
// needs to say what it observed. It exists because "the buffer was full" is no
// longer a single condition: an elastic queue distinguishes transient pressure
// (queued, above the soft bound) from the two refusals above.
type pushResult struct {
	queued    bool
	compacted int
	// depth is the queue length after the push, or at the moment of refusal.
	depth int
	soft  int
	hard  int
	// overSoft is set when the queue is above its soft bound, whether or not
	// the frame was queued.
	overSoft bool
	// entered is set only on the push that CROSSED from below the soft bound
	// into the elastic region, so the pressure record fires once per episode
	// rather than once per frame.
	entered    bool
	reason     overflowReason
	stalledFor time.Duration
	// closed reports a push onto a queue whose connection is already gone. It
	// is NOT a slow consumer — there is nothing left to disconnect — but it is
	// not a delivery either, so it is its own outcome rather than a queued=true
	// lie. The caller reports the frame undelivered.
	closed bool
}

// outbox is one connection's ELASTIC bounded outbound queue.
//
// It is an explicit slice under a mutex rather than a buffered channel because
// a saturated queue is COMPACTED before the connection is given up on, and a
// channel's contents cannot be rewritten in place. ready is a wakeup signal of
// capacity one, not the queue itself: the writer drains everything each time it
// wakes, so a coalesced token is never a lost frame.
//
// TWO bounds, not one. soft is the depth at which the consumer is declared
// behind; hard is the absolute ceiling. Between them the queue keeps accepting
// frames for up to grace, PROVIDED the consumer drains at least one frame in
// that window. That window is what separates a single-threaded UI blocking its
// event loop for a few seconds — which always drains afterwards — from a
// consumer that will never read again. Setting soft == hard with grace == 0
// collapses the whole thing back to a plain bounded queue, which is exactly
// what every non-host connection uses.
//
// TWO LANES, not one, and the split is the ack head-of-line fix.
//
// The bulk lane (frames) keeps every property it ever had: its frames leave in
// exactly the order they were pushed, compaction rewrites it in place keeping
// the newest occurrence's POSITION, and no state frame overtakes an
// irreplaceable frame that preceded it. Snapshot-before-delta and every other
// state ordering the frontends depend on is a statement about THAT lane, and
// nothing here weakens it.
//
// The control lane (control) carries only CORRELATED, REQUEST-KEYED REPLIES —
// a command's CommandAck and the one response frame a command may produce.
// They are matched by request_id, never applied as state, and no frontend
// derives anything from where they sit relative to a push. So the FIFO's
// load-bearing property does not extend to them, and holding them behind a
// bulk backlog is pure harm: a 104-workspace connect snapshot queued ahead of
// an ack pushed that ack's DELIVERY ~15s past a command the daemon had
// answered in ~12ms, blowing the client's 10s deadline and opening a
// `client.command_unacked` failure card for a command that had SUCCEEDED.
//
// Control frames leave BEFORE bulk frames, and among themselves in push order
// — which is what keeps a command's response frame ahead of that command's own
// ack. The lane cannot starve the bulk one: it holds at most one reply pair per
// command actually in flight, and the writer drains everything each wake, so an
// ack's delivery latency is bounded by write throughput rather than by queue
// depth.
//
// Both lanes share ONE set of bounds. The soft/hard/grace accounting is over
// the TOTAL depth, so the memory ceiling and the slow-consumer eviction are
// exactly as strict as they were; a control frame is refused at the ceiling
// like any other, and its sender is told rather than left waiting.
type outbox struct {
	mu      sync.Mutex
	frames  []outFrame
	control []outFrame
	// closed is set once the connection is gone. A push onto a closed queue is
	// reported to its sender rather than accepted into a queue no writer will
	// ever drain.
	closed bool
	soft   int
	hard   int
	grace  time.Duration
	now    func() time.Time
	ready  chan struct{}
	// popped counts every frame the writer has taken (log/introspection aid).
	popped uint64
	// progress counts every OBSERVABLE ACT OF DRAIN by this connection's
	// consumer: each frame the writer popped, AND each chunk of a frame's bytes
	// the socket accepted while a write was in flight.
	//
	// The byte half is not an embellishment, it is the whole point. The frame
	// count was the only progress evidence, and a single frame can take tens of
	// seconds to write: an observed Emacs host received a 162-workspace connect
	// snapshot, spent 31s consuming it — reading the whole time — and was
	// evicted at 30s as "stalled" for having drained nothing, 391ms before the
	// write it was draining completed successfully. Byte-level progress is what
	// distinguishes a consumer that is slow from one that is gone; frame-level
	// progress cannot, because a consumer working on one huge frame looks
	// identical to a consumer that died holding it.
	progress uint64
	// overSince is when the queue last crossed into the elastic region with no
	// drain progress since; zero means it is not currently under pressure.
	overSince    time.Time
	overProgress uint64
}

// newOutbox builds a plain bounded queue: no elastic region, so a push refused
// at the bound is refused immediately, exactly as it always was.
func newOutbox(max int) *outbox {
	return newElasticOutbox(max, max, 0)
}

// newElasticOutbox builds a queue that may grow from soft up to hard while the
// consumer keeps draining, giving up only past hard or past grace without a
// single drained frame. A hard below soft is meaningless and is clamped up, so
// no caller can accidentally build a queue that refuses below its own bound.
func newElasticOutbox(soft, hard int, grace time.Duration) *outbox {
	if hard < soft {
		hard = soft
	}
	return &outbox{soft: soft, hard: hard, grace: grace, now: time.Now, ready: make(chan struct{}, 1)}
}

// capacity reports the soft bound this queue was built with (log/introspection
// aid): the depth at which the consumer is declared behind.
func (o *outbox) capacity() int { return o.soft }

// ceiling reports the absolute frame bound (log/introspection aid).
func (o *outbox) ceiling() int { return o.hard }

// depth reports how many frames are waiting across BOTH lanes
// (test/introspection aid).
func (o *outbox) depth() int {
	o.mu.Lock()
	defer o.mu.Unlock()
	return o.depthLocked()
}

// depthLocked is the queue's total occupancy, which is what every bound is
// judged against: the two lanes are one queue for memory and eviction purposes
// and differ only in drain order. Caller holds mu.
func (o *outbox) depthLocked() int { return len(o.control) + len(o.frames) }

// push appends f, compacting first if the queue is already at its soft bound.
// It reports whether the frame was queued, how many superseded frames
// compaction removed, and the pressure the queue is under, so a caller that
// must give up on the connection can say what it already tried. A refused push
// has left the queue untouched apart from that compaction: nothing
// irreplaceable is ever dropped to make room.
// A frame is appended to its own lane (control frames to the control lane,
// everything else to the bulk one), and every bound above is judged against the
// two lanes' combined depth.
func (o *outbox) push(f outFrame) pushResult {
	o.mu.Lock()
	defer o.mu.Unlock()
	res := pushResult{soft: o.soft, hard: o.hard}
	if o.closed {
		// The connection is gone. Refusing here rather than queueing is what
		// makes "every pushed frame gets exactly one disposition" hold: a frame
		// accepted now would sit in a queue no writer will ever drain.
		res.closed, res.depth = true, o.depthLocked()
		return res
	}
	if o.depthLocked() >= o.soft {
		res.compacted = o.compactLocked()
	}
	// The ceiling is unconditional: no amount of recent drain progress buys a
	// queue past its memory bound.
	if o.depthLocked() >= o.hard {
		res.depth, res.overSoft, res.reason = o.depthLocked(), true, overflowCeiling
		res.stalledFor = o.stalledForLocked()
		return res
	}
	if o.depthLocked() >= o.soft {
		res.overSoft = true
		switch {
		case o.overSince.IsZero() || o.progress != o.overProgress:
			// Either pressure just began, or the consumer has made observable
			// drain progress since the mark — a popped frame or accepted bytes.
			// Both are progress: re-mark and keep accepting.
			res.entered = o.overSince.IsZero()
			o.overSince, o.overProgress = o.now(), o.progress
		case o.now().Sub(o.overSince) >= o.grace:
			res.depth, res.reason = o.depthLocked(), overflowStalled
			res.stalledFor = o.stalledForLocked()
			return res
		}
	}
	if f.control {
		o.control = append(o.control, f)
	} else {
		o.frames = append(o.frames, f)
	}
	o.signalLocked()
	res.queued, res.depth = true, o.depthLocked()
	return res
}

// close marks the queue dead and hands back every frame still waiting, so the
// caller can report each one undelivered. It is idempotent: a second call
// returns nothing, because the first call already took everything.
//
// The frames are RETURNED rather than notified here, so no callback runs under
// the queue lock.
func (o *outbox) close() []outFrame {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.closed = true
	stranded := append(append([]outFrame(nil), o.control...), o.frames...)
	o.control, o.frames = nil, nil
	return stranded
}

// noteWriteProgress records that the socket accepted a chunk of the frame
// currently being written. The writer calls it from inside writeFrame, which is
// the only place that can see progress a queue-depth reading cannot: a frame
// that has been popped is off the queue, so from the queue's point of view a
// 31-second write and a dead consumer look exactly the same until this says
// otherwise.
//
// It deliberately does NOT clear the pressure mark. The queue is still deep and
// the next push must still report that; what it clears is only the STALL
// verdict, by advancing the progress counter the mark is compared against.
func (o *outbox) noteWriteProgress() {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.progress++
}

// stalledForLocked reports how long the queue has been under pressure with no
// drain progress, or zero when it is not marked. Caller holds mu.
func (o *outbox) stalledForLocked() time.Duration {
	if o.overSince.IsZero() {
		return 0
	}
	return o.now().Sub(o.overSince)
}

// pop removes the next frame to write, reporting false when none is waiting.
//
// The CONTROL lane is drained first. That is the whole ack fix: a correlated
// reply's wait is bounded by how fast the socket accepts bytes, never by how
// many bulk frames were queued ahead of it. Within each lane the order is
// strictly the order frames were pushed.
func (o *outbox) pop() (outFrame, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	var f outFrame
	switch {
	case len(o.control) > 0:
		f = o.control[0]
		o.control[0] = outFrame{}
		o.control = o.control[1:]
	case len(o.frames) > 0:
		f = o.frames[0]
		o.frames[0] = outFrame{}
		o.frames = o.frames[1:]
	default:
		return outFrame{}, false
	}
	o.popped++
	o.progress++
	if o.depthLocked() < o.soft {
		// The episode is over: clear the mark so the next one is reported as a
		// new entry into the elastic region rather than a continuation.
		o.overSince, o.overProgress = time.Time{}, 0
	}
	return f, true
}

// compactLocked drops every queued frame that a LATER queued frame supersedes,
// returning how many it removed. Caller holds mu.
//
// The survivor keeps the newest occurrence's POSITION, not the oldest's, so the
// frames that remain sit in exactly the order the producer emitted them and no
// superseding state ever overtakes an irreplaceable frame that preceded it.
//
// It rewrites the BULK lane only. Control frames are correlated one-shot
// replies: nothing supersedes them, they carry no coalesce key, and dropping
// one would strand the sender waiting on it forever.
func (o *outbox) compactLocked() int {
	newest := make(map[string]int, len(o.frames))
	superseded := 0
	for i, f := range o.frames {
		if f.key == "" {
			continue
		}
		if _, seen := newest[f.key]; seen {
			superseded++
		}
		newest[f.key] = i
	}
	if superseded == 0 {
		// Nothing supersedes anything: leave the queue exactly as it is rather
		// than rewriting a slice to the same contents.
		return 0
	}
	kept := o.frames[:0]
	for i, f := range o.frames {
		if f.key != "" && newest[f.key] != i {
			continue
		}
		kept = append(kept, f)
	}
	removed := len(o.frames) - len(kept)
	for i := len(kept); i < len(o.frames); i++ {
		o.frames[i] = outFrame{}
	}
	o.frames = kept
	return removed
}

// notifyFrame reports f's terminal disposition to whoever is waiting on it, if
// anyone is. It is the ONE place a disposition is announced, so "exactly one
// notification per pushed frame" is a property of the call sites rather than of
// each caller's memory.
func notifyFrame(f outFrame, err error) {
	if f.notify != nil {
		f.notify(err)
	}
}

// signalLocked wakes the writer without blocking. A token already pending is
// wakeup enough: the writer drains the whole queue every time it wakes.
func (o *outbox) signalLocked() {
	select {
	case o.ready <- struct{}{}:
	default:
	}
}
