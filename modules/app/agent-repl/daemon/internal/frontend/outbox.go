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

// outFrame is one queued wire payload plus the state it supersedes. key is
// empty for a frame nothing may replace.
type outFrame struct {
	key  string
	data []byte
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
type outbox struct {
	mu     sync.Mutex
	frames []outFrame
	soft   int
	hard   int
	grace  time.Duration
	now    func() time.Time
	ready  chan struct{}
	// popped counts every frame the writer has taken. It is the drain-progress
	// evidence: a wedged consumer is one whose count does not move.
	popped uint64
	// overSince is when the queue last crossed into the elastic region with no
	// drain progress since; zero means it is not currently under pressure.
	overSince  time.Time
	overPopped uint64
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

// depth reports how many frames are waiting (test/introspection aid).
func (o *outbox) depth() int {
	o.mu.Lock()
	defer o.mu.Unlock()
	return len(o.frames)
}

// push appends f, compacting first if the queue is already at its soft bound.
// It reports whether the frame was queued, how many superseded frames
// compaction removed, and the pressure the queue is under, so a caller that
// must give up on the connection can say what it already tried. A refused push
// has left the queue untouched apart from that compaction: nothing
// irreplaceable is ever dropped to make room.
func (o *outbox) push(f outFrame) pushResult {
	o.mu.Lock()
	defer o.mu.Unlock()
	res := pushResult{soft: o.soft, hard: o.hard}
	if len(o.frames) >= o.soft {
		res.compacted = o.compactLocked()
	}
	// The ceiling is unconditional: no amount of recent drain progress buys a
	// queue past its memory bound.
	if len(o.frames) >= o.hard {
		res.depth, res.overSoft, res.reason = len(o.frames), true, overflowCeiling
		res.stalledFor = o.stalledForLocked()
		return res
	}
	if len(o.frames) >= o.soft {
		res.overSoft = true
		switch {
		case o.overSince.IsZero() || o.popped != o.overPopped:
			// Either pressure just began, or the consumer has drained something
			// since the mark. Both are progress: re-mark and keep accepting.
			res.entered = o.overSince.IsZero()
			o.overSince, o.overPopped = o.now(), o.popped
		case o.now().Sub(o.overSince) >= o.grace:
			res.depth, res.reason = len(o.frames), overflowStalled
			res.stalledFor = o.stalledForLocked()
			return res
		}
	}
	o.frames = append(o.frames, f)
	o.signalLocked()
	res.queued, res.depth = true, len(o.frames)
	return res
}

// stalledForLocked reports how long the queue has been under pressure with no
// drain progress, or zero when it is not marked. Caller holds mu.
func (o *outbox) stalledForLocked() time.Duration {
	if o.overSince.IsZero() {
		return 0
	}
	return o.now().Sub(o.overSince)
}

// pop removes the oldest queued frame, reporting false when none is waiting.
func (o *outbox) pop() ([]byte, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if len(o.frames) == 0 {
		return nil, false
	}
	f := o.frames[0]
	o.frames[0] = outFrame{}
	o.frames = o.frames[1:]
	o.popped++
	if len(o.frames) < o.soft {
		// The episode is over: clear the mark so the next one is reported as a
		// new entry into the elastic region rather than a continuation.
		o.overSince, o.overPopped = time.Time{}, 0
	}
	return f.data, true
}

// compactLocked drops every queued frame that a LATER queued frame supersedes,
// returning how many it removed. Caller holds mu.
//
// The survivor keeps the newest occurrence's POSITION, not the oldest's, so the
// frames that remain sit in exactly the order the producer emitted them and no
// superseding state ever overtakes an irreplaceable frame that preceded it.
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

// signalLocked wakes the writer without blocking. A token already pending is
// wakeup enough: the writer drains the whole queue every time it wakes.
func (o *outbox) signalLocked() {
	select {
	case o.ready <- struct{}{}:
	default:
	}
}
