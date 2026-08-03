package frontend

import (
	"sync"

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
	case *frontendv1.FrontendFrame_Progress:
		return "progress\x00" + f.Progress.GetWorkspace() + "\x00" + f.Progress.GetSessionId()
	case *frontendv1.FrontendFrame_Queue:
		return "queue\x00" + f.Queue.GetWorkspace() + "\x00" + f.Queue.GetSessionId()
	case *frontendv1.FrontendFrame_Heartbeat:
		return "heartbeat\x00" + f.Heartbeat.GetWorkspace() + "\x00" +
			f.Heartbeat.GetSessionId() + "\x00" + f.Heartbeat.GetProgress().GetToolUseId()
	case *frontendv1.FrontendFrame_WorkspaceRoster:
		return "workspace_roster"
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

// outbox is one connection's bounded outbound queue.
//
// It is an explicit slice under a mutex rather than a buffered channel because
// a saturated queue is COMPACTED before the connection is given up on, and a
// channel's contents cannot be rewritten in place. ready is a wakeup signal of
// capacity one, not the queue itself: the writer drains everything each time it
// wakes, so a coalesced token is never a lost frame.
type outbox struct {
	mu     sync.Mutex
	frames []outFrame
	max    int
	ready  chan struct{}
}

func newOutbox(max int) *outbox {
	return &outbox{max: max, ready: make(chan struct{}, 1)}
}

// capacity reports the bound this queue was built with (log/introspection aid).
func (o *outbox) capacity() int { return o.max }

// depth reports how many frames are waiting (test/introspection aid).
func (o *outbox) depth() int {
	o.mu.Lock()
	defer o.mu.Unlock()
	return len(o.frames)
}

// push appends f, compacting first if the queue is already at its bound.
// It reports whether the frame was queued and how many superseded frames
// compaction removed, so a caller that must give up on the connection can say
// what it already tried. A refused push has left the queue untouched apart from
// that compaction: nothing irreplaceable is ever dropped to make room.
func (o *outbox) push(f outFrame) (queued bool, compacted int) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if len(o.frames) >= o.max {
		compacted = o.compactLocked()
	}
	if len(o.frames) >= o.max {
		return false, compacted
	}
	o.frames = append(o.frames, f)
	o.signalLocked()
	return true, compacted
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
