package sessionwatcher

import (
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE LIVE-WORK LEDGER.
//
// LIVENESS IS THE SHIM'S LIFECYCLE, NOT A WATCH'S. An item enters the ledger at
// the shim's announcement of it and leaves at the shim's CONCLUSION of it, and
// at nothing else: not at its watch's open failing, not at its watch's open
// hanging, not at its stream ending, and not for want of a watch at all. The
// watches are how an item's progress is DRAWN; they were once also how its
// liveness was decided, and a WatchBash open that never got its first frame
// held a finished hand-backgrounded shell in the live set for good -- the
// bounce registry waited on a freeness edge that never came, a handover never
// transferred, and every later deploy was refused (2026-09-27, workspace
// 3e2d9cadc6794e13).
//
// THE CONCLUSIONS THE WIRE CARRIES, per kind:
//   - a subagent: its spawn unit's own terminal arm (AgentSubagent success or
//     failure), addressed by its handle on whichever book carries it; and its
//     own agent stream's terminal, when a watch of it is open;
//   - a shell: its run's AgentBash terminal, on its WatchBash;
//   - a monitor: its activity's own terminal arm;
//   - every kind: the session's query dying, and a re-announcement that no
//     longer names it (reconcileLiveWorkLocked).
//
// EVERY ONE OF THEM GOES THROUGH concludeLocked, which retires the handle,
// tears down whatever watch the item has -- an open still in flight is
// cancelled, an open stream is closed off the lock -- and records the
// retirement. The caller republishes the set, which is the freeness edge.

// liveItem is one detached item the shim has announced and not concluded.
type liveItem struct {
	// work is the item's handle, the ledger's key.
	work *conversationv1.DetachedWorkId
	// kind is the kind its announcement stated.
	kind detachedKind
	// agent is a subagent's watched agent, nil for every other kind and for a
	// subagent whose announcement named no agent to watch.
	agent *conversationv1.AgentId
	// seq is the ledger's admission count at this item's admission.
	seq uint64
}

// agentOrHandle is how a subagent is listed in the live-work set: its agent,
// or, for one that named no agent, its HANDLE. The contract mints a subagent's
// AgentId from its spawning call's tool_use_id, which is also its handle
// (DetachedWorkId.value == AgentActivityId.value), so the handle is the id the
// footer's rows and the stop sweep already address it by.
func (i *liveItem) agentOrHandle() *conversationv1.AgentId {
	if i.agent.GetValue() != "" {
		return i.agent
	}
	return &conversationv1.AgentId{Value: i.work.GetValue()}
}

// conclusion names the edge that retired an item, for its record.
type conclusion string

// The conclusions.
const (
	// concludedUnitTerminal is a subagent's spawn unit reaching its terminal.
	concludedUnitTerminal conclusion = "unit_terminal"
	// concludedAgentTerminal is a detached subagent's own stream terminal.
	concludedAgentTerminal conclusion = "agent_terminal"
	// concludedBashTerminal is a shell run's AgentBash terminal.
	concludedBashTerminal conclusion = "bash_terminal"
	// concludedMonitorTerminal is a monitor activity's terminal.
	concludedMonitorTerminal conclusion = "monitor_terminal"
	// concludedQueryDied is the session's query dying with the item live.
	concludedQueryDied conclusion = "query_died"
	// concludedReconciled is a re-announcement that no longer names the item.
	concludedReconciled conclusion = "reconciled"
	// concludedDeparted is the watched shim's PROCESS being gone: a kill, a
	// forced bounce, a stand-down or a death. Everything it ran ended with it.
	concludedDeparted conclusion = "departed"
)

// admitLocked puts one announced item in the ledger, reporting whether it was
// new. A handle already live is not admitted twice.
func (w *watcher) admitLocked(work *conversationv1.DetachedWorkId, kind detachedKind, agent *conversationv1.AgentId) bool {
	handle := work.GetValue()
	if _, ok := w.live[handle]; ok {
		return false
	}
	w.liveSeq++
	w.live[handle] = &liveItem{work: work, kind: kind, agent: agent, seq: w.liveSeq}
	w.log.Debug("daemon.sessionwatcher.live_work_admitted", "an announced detached item entered the live set", dlog.Context{
		"work_id": handle, "kind": kind.String(), "agent_id": agent.GetValue(),
	})
	return true
}

// liveHandleLocked answers the live item a handle names, if it is live.
func (w *watcher) liveHandleLocked(handle string) (*liveItem, bool) {
	if handle == "" {
		return nil, false
	}
	item, ok := w.live[handle]
	return item, ok
}

// concludeLocked is THE ONE DOOR OUT OF THE LIVE SET. It retires the handle for
// good, tears the item's watch down whatever state it is in, and records the
// retirement; it reports whether the item was live, and the caller republishes
// the set when it was.
func (w *watcher) concludeLocked(handle string, why conclusion) bool {
	item, ok := w.liveHandleLocked(handle)
	if !ok {
		return false
	}
	delete(w.live, handle)
	w.retiredWork[handle] = struct{}{}
	watch := "none"
	switch item.kind {
	case kindSubagent:
		if item.agent != nil {
			if entry, ok := w.agents[item.agent.GetValue()]; ok && entry.work.GetValue() == handle {
				watch = w.dropAgentWatchLocked(item.agent.GetValue(), entry)
			}
		}
	case kindBash:
		if entry, ok := w.shells[handle]; ok {
			watch = w.dropShellWatchLocked(handle, entry)
		}
	}
	w.log.Info("daemon.sessionwatcher.live_work_retired", "a detached item concluded and left the live set", dlog.Context{
		"work_id": handle, "kind": item.kind.String(), "agent_id": item.agent.GetValue(),
		"conclusion": string(why), "watch": watch, "live_after": len(w.live),
	})
	return true
}

// dropAgentWatchLocked closes and forgets one agent watch, answering the state
// it was in. The stream is closed OFF the mutex: its own goroutine takes the
// mutex to report the end, and waiting for it here would deadlock.
func (w *watcher) dropAgentWatchLocked(key string, entry *agentWatch) string {
	entry.done = true
	delete(w.agents, key)
	state := watchState(entry.stream != nil, w.inFlightLocked(entry.opening))
	// AN OPEN STILL IN FLIGHT FOR IT IS ABANDONED, so a hung open is released
	// now rather than at the watcher's close; its completion is discarded.
	entry.opening.abandon()
	if entry.stream != nil {
		stream := entry.stream
		entry.stream = nil
		go stream.Close()
	}
	w.log.Debug("daemon.sessionwatcher.reap", "a subagent watch was reaped", dlog.Context{"agent_id": key, "watch": state})
	return state
}

// dropShellWatchLocked closes and forgets one shell watch, as for an agent.
func (w *watcher) dropShellWatchLocked(key string, entry *shellWatch) string {
	entry.done = true
	delete(w.shells, key)
	state := watchState(entry.stream != nil, w.inFlightLocked(entry.opening))
	entry.opening.abandon()
	if entry.stream != nil {
		stream := entry.stream
		entry.stream = nil
		go stream.Close()
	}
	w.log.Debug("daemon.sessionwatcher.reap", "a shell watch was reaped", dlog.Context{"work_id": key, "watch": state})
	return state
}

// watchState names what a torn-down watch was doing, for the record.
func watchState(open, opening bool) string {
	switch {
	case open:
		return "open"
	case opening:
		return "opening"
	default:
		return "none"
	}
}

// retireAgentLocked handles one agent's stream reaching its terminal: a
// DETACHED agent concludes, and a sync spawn's watch is simply dropped (it was
// never live work). It reports whether the live set changed.
func (w *watcher) retireAgentLocked(key string, why conclusion) bool {
	entry, ok := w.agents[key]
	if !ok {
		return false
	}
	if handle := entry.work.GetValue(); handle != "" {
		if w.concludeLocked(handle, why) {
			return true
		}
	}
	// NOT (OR NO LONGER) LIVE WORK: the watch alone goes.
	if w.agents[key] == entry {
		w.dropAgentWatchLocked(key, entry)
	}
	return false
}

// concludeAllLocked retires every live item, and drops every sync watch with
// them: the session's query is gone, and nothing it ran survives it. It
// reports whether anything was live.
func (w *watcher) concludeAllLocked(why conclusion) bool {
	changed := false
	for handle := range w.live {
		if w.concludeLocked(handle, why) {
			changed = true
		}
	}
	for key, entry := range w.agents {
		w.dropAgentWatchLocked(key, entry)
	}
	return changed
}

// settleDepartedWorkLocked concludes every item the departed shim was running
// and republishes the set, so every view -- the footer's chips and panels, the
// roster's `idle_async`, the feed's shell heads, monitor cards and subagent
// bubbles -- shows the work ENDED the moment the shim is known gone. The caller
// holds mu and has just recorded the departure.
//
// THE PROCESS IS THE WORK'S LIFETIME. Detached work runs inside the shim's
// process group (the vendor's subagents and monitors in the CLI process, its
// shells as the CLI's children), and a departure is that group's end: a kill
// this daemon ordered, a forced bounce's escalation, or a death. Before this the
// ledger outlived the departure. A shim force-killed by a restart, or one that
// died, wrote no terminals and could not be asked to re-announce anything, so
// the watcher's last set stood: the webapp's expanded footer listed the dead
// work, the roster stayed `idle_async`, and a shell head kept its stop button,
// until a replacement shim's first set happened to overwrite them -- and for
// good when none came up.
//
// ONLY THE WATCHER THAT SPEAKS FOR THE WORKSPACE PUBLISHES. A watcher a newer
// one already displaced (see speakers) settles its own ledger but publishes
// nothing: the views already hold the newer watcher's set, and an empty set
// from the old one would hide work the new shim is running.
func (w *watcher) settleDepartedWorkLocked() {
	live := len(w.live)
	if !w.concludeAllLocked(concludedDeparted) {
		return
	}
	if !speakers.speaksFor(w.ws, w) {
		w.log.Info("daemon.sessionwatcher.departed_work_settled", "the departed shim's live work was concluded; a newer watcher speaks for the workspace, so nothing was republished", dlog.Context{
			"concluded": live, "published": false,
		})
		return
	}
	w.log.Info("daemon.sessionwatcher.departed_work_settled", "the departed shim's live work was concluded and the empty set republished to every view", dlog.Context{
		"concluded": live, "published": true,
	})
	w.publishLiveWorkLocked()
}

// reconcileLiveWorkLocked holds the ledger to a re-announcement's live
// membership: the shim's own statement of what is live NOW. It reports whether
// anything was retired.
//
// AN ITEM THE LEDGER HOLDS THAT THE SHIM NO LONGER NAMES HAS ENDED, and the
// daemon missed the edge that said so. That is an INVARIANT VIOLATION -- the
// ledger was meant to mirror the shim's lifecycle and did not -- so it is
// recorded at ERROR, naming the item, before the item is retired.
//
// ONLY WHAT THE RE-ANNOUNCEMENT CAN SPEAK FOR IS JUDGED. The shim computes it
// after the session watch's open was decided (openedAt), while an announcement
// rides an AGENT stream the two planes share no ordering with. An item admitted
// after that open was decided may postdate the membership the shim read, so its
// absence says nothing and it is left alone.
//
// NOTHING IS ADMITTED HERE. A named item the ledger lacks is adoption's
// business on a first taking of the facts (adoptLiveWorkLocked), and on a
// re-open its announcement is on the catch-up page of the stream that owns it.
func (w *watcher) reconcileLiveWorkLocked(started *conversationv1.SessionStarted, openedAt uint64) bool {
	named := make(map[string]struct{}, len(started.GetLiveWork()))
	for _, item := range started.GetLiveWork() {
		named[item.GetWork().GetValue()] = struct{}{}
	}
	changed := false
	for handle, item := range w.live {
		if _, ok := named[handle]; ok {
			continue
		}
		if item.seq > openedAt {
			w.log.Debug("daemon.sessionwatcher.live_work_unjudged", "a live item admitted after the re-announcement was asked for is not judged by it", dlog.Context{
				"work_id": handle, "kind": item.kind.String(), "admitted": item.seq, "opened_at": openedAt,
			})
			continue
		}
		w.log.Error("daemon.sessionwatcher.live_work_stale", "the daemon held detached work the shim no longer names as live; its conclusion was missed, and it is retired now", dlog.Context{
			"work_id": handle, "kind": item.kind.String(), "agent_id": item.agent.GetValue(),
			"shim_live": len(named),
		})
		if w.concludeLocked(handle, concludedReconciled) {
			changed = true
		}
	}
	return changed
}

// speakerRegistry records, per workspace, the watcher whose live-work set the
// views hold now: the newest one started. A watcher stops speaking when it is
// closed or when a newer watcher of the same workspace starts.
//
// IT EXISTS FOR THE DISPLACED CLOSE. The fleet can start a workspace's new
// watcher before it closes the one it displaced (Fleet.remember), and a close
// that finds the old shim reaped is a departure. That departure must not
// republish the old ledger's empty set over the set the new watcher already
// published. A registry rather than a fleet callback, because every watcher is
// minted by start in this package and the rule is this package's to keep.
//
// THE LOCK IS A LEAF: it is taken under a watcher's mu and takes nothing.
type speakerRegistry struct {
	mu      sync.Mutex
	current map[ids.WorkspaceID]*watcher
}

// speakers is the daemon's one registry.
var speakers = &speakerRegistry{current: map[ids.WorkspaceID]*watcher{}}

// speak makes w the watcher that speaks for ws.
func (r *speakerRegistry) speak(ws ids.WorkspaceID, w *watcher) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.current[ws] = w
}

// silence retires w from speaking for ws, if it still does.
func (r *speakerRegistry) silence(ws ids.WorkspaceID, w *watcher) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.current[ws] == w {
		delete(r.current, ws)
	}
}

// speaksFor reports whether w is the watcher that speaks for ws.
func (r *speakerRegistry) speaksFor(ws ids.WorkspaceID, w *watcher) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.current[ws] == w
}
