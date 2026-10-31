package server

import (
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE FAN-OUT CAP: AN AGENT NEVER BROADCASTS TO THE FLEET.
//
// Every workspace an agent-sent prompt reaches wakes, runs a turn and resends
// its whole context to the vendor. On 2026-10-30 one workspace's agent sent a
// notice "on the user's behalf" to every open workspace -- 33 of them -- and
// every one went into the working state at once to answer it (owner's report:
// "like all these prompts are being sent, why?"). Nothing in agent-repl asked
// for it, and nothing stopped it.
//
// So the daemon, not the agent's judgement, bounds it: in the trailing
// fanOutWindow a workspace may prompt at most fanOutCap DISTINCT workspaces it
// has no relationship with. A related recipient never counts: its parent or
// child, the workspace it was forked from or lands into, its merge's
// requester, the one it awaits, or one that prompted it in the window (a
// reply to a correspondent). Prompting a recipient it already reached in the
// window does not widen the fan-out either. Only the user lifts the cap
// (SubmitPromptRequest.as_user).
//
// DO NOT RAISE THE CAP TO LET A BROADCAST THROUGH. A notice every workspace
// must see belongs in the harness (the metaprompt, a skill, AGENTS.md), which
// every session reads at its next turn without anyone being woken for it.
const (
	fanOutCap    = 3
	fanOutWindow = time.Hour
)

// fanOut is the daemon's record of agent-sent prompts in the trailing window.
type fanOut struct {
	mu sync.Mutex
	// sent is every agent-sent prompt's last instant, sender to recipient:
	// what tells a reply to a correspondent.
	sent map[ids.WorkspaceID]map[ids.WorkspaceID]time.Time
	// unrelated is the instant each sender last prompted each recipient it
	// has no relationship with: what the cap counts.
	unrelated map[ids.WorkspaceID]map[ids.WorkspaceID]time.Time
}

func newFanOut() *fanOut {
	return &fanOut{
		sent:      map[ids.WorkspaceID]map[ids.WorkspaceID]time.Time{},
		unrelated: map[ids.WorkspaceID]map[ids.WorkspaceID]time.Time{},
	}
}

// admit decides one agent-sent prompt from sender to recipient at now and
// records it when admitted. It answers false when the prompt would take the
// sender past fanOutCap unrelated recipients in the window.
func (f *fanOut) admit(sender, recipient *wsm.Workspace, asUser bool, now time.Time) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	from, to := sender.ID, recipient.ID
	prune(f.sent[from], now)
	prune(f.unrelated[from], now)
	prune(f.sent[to], now)
	if !asUser && !related(sender, recipient) && !f.answers(from, to) {
		if _, reached := f.unrelated[from][to]; !reached && len(f.unrelated[from]) >= fanOutCap {
			return false
		}
		record(f.unrelated, from, to, now)
	}
	record(f.sent, from, to, now)
	return true
}

// answers reports whether to prompted from in the window: from replying to a
// correspondent widens nothing.
func (f *fanOut) answers(from, to ids.WorkspaceID) bool {
	_, ok := f.sent[to][from]
	return ok
}

// related reports whether the two workspaces stand in any relationship the
// registry records, in either direction.
func related(a, b *wsm.Workspace) bool {
	return names(a, b.ID) || names(b, a.ID)
}

// names reports whether w's record points at other.
func names(w *wsm.Workspace, other ids.WorkspaceID) bool {
	for _, p := range []*ids.WorkspaceID{w.Parent, w.ContextFrom, w.LandsInto, w.RequestedBy, w.Awaiting} {
		if p != nil && *p == other {
			return true
		}
	}
	return w.MergeOf != nil && (w.MergeOf.Requester == other || w.MergeOf.Maker == other)
}

func prune(m map[ids.WorkspaceID]time.Time, now time.Time) {
	for id, at := range m {
		if now.Sub(at) >= fanOutWindow {
			delete(m, id)
		}
	}
}

func record(m map[ids.WorkspaceID]map[ids.WorkspaceID]time.Time, from, to ids.WorkspaceID, now time.Time) {
	if m[from] == nil {
		m[from] = map[ids.WorkspaceID]time.Time{}
	}
	m[from][to] = now
}

// fanOutRefusal is SubmitPromptError.fan_out_refused.
func (s *server) fanOutRefusal() refusal {
	return submitRefusal(s.fill(refusal{
		Arm: "fan_out_refused",
		Reason: fmt.Sprintf("this workspace already prompted %d unrelated workspaces in the last %s; "+
			"an agent never broadcasts to the fleet. Put a notice every workspace needs in the harness, "+
			"or send with -as-user only when the user explicitly asked for this send", fanOutCap, fanOutWindow),
		Fields: map[string]any{"cap": uint32(fanOutCap), "window_minutes": uint32(fanOutWindow / time.Minute)},
	}))
}
