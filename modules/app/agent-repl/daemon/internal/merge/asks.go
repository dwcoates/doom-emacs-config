package merge

import (
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
)

// Asks is the set of permission asks and question batches open in each
// workspace's session, as the session watcher reports them. It is the merge's
// reading of the SAME edges the footer resolver reads, opened by a start and
// closed by any other result, so the merge bubble's tab and the footer's
// "waiting on user" substatus stand and fall together.
//
// IT IS BUILT BEFORE THE ORCHESTRATOR: the session watchers start routing
// edges before the merge queue exists, and an ask opened then is still open
// when a merge resumed at boot reaches its conflict resolution or fixing.
type Asks struct {
	mu sync.Mutex
	// open is each workspace's open asks, keyed "permission:<id>" and
	// "question:<id>".
	open map[ids.WorkspaceID]map[string]struct{}
	// changed is told each time a workspace goes from no open ask to one, or
	// back. Nil until the orchestrator binds it.
	changed func(ws ids.WorkspaceID, waiting bool)
}

// NewAsks builds an empty ask set.
func NewAsks() *Asks {
	return &Asks{open: map[ids.WorkspaceID]map[string]struct{}{}}
}

// OnPermission takes a consent ask's edge: a start opens it, any other result
// closes it.
func (a *Asks) OnPermission(ws ids.WorkspaceID, _ *conversationv1.AgentId, p *conversationv1.AgentPermission) {
	if p == nil {
		return
	}
	_, start := p.GetResult().(*conversationv1.AgentPermission_Start)
	a.take(ws, "permission:"+p.GetId().GetValue(), start)
}

// OnQuestion takes a question batch's edge: a start opens it, any other result
// closes it.
func (a *Asks) OnQuestion(ws ids.WorkspaceID, _ *conversationv1.AgentId, q *conversationv1.AgentQuestion) {
	if q == nil {
		return
	}
	_, start := q.GetResult().(*conversationv1.AgentQuestion_Start)
	a.take(ws, "question:"+q.GetId().GetValue(), start)
}

// Waiting reports whether the workspace's session has any ask open.
func (a *Asks) Waiting(ws ids.WorkspaceID) bool {
	a.mu.Lock()
	defer a.mu.Unlock()
	return len(a.open[ws]) > 0
}

// bind installs the one listener told of each change. It is the orchestrator's.
func (a *Asks) bind(changed func(ws ids.WorkspaceID, waiting bool)) {
	a.mu.Lock()
	defer a.mu.Unlock()
	a.changed = changed
}

// take opens or closes one ask, and tells the listener when the workspace's
// standing changed. The listener is called outside the lock.
func (a *Asks) take(ws ids.WorkspaceID, key string, open bool) {
	a.mu.Lock()
	was := len(a.open[ws]) > 0
	if open {
		if a.open[ws] == nil {
			a.open[ws] = map[string]struct{}{}
		}
		a.open[ws][key] = struct{}{}
	} else {
		delete(a.open[ws], key)
		if len(a.open[ws]) == 0 {
			delete(a.open, ws)
		}
	}
	now := len(a.open[ws]) > 0
	changed := a.changed
	a.mu.Unlock()
	if now != was && changed != nil {
		changed(ws, now)
	}
}
