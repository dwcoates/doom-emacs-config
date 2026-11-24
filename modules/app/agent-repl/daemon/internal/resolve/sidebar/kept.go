package sidebar

import (
	"reflect"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A RENDER COSTS WHAT CHANGED, NOT WHAT THE REGISTRY HOLDS.
//
// Every change republishes the whole roster, and a render used to derive
// every registered workspace's arm and build every workspace's row, closed
// ones included, resolving a log sink for each by a scan of the registry. On
// 2026-11-23 a handover's successor held 1,529 workspaces, about 1,500 of
// them closed: every edge of every adopted session waited on renders of all
// of them, and `daemon.lockwatch.stall` fired twice on the watchers waiting.
//
// So each workspace's arm and row are KEPT from the render that derived them,
// with the inputs they were derived from (keptKey), and a render derives
// again only those whose inputs moved:
//   - a change about one workspace (a workspace-scoped mutation, the
//     footer's status edge) moves that workspace's version alone;
//   - a registry install moves the version of each workspace whose record or
//     session record changed, and of its parent, whose detail lists it
//     (diffRegistry); any other part of the registry moving moves the epoch;
//   - every other change (merge queues, the version, incumbent arms, a quiet
//     resync) moves the epoch, which every workspace's key carries.
// A row's key also carries what the render decides before any row is drawn
// (its arm, its visibility) and the selection, so a row is rebuilt whenever
// any of them moves. The one arm read off the clock (a hidden workspace's
// lapsing cache, armReadsClock) is derived on every render.

// keptKey is the inputs of a workspace's kept facts: its own version and the
// roster's epoch.
type keptKey struct {
	version, epoch uint64
}

// rowKey is the inputs of a workspace's kept row: its keptKey and what the
// render decided for it before drawing any row.
type rowKey struct {
	keptKey
	arm        string
	visibility wsm.Visibility
	current    bool
}

// kept is what the renders keep for one workspace.
type kept struct {
	// version moves with every change about the workspace (touch).
	version uint64

	// arm and judged are the arms resolveArms last derived, from armKey.
	armKey  keptKey
	armKept bool
	arm     string
	judged  string

	// row is the row last built, from rowKey; factsKey are the inputs its
	// facts were last derived from, by a full render or a facts-only one.
	rowKey    rowKey
	row       *frontendv1.RosterRow
	factsKey  rowKey
	factsKept bool

	// log is the workspace's resolved log sink (logFor), resolved for logDir.
	// A sink that fell back to the central one (logCentral) is kept only for
	// a closed workspace, whose directory does not come back; an open one's
	// is resolved again, since its worktree may be recreated.
	log        dlog.Logger
	logDir     string
	logCentral bool
}

// registryIndex is what the roster derives from one registry, once per
// install rather than once per render.
type registryIndex struct {
	// at is each workspace's index in the registry's Workspaces.
	at map[ids.WorkspaceID]int
	// sessions are the durable session records, by workspace.
	sessions map[ids.WorkspaceID]*wsm.Session
	// children are each workspace's children (childrenOf).
	children map[ids.WorkspaceID][]wsm.Workspace
	// lists are the registry sorted into its lists (partitionOf), and closed
	// the closed list in roster order (sortWorkspaces).
	lists  partition
	closed []wsm.Workspace
}

// indexRegistry derives REG's index.
func indexRegistry(reg Registry) registryIndex {
	out := registryIndex{
		at:       make(map[ids.WorkspaceID]int, len(reg.Workspaces)),
		sessions: make(map[ids.WorkspaceID]*wsm.Session, len(reg.Sessions)),
		children: childrenOf(reg.Workspaces),
		lists:    partitionOf(reg.Workspaces),
	}
	for i := range reg.Workspaces {
		out.at[reg.Workspaces[i].ID] = i
	}
	for i := range reg.Sessions {
		out.sessions[reg.Sessions[i].Workspace] = &reg.Sessions[i]
	}
	out.closed = sortWorkspaces(out.lists.closed)
	return out
}

// keptOf answers WS's kept facts, made at the first ask. Caller holds mu.
func (r *rosterState) keptOf(ws ids.WorkspaceID) *kept {
	k, ok := r.kept[ws]
	if !ok {
		k = &kept{}
		r.kept[ws] = k
	}
	return k
}

// touch moves WS's version: every fact kept for it is derived again.
// Caller holds mu.
func (r *rosterState) touch(ws ids.WorkspaceID) {
	r.keptOf(ws).version++
}

// touchAll moves the epoch: every fact kept for every workspace is derived
// again. Caller holds mu.
func (r *rosterState) touchAll() {
	r.epoch++
}

// keyOf answers WS's keptKey now. Caller holds mu.
func (r *rosterState) keyOf(ws ids.WorkspaceID) keptKey {
	return keptKey{version: r.keptOf(ws).version, epoch: r.epoch}
}

// rowKeyOf answers the key WS's row is drawn from in this render, whose arm
// for it is ARM. Caller holds mu.
func (r *resolver) rowKeyOf(ws ids.WorkspaceID, arm string, rc rowContext) rowKey {
	return rowKey{
		keptKey:    r.state.keyOf(ws),
		arm:        arm,
		visibility: r.state.visibility[ws],
		current:    rc.selected != nil && *rc.selected == ws,
	}
}

// setRegistry installs REG as the registry the roster holds, with its index,
// and moves the version of everything it changed (diffRegistry). Every
// install of a registry goes through it. Caller holds mu.
func (r *resolver) setRegistry(reg Registry) {
	before, beforeIndex, seen := r.state.reg, r.state.index, r.state.regSeen
	r.state.reg = reg
	r.state.index = indexRegistry(reg)
	if !seen {
		r.state.touchAll()
		return
	}
	diffRegistry(r.state, before, beforeIndex, reg)
}

// diffRegistry moves the version of every workspace whose record or session
// record differs between BEFORE and AFTER, and of each such workspace's
// parent before and after (detail lists its children); a workspace AFTER no
// longer carries moves its parent's. Any other part of the registry moving
// moves the epoch. Caller holds mu.
func diffRegistry(state *rosterState, before Registry, beforeIndex registryIndex, after Registry) {
	if !sameArrangement(before, after) {
		state.touchAll()
		return
	}
	touchWithParent := func(ws wsm.Workspace) {
		state.touch(ws.ID)
		if ws.Parent != nil {
			state.touch(*ws.Parent)
		}
	}
	seen := make(map[ids.WorkspaceID]bool, len(after.Workspaces))
	for i := range after.Workspaces {
		rec := &after.Workspaces[i]
		seen[rec.ID] = true
		at, ok := beforeIndex.at[rec.ID]
		if !ok {
			touchWithParent(*rec)
			continue
		}
		old := &before.Workspaces[at]
		if !reflect.DeepEqual(*old, *rec) {
			touchWithParent(*old)
			touchWithParent(*rec)
			continue
		}
		if !reflect.DeepEqual(beforeIndex.sessions[rec.ID], state.index.sessions[rec.ID]) {
			touchWithParent(*rec)
		}
	}
	for i := range before.Workspaces {
		if old := before.Workspaces[i]; !seen[old.ID] && old.Parent != nil {
			state.touch(*old.Parent)
		}
	}
}

// sameArrangement reports whether BEFORE and AFTER agree on every part of a
// registry but its workspaces, their sessions and its selection, which
// diffRegistry and the row keys follow on their own.
func sameArrangement(before, after Registry) bool {
	return reflect.DeepEqual(before.Repositories, after.Repositories) &&
		reflect.DeepEqual(before.Subsections, after.Subsections) &&
		reflect.DeepEqual(before.RepositoryBlocks, after.RepositoryBlocks) &&
		reflect.DeepEqual(before.Tasks, after.Tasks) &&
		reflect.DeepEqual(before.View, after.View)
}
