package sidebar

import (
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// RenderLevel exposes renderLevel, the level a render's timing record is
// written at.
var RenderLevel = renderLevel

// SortMergedNewestFirst exposes sortMergedNewestFirst to the external test
// package.
var SortMergedNewestFirst = sortMergedNewestFirst

// SortBackloggedNewestFirst exposes sortBackloggedNewestFirst to the external test
// package.
var SortBackloggedNewestFirst = sortBackloggedNewestFirst

// RunningTurnStart exports runningTurnStart, the one reading of "a turn is
// running" off the footer clock.
var RunningTurnStart = runningTurnStart

// DepartureArm exports departureArm, the arms a quiet roster still
// publishes.
var DepartureArm = departureArm

// ProjectArm exports projectArm, the one projection of the footer's status
// onto a roster arm.
var ProjectArm = projectArm

// PatchedRegistry exposes patchedRegistry, the registry a select's change
// installs into.
var PatchedRegistry = patchedRegistry

// SetOnQueued installs the hook the combined commit tells the queue's depth
// each time a change joins it (combine.go).
func SetOnQueued(r Resolver, hook func(depth int)) { r.(*resolver).onQueued = hook }

// SetOnSettleWaits installs the hook Settle tells each time it waits for an
// earlier batch's report (combine.go).
func SetOnSettleWaits(r Resolver, hook func()) { r.(*resolver).onSettleWaits = hook }

// OwnMergeDrawn exposes ownMergeDrawn to the external test package.
var OwnMergeDrawn = ownMergeDrawn

// OpenCount exposes openCount, the one count every Open-section count is.
var OpenCount = openCount

// UnrenderedHolding answers a resolver whose registry carries WORKSPACES and
// that has rendered nothing yet, as a daemon that just booted or joined is
// until its first render.
func UnrenderedHolding(workspaces ...wsm.Workspace) Resolver {
	c := &core{state: newRosterState(), log: dlog.NewTestSurfaces()}
	c.initQueue()
	c.state.reg.Workspaces = workspaces
	c.state.index = indexRegistry(c.state.reg)
	return c.commitView()
}

// UnrenderedRegistry answers a resolver holding REG that has rendered nothing
// yet, as a daemon that just booted or joined is until its first render.
func UnrenderedRegistry(reg Registry) Resolver {
	c := &core{state: newRosterState(), log: dlog.NewTestSurfaces()}
	c.initQueue()
	c.state.reg = reg
	c.state.index = indexRegistry(c.state.reg)
	return c.commitView()
}

// IndexedRecord answers rosterState.record for WS in a roster holding REG.
func IndexedRecord(reg Registry, ws ids.WorkspaceID) (*wsm.Workspace, bool) {
	state := newRosterState()
	state.reg, state.index = reg, indexRegistry(reg)
	return state.record(ws)
}
