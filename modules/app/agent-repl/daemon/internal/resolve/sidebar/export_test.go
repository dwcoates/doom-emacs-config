package sidebar

// SortMergedNewestFirst exposes sortMergedNewestFirst to the external test
// package.
var SortMergedNewestFirst = sortMergedNewestFirst

// SortHiddenNewestFirst exposes sortHiddenNewestFirst to the external test
// package.
var SortHiddenNewestFirst = sortHiddenNewestFirst

// RunningTurnStart exports runningTurnStart, the one reading of "a turn is
// running" off the footer clock.
var RunningTurnStart = runningTurnStart

// ProjectArm exports projectArm, the one projection of the footer's status
// onto a roster arm.
var ProjectArm = projectArm

// PatchedRegistry exposes patchedRegistry, the registry a select's change
// installs into.
var PatchedRegistry = patchedRegistry

// SetOnQueued installs the hook the combined commit tells the queue's depth
// each time a change joins it (combine.go).
func SetOnQueued(r Resolver, hook func(depth int)) { r.(*resolver).onQueued = hook }

// OwnMergeDrawn exposes ownMergeDrawn to the external test package.
var OwnMergeDrawn = ownMergeDrawn
