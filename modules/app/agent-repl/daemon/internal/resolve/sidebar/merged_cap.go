package sidebar

import (
	"sort"
	"time"

	"agent-repld/internal/wsm"
)

// MergedShown is how many merges the Recently Merged band lists: the newest
// MergedShown, and no others (owner ruling, 2026-10-15: the rail shows open
// workspaces, hidden workspaces and the 100 most recent merges, and nothing
// else). A merge leaves the band in the render that carries the newer merge
// displacing it, so nothing waits on a clock.
const MergedShown = 100

// WithClock replaces the clock a status change is stamped by.
func WithClock(now func() time.Time) Option {
	return func(r *resolver) {
		r.now = now
	}
}

// recentMerges keeps the newest MergedShown of the SETTLED merges. Ties on
// the merge instant keep the registry's order, so the cut is stable.
func recentMerges(settled []wsm.Workspace) []wsm.Workspace {
	kept := make([]wsm.Workspace, len(settled))
	copy(kept, settled)
	sort.SliceStable(kept, func(i, j int) bool { return kept[i].MergedAt.After(*kept[j].MergedAt) })
	if len(kept) > MergedShown {
		kept = kept[:MergedShown]
	}
	return kept
}
