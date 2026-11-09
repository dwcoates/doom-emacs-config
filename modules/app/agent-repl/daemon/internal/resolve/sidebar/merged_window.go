package sidebar

import (
	"sort"
	"time"

	"agent-repld/internal/wsm"
)

// WithClock replaces the clock a status change is stamped by.
func WithClock(now func() time.Time) Option {
	return func(r *resolver) {
		r.now = now
	}
}

// recentMerges orders the SETTLED merges newest first. There is no window and
// no count cap (owner request, 2026-11-08; it was the last 24 hours): the
// webapp shows ten of a repository's merges and loads ten more as the reader
// scrolls. Ties on the merge instant keep the registry's order, so the order
// is stable.
func recentMerges(settled []wsm.Workspace) []wsm.Workspace {
	kept := append([]wsm.Workspace(nil), settled...)
	sort.SliceStable(kept, func(i, j int) bool { return kept[i].MergedAt.After(*kept[j].MergedAt) })
	return kept
}
