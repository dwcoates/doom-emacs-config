package sidebar

import (
	"sort"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// MergedWindow is how far back the Recently Merged band reaches: every merge
// of the last 24 hours, and no others (owner request, 2026-10-31; it was the
// 100 newest merges). An older merge is drawn nowhere.
const MergedWindow = 24 * time.Hour

// AfterFunc runs f once, D from now, off the caller's goroutine, and returns
// the stop that cancels it (time.AfterFunc's shape).
type AfterFunc func(d time.Duration, f func()) (stop func() bool)

// timerAfter is the production AfterFunc: a real timer.
func timerAfter(d time.Duration, f func()) func() bool {
	return time.AfterFunc(d, f).Stop
}

// WithClock replaces the clock the Recently Merged window is measured against
// and a status change is stamped by, and the timer that re-renders when a
// merge ages out of the window.
func WithClock(now func() time.Time, after AfterFunc) Option {
	return func(r *resolver) {
		r.now = now
		r.after = after
	}
}

// recentMerges keeps the SETTLED merges inside the window ending at NOW,
// newest first, and says when the oldest of them ages out of it (nil when
// none is kept). A merge exactly MergedWindow old has aged out. Ties on the
// merge instant keep the registry's order, so the order is stable.
func recentMerges(settled []wsm.Workspace, now time.Time) ([]wsm.Workspace, *time.Time) {
	var kept []wsm.Workspace
	var agesOut *time.Time
	for _, ws := range settled {
		end := ws.MergedAt.Add(MergedWindow)
		if !end.After(now) {
			continue
		}
		kept = append(kept, ws)
		if agesOut == nil || end.Before(*agesOut) {
			agesOut = &end
		}
	}
	sort.SliceStable(kept, func(i, j int) bool { return kept[i].MergedAt.After(*kept[j].MergedAt) })
	return kept, agesOut
}

// armAgingOut keeps ONE re-render armed for the instant the oldest shown
// merge ages out, so the merge leaves the band on that edge rather than on
// whichever unrelated fact happens to render next. A render whose oldest
// shown merge ages out when the armed one does keeps it; any other replaces
// it. Called under the lock, from render.
//
// A re-render is always measured at its own NOW, so a timer that fires after
// it was replaced only renders the roster it would have rendered anyway.
func (r *resolver) armAgingOut(agesOut *time.Time, log dlog.Logger) {
	if agesOut != nil && r.agingOutAt != nil && agesOut.Equal(*r.agingOutAt) {
		return
	}
	if r.agingOut != nil {
		r.agingOut()
		r.agingOut = nil
		r.agingOutAt = nil
	}
	if agesOut == nil {
		return
	}
	wait := agesOut.Sub(r.now())
	r.agingOutAt = agesOut
	r.agingOut = r.after(wait, func() {
		r.mutate("daemon.sidebar.merged_aged_out", "the roster dropped a merge older than the Recently Merged window",
			dlog.Context{"window": MergedWindow.String()}, r.log.Global(), func() {})
	})
	log.Debug("daemon.sidebar.merged_aging_armed", "the roster armed the re-render for the oldest shown merge aging out",
		dlog.Context{"ages_out": agesOut.UTC().Format(time.RFC3339), "wait": wait.String()})
}
