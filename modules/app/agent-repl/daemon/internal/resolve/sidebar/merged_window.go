package sidebar

import (
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// MergedWindow is how far back the Recently Merged section reaches: a week's
// worth of merges (owner request, 2026-10-11). An older merge is drawn
// nowhere.
const MergedWindow = 7 * 24 * time.Hour

// AfterFunc runs f once, D from now, off the caller's goroutine, and returns
// the stop that cancels it (time.AfterFunc's shape).
type AfterFunc func(d time.Duration, f func()) (stop func() bool)

// timerAfter is the production AfterFunc: a real timer.
func timerAfter(d time.Duration, f func()) func() bool {
	return time.AfterFunc(d, f).Stop
}

// WithClock replaces the clock the Recently Merged window is measured against
// and a status change is stamped by, and the timer that re-renders when a merge ages out of it.
func WithClock(now func() time.Time, after AfterFunc) Option {
	return func(r *resolver) {
		r.now = now
		r.after = after
	}
}

// recentMerges keeps the merges inside the window ending at NOW, and says when
// the oldest of them ages out of it (nil when none is kept). A merge exactly
// MergedWindow old has aged out.
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
	return kept, agesOut
}

// armAgingOut arms the ONE re-render that drops the oldest shown merge the
// instant it ages out, replacing any armed before it. A merge leaves the band
// on that edge, not on whichever unrelated fact happens to render next.
// Called under the lock, from render.
func (r *resolver) armAgingOut(agesOut *time.Time, log dlog.Logger) {
	if r.agingOut != nil {
		r.agingOut()
		r.agingOut = nil
	}
	if agesOut == nil {
		return
	}
	wait := agesOut.Sub(r.now())
	r.agingOut = r.after(wait, func() {
		r.mutate("daemon.sidebar.merged_aged_out", "the roster dropped a merge older than the Recently Merged window",
			dlog.Context{"window": MergedWindow.String()}, r.log.Global(), func() {})
	})
	log.Debug("daemon.sidebar.merged_aging_armed", "the roster armed the re-render for the oldest shown merge aging out",
		dlog.Context{"ages_out": agesOut.UTC().Format(time.RFC3339), "wait": wait.String()})
}
