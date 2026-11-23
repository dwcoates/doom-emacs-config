package sidebar

import (
	"agent-repld/internal/dlog"
)

// THE ROSTER'S DECISION RECORDS ARE LOGGED WHEN THEY CHANGE, not on every
// render (owner ruling, 2026-10-12). A render draws every workspace's row --
// in each of the roster's views -- and every row logs its decisions at debug,
// so a state change of one workspace re-logged every other workspace's
// unchanged decisions: four records a row, three views, every workspace,
// every change -- 26% of a test daemon's records and the bulk of an owner's
// debug window.
//
// While debug is enabled a render logs through a dlog.RepeatFilter, which
// gathers its debug records by the workspace they name and logs only the
// groups that changed since the last render; one record per render counts
// what it skipped, so the skipping is itself on the record. At info nothing
// is gathered.

// newRepeatFilter wraps log for one render, against the groups the last
// render logged (last, which the filter updates).
func newRepeatFilter(log dlog.Logger, last map[string]string) *repeatFilter {
	return &repeatFilter{RepeatFilter: dlog.NewRepeatFilter(log, last), log: log}
}

// repeatFilter is a render's filter while debug is enabled (its
// RepeatFilter is the render's Logger), and the logger its end records the
// skipped rows on.
type repeatFilter struct {
	*dlog.RepeatFilter
	log dlog.Logger
}

// flush logs every workspace group that differs from its last render's, and
// counts the ones it skipped.
func (f *repeatFilter) flush() {
	if skipped, drawn := f.Flush(); skipped > 0 {
		f.log.Debug("daemon.sidebar.unchanged_rows", "the roster drew rows whose decisions are unchanged since its last render; they are not logged again",
			dlog.Context{"unchanged": skipped, "drawn": drawn})
	}
}
