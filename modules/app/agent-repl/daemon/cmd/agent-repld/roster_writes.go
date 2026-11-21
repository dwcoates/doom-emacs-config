package main

import (
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/latestwrite"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// rosterWrites makes the roster's durable writes off the roster's callers.
//
// THE ROSTER NEVER WAITS ON THE STATE DATABASE. Its result and status change
// sinks run in its combined commit's report, which every roster caller waits
// for, and the state database has one connection; told there, a write queued
// behind an adoption burst held a session watcher's view delivery past the
// lockwatch threshold (2026-11-15, latestwrite). Each sink therefore only
// submits its change to a latestwrite.Writer, which writes the latest one per
// workspace on its own goroutine, and the exit closes them after the watchers
// and before the state client, so no change is lost.
type rosterWrites struct {
	results       *latestwrite.Writer[ids.WorkspaceID, *wsm.TurnResult]
	statusChanges *latestwrite.Writer[ids.WorkspaceID, time.Time]
	visibilities  *latestwrite.Writer[ids.WorkspaceID, wsm.Visibility]
}

// newRosterWrites starts the writers behind the roster's three sinks: RESULTS,
// STATUS CHANGES and VISIBILITY make the writes themselves (rosterResults,
// rosterStatusChanges, rosterVisibility) and record their own failures.
func newRosterWrites(results sidebar.ResultSink, statusChanges sidebar.StatusChangeSink, visibility func(ids.WorkspaceID, wsm.Visibility)) *rosterWrites {
	return &rosterWrites{
		// THE NEWER RESULT IS THE ONE THAT STANDS: a result submitted later
		// replaces one not yet written, as its write would have overwritten it.
		results: latestwrite.New(func(ws ids.WorkspaceID, result *wsm.TurnResult) { results(ws, result) },
			func(_, next *wsm.TurnResult) *wsm.TurnResult { return next }),
		// THE LATER INSTANT IS THE ONE THAT STANDS, as the write itself only
		// moves forward (wsm.SetStatusChangedAt).
		statusChanges: latestwrite.New(func(ws ids.WorkspaceID, at time.Time) { statusChanges(ws, at) },
			func(held, next time.Time) time.Time {
				if next.After(held) {
					return next
				}
				return held
			}),
		// THE NEWER VISIBILITY IS THE ONE THAT STANDS, as a result is.
		visibilities: latestwrite.New(visibility, func(_, next wsm.Visibility) wsm.Visibility { return next }),
	}
}

// ResultSink is the roster's result sink: it submits the change and returns.
func (w *rosterWrites) ResultSink() sidebar.ResultSink { return w.results.Submit }

// StatusChangeSink is the roster's status change sink: it submits the change
// and returns.
func (w *rosterWrites) StatusChangeSink() sidebar.StatusChangeSink { return w.statusChanges.Submit }

// VisibilitySink is the roster's visibility sink: it submits each change and
// returns.
func (w *rosterWrites) VisibilitySink() sidebar.VisibilitySink {
	return func(changes map[ids.WorkspaceID]wsm.Visibility) {
		for ws, v := range changes {
			w.visibilities.Submit(ws, v)
		}
	}
}

// Close writes every change still held and returns once every writer has.
func (w *rosterWrites) Close() {
	w.results.Close()
	w.statusChanges.Close()
	w.visibilities.Close()
}
