package main

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// resultWriter is the one write the roster's result sink makes.
type resultWriter interface {
	SetResult(ctx context.Context, id wsm.WorkspaceID, result *wsm.TurnResult) error
}

// rosterResults is the roster's result sink: it keeps each workspace's last
// turn result durable, so a daemon that did not see a turn end -- a successor
// after a handover, a restart -- draws the row as it stood. A write that fails
// is recorded at ERROR; the roster already draws the result it reported, and
// only the next daemon's restore misses it.
func rosterResults(db resultWriter, log dlog.Logger) sidebar.ResultSink {
	return rosterWrite(log, "daemon.cmd.roster_results", logsite.Msg("could not keep the workspace's last turn result durable"),
		db.SetResult,
		func(result *wsm.TurnResult) dlog.Context {
			ctx := dlog.Context{"cleared": result == nil}
			if result != nil {
				ctx["end"] = string(result.End)
			}
			return ctx
		})
}
