package main

import (
	"context"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/wsm"
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
	return func(ws ids.WorkspaceID, result *wsm.TurnResult) {
		if err := db.SetResult(context.Background(), ws, result); err != nil {
			ctx := dlog.Context{"workspace": string(ws), "cause": err.Error(), "cleared": result == nil}
			if result != nil {
				ctx["end"], ctx["read"] = string(result.End), result.Read
			}
			log.Error("daemon.cmd.roster_results", "could not keep the workspace's last turn result durable", ctx)
		}
	}
}
