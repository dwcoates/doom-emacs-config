package main

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// visibilityWriter is the one write the roster's visibility sink makes.
type visibilityWriter interface {
	SetVisibilities(ctx context.Context, changes map[wsm.WorkspaceID]wsm.Visibility) error
}

// rosterVisibility keeps one workspace's visibility durable, so a boot or a
// deploy reads it before it allocates anything (wsm.Workspace.Visibility). A
// failed write is recorded at ERROR; the roster already gates on the live
// value, and only the next boot reads the stale one.
func rosterVisibility(db visibilityWriter, log dlog.Logger) func(ws ids.WorkspaceID, v wsm.Visibility) {
	return rosterWrite(log, "daemon.cmd.roster_visibilities", logsite.Msg("could not keep the workspace's visibility durable"),
		func(ctx context.Context, ws ids.WorkspaceID, v wsm.Visibility) error {
			return db.SetVisibilities(ctx, map[wsm.WorkspaceID]wsm.Visibility{ws: v})
		},
		func(v wsm.Visibility) dlog.Context { return dlog.Context{"visibility": v.String()} })
}
