package main

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
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
	return func(ws ids.WorkspaceID, v wsm.Visibility) {
		if err := db.SetVisibilities(context.Background(), map[wsm.WorkspaceID]wsm.Visibility{ws: v}); err != nil {
			log.Error("daemon.cmd.roster_visibilities", "could not keep the workspace's visibility durable",
				dlog.Context{"workspace": string(ws), "visibility": v.String(), "cause": err.Error()})
		}
	}
}
