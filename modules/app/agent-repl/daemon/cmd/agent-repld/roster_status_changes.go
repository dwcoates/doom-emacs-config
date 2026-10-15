package main

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// statusChangeWriter is the one write the roster's status change sink makes.
type statusChangeWriter interface {
	SetStatusChangedAt(ctx context.Context, id wsm.WorkspaceID, at time.Time) error
}

// rosterStatusChanges is the roster's status change sink: it keeps the instant
// each workspace's status arm last moved durable, so a daemon that did not see
// the change -- a successor after a handover, a restart -- still ranks the row
// by it (frontend.v1.RosterRowLastUpdated). A write that fails is recorded at
// ERROR; the roster already ranks by the live instant, and only the next
// daemon's ranking misses it.
func rosterStatusChanges(db statusChangeWriter, log dlog.Logger) sidebar.StatusChangeSink {
	return func(ws ids.WorkspaceID, at time.Time) {
		if err := db.SetStatusChangedAt(context.Background(), ws, at); err != nil {
			log.Error("daemon.cmd.roster_status_changes", "could not keep the workspace's status change instant durable",
				dlog.Context{"workspace": string(ws), "status_changed_at": at, "cause": err.Error()})
		}
	}
}
