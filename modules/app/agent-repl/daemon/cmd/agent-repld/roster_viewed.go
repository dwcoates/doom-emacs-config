package main

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// viewedWriter is the one write the roster's viewed sink makes.
type viewedWriter interface {
	SetViewedStatus(ctx context.Context, id wsm.WorkspaceID, arm string) error
}

// rosterViewed is the roster's viewed sink: it keeps each workspace's viewed
// fact durable (the status arm the user last interacted on, empty when not
// viewed), so a daemon that did not see the interaction -- a successor after
// a handover, a restart -- draws the row viewed as it stood. A write that
// fails is recorded at ERROR; the roster already draws the fact it reported,
// and only the next daemon's restore misses it.
func rosterViewed(db viewedWriter, log dlog.Logger) sidebar.ViewedSink {
	return rosterWrite(log, "daemon.cmd.roster_viewed", logsite.Msg("could not keep the workspace's viewed fact durable"),
		db.SetViewedStatus,
		func(arm string) dlog.Context {
			return dlog.Context{"viewed_status": arm, "cleared": arm == ""}
		})
}
