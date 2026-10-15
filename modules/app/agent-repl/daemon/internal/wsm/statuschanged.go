package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of a workspace's STATUS CHANGE instant: when
// the roster's status arm for the workspace last moved. It is one of the
// instants the roster's update recency (frontend.v1.RosterRowLastUpdated) is
// the latest of, and the only one no other write records: an open, a re-open,
// a permission ask and a fault each move the arm without any lifecycle write.
// Kept durable so a restart of the daemon still ranks a workspace by the
// status change that last moved it.

// statusChangedAtDDL is the layout-54 addition: workspaces.status_changed_at,
// the status change instant in nanoseconds.
//
// IT IS ADDITIVE: the previous build names no such column, so a row it writes
// keeps NULL, which reads as a workspace with no recorded status change.
const statusChangedAtDDL = `
ALTER TABLE workspaces ADD COLUMN status_changed_at INTEGER;
`

// SetStatusChangedAt advances the workspace's status change instant to at.
//
// IT ONLY EVER MOVES FORWARD (advanceInstant), as bumpLastActivity does: an instant
// older than the one recorded (a report a slower write overtook) leaves the
// recorded one standing, so the update recency it feeds never runs backward.
// An unknown workspace is refused rather than silently affecting no rows.
func (s *store) SetStatusChangedAt(ctx context.Context, id WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.set_status_changed_at"
	fields := dlog.Context{"workspace": string(id), "status_changed_at": at}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := advanceInstant(ctx, tx, "status_changed_at", id, at)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}
