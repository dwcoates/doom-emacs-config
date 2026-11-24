package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of the user's INTERACTION with a workspace:
// the status arm the user last interacted on (viewed_status), which the
// roster draws the row viewed by, and when that interaction was
// (last_interacted_at), which the recency walk orders by. Both are written
// from the one report the user's interaction makes
// (agentrepl.v1.ReportUserInteraction), so a restart of the daemon draws
// every row viewed or not as it stood and keeps the recency it had.

// interactionDDL is the layout-198 addition: workspaces.viewed_status, the
// roster status arm the user last interacted on (NULL when the workspace is
// not viewed), and workspaces.last_interacted_at, the interaction instant in
// nanoseconds (NULL when the user never interacted).
//
// IT IS ADDITIVE: the previous build names neither column, so a row it writes
// keeps NULL, which reads as a workspace that is not viewed and was never
// interacted with.
const interactionDDL = `
ALTER TABLE workspaces ADD COLUMN viewed_status TEXT;
ALTER TABLE workspaces ADD COLUMN last_interacted_at INTEGER;
`

// retiredResultReadDDL is the layout-199 contract: workspaces.result_read,
// whether the user had seen the last turn's result. The viewed fact is
// viewed_status now (interactionDDL), which no status change survives, and
// no build past layout 198 reads result_read.
const retiredResultReadDDL = `
ALTER TABLE workspaces DROP COLUMN result_read;
`

// SetViewedStatus records the status arm the user is viewing the workspace
// on, or clears it when ARM is empty. The roster is its one writer: it sets
// the arm when the user interacts with the workspace and clears it on the
// workspace's next status change, so the record always says what the row
// drew. An unknown workspace is refused rather than silently affecting no
// rows.
func (s *store) SetViewedStatus(ctx context.Context, id WorkspaceID, arm string) error {
	const op = "daemon.wsm.set_viewed_status"
	fields := dlog.Context{"workspace": string(id), "viewed_status": arm}
	var value any
	if arm != "" {
		value = arm
	}
	return s.setWorkspaceColumns(ctx, op, id, fields, workspaceColumn{name: "viewed_status", value: value})
}

// SetInteractedAt advances the workspace's interaction instant to AT.
//
// IT ONLY EVER MOVES FORWARD (advanceInstant), as the status change instant
// does: an interaction older than the one recorded leaves the recorded one
// standing, so the recency it orders by never runs backward. An unknown
// workspace is refused rather than silently affecting no rows.
func (s *store) SetInteractedAt(ctx context.Context, id WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.set_interacted_at"
	fields := dlog.Context{"workspace": string(id), "last_interacted_at": at}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := advanceInstant(ctx, tx, "last_interacted_at", id, at)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}
