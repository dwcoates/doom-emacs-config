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
//
// A READ-ONLY HANDLE HOLDS IT. A joining successor's roster moves rows' arms
// while the incumbent is still the writer (2026-11-13: a mirrored row's arm
// returning to the successor's own refused the write); the latest instant per
// workspace is kept and written at the promotion (writeHeldStatusChangesLocked),
// so no status change is lost and none is refused.
func (s *store) SetStatusChangedAt(ctx context.Context, id WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.set_status_changed_at"
	fields := dlog.Context{"workspace": string(id), "status_changed_at": at}
	s.mu.RLock()
	if s.ReadOnly() {
		s.holdStatusChangeLocked(id, at)
		s.mu.RUnlock()
		s.log.Debug(op, "held the workspace's status change instant until this handle writes; the incumbent is still the writer", fields)
		return nil
	}
	s.mu.RUnlock()
	return s.writeStatusChangedAt(ctx, op, fields, id, at)
}

// writeStatusChangedAt is the instant's one write.
func (s *store) writeStatusChangedAt(ctx context.Context, op string, fields dlog.Context, id WorkspaceID, at time.Time) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := advanceInstant(ctx, tx, "status_changed_at", id, at)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}

// holdStatusChangeLocked keeps at as id's latest held instant. The caller
// holds mu's read side.
func (s *store) holdStatusChangeLocked(id WorkspaceID, at time.Time) {
	s.heldMu.Lock()
	defer s.heldMu.Unlock()
	if s.heldStatusChanges == nil {
		s.heldStatusChanges = make(map[WorkspaceID]time.Time)
	}
	if at.After(s.heldStatusChanges[id]) {
		s.heldStatusChanges[id] = at
	}
}

// writeHeldStatusChangesLocked writes every held status change instant and
// forgets it. The caller holds mu EXCLUSIVELY and the handle already writes.
// A failed write is recorded at ERROR by the write itself and is not held
// again: the roster's next change of that row writes afresh.
func (s *store) writeHeldStatusChangesLocked(ctx context.Context) {
	const op = "daemon.wsm.set_status_changed_at"
	s.heldMu.Lock()
	held := s.heldStatusChanges
	s.heldStatusChanges = nil
	s.heldMu.Unlock()
	for id, at := range held {
		fields := dlog.Context{"workspace": string(id), "status_changed_at": at, "held": true}
		if err := s.writeStatusChangedAt(ctx, op, fields, id, at); err == nil {
			s.log.Debug(op, "wrote the status change instant held while this handle was read-only", fields)
		}
	}
}
