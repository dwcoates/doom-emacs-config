package workspacenotice

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// ReportingCloses answers DB with every close REPORTED: a MarkClosed that
// closes an open workspace reports Landed when its branch landed
// (wsm.Workspace.Landed) and ClosedWithoutLanding when it did not. Every close
// path -- the user's close and kill, a landing's close, the daemon's boot
// housekeeping -- goes through MarkClosed, so wrapping it is what makes a
// close that tells nobody unrepresentable, rather than a hook each path must
// remember. Re-closing a closed row reports nothing.
func ReportingCloses(db wsm.DB, s *Service) wsm.DB {
	return closingDB{DB: db, s: s}
}

type closingDB struct {
	wsm.DB
	s *Service
}

func (d closingDB) MarkClosed(ctx context.Context, id wsm.WorkspaceID, closure wsm.Closure) error {
	before, err := d.DB.Workspace(ctx, id)
	if err != nil {
		d.s.deps.Log.Global().Error(op, "could not read a workspace before closing it, so the close is refused rather than left untold",
			dlog.WithCause(dlog.Context{"workspace": string(id), "closure_by": string(closure.By)}, err))
		return fmt.Errorf("workspacenotice: read %s before its close: %w", id, err)
	}
	if err := d.DB.MarkClosed(ctx, id, closure); err != nil {
		return err
	}
	if before.Closed {
		return nil
	}
	kind := ClosedWithoutLanding
	if before.Landed != nil {
		kind = Landed
	}
	d.s.Report(Event{Kind: kind, Workspace: before})
	return nil
}
