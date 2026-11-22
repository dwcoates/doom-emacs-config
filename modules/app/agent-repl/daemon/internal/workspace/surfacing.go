package workspace

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A FOLD HIDES A WORKING WORKSPACE ONLY UNTIL ITS TURN STARTS (owner request,
// 2026-11-15, replacing the 2026-10-29 unfold). A workspace that starts a turn
// while a fold hides it (its repository's, its subsection's, or its
// ancestors' children fold) is marked surfaced: the roster draws it, and only
// it, inside the fold. The fold itself stays folded. The mark comes off when
// the turn ends, and folding the section again leaves it.

const (
	opSurfaceForWork = "daemon.workspace.surface_for_work"
	opEndSurfacing   = "daemon.workspace.end_surfacing"
)

// NewWorkSurfacings builds the runner that, at the edge where a turn starts,
// marks its workspace surfaced when a fold hides it. SURFACE is the verbs'
// SurfaceForWork.
func NewWorkSurfacings(surface func(ctx context.Context, ws ids.WorkspaceID) error, log dlog.Logger) *TurnEdgeRunner {
	return newTurnEdgeRunner(turnStarts, opSurfaceForWork,
		"a turn started after the surfacing runner stopped, as the daemon stood down; its workspace stays as it was",
		"could not surface a workspace whose turn started under a fold", surface, log)
}

// NewWorkSurfacingEnds builds the runner that, at the edge where a turn ends,
// takes its workspace's surfaced mark off, so the fold hides it again. END is
// the verbs' EndSurfacing.
func NewWorkSurfacingEnds(end func(ctx context.Context, ws ids.WorkspaceID) error, log dlog.Logger) *TurnEdgeRunner {
	return newTurnEdgeRunner(turnEnds, opEndSurfacing,
		"a turn ended after the surfacing runner stopped, as the daemon stood down; its workspace keeps its mark until the next start of the daemon",
		"could not end the surfacing of a workspace whose turn ended", end, log)
}

// SurfaceForWork implements Verbs. The roster is republished only when a fold
// hid the workspace, so a turn in an open section costs one write that
// touches nothing.
func (v *verbs) SurfaceForWork(ctx context.Context, ws ids.WorkspaceID) error {
	log := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	surfaced, err := v.deps.DB.SurfaceHeldWork(ctx, ws)
	if err != nil {
		return err
	}
	if !surfaced {
		return nil
	}
	log.Info(opSurfaceForWork, "a workspace's turn started under a fold, so the roster draws it inside the fold until the turn ends", nil)
	v.republishRegistry(ctx, log, opSurfaceForWork)
	return nil
}

// EndSurfacing implements Verbs, republishing only when the workspace carried
// the mark.
func (v *verbs) EndSurfacing(ctx context.Context, ws ids.WorkspaceID) error {
	log := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	ended, err := v.deps.DB.EndSurfacing(ctx, ws)
	if err != nil {
		return err
	}
	if !ended {
		return nil
	}
	log.Info(opEndSurfacing, "a surfaced workspace's turn ended, so its fold hides it again", nil)
	v.republishRegistry(ctx, log, opEndSurfacing)
	return nil
}
