package workspace

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

const opUnfoldForWork = "daemon.workspace.unfold_subsection_for_work"

// NewSubsectionUnfolds builds the runner that expands, at the edge where a
// turn starts, the collapsed subsection its workspace sits in (owner request,
// 2026-10-29): a working workspace is never left hidden inside a folded
// subsection. EXPAND is the verbs' UnfoldSubsectionForWork.
func NewSubsectionUnfolds(expand func(ctx context.Context, ws ids.WorkspaceID) error, log dlog.Logger) *TurnEdgeRunner {
	return newTurnEdgeRunner(turnStarts, opUnfoldForWork,
		"a turn started after the subsection unfolder stopped, as the daemon stood down; its subsection stays as it was",
		"could not unfold the subsection of a workspace whose turn started", expand, log)
}

// UnfoldSubsectionForWork implements Verbs. The roster is republished only
// when a fold changed, so a turn in an open subsection costs one write that
// touches nothing.
func (v *verbs) UnfoldSubsectionForWork(ctx context.Context, ws ids.WorkspaceID) error {
	log := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	expanded, err := v.deps.DB.ExpandSubsectionHolding(ctx, ws)
	if err != nil {
		return err
	}
	expandedChildren, err := v.deps.DB.ExpandChildrenFoldsHolding(ctx, ws)
	if err != nil {
		return err
	}
	if !expanded && !expandedChildren {
		return nil
	}
	log.Info(opUnfoldForWork, "a workspace's turn started, so the collapsed folds holding it are expanded",
		dlog.Context{"expanded_subsection": expanded, "expanded_children": expandedChildren})
	v.republishRegistry(ctx, log, opUnfoldForWork)
	return nil
}
