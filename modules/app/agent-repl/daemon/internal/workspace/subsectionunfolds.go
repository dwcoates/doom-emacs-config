package workspace

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/noticeloop"
	"agent-repld/internal/wsm"
)

const opUnfoldForWork = "daemon.workspace.unfold_subsection_for_work"

// SubsectionUnfolds expands, at the edge where a turn starts, the collapsed
// subsection its workspace sits in (owner request, 2026-10-29): a working
// workspace is never left hidden inside a folded subsection. It implements
// wsedges.Observer, and only the turn-start edge moves it.
//
// AN EDGE NEVER WAITS ON A WRITE: it only appends, and Run carries each out.
type SubsectionUnfolds struct {
	expand func(ctx context.Context, ws ids.WorkspaceID) error
	log    dlog.Logger
	loop   *noticeloop.Loop[ids.WorkspaceID]
}

// NewSubsectionUnfolds builds the observer over EXPAND, the verbs'
// UnfoldSubsectionForWork.
func NewSubsectionUnfolds(expand func(ctx context.Context, ws ids.WorkspaceID) error, log dlog.Logger) *SubsectionUnfolds {
	s := &SubsectionUnfolds{expand: expand, log: log}
	s.loop = noticeloop.New(func(ws ids.WorkspaceID) {
		log.Info(opUnfoldForWork, "a turn started after the subsection unfolder stopped, as the daemon stood down; its subsection stays as it was", dlog.Context{"workspace": string(ws)})
	})
	return s
}

// TurnStarted implements wsedges.Observer.
func (s *SubsectionUnfolds) TurnStarted(ws ids.WorkspaceID) { s.loop.Report(ws) }

// TurnEnded implements wsedges.Observer: no unfold waits on an end.
func (s *SubsectionUnfolds) TurnEnded(ids.WorkspaceID) {}

// TurnEndedUnobserved implements wsedges.Observer: no unfold waits on an end.
func (s *SubsectionUnfolds) TurnEndedUnobserved(ids.WorkspaceID) {}

// NeedsInput implements wsedges.Observer: no unfold waits on an ask.
func (s *SubsectionUnfolds) NeedsInput(ids.WorkspaceID) {}

// Landed implements wsedges.Observer: no unfold waits on a landing.
func (s *SubsectionUnfolds) Landed(ids.WorkspaceID) {}

// Closed implements wsedges.Observer: no unfold waits on a close.
func (s *SubsectionUnfolds) Closed(wsm.Workspace) {}

// Run carries out every reported turn start's unfold, in order, until ctx
// ends, then carries out every one still pending before it returns.
func (s *SubsectionUnfolds) Run(ctx context.Context) {
	s.loop.Run(ctx, func(ctx context.Context, ws ids.WorkspaceID) {
		if err := s.expand(ctx, ws); err != nil {
			s.log.Error(opUnfoldForWork, "could not unfold the subsection of a workspace whose turn started", dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
		}
	})
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
	if !expanded {
		return nil
	}
	log.Info(opUnfoldForWork, "a workspace's turn started, so the collapsed subsection holding it is expanded", nil)
	v.republishRegistry(ctx, log, opUnfoldForWork)
	return nil
}
