package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/noticeloop"
	"agent-repld/internal/wsm"
)

const opSelfClose = "daemon.workspace.self_close"

// SelfCloses carries out, at the edge where a turn ends, the close a
// workspace asked of itself from inside that turn (CloseSelf). It implements
// wsedges.Observer, and only the turn-end edges move it: a turn closing, and
// turns an adoption found ended while no daemon watched -- so a self-close a
// daemon restart cut short is carried out once the successor learns the
// asking turn is over.
//
// AN EDGE NEVER WAITS ON A CLOSE: it only appends, and Run carries each out.
type SelfCloses struct {
	carry func(ctx context.Context, ws ids.WorkspaceID) error
	log   dlog.Logger
	loop  *noticeloop.Loop[ids.WorkspaceID]
}

// NewSelfCloses builds the observer over CARRY, the verbs'
// CarryOutSelfClose.
func NewSelfCloses(carry func(ctx context.Context, ws ids.WorkspaceID) error, log dlog.Logger) *SelfCloses {
	s := &SelfCloses{carry: carry, log: log}
	s.loop = noticeloop.New(func(ws ids.WorkspaceID) {
		log.Info(opSelfClose, "a turn ended after the self-close carrier stopped, as the daemon stood down; the next daemon carries the close out", dlog.Context{"workspace": string(ws)})
	})
	return s
}

// TurnEnded implements wsedges.Observer.
func (s *SelfCloses) TurnEnded(ws ids.WorkspaceID) { s.loop.Report(ws) }

// TurnEndedUnobserved implements wsedges.Observer.
func (s *SelfCloses) TurnEndedUnobserved(ws ids.WorkspaceID) { s.loop.Report(ws) }

// TurnStarted implements wsedges.Observer: no close waits on a start.
func (s *SelfCloses) TurnStarted(ids.WorkspaceID) {}

// NeedsInput implements wsedges.Observer: no close waits on an ask.
func (s *SelfCloses) NeedsInput(ids.WorkspaceID) {}

// Landed implements wsedges.Observer: no close waits on a landing.
func (s *SelfCloses) Landed(ids.WorkspaceID) {}

// Closed implements wsedges.Observer: a close ends its own mark.
func (s *SelfCloses) Closed(wsm.Workspace) {}

// PullRequestOwned implements wsedges.Observer: no close waits on a pull
// request.
func (s *SelfCloses) PullRequestOwned(ids.WorkspaceID, wsm.PullRequestKey) {}

// Run carries out every reported turn end's self-close, in order, until ctx
// ends, then carries out every one still pending before it returns.
func (s *SelfCloses) Run(ctx context.Context) {
	s.loop.Run(ctx, func(ctx context.Context, ws ids.WorkspaceID) {
		if err := s.carry(ctx, ws); err != nil {
			s.log.Error(opSelfClose, "could not carry out the close a workspace asked of itself at its turn's end", dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
		}
	})
}

// CarryOutSelfClose implements Verbs.
func (v *verbs) CarryOutSelfClose(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.record(ctx, opSelfClose, ws)
	if err != nil {
		return err
	}
	// AN INTAKE WORKSPACE IS RETIRED AT EVERY TURN END, asked or not: the
	// retirement closes it, so a self-close it also asked for needs no taking.
	intake, err := v.deps.DB.IsIntake(ctx, ws)
	if err != nil {
		log.Error(opSelfClose, "could not read whether the workspace whose turn ended is an intake workspace", dlog.WithCause(nil, err))
		return fmt.Errorf("turn end of %q: read whether it is an intake workspace: %w", ws, err)
	}
	if intake {
		return v.retireIntake(ctx, ws)
	}
	if record.CloseWhenTurnEnds == nil {
		return nil
	}
	return v.takeSelfClose(ctx, log, ws)
}
