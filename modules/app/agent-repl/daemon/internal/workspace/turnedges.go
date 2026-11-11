package workspace

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/noticeloop"
	"agent-repld/internal/wsm"
)

// turnEdge is which edge of a turn moves a TurnEdgeRunner.
type turnEdge int

const (
	// turnStarts is the shim taking a turn.
	turnStarts turnEdge = iota + 1
	// turnEnds is a turn ending, and turns an adoption found ended while no
	// daemon watched, so work a daemon restart cut short is carried out once
	// the successor learns the turn is over.
	turnEnds
)

// TurnEdgeRunner carries out, at one edge of every workspace's turn, the work
// that edge owes the workspace. It implements wsedges.Observer, and only its
// own edge moves it; every other edge is nothing to it.
//
// AN EDGE NEVER WAITS ON THE WORK: it only appends, and Run carries each out.
type TurnEdgeRunner struct {
	edge   turnEdge
	op     string
	failed string
	carry  func(ctx context.Context, ws ids.WorkspaceID) error
	log    dlog.Logger
	loop   *noticeloop.Loop[ids.WorkspaceID]
}

// newTurnEdgeRunner builds the runner that carries out CARRY at EDGE. OP
// files its records: UNTOLD is the record of an edge reported after Run
// returned, as the daemon stood down, and FAILED is the record of a CARRY
// that failed.
func newTurnEdgeRunner(edge turnEdge, op, untold, failed string, carry func(ctx context.Context, ws ids.WorkspaceID) error, log dlog.Logger) *TurnEdgeRunner {
	r := &TurnEdgeRunner{edge: edge, op: op, failed: failed, carry: carry, log: log}
	r.loop = noticeloop.New(func(ws ids.WorkspaceID) {
		log.Info(op, untold, dlog.Context{"workspace": string(ws)})
	})
	return r
}

// report appends WS's work when EDGE is the runner's own.
func (r *TurnEdgeRunner) report(edge turnEdge, ws ids.WorkspaceID) {
	if edge == r.edge {
		r.loop.Report(ws)
	}
}

// TurnStarted implements wsedges.Observer.
func (r *TurnEdgeRunner) TurnStarted(ws ids.WorkspaceID) { r.report(turnStarts, ws) }

// TurnEnded implements wsedges.Observer.
func (r *TurnEdgeRunner) TurnEnded(ws ids.WorkspaceID) { r.report(turnEnds, ws) }

// TurnEndedUnobserved implements wsedges.Observer.
func (r *TurnEdgeRunner) TurnEndedUnobserved(ws ids.WorkspaceID) { r.report(turnEnds, ws) }

// NeedsInput implements wsedges.Observer: no turn-edge work waits on an ask.
func (r *TurnEdgeRunner) NeedsInput(ids.WorkspaceID) {}

// Landed implements wsedges.Observer: no turn-edge work waits on a landing.
func (r *TurnEdgeRunner) Landed(ids.WorkspaceID) {}

// Closed implements wsedges.Observer: no turn-edge work waits on a close.
func (r *TurnEdgeRunner) Closed(wsm.Workspace, wsm.CloseHow) {}

// PullRequestOwned implements wsedges.Observer: no turn-edge work waits on a
// pull request.
func (r *TurnEdgeRunner) PullRequestOwned(ids.WorkspaceID, wsm.PullRequestKey) {}

// Deployed implements wsedges.Observer: no turn-edge work waits on a deploy.
func (r *TurnEdgeRunner) Deployed(ids.WorkspaceID, string) {}

// Evicted implements wsedges.Observer: no turn-edge work waits on an eviction.
func (r *TurnEdgeRunner) Evicted(ids.WorkspaceID, string) {}

// ColdGated implements wsedges.Observer: no turn-edge work waits on a cold
// gate.
func (r *TurnEdgeRunner) ColdGated(ids.WorkspaceID) {}

// Run carries out every reported edge's work, in order, until ctx ends, then
// carries out every one still pending before it returns.
func (r *TurnEdgeRunner) Run(ctx context.Context) {
	r.loop.Run(ctx, func(ctx context.Context, ws ids.WorkspaceID) {
		if err := r.carry(ctx, ws); err != nil {
			r.log.Error(r.op, r.failed, dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
		}
	})
}
