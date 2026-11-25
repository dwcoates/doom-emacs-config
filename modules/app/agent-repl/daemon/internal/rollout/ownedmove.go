package rollout

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// opOwedMove is the operation taking up an owed move is recorded under.
const opOwedMove = "daemon.rollout.owed_move"

// takeUpOwedMoves takes up, at a fresh boot, every move renames owe the
// workspaces this daemon serves (TakeUpOwedMove): the bounce registry lives
// in memory, so a move registered before the stop is registered again.
func (c *controller) takeUpOwedMoves(ctx context.Context) {
	moves, err := c.deps.DB.OwedMoves(ctx)
	if err != nil {
		c.log.Error(opOwedMove, "could not list the moves renames owe; they wait for the next daemon that serves their workspaces", dlog.Context{"cause": err.Error()})
		return
	}
	for _, m := range moves {
		c.TakeUpOwedMove(ctx, m.Workspace)
	}
}

// TakeUpOwedMove asks this daemon's bounce registry for the move a rename
// owes WS, when one is owed and not yet under way: the daemon that comes to
// serve a workspace runs its owed move, whichever daemon recorded it. A
// registration already standing (a move the handover carried) is joined, so
// the move runs once. A failure is recorded, and the move stays owed for the
// next daemon that serves the workspace.
func (c *controller) TakeUpOwedMove(ctx context.Context, ws ids.WorkspaceID) {
	fields := dlog.Context{"workspace": string(ws)}
	owed, err := c.deps.Moves.MoveOwed(ctx, ws)
	if err != nil {
		c.log.Error(opOwedMove, "could not read whether a rename owes the workspace a move; any owed move waits for the next daemon that serves it", dlog.WithCause(fields, err))
		return
	}
	if !owed {
		return
	}
	if _, err := c.BounceShim(context.WithoutCancel(ctx), ws, ReasonRename, false, nil, nil); err != nil {
		return
	}
	c.log.Info(opOwedMove, "took up the move a rename owes the workspace", fields)
}
