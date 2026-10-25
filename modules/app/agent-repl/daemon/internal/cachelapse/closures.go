package cachelapse

import (
	"context"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// ClosureSink is told of every close and reopen once it is recorded. The
// Keeper is the production one.
type ClosureSink interface {
	ClosureChanged(ws ids.WorkspaceID)
}

// ObserveClosures wraps a state client so every close (MarkClosed) and reopen
// (Reopen) it records is told to SINK, after the write. Nothing else about
// the client changes.
//
// EVERY CLOSE REACHES THE KEEPER, NOT ONLY THE ONES A CALLER REMEMBERS TO
// REPORT. A workspace closes from the user's close, an agent's command file, a
// landed merge, a kill and a boot, and each of them leaves the session as it
// finds it; wrapping the one state client they all write through makes a
// close that skips its last keep-alive round unrepresentable. A write that
// failed recorded nothing, so the sink is not told.
func ObserveClosures(db wsm.DB, sink ClosureSink) wsm.DB {
	return &closureObservedDB{DB: db, sink: sink}
}

type closureObservedDB struct {
	wsm.DB
	sink ClosureSink
}

// MarkClosed records the close, then tells the sink.
func (o *closureObservedDB) MarkClosed(ctx context.Context, id wsm.WorkspaceID, closure wsm.Closure) error {
	if err := o.DB.MarkClosed(ctx, id, closure); err != nil {
		return err
	}
	o.sink.ClosureChanged(id)
	return nil
}

// Reopen records the reopen, then tells the sink.
func (o *closureObservedDB) Reopen(ctx context.Context, id wsm.WorkspaceID, serving wsm.InstanceID) error {
	if err := o.DB.Reopen(ctx, id, serving); err != nil {
		return err
	}
	o.sink.ClosureChanged(id)
	return nil
}
