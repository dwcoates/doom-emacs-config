package feed

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// DURABLE TURN SETS. The feed refuses some turns' rows for good (a rollback
// cut them, a sign-in answered their failure), and each such set is recorded
// in the state database so a later process refuses them too. Every set is
// loaded the same way, by restoreTurnSet, when a process first builds a
// workspace's feed state and before any row is drawn.

// turnSetRestore is how one durable turn set is loaded and told.
type turnSetRestore struct {
	// op is the operation the load's records carry, and the warning's key.
	op string
	// load reads the workspace's set from the store.
	load func(context.Context, ids.WorkspaceID) ([]ids.TurnID, error)
	// failed is the ERROR record's message for a load that failed.
	failed string
	// warning is the topbar's line for a load that failed.
	warning string
	// loaded is the INFO record's message for a set that held turns.
	loaded string
}

// restoreTurnSet loads RESTORE's set into INTO. A load that fails is logged at
// ERROR and raised on the topbar, and leaves INTO as it was: the set's rows
// may be drawn again, which the warning says.
func (r *resolver) restoreTurnSet(s *wsState, restore turnSetRestore, into map[ids.TurnID]bool) {
	turns, err := restore.load(context.Background(), s.id)
	if err != nil {
		r.logger(s.id).Error(restore.op, restore.failed, dlog.Context{"error": err.Error()})
		r.raiseWarning(s, restore.op, restore.warning)
		return
	}
	for _, turn := range turns {
		into[turn] = true
	}
	if len(turns) > 0 {
		r.logger(s.id).Info(restore.op, restore.loaded, dlog.Context{"turns": len(turns)})
	}
}
