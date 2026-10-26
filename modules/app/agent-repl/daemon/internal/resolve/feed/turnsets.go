package feed

import (
	"context"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
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

// turnAnswer is how one act answers turns' failures for good: which drawn rows
// name the turns it answers, which rows of an answered turn leave the feed,
// the set the upsert door refuses them by, and how the answer is recorded.
type turnAnswer struct {
	// op is the operation the answer's records carry, and the warning's key.
	op string
	// names reports whether a drawn row names a turn the act answers.
	names func(row *frontendv1.FeedRow) bool
	// retires reports whether a row of an answered turn leaves the feed.
	retires func(row *frontendv1.FeedRow) bool
	// into is the set the upsert door refuses the answered turns' rows by.
	into map[ids.TurnID]bool
	// record writes the answered turns durably; nil records nothing.
	record func(context.Context, ids.WorkspaceID, []ids.TurnID) error
	// fields is the context every record of the answer carries.
	fields dlog.Context
	// nothing is the DEBUG record's message when no drawn row named a turn.
	nothing string
	// failed is the ERROR record's message for a recording that failed.
	failed string
	// warning prefixes the topbar's line for a recording that failed.
	warning string
	// answered is the INFO record's message for an answer recorded.
	answered string
}

// answerTurns answers every turn a drawn row names: its rows the answer
// retires leave every feed and are refused from now on, and the turns are
// recorded so a later process refuses them too. The removal always happens;
// a recording that fails is logged at ERROR, raised on the topbar, and
// returned, since a daemon restart would draw those rows again.
func (r *resolver) answerTurns(s *wsState, answer turnAnswer) error {
	log := r.logger(s.id)
	var turns []ids.TurnID
	r.eachStoredRow(s, func(_ feedid.Feed, _ *feedState, _ string, row *frontendv1.FeedRow) {
		if turn := row.GetTurn().GetValue(); answer.names(row) && turn != "" {
			turns = append(turns, ids.TurnID(turn))
		}
	})
	fields := dlog.Context{}
	for k, v := range answer.fields {
		fields[k] = v
	}
	if len(turns) == 0 {
		log.Debug(answer.op, answer.nothing, fields)
		return nil
	}
	for _, turn := range turns {
		answer.into[turn] = true
	}
	retired := 0
	r.eachStoredRow(s, func(addr feedid.Feed, _ *feedState, id string, row *frontendv1.FeedRow) {
		if answer.into[ids.TurnID(row.GetTurn().GetValue())] && answer.retires(row) && r.retire(s, addr, id) {
			retired++
		}
	})
	fields["turns"], fields["first"], fields["retired"] = len(turns), firstTurn(turns), retired
	if answer.record != nil {
		if err := answer.record(context.Background(), s.id, turns); err != nil {
			fields["error"] = err.Error()
			log.Error(answer.op, answer.failed, fields)
			r.raiseWarning(s, answer.op, fmt.Sprintf("%s: %v", answer.warning, err))
			return err
		}
	}
	log.Info(answer.op, answer.answered, fields)
	return nil
}
