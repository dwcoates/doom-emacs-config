package feed

import (
	"context"

	"agentrepl/logging/logsite"
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
	// read is the workspace's set as prime read it from the store.
	read primedTurnSet
	// failed is the ERROR record's message for a load that failed.
	failed logsite.Message
	// loaded is the INFO record's message for a set that held turns.
	loaded logsite.Message
}

// restoreTurnSet installs RESTORE's set, read by prime, into INTO. A load that fails is logged at
// ERROR, and leaves INTO as it was: the set's rows may be drawn again, which
// the record says.
func (r *resolver) restoreTurnSet(s *wsState, restore turnSetRestore, into map[ids.TurnID]bool) {
	turns, err := restore.read.turns, restore.read.err
	if err != nil {
		r.logger(s.id).Log(dlog.LevelError, restore.op, restore.failed, dlog.Context{"error": err.Error()})
		return
	}
	for _, turn := range turns {
		into[turn] = true
	}
	if len(turns) > 0 {
		r.logger(s.id).Log(dlog.LevelInfo, restore.op, restore.loaded, dlog.Context{"turns": len(turns)})
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
	// retired, when set, is told of every row the answer retired, so it can
	// record why that row left the feed.
	retired func(s *wsState, addr feedid.Feed, id string, row *frontendv1.FeedRow)
	// into is the set the upsert door refuses the answered turns' rows by.
	into map[ids.TurnID]bool
	// record writes the answered turns durably; nil records nothing.
	record func(context.Context, ids.WorkspaceID, []ids.TurnID) error
	// fields is the context every record of the answer carries.
	fields dlog.Context
	// nothing is the DEBUG record's message when no drawn row named a turn.
	nothing logsite.Message
	// failed is the ERROR record's message for a recording that failed.
	failed logsite.Message
	// answered is the INFO record's message for an answer recorded.
	answered logsite.Message
}

// answerTurns answers every turn a drawn row names: its rows the answer
// retires leave every feed and are refused from now on, and the turns are
// recorded so a later process refuses them too. The removal always happens;
// a recording that fails is logged at ERROR, raised on the topbar, and
// returned, since a daemon restart would draw those rows again.
func (r *resolver) answerTurns(s *wsState, answer turnAnswer) *storeOutcome {
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
		log.Log(dlog.LevelDebug, answer.op, answer.nothing, fields)
		return &storeOutcome{}
	}
	for _, turn := range turns {
		answer.into[turn] = true
	}
	retired := 0
	r.eachStoredRow(s, func(addr feedid.Feed, _ *feedState, id string, row *frontendv1.FeedRow) {
		if answer.into[ids.TurnID(row.GetTurn().GetValue())] && answer.retires(row) && r.retire(s, addr, id) {
			retired++
			if answer.retired != nil {
				answer.retired(s, addr, id, row)
			}
		}
	})
	fields["turns"], fields["first"], fields["retired"] = len(turns), firstTurn(turns), retired
	answered := func() { log.Log(dlog.LevelInfo, answer.op, answer.answered, fields) }
	if answer.record == nil {
		answered()
		return &storeOutcome{}
	}
	// THE ANSWER IS RECORDED OFF THE LOCK (storework.go); its outcome is
	// known once the caller's hold is released.
	ws := s.id
	return r.writeAfterUnlock(func(ctx context.Context) error { return answer.record(ctx, ws, turns) },
		func(err error) {
			fields["error"] = err.Error()
			log.Log(dlog.LevelError, answer.op, answer.failed, fields)
		}, answered)
}
