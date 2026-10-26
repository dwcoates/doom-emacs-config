package feed

import (
	"context"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// A SIGN-IN ANSWERS AN ACCOUNT FAILURE. A turn that ended on an account cause
// signing in again cures (ladder.FailureCuredBySignIn) draws two rows of its
// failure: the broken response bubble the failure cut short, which offers the
// sign-in action inside it (frontend.v1.FeedResponseError.sign_in), and the
// turn's errored ending, whose marker offers it too. When a sign-in made
// through agent-repl completes for the workspace's account root (OnSignedIn),
// both rows leave the feed: the failure has been answered, and a "sign in"
// still on screen would ask for what was just done.
//
// THE REFUSAL LIVES AT THE ONE UPSERT DOOR (signedInRow), as a rolled-back
// turn's does, so a live re-delivery from the other store plane, a history page
// replayed after a restart, and a page loaded on scroll-back all stay retired.
// The answered turns are recorded durably and loaded when a process first
// builds the workspace's feed state.

// SignedInTurnStore is the durable record of turns whose account failure a
// sign-in answered (wsm.DB).
type SignedInTurnStore interface {
	RecordSignedInTurns(ctx context.Context, id ids.WorkspaceID, turns []ids.TurnID) error
	SignedInTurns(ctx context.Context, id ids.WorkspaceID) ([]ids.TurnID, error)
}

const (
	opSignedIn        = "daemon.feed.signed_in"
	opSignedInRestore = "daemon.feed.signed_in_restore"
)

// isFailureRow reports whether ROW is one of the two rows an account failure
// draws for its turn: a broken response bubble, or the errored ending.
func isFailureRow(row *frontendv1.FeedRow) bool {
	return row.GetActivity().GetResponse().GetError() != nil || row.GetTurnEnded().GetErrored() != nil
}

// offersSignIn reports whether ROW is a turn's errored ending whose marker
// offers the sign-in action: the ending of a turn a sign-in answers.
func offersSignIn(row *frontendv1.FeedRow) bool {
	return row.GetTurnEnded().GetErrored().GetMarker().GetVendorFault().GetExpansion().GetSignIn() != nil
}

// noteSignInEnding records that TURN ended on an account cause a sign-in
// cures, and puts the sign-in action inside every broken bubble of the turn
// already drawn. A bubble the turn draws later carries it from drawResponse.
func (r *resolver) noteSignInEnding(s *wsState, turn ids.TurnID) {
	s.signInTurns[turn] = true
	r.eachStoredRow(s, func(addr feedid.Feed, f *feedState, id string, row *frontendv1.FeedRow) {
		broken := row.GetActivity().GetResponse().GetError()
		if broken == nil || row.GetTurn().GetValue() != string(turn) || broken.GetSignIn() != nil {
			return
		}
		why := unclonable{
			operation: opSignedIn,
			message:   "a broken bubble could not be copied to carry the sign-in action",
			context:   dlog.Context{"turn": string(turn), "row": id},
		}
		if r.restateRow(s, placement{feed: addr}, row, !f.nonDurable[id], why, func(stamped *frontendv1.FeedRow) {
			stamped.GetActivity().GetResponse().GetError().SignIn = &frontendv1.FeedOutcomeSignIn{}
		}) {
			r.logger(s.id).Info(opSignedIn, "the turn ended on an account cause; its broken bubble now offers the sign-in action",
				dlog.Context{"turn": string(turn), "row": id})
		}
	})
}

// signedInRow reports whether ROW is a failure row of a turn a sign-in
// answered, which the upsert door refuses to draw.
func (r *resolver) signedInRow(s *wsState, f *feedState, row *frontendv1.FeedRow) bool {
	turn := row.GetTurn().GetValue()
	if turn == "" || !s.signedIn[ids.TurnID(turn)] || !isFailureRow(row) {
		return false
	}
	r.logger(s.id).Debug("daemon.feed.signed_in_row_refused",
		"a failure row of a turn a sign-in answered was not drawn",
		dlog.Context{"feed": f.key, "row": row.GetId().GetValue(), "turn": turn})
	return true
}

// OnSignedIn takes a completed sign-in for the workspace's account root: every
// turn whose drawn ending offers the sign-in action is answered, its failure
// rows retired from every feed and refused from now on, and the turns recorded
// so a later process refuses them too. The removal always happens; an error
// says only that recording failed (logged at ERROR and raised on the topbar
// here), so a daemon restart would draw those failures again.
func (r *resolver) OnSignedIn(ws ids.WorkspaceID) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws)
	var turns []ids.TurnID
	r.eachStoredRow(s, func(_ feedid.Feed, _ *feedState, _ string, row *frontendv1.FeedRow) {
		if offersSignIn(row) && row.GetTurn().GetValue() != "" {
			turns = append(turns, ids.TurnID(row.GetTurn().GetValue()))
		}
	})
	if len(turns) == 0 {
		log.Debug(opSignedIn, "a sign-in completed and no drawn turn ending offered it; nothing was retired", nil)
		return nil
	}
	for _, turn := range turns {
		s.signedIn[turn] = true
	}
	retired := 0
	r.eachStoredRow(s, func(addr feedid.Feed, _ *feedState, id string, row *frontendv1.FeedRow) {
		if s.signedIn[ids.TurnID(row.GetTurn().GetValue())] && isFailureRow(row) && r.retire(s, addr, id) {
			retired++
		}
	})
	fields := dlog.Context{"turns": len(turns), "first": firstTurn(turns), "retired": retired}
	if r.deps.SignedIn != nil {
		if err := r.deps.SignedIn.RecordSignedInTurns(context.Background(), ws, turns); err != nil {
			fields["error"] = err.Error()
			log.Error(opSignedIn,
				"a sign-in's answered failures were removed from the feed but could not be recorded; a daemon restart will draw them again", fields)
			r.raiseWarning(s, opSignedIn,
				fmt.Sprintf("the answered sign-in failures could not be saved and will reappear after a daemon restart: %v", err))
			return err
		}
	}
	log.Info(opSignedIn, "a sign-in answered the account failures drawn in the feed; their rows were retired and recorded", fields)
	return nil
}

// restoreSignedIn loads the workspace's answered turns when a process first
// builds its feed state, before any row is drawn.
func (r *resolver) restoreSignedIn(s *wsState) {
	if r.deps.SignedIn == nil {
		return
	}
	r.restoreTurnSet(s, turnSetRestore{
		op:      opSignedInRestore,
		load:    r.deps.SignedIn.SignedInTurns,
		failed:  "the workspace's sign-in-answered turns could not be loaded; their failures may be drawn again",
		warning: "answered sign-in failures could not be loaded and may be drawn again",
		loaded:  "the workspace's sign-in-answered turns were loaded; their failures are not drawn",
	}, s.signedIn)
}
