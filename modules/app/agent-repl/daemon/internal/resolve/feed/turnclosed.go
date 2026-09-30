package feed

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE DOOR'S FEED HALF. A turn's durable row closes only through the prompt
// queue's door (promptqueue/turnclose.go), and the door tells the feed here
// with the close it recorded. So every turn that ends has its ending row,
// whatever ended it: a vendor terminal has already drawn one (and this is a
// no-op), and a close no terminal will ever follow — the agent process dying,
// an adoption or a boot finding a turn nobody saw end, a displaced turn whose
// kill never answered — draws its ending from the close itself.
//
// A REPLAY DRAWS THE SAME ENDING from the same record: a replayed turn whose
// durable row is closed but whose page carries no terminal for it is ended
// from its recorded close (replayCloses), so an ended turn never replays as
// running, old data included.

// OnTurnClosed draws the ending of a turn the door just closed, unless the
// feed already drew one.
func (r *resolver) OnTurnClosed(ws ids.WorkspaceID, turn ids.TurnID, close wsm.RecordedClose) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	// A FOLDED PROMPT'S TURN NEVER RAN: its bubble was drawn under the turn it
	// joined, whose own terminal settles it, so there is no ending to draw.
	if close.How == wsm.CloseFolded {
		r.logger(ws).Debug("daemon.feed.turn_closed_folded",
			"a prompt folded into another turn closed; the turn it joined carries its ending",
			dlog.Context{"turn": string(turn)})
		return
	}
	if !s.knownTurns[turn] {
		// The feed has not seen this turn opened (a boot closes turns before
		// any watch replays them). Its prompt, when it is replayed, is ended
		// from this same recorded close.
		r.logger(ws).Debug("daemon.feed.turn_closed_unseen",
			"a turn closed that this feed never saw opened; its replay draws the ending from the recorded close",
			dlog.Context{"turn": string(turn), "close": close.How.String()})
		return
	}
	r.endClosedTurn(s, turn, close)
}

// endClosedTurn draws a closed turn's ending from its recorded close, unless
// the turn already ended in this feed (its own terminal drew one). EXACTLY ONE
// ROW: the row is keyed by the turn, and a turn this feed has ended is never
// drawn a second ending.
func (r *resolver) endClosedTurn(s *wsState, turn ids.TurnID, close wsm.RecordedClose) {
	log := r.logger(s.id)
	if s.endedTurns[turn] {
		log.Debug("daemon.feed.turn_closed_already_ended",
			"a turn's close found its ending already drawn",
			dlog.Context{"turn": string(turn), "close": close.How.String()})
		return
	}
	at := r.outputPlacement(s)
	ended := r.closedEnding(s, turn, close)

	r.disarmTurnStalls(s, string(turn))
	r.clearTurnStalledAnswerFault(s, string(turn), "the turn closed")
	r.breakPlanEpisodes(s, "the turn closed while plan mode was still open")

	row := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindTurnEnded, ID: string(turn)}),
		Turn: &conversationv1.TurnId{Value: string(turn)},
		Row:  &frontendv1.FeedRow_TurnEnded{TurnEnded: ended},
	}
	log.Info("daemon.feed.turn_closed",
		"a turn closed with no terminal of its own; its ending row was drawn from its recorded close",
		dlog.Context{"turn": string(turn), "close": close.How.String(), "outcome": terminalArm(ended), "plane": s.plane.String()})
	r.upsert(s, at, row, true)
	r.fileLiveEnding(s, turn, ended, nil)
	r.settleTurnPrompts(s, turn)

	delete(s.turnEvidence, string(turn))
	delete(s.turnRefusals, string(turn))
	if s.turnInFlight != nil && *s.turnInFlight == turn {
		s.turnInFlight = nil
	}
	if s.turnStamp != nil && *s.turnStamp == turn {
		s.turnStamp = nil
	}
}

// closedEnding composes the ending a recorded close draws. The two ordinary
// closes draw what their terminal would have (a conclusion, a stop); every
// other close is the turn ending abnormally and says so in plain words.
func (r *resolver) closedEnding(s *wsState, turn ids.TurnID, close wsm.RecordedClose) *frontendv1.FeedTurnEnded {
	ended := &frontendv1.FeedTurnEnded{EndedAtMs: close.At.UnixMilli()}
	errored := func(arm func(*frontendv1.FeedTurnEndedErrored), sentence string) {
		e := &frontendv1.FeedTurnEndedErrored{}
		arm(e)
		applyHeadline(e, headline{Text: sentence}, "")
		ended.Outcome = &frontendv1.FeedTurnEnded_Errored{Errored: e}
	}
	failed := func(reason string) func(*frontendv1.FeedTurnEndedErrored) {
		return func(e *frontendv1.FeedTurnEndedErrored) { e.Error = turnFailedArm(reason) }
	}
	switch close.How {
	case wsm.CloseCompleted:
		concludedArm(&frontendv1.FeedTurnEndedConcluded{})(ended)
	case wsm.CloseKilled:
		// NO `by_user` REACHES THIS PATH. It draws only a turn whose terminal
		// never reached this feed (a terminal that did reach it drew the
		// ending first, and this close is then a no-op), and a recorded close
		// carries how and when, never the cause. So the command stays unset,
		// through the same setter the terminal path uses.
		interruptedArm(nil)(ended)
	case wsm.CloseAgentDied:
		errored(func(e *frontendv1.FeedTurnEndedErrored) {
			e.Error = &frontendv1.FeedTurnEndedErrored_AgentProcessDied{
				AgentProcessDied: &frontendv1.FeedTurnErrorAgentProcessDied{},
			}
		}, "the agent process died, and the turn it was running ended with it")
	case wsm.CloseOrphaned:
		errored(failed("closed:orphaned"), "the turn was dropped: nothing saw it end")
	case wsm.CloseFailed:
		errored(failed("closed:failed"), "the turn failed with an error, and no account of the failure was recorded")
	default:
		r.logger(s.id).Error("daemon.feed.turn_closed_undeclared",
			"a turn's recorded close is not one this feed can draw; it is drawn as an unexplained end",
			dlog.Context{"turn": string(turn), "close": int(close.How)})
		errored(failed(fmt.Sprintf("closed:%d", int(close.How))), "the turn ended in a way this daemon does not know how to describe")
	}
	return ended
}

// recordedCloses reads the durable close of every turn a history page opens,
// BEFORE the resolver's lock is taken (no database read belongs inside it).
//
// A FAILED READ IS RECORDED, NEVER SWALLOWED, and the page still draws: its
// turns' own terminals are drawn either way, and refusing the page because the
// fallback could not be read would hide the whole conversation.
func (r *resolver) recordedCloses(ws ids.WorkspaceID, page *conversationv1.HistoryPage) map[ids.TurnID]wsm.RecordedClose {
	if r.deps.TurnCloses == nil {
		return nil
	}
	var turns []ids.TurnID
	for _, at := range page.GetEntries() {
		if turn := at.GetEntry().GetUserPrompt().GetId().GetValue(); turn != "" {
			turns = append(turns, ids.TurnID(turn))
		}
	}
	if len(turns) == 0 {
		return nil
	}
	closes, err := r.deps.TurnCloses(context.Background(), ws, turns)
	if err != nil {
		r.logger(ws).Error("daemon.feed.turn_closes_unreadable",
			"the replayed turns' recorded closes could not be read; a turn with no terminal on the page replays as running",
			dlog.Context{"turns": len(turns), "cause": err.Error()})
		return nil
	}
	return closes
}

// endReplayedTurn ends the turn the replay stood in, when its page carried no
// terminal for it and its durable row is closed. It is called where the
// replay LEAVES that turn — as the next turn's prompt is drawn, and at the
// page's end — so the ending lands under the turn it ends. NEXT is the turn
// the replay moves into ("" at the page's end), which is never ended here.
func (r *resolver) endReplayedTurn(s *wsState, next ids.TurnID) {
	if s.replayTurn == nil || *s.replayTurn == next {
		return
	}
	close, closed := s.replayCloses[*s.replayTurn]
	if !closed {
		return
	}
	r.endClosedTurn(s, *s.replayTurn, close)
}
