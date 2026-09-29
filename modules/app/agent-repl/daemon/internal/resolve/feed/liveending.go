package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/ladder"
)

// THE LIVE TURN ENDING, as the desktop banner reads it. The feed is the one
// place that holds what a turn's end SAID — the green final answer's prose,
// the errored ending's headline — because it draws both. When a turn ends
// LIVE, the feed files that account under the turn; the prompt queue's turn
// end takes it (TakeTurnEnding) and hands it to the banner.
//
// ONLY THE LIVE PLANE FILES ONE. A history replay draws old endings through
// the same functions, and a banner for a turn that ended long ago is noise, so
// a replayed ending files nothing. The queue takes each ending once, which
// empties the entry; an ending nobody takes (a turn closed as orphaned by a
// boot, which raises no banner) stays until the workspace's state is dropped.

// TurnEnding is what a live turn's end said, for the desktop banner.
type TurnEnding struct {
	// Failure is the class of the failure the turn's terminal carried
	// (ladder.NoFailure for a success, or for a close no terminal followed).
	// With the close it is ladder.ResolveTurnEnd's input, so the banner reads
	// the same table the roster does.
	Failure ladder.FailureClass
	// Answer is the settled markdown of the green final-answer bubble the turn
	// concluded on. Empty when it concluded naming none.
	Answer string
	// Error is the errored ending's line: the drawn headline, then the
	// vendor's own sentence when one was recorded. Empty unless the ending is
	// errored.
	Error string
}

// fileLiveEnding records a turn's ending for the banner, when it was drawn on
// the live plane. ended is the row's ending exactly as drawn.
func (r *resolver) fileLiveEnding(s *wsState, turn ids.TurnID, ended *frontendv1.FeedTurnEnded, failure *conversationv1.AgentFailure) {
	if s.plane != planeLive {
		return
	}
	ending := TurnEnding{
		Failure: ladder.ClassifyFailure(failure),
		Error:   erroredLine(ended.GetErrored()),
	}
	if answer := ended.GetConcluded().GetAnswer().GetValue(); answer != "" {
		ending.Answer = s.answerMarkdown[answer]
	}
	s.liveEndings[turn] = ending
	r.logger(s.id).Debug("daemon.feed.live_ending_filed",
		"a live turn's ending was filed for its desktop banner", dlog.Context{
			"turn": string(turn), "outcome": terminalArm(ended), "failure_class": ending.Failure.String(),
			"answer_runes": len([]rune(ending.Answer)),
		})
}

// erroredLine composes an errored ending's one line: the headline the feed
// draws, then the vendor's sentence after a colon when there is one.
func erroredLine(errored *frontendv1.FeedTurnEndedErrored) string {
	if errored == nil {
		return ""
	}
	line := errored.GetHeadline().GetText()
	message := errored.GetMessage().GetText()
	switch {
	case message == "" || message == line:
		return line
	case line == "":
		return message
	default:
		return line + ": " + message
	}
}

// TakeTurnEnding answers the ending a live turn filed, and forgets it. False
// is a turn whose ending was not drawn live in this feed: a /clear whose
// divider stands in for its terminal, or a feed that never saw the turn.
func (r *resolver) TakeTurnEnding(ws ids.WorkspaceID, turn ids.TurnID) (TurnEnding, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return TurnEnding{}, false
	}
	ending, ok := s.liveEndings[turn]
	delete(s.liveEndings, turn)
	return ending, ok
}
