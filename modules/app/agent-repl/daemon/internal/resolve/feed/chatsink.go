package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE ANSWER SINK'S EDGES (api.go, AnswerSink). The feed is the one place
// that folds a turn's prose, sees a prompt folded into a turn, and names a
// turn's concluded answer, so a zettel chat thread and a workspace gather
// learn them here rather than re-folding the session's frames.

// tellAnswerProse tells the answer sink a root-feed prose bubble grew on the
// live plane. A notice is not the agent's words and is never told.
func (r *resolver) tellAnswerProse(s *wsState, at placement, fold *proseState) {
	if r.deps.Answers == nil || s.plane != planeLive || !at.feed.Root || fold.notice || fold.turn == "" {
		return
	}
	r.deps.Answers.AnswerProse(s.id, ids.TurnID(fold.turn), fold.markdown)
}

// tellTurnEnded tells the answer sink how a turn ended: its answer, on the
// workspace's own planes, when it concluded on one, then its end on the live
// plane. An INHERITED turn is a fork's ancestor's (its record is the
// ancestor's), so the sink is told nothing of it.
func (r *resolver) tellTurnEnded(s *wsState, turn ids.TurnID, ended *frontendv1.FeedTurnEnded) {
	if r.deps.Answers == nil || s.plane == planeInherited {
		return
	}
	if answer := ended.GetConcluded().GetAnswer().GetValue(); answer != "" {
		markdown, ok := s.answerMarkdown[answer]
		switch {
		case ok && markdown != "":
			r.deps.Answers.TurnAnswered(s.id, turn, markdown)
		default:
			r.logger(s.id).Error("daemon.feed.chat_answer_unread",
				"a concluded turn named an answer whose markdown was never recorded", dlog.Context{
					"turn": string(turn), "answer": answer, "recorded": ok,
				})
		}
	}
	if s.plane == planeLive {
		r.deps.Answers.TurnEnded(s.id, turn)
	}
}

// tellPromptFolded tells the answer sink a prompt minted as TURN was folded
// into INTO. An INHERITED fold ran in a fork's ancestor, so it is not told.
func (r *resolver) tellPromptFolded(s *wsState, turn, into ids.TurnID) {
	if r.deps.Answers == nil || s.plane == planeInherited {
		return
	}
	r.deps.Answers.PromptFolded(s.id, turn, into)
}
