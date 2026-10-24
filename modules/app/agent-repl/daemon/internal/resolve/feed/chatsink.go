package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE CHAT SINK'S THREE EDGES (api.go, ChatSink). The feed is the one place
// that folds a turn's prose and names its concluded answer, so a zettel chat
// thread learns both from here rather than re-folding the session's frames.

// tellAnswerProse tells the chat sink a root-feed prose bubble grew on the
// live plane. A notice is not the agent's words and is never told.
func (r *resolver) tellAnswerProse(s *wsState, at placement, fold *proseState) {
	if r.deps.Chat == nil || s.plane != planeLive || !at.feed.Root || fold.notice || fold.turn == "" {
		return
	}
	r.deps.Chat.AnswerProse(s.id, ids.TurnID(fold.turn), fold.markdown)
}

// tellTurnEnded tells the chat sink how a turn ended: its answer, on any
// plane, when it concluded on one, then its end on the live plane.
func (r *resolver) tellTurnEnded(s *wsState, turn ids.TurnID, ended *frontendv1.FeedTurnEnded) {
	if r.deps.Chat == nil {
		return
	}
	if answer := ended.GetConcluded().GetAnswer().GetValue(); answer != "" {
		markdown, ok := s.answerMarkdown[answer]
		switch {
		case ok && markdown != "":
			r.deps.Chat.TurnAnswered(s.id, turn, markdown)
		default:
			r.logger(s.id).Error("daemon.feed.chat_answer_unread",
				"a concluded turn named an answer whose markdown was never recorded", dlog.Context{
					"turn": string(turn), "answer": answer, "recorded": ok,
				})
		}
	}
	if s.plane == planeLive {
		r.deps.Chat.TurnEnded(s.id, turn)
	}
}
