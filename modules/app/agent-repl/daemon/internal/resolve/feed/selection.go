package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
)

// The feed resolver's read side for reply-to-a-past-response mode. The resolver
// is the one place that knows which rows wear the GREEN final-answer border —
// it draws them — so it owns the selectable set and the copy of each row's
// markdown. The SELECTION CURSOR itself (which of these is selected, and the
// push to the webapp) is the server's, per the plan's "the daemon owns the
// selection state": this package answers only what the set is and what each
// row said.

// recordFinalAnswer appends one concluded turn's answering row to the workspace
//'s ordered selectable set and copies its settled markdown. It is called from
// the conclusion site under r.mu, append-once by FeedId value because a
// terminal replays across planes.
func (r *resolver) recordFinalAnswer(s *wsState, id *frontendv1.FeedId, unit string) {
	value := id.GetValue()
	if value == "" {
		return
	}
	if !s.finalAnswerSeen[value] {
		s.finalAnswerSeen[value] = true
		s.finalAnswers = append(s.finalAnswers, id)
	}
	// The markdown is (re)copied on every conclusion: a terminal that replays
	// after the fold settled carries the same text, and a fold that grew
	// between drawings is captured at its latest settled state, which is what
	// the user saw as the final answer.
	if fold, ok := s.responses[unit]; ok {
		s.answerMarkdown[value] = fold.markdown
	}
}

// FinalResponses answers the workspace's ordered selectable final-response
// rows — the rows drawn with the green final-answer border, oldest first, so
// the last element is the most recent. It returns a copy so a caller may hold
// it without racing later conclusions. An empty slice is a workspace with no
// final responses yet, never an error.
func (r *resolver) FinalResponses(ws ids.WorkspaceID) []*frontendv1.FeedId {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return nil
	}
	out := make([]*frontendv1.FeedId, len(s.finalAnswers))
	copy(out, s.finalAnswers)
	return out
}

// ResponseMarkdown answers the settled markdown of one selectable final
// response, and whether the daemon deems the feedid selectable at all. A miss
// (false) is a feedid that names no final-response row of this workspace — the
// caller's to refuse, never to paper over with an empty prefix.
func (r *resolver) ResponseMarkdown(ws ids.WorkspaceID, id *frontendv1.FeedId) (string, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return "", false
	}
	md, ok := s.answerMarkdown[id.GetValue()]
	return md, ok
}
