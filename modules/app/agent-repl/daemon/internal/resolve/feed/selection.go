package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// The feed resolver's read side for reply-to-a-past-response mode. The resolver
// is the one place that knows which rows wear the GREEN final-answer border —
// it draws them — so it owns the selectable set and the copy of each row's
// markdown. The SELECTION CURSOR itself (which of these is selected, and the
// push to the webapp) is the server's, per the plan's "the daemon owns the
// selection state": this package answers only what the set is and what each
// row said.

// recordFinalAnswer appends one concluded turn's answering row to the
// workspace's ordered selectable set and copies its settled markdown. It is
// called from the conclusion site under r.mu, append-once by FeedId value
// because a terminal replays across planes.
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
	if fold, ok := s.foldOfAnswer(id, unit); ok {
		s.answerMarkdown[value] = fold.markdown
	}
}

// foldOfAnswer is the fold that OWNS the answering row, which is NOT always
// the fold filed under the named unit. A response block delivered under two
// divergent activity ids leaves one row standing and retires the other fold,
// and the retired unit is ALIASED onto the survivor's row (response.go's
// aliasAnswerRow) so a terminal naming it still resolves. Reading the named
// unit's fold in that case would copy the RETIRED FRAGMENT's partial text as
// the final answer's markdown, and would find no row to re-stamp at all — so
// the row's own owner is looked up first, and the named unit's fold is the
// fallback for every ordinary answer, where the two are the same fold.
func (s *wsState) foldOfAnswer(id *frontendv1.FeedId, unit string) (*proseState, bool) {
	if value := id.GetValue(); value != "" {
		for _, fold := range s.responses {
			if fold.row.GetValue() == value {
				return fold, true
			}
		}
	}
	fold, ok := s.responses[unit]
	return fold, ok
}

// restampFinalAnswer re-publishes an already-drawn answer row with
// final_answer=true, and REPORTS WHETHER IT FOUND ONE TO STAMP. False is the
// whole of "the terminal named an answer no drawn response row resolves": the
// fold is gone, or it drew no row, or the row has left the feed. The caller
// raises the final-answer fault on it rather than concluding silently.
//
// restampFinalAnswer re-publishes an already-drawn answer row with
// final_answer=true. The response frames precede the turn's terminal both live
// and on history replay, so the answering row was drawn WITHOUT the flag by the
// time the terminal names it the answer. Rather than recompose the bubble (and
// risk drifting from drawResponse's arm/notice/usage handling), this re-pushes
// the row the fold already composed, reusing it verbatim and flipping only the
// data flag. It is the ONE site that makes the green appear at turn-end AND on
// a reloaded/replayed feed with no live turn-ended event: replay runs the same
// terminal path, so the recorded answer row is re-stamped there too. The write
// is idempotent — an already-stamped row upserts to an equal snapshot, which
// upsert drops as churn.
func (r *resolver) restampFinalAnswer(s *wsState, id *frontendv1.FeedId, unit string) bool {
	fold, ok := s.foldOfAnswer(id, unit)
	if !ok || fold.row == nil {
		return false
	}
	f := r.feed(s, fold.feed)
	existing, ok := f.rows[fold.row.GetValue()]
	if !ok {
		return false
	}
	resp := existing.GetActivity().GetResponse()
	if resp == nil {
		// A ROW THAT IS NOT A RESPONSE BUBBLE IS NOT AN ANSWER. Rule 1 asks for
		// a drawn, NON-THINKING response row, and this is the one place that can
		// tell: a thinking bubble folds through its own map and its row carries
		// no response arm.
		return false
	}
	if resp.GetFinalAnswer() {
		// ALREADY GREEN — the terminal replayed across planes. Landed, and the
		// upsert it would produce is churn.
		return true
	}
	return r.restateRow(s, placement{feed: fold.feed}, existing, true, unclonable{
		operation: "daemon.feed.final_answer_restamp_unclonable",
		message:   "the recorded answer row could not be cloned to stamp final_answer",
		context:   dlog.Context{"unit": unit, "row": fold.row.GetValue()},
	}, func(clone *frontendv1.FeedRow) {
		bubble := clone.GetActivity().GetResponse()
		bubble.FinalAnswer = true
		// THE FINAL ANSWER CARRIES THE TURN'S WHOLE FRESH INPUT. The bubble
		// landed with its own frozen delta; turning green re-stamps it ONCE with
		// the main agent's turn tally (usage.go), keeping its settled instant.
		if s.stampFinalAnswerTotal(fold) {
			bubble.Usage = &frontendv1.FeedResponseUsageStamp{Text: fold.usage, AtMs: bubble.GetUsage().GetAtMs()}
		}
	})
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
