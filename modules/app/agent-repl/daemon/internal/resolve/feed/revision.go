package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prose"
	"agent-repld/internal/wsm"
)

// A FINAL RESPONSE REVISED FROM HIGHLIGHTED WORDS. The user highlighted words
// in a final response and asked about them; the shim rewrote the response on
// a side query and wrote the answer as the response_revision page line
// (conversation.v1.AgentResponseRevision). The feed draws no row for that
// line: it redraws the response's own bubble with the revised prose.
//
// THE REVISION IS FILED BY ITS RESPONSE UNIT, NOT APPLIED ONCE. Every draw of
// the response consults it (drawResponse), so the outcome is the same whatever
// order the two arrive in: a replay that loads the revision's page before the
// response's, a file-plane re-delivery of the response's original settle after
// the revision, and a live revision of a bubble already drawn all end with the
// revised prose standing.

// RevisableResponse is a final response a revision can rewrite: its unit and
// the markdown it says now.
type RevisableResponse struct {
	// Unit is the response block's activity id, which the revision names.
	Unit string
	// Markdown is what the response says now, a previous revision included.
	Markdown string
}

// RevisableFinalResponse answers the final response ROW names on the
// workspace's root feed, and false for a row that is no settled final
// response (a prompt, an interim, a notice, a bubble still arriving, a row
// that has left the feed).
func (r *resolver) RevisableFinalResponse(ws ids.WorkspaceID, row *frontendv1.FeedId) (RevisableResponse, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok || !s.finalAnswerSeen[row.GetValue()] {
		return RevisableResponse{}, false
	}
	unit, fold, ok := s.foldOfRow(row)
	if !ok || !fold.settled || fold.notice {
		return RevisableResponse{}, false
	}
	return RevisableResponse{Unit: unit, Markdown: fold.markdown}, true
}

// SetResponseRevision states where a revision of ROW's response stands
// (pending, failed, or nil once it is neither) and restates the row with it.
// False is a row that is no drawn response of this workspace.
func (r *resolver) SetResponseRevision(ws ids.WorkspaceID, row *frontendv1.FeedId, state *frontendv1.FeedResponseRevision) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return false
	}
	unit, fold, ok := s.foldOfRow(row)
	if !ok {
		return false
	}
	if state == nil {
		delete(s.revisionStates, unit)
	} else {
		s.revisionStates[unit] = state
	}
	r.logger(ws).Info("daemon.feed.response_revision_state", "a response's revision changed state; its bubble is restated", dlog.Context{
		"unit": unit, "row": row.GetValue(), "state": revisionStateName(state),
	})
	return r.restateRevised(s, fold, unit, nil)
}

// OnResponseRevision is the response_revision page line, live: the revised
// prose is typed out over the bubble at the response's reveal pace.
func (r *resolver) OnResponseRevision(ws ids.WorkspaceID, agent *conversationv1.AgentId, revision *conversationv1.AgentResponseRevision) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.applyRevision(r.state(ws), agent, revision, true)
}

// applyRevision files REVISION under its response unit, ends the unit's
// pending revision, and restates the response's bubble when it is drawn. LIVE
// types the change out; a replay draws it whole.
func (r *resolver) applyRevision(s *wsState, agent *conversationv1.AgentId, revision *conversationv1.AgentResponseRevision, live bool) {
	unit := revision.GetResponse().GetValue()
	markdown := revision.GetProse().GetMarkdown()
	log := r.logger(s.id)
	if unit == "" || markdown == "" {
		log.Error("daemon.feed.response_revision_malformed", "a response revision named no response or carried no prose; it was not applied", dlog.Context{
			"unit": unit, "characters": len(markdown),
		})
		return
	}
	s.revisions[unit] = markdown
	delete(s.revisionStates, unit)
	fold, ok := s.responses[unit]
	if !ok || fold.row == nil {
		// THE RESPONSE IS NOT DRAWN YET (its page is older than the one that
		// carried the revision): its own draw consults the filed revision.
		log.Debug("daemon.feed.response_revision_filed", "a response revision arrived before its response was drawn; the response's draw applies it", dlog.Context{
			"unit": unit,
		})
		return
	}
	fold.markdown = markdown
	if value := fold.row.GetValue(); s.finalAnswerSeen[value] {
		s.answerMarkdown[value] = markdown
	}
	var pace *frontendv1.FeedResponseRevealPace
	if live {
		pace = r.revealPace(r.revealSourceOf(s, agent, wsm.RevealKindProse))
	}
	log.Info("daemon.feed.response_revised", "a response was revised; its bubble is restated with the revised prose", dlog.Context{
		"unit": unit, "row": fold.row.GetValue(), "live": live, "characters": len(markdown),
	})
	r.restateRevised(s, fold, unit, &revisedProse{markdown: markdown, pace: pace})
}

// revisedProse is the prose a restatement replaces the bubble's with.
type revisedProse struct {
	markdown string
	// pace types the change out; nil draws it whole.
	pace *frontendv1.FeedResponseRevealPace
}

// restateRevised re-pushes FOLD's row with the unit's revision state and, when
// PROSE is set, the revised prose. It reports whether the row was re-pushed.
func (r *resolver) restateRevised(s *wsState, fold *proseState, unit string, revised *revisedProse) bool {
	f := r.feed(s, fold.feed)
	existing, ok := f.rows[fold.row.GetValue()]
	if !ok || existing.GetActivity().GetResponse() == nil {
		return false
	}
	return r.restateRow(s, placement{feed: fold.feed}, existing, true, unclonable{
		operation: "daemon.feed.response_revision_unclonable",
		message:   "the revised response's row could not be cloned to restate it",
		context:   dlog.Context{"unit": unit, "row": fold.row.GetValue()},
	}, func(clone *frontendv1.FeedRow) {
		bubble := clone.GetActivity().GetResponse()
		bubble.Revision = s.revisionStates[unit]
		if revised == nil {
			return
		}
		bubble.Result = &frontendv1.FeedResponse_Success{Success: &frontendv1.FeedResponseSuccess{
			Prose:      &frontendv1.FeedResponseProse{Prose: r.parsedProse(s, revised.markdown, prose.Settled)},
			RevealPace: revised.pace,
		}}
	})
}

// foldOfRow is the response unit and fold whose row ROW names. It is THE ONE
// row-to-fold lookup (foldOfAnswer reads through it), so a fold retired from
// its row and a fold that drew none are missed the same way everywhere.
func (s *wsState) foldOfRow(row *frontendv1.FeedId) (string, *proseState, bool) {
	value := row.GetValue()
	if value == "" {
		return "", nil, false
	}
	for unit, fold := range s.responses {
		if fold.row.GetValue() == value {
			return unit, fold, true
		}
	}
	return "", nil, false
}

// revisionStateName names a revision state for the records.
func revisionStateName(state *frontendv1.FeedResponseRevision) string {
	switch state.GetState().(type) {
	case *frontendv1.FeedResponseRevision_Pending:
		return "pending"
	case *frontendv1.FeedResponseRevision_Failed:
		return "failed"
	default:
		return "none"
	}
}
