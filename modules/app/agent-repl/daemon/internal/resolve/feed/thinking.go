package feed

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/wsm"
)

// THINKING IS THE AGENT'S INTERMEDIATE REASONING drawn as its own bubble, one
// per reasoning block, in feed order (a block's reasoning arrives before the
// prose it precedes). It reuses the response bubble's whole machinery — the
// FeedResponse row, the fragment fold, the markdown reveal — and rides it on
// the `thinking` flag so the client draws it purple and non-bordered and its
// green-final-answer rule can never match it.
//
// A THINKING BUBBLE IS NEVER THE TURN'S ANSWER. Unlike drawResponse, this never
// files the row in s.answerRows, so the turn's conclusion (turnended.go, which
// only walks answerRows) can never name a thinking row as the answer — the
// structural half of "thinking is never green". The `thinking` flag is the
// second, client-side half.
//
// THE PROSE FOLD, exactly as response.go's: the shim forwards each fragment as
// the vendor emits it and accumulates nothing; the daemon accumulates and
// re-pushes the whole row; the terminal frame restates the whole, so a missed
// fragment self-corrects and a settled bubble is always right.

// drawThinking folds one reasoning block into its bubble.
func (r *resolver) drawThinking(s *wsState, at placement, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, thinking *conversationv1.AgentThinking) (*frontendv1.FeedRow, error) {
	unit := act.GetActivityId().GetValue()
	fold := s.thinkingProse(unit)
	log := r.logger(s.id)
	// hadContentBefore records whether an earlier call already emitted this
	// unit's thinking row (the fold's markdown became non-empty then and is
	// never cleared back to empty), so the emit log below can tell the row's
	// first appearance in the feed from a later update to it.
	hadContentBefore := fold.markdown != ""

	// NO USAGE STAMP ON THE THINKING BUBBLE. Reasoning's cost is the footer's
	// (and the thinking block is frequently the sibling unit that carries the
	// PROSE bubble's usage — see response.go's "THE STAMP IS THE API RESPONSE'S
	// FIGURES"), so drawing a corner here would double the same figure. The
	// sink still files this unit's usage for the prose bubble; this handler
	// simply never presents it.
	bubble := &frontendv1.FeedResponse{Thinking: true}
	pace, paced := r.paceKey(s, agent, wsm.RevealKindThinking)

	switch state := thinking.GetResult().(type) {
	case *conversationv1.AgentThinking_Start:
		// THE ROW IS DEFERRED UNTIL THE BLOCK HAS CONTENT. A block opens on
		// content_block_start before its shown-vs-withheld fate is known, so
		// drawing here would open an empty bubble that a withheld block would
		// then have to retire — the empty card that FLASHED in the feed. Instead
		// the Start arm only initializes the fold and draws NOTHING; the first
		// content-bearing Update (or a Success carrying the whole) is what first
		// emits the row. A withheld block therefore draws nothing at all, with
		// no retire required.
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawThinking", "branch": "case *conversationv1.AgentThinking_Start"})
		fold.markdown = ""
		fold.settled = false
		fold.lastFragmentAt = time.Time{}
		log.Debug("daemon.feed.thinking_deferred",
			"a reasoning block opened; its bubble is deferred until it carries content",
			dlog.Context{"unit": unit})
		return nil, errNotARow
	case *conversationv1.AgentThinking_Update:
		if fold.settled {
			// A fragment after the terminal cannot re-open a closed bubble; the
			// settled whole is authoritative. ORDINARY across two store planes,
			// recorded at DEBUG, exactly as the prose fold treats it.
			log.Debug("daemon.feed.thinking_fragment_after_settle",
				"a reasoning fragment arrived after the block settled and was not folded in; the settled whole stands",
				dlog.Context{"unit": unit})
			return nil, errNotARow
		}
		text, ok := state.Update.GetReasoning().(*conversationv1.AgentThinkingUpdate_Text)
		if !ok {
			// WITHHELD REASONING DRAWS NO BUBBLE. The model emits the block and
			// a signature but no text; the proto is explicit that there is
			// "nothing to draw but a live indicator", and the feed's live
			// indicator is the footer's, not a bubble. Because the row is
			// deferred until first content, no bubble was ever opened — so a
			// withheld update simply draws nothing, with no retire to perform.
			log.Debug("daemon.feed.thinking_withheld",
				"a reasoning block is withholding its text; no thinking bubble is drawn",
				dlog.Context{"unit": unit})
			return nil, errNotARow
		}
		fold.markdown += text.Text.GetNewText()
		if text.Text.GetNewText() != "" {
			r.observeFragment(fold, pace, paced)
		}
		if fold.markdown == "" {
			// A content-free delta (an empty fragment before any real text)
			// carries no content yet, so it keeps the row deferred rather than
			// opening an empty bubble.
			log.Debug("daemon.feed.thinking_deferred",
				"a reasoning delta carried no text yet; the bubble stays deferred",
				dlog.Context{"unit": unit})
			return nil, errNotARow
		}
		bubble.Result = &frontendv1.FeedResponse_Update{Update: &frontendv1.FeedResponseUpdate{
			Prose:        &frontendv1.FeedResponseProse{Markdown: fold.markdown},
			RevealWindow: r.revealWindow(pace, paced),
		}}
	case *conversationv1.AgentThinking_Success:
		text, ok := state.Success.GetReasoning().(*conversationv1.AgentThinkingSuccess_Text)
		if !ok {
			// A withheld block settles withheld: the proto says draw NOTHING
			// once it settles, "not an empty card". An unset reasoning arm is
			// treated the same — nothing to draw. Because the row is deferred
			// until first content, no bubble was ever opened, so this simply
			// draws nothing with no retire.
			log.Debug("daemon.feed.thinking_withheld_settled",
				"a reasoning block settled withheld; no thinking bubble is drawn",
				dlog.Context{"unit": unit})
			return nil, errNotARow
		}
		// THE TERMINAL RESTATES THE WHOLE, replacing whatever the fold
		// accumulated — which is what makes a lost fragment harmless.
		fold.markdown = text.Text.GetText()
		fold.settled = true
		r.settlePacing(s, fold, pace, paced)
		if fold.markdown == "" {
			// A shown block whose settled whole is empty carries no content, so
			// it draws nothing rather than settling an empty card.
			log.Debug("daemon.feed.thinking_deferred",
				"a reasoning block settled with no text; no thinking bubble is drawn",
				dlog.Context{"unit": unit})
			return nil, errNotARow
		}
		bubble.Result = &frontendv1.FeedResponse_Success{Success: &frontendv1.FeedResponseSuccess{
			Prose:        &frontendv1.FeedResponseProse{Markdown: fold.markdown},
			RevealWindow: r.revealWindow(pace, paced),
		}}
	case *conversationv1.AgentThinking_Failure:
		// The reasoning that landed stays drawn, marked broken — the mirror of
		// the prose bubble's error arm. WHY it died is the turn's terminal row,
		// never this bubble's business.
		fold.settled = true
		r.settlePacing(s, fold, pace, paced)
		bubble.Result = &frontendv1.FeedResponse_Error{Error: &frontendv1.FeedResponseError{
			Prose: &frontendv1.FeedResponseProse{Markdown: fold.markdown},
		}}
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawThinking", "branch": "default"})
		return nil, errNotARow
	}

	id := r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unit})
	log.Debug("daemon.feed.thinking_emitted",
		"emitted a thinking row to the feed",
		dlog.Context{"unit": unit, "first_emit": !hadContentBefore})
	// DELIBERATELY NOT filed in s.answerRows: a thinking row can never be named
	// the turn's answer, so it can never take the green final-answer treatment.
	return &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Response{Response: bubble},
		}},
	}, nil
}
