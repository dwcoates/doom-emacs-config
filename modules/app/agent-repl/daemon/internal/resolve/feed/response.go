package feed

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// THE PROSE FOLD. The shim forwards each fragment as the vendor emits it and
// accumulates nothing; the DAEMON accumulates and re-pushes the whole row, so
// the client renders what arrives and accumulates nothing either. A missed
// push self-corrects on the next one, and the terminal frame restates the
// whole regardless — which is why a settled bubble is always right.

// drawResponse folds one response block into its bubble.
func (r *resolver) drawResponse(s *wsState, at placement, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, response *conversationv1.AgentResponse) (*frontendv1.FeedRow, error) {
	unit := act.GetActivityId().GetValue()
	fold := s.prose(unit)
	log := r.logger(s.id)

	// A CONTEXT-CUT DIRECTIVE PRODUCES NO RESPONSE BUBBLE. /clear and /compact
	// emit an empty "(no content)" response the webapp would draw as a cut-short
	// card below the bar; a directive's only visible outcome is its separation
	// bar. A genuine user-stop of a real turn keeps its partial prose — only a
	// directive draws nothing. Marked per unit so the OTHER store plane's later
	// re-delivery of the same response, after the terminal cleared the in-flight
	// turn, is dropped too.
	if s.directiveUnits[unit] || (s.rowTurn() != nil && s.directiveTurns[*s.rowTurn()]) {
		s.directiveUnits[unit] = true
		log.Debug("daemon.feed.directive_response_suppressed",
			"a context-cut directive's response frame drew no bubble",
			dlog.Context{"unit": unit})
		return nil, errNotARow
	}

	// THIS FOLD'S TURN, learned once from the turn the session is running. It
	// scopes the CROSS-PLANE reconciliation below: a response block that reaches
	// the resolver under two divergent activity ids only ever collapses against a
	// sibling of the same turn.
	if turn := s.rowTurn(); fold.turn == "" && turn != nil {
		fold.turn = string(*turn)
	}

	// A FRAME ARRIVED, so this fold is not silent. The stall window is dropped
	// and a stall fault raised about this fold is retracted THE INSTANT the
	// frame lands, before anything else is decided about it; the window is
	// restarted at the foot of this function only if the fold is still open.
	r.disarmAnswerStall(s, unit)
	r.clearStalledAnswerFault(s, unit, "a response frame arrived")

	// THE STAMP IS THE FRESH INPUT THIS BUBBLE'S AGENT ADDED SINCE ITS PREVIOUS
	// BUBBLE LANDED (usage.go): growing while the bubble arrives, frozen once it
	// settles. The sink tallied this frame's usage before the draw.
	if stamp := s.openStamp(fold, agent); stamp != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "stamp := s.openStamp(fold, agent); stamp != \"\""})
		fold.usage = stamp
	}

	bubble := &frontendv1.FeedResponse{}
	if fold.usage != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "fold.usage != \"\""})
		bubble.Usage = &frontendv1.FeedResponseUsageStamp{Text: fold.usage}
	}

	switch state := response.GetResult().(type) {
	case *conversationv1.AgentResponse_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawResponse", "branch": "case *conversationv1.AgentResponse_Start"})
		fold.markdown = ""
		fold.settled = false
		fold.notice = false
		bubble.Result = &frontendv1.FeedResponse_Update{Update: &frontendv1.FeedResponseUpdate{
			Prose: &frontendv1.FeedResponseProse{Markdown: ""},
		}}
	case *conversationv1.AgentResponse_Update:
		if fold.settled {
			// A fragment after the terminal cannot re-open a closed bubble;
			// the settled whole is authoritative.
			//
			// THIS IS ORDINARY, NOT A FAULT, AND IT IS RECORDED AT DEBUG.
			// One block's frames reach this fold from TWO STORE PLANES that
			// share an upsert key and are not ordered against one another:
			// the shim's stream plane pays out `start` and delta `update`s as
			// the vendor emits them, while the sidecar's file plane converts
			// the same assistant message out of the transcript into a single
			// settled `success`. The sidecar's success routinely lands
			// BETWEEN the shim's start and its own trailing deltas — measured
			// on 2026-09-04 in TestPerfSubmitPromptAck, where the live order
			// was start, success (file plane), update, update, success
			// (stream plane) for one prose block.
			//
			// Nothing is lost when it happens: a settled frame restates the
			// WHOLE, so the dropped delta was already inside the text on
			// screen. The daemon cannot tell this apart from a single
			// producer disordering its own stream — HistoryEntryAt carries no
			// plane — so warning here can only ever be a false alarm, and a
			// warning that is always false is worse than no warning.
			log.Debug("daemon.feed.response_fragment_after_settle",
				"a prose fragment arrived after the block settled and was not folded in; the settled whole stands",
				dlog.Context{"unit": unit})
			return nil, errNotARow
		}
		fold.markdown += state.Update.GetNewMarkdown()
		// THE SAME BLOCK'S SETTLED WHOLE MAY HAVE LANDED UNDER A DIFFERENT UNIT.
		// When the two store planes disagree on this block's activity id, the
		// settling whole can settle a SIBLING fold of this turn before this
		// fold's own deltas arrive. This delta is then a fragment of an
		// already-settled whole exactly as a same-unit fragment-after-settle is,
		// so it draws nothing — and any partial row this fold already drew is
		// retired, so the settled whole is the block's ONLY row.
		if survivor, ok := r.settledWholeContaining(s, fold.turn, unit, fold.markdown); ok {
			fold.settled = true
			if fold.row != nil {
				r.retire(s, fold.feed, fold.row.GetValue())
				fold.row = nil
			}
			r.aliasAnswerRow(s, unit, survivor, fold.turn, "the settled whole landed under a sibling activity id")
			log.Debug("daemon.feed.response_fragment_of_divergent_settle",
				"a prose fragment matched a same-turn block already settled under a different activity id; the settled whole stands",
				dlog.Context{"unit": unit, "turn": fold.turn})
			return nil, errNotARow
		}
		bubble.Result = &frontendv1.FeedResponse_Update{Update: &frontendv1.FeedResponseUpdate{
			Prose: &frontendv1.FeedResponseProse{Markdown: fold.markdown},
		}}
	case *conversationv1.AgentResponse_Success:
		// THE TERMINAL RESTATES THE WHOLE. Whatever the fold accumulated is
		// replaced outright, which is what makes a lost fragment harmless.
		//
		// THE PROSE IS SERVED VERBATIM, exactly as the streaming delta arm and
		// the failure arm serve it. The response is the tree the metaprompt
		// prescribes, but the daemon no longer wraps it: only the webapp can
		// wrap the tree to the bubble's true live pixel width and re-flow it on
		// resize, which a fixed daemon column limit cannot, so the wrapping is
		// the webapp's alone (webapp/src/metaprompt-tree.ts, a port of the
		// former daemon treefmt engine).
		fold.markdown = state.Success.GetProse().GetMarkdown()
		fold.settled = true
		s.landStamp(fold)
		r.stampSettled(fold, state.Success.GetSettledAt().GetAtMs())
		notice, isNotice := state.Success.GetAuthorship().(*conversationv1.AgentResponseSuccess_SynthesizedNotice)
		// THE SETTLED WHOLE DECIDES AUTHORSHIP, so a later settle restating the
		// block from the other store plane re-decides it rather than inheriting.
		fold.notice = isNotice
		if isNotice {
			// THE VENDOR SYNTHESIZES ERROR NOTICES AS ASSISTANT PROSE. Drawing
			// one as the agent's answer would present an outage as something
			// the agent said, so the daemon states the authorship in the
			// bubble's OWN NOTICE FIELD, which puts the row in the notice
			// register. The PROSE STAYS VERBATIM: a heading spliced into the
			// markdown would be indistinguishable from words the vendor
			// actually wrote, and nothing downstream could pull them apart
			// again.
			bubble.Notice = &frontendv1.FeedResponseNotice{
				Heading: noticeHeading(notice.SynthesizedNotice),
			}
			log.Debug("daemon.feed.response_synthesized_notice",
				"a vendor-synthesized notice was drawn as a notice rather than as the agent's answer",
				dlog.Context{"unit": unit, "subject": noticeSubject(notice.SynthesizedNotice)})
		}
		bubble.Result = &frontendv1.FeedResponse_Success{Success: &frontendv1.FeedResponseSuccess{
			Prose: &frontendv1.FeedResponseProse{Markdown: fold.markdown},
		}}
	case *conversationv1.AgentResponse_Failure:
		// The prose that landed stays drawn, marked broken. WHY it died is the
		// turn's terminal row, never this bubble's business.
		//
		// A REFUSAL IS REMEMBERED FOR THE TERMINAL, though: the failure the
		// producer ends the run with (AgentModelError) is an empty message, so
		// this is the only frame that says the vendor refused rather than
		// errored, and feed.proto's `refusal` arm is drawn from it.
		if _, refused := state.Failure.GetReason().GetReason().(*conversationv1.AgentResponseFailureReason_Refused); refused && s.evidenceTurn() != nil {
			turn := string(*s.evidenceTurn())
			s.turnRefusals[turn] = true
			log.Debug("daemon.feed.response_refused",
				"a response ended on the vendor's refusal; the turn's terminal draws the refusal arm",
				dlog.Context{"unit": unit, "turn": turn})
		}
		fold.markdown = state.Failure.GetProse().GetMarkdown()
		fold.settled = true
		s.landStamp(fold)
		fold.notice = false
		r.stampSettled(fold, state.Failure.GetSettledAt().GetAtMs())
		bubble.Result = &frontendv1.FeedResponse_Error{Error: &frontendv1.FeedResponseError{
			Prose: &frontendv1.FeedResponseProse{Markdown: fold.markdown},
		}}
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawResponse", "branch": "default"})
		return nil, errNotARow
	}

	// THE STAMP'S INSTANT RIDES THE SETTLE, not the push. The corner is drawn
	// from the fold's figure in every arm, but its instant is meaningful only
	// once the fold settled; set after the switch so a settling frame carries
	// the instant it just stamped, and a still-arriving frame carries zero.
	if bubble.Usage != nil {
		bubble.Usage.AtMs = fold.settledAtMs
	}

	id := r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unit})
	s.answerRows[unit] = id
	// THE SETTLED WHOLE IS THE BLOCK'S ONLY ROW, even when the two store planes
	// delivered the block under DIVERGENT activity ids. A settling frame retires
	// any same-turn fragment that lost its opening deltas to the other id, so the
	// settle can never leave an unsettled partial standing beside the green whole.
	//
	// RUN AFTER THE ROW ID IS MINTED, because the retirement ALIASES each retired
	// unit onto THIS row: the shim may name the retired unit as the turn's answer,
	// and an alias is what makes that lookup land on the surviving row.
	if fold.settled {
		r.reconcileDivergentProse(s, fold.turn, unit, id, fold.markdown)
	}
	// THE GREEN FINAL-ANSWER BORDER IS A DATA PROPERTY, STAMPED ON EVERY DRAW.
	// When this row is the one the workspace has recorded as a turn's concluded
	// answer, the flag rides the row itself so no redraw, tool-group re-arrange,
	// or history replay can lose it — the client draws the green from the flag
	// alone. The recording happens at the terminal, which fires AFTER this
	// response's frames both live and on replay, so a row drawn before its turn
	// concluded is re-stamped by restampFinalAnswer at the terminal; this branch
	// carries the flag on every LATER draw of an already-recorded answer (a
	// file-plane re-delivery, a resize redraw, a replay where the terminal
	// already ran). A thinking bubble never reaches here — it draws through
	// drawThinking — so it is excluded structurally.
	if s.finalAnswerSeen[id.GetValue()] {
		bubble.FinalAnswer = true
	}
	// KEPT SO A DIVERGENT SIBLING CAN RETIRE THIS FRAGMENT'S ROW. When the same
	// block's settled whole later lands under another id, or a late delta of this
	// fold matches a whole already settled under another id, the reconciler needs
	// the feed and row identity this fold drew on.
	fold.feed = at.feed
	fold.row = id
	// AN OPEN FOLD IS EXPECTED TO KEEP MOVING. Rearmed from THIS frame, so the
	// window measures silence rather than the fold's whole lifetime; a settled
	// fold is owed nothing more and stays disarmed.
	if !fold.settled {
		r.armAnswerStall(s, unit, fold.turn)
	}
	return &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Response{Response: bubble},
		}},
	}, nil
}

// THE TWO STORE PLANES CAN DELIVER ONE RESPONSE BLOCK UNDER DIVERGENT ACTIVITY
// IDS. A prose block's activity id is `<message.id>:<block index>`, and the
// shim's stream plane and the sidecar's file plane are supposed to mint the same
// one so their frames collapse onto a single fold. When they DON'T — a stream
// that re-mints its message id across a resume, or a block index that shifts
// under a leading thinking block — the block splits into two folds: the stream's
// start+delta updates under one id, the settling whole under another. Keyed on
// the activity id alone, that drew a settled green whole beside an unsettled
// fragment that had lost its opening deltas (the owner's double render), or, when
// the whole's own row never reached the reader, a lone streamed bubble that never
// went green even though the turn's Success exists. The two functions below
// reconcile the split against the ONE fact both folds share — this turn, and the
// prose one is a fragment of the other — so the settled whole is always the
// block's only row.

// reconcileDivergentProse retires any OTHER response fold of the same turn whose
// accumulated fragment is a suffix of the settled whole `prose`. The lost frames
// are the block's OPENING deltas, so the surviving fragment is a suffix of the
// whole; requiring a non-empty suffix that is no longer than the whole keeps two
// genuinely distinct prose blocks — whose texts do not nest — apart. The retired
// fold is marked settled so a still-arriving delta of it can never re-open a row.
func (r *resolver) reconcileDivergentProse(s *wsState, turn, keepUnit string, keepRow *frontendv1.FeedId, prose string) {
	if turn == "" || prose == "" {
		return
	}
	for unit, fold := range s.responses {
		if unit == keepUnit || fold.turn != turn || fold.markdown == "" {
			continue
		}
		if len(fold.markdown) > len(prose) || !strings.HasSuffix(prose, fold.markdown) {
			continue
		}
		fold.settled = true
		if fold.row != nil {
			r.retire(s, fold.feed, fold.row.GetValue())
			fold.row = nil
		}
		r.aliasAnswerRow(s, unit, keepRow, turn, "the settled whole retired this fragment's row")
		r.logger(s.id).Debug("daemon.feed.response_divergent_fold_retired",
			"a same-turn response fragment under a divergent activity id was retired in favour of the settled whole",
			dlog.Context{"turn": turn, "retired_unit": unit, "kept_unit": keepUnit})
	}
}

// aliasAnswerRow POINTS A RETIRED UNIT AT THE ROW THAT SURVIVED IT.
//
// THIS IS THE FIX FOR THE ANSWERS THAT NEVER WENT GREEN. A response block that
// reaches the resolver under two divergent activity ids leaves ONE row standing
// and retires the other fold — and the producer is then free to name EITHER id
// as the turn's answer, because it knows nothing about which id this resolver
// kept. Dropping the retired unit's mapping (what this used to do) made the
// terminal's lookup miss whenever the producer named the retired one: no green
// border, no selectable final response, and a debug line as the only trace. Over
// one 40-hour window the daemon's own records had 199 answers named and 175 rows
// found; the 24 that went missing are exactly this.
//
// So the mapping is REPOINTED rather than deleted. Both ids name the same block,
// the surviving row IS that block's row, and a lookup through either id lands on
// it. A survivor with no row of its own (the whole settled but drew nothing)
// leaves no alias to make, and the mapping is dropped as before — an alias to
// nothing would be a worse answer than none.
func (r *resolver) aliasAnswerRow(s *wsState, unit string, survivor *frontendv1.FeedId, turn, why string) {
	if survivor.GetValue() == "" {
		delete(s.answerRows, unit)
		r.logger(s.id).Debug("daemon.feed.answer_row_alias_unavailable",
			"a retired response fold had no surviving sibling row to alias onto; the unit names no answer row",
			dlog.Context{"unit": unit, "turn": turn, "why": why})
		return
	}
	s.answerRows[unit] = survivor
	r.logger(s.id).Debug("daemon.feed.answer_row_aliased",
		"a retired response unit was aliased onto the surviving sibling's row so a terminal naming it still resolves",
		dlog.Context{"unit": unit, "turn": turn, "row": survivor.GetValue(), "why": why})
}

// settledWholeContaining reports whether some OTHER settled response fold of the
// same turn already restated a whole that ENDS WITH this fragment — the mirror of
// reconcileDivergentProse for the order where the whole settles first and this
// fold's own deltas arrive after, under the divergent id.
func (r *resolver) settledWholeContaining(s *wsState, turn, unit, fragment string) (*frontendv1.FeedId, bool) {
	if turn == "" || fragment == "" {
		return nil, false
	}
	for other, fold := range s.responses {
		if other == unit || fold.turn != turn || !fold.settled || fold.markdown == "" {
			continue
		}
		if len(fragment) <= len(fold.markdown) && strings.HasSuffix(fold.markdown, fragment) {
			// THE SURVIVOR'S ROW IS ANSWERED, not merely the fact of it, so the
			// caller can alias this fragment's unit onto the row that stands.
			return fold.row, true
		}
	}
	return nil, false
}

// stampSettled records the instant a fold reached its terminal state, ONCE.
// A second terminal frame for the same fold (the file plane replaying the
// stream plane's settle) keeps the first instant rather than moving the
// corner's "N ago" to the replay time.
//
// carriedAtMs is the REAL settle instant the producer stamped on the terminal
// arm (AgentResponseSuccess/Failure.settled_at), epoch ms, or zero when the
// arm carried none. When present it IS the stamp, so a re-compose from a fresh
// fold (store/history replay, file-plane redelivery, reconnect) reproduces the
// SAME instant instead of the compose-time clock. r.deps.Now() is only the
// fallback for a truly live settle whose source carried no instant.
//
// REGRESSION WATCH (2026-09-14): the corner read "0s ago" on every replay
// because this stamped r.deps.Now() unconditionally — compose time, not the
// settle instant — so any re-compose reset "N ago" to zero. The carried
// settled_at (mirroring every other activity terminal) is what keeps at_ms
// stable across replays; do not drop it back to an unconditional Now().
func (r *resolver) stampSettled(fold *proseState, carriedAtMs int64) {
	if fold.settledAtMs != 0 {
		return
	}
	if carriedAtMs != 0 {
		fold.settledAtMs = carriedAtMs
		return
	}
	fold.settledAtMs = r.deps.Now().UnixMilli()
}

// noticeHeading composes the notice register's heading. It is a HEADING, not
// markdown prose: the client draws it above the bubble in its own treatment,
// so it carries no emphasis markup of its own.
func noticeHeading(notice *conversationv1.AgentResponseSynthesizedNotice) string {
	switch notice.GetSubject().(type) {
	case *conversationv1.AgentResponseSynthesizedNotice_UsageLimit:
		return "Notice — your allowance is exhausted. This is the vendor's own message, not the agent's."
	case *conversationv1.AgentResponseSynthesizedNotice_UsageTransition:
		return "Notice — your allowance window changed. This is the vendor's own message, not the agent's."
	case *conversationv1.AgentResponseSynthesizedNotice_UsageWarning:
		return "Notice — you are approaching an allowance limit. This is the vendor's own message, not the agent's."
	}
	return "Notice from the vendor's tooling, not the agent's answer."
}

// noticeSubject names the notice's subject for a log record.
func noticeSubject(notice *conversationv1.AgentResponseSynthesizedNotice) string {
	switch notice.GetSubject().(type) {
	case *conversationv1.AgentResponseSynthesizedNotice_UsageLimit:
		return "usage_limit"
	case *conversationv1.AgentResponseSynthesizedNotice_UsageTransition:
		return "usage_transition"
	case *conversationv1.AgentResponseSynthesizedNotice_UsageWarning:
		return "usage_warning"
	}
	return "unclassified"
}
