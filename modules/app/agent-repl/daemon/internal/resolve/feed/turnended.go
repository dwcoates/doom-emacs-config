package feed

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// THE TURN'S TERMINAL — a row, so history replays how every turn ended and
// liveness is STRUCTURAL: no terminal row for the current turn means the turn
// is live. A connection dying without one stays a transport failure.

// drawTerminal draws how one agent's stream ended.
func (r *resolver) drawTerminal(s *wsState, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	log := r.logger(s.id)

	if turn == nil {
		// A SUBAGENT's stream ending is its bubble's business, not a turn
		// terminal: the bubble already settles from the spawn unit's own
		// frames, and a second terminal row would claim the turn ended.
		log.Debug("daemon.feed.agent_terminal_without_turn",
			"an agent stream ended outside a turn; its bubble carries the ending",
			dlog.Context{"agent": agent.GetValue()})
		return
	}

	// AN UNPLACEABLE TERMINAL STILL ENDS ITS TURN. Only its row is not drawn
	// (place has reported why); the turn's bookkeeping below — its stalls, its
	// prompts, its held detachments — is owed whether or not a row can land.
	at, placed := r.place(s, agent)

	turnID := &conversationv1.TurnId{Value: string(*turn)}
	ended := &frontendv1.FeedTurnEnded{EndedAtMs: r.deps.Now().UnixMilli()}

	// THE TERMINAL ANSWERS EVERY STALL THIS TURN HAD OPEN. No fold of a turn
	// that ended is still expecting a frame, so the windows are dropped and a
	// stall fault is retracted BEFORE the conclusion below decides whether this
	// terminal raises a fault of its own.
	r.disarmTurnStalls(s, string(*turn))
	r.clearTurnStalledAnswerFault(s, string(*turn), "the turn's terminal arrived")

	switch {
	case success != nil:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawTerminal", "branch": "case success != nil"})
		r.concludedOutcome(s, string(*turn), success)(ended)
	case failure != nil:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawTerminal", "branch": "case failure != nil"})
		ended.Outcome = &frontendv1.FeedTurnEnded_Errored{Errored: r.erroredOutcome(s, string(*turn), failure)}
	default:
		log.Error("daemon.feed.terminal_without_outcome",
			"an agent terminal carried neither success nor failure",
			dlog.Context{"agent": agent.GetValue(), "turn": string(*turn)})
		return
	}

	// A turn that ended still in plan mode BROKE the episode.
	r.breakPlanEpisodes(s, "the turn ended while plan mode was still open")

	// A /clear TURN OWNS NO TERMINAL BUBBLE. The clear interrupts the turn to
	// cut the context, and drawing that interrupt as a card below the cleared
	// divider is the "response cut short" bubble a clear must never leave. A
	// CONFIRMED clear (its ContextCut arrived) suppresses its SUCCESS terminal
	// outright; a clear that never confirmed FAILED, so its optimistic red bar is
	// retired and the terminal is drawn — a failure is surfaced, never swallowed.
	suppress := false
	switch {
	case success != nil && s.clearConfirmed[*turn]:
		// The clear SUCCEEDED — its ContextCut confirmed it. This holds whether
		// the terminal is watched live or replayed from history, because the
		// replayed cut sets the same confirmation, so the bubble never returns on
		// a restart.
		suppress = true
		log.Info("daemon.feed.clear_terminal_suppressed",
			"a /clear turn concluded; the cleared divider is its outcome and no terminal bubble is drawn",
			dlog.Context{"turn": string(*turn)})
	case s.clearTurns[*turn]:
		// The turn was opened as a /clear and drew an optimistic bar, but no cut
		// ever confirmed it: the clear FAILED. Retire the phantom bar and let the
		// terminal draw so the failure is surfaced, never swallowed.
		r.retireOptimisticClear(s, *turn, "the /clear turn ended without cutting context")
	}
	// THE DIRECTIVE FLAGS ARE NOT FORGOTTEN AT THE TERMINAL. Each store plane
	// delivers the directive's prompt, response and terminal independently, and
	// the file plane's copy can land after this terminal; a flag dropped here let
	// that late copy draw a stale prompt/response/terminal below the bar (the
	// owner's "/clear renders after a later prompt"). The turn ids are unique, so
	// keeping the flags is cheap and is what makes the suppression hold across
	// every delivery of the turn's frames.

	row := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindTurnEnded, ID: string(*turn)}),
		Turn: turnID,
		Row:  &frontendv1.FeedRow_TurnEnded{TurnEnded: ended},
	}
	switch {
	case suppress:
		log.Debug("daemon.feed.turn_ended",
			"a /clear turn's terminal row was suppressed",
			dlog.Context{"turn": string(*turn), "outcome": terminalArm(ended)})
	case !placed:
		log.Debug("daemon.feed.turn_ended",
			"a turn's terminal row has no feed to land on and was not drawn",
			dlog.Context{"turn": string(*turn), "outcome": terminalArm(ended), "agent": agent.GetValue()})
	default:
		log.Debug("daemon.feed.turn_ended",
			"a turn's terminal row was upserted",
			dlog.Context{"turn": string(*turn), "outcome": terminalArm(ended)})
		r.upsert(s, at, row, true)
	}

	// A LIVE ENDING IS FILED FOR THE DESKTOP BANNER — unless a confirmed /clear
	// suppressed it, whose divider is the whole of its outcome.
	if !suppress {
		r.fileLiveEnding(s, *turn, ended, failure)
	}

	// THE TURN'S PROMPTS STOP WORKING ON THIS EDGE, whether or not the terminal
	// row itself was drawn (a confirmed /clear suppresses it, and still ended).
	r.settleTurnPrompts(s, *turn)

	// A SPAWN WHOSE START NEVER ARRIVED is the same class of producer fault,
	// and the turn ending is the last moment its start could still have named
	// the created agent.
	r.retireHeldSpawns(s, "the turn ended")

	// A DETACHMENT THAT NEVER FOUND ITS UNIT is unplaceable: its head belongs
	// at the spawning call's row, in the spawner's feed, and no such row was
	// ever drawn. The turn ending is the last moment it could still have been
	// claimed, so it is reported here — at ERROR and on the topbar — and
	// nothing is drawn for it.
	//
	// A DETACHMENT REPLAYED FROM HISTORY IS NOT ONE. A replay serves the newest
	// page of a book, and a detachment's own row sits at its LAST upsert while
	// its call's row can sit further back than the page reaches: the work is
	// long over, its call simply was not replayed, and nothing was lost. That
	// is recorded at DEBUG; only a detachment that arrived LIVE is a failure.
	for unit, work := range s.detachedUnits {
		said := s.heldDetachments[unit]
		if said.plane == planeLive {
			r.reportUnplaceableWork(s, work, "unit "+unit, said.owner, said.announcer,
				"the spawning call was never drawn in any feed, as of the end of turn "+string(*turn))
		} else {
			log.Debug("daemon.feed.replayed_detachment_unclaimed",
				"a detachment replayed from history named a call the replay never reached; nothing is drawn for it",
				dlog.Context{"unit": unit, "work": work, "owner": said.owner, "announcer": said.announcer, "turn": string(*turn)})
		}
		delete(s.detachedUnits, unit)
		delete(s.heldDetachments, unit)
	}

	delete(s.turnEvidence, string(*turn))
	delete(s.turnRefusals, string(*turn))
	if s.turnInFlight != nil && *s.turnInFlight == *turn {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.turnInFlight != nil && *s.turnInFlight == *turn"})
		s.turnInFlight = nil
	}
	// A QUERY DEATH STILL OWES ROWS (wsState.turnStamp), and its terminal is
	// the death as much as the session's push is: whichever of the two lands
	// first, the stamp stands for the stand-down's denials still to come.
	if s.turnStamp != nil && *s.turnStamp == *turn && !diedOfQuery(failure) {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.turnStamp != nil && *s.turnStamp == *turn && !diedOfQuery(failure)"})
		s.turnStamp = nil
	}
}

// terminalArm names a terminal's arm for a log record.
func terminalArm(ended *frontendv1.FeedTurnEnded) string {
	switch ended.GetOutcome().(type) {
	case *frontendv1.FeedTurnEnded_Concluded:
		return "concluded"
	case *frontendv1.FeedTurnEnded_Errored:
		return "errored"
	case *frontendv1.FeedTurnEnded_Interrupted:
		return "interrupted"
	}
	return "unset"
}

// concludedOutcome renders the two SUCCESS endings. Both are answers, never
// failures: a stop the user asked for is not the turn breaking.
func (r *resolver) concludedOutcome(s *wsState, turn string, success *conversationv1.AgentSuccess) turnOutcome {
	switch outcome := success.GetOutcome().(type) {
	case *conversationv1.AgentSuccess_Completed:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "concludedOutcome", "branch": "case *conversationv1.AgentSuccess_Completed"})
		concluded := &frontendv1.FeedTurnEndedConcluded{}
		// THE ANSWERING RESPONSE, named by the producer rather than derived
		// from position.
		//
		// EVERY WAY THIS FAILS IS A FAULT NOW, not a silence. See
		// finalanswer.go: a turn that concluded with no green answer standing
		// is a fact the reader is owed, and the footer's fault chip is where
		// they are told it. A CONTEXT-CUT DIRECTIVE is excluded outright — its
		// response draws no bubble at all, so it never had an answer to lose.
		answer := outcome.Completed.GetAnswer().GetValue()
		switch {
		case s.directiveTurns[ids.TurnID(turn)] || s.directiveUnits[answer]:
			r.logger(s.id).Debug("daemon.feed.final_answer_directive_turn",
				"a context-cut directive's turn concluded; it draws no answering bubble and owes no final answer",
				dlog.Context{"turn": turn, "unit": answer})
		case answer == "":
			// NOT LANDED (a): the producer named nothing while this turn's
			// prose was drawn, so the words on screen belong to no answer.
			if unit, drew := r.turnDrewProse(s, turn); drew {
				r.raiseAnswerFault(s, turn, unit, whyNoAnswerNamed,
					"the turn concluded naming no answering response while its prose was drawn")
			}
		default:
			id, known := s.answerRows[answer]
			if known && r.restampFinalAnswer(s, id, answer) {
				concluded.Answer = id
				// THE GREEN FINAL-ANSWER ROW BECOMES SELECTABLE HERE, at the
				// one site that names it — reply-to-a-past-response mode walks
				// exactly the rows drawn with that border. Recorded append-once
				// (the terminal replays across planes) so the selectable set
				// carries each answer once, in conclusion order, which is
				// root-feed order.
				//
				// THE RE-STAMP IS WHAT MAKES THE GREEN APPEAR WITHOUT A LIVE
				// EVENT, and it is the gate above rather than a statement after
				// it: the response's frames drew this row before the terminal
				// named it the answer, so it is on screen without the flag, and
				// re-pushing it with final_answer=true is the whole of the
				// border. History replay walks this same path, which is what
				// makes the green survive a reconnect with no turn-ended event.
				// When it answers false there is no drawn row to stand behind,
				// which is exactly the NOT LANDED (b) case.
				r.recordFinalAnswer(s, id, answer)
				break
			}
			// NOT LANDED (b): the producer named an answer this resolver
			// resolves to no drawn response row.
			r.raiseAnswerFault(s, turn, answer, whyAnswerRowUnresolved,
				"the turn's named answering response resolves to no drawn response row")
		}
		return concludedArm(concluded)
	case *conversationv1.AgentSuccess_Interrupted:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{
			"function": "concludedOutcome", "branch": "case *conversationv1.AgentSuccess_Interrupted",
			"turn": turn, "command": byUserCommandWord(outcome.Interrupted.GetByUser()),
		})
		return interruptedArm(outcome.Interrupted.GetByUser())
	case *conversationv1.AgentSuccess_Backgrounded:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "concludedOutcome", "branch": "case *conversationv1.AgentSuccess_Backgrounded"})
		// The stream ended while the work did not. It is what was asked for,
		// so the turn concluded — with no answering prose to point at.
		return concludedArm(&frontendv1.FeedTurnEndedConcluded{})
	}
	return concludedArm(&frontendv1.FeedTurnEndedConcluded{})
}

// turnOutcome sets a terminal row's outcome arm. A setter rather than the
// generated oneof interface, whose method is unexported and unimplementable
// from here.
type turnOutcome func(*frontendv1.FeedTurnEnded)

// concludedArm is the conclusion setter.
func concludedArm(concluded *frontendv1.FeedTurnEndedConcluded) turnOutcome {
	return func(ended *frontendv1.FeedTurnEnded) {
		ended.Outcome = &frontendv1.FeedTurnEnded_Concluded{Concluded: concluded}
	}
}

// byUserCommandWord names a recorded stop's command for a log record.
func byUserCommandWord(byUser *conversationv1.AgentInterruptedByUser) string {
	switch byUser.GetCommand().(type) {
	case *conversationv1.AgentInterruptedByUser_Direct:
		return "direct"
	case *conversationv1.AgentInterruptedByUser_Interjection:
		return "interjection"
	}
	return "unset"
}

// interruptedArm is THE ONE interruption setter, shared by the two paths that
// draw a stopped turn's ending: the terminal's (concludedOutcome) and the
// daemon-built close's (closedEnding). One builder is what keeps the two
// drawing the same row for the same stop, live and rebuilt.
//
// byUser is the recorded `by_user` cause, and its command becomes the row's:
// a direct stop draws the interruption bubble, an interjection draws nothing
// (the superseding prompt is the whole account of it), and a cause that stated
// no command — or no `by_user` at all — stays UNSET, which the client draws as
// a direct stop.
func interruptedArm(byUser *conversationv1.AgentInterruptedByUser) turnOutcome {
	interrupted := &frontendv1.FeedTurnEndedInterrupted{}
	switch byUser.GetCommand().(type) {
	case *conversationv1.AgentInterruptedByUser_Direct:
		interrupted.Command = &frontendv1.FeedTurnEndedInterrupted_Direct{Direct: &frontendv1.FeedTurnEndedInterruptedDirect{}}
	case *conversationv1.AgentInterruptedByUser_Interjection:
		interrupted.Command = &frontendv1.FeedTurnEndedInterrupted_Interjection{Interjection: &frontendv1.FeedTurnEndedInterruptedInterjection{}}
	}
	return func(ended *frontendv1.FeedTurnEnded) {
		ended.Outcome = &frontendv1.FeedTurnEnded_Interrupted{Interrupted: interrupted}
	}
}

// erroredOutcome respells an agent failure into the feed's drawn taxonomy, and
// composes the headline the client draws verbatim.
func (r *resolver) erroredOutcome(s *wsState, turn string, failure *conversationv1.AgentFailure) *frontendv1.FeedTurnEndedErrored {
	errored := &frontendv1.FeedTurnEndedErrored{}
	var (
		vendorMessage string
		sentence      string
	)

	// A TERMINAL THAT SAYS THE QUERY DIED IS DRAWN AS THE DEATH, by the very
	// builder the session's own query_died push draws with. The two statements
	// travel by independent channels with no ordering between them, and a
	// replayed turn has only this one, so the row is the same whichever of
	// them drew it -- live in either order, and on replay.
	if died, ok := failure.GetFailure().(*conversationv1.AgentFailure_QueryDied); ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "died, ok := failure.GetFailure().(*conversationv1.AgentFailure_QueryDied); ok"})
		return queryDiedErrored(died.QueryDied)
	}

	var endedOn *conversationv1.ApiRequestFailed
	if api, ok := failure.GetFailure().(*conversationv1.AgentFailure_ApiRequestFailed); ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "api, ok := failure.GetFailure().(*conversationv1.AgentFailure_ApiRequestFailed); ok"})
		endedOn = api.ApiRequestFailed
		vendorMessage = endedOn.GetMessage()
		sentence = apiErrorArm(errored, endedOn)
	} else {
		sentence = producerErrorArm(errored, failure, s.turnRefusals[turn])
		vendorMessage = strings.Join(failure.GetErrors(), "; ")
	}

	// The turn's own EVIDENCE — a mid-turn api error that was retried, a
	// compaction that failed — rides the headline, because the turn's terminal
	// is the one place a reader is looking when they ask what went wrong.
	//
	// A TURN NEVER RESTATES THE FAILURE IT DIED OF, and that is a race being
	// closed rather than a nicety. The vendor's failure reaches this resolver
	// TWICE by two independent producers: the sidecar tailing the session
	// transcript's `system:api_error` line, and the shim's own stream terminal.
	// So an api failure that ENDS a turn also arrives as mid-turn evidence, and
	// which of the two lands first is a schedule nobody controls. Measured in
	// one headless run of the twelve `!api-*` arms: the evidence lost that race
	// by 17ms on `api-429` and won it by 3ms on `api-401`, so ONE run drew two
	// different headlines for the same shape of failure. The evidence line's own
	// words settle which is right — it says the turn WENT ON, which is false of
	// the failure that ended it — so the terminal drops its own failure from its
	// evidence and the headline is the arm's sentence whatever the schedule. A
	// DIFFERENT mid-turn failure still rides: a 429 the turn survived and a 500
	// it then died of are two facts, and the reader wants both.
	evidence, dropped := evidenceBesides(s.turnEvidence[turn], endedOn)
	if dropped > 0 {
		r.logger(s.id).Debug("daemon.feed.terminal_evidence_is_the_terminal",
			"the turn's terminal did not restate the api failure it ended on",
			dlog.Context{"dropped": dropped, "turn": turn})
	}
	if len(evidence) > 0 {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "len(evidence) > 0"})
		sentence = sentence + " (" + strings.Join(evidence, "; ") + ")"
	}
	applyHeadline(errored, headline{Text: sentence}, vendorMessage)
	return errored
}

// evidenceBesides is a turn's evidence sentences, minus the mid-turn api
// failure the turn's terminal IS. `endedOn` is nil when the turn did not end on
// an api failure at all, and then every line stands. The count of what was
// dropped is answered so the suppression is RECORDED rather than silent.
//
// The vendor's message is the discriminator because it is the SAME record
// reaching this resolver by two paths, so the two carry one string; a mid-turn
// failure the turn survived says something else and is kept.
func evidenceBesides(lines []turnEvidenceLine, endedOn *conversationv1.ApiRequestFailed) (texts []string, dropped int) {
	texts = make([]string, 0, len(lines))
	for _, line := range lines {
		if endedOn != nil && line.apiFailure && line.apiMessage == endedOn.GetMessage() {
			dropped++
			continue
		}
		texts = append(texts, line.text)
	}
	return texts, dropped
}

// apiErrorArm maps the vendor's own error taxonomy onto the feed's, and words
// each arm. THE ARM IS THE CAUSE and the sentence is ours: the client holds no
// per-arm table.
func apiErrorArm(errored *frontendv1.FeedTurnEndedErrored, failed *conversationv1.ApiRequestFailed) string {
	switch kind := failed.GetKind().(type) {
	case *conversationv1.ApiRequestFailed_RateLimited:
		arm := &frontendv1.FeedTurnErrorRateLimited{}
		if kind.RateLimited.RetryAfterMs != nil {
			ms := kind.RateLimited.GetRetryAfterMs()
			arm.RetryAfterMs = &ms
		}
		errored.Error = &frontendv1.FeedTurnEndedErrored_RateLimited{RateLimited: arm}
		return "rate limited by the vendor"
	case *conversationv1.ApiRequestFailed_Overloaded:
		arm := &frontendv1.FeedTurnErrorOverloaded{}
		if kind.Overloaded.RetryAfterMs != nil {
			ms := kind.Overloaded.GetRetryAfterMs()
			arm.RetryAfterMs = &ms
		}
		errored.Error = &frontendv1.FeedTurnEndedErrored_Overloaded{Overloaded: arm}
		return "the vendor API is overloaded"
	case *conversationv1.ApiRequestFailed_AuthenticationFailed:
		errored.Error = &frontendv1.FeedTurnEndedErrored_AuthenticationFailed{
			AuthenticationFailed: &frontendv1.FeedTurnErrorAuthenticationFailed{},
		}
		return "the credential was rejected — sign in again"
	case *conversationv1.ApiRequestFailed_PermissionDenied:
		errored.Error = &frontendv1.FeedTurnEndedErrored_PermissionDenied{
			PermissionDenied: &frontendv1.FeedTurnErrorPermissionDenied{},
		}
		return "the credential lacks permission for this request"
	case *conversationv1.ApiRequestFailed_InvalidRequest:
		errored.Error = &frontendv1.FeedTurnEndedErrored_InvalidRequest{
			InvalidRequest: &frontendv1.FeedTurnErrorInvalidRequest{},
		}
		return "the vendor refused the request as malformed"
	case *conversationv1.ApiRequestFailed_RequestTooLarge:
		errored.Error = &frontendv1.FeedTurnEndedErrored_RequestTooLarge{
			RequestTooLarge: &frontendv1.FeedTurnErrorRequestTooLarge{},
		}
		return "the request exceeded the vendor's size limit"
	case *conversationv1.ApiRequestFailed_NotFound:
		errored.Error = &frontendv1.FeedTurnEndedErrored_NotFound{
			NotFound: &frontendv1.FeedTurnErrorNotFound{},
		}
		return "the model or resource does not exist"
	case *conversationv1.ApiRequestFailed_Internal:
		errored.Error = &frontendv1.FeedTurnEndedErrored_Internal{
			Internal: &frontendv1.FeedTurnErrorInternal{},
		}
		return "the vendor API hit its own internal error"
	case *conversationv1.ApiRequestFailed_BillingError:
		errored.Error = &frontendv1.FeedTurnEndedErrored_BillingError{
			BillingError: &frontendv1.FeedTurnErrorBillingError{},
		}
		return "the account could not be charged — check your billing"
	case *conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		errored.Error = &frontendv1.FeedTurnEndedErrored_OauthOrgNotAllowed{
			OauthOrgNotAllowed: &frontendv1.FeedTurnErrorOauthOrgNotAllowed{},
		}
		return "your organization does not allow this OAuth access"
	case *conversationv1.ApiRequestFailed_MaxOutputTokens:
		errored.Error = &frontendv1.FeedTurnEndedErrored_MaxOutputTokens{
			MaxOutputTokens: &frontendv1.FeedTurnErrorMaxOutputTokens{},
		}
		return "the request asked for more output than the model will produce"
	case *conversationv1.ApiRequestFailed_Unmodeled:
		errored.Error = &frontendv1.FeedTurnEndedErrored_VendorUnmodeled{
			VendorUnmodeled: &frontendv1.FeedTurnErrorVendorUnmodeled{Type: kind.Unmodeled.GetType()},
		}
		return "the vendor reported an error class we do not model yet"
	}
	errored.Error = &frontendv1.FeedTurnEndedErrored_VendorUnmodeled{
		VendorUnmodeled: &frontendv1.FeedTurnErrorVendorUnmodeled{Type: "unset"},
	}
	return "the vendor reported a failure with no stated class"
}

// producerErrorArm respells the PRODUCER's own turn-ending vocabulary. Each
// arm leads somewhere different, which is why the headline is per-arm: some
// are waited out, some are the user's to raise, some are a fault to report.
//
// A PRODUCER TERMINAL WITH NO DRAWN COUNTERPART IS `turn_failed`, never
// `vendor_unmodeled`: feed.proto confines vendor_unmodeled to "an API error
// class this schema does not model", while turn_failed carries "every other
// unclassified abnormal end" with `stop_reason` naming the vendor's own word.
// The reader still learns which of these happened — from the stop reason.
func producerErrorArm(errored *frontendv1.FeedTurnEndedErrored, failure *conversationv1.AgentFailure, refused bool) string {
	if cause := lostCauseOfAgentFailure(failure); cause != lostNone {
		// A LOST RUN IS NOT AN UNMODELED API ERROR CLASS: vendor_unmodeled is
		// confined to those, so the producer's own "lost" vocabulary lands
		// under turn_failed with the cause as the stop reason.
		errored.Error = turnFailedArm("lost:" + cause.String())
		return lostSentence(cause)
	}
	switch failure.GetFailure().(type) {
	case *conversationv1.AgentFailure_PromptTooLong:
		// NOT request_too_large: that arm is the vendor's 413, an API status
		// this producer terminal never carried.
		errored.Error = turnFailedArm("prompt_too_long")
		return "the prompt was too long to send — the context must be cut first"
	case *conversationv1.AgentFailure_BlockingLimit:
		errored.Error = turnFailedArm("blocking_limit")
		return "an account-level block stopped the run"
	case *conversationv1.AgentFailure_RapidRefillBreaker:
		errored.Error = turnFailedArm("rapid_refill_breaker")
		return "the account's refill-rate breaker tripped — this is a wait, not a fault"
	case *conversationv1.AgentFailure_ImageError:
		errored.Error = turnFailedArm("image_error")
		return "an image in the request could not be processed"
	case *conversationv1.AgentFailure_ModelError:
		// THE REFUSAL'S ONLY WITNESS is the response frame this turn already
		// drew: AgentModelError is empty, so a refusal and an unclassified
		// model error arrive as the same terminal.
		if refused {
			errored.Error = &frontendv1.FeedTurnEndedErrored_Refusal{
				Refusal: &frontendv1.FeedTurnErrorRefusal{},
			}
			return "the model refused to continue — there is no answer"
		}
		errored.Error = turnFailedArm("model_error")
		return "the model errored in a way the API did not classify"
	case *conversationv1.AgentFailure_MalformedToolUseExhausted:
		errored.Error = turnFailedArm("malformed_tool_use_exhausted")
		return "the model's tool calls could not be parsed and the attempts ran out"
	case *conversationv1.AgentFailure_StopHookPrevented:
		errored.Error = &frontendv1.FeedTurnEndedErrored_StopHookPrevented{
			StopHookPrevented: &frontendv1.FeedTurnErrorStopHookPrevented{},
		}
		return "a Stop hook ended the run"
	case *conversationv1.AgentFailure_HookStopped:
		errored.Error = turnFailedArm("hook_stopped")
		return "a hook ended the run"
	case *conversationv1.AgentFailure_ToolDeferred:
		errored.Error = turnFailedArm("tool_deferred")
		return "the run ended waiting on a deferred tool call"
	case *conversationv1.AgentFailure_ToolDeferredUnavailable:
		errored.Error = turnFailedArm("tool_deferred_unavailable")
		return "the run ended on a tool call deferred to something unavailable"
	case *conversationv1.AgentFailure_MaxTurns:
		errored.Error = &frontendv1.FeedTurnEndedErrored_MaxTurns{
			MaxTurns: &frontendv1.FailureVendorMaxTurns{Vendor: vendorFailureContext()},
		}
		return "stopped at the turn limit"
	case *conversationv1.AgentFailure_BudgetExhausted:
		errored.Error = &frontendv1.FeedTurnEndedErrored_MaxBudget{
			MaxBudget: &frontendv1.FailureVendorMaxBudget{Vendor: vendorFailureContext()},
		}
		return "stopped at the budget"
	case *conversationv1.AgentFailure_StructuredOutputRetryExhausted:
		errored.Error = turnFailedArm("structured_output_retry_exhausted")
		return "the run ended: structured_output_retry_exhausted"
	case *conversationv1.AgentFailure_TurnSetupFailed:
		errored.Error = turnFailedArm("turn_setup_failed")
		return "the run could not be set up and never reached the model"
	case *conversationv1.AgentFailure_ExecutionError:
		errored.Error = &frontendv1.FeedTurnEndedErrored_ExecutionError{
			ExecutionError: &frontendv1.FailureVendorExecutionError{Vendor: vendorFailureContext()},
		}
		return "the run broke while executing"
	case *conversationv1.AgentFailure_ContinuationPrevented:
		errored.Error = turnFailedArm("continuation_prevented")
		return "a producer notice ended the run"
	}
	errored.Error = turnFailedArm("unset")
	return "the run ended on a failure with no stated cause"
}

// turnFailedArm keeps an unclassified producer terminal BY ITS VENDOR WORD
// under turn_failed, which is the arm feed.proto gives every abnormal end it
// draws no dedicated arm for.
func turnFailedArm(reason string) *frontendv1.FeedTurnEndedErrored_TurnFailed {
	return &frontendv1.FeedTurnEndedErrored_TurnFailed{
		TurnFailed: &frontendv1.FailureVendorTurnFailed{
			Vendor:     vendorFailureContext(),
			StopReason: reason,
		},
	}
}

// vendorFailureContext is the vendor correlation the run's own terminals carry.
// PRESENT AND EMPTY on purpose: conversation.v1's AgentMaxTurnsReached,
// AgentBudgetExhausted, AgentExecutionError, AgentStructuredOutputRetriesExhausted
// and AgentStoppedByStopHook are all empty messages, and no vendor conversation,
// request or message id reaches this seam — which failure.proto states is exactly
// what an empty field means, rather than a figure invented here.
func vendorFailureContext() *frontendv1.VendorFailureContext {
	return &frontendv1.VendorFailureContext{}
}

// drawQueryDied draws the turn's terminal for a query that died out from under
// it. DUPLICATED ON PURPOSE upstream: a consumer with no stream open still
// needs to know the session is over.
func (r *resolver) drawQueryDied(s *wsState, died *conversationv1.SessionQueryDied) {
	log := r.logger(s.id)
	if s.turnInFlight == nil {
		log.Debug("daemon.feed.query_died_without_turn",
			"the query died with no turn in flight; no terminal row is owed",
			dlog.Context{"cause": queryDeathWord(died)})
		return
	}
	turn := string(*s.turnInFlight)
	at := r.outputPlacement(s)

	errored := queryDiedErrored(died)

	row := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindTurnEnded, ID: turn}),
		Turn: &conversationv1.TurnId{Value: turn},
		Row: &frontendv1.FeedRow_TurnEnded{TurnEnded: &frontendv1.FeedTurnEnded{
			EndedAtMs: r.deps.Now().UnixMilli(),
			Outcome:   &frontendv1.FeedTurnEnded_Errored{Errored: errored},
		}},
	}
	log.Warn("daemon.feed.query_died",
		"the query died out from under the turn; the turn's terminal row was drawn",
		dlog.Context{"turn": turn, "cause": queryDeathWord(died)})
	r.upsert(s, at, row, true)
	r.settleTurnPrompts(s, ids.TurnID(turn))
	r.breakPlanEpisodes(s, "the query died while plan mode was still open")
	s.turnInFlight = nil
}

// queryDiedErrored is THE drawing of a query death, shared by both of its
// statements: the session's query_died push and the turn terminal's own
// query_died arm. One builder is what makes the drawn row independent of which
// of them reached the resolver first.
func queryDiedErrored(died *conversationv1.SessionQueryDied) *frontendv1.FeedTurnEndedErrored {
	errored := &frontendv1.FeedTurnEndedErrored{Error: queryDiedArm(died)}
	applyHeadline(errored, headline{Text: queryDeathSentence(died)}, queryDeathDetail(died))
	return errored
}

// diedOfQuery reports whether a turn's failure terminal is a query death.
func diedOfQuery(failure *conversationv1.AgentFailure) bool {
	_, ok := failure.GetFailure().(*conversationv1.AgentFailure_QueryDied)
	return ok
}

// queryDiedArm builds feed.proto's query-died arm, carrying the cause through
// from SessionQueryDied's own. The two vocabularies are deliberately parallel:
// an EOF is the agent binary vanishing and an iterator failure is the SDK
// throwing, and a reader who cannot tell them apart cannot tell a crashed
// producer from a broken one.
func queryDiedArm(died *conversationv1.SessionQueryDied) *frontendv1.FeedTurnEndedErrored_QueryDied {
	arm := &frontendv1.FeedTurnErrorQueryDied{}
	switch died.GetCause().(type) {
	case *conversationv1.SessionQueryDied_UnexpectedEof:
		arm.Cause = &frontendv1.FeedTurnErrorQueryDied_UnexpectedEof{
			UnexpectedEof: &frontendv1.FeedTurnErrorQueryUnexpectedEof{},
		}
	case *conversationv1.SessionQueryDied_IteratorFailure:
		arm.Cause = &frontendv1.FeedTurnErrorQueryDied_IteratorFailure{
			IteratorFailure: &frontendv1.FeedTurnErrorQueryIteratorFailure{},
		}
	}
	// NO DEFAULT ARM: a death the producer stated no cause for leaves the
	// oneof unset, which is the honest reading -- inventing one would claim
	// the producer said something it did not.
	return &frontendv1.FeedTurnEndedErrored_QueryDied{QueryDied: arm}
}

// queryDeathSentence words a query death for the headline.
func queryDeathSentence(died *conversationv1.SessionQueryDied) string {
	switch died.GetCause().(type) {
	case *conversationv1.SessionQueryDied_UnexpectedEof:
		return "the query died — the agent binary's stream ended without closing"
	case *conversationv1.SessionQueryDied_IteratorFailure:
		return "the query died — the SDK's iterator threw"
	}
	return "the query died out from under the turn"
}

// queryDeathDetail is the thrown cause, when the producer gave one. A query
// death otherwise carries NO vendor wording, and the arm still names it.
func queryDeathDetail(died *conversationv1.SessionQueryDied) string {
	if iterator, ok := died.GetCause().(*conversationv1.SessionQueryDied_IteratorFailure); ok {
		return iterator.IteratorFailure.GetCause()
	}
	return ""
}

// queryDeathWord names the cause for a log record.
func queryDeathWord(died *conversationv1.SessionQueryDied) string {
	switch died.GetCause().(type) {
	case *conversationv1.SessionQueryDied_UnexpectedEof:
		return "unexpected_eof"
	case *conversationv1.SessionQueryDied_IteratorFailure:
		return "iterator_failure"
	}
	return "unset"
}
