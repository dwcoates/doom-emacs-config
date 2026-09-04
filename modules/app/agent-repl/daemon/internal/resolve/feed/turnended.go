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
	at := r.place(s, agent)

	if turn == nil {
		// A SUBAGENT's stream ending is its bubble's business, not a turn
		// terminal: the bubble already settles from the spawn unit's own
		// frames, and a second terminal row would claim the turn ended.
		log.Debug("daemon.feed.agent_terminal_without_turn",
			"an agent stream ended outside a turn; its bubble carries the ending",
			dlog.Context{"agent": agent.GetValue()})
		return
	}

	turnID := &conversationv1.TurnId{Value: string(*turn)}
	ended := &frontendv1.FeedTurnEnded{EndedAtMs: r.deps.Now().UnixMilli()}

	switch {
	case success != nil:
		r.concludedOutcome(s, success)(ended)
	case failure != nil:
		ended.Outcome = &frontendv1.FeedTurnEnded_Errored{Errored: r.erroredOutcome(s, string(*turn), failure)}
	default:
		log.Error("daemon.feed.terminal_without_outcome",
			"an agent terminal carried neither success nor failure",
			dlog.Context{"agent": agent.GetValue(), "turn": string(*turn)})
		return
	}

	// A turn that ended still in plan mode BROKE the episode.
	r.breakPlanEpisodes(s, "the turn ended while plan mode was still open")

	row := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindTurnEnded, ID: string(*turn)}),
		Turn: turnID,
		Row:  &frontendv1.FeedRow_TurnEnded{TurnEnded: ended},
	}
	log.Debug("daemon.feed.turn_ended",
		"a turn's terminal row was upserted",
		dlog.Context{"turn": string(*turn), "outcome": terminalArm(ended)})
	r.upsert(s, at, row, true)

	// A DETACHMENT THAT NEVER FOUND ITS UNIT is a producer fault, and the turn
	// ending is the last moment it could still have been claimed.
	for unit := range s.detachedUnits {
		log.Warn("daemon.feed.detached_unknown_unit",
			"work detached from a unit this resolver never drew",
			dlog.Context{"unit": unit, "turn": string(*turn)})
		delete(s.detachedUnits, unit)
	}

	delete(s.turnEvidence, string(*turn))
	delete(s.turnRefusals, string(*turn))
	delete(s.turnQueryDeaths, string(*turn))
	if s.turnInFlight != nil && *s.turnInFlight == *turn {
		s.turnInFlight = nil
	}
	if s.turnStamp != nil && *s.turnStamp == *turn {
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
func (r *resolver) concludedOutcome(s *wsState, success *conversationv1.AgentSuccess) turnOutcome {
	switch outcome := success.GetOutcome().(type) {
	case *conversationv1.AgentSuccess_Completed:
		concluded := &frontendv1.FeedTurnEndedConcluded{}
		// THE ANSWERING RESPONSE, named by the producer rather than derived
		// from position. Absence draws no final-answer border anywhere.
		if answer := outcome.Completed.GetAnswer().GetValue(); answer != "" {
			if id, ok := s.answerRows[answer]; ok {
				concluded.Answer = id
			}
		}
		return concludedArm(concluded)
	case *conversationv1.AgentSuccess_Interrupted:
		return func(ended *frontendv1.FeedTurnEnded) {
			ended.Outcome = &frontendv1.FeedTurnEnded_Interrupted{
				Interrupted: &frontendv1.FeedTurnEndedInterrupted{},
			}
		}
	case *conversationv1.AgentSuccess_Backgrounded:
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

// erroredOutcome respells an agent failure into the feed's drawn taxonomy, and
// composes the headline the client draws verbatim.
func (r *resolver) erroredOutcome(s *wsState, turn string, failure *conversationv1.AgentFailure) *frontendv1.FeedTurnEndedErrored {
	errored := &frontendv1.FeedTurnEndedErrored{}
	var (
		vendorMessage string
		sentence      string
	)

	// A DEAD QUERY OUTRANKS THE TERMINAL THE SHIM OWED FOR IT. The shim
	// concludes an open turn with AgentFailure.execution_error when the query
	// dies under it, because conversation.v1 gives a dead query no failure arm
	// of its own -- but the session already stated the death, feed.proto has
	// the arm for it, and drawing "the run broke while executing" over it
	// loses the one fact that says the producer is gone.
	if died, ok := s.turnQueryDeaths[turn]; ok {
		errored.Error = queryDiedArm(died)
		applyHeadline(errored, headline{Text: queryDeathSentence(died)},
			strings.Join(failure.GetErrors(), "; "))
		return errored
	}

	if api, ok := failure.GetFailure().(*conversationv1.AgentFailure_ApiRequestFailed); ok {
		vendorMessage = api.ApiRequestFailed.GetMessage()
		sentence = apiErrorArm(errored, api.ApiRequestFailed)
	} else {
		sentence = producerErrorArm(errored, failure, s.turnRefusals[turn])
		vendorMessage = strings.Join(failure.GetErrors(), "; ")
	}

	// The turn's own EVIDENCE — a mid-turn api error that was retried, a
	// compaction that failed — rides the headline, because the turn's terminal
	// is the one place a reader is looking when they ask what went wrong.
	if evidence := s.turnEvidence[turn]; len(evidence) > 0 {
		sentence = sentence + " (" + strings.Join(evidence, "; ") + ")"
	}
	applyHeadline(errored, headline{Text: sentence}, vendorMessage)
	return errored
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
	at := r.place(s, nil)

	s.turnQueryDeaths[turn] = died

	errored := &frontendv1.FeedTurnEndedErrored{Error: queryDiedArm(died)}
	applyHeadline(errored, headline{Text: queryDeathSentence(died)}, queryDeathDetail(died))

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
	r.breakPlanEpisodes(s, "the query died while plan mode was still open")
	s.turnInFlight = nil
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
