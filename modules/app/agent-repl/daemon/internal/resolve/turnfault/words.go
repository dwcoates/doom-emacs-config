// Package turnfault is THE ONE PLACE a failed turn is put into words.
//
// A turn that ends abnormally is told on three surfaces: the feed's turn-end
// row (its headline, and the outcome marker the client draws), the footer's
// activity cell (the turn fault's `turn_ended` line, owner ruling 2026-10-06),
// and the desktop banner. The owner's ruling is that the footer carries "the
// daemon's existing per-cause headline sentence", so the sentence must be ONE
// sentence, composed once, from the same facts — a footer that worded a cause
// differently from the feed's row beside it would be two accounts of one end.
//
// The package composes words only. Which DOMAIN a failure belongs to (vendor,
// agent-repl, none) is resolve/ladder's (ClassifyFailure, ResolveTurnFault),
// and which feed arm draws it is resolve/feed's.
package turnfault

import (
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/wsm"
)

// Words is how one failed turn is told.
type Words struct {
	// Sentence is the per-cause headline sentence: the feed row's headline
	// (before any evidence the feed adds) and the footer's turn-fault line.
	Sentence string
	// Cause is the cause's own word, verbatim: the vendor's api error class
	// ("rate_limited", or an unmodeled class by the vendor's name), or the
	// run's stop word ("max_turns", "prompt_too_long", "lost:went_silent",
	// "closed:orphaned"). It is the feed's `turn_failed` stop reason and the
	// outcome marker's error type.
	Cause string
	// Detail is the outcome marker's short detail after its label: the cause
	// read aloud ("rate limited", "process died").
	Detail string
}

// readAloud turns a cause word into a marker detail: the schema's underscores
// become spaces, the same rule every status and substatus cell follows.
func readAloud(cause string) string {
	return strings.ReplaceAll(cause, "_", " ")
}

// words builds Words whose detail is the cause read aloud.
func words(sentence, cause string) Words {
	return Words{Sentence: sentence, Cause: cause, Detail: readAloud(cause)}
}

// OfAgentFailure words a turn-ending agent failure. REFUSED reports that the
// turn's own response frame witnessed a refusal: a model error and a refusal
// arrive as the same terminal (conversationv1.AgentModelError is empty), and
// the response frame is the refusal's only witness.
func OfAgentFailure(failure *conversationv1.AgentFailure, refused bool) Words {
	switch item := failure.GetFailure().(type) {
	case *conversationv1.AgentFailure_ApiRequestFailed:
		return OfApiFailure(item.ApiRequestFailed)
	case *conversationv1.AgentFailure_QueryDied:
		return OfQueryDeath(item.QueryDied)
	case *conversationv1.AgentFailure_Lost:
		return ofLost(item.Lost)
	case *conversationv1.AgentFailure_PromptTooLong:
		return words("the prompt was too long to send — the context must be cut first", "prompt_too_long")
	case *conversationv1.AgentFailure_BlockingLimit:
		return words("an account-level block stopped the run", "blocking_limit")
	case *conversationv1.AgentFailure_RapidRefillBreaker:
		return words("the account's refill-rate breaker tripped — this is a wait, not a fault", "rapid_refill_breaker")
	case *conversationv1.AgentFailure_ImageError:
		return words("an image in the request could not be processed", "image_error")
	case *conversationv1.AgentFailure_ModelError:
		if refused {
			return words("the model refused to continue — there is no answer", "refusal")
		}
		return words("the model errored in a way the API did not classify", "model_error")
	case *conversationv1.AgentFailure_MalformedToolUseExhausted:
		return words("the model's tool calls could not be parsed and the attempts ran out", "malformed_tool_use_exhausted")
	case *conversationv1.AgentFailure_StopHookPrevented:
		return Words{Sentence: "a Stop hook ended the run", Cause: "stop_hook_prevented", Detail: "Stop hook ended the run"}
	case *conversationv1.AgentFailure_HookStopped:
		return words("a hook ended the run", "hook_stopped")
	case *conversationv1.AgentFailure_ToolDeferred:
		return words("the run ended waiting on a deferred tool call", "tool_deferred")
	case *conversationv1.AgentFailure_ToolDeferredUnavailable:
		return words("the run ended on a tool call deferred to something unavailable", "tool_deferred_unavailable")
	case *conversationv1.AgentFailure_MaxTurns:
		return words("stopped at the turn limit", "max_turns")
	case *conversationv1.AgentFailure_BudgetExhausted:
		return words("stopped at the budget", "max_budget")
	case *conversationv1.AgentFailure_StructuredOutputRetryExhausted:
		return words("the run ended: structured_output_retry_exhausted", "structured_output_retry_exhausted")
	case *conversationv1.AgentFailure_TurnSetupFailed:
		return words("the run could not be set up and never reached the model", "turn_setup_failed")
	case *conversationv1.AgentFailure_ExecutionError:
		return words("the run broke while executing", "execution_error")
	case *conversationv1.AgentFailure_ContinuationPrevented:
		return words("a producer notice ended the run", "continuation_prevented")
	}
	return words("the run ended on a failure with no stated cause", "unset")
}

// OfApiFailure words a turn that ended on a failed vendor api request, by the
// vendor's own error taxonomy.
func OfApiFailure(failed *conversationv1.ApiRequestFailed) Words {
	switch kind := failed.GetKind().(type) {
	case *conversationv1.ApiRequestFailed_RateLimited:
		return words("rate limited by the vendor", "rate_limited")
	case *conversationv1.ApiRequestFailed_Overloaded:
		return words("the vendor API is overloaded", "overloaded")
	case *conversationv1.ApiRequestFailed_AuthenticationFailed:
		return words("the credential was rejected — sign in again", "authentication_failed")
	case *conversationv1.ApiRequestFailed_PermissionDenied:
		return words("the credential lacks permission for this request", "permission_denied")
	case *conversationv1.ApiRequestFailed_InvalidRequest:
		return words("the vendor refused the request as malformed", "invalid_request")
	case *conversationv1.ApiRequestFailed_RequestTooLarge:
		return words("the request exceeded the vendor's size limit", "request_too_large")
	case *conversationv1.ApiRequestFailed_NotFound:
		return words("the model or resource does not exist", "not_found")
	case *conversationv1.ApiRequestFailed_Internal:
		return words("the vendor API hit its own internal error", "internal")
	case *conversationv1.ApiRequestFailed_BillingError:
		return words("the account could not be charged — check your billing", "billing_error")
	case *conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		return words("your organization does not allow this OAuth access", "oauth_org_not_allowed")
	case *conversationv1.ApiRequestFailed_MaxOutputTokens:
		return words("the request asked for more output than the model will produce", "max_output_tokens")
	case *conversationv1.ApiRequestFailed_Unmodeled:
		// THE VENDOR'S OWN CLASS NAME is the cause, verbatim: it is the only
		// handle the reader has on what happened.
		return Words{
			Sentence: "the vendor reported an error class we do not model yet",
			Cause:    kind.Unmodeled.GetType(),
			Detail:   kind.Unmodeled.GetType(),
		}
	}
	return words("the vendor reported a failure with no stated class", "unset")
}

// OfQueryDeath words a vendor query that died under its turn.
func OfQueryDeath(died *conversationv1.SessionQueryDied) Words {
	switch died.GetCause().(type) {
	case *conversationv1.SessionQueryDied_UnexpectedEof:
		return Words{Sentence: "the query died — the agent binary's stream ended without closing", Cause: "query_died", Detail: "query died"}
	case *conversationv1.SessionQueryDied_IteratorFailure:
		return Words{Sentence: "the query died — the SDK's iterator threw", Cause: "query_died", Detail: "query died"}
	}
	return Words{Sentence: "the query died out from under the turn", Cause: "query_died", Detail: "query died"}
}

// QueryDeathThrown is what the SDK threw when the query died of an iterator
// failure, verbatim, and "" when it died any other way or threw nothing.
func QueryDeathThrown(died *conversationv1.SessionQueryDied) string {
	if iterator, ok := died.GetCause().(*conversationv1.SessionQueryDied_IteratorFailure); ok {
		return iterator.IteratorFailure.GetCause()
	}
	return ""
}

// ofLost words a run we stopped being able to see. WE STOPPED BEING ABLE TO SEE
// IT is the whole claim; nothing here says the work failed.
func ofLost(lost *conversationv1.DetachedLost) Words {
	switch lost.GetHow().(type) {
	case *conversationv1.DetachedLost_FileVanished:
		return words("we lost sight of this work — its transcript disappeared from disk", "lost:file_vanished")
	case *conversationv1.DetachedLost_WentSilent:
		return words("we lost sight of this work — it went silent past the reader's ruling", "lost:went_silent")
	case *conversationv1.DetachedLost_SweptUp:
		return words("we lost sight of this work — a boot sweep found it open with no living producer", "lost:swept_up")
	}
	// A LOST ARM THAT NAMES NO HOW is no lost cause at all: the run ended on
	// a failure it stated no cause for.
	return words("the run ended on a failure with no stated cause", "unset")
}

// OfClose words a turn whose durable close is all there is to tell it by: no
// terminal of its own explained it. It reports false for a close that is no
// failure (a completion, an interrupt, a folded prompt).
func OfClose(how wsm.TurnClose) (Words, bool) {
	switch how {
	case wsm.CloseAgentDied:
		return Words{
			Sentence: "the agent process died, and the turn it was running ended with it",
			Cause:    "agent_process_died",
			Detail:   "process died",
		}, true
	case wsm.CloseOrphaned:
		return words("the turn was dropped: nothing saw it end", "closed:orphaned"), true
	case wsm.CloseFailed:
		return words("the turn failed with an error, and no account of the failure was recorded", "closed:failed"), true
	case wsm.CloseCompleted, wsm.CloseKilled, wsm.CloseFolded:
		return Words{}, false
	}
	return Words{}, false
}
