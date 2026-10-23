package ladder

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// FailureClass is what a turn-ending agent failure means for the workspace's
// status: which rung it leaves the workspace on, and how its turn end reads.
type FailureClass int

// The failure classes.
const (
	// NoFailure is a turn that did not fail.
	NoFailure FailureClass = iota
	// VendorBlocked is the vendor or the account refusing the session until
	// something outside it is resolved: drawn `vendor_blocked` on the roster
	// and `vendor_fault` with the block's own step (auth, usage limit,
	// billing, vendor error) on the footer, both turquoise.
	VendorBlocked
	// VendorFailed is a turn the vendor ENDED OR REFUSED for a reason that
	// does not block the session: a transient api error, a token or turn
	// ceiling, a refusal, an execution error, every other abnormal end the
	// run reported. Its turn raises a VENDOR TURN FAULT (ResolveTurnFault).
	VendorFailed
	// AgentReplFailed is a turn agent-repl's own machinery ended: the vendor
	// query died under it. Its turn raises an AGENT-REPL TURN FAULT.
	AgentReplFailed
	// NetworkFailed is a turn whose request never reached the API: the
	// connection failed below any answer (conversation.v1.ApiUnreachable).
	// Its turn raises a NETWORK TURN FAULT, which puts nothing on the status:
	// the session's own network fault, stood from the same failure, carries
	// it and clears the moment the API answers again.
	NetworkFailed
	// ExpectedStop is a stop someone configured or asked for, awaiting the
	// user: a turn end on the idle rung, drawn exactly as a completion
	// (`done`, green) and never as a failure.
	ExpectedStop
)

// String names a class, for the record.
func (c FailureClass) String() string {
	switch c {
	case NoFailure:
		return "none"
	case VendorBlocked:
		return "vendor_blocked"
	case VendorFailed:
		return "vendor_failed"
	case AgentReplFailed:
		return "agent_repl_failed"
	case NetworkFailed:
		return "network_failed"
	case ExpectedStop:
		return "expected_stop"
	default:
		return "unknown"
	}
}

// ClassifyFailure is THE ONE CLASSIFIER of a turn-ending agent failure, which
// the footer's `vendor_fault` arm, the roster's `vendor_blocked` and
// `turn_died` arms, the feed's outcome marker and (through the roster) the tab
// bar all consult, so no two surfaces can read the same failure differently.
//
// VENDOR_BLOCKED IS A VENDOR OR ACCOUNT BLOCK THAT STANDS UNTIL IT IS
// RESOLVED (owner rulings, 2026-09-28): an account blocking limit, a
// rapid-refill breaker, and an api request refused for a usage limit,
// authentication, a permission the key lacks, billing, or an organization the
// account may not use.
//
// AGENT_REPL_FAILED IS THE QUERY DYING under the turn (owner ruling,
// 2026-10-06): agent-repl's own machinery ended it, never the vendor.
//
// NETWORK_FAILED IS A REQUEST THAT NEVER REACHED THE API (owner ruling,
// 2026-10-21): the vendor neither ended nor refused it, so it is no vendor
// failure.
//
// EVERY OTHER FAILURE IS ONE THE VENDOR ENDED OR REFUSED (owner ruling,
// 2026-10-06): an overloaded or erroring api, a malformed or oversized
// request, a model error or refusal, `budget_exhausted` and `max_turns`, an
// execution error, every other stop the run reported, the lost arm, and any
// arm a later contract adds, by default. Two stops are EXPECTED and await the
// human rather than failing: a Stop hook that forbade continuing (a deliberate
// configured stop) and a tool the run deferred.
func ClassifyFailure(failure *conversationv1.AgentFailure) FailureClass {
	if failure == nil {
		return NoFailure
	}
	switch item := failure.GetFailure().(type) {
	case *conversationv1.AgentFailure_ApiRequestFailed:
		if ApiFailureBlocks(item.ApiRequestFailed) {
			return VendorBlocked
		}
		if _, unreachable := item.ApiRequestFailed.GetKind().(*conversationv1.ApiRequestFailed_Unreachable); unreachable {
			return NetworkFailed
		}
		return VendorFailed
	case *conversationv1.AgentFailure_BlockingLimit,
		*conversationv1.AgentFailure_RapidRefillBreaker:
		return VendorBlocked
	case *conversationv1.AgentFailure_StopHookPrevented,
		*conversationv1.AgentFailure_ToolDeferred:
		return ExpectedStop
	case *conversationv1.AgentFailure_QueryDied:
		return AgentReplFailed
	default:
		return VendorFailed
	}
}

// ApiFailureBlocks reports whether a failed api request blocks the workspace
// until something outside it is resolved: a usage limit, authentication, a
// permission the key lacks, billing, or an organization the account may not
// use. Every other kind — overloaded, internal, a malformed or oversized
// request, an unmodeled error, and any kind a later contract adds — is
// transient, and leaves the workspace usable.
func ApiFailureBlocks(failed *conversationv1.ApiRequestFailed) bool {
	switch failed.GetKind().(type) {
	case *conversationv1.ApiRequestFailed_RateLimited,
		*conversationv1.ApiRequestFailed_AuthenticationFailed,
		*conversationv1.ApiRequestFailed_PermissionDenied,
		*conversationv1.ApiRequestFailed_BillingError,
		*conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		return true
	default:
		return false
	}
}

// RateLimitWindow names the allowance window a rate-limit event is about,
// "unset" when the vendor named none. It is the key RateLimitRejections
// tracks each window under, and the name every log records.
func RateLimitWindow(status *conversationv1.SessionRateLimitStatus) string {
	switch status.GetRateLimitType().GetWindow().(type) {
	case *conversationv1.SessionRateLimitType_FiveHour:
		return "five_hour"
	case *conversationv1.SessionRateLimitType_SevenDay:
		return "seven_day"
	case *conversationv1.SessionRateLimitType_SevenDayOpus:
		return "seven_day_opus"
	case *conversationv1.SessionRateLimitType_SevenDaySonnet:
		return "seven_day_sonnet"
	case *conversationv1.SessionRateLimitType_SevenDayOverageIncluded:
		return "seven_day_overage_included"
	case *conversationv1.SessionRateLimitType_Overage:
		return "overage"
	default:
		return "unset"
	}
}

// RateLimitRejections is the set of allowance windows whose newest
// rate-limit verdict REJECTED the session. The vendor gives a verdict per
// window, so a verdict on one window says nothing about another: an allowed
// five-hour verdict never lifts a weekly rejection. The session is
// vendor-blocked by rate limits exactly while the set is non-empty. Both
// resolvers keep one per workspace and feed it the same events, so the strip
// and the dot move together. The zero value is an empty set.
type RateLimitRejections struct {
	rejected map[string]struct{}
}

// Observe files one rate-limit event under its window: a rejected verdict
// adds the window, any other verdict (or none) removes it. It reports whether
// any window still stands rejected afterwards.
func (r *RateLimitRejections) Observe(status *conversationv1.SessionRateLimitStatus) bool {
	window := RateLimitWindow(status)
	if _, rejected := status.GetStatus().(*conversationv1.SessionRateLimitStatus_Rejected); rejected {
		if r.rejected == nil {
			r.rejected = map[string]struct{}{}
		}
		r.rejected[window] = struct{}{}
	} else {
		delete(r.rejected, window)
	}
	return r.Blocked()
}

// Blocked reports whether any window's newest verdict rejected the session.
func (r *RateLimitRejections) Blocked() bool {
	return len(r.rejected) > 0
}

// Clear forgets every window's rejection, for the moments a resolver lifts
// the session's block wholesale (a fresh session, a completed sign-in).
func (r *RateLimitRejections) Clear() {
	r.rejected = nil
}

// ApiFailureCuredBySignIn reports whether a failed api request names an
// ACCOUNT CAUSE that signing in again cures: the credential rejected, or the
// organization the account may not use. It is the one definition every
// surface that offers or answers a sign-in reads — the feed's sign-in action,
// the footer's `auth` step, and the lift a completed sign-in makes on the
// feed, the footer and the roster — so no two can disagree about which
// failures a sign-in answers.
func ApiFailureCuredBySignIn(failed *conversationv1.ApiRequestFailed) bool {
	switch failed.GetKind().(type) {
	case *conversationv1.ApiRequestFailed_AuthenticationFailed,
		*conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		return true
	default:
		return false
	}
}

// FailureCuredBySignIn reports whether a turn-ending failure is a failed api
// request ApiFailureCuredBySignIn says a sign-in cures. Every other failure,
// and none, is not.
func FailureCuredBySignIn(failure *conversationv1.AgentFailure) bool {
	api, ok := failure.GetFailure().(*conversationv1.AgentFailure_ApiRequestFailed)
	return ok && ApiFailureCuredBySignIn(api.ApiRequestFailed)
}
