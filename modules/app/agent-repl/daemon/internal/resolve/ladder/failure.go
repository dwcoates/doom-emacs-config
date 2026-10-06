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

// RateLimitBlocks reports whether a rate-limit verdict leaves the session
// vendor-blocked: only a REJECTED one does, and any other verdict lifts a
// block that stands. Both resolvers apply it to the same event, so the strip
// and the dot move together.
func RateLimitBlocks(status *conversationv1.SessionRateLimitStatus) bool {
	_, rejected := status.GetStatus().(*conversationv1.SessionRateLimitStatus_Rejected)
	return rejected
}
