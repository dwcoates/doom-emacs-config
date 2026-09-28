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
	// something outside it is resolved: the blocked rung, drawn
	// `vendor_blocked` on the roster and `blocked` on the footer, both blue —
	// the workspace is unusable until then.
	VendorBlocked
	// TurnFailed is a failure the workspace survives: a turn end on the idle
	// rung, drawn `turn_failed` (turquoise) on the roster and on the footer.
	// The workspace stays usable, and the next prompt may well succeed.
	TurnFailed
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
	case TurnFailed:
		return "turn_failed"
	case ExpectedStop:
		return "expected_stop"
	default:
		return "unknown"
	}
}

// ClassifyFailure is THE ONE CLASSIFIER of a turn-ending agent failure, which
// the footer's `blocked` arm, the roster's `vendor_blocked` and `turn_failed`
// arms and (through the roster) the tab bar all consult, so no two surfaces
// can read the same failure differently.
//
// VENDOR_BLOCKED IS A VENDOR OR ACCOUNT BLOCK THAT MAKES THE WORKSPACE
// UNUSABLE UNTIL IT IS RESOLVED (owner rulings, 2026-09-28): an account
// blocking limit, a rapid-refill breaker, and an api request refused for a
// usage limit, authentication, a permission the key lacks, billing, or an
// organization the account may not use. A TRANSIENT vendor failure — an
// overloaded or erroring api, a malformed or oversized request, a model error
// — leaves the workspace usable, so it is the turn's own failure: the next
// prompt may well succeed. Every other failure is the turn's own too —
// including `budget_exhausted`, which is the run's configured ceiling rather
// than the account's, `query_died`, the lost arm, and any arm a later
// contract adds, by default. Two stops are EXPECTED and await the human
// rather than failing: a Stop hook that forbade continuing (a deliberate
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
		return TurnFailed
	case *conversationv1.AgentFailure_BlockingLimit,
		*conversationv1.AgentFailure_RapidRefillBreaker:
		return VendorBlocked
	case *conversationv1.AgentFailure_StopHookPrevented,
		*conversationv1.AgentFailure_ToolDeferred:
		return ExpectedStop
	default:
		return TurnFailed
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
