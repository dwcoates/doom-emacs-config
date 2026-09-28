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
	// VendorBlocked is the vendor or the account refusing the session: the
	// blocked rung, drawn `vendor_blocked` on the roster and `blocked` on
	// the footer, both blue.
	VendorBlocked
	// TurnFailed is the turn's own failure: a turn end on the idle rung,
	// drawn `turn_failed` (blue) on the roster and `idle · turn_failed` on
	// the footer.
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
// VENDOR_BLOCKED IS ONLY FOR THE VENDOR OR THE ACCOUNT (owner ruling,
// 2026-09-28): an api request the vendor refused or failed (every kind), an
// account blocking limit, a rapid-refill breaker, and a model error. Every
// other failure is the turn's own — including `budget_exhausted`, which is the
// run's configured ceiling rather than the account's, `query_died`, the lost
// arm, and any arm a later contract adds, by default. Two stops are EXPECTED
// and await the human rather than failing: a Stop hook that forbade
// continuing (a deliberate configured stop) and a tool the run deferred.
func ClassifyFailure(failure *conversationv1.AgentFailure) FailureClass {
	if failure == nil {
		return NoFailure
	}
	switch failure.GetFailure().(type) {
	case *conversationv1.AgentFailure_ApiRequestFailed,
		*conversationv1.AgentFailure_BlockingLimit,
		*conversationv1.AgentFailure_RapidRefillBreaker,
		*conversationv1.AgentFailure_ModelError:
		return VendorBlocked
	case *conversationv1.AgentFailure_StopHookPrevented,
		*conversationv1.AgentFailure_ToolDeferred:
		return ExpectedStop
	default:
		return TurnFailed
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
