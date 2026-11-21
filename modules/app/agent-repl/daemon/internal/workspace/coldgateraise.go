package workspace

import "fmt"

// ColdGateRaise is why a workspace's cold gate stands at a raise.
//
// IT IS WHAT KEEPS AN AUTOMATIC ANSWER TO ONE. The merge queue and the
// standing deploy checks answer a gate themselves the moment it is raised
// (owner rulings, 2026-11-17 and 2026-11-21), and an answer whose re-open
// fails stands the gate again (Fleet.ReraiseColdGate). Read as a new raise,
// that stand was answered again, failed again and stood again, every 10s for
// as long as the cause lasted (2026-11-21: 232 cycles in 40 minutes on
// feed-follow-only-at-tail). A raise after a failed answer is told apart, so
// an automatic answerer never answers it: the gate stays standing for the
// user, and the failure that stood it is already on the log.
type ColdGateRaise int

const (
	// RaiseJudged is a gate a shim judged just now.
	RaiseJudged ColdGateRaise = iota + 1
	// RaiseStanding is a gate already standing, raised again on this daemon:
	// from its record at a bring-up, or carried over a handover.
	RaiseStanding
	// RaiseAfterFailedAnswer is a gate an answer was spent on, stood again
	// because the answer was refused or its re-open failed.
	RaiseAfterFailedAnswer
)

// Judged answers whether the raise is a gate a shim judged just now: the one
// raise the telemetry counts and the workspace's followers are told of.
func (r ColdGateRaise) Judged() bool { return r == RaiseJudged }

// AnsweredAutomatically answers whether an automatic answerer (the merge
// queue, a standing deploy check) answers the gate at this raise. A gate an
// answer just failed on is never answered again by one.
func (r ColdGateRaise) AnsweredAutomatically() bool {
	switch r {
	case RaiseJudged, RaiseStanding:
		return true
	case RaiseAfterFailedAnswer:
		return false
	default:
		panic(fmt.Sprintf("workspace: cold gate raise %d names no raise", int(r)))
	}
}

// String names the raise for a log record.
func (r ColdGateRaise) String() string {
	switch r {
	case RaiseJudged:
		return "judged"
	case RaiseStanding:
		return "standing"
	case RaiseAfterFailedAnswer:
		return "after_failed_answer"
	default:
		return fmt.Sprintf("raise(%d)", int(r))
	}
}
