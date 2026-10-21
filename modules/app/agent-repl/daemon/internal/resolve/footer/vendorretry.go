package footer

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// AGENT-REPL'S RETRY OF A TURN THE VENDOR FAILED (owner ruling, 2026-10-20).
//
// The shim runs the vendor with its own API retries off and retries a turn
// that failed retryably itself, every 30 seconds or at an exhausted usage
// window's reset, stating the schedule whole on the session stream
// (`SessionUpdate.vendor_error_retry`). While a retry is scheduled the footer:
//
//   - draws the `retrying` line, counting down to the shim's instant, in
//     place of the failed turn's own sentence;
//   - stands as `vendor_fault · api_retrying` unless a standing account block
//     names a finer step (a usage limit keeps `usage_limit`, under the same
//     countdown);
//   - holds no prompt (vendorBlock): a prompt sent now IS the retry, sent at
//     once, and the shim's chain yields to it.
//
// The schedule is SESSION-scoped: a (re)started session states its own, so a
// start drops the one on hand (OnSessionStarted).

// observeVendorRetry takes the shim's stated schedule, whole. A scheduled
// retry naming no instant or no attempt is a producer defect: it is refused
// at ERROR and the schedule on hand stands, so no half-stated retry is drawn.
func (r *resolver) observeVendorRetry(ws ids.WorkspaceID, s *wsState, stated *conversationv1.SessionVendorErrorRetry) {
	// THE RETRY BEING MADE is drawn as "retrying now": the shim states it at
	// the code point that makes the retry, so the line says the attempt is
	// happening rather than holding a countdown at zero (owner ruling,
	// 2026-10-21). One naming no attempt is a producer defect, refused as a
	// scheduled one is.
	if attempting := stated.GetAttempting(); attempting != nil {
		if attempting.GetAttempt() == 0 {
			r.logOf(ws, s).Error("daemon.footer.vendor_retry_refused",
				"a vendor-error retry being made names no attempt; the statement is refused and the schedule on hand stands",
				dlog.Context{"invariant_violation": "a vendor-error retry being made names its attempt"})
			return
		}
		s.vendorRetry = &retryState{
			attempt: attemptOfRetry(attempting.GetAttempt()),
			status:  truncate(attempting.GetStatus(), DefaultWarningRowWidth),
			at:      r.opts.clock.Now(),
			now:     true,
		}
		r.logOf(ws, s).Info("daemon.footer.vendor_retry", "agent-repl is making its vendor-error retry now; the line says so",
			dlog.Context{"attempt": attempting.GetAttempt()})
		return
	}
	scheduled := stated.GetScheduled()
	if scheduled == nil {
		if s.vendorRetry != nil {
			r.logOf(ws, s).Info("daemon.footer.vendor_retry", "no vendor-error retry is scheduled any more; the countdown came down",
				dlog.Context{"attempt": s.vendorRetry.attempt})
		}
		s.vendorRetry = nil
		return
	}
	if scheduled.GetAttempt() == 0 || scheduled.GetNextAttemptAtMs() <= 0 {
		r.logOf(ws, s).Error("daemon.footer.vendor_retry_refused",
			"a scheduled vendor-error retry names no attempt or no instant; the statement is refused and the schedule on hand stands",
			dlog.Context{
				"attempt":             scheduled.GetAttempt(),
				"next_attempt_at_ms":  scheduled.GetNextAttemptAtMs(),
				"invariant_violation": "a scheduled vendor-error retry names its attempt and its instant",
			})
		return
	}
	nextAt := time.UnixMilli(scheduled.GetNextAttemptAtMs())
	s.vendorRetry = &retryState{
		attempt: attemptOfRetry(scheduled.GetAttempt()),
		status:  truncate(scheduled.GetStatus(), DefaultWarningRowWidth),
		at:      r.opts.clock.Now(),
		nextAt:  &nextAt,
	}
	r.logOf(ws, s).Info("daemon.footer.vendor_retry", "the footer counts down to agent-repl's scheduled vendor-error retry",
		dlog.Context{"attempt": scheduled.GetAttempt(), "next_attempt_at_ms": scheduled.GetNextAttemptAtMs()})
}

// attemptOfRetry is the footer's attempt number for the RETRY-th retry
// (1-based). THE FOOTER COUNTS ATTEMPTS, a retry schedule counts retries: the
// first retry is the second attempt. Both schedules the retrying line draws,
// the vendor's (`conversation.v1.ApiRetry.attempt`) and agent-repl's
// (`conversation.v1.SessionVendorErrorRetryScheduled.attempt`), count this
// way, so both convert here.
func attemptOfRetry(retry uint32) int32 {
	return int32(retry) + 1
}
