package ladder

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// A STANDING API RETRY IS A VENDOR FAULT (owner ruling, 2026-10-02,
// superseding 2026-10-01's block). While the vendor retries a call that failed
// mid-turn the turn cannot advance, and both surfaces claim the vendor-fault
// rung, turquoise: the footer as `vendor_fault · api_retrying`, the roster as
// `api_retrying`. The retry stands
// from the failure the vendor reports until RetryAnswered says the retried
// agent was answered, the turn ends, or a new turn opens.

// RetryAnswered reports whether ACT is the vendor answering the agent whose
// call was being retried: a frame of its reasoning or prose, or any frame that
// carries an API response's usage. It is the ONE answer both resolvers end a
// retry on, so the footer and the roster cannot disagree about when the
// workspace stops being blocked.
func RetryAnswered(act *conversationv1.AgentActivity) bool {
	switch act.GetItem().(type) {
	case *conversationv1.AgentActivity_Thinking, *conversationv1.AgentActivity_Response:
		return true
	default:
		return act.GetUsage() != nil
	}
}

// RetryAnsweredBy reports whether ACT, a frame of AGENT's, ends the retry of
// RETRIED's call: the frame is the retried agent's own and it answers
// (RetryAnswered). Another agent's frame says nothing about this call. It is
// the ONE retry-end rule the footer and the roster both apply to every
// activity, so the two cannot leave `api_retrying` on different frames; each
// keeps its own record of whether a retry stands at all.
func RetryAnsweredBy(retried, agent string, act *conversationv1.AgentActivity) bool {
	return retried == agent && RetryAnswered(act)
}
