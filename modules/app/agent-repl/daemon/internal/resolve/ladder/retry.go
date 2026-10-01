package ladder

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// A STANDING API RETRY IS A BLOCK (owner ruling, 2026-10-01). While the vendor
// retries a call that failed mid-turn the turn cannot advance, so the
// workspace is unusable and both surfaces claim the blocked rung: the footer
// as `blocked · api_retrying`, the roster as `api_retrying`. The retry stands
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
