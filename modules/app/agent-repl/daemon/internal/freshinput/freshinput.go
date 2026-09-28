// Package freshinput defines FRESH INPUT, the one token quantity every
// per-turn spend figure the daemon draws counts: the footer's tokens cell, a
// response bubble's cost corner and a subagent card's figure. ONE DEFINITION,
// daemon-wide, because two resolvers summing "the same" figure from two
// spellings of it would eventually disagree about a turn the owner reads in
// both places.
//
// Fresh input is every input token of an API response that was NOT a cache
// hit: the vendor's `input_tokens` plus `cache_creation_input_tokens`, which
// the wire carries as `conversation.v1.TokenCacheMisses` (unwritten plus
// written). Cache reads are excluded (cheap context reuse); cache writes are
// included (the expensive part of a turn, and the whole prefix again on a
// cold cache); output is excluded (it is re-sent as input on the next request,
// where it is counted once).
package freshinput

import conversationv1 "agentrepl/proto/conversation/v1"

// Of answers one API response's fresh input. A nil usage is zero: it states
// nothing, so it adds nothing.
func Of(u *conversationv1.TokenUsage) uint64 {
	misses := u.GetInputMisses()
	return misses.GetWritten() + misses.GetUnwritten()
}
