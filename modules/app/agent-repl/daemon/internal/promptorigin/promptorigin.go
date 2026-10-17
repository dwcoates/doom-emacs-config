// Package promptorigin is what a prompt's origin decides about whether the
// prompt is DRAWN, shared by the two resolvers that draw a prompt: the feed's
// prompt rows (resolve/feed) and the held section's cards (resolve/holds). It
// is a leaf, so both read the one answer.
package promptorigin

import conversationv1 "agentrepl/proto/conversation/v1"

// Unseen reports whether a prompt of ORIGIN is drawn NOWHERE in the webapp:
// no prompt row in the feed, and no held card while it waits behind a
// running turn. Such a prompt is the daemon's word to the agent alone, which
// the user neither said nor needs to read: a merge queue subscription's
// notice, a workspace subscription's notice, and the brief notice that a
// workspace derived from this one was created, landed or closed. The agent's
// reply to it is drawn as any reply is.
func Unseen(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE:
		return true
	}
	return false
}

// UnseenNamed is Unseen for an origin spelled as conversation.v1.PromptOrigin
// names it, as a durable hold stores it. A name the vocabulary does not hold
// is not unseen.
func UnseenNamed(origin string) bool {
	return Unseen(conversationv1.PromptOrigin(conversationv1.PromptOrigin_value[origin]))
}
