// Package promptorigin is what a prompt's origin decides: whether the USER
// sent it, which takes a one-shot back (UserSent); whether the prompt
// is DRAWN, shared by the two resolvers that draw a prompt (the feed's prompt
// rows in resolve/feed and the held section's cards in resolve/holds), and
// whether it NAMES ITS SENDER, shared by every layer that carries a prompt's
// sender (the rpc, the prompt handler, the queue, the store and the feed). It
// is a leaf, so every one of them reads the one answer.
package promptorigin

import (
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// ErrSenderMismatch is a prompt whose sender disagrees with its origin: a
// sender on a prompt no agent sent, or an agent-sent prompt naming none.
var ErrSenderMismatch = errors.New("a prompt names its sender exactly when its origin is PROMPT_ORIGIN_AGENT_SENT")

// CheckSender answers ErrSenderMismatch, wrapped with ORIGIN, when whether a
// prompt NAMED its sender disagrees with ORIGIN: a prompt of origin
// PROMPT_ORIGIN_AGENT_SENT names the workspace whose agent sent it
// (agentrepl.v1.SubmitPromptRequest.sent_by), and no other prompt names one.
func CheckSender(origin conversationv1.PromptOrigin, named bool) error {
	if (origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT) != named {
		return fmt.Errorf("%w (origin %s, sender named: %t)", ErrSenderMismatch, origin, named)
	}
	return nil
}

// CheckSenderNamed is CheckSender for an origin spelled as
// conversation.v1.PromptOrigin names it, as the store records it.
func CheckSenderNamed(origin string, named bool) error {
	return CheckSender(conversationv1.PromptOrigin(conversationv1.PromptOrigin_value[origin]), named)
}

// Unseen reports whether a prompt of ORIGIN is drawn NOWHERE in the webapp:
// no prompt row in the feed, and no held card while it waits behind a
// running turn. Such a prompt is the daemon's word to the agent alone, which
// the user neither said nor needs to read: a merge queue subscription's
// notice, a workspace subscription's notice, the brief notice that a
// workspace derived from this one was created, landed or closed, and the
// brief asking a workspace that left its one-shot state to file itself into
// a subsection. The agent's reply to it is drawn as any reply is.
func Unseen(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT:
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

// UserSent reports whether a prompt of ORIGIN is the user's own, typed into
// the editor or the webapp: the user stepping into the workspace, which takes
// a one-shot back from its agent (workspace.Verbs.TakeBackOneShot). A prompt
// another workspace's agent sent, or one the daemon or a program submits on
// the user's behalf, is not.
func UserSent(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_AND_HIDE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_POSTFIX,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_PREFIX,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT:
		return true
	}
	return false
}
