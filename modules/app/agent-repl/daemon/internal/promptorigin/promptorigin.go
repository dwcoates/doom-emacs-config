// Package promptorigin is what a prompt's origin decides: whether the USER
// sent it, which takes a one-shot back (UserSent); whether its turn is one the
// shim ADOPTED, which no queue delivered (Adopted); whether it is one of the
// daemon's NOTICES (DaemonNotice), shared by the feed, the held section and
// the queue, and
// whether it is DELIVERED ON ARRIVAL, never held (DeliveredOnArrival),
// whether it NAMES ITS SENDER, shared by every layer that carries a prompt's
// sender (the rpc, the prompt handler, the queue, the store and the feed). It
// is a leaf, so every one of them reads the one answer.
package promptorigin

import (
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// Named answers the origin spelled as conversation.v1.PromptOrigin names it,
// as the store records it: the ONE reading of a stored origin. A name the
// vocabulary does not hold reads as PROMPT_ORIGIN_UNSPECIFIED.
func Named(origin string) conversationv1.PromptOrigin {
	return conversationv1.PromptOrigin(conversationv1.PromptOrigin_value[origin])
}

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
	return CheckSender(Named(origin), named)
}

// DaemonNotice reports whether a prompt of ORIGIN is one of the daemon's
// NOTICES to a workspace's agent: a merge queue subscription's notice, a
// workspace subscription's notice, the brief notice that a workspace derived
// from this one was created, landed or closed, a GitHub pull request's,
// GitHub merge queue's or GitHub workflow run's notice to the workspace
// following it, and the brief asking a workspace that left its one-shot state
// to file itself into a subsection.
//
// A NOTICE IS DRAWN IN THE FEED (owner ruling, 2026-11-21, reversing the
// earlier rule that it was drawn nowhere): as a prompt the daemon sent, headed
// "Zettel <subsystem>" (frontend.v1.FeedUserPromptDaemonSender). It is
// delivered on arrival, so it never waits as a held card, and it stands
// outside the line the user's held prompts are judged and folded in
// (holdfold.Seen); a turn it causes ends with no desktop banner.
func DaemonNotice(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_PULL_REQUEST_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_WORKFLOW_RUN_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT:
		return true
	}
	return false
}

// DaemonNoticeNamed is DaemonNotice for an origin spelled as conversation.v1.PromptOrigin
// names it, as a durable hold stores it. A name the vocabulary does not hold
// is not a notice.
func DaemonNoticeNamed(origin string) bool {
	return DaemonNotice(Named(origin))
}

// DeliveredOnArrival reports whether a prompt of ORIGIN is delivered the
// moment it arrives and is never held (owner ruling, 2026-11-21): every
// daemon notice to an agent, and every prompt one workspace's agent sent
// another (`agent-repld workspace prompt`). Into an idle session it starts a
// turn; into a running turn it joins after the turn's current tool call. One
// that genuinely cannot go yet waits under a NAMED wait the event it waits on
// ends (wsm.ArrivalWait), never as a held prompt.
//
// The notices are: a merge queue's, a workspace subscription's, a derived
// workspace's, a GitHub pull request's and a GitHub merge queue's notice, an
// async merge's handback, a retarget's notice, a failed create's report to
// the main workspace, the main checkout's sync repair, an unresolved link's
// question, a GitHub workflow run's notice, and the retired
// subsection-placement brief. A merge's own briefs
// (its repairs, its actions, its hooks, its revert) drive the merge's work
// and are not notices, nor is a deploy mediator's brief, which drives its own
// hidden workspace's work.
func DeliveredOnArrival(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_PULL_REQUEST_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_RETARGETED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATE_FAILED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MAIN_SYNC_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_LINK_UNRESOLVED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_WORKFLOW_RUN_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT:
		return true
	}
	return false
}

// DeliveredOnArrivalNamed is DeliveredOnArrival for an origin spelled as
// conversation.v1.PromptOrigin names it, as the store records it.
func DeliveredOnArrivalNamed(origin string) bool {
	return DeliveredOnArrival(Named(origin))
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

// Adopted reports whether a prompt of ORIGIN opens a turn the SHIM adopted:
// one no queue delivered, so its prompt row, written as the turn's first, is
// the only statement that the turn opened. Two origins are: a turn the vendor
// started on its own (PROMPT_ORIGIN_VENDOR_STARTED), and one a prompt typed on
// claude.ai started through the session's Remote Control bridge
// (PROMPT_ORIGIN_REMOTE_CONTROL). Neither is ever submitted by a client.
//
// A REMOTE-CONTROL PROMPT IS THE USER'S WORDS, but not UserSent: that answers
// for a prompt this daemon's submit pipeline took from the editor or the
// webapp, and this one went to the vendor past it. The feed draws it as a
// prompt the user said (resolve/feed AuthorLabel).
func Adopted(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_REMOTE_CONTROL:
		return true
	}
	return false
}

// StartsAgentTurn reports whether a turn of ORIGIN set the agent working: a
// prompt the user sent (from the editor, the webapp or claude.ai), one another
// workspace's agent sent, a daemon brief or notice, or a turn the vendor
// started on its own. Two kinds of turn are not: the re-drive of a turn a
// restart or a merge interrupted, which continues work an earlier turn was
// asked for, and a compaction the daemon submitted, which only cuts the
// context. (A compaction or /clear the user TYPED carries a user origin; its
// text, not its origin, says it is no agent turn.)
//
// THE SWITCH NAMES EVERY ORIGIN, so a new one fails hard here until it is
// classified, rather than being counted or dropped by default.
// PROMPT_ORIGIN_UNSPECIFIED, which no submit site sends, fails hard too.
func StartsAgentTurn(origin conversationv1.PromptOrigin) bool {
	switch origin {
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_COMPACT_AFTER:
		return false
	case conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_AND_HIDE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_POSTFIX,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_PREFIX,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_METAPROMPT_READ,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_COMMAND_DIFF_ANALYSIS,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_COMMAND_EXPLAIN_CONTEXT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_COMMAND_EXPLAIN_PROMPT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_COMMAND_UPDATE_PR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_COMMAND_REBASE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_COMMAND_CREATE_OR_UPDATE_PR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_PANEL_SELECTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DEFERRED_PROMPT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_LEGACY_HOST_PROMPT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GNS_SOCKETS_CLOSE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_LEGACY_HOST_EVAL_RESULT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_EXPLAIN_CONFIG,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_CARD_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_LINK_UNRESOLVED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_RETARGETED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_PICK,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATE_FAILED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_COMMAND_TERMINAL_OUTPUT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_COMMAND_THREAD_BACKGROUND,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_MEDIATION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_FINISH_HOOK,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_MERGE_QUEUE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_GITHUB_WORKFLOW_RUN_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_PRODUCTION_MEDIATION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_SLACK_THREAD_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_OWNER_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_DEPLOY_HOOK,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_REMOTE_CONTROL,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MAIN_SYNC_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_PULL_REQUEST_NOTICE:
		return true
	}
	panic(fmt.Sprintf("promptorigin: origin %s is not classified as starting an agent turn or not", origin))
}
