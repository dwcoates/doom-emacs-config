package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// forkConversation ports the half of the conversation the VENDOR TRANSCRIPT
// DOES NOT HOLD: the prompt rows the daemon itself records.
//
// A fork ports the WHOLE conversation under the child's identities. The
// transcript carries what the agent said, and its assistant lines reach the
// child's feed through the store; what the PERSON asked reaches a feed as a
// row the daemon draws from its own record, and porting only the transcript
// is what left a forked feed showing an answer to a question it did not show.
//
// The turn ids are carried through the SAME mapping the transcript was ported
// under, so the child's questions and its ported answers name one another
// exactly as the parent's did.
//
// A HALF-PORTED CONVERSATION FAILS THE FORK. A child whose feed carries some
// of its parent's questions and not others is the defect this closes wearing
// a different face, so the create refuses rather than proceeding.
func (v *verbs) forkConversation(ctx context.Context, log dlog.Logger, parent ids.WorkspaceID, child ids.WorkspaceID, minted account.RemintedID) error {
	inherited, err := v.deps.DB.ConversationPrompts(ctx, parent)
	if err != nil {
		log.Error(opCreate, "could not read the parent conversation", dlog.Context{
			"parent": string(parent), "cause": err.Error(),
		})
		return fmt.Errorf("fork from %q: read the conversation: %w", parent, err)
	}
	if len(inherited) == 0 {
		log.Debug(opCreate, "the parent carries no prompt rows to port", dlog.Context{"parent": string(parent)})
		return nil
	}
	ported, err := wsm.RemintPortedPrompts(child, inherited, minted)
	if err != nil {
		log.Error(opCreate, "could not re-mint the parent conversation", dlog.Context{
			"parent": string(parent), "cause": err.Error(),
		})
		return fmt.Errorf("fork from %q: re-mint the conversation: %w", parent, err)
	}
	if err := v.deps.DB.PutPortedPrompts(ctx, child, ported); err != nil {
		log.Error(opCreate, "could not record the ported conversation", dlog.Context{
			"parent": string(parent), "cause": err.Error(),
		})
		return fmt.Errorf("fork from %q: record the conversation: %w", parent, err)
	}
	log.Debug(opCreate, "ported the parent's prompt rows under the child's own turn ids", dlog.Context{
		"parent": string(parent), "rows": len(ported),
	})
	return nil
}
