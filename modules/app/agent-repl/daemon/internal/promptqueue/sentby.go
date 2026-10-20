package promptqueue

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// opSentBy is the operation every record of reading a prompt's sender carries.
const opSentBy = "daemon.promptqueue.sent_by"

// sentByOf answers how SUB's sender is declared to the shim, nil for a prompt
// no agent sent. It is read when the turn starts, not when the prompt was
// sent: the vendor weighs the sender's permission class against the
// recipient's at delivery, so the declaration is the sender's mode now.
//
// A SENDER THE DAEMON CANNOT FULLY STATE IS STATED AS FAR AS IT IS KNOWN,
// never filled in: a sender forgotten since it sent goes unnamed, and one with
// no session record declares no mode. Any other failed read fails the start,
// so no agent's prompt is delivered with its sender misstated.
func (q *queue) sentByOf(ctx context.Context, sub Submission, log dlog.Logger) (*SentBy, error) {
	if sub.SentBy == nil {
		return nil, nil
	}
	sentBy := &SentBy{Workspace: *sub.SentBy}
	record, err := q.deps.DB.Workspace(ctx, *sub.SentBy)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		log.Info(opSentBy, "the workspace that sent the prompt is no longer registered; it is declared unnamed", dlog.Context{
			"turn": string(sub.Turn), "sent_by": string(*sub.SentBy)})
	case err != nil:
		log.Error(opSentBy, "could not read the workspace that sent the prompt", dlog.Context{
			"turn": string(sub.Turn), "sent_by": string(*sub.SentBy), "cause": err.Error()})
		return nil, fmt.Errorf("read the sender %q of turn %q: %w", *sub.SentBy, sub.Turn, err)
	default:
		sentBy.Name = record.Name
	}
	session, found, err := q.deps.DB.Session(ctx, *sub.SentBy)
	if err != nil {
		log.Error(opSentBy, "could not read the session of the workspace that sent the prompt", dlog.Context{
			"turn": string(sub.Turn), "sent_by": string(*sub.SentBy), "cause": err.Error()})
		return nil, fmt.Errorf("read the session of the sender %q of turn %q: %w", *sub.SentBy, sub.Turn, err)
	}
	if found {
		sentBy.PermissionMode = session.PermissionMode
	} else {
		log.Info(opSentBy, "the workspace that sent the prompt has no session record; it declares no permission mode", dlog.Context{
			"turn": string(sub.Turn), "sent_by": string(*sub.SentBy)})
	}
	log.Debug(opSentBy, "read the sender of an agent's prompt for its delivery", dlog.Context{
		"turn": string(sub.Turn), "sent_by": string(*sub.SentBy), "sender_name": sentBy.Name,
		"sender_permission_mode": sentBy.PermissionMode})
	return sentBy, nil
}
