package promptqueue

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// AN AGENT'S PROMPT TO THIS WORKSPACE IS NEVER CLASSIFIED AND IS NEVER HELD
// FOR THE RUNNING TURN'S END (owner ruling, 2026-11-14). The classifier weighs
// a person's prompt against the running work to decide whether it interrupts,
// joins, or waits. A message another agent sent is neither a correction nor a
// new request to be weighed: it is delivered to the recipient as soon as the
// vendor can take it, so it is sent to join the running turn at once, the
// verdict the classifier would have called after_tool_call, and nothing is
// interrupted. It joins beside any prompt already joining, as a flush's and a
// release's do.
//
// What keeps it from joining is the same as for any joining prompt (joinLocked):
// a running turn the vendor started on its own, an open ask, a drain for a
// bounce, an edit that withholds it, or a session act running. It then waits
// for the running turn's end with the reason stamped on its verdict.

// agentSent reports whether SUB is a prompt another agent sent.
func agentSent(sub Submission) bool {
	return sub.SentBy != nil
}

// agentSentVerdict is the verdict an agent's prompt earns while a turn runs.
func (q *queue) agentSentVerdict() wsm.Classification {
	return wsm.Classification{
		Arm:    wsm.ArmAfterToolCall,
		Reason: "the prompt was sent by another agent: it joins the running turn at once and is never classified",
		At:     q.deps.Now(),
	}
}

// agentSentNeverClassified records, at INFO, an agent's prompt kept from the
// classifier.
func agentSentNeverClassified(log dlog.Logger, sub Submission, running ids.TurnID) {
	log.Info(opClassify, "the prompt was sent by another agent; it joins the running turn and is never classified", dlog.Context{
		"held_turn": string(sub.Turn), "sent_by": string(*sub.SentBy), "running_turn": string(running),
	})
}

// startAgentJoin sends an agent's held prompt to join RUNNING, off the
// submission's goroutine: the join takes the delivery lock the submission
// holds.
func (q *queue) startAgentJoin(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) {
	epoch := q.contentEpoch(sub.WS, sub.Turn)
	q.classifying.Add(1)
	inflight.ForWorkspace(string(sub.WS)).Go(func() {
		defer q.classifying.Done()
		q.settleJoin(context.WithoutCancel(ctx), sub, running, epoch, q.agentSentVerdict(), joinAgentSent, log)
	})
}
