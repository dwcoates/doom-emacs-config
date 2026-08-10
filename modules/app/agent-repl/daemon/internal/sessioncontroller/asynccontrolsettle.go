// asynccontrolsettle.go holds what EVERY control-plane bubble settlement
// shares.
//
// A control-plane settlement is one a COMMAND ordered rather than one the
// event stream reported: the interrupt's window close and the detached-agent
// cancel are both of them, and each has exactly one thing of its own — which
// bubbles it settles and what it says when a settle is refused. Everything
// else (the frame, the fence, the through_seq rule, the fault channel, the
// gap classification) is one decision, so it is made once, here, rather than
// twice in two files that would drift the moment either changed.
package sessioncontroller

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// controlSettlePush turns one control-ordered settlement into its push,
// classifying a refusal exactly as every other async refusal is classified:
// the gap faults become failure cards, and whatever is left over is warned
// about.
//
// `degraded` writes the caller's OWN sentence for that leftover, because what
// a failed settle leaves on screen differs by route — a window that closed
// with a live-looking bubble under it is not the same regression as an agent
// the daemon has already stopped still rendering as working — and a shared
// sentence could only describe one of them.
func (c *consumer) controlSettlePush(ups []*frontendv1.AsyncBubbleUpdate, err error, degraded func(residual error)) asyncPush {
	var push asyncPush
	gaps, residual := splitAsyncGaps(err)
	push.Faults = append(push.Faults, gaps...)
	if residual != nil {
		degraded(residual)
	}
	push.Updates = append(push.Updates, ups...)
	return push
}

// publishControlSettle pushes a settlement that no store event produced — one
// ordered by a control command rather than observed on the stream.
//
// SHARED BY EVERY CONTROL-PLANE SETTLEMENT (the interrupt's window close and
// the detached-agent cancel alike), because they differ only in what they
// settle: the frame, the fence, the through_seq rule and the fault channel are
// one decision, made once. `label` names the ordering command in both records
// so the two routes stay distinguishable in the log without being two
// functions that could drift.
//
// It goes out through the SAME frame every async push uses, fenced and
// sequenced identically, so a reconnecting client applies it with the same
// staleness rule as any other push.
func (c *consumer) publishControlSettle(push asyncPush, throughSeq uint64, label string) {
	for _, fault := range push.Faults {
		c.warn("session-controller: %s SETTLE FAULT session=%s ws=%q card_uuid=%s — %s",
			label, c.sessionID, c.workspace, fault.UUID, fault.Detail)
	}
	if len(push.Updates) == 0 {
		return
	}
	c.logf("session-controller: %s settle push session=%s ws=%q through_seq=%d updates=%s",
		label, c.sessionID, c.workspace, throughSeq, updatedBubbleIDs(push.Updates))
	c.push.PushAsyncBubbleDelta(&frontendv1.AsyncBubbleDelta{
		Workspace:  c.workspace,
		Updates:    push.Updates,
		ThroughSeq: throughSeq,
		Fence:      c.fence(),
	})
}
