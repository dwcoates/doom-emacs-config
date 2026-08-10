// asynccancel.go is the async plane's half of the detached-agent cancel: when
// the shim reports which agents it stopped, the bubbles those agents own reach
// a terminal state instead of rendering as still live.
//
// WHY THE BUBBLES ARE SETTLED FROM THE ACK AT ALL. The cancel's whole point is
// that the user can SEE the work stop. A bubble left running in the feed and a
// live count left standing in the footer say the opposite of what the daemon
// just did, and "it will settle when the CLI's stopped notification is folded"
// is a promise the user is watching a stale screen to collect on. The ack is
// the shim's direct observation that the stop resolved — the same class of
// evidence a TaskEnded is — so it settles here, immediately, and the event
// plane remains free to overwrite the verdict with its own (see
// asyncBubbleStore.settleCancelledTasks).
package sessioncontroller

import (
	"context"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// CancelDetachedAgents stops the workspace session's detached background
// agents and settles the bubbles of every agent it stopped.
//
// IT DOES NOT RECOVER A MISSING SESSION CONTROLLER, and that is the difference
// from Interrupt. A stop aimed at a turn is worth bringing a session up for:
// the log can still show a turn in flight behind a controller that is gone,
// and the tokens are still burning. Detached agents live INSIDE the vendor CLI
// process the shim holds — no shim, no agents — so a workspace with no live
// session has nothing detached by construction, and spawning one to ask would
// create the very session it was meant to stop work in.
//
// THE OUTCOME IS RETURNED, NOT INTERPRETED. Whether `nothing_running` reads as
// a refusal to the user is the frontend command layer's ruling; here it is an
// answer that settles no bubbles because there were none to settle.
//
// requestID is the FRONTEND COMMAND'S OWN id, carried through for the same
// reason Interrupt's is: the wire travels under a daemon-minted control id
// that appears in no caller's records, so without it the cancel would be
// unfindable end to end.
func (m *Manager) CancelDetachedAgents(ctx context.Context, workspace, requestID string) (*corev1.DetachedCancelOutcome, error) {
	d, err := m.existing(workspace)
	if err != nil {
		return nil, err
	}
	outcome, err := d.client.CancelDetachedAgents(ctx, requestID)
	if err != nil {
		m.logf("session-controller: cancel-detached-agents FAILED ws=%s session=%s request_id=%s: %v — no bubble is settled, because nothing is known to have stopped",
			workspace, d.sessionID, requestID, err)
		return nil, err
	}
	cancelled := outcome.GetCancelled()
	if cancelled == nil {
		m.logf("session-controller: cancel-detached-agents ws=%s session=%s request_id=%s stopped NOTHING outcome=%T",
			workspace, d.sessionID, requestID, outcome.GetOutcome())
		return outcome, nil
	}
	m.logf("session-controller: cancel-detached-agents ws=%s session=%s request_id=%s STOPPED %d agent(s) task_ids=%v",
		workspace, d.sessionID, requestID, len(cancelled.GetTaskIds()), cancelled.GetTaskIds())
	d.consumer.settleDetachedOnCancel(cancelled.GetTaskIds(),
		fmt.Sprintf("user cancelled detached agents (%s)", requestID))
	return outcome, nil
}

// settleDetachedOnCancel settles the bubbles of the agents a cancel stopped and
// publishes the result.
//
// STOPPED, not done, and the verdict is the machinery's own: a cancelled agent
// is TERMINAL_STATUS_STOPPED, which frontend.SettleAsyncBubble resolves to the
// killed arm — "the work did not fail, it was not allowed to conclude". This
// function does not re-decide that mapping, exactly as settleWindowsOnInterrupt
// does not.
//
// A CANCEL THAT NAMED NO AGENTS SETTLES NOTHING and is not an error: the shim
// answers `nothing_running` in that case, and the daemon refuses the command
// on it — there was never a bubble to close.
func (c *consumer) settleDetachedOnCancel(taskIDs []string, reason string) {
	if len(taskIDs) == 0 {
		return
	}
	ups, err := c.bubbles.settleCancelledTasks(taskIDs, frontend.AsyncVerdict{
		Status: corev1.TerminalStatus_TERMINAL_STATUS_STOPPED,
		AtMs:   c.now(),
		Reason: reason,
	})
	push := c.detachedCancelSettlePush(ups, err, "reason="+reason)
	if push.empty() {
		return
	}
	// The cancel arrives on a control path, not on a store event, so the push
	// carries the newest seq the session has SEEN rather than a seq of its own
	// — through_seq is the client's replay cursor and inventing a number ahead
	// of the stream would move it past events it never received. Same rule
	// settleWindowsOnInterrupt follows, for the same reason.
	c.publishControlSettle(push, c.newestRetainedSeq(), "detached cancel")
}

// detachedCancelSettlePush turns one cancel settlement into its push.
//
// A DEGRADED SETTLE IS LOUD BECAUSE OF WHAT IT LEAVES ON SCREEN. The agent HAS
// been stopped by this point — the shim said so — so a bubble that could not be
// settled renders live work that is not running, which is the one reading a
// user cannot correct by waiting.
func (c *consumer) detachedCancelSettlePush(ups []*frontendv1.AsyncBubbleUpdate, err error, where string) asyncPush {
	return c.controlSettlePush(ups, err, func(residual error) {
		c.warn("session-controller: DETACHED CANCEL BUBBLE SETTLE DEGRADED session=%s ws=%q %s — the agents ARE stopped, but their bubbles will keep rendering as live work: %v",
			c.sessionID, c.workspace, where, residual)
	})
}
