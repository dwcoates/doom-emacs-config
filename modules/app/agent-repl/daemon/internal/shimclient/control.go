package shimclient

import (
	"context"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"

	"google.golang.org/protobuf/proto"
)

// The control-plane sentinels. Their VALUES live in internal/errclass, which
// is the package that classifies them; these are the historic names, kept so
// every existing errors.Is call site is unchanged and so the sentinel and its
// classification cannot be added independently of each other.

// ErrNotConnected is returned by a control send when there is no live shim
// connection. Control commands are never silently queued or dropped.
var ErrNotConnected = errclass.ErrShimNotConnected

// ErrNack is returned when the shim Nacks a control request. The reason is
// wrapped; there is NO silent retry (metaprompt no-fallbacks rule).
var ErrNack = errclass.ErrShimNack

// ErrAckTimeout is returned when no Ack/Nack arrives within the AckTimeout.
var ErrAckTimeout = errclass.ErrShimAckTimeout

// protoControl is a proto control message carrying a request_id.
type protoControl interface {
	proto.Message
	GetRequestId() string
}

// SubmitPrompt sends a prompt to the shim and blocks until the shim Acks or
// Nacks it (or ctx / the AckTimeout elapses). A Nack or timeout is a loud
// error — this layer never retries (the caller decides).
//
// requestID IS THE TURN'S IDENTITY, not merely a correlation token for the Ack.
// The shim adopts the accepted SubmitPrompt's request_id as the turn_id it
// stamps on TurnStarted, TurnEnded, Event.request_id and every consequence
// derived from them (core.proto §"Turn lifecycle authority"), so whatever id
// goes out here is the id the durable turn ledger, the keep-alive window row
// and SessionRewound.dropped_turn_ids will all name.
//
// A caller that already owns that identity — the keep-alive ping, whose claim,
// queue holds and window row are keyed by the id BEFORE the submit — passes it
// here, and the id it published is the id the turn comes back under. Minting a
// fresh one below would hand the same turn two names, and every match against
// the caller's name would then fail: exactly the defect that left keep-alive
// windows open forever and pings leaking into the live conversation.
//
// An empty requestID means the caller has no identity of its own for this turn
// and one is minted (newRequestID). That is not a fallback for a failed lookup:
// it is the honest statement that nothing daemon-side is keyed by this turn's
// name, so any unique name will do.
func (c *Client) SubmitPrompt(ctx context.Context, requestID, text, origin, permissionMode string, promptOrigin corev1.PromptOrigin) error {
	if promptOrigin == corev1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return fmt.Errorf("shimclient: submit prompt requires a non-UNSPECIFIED prompt origin")
	}
	if _, ok := corev1.PromptOrigin_name[int32(promptOrigin)]; !ok {
		return fmt.Errorf("shimclient: submit prompt received unknown prompt origin %d", promptOrigin)
	}
	reqID := requestID
	if reqID == "" {
		minted, err := c.newRequestID("prompt")
		if err != nil {
			return err
		}
		reqID = minted
	}
	_, err := c.sendAwait(ctx, &corev1.SubmitPrompt{
		RequestId:      reqID,
		Text:           text,
		Origin:         origin,
		PermissionMode: permissionMode,
		PromptOrigin:   promptOrigin,
	})
	return err
}

// Interrupt asks the shim to interrupt the current turn (the SDK's
// interrupt()) and awaits the Ack/Nack.
//
// It returns the shim's OWN verdict on what the stop did. That verdict is
// only obtainable here: from outside, a stop that failed and a turn that had
// already ended arrive as the same silence, which is how a no-op stop came to
// be painted as a failed one. A Nack or timeout is still an error, unchanged.
func (c *Client) Interrupt(ctx context.Context) (corev1.InterruptOutcome, error) {
	reqID, err := c.newRequestID("interrupt")
	if err != nil {
		return corev1.InterruptOutcome_INTERRUPT_OUTCOME_UNSPECIFIED, err
	}
	ack, err := c.sendAwait(ctx, &corev1.Interrupt{RequestId: reqID})
	if err != nil {
		return corev1.InterruptOutcome_INTERRUPT_OUTCOME_UNSPECIFIED, err
	}
	return ack.GetInterruptOutcome(), nil
}

// SetModel asks the live shim to change its SDK model and returns the model the
// shim confirmed. Callers persist only this value, never their requested one.
func (c *Client) SetModel(ctx context.Context, model string) (string, error) {
	requested := registry.NormalizeModel(model)
	if requested == "" {
		return "", fmt.Errorf("set model: model id is empty")
	}
	reqID, err := c.newRequestID("set-model")
	if err != nil {
		return "", err
	}
	ack, nack, err := c.sendAwaitReceipt(ctx, &corev1.SetModel{RequestId: reqID, Model: requested})
	if err != nil {
		return "", err
	}
	if nack != nil {
		selected := registry.NormalizeModel(nack.GetSelectedModel())
		if selected == "" {
			return "", fmt.Errorf("set model: shim rejected request_id=%s without its current selected_model: %w", reqID, ErrNack)
		}
		c.logf("set-model REJECTED request_id=%s requested=%q selected=%q reason=%q", reqID, requested, selected, nack.GetReason())
		return selected, fmt.Errorf("%w: request_id=%s reason=%q", ErrNack, reqID, nack.GetReason())
	}
	selected := registry.NormalizeModel(ack.GetSelectedModel())
	if selected == "" {
		return "", fmt.Errorf("set model: shim ack request_id=%s omitted selected_model", reqID)
	}
	c.logf("set-model request_id=%s requested=%q selected=%q", reqID, requested, selected)
	return selected, nil
}

// PermissionResponse sends the answer to an inbound PermissionRequest,
// correlated by the SAME request_id the shim used. It is fire-and-forget on
// the control plane (the shim uses it to unblock canUseTool); it is NOT
// Ack-awaited.
func (c *Client) PermissionResponse(resp *corev1.PermissionResponse) error {
	ac := c.currentConn()
	if ac == nil {
		return ErrNotConnected
	}
	if err := ac.writeMsg(resp); err != nil {
		return fmt.Errorf("sending PermissionResponse (request_id=%s): %w", resp.GetRequestId(), err)
	}
	c.logf("sent permission response request_id=%s decision=%s",
		resp.GetRequestId(), resp.GetDecision())
	return nil
}

// currentConn returns the live connection or nil.
func (c *Client) currentConn() *activeConn {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.active
}

// sendAwait registers a correlation waiter, sends msg, then blocks for the
// matching Ack/Nack. The waiter is always deregistered before returning.
//
// It returns the ACK itself, not just success: an Ack carries a verdict on
// some commands (an interrupt's outcome) and discarding it here would put the
// only process that can see that verdict in the position of having to guess
// at it later.
func (c *Client) sendAwait(ctx context.Context, msg protoControl) (*corev1.Ack, error) {
	ack, nack, err := c.sendAwaitReceipt(ctx, msg)
	if err != nil {
		return nil, err
	}
	if nack != nil {
		return nil, fmt.Errorf("%w: request_id=%s reason=%q", ErrNack, nack.GetRequestId(), nack.GetReason())
	}
	return ack, nil
}

// sendAwaitReceipt is the shared correlated-control exchange.  SetModel needs
// a rejected receipt's shim-confirmed selected_model; legacy controls retain
// sendAwait's simpler Ack-or-error contract above.
func (c *Client) sendAwaitReceipt(ctx context.Context, msg protoControl) (*corev1.Ack, *corev1.Nack, error) {
	reqID := msg.GetRequestId()
	ac := c.currentConn()
	if ac == nil {
		return nil, nil, ErrNotConnected
	}

	ch := make(chan ackResult, 1)
	ac.pendMu.Lock()
	ac.pending[reqID] = ch
	ac.pendMu.Unlock()
	defer func() {
		ac.pendMu.Lock()
		delete(ac.pending, reqID)
		ac.pendMu.Unlock()
	}()

	if err := ac.writeMsg(msg); err != nil {
		return nil, nil, fmt.Errorf("sending control request (request_id=%s): %w", reqID, err)
	}
	c.logf("sent control request request_id=%s, awaiting ack (timeout=%s)", reqID, c.cfg.AckTimeout)

	timer := time.NewTimer(c.cfg.AckTimeout)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return nil, nil, fmt.Errorf("control request %s cancelled: %w", reqID, ctx.Err())
	case <-timer.C:
		return nil, nil, fmt.Errorf("%w: request_id=%s after %s", ErrAckTimeout, reqID, c.cfg.AckTimeout)
	case res := <-ch:
		if res.err != nil {
			return nil, nil, res.err
		}
		if res.nack != nil {
			return nil, res.nack, nil
		}
		c.logf("control request request_id=%s acked outcome=%s", reqID, res.ack.GetInterruptOutcome())
		return res.ack, nil, nil
	}
}

// resolveAck delivers an Ack to the correlated waiter. An Ack with no waiter is
// loud-logged (a stray or duplicate ack) but not fatal.
func (c *Client) resolveAck(ac *activeConn, ack *corev1.Ack) {
	if !ac.deliver(ack.GetRequestId(), ackResult{ack: ack}) {
		c.logf("received Ack for unknown request_id=%s (stray or late)", ack.GetRequestId())
	}
}

// resolveNack delivers a Nack (loud) to the correlated waiter.
func (c *Client) resolveNack(ac *activeConn, nack *corev1.Nack) {
	c.logf("received Nack request_id=%s reason=%q", nack.GetRequestId(), nack.GetReason())
	if !ac.deliver(nack.GetRequestId(), ackResult{nack: nack}) {
		c.logf("received Nack for unknown request_id=%s (stray or late)", nack.GetRequestId())
	}
}

// deliver hands res to the waiter for reqID, returning whether one existed.
func (ac *activeConn) deliver(reqID string, res ackResult) bool {
	ac.pendMu.Lock()
	ch, ok := ac.pending[reqID]
	if ok {
		delete(ac.pending, reqID)
	}
	ac.pendMu.Unlock()
	if !ok {
		return false
	}
	ch <- res
	return true
}
