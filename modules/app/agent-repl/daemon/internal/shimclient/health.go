package shimclient

import (
	"context"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// healthResult is the correlated answer to one HealthCheck.  It has its own
// waiter map because a HealthStatus is not an Ack: an unhealthy answer is a
// valid, useful assertion rather than a control-plane failure.
type healthResult struct {
	status *corev1.HealthStatus
	err    error
}

// Health asks the already handshaked shim to prove the dependencies in its
// responsibility boundary.  It never dials or starts a shim: an absent active
// connection is reported as ErrNotConnected, because session health is only
// true for a live handshaked daemon-to-shim connection.
func (c *Client) Health(ctx context.Context, requestID string) (*corev1.HealthStatus, error) {
	if requestID == "" {
		return nil, fmt.Errorf("shimclient: health requires a non-empty request_id")
	}
	ac := c.currentConn()
	if ac == nil {
		return nil, ErrNotConnected
	}

	ch := make(chan healthResult, 1)
	ac.pendMu.Lock()
	if _, exists := ac.health[requestID]; exists {
		ac.pendMu.Unlock()
		return nil, fmt.Errorf("shimclient: duplicate in-flight health request_id=%q", requestID)
	}
	ac.health[requestID] = ch
	ac.pendMu.Unlock()
	defer func() {
		ac.pendMu.Lock()
		delete(ac.health, requestID)
		ac.pendMu.Unlock()
	}()

	if err := ac.writeMsg(&corev1.HealthCheck{RequestId: requestID}); err != nil {
		return nil, fmt.Errorf("shimclient: send HealthCheck request_id=%s: %w", requestID, err)
	}
	c.logf("health check sent request_id=%s", requestID)

	timer := time.NewTimer(c.cfg.AckTimeout)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return nil, fmt.Errorf("shimclient: health request_id=%s cancelled: %w", requestID, ctx.Err())
	case <-timer.C:
		return nil, fmt.Errorf("%w: health request_id=%s after %s", ErrAckTimeout, requestID, c.cfg.AckTimeout)
	case result := <-ch:
		if result.err != nil {
			return nil, result.err
		}
		c.logf("health status received request_id=%s healthy=%v component=%s reason=%q", requestID, result.status.GetHealthy(), result.status.GetComponent(), result.status.GetReason())
		return result.status, nil
	}
}

// resolveHealth delivers a HealthStatus only to the matching outstanding
// probe.  A mismatched or late status is evidence of a protocol violation and
// is logged; it can never make another request look healthy.
func (c *Client) resolveHealth(ac *activeConn, status *corev1.HealthStatus) {
	requestID := status.GetRequestId()
	ac.pendMu.Lock()
	ch, ok := ac.health[requestID]
	if ok {
		delete(ac.health, requestID)
	}
	ac.pendMu.Unlock()
	if !ok {
		c.logf("received HealthStatus for unknown request_id=%s healthy=%v component=%s reason=%q", requestID, status.GetHealthy(), status.GetComponent(), status.GetReason())
		return
	}
	ch <- healthResult{status: status}
}
