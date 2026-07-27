package shimclient

import (
	"context"
	"errors"
	"fmt"
	"io"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// readLoop is the single-goroutine demux: it reads frames in arrival order and
// dispatches each to the right consumer, so per-session ordering is preserved
// by construction. It NEVER dedups (the store already did) and it tracks
// last_seen_seq monotonically, erroring loudly on a regression. It returns when
// the connection closes, ctx is cancelled, or a protocol violation is hit.
func (c *Client) readLoop(ctx context.Context, ac *activeConn) error {
	for {
		msg, err := wire.ReadAny(ac.conn)
		if err != nil {
			if ctx.Err() != nil {
				return ctx.Err()
			}
			if errors.Is(err, io.EOF) {
				c.logf("shim closed the connection cleanly")
				return io.EOF
			}
			return fmt.Errorf("reading frame: %w", err)
		}
		c.markRecv()

		switch m := msg.(type) {
		case *corev1.Event:
			if err := c.dispatchEvent(m); err != nil {
				return err // seq regression: terminal
			}
		case *corev1.Ack:
			c.resolveAck(ac, m)
		case *corev1.Nack:
			c.resolveNack(ac, m)
		case *corev1.HealthStatus:
			c.resolveHealth(ac, m)
		case *corev1.PermissionRequest:
			c.dispatchPermission(ctx, ac, m)
		// REPLAYED HISTORY. Its own arms, physically apart from the live
		// *corev1.Event case above: a replayed event cannot reach dispatchEvent
		// (and so cannot reach the SSM, the task catalog, or the progress
		// resolver) because it is not that type. See replay.go.
		case *corev1.ReplayEvent:
			c.dispatchReplayEvent(m)
		case *corev1.ReplayDone:
			c.dispatchReplayDone(m)
		case *corev1.Heartbeat:
			// Liveness only (already recorded via markRecv). No reply: our own
			// heartbeatSender covers the reverse direction.
		case *corev1.ShimHello:
			c.logf("unexpected ShimHello after handshake; ignoring")
		default:
			c.logf("unexpected inbound message type %T; ignoring", m)
		}
	}
}

// dispatchEvent routes one Event and maintains last_seen_seq. Lifecycle
// payloads go to the StateSink (SSM), DegradedState to the DegradedReporter,
// and everything else (data.v1 vendor Any, ContentDelta, HeartbeatProgress,
// MessageLatency, ContextCleared, ContextCompacted, UnparsedEvent) to the
// FrameSink. PERSISTENT events (seq > 0) advance the monotonic high-water mark;
// a regression is a fatal protocol violation.
func (c *Client) dispatchEvent(ev *corev1.Event) error {
	if seq := ev.GetSeq(); seq > 0 {
		if seq <= c.lastSeen {
			return fmt.Errorf("%w: session=%s got seq=%d after last_seen=%d",
				ErrSeqRegression, ev.GetSessionId(), seq, c.lastSeen)
		}
		c.lastSeen = seq
		c.cfg.SeqStore.SetLastSeq(c.cfg.SessionID, seq)
	}

	switch p := ev.GetPayload().(type) {
	case *corev1.Event_SessionStarted,
		*corev1.Event_SessionEnded,
		*corev1.Event_TurnStarted,
		*corev1.Event_TurnEnded,
		*corev1.Event_TaskStarted,
		*corev1.Event_TaskProgress,
		*corev1.Event_TaskEnded:
		c.cfg.StateSink.Apply(ev)
	case *corev1.Event_DegradedState:
		c.logf("shim reported DegradedState component=%s reason=%q dropped=%d recovered=%v",
			p.DegradedState.GetComponent(), p.DegradedState.GetReason(),
			p.DegradedState.GetDroppedCount(), p.DegradedState.GetRecovered())
		c.cfg.Degraded.Degraded(c.cfg.SessionID, p.DegradedState)
	case *corev1.Event_Unparsed:
		c.logf("received UnparsedEvent producer=%s path=%s offset=%d error=%q",
			p.Unparsed.GetProducer(), p.Unparsed.GetSourcePath(),
			p.Unparsed.GetByteOffset(), p.Unparsed.GetError())
		c.cfg.FrameSink.Consume(ev)
	case *corev1.Event_ContentDelta,
		*corev1.Event_HeartbeatProgress,
		*corev1.Event_MessageLatency,
		// The clear and the compaction. Both are CONVERSATION content — each
		// renders as its own bubble and floors the frontend's replay — so they
		// belong to the frame sink, not the lifecycle sink: nothing in the SSM's
		// state axes moves because a conversation's history stopped informing
		// the agent.
		*corev1.Event_ContextCleared,
		*corev1.Event_ContextCompacted,
		*corev1.Event_Vendor:
		c.cfg.FrameSink.Consume(ev)
	case nil:
		c.logf("received Event with empty payload seq=%d; forwarding to frame sink", ev.GetSeq())
		c.cfg.FrameSink.Consume(ev)
	default:
		c.logf("received Event with unhandled payload %T; forwarding to frame sink", p)
		c.cfg.FrameSink.Consume(ev)
	}
	return nil
}

// dispatchPermission runs the injected handler on its own goroutine (it may
// block on a human) and sends the returned PermissionResponse. The event demux
// is never blocked by a pending permission answer.
func (c *Client) dispatchPermission(ctx context.Context, ac *activeConn, req *corev1.PermissionRequest) {
	c.logf("received PermissionRequest request_id=%s tool=%s", req.GetRequestId(), req.GetToolName())
	go func() {
		resp := c.cfg.Permissions.HandlePermission(c.cfg.SessionID, req)
		if resp == nil {
			c.logf("permission handler returned nil for request_id=%s; no response sent (shim stays blocked)",
				req.GetRequestId())
			return
		}
		if resp.GetRequestId() == "" {
			resp.RequestId = req.GetRequestId()
		}
		if ctx.Err() != nil {
			c.logf("connection gone before permission response for request_id=%s", req.GetRequestId())
			return
		}
		if err := c.PermissionResponse(resp); err != nil {
			c.logf("failed to send permission response for request_id=%s: %v", req.GetRequestId(), err)
		}
	}()
}
