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
		case *corev1.ModelCatalog:
			c.dispatchModelCatalog(m)
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
		case *corev1.ShimReady:
			// GATE STAGE 3. Nothing else releases AwaitReady.
			c.dispatchShimReady(ac, m)
		case *corev1.ShimHello:
			c.logf("unexpected ShimHello after handshake; ignoring")
		default:
			c.logf("unexpected inbound message type %T; ignoring", m)
		}
	}
}

func (c *Client) dispatchModelCatalog(catalog *corev1.ModelCatalog) {
	if catalog.GetSessionId() != c.cfg.SessionID {
		c.logf("received ModelCatalog for session=%s on session=%s connection; ignoring protocol violation", catalog.GetSessionId(), c.cfg.SessionID)
		return
	}
	if c.cfg.Models == nil {
		c.logf("received ModelCatalog session=%s models=%d with no daemon sink", catalog.GetSessionId(), len(catalog.GetModels()))
		return
	}
	c.logf("received ModelCatalog session=%s models=%d", catalog.GetSessionId(), len(catalog.GetModels()))
	c.cfg.Models.ModelCatalog(c.cfg.SessionID, catalog)
}

// dispatchEvent routes one Event and maintains last_seen_seq. Lifecycle
// payloads go to the StateSink (SSM), TurnClaimBridge goes only to its durable
// ledger sink, DegradedState to the DegradedReporter, and everything else
// (data.v1 vendor Any, ContentDelta, HeartbeatProgress, MessageLatency,
// ContextCleared, ContextCompacted, UnparsedEvent) to the FrameSink. PERSISTENT
// events (seq > 0) advance the monotonic high-water mark; a regression is a
// fatal protocol violation.
func (c *Client) dispatchEvent(ev *corev1.Event) error {
	if seq := ev.GetSeq(); seq > 0 {
		if seq <= c.lastSeen {
			return fmt.Errorf("%w: session=%s got seq=%d after last_seen=%d",
				ErrSeqRegression, ev.GetSessionId(), seq, c.lastSeen)
		}
	}

	switch p := ev.GetPayload().(type) {
	case *corev1.Event_FilePlaneDiagnostic:
		if err := validateFilePlaneDiagnostic(ev, p.FilePlaneDiagnostic); err != nil {
			return err
		}
		if c.cfg.FileDiagnostics == nil {
			return errors.New("shimclient: file-plane diagnostic sink is not wired")
		}
		if err := c.cfg.FileDiagnostics.PersistFileDiagnostic(ev, p.FilePlaneDiagnostic); err != nil {
			return fmt.Errorf("shimclient: persist file-plane diagnostic: %w", err)
		}
	case *corev1.Event_TurnClaimBridge:
		if c.cfg.TurnClaims == nil {
			return fmt.Errorf("%w session=%s seq=%d: sink is not wired",
				ErrTurnClaimRejected, ev.GetSessionId(), ev.GetSeq())
		}
		if err := c.cfg.TurnClaims.ApplyTurnClaimBridge(ev); err != nil {
			return fmt.Errorf("%w session=%s seq=%d turn_id=%q: %v",
				ErrTurnClaimRejected, ev.GetSessionId(), ev.GetSeq(),
				p.TurnClaimBridge.GetTurnId(), err)
		}
	case *corev1.Event_SessionStarted,
		*corev1.Event_SessionEnded,
		*corev1.Event_TurnStarted,
		*corev1.Event_TurnEnded,
		*corev1.Event_TaskStarted,
		*corev1.Event_TaskProgress,
		*corev1.Event_TaskEnded:
		if err := c.cfg.StateSink.Apply(ev); err != nil {
			return fmt.Errorf("%w session=%s seq=%d kind=%T: %v",
				ErrLifecycleRejected, ev.GetSessionId(), ev.GetSeq(), p, err)
		}
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
	if seq := ev.GetSeq(); seq > 0 {
		c.lastSeen = seq
		c.cfg.SeqStore.SetLastSeq(c.cfg.SessionID, seq)
	}
	return nil
}

func validateFilePlaneDiagnostic(ev *corev1.Event, diagnostic *corev1.FilePlaneDiagnostic) error {
	if ev.GetSeq() == 0 {
		return errors.New("shimclient: file-plane diagnostic must be persistent")
	}
	if ev.GetClass() != corev1.EventClass_EVENT_CLASS_PERSISTENT {
		return fmt.Errorf("shimclient: file-plane diagnostic class %s is not persistent", ev.GetClass())
	}
	if ev.GetPlane() != corev1.Plane_PLANE_FILE {
		return fmt.Errorf("shimclient: file-plane diagnostic has plane %s, want PLANE_FILE", ev.GetPlane())
	}
	if ev.GetProducedAtMs() <= 0 {
		return errors.New("shimclient: file-plane diagnostic source timestamp is required")
	}
	if diagnostic == nil {
		return errors.New("shimclient: file-plane diagnostic payload is required")
	}
	if diagnostic.GetSourceRuntime() != corev1.DiagnosticSourceRuntime_DIAGNOSTIC_SOURCE_RUNTIME_SIDECAR {
		return fmt.Errorf("shimclient: unsupported file-plane diagnostic source runtime %s", diagnostic.GetSourceRuntime())
	}
	if diagnostic.GetSourcePid() <= 0 || diagnostic.GetOperation() == "" || diagnostic.GetMessage() == "" || diagnostic.GetContext() == nil {
		return errors.New("shimclient: file-plane diagnostic is missing required source fields")
	}
	switch diagnostic.GetLevel() {
	case "debug", "info", "warn", "error":
	default:
		return fmt.Errorf("shimclient: file-plane diagnostic has invalid level %q", diagnostic.GetLevel())
	}
	switch diagnostic.GetVerbosity() {
	case "normal", "verbose":
	default:
		return fmt.Errorf("shimclient: file-plane diagnostic has invalid verbosity %q", diagnostic.GetVerbosity())
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
