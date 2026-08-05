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
			if err := c.dispatchModelCatalog(m); err != nil {
				return err
			}
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

func (c *Client) dispatchModelCatalog(catalog *corev1.ModelCatalog) error {
	if catalog.GetSessionId() != c.cfg.SessionID {
		return c.modelCatalogInvariant("model catalog session=%s arrived on session=%s connection", catalog.GetSessionId(), c.cfg.SessionID)
	}
	if c.cfg.Models == nil {
		return c.modelCatalogInvariant("model catalog session=%s models=%d has no configured sink", catalog.GetSessionId(), len(catalog.GetModels()))
	}
	c.logf("received ModelCatalog session=%s models=%d", catalog.GetSessionId(), len(catalog.GetModels()))
	if err := c.cfg.Models.ModelCatalog(c.cfg.SessionID, catalog); err != nil {
		return c.modelCatalogInvariant("model catalog session=%s rejected by sink: %v", catalog.GetSessionId(), err)
	}
	return nil
}

// modelCatalogInvariant makes a broken capability channel visible before the
// read loop aborts and reconnects.  The error remains terminal for THIS link;
// the card is not a substitute for a correct catalog sink.
func (c *Client) modelCatalogInvariant(format string, args ...any) error {
	reason := fmt.Sprintf(format, args...)
	c.logf("MODEL CATALOG INVARIANT VIOLATION: %s", reason)
	if c.cfg.Degraded != nil {
		c.cfg.Degraded.Degraded(c.cfg.SessionID, &corev1.DegradedState{
			Component: "daemon-model-catalog",
			Reason:    reason,
		})
	}
	return fmt.Errorf("shimclient: %s", reason)
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
			// Fatal inside the mark's own generation, a rebase across a proven
			// generation change, and fatal again when the generation cannot be
			// identified. See seqgeneration.go.
			if err := c.reconcileSeqGeneration(ev, seq); err != nil {
				return err
			}
		}
	}
	if err := c.validateDurableCursorTransition(ev); err != nil {
		c.logf("replay cursor invariant REJECTED before sink mutation session=%s seq=%d kind=%T active_turns=%d pending_termination_query=%q error=%v", c.cfg.SessionID, ev.GetSeq(), ev.GetPayload(), len(c.pinnedAccountingTurns), c.pendingTerminationQuery, err)
		return fmt.Errorf("%w: %v", ErrReplayCursorInvariant, err)
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
		// Account-window observations participate in terminal accounting and
		// can be rejected when they name no admitted turn. They must use the
		// error-returning state sink so a rejection cannot advance lastSeen.
		*corev1.Event_AccountUsageObservation,
		*corev1.Event_TaskStarted,
		*corev1.Event_TaskProgress,
		*corev1.Event_TaskEnded:
		if err := c.cfg.StateSink.Apply(ev); err != nil {
			return fmt.Errorf("%w session=%s seq=%d kind=%T: %v",
				ErrLifecycleRejected, ev.GetSessionId(), ev.GetSeq(), p, err)
		}
	case *corev1.Event_SessionRewound:
		// A REAL ROUTE, not the default FrameSink fallthrough it used to take.
		// SessionRewound is correlation evidence in TurnClaimBridge's sense: the
		// rotation itself is announced by the ordinary handshake bounce, and
		// this is the durable explanation of WHY the vendor identity changed, so
		// a lineage can be reconstructed from the store alone.
		//
		// It is NOT conversation, which is why it must not reach the frame sink:
		// the curator would find no items on it and the event would be
		// indistinguishable from an unhandled payload in the log.
		if c.cfg.Rewinds == nil {
			return fmt.Errorf("shimclient: session rewind sink is not wired session=%s seq=%d previous_vendor_session=%s",
				ev.GetSessionId(), ev.GetSeq(), p.SessionRewound.GetPreviousVendorSessionId())
		}
		if err := c.cfg.Rewinds.ApplySessionRewound(ev, p.SessionRewound); err != nil {
			return fmt.Errorf("shimclient: apply session rewound session=%s seq=%d previous_vendor_session=%s new_vendor_session=%s: %w",
				ev.GetSessionId(), ev.GetSeq(), p.SessionRewound.GetPreviousVendorSessionId(),
				p.SessionRewound.GetNewVendorSessionId(), err)
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
		if err := c.consumeFrame(ev, fmt.Sprintf("%T", p)); err != nil {
			return err
		}
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
		if err := c.consumeFrame(ev, fmt.Sprintf("%T", p)); err != nil {
			return err
		}
	case nil:
		c.logf("received Event with empty payload seq=%d; forwarding to frame sink", ev.GetSeq())
		if err := c.consumeFrame(ev, "empty"); err != nil {
			return err
		}
	default:
		c.logf("received Event with unhandled payload %T; forwarding to frame sink", p)
		if err := c.consumeFrame(ev, fmt.Sprintf("%T", p)); err != nil {
			return err
		}
	}
	if seq := ev.GetSeq(); seq > 0 {
		c.lastSeen = seq
		// The mark is only comparable against a later seq from the SAME seq
		// space, so it is stamped with the generation that advanced it. An
		// unidentifiable generation ("") is recorded honestly as such — see
		// reconcileSeqGeneration, which never grants it an amnesty.
		c.seqGeneration = c.connGeneration
		c.advanceDurableCursor(ev)
	}
	return nil
}

func (c *Client) validateDurableCursorTransition(ev *corev1.Event) error {
	if ev.GetSeq() == 0 {
		return nil
	}
	switch payload := ev.GetPayload().(type) {
	case *corev1.Event_TurnEnded:
		turnID := payload.TurnEnded.GetTurnId()
		if turnID != "" {
			if _, ok := c.pinnedAccountingTurns[turnID]; !ok {
				// A REPLAYED END IS EXPECTED INPUT, NOT A VIOLATION.
				//
				// The pin set is rebuilt at handshake from the durable open
				// claims, so a turn absent from claimsOpenAtHandshake is one the
				// daemon had ALREADY COMPLETED before this connection resumed.
				// Its end replaying from below the cursor is evidence about a
				// finished turn — there is no start still owed and nothing to
				// keep atomic. Treating it as a protocol violation made the
				// rejection terminal and left the workspace unopenable
				// (slack-ceac-tech-xfq), which is the same mistake the accounting
				// reducer made when it judged replayed rows by live identity.
				//
				// The GENUINE inconsistency is still fatal: a turn whose claim
				// WAS open at handshake was pinned by that reconstruction, so
				// finding it unpinned here means the pin was lost underneath us.
				// WITHOUT DURABLE TRUTH THERE IS NO CLASSIFICATION, so the check
				// stays strict. A client that never reconstructed from the ledger
				// cannot prove an end is history, and guessing would weaken the
				// invariant for a daemon that never wired the authority.
				if !c.hasDurableClaimAuthority() {
					return fmt.Errorf("turn end names unpinned accounting turn %q", turnID)
				}
				if _, wasOpen := c.claimsOpenAtHandshake[turnID]; wasOpen {
					return fmt.Errorf("turn end names unpinned accounting turn %q whose claim was open at handshake", turnID)
				}
				c.logf("shimclient: replayed turn end for an already-completed turn session=%s seq=%d turn_id=%q — its claim was closed before this connection resumed, so the end is history rather than a protocol violation",
					c.cfg.SessionID, ev.GetSeq(), turnID)
			}
		}
	case *corev1.Event_QueryLifecycle:
		lifecycle := payload.QueryLifecycle
		if runtime := lifecycle.GetRuntimeObserved(); runtime != nil && c.pendingResumeQuery != "" && lifecycle.GetQueryInstanceId() != c.pendingResumeQuery {
			return fmt.Errorf("runtime identity for query %q arrived while resumed query %q awaits identity proof", lifecycle.GetQueryInstanceId(), c.pendingResumeQuery)
		}
		if queryTerminationNeedsCompanion(lifecycle) {
			queryID := lifecycle.GetQueryInstanceId()
			if queryID == "" {
				return errors.New("typed query termination has no query instance id")
			}
			if c.pendingTerminationQuery != "" && c.pendingTerminationQuery != queryID {
				return fmt.Errorf("query termination %q arrived while %q awaits its companion", queryID, c.pendingTerminationQuery)
			}
		}
	case *corev1.Event_DegradedState:
		degraded := payload.DegradedState
		if degraded.GetComponent() == "claude-shim-sdk" && degraded.GetReason() == "unexpected_query_termination" && !degraded.GetRecovered() {
			if degraded.QueryInstanceId == nil || degraded.GetQueryInstanceId() == "" {
				return errors.New("unexpected query termination degradation has no query instance id")
			}
			if c.pendingTerminationQuery == "" || degraded.GetQueryInstanceId() != c.pendingTerminationQuery {
				return fmt.Errorf("unexpected query termination degradation query %q does not match pending termination %q", degraded.GetQueryInstanceId(), c.pendingTerminationQuery)
			}
		}
	}
	return nil
}

// advanceDurableCursor keeps crash recovery behind every logical record that
// needs multiple store events or an external transaction to become complete.
// Transport reconnects use lastSeen in memory, while a new daemon has only the
// pinned SeqStore cursor and must replay the complete turn or termination pair.
func (c *Client) advanceDurableCursor(ev *corev1.Event) {
	if c.pinnedAccountingTurns == nil {
		c.pinnedAccountingTurns = map[string]struct{}{}
	}
	switch payload := ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted:
		turnID := payload.TurnStarted.GetTurnId()
		if turnID != "" {
			c.pinnedAccountingTurns[turnID] = struct{}{}
		}
	case *corev1.Event_TurnClaimBridge:
		// A rotated sequence deliberately contains no duplicate TurnStarted.
		// Its durable bridge is the proof that pins the same logical accounting
		// transaction before assistant usage and the terminal boundary arrive.
		turnID := payload.TurnClaimBridge.GetTurnId()
		if turnID != "" {
			c.pinnedAccountingTurns[turnID] = struct{}{}
		}
	case *corev1.Event_TurnEnded:
		turnID := payload.TurnEnded.GetTurnId()
		if turnID == "" {
			break
		}
		delete(c.pinnedAccountingTurns, turnID)
	case *corev1.Event_QueryLifecycle:
		lifecycle := payload.QueryLifecycle
		if created := lifecycle.GetCreated(); created != nil && created.GetResumed() != nil {
			c.pendingResumeQuery = lifecycle.GetQueryInstanceId()
		}
		if lifecycle.GetRuntimeObserved() != nil && lifecycle.GetQueryInstanceId() == c.pendingResumeQuery {
			c.pendingResumeQuery = ""
		}
		if lifecycle.GetTerminated() != nil && lifecycle.GetQueryInstanceId() == c.pendingResumeQuery {
			c.pendingResumeQuery = ""
		}
		if queryTerminationNeedsCompanion(lifecycle) {
			queryID := lifecycle.GetQueryInstanceId()
			c.pendingTerminationQuery = queryID
		}
	case *corev1.Event_DegradedState:
		degraded := payload.DegradedState
		if degraded.GetComponent() == "claude-shim-sdk" && degraded.GetReason() == "unexpected_query_termination" && !degraded.GetRecovered() {
			if c.pendingTerminationQuery != "" {
				c.pendingTerminationQuery = ""
			}
		}
	}
	if len(c.pinnedAccountingTurns) == 0 && c.pendingTerminationQuery == "" && c.pendingResumeQuery == "" {
		c.cfg.SeqStore.SetLastSeq(c.cfg.SessionID, ev.GetSeq())
	}
}

// queryTerminationNeedsCompanion names the termination records whose durable
// meaning is completed by the following unexpected-query DegradedState. An
// intentional shutdown has no such companion: pinning it would leave the
// cursor behind a completed hibernation and reject the next query instance.
func queryTerminationNeedsCompanion(lifecycle *corev1.QueryLifecycle) bool {
	terminated := lifecycle.GetTerminated()
	return terminated != nil && (terminated.GetUnexpectedEof() != nil ||
		terminated.GetIteratorFailure() != nil || terminated.GetStartupFailure() != nil)
}

func (c *Client) consumeFrame(ev *corev1.Event, kind string) error {
	if err := c.cfg.FrameSink.Consume(ev); err != nil {
		return fmt.Errorf("%w: frame sink rejected session=%s seq=%d kind=%s: %w",
			ErrLifecycleRejected, ev.GetSessionId(), ev.GetSeq(), kind, err)
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
