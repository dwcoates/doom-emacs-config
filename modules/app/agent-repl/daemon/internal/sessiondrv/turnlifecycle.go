package sessiondrv

import (
	"fmt"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// turnResolution is the one decision every daemon consumer makes before a
// turn boundary can reach either the prompt queue, the SSM, or progress state.
type turnResolution struct {
	apply       bool
	notify      bool
	active      bool
	decision    string
	before      string
	after       string
	correlation string
	replayed    bool
}

// turnLifecycle routes every turn boundary through the SSM-owned durable
// claim ledger. Process memory is deliberately not an authority: after a
// daemon restart, an end is admitted only if the ledger proves the matching
// start (or the exact same completed boundary is being replayed).
type turnLifecycle struct {
	store             StateApplier
	workspace         string
	claimantSessionID string
}

func newTurnLifecycle(store StateApplier, workspace, claimantSessionID string) turnLifecycle {
	return turnLifecycle{store: store, workspace: workspace, claimantSessionID: claimantSessionID}
}

// reconcileTurnHandshake runs at ShimHello, before DaemonHello opens the store
// subscription. A contradictory snapshot therefore fails the bring-up gate
// before any replay or live event can mutate state.
func (c *consumer) reconcileTurnHandshake(hello *corev1.ShimHello) (bool, error) {
	before, after, err := c.ssm.ReconcileTurnHandshake(
		c.workspace, c.sessionID, hello.GetActiveTurnIds(), hello.GetTurnInFlight(),
	)
	c.logf("sessiondrv: turn handshake plane=stream kind=shim_hello session=%s seq=none turn_ids=%s turn_in_flight=%v durable_before=%s durable_after=%s decision=%s notify=%v error=%v",
		c.sessionID, formatTurnIDs(hello.GetActiveTurnIds()), hello.GetTurnInFlight(),
		formatTurnIDs(before), formatTurnIDs(after), handshakeDecision(before, after, err),
		true, err)
	if err != nil {
		return false, err
	}
	return len(after) > 0, nil
}

// resolve decides whether ev may mutate live turn state.
func (t turnLifecycle) resolve(ev *corev1.Event) (turnResolution, error) {
	base := turnResolution{correlation: turnID(ev)}
	if ev.GetPlane() != corev1.Plane_PLANE_STREAM {
		base.decision = "reject_non_authoritative_plane"
		base.before, base.after = "unknown", "unknown"
		return base, fmt.Errorf("turn lifecycle plane %s is not authoritative", ev.GetPlane().String())
	}
	before, after, replayed, err := t.store.ResolveTurnLifecycle(
		t.workspace, t.claimantSessionID, ev,
	)
	base.before = formatTurnIDs(before)
	base.after = formatTurnIDs(after)
	base.replayed = replayed
	if err != nil {
		base.decision = "reject_durable_claim"
		return base, err
	}
	base.active = len(after) > 0
	// An exact receipt replay means the daemon died after the durable claim
	// moved but before last_seen_seq advanced. Re-emit the boundary snapshot so
	// process-local consumers can finish their side of that interrupted
	// delivery; ordinary first delivery only emits on the active/idle edge.
	base.notify = replayed || ((len(before) == 0) != (len(after) == 0))

	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted:
		base.apply = true
		if replayed {
			base.decision = "accept_replayed_stream_start"
		} else if base.correlation == "" {
			base.decision = "accept_legacy_stream_start"
		} else {
			base.decision = "accept_correlated_stream_start"
		}
	case *corev1.Event_TurnEnded:
		if replayed && base.active {
			base.decision = "accept_replayed_stream_end_queued_turn_remains"
		} else if replayed {
			base.apply = true
			base.decision = "accept_replayed_stream_end"
		} else if base.active {
			base.decision = "accept_correlated_stream_end_queued_turn_remains"
		} else {
			base.apply = true
			if base.correlation == "" {
				base.decision = "accept_legacy_stream_end"
			} else {
				base.decision = "accept_correlated_stream_end"
			}
		}
	default:
		return base, fmt.Errorf("turn lifecycle resolver received %T", ev.GetPayload())
	}
	return base, nil
}

func handshakeDecision(before, after []string, err error) string {
	if err != nil {
		return "reject_durable_handshake_claim"
	}
	if len(before) == 0 && len(after) > 0 {
		return "accept_handshake_claim"
	}
	return "confirm_handshake_claim"
}

func turnID(ev *corev1.Event) string {
	if started := ev.GetTurnStarted(); started != nil {
		return started.GetTurnId()
	}
	if ended := ev.GetTurnEnded(); ended != nil {
		return ended.GetTurnId()
	}
	return ""
}

func formatTurnIDs(ids []string) string {
	if len(ids) == 0 {
		return "[]"
	}
	printable := make([]string, len(ids))
	for i, id := range ids {
		if id == "" {
			printable[i] = "<legacy>"
		} else {
			printable[i] = id
		}
	}
	return "[" + strings.Join(printable, ",") + "]"
}
