package sessioncontroller

import (
	"fmt"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/ssm"
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
	// afterIDs is the durable claim set the ledger holds AFTER this boundary,
	// unformatted. It is what the session controller's turn record is projected
	// from: the ledger is the authority on which turns are in flight, so the
	// record is derived from it rather than accumulated beside it.
	afterIDs []string
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
// closed names the phantom claims the reconciliation ended, and it is what the
// caller releases its queue on: a prompt held behind one of them is waiting for
// a boundary the process that owed it no longer exists to send.
func (c *consumer) reconcileTurnHandshake(hello *corev1.ShimHello) (active bool, closed []string, err error) {
	before, after, closed, err := c.ssm.ReconcileTurnHandshake(
		c.workspace, c.sessionID, hello.GetActiveTurnIds(), hello.GetTurnInFlight(),
	)
	c.logf("session-controller: turn handshake plane=stream kind=shim_hello session=%s seq=none turn_ids=%s turn_in_flight=%v durable_before=%s durable_after=%s phantom_closed=%s decision=%s notify=%v error=%v",
		c.sessionID, formatTurnIDs(hello.GetActiveTurnIds()), hello.GetTurnInFlight(),
		formatTurnIDs(before), formatTurnIDs(after), formatTurnIDs(closed),
		handshakeDecision(before, after, err), true, err)
	if err != nil {
		return false, nil, err
	}
	if len(closed) > 0 {
		c.logf("session-controller: turn INTERRUPTED BY RESTART ws=%q session=%s closed=%s cause=%s — the shim came back reporting no turn in flight over a workspace still claiming %s; those turns were CUT when the process behind them went away and are reported interrupted rather than left thinking",
			c.workspace, c.sessionID, formatTurnIDs(closed), ssm.TurnCloseRestartInterrupted,
			formatTurnIDs(before))
	}
	return len(after) > 0, closed, nil
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
	base.afterIDs = after
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
