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
	// liveness is the SSM's ONE derivation of turn liveness, produced in the
	// same transaction that moved the ledger and painted the workspace color.
	// The session controller's turn record IS this value; it is not projected
	// from it and not recomputed beside it, which is what makes the queue's
	// "turn in flight" answer and the color's the same answer.
	liveness ssm.TurnLiveness
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
//
// ---------------------------------------------------------------------------
// THE STORE'S RECORD OUTRANKS ANY PROCESS'S MEMORY
// ---------------------------------------------------------------------------
//
// This is the one place a turn is judged CUT, and the judgment may not be made
// from the hello alone. A hello is a live process's account of what it is
// running NOW, and by that account a turn that finished during the daemon gap
// and a turn that died with its process are the same silence. Only the durable
// terminal record separates them, and it is the higher authority: a `TurnEnded`
// in the store is a recorded fact, while process memory on either side is an
// inference about one.
//
// So the store is read FIRST (durableTurnEnds), and every claim it can prove a
// terminal for is spared. A spared claim is settled COMPLETED by its own
// replayed boundary through the ordinary turn lifecycle — accounting, result
// item, progress edge and all — rather than replaced by a synthesized cut.
// Only a claim the store has NO terminal evidence for is cut as interrupted.
func (c *consumer) reconcileTurnHandshake(hello *corev1.ShimHello) (active bool, closed []string, err error) {
	durablyEnded := c.durablySettledClaims(hello)
	before, after, closed, err := c.ssm.ReconcileTurnHandshake(
		c.workspace, c.sessionID, hello.GetActiveTurnIds(), hello.GetTurnInFlight(), durablyEnded,
	)
	c.logf("session-controller: turn handshake plane=stream kind=shim_hello session=%s seq=none turn_ids=%s turn_in_flight=%v durable_before=%s durable_after=%s durably_ended=%s phantom_closed=%s decision=%s notify=%v error=%v",
		c.sessionID, formatTurnIDs(hello.GetActiveTurnIds()), hello.GetTurnInFlight(),
		formatTurnIDs(before), formatTurnIDs(after), formatTurnIDs(durablyEnded), formatTurnIDs(closed),
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

// durablySettledClaims names the standing turn claims the STORE already holds a
// terminal event for, and it is consulted before any claim may be cut.
//
// IT RUNS ONLY ON THE HELLO THAT WOULD OTHERWISE CUT. The probe is a store
// replay, and it can change exactly one answer: whether a claim the hello no
// longer names was interrupted or completed. A hello that names its turns, or a
// legacy shim positively asserting one, or a workspace holding no claim at all,
// all reach a decision without it, so none of them pays for it.
//
// EVERY FAILURE RETURNS NO EVIDENCE AND IS LOUD. Absence of evidence is not
// evidence of completion: with nothing proved, the reconciliation cuts the claim
// exactly as it did before, and this record is what explains a completed turn
// that was nonetheless reported interrupted.
func (c *consumer) durablySettledClaims(hello *corev1.ShimHello) []string {
	if len(hello.GetActiveTurnIds()) > 0 || hello.GetTurnInFlight() {
		return nil
	}
	standing, err := c.ssm.ActiveTurnIDs(c.workspace, c.sessionID)
	if err != nil {
		c.warn("session-controller: durable turn-end evidence UNREADABLE ws=%q session=%s: reading the standing turn claims failed: %v — the handshake proceeds on the hello alone, so a turn that finished during the gap can still be reported interrupted",
			c.workspace, c.sessionID, err)
		return nil
	}
	if len(standing) == 0 {
		return nil
	}
	if c.durableTurnEnds == nil {
		c.warn("session-controller: durable turn-end evidence UNAVAILABLE ws=%q session=%s standing=%s — no durable history source is wired to this consumer, so a turn that COMPLETED during the gap cannot be told apart from one that was CUT, and these claims take the interrupted verdict",
			c.workspace, c.sessionID, formatTurnIDs(standing))
		return nil
	}
	settled, err := c.durableTurnEnds(standing)
	if err != nil {
		c.warn("session-controller: durable turn-end evidence UNREADABLE ws=%q session=%s standing=%s: %v — with nothing proved these claims are cut as interrupted, which is the pre-existing verdict and may misreport a turn that finished",
			c.workspace, c.sessionID, formatTurnIDs(standing), err)
		return nil
	}
	if len(settled) == 0 {
		c.logf("session-controller: durable turn-end evidence ABSENT ws=%q session=%s standing=%s — the store holds no terminal for any standing claim, so these turns really were cut when the process behind them went away",
			c.workspace, c.sessionID, formatTurnIDs(standing))
		return nil
	}
	c.logf("session-controller: turn COMPLETED DURING THE GAP ws=%q session=%s standing=%s durably_ended=%s — the store's record outranks the hello's silence, so these claims are NOT cut; each is settled by its own replayed `TurnEnded` through the ordinary lifecycle, keeping its accounting and its result",
		c.workspace, c.sessionID, formatTurnIDs(standing), formatTurnIDs(settled))
	return settled
}

// resolve decides whether ev may mutate live turn state.
//
// liveQueryInstanceID is the query the consumer is currently bound to. It is
// handed down per call rather than held on the struct because the binding
// changes at each handshake while the resolver lives for the consumer.
func (t turnLifecycle) resolve(ev *corev1.Event, liveQueryInstanceID string) (turnResolution, error) {
	base := turnResolution{correlation: turnID(ev)}
	if ev.GetPlane() != corev1.Plane_PLANE_STREAM {
		base.decision = "reject_non_authoritative_plane"
		base.before, base.after = "unknown", "unknown"
		return base, fmt.Errorf("turn lifecycle plane %s is not authoritative", ev.GetPlane().String())
	}
	boundary, err := t.store.ApplyTurnBoundary(
		t.workspace, t.claimantSessionID, liveQueryInstanceID, ev,
	)
	before, after, replayed := boundary.Before, boundary.After, boundary.Replayed
	base.before = formatTurnIDs(before)
	base.after = formatTurnIDs(after)
	base.liveness = boundary.Liveness
	base.replayed = replayed
	if err != nil {
		base.decision = "reject_durable_claim"
		return base, err
	}
	// THE ONE DERIVATION, not a second count of the claim set. `active` is what
	// the workspace color was painted from a few microseconds ago, inside the
	// same transaction; reading len(after) here would be this file folding the
	// ledger on its own terms again.
	base.active = boundary.Liveness.Active()
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
