package sessiondrv

import (
	"fmt"
	"strings"
	"sync"

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
}

// turnLifecycle owns the daemon's ordered active-turn claims for one shim
// stream. Its only producer is the shim's STREAM plane.
//
// The queue is identity-bearing because the shim accepts streaming input in
// FIFO order. A result must close the head claim it names. File-plane
// stop_hook_summary records never enter this structure, so their delay relative
// to the next prompt is irrelevant by construction.
type turnLifecycle struct {
	mu     sync.Mutex
	active []string
}

// reconcileTurnHandshake folds the shim's attach snapshot through the same
// authority that consumes events, then synchronizes the prompt queue's active
// bit when the snapshot supplies a previously unseen claim.
func (c *consumer) reconcileTurnHandshake(hello *corev1.ShimHello) error {
	res, err := c.turns.reconcileHandshake(hello.GetActiveTurnIds(), hello.GetTurnInFlight())
	c.logf("sessiondrv: turn handshake plane=stream kind=shim_hello session=%s seq=none turn_ids=%s turn_in_flight=%v active_before=%s active_after=%s decision=%s notify=%v error=%v",
		c.sessionID, formatTurnIDs(hello.GetActiveTurnIds()), hello.GetTurnInFlight(),
		res.before, res.after, res.decision, res.notify, err)
	if err != nil {
		return err
	}
	if res.notify && c.onTurn != nil {
		c.onTurn(res.active)
	}
	return nil
}

// resolve decides whether ev may mutate live turn state.
func (t *turnLifecycle) resolve(ev *corev1.Event) turnResolution {
	t.mu.Lock()
	defer t.mu.Unlock()

	before := formatTurnIDs(t.active)
	base := turnResolution{before: before, after: before, correlation: turnID(ev)}
	if ev.GetPlane() != corev1.Plane_PLANE_STREAM {
		base.decision = "reject_non_authoritative_plane"
		return base
	}

	switch p := ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted:
		id := p.TurnStarted.GetTurnId()
		if id != "" && ev.GetRequestId() != id {
			base.decision = "reject_start_envelope_mismatch"
			return base
		}
		if id != "" && containsTurnID(t.active, id) {
			base.decision = "reject_duplicate_active_turn"
			return base
		}
		t.active = append(t.active, id)
		base.apply = true
		base.notify = len(t.active) == 1
		base.active = true
		base.after = formatTurnIDs(t.active)
		if id == "" {
			base.decision = "accept_legacy_stream_start"
		} else {
			base.decision = "accept_correlated_stream_start"
		}
		return base

	case *corev1.Event_TurnEnded:
		id := p.TurnEnded.GetTurnId()
		if id != "" && ev.GetRequestId() != id {
			base.decision = "reject_end_envelope_mismatch"
			return base
		}
		if len(t.active) == 0 {
			// The daemon may reconnect after consuming TurnStarted but before
			// consuming its durable end. The SSM already holds the active claim,
			// while this process-local correlator is newly empty. A STREAM end
			// is still authoritative in that replay window.
			base.apply = true
			base.notify = true
			base.active = false
			base.decision = "accept_recovered_stream_end"
			return base
		}

		head := t.active[0]
		switch {
		case head == "" && id == "":
			base.decision = "accept_legacy_stream_end"
		case head != "" && id == head:
			base.decision = "accept_correlated_stream_end"
		default:
			base.decision = "reject_turn_id_mismatch"
			return base
		}

		t.active = append([]string(nil), t.active[1:]...)
		base.after = formatTurnIDs(t.active)
		base.active = len(t.active) > 0
		if base.active {
			// A queued input is still running. Applying this intermediate end
			// to the SSM would settle the workspace while another accepted turn
			// remains live.
			base.decision += "_queued_turn_remains"
			return base
		}
		base.apply = true
		base.notify = true
		return base
	default:
		panic(fmt.Sprintf("sessiondrv: turn lifecycle resolver received %T", ev.GetPayload()))
	}
}

// reconcileHandshake adopts the shim-owned active turn identities on attach.
// ids is authoritative for new shims. legacyActive is the compatibility
// projection carried by shims built before active_turn_ids existed.
func (t *turnLifecycle) reconcileHandshake(ids []string, legacyActive bool) (turnResolution, error) {
	t.mu.Lock()
	defer t.mu.Unlock()

	before := formatTurnIDs(t.active)
	res := turnResolution{before: before, after: before}
	for _, id := range ids {
		if id == "" {
			return res, fmt.Errorf("active_turn_ids contains an empty identity")
		}
	}

	switch {
	case len(ids) > 0 && len(t.active) == 0:
		t.active = append([]string(nil), ids...)
		res.notify = true
		res.active = true
		res.after = formatTurnIDs(t.active)
		res.decision = "accept_correlated_handshake_claim"
	case len(ids) > 0 && equalTurnIDs(t.active, ids):
		res.active = true
		res.decision = "confirm_correlated_handshake_claim"
	case len(ids) > 0:
		res.decision = "reject_handshake_claim_mismatch"
		return res, fmt.Errorf("handshake active_turn_ids=%s disagree with observed active turns=%s",
			formatTurnIDs(ids), before)
	case legacyActive && len(t.active) == 0:
		t.active = []string{""}
		res.notify = true
		res.active = true
		res.after = formatTurnIDs(t.active)
		res.decision = "accept_legacy_handshake_claim"
	case legacyActive:
		res.active = true
		res.decision = "confirm_legacy_handshake_claim"
	default:
		// Do not clear an observed claim here. ShimHello is captured before the
		// standing subscription replays, so a start can legitimately reach the
		// consumer between that snapshot and ShimReady.
		res.active = len(t.active) > 0
		res.decision = "confirm_idle_handshake_snapshot"
	}
	return res, nil
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

func containsTurnID(ids []string, target string) bool {
	for _, id := range ids {
		if id == target {
			return true
		}
	}
	return false
}

func equalTurnIDs(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
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
