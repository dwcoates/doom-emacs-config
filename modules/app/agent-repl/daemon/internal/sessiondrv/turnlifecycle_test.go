package sessiondrv

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func turnStartEvent(plane corev1.Plane, seq uint64, id string) *corev1.Event {
	return &corev1.Event{
		SessionId: "vendor-session",
		Seq:       seq,
		Plane:     plane,
		RequestId: id,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{
			TurnId: id,
		}},
	}
}

func turnEndEvent(plane corev1.Plane, seq uint64, id string) *corev1.Event {
	return &corev1.Event{
		SessionId: "vendor-session",
		Seq:       seq,
		Plane:     plane,
		RequestId: id,
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{
			TurnId: id,
		}},
	}
}

func TestTurnLifecycleRejectsDelayedFileEndAcrossANewerTurn(t *testing.T) {
	var lifecycle turnLifecycle
	start := lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 12885, "turn-new"))
	if !start.apply || !start.notify || !start.active {
		t.Fatalf("start resolution = %+v, want applied active transition", start)
	}

	stale := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_FILE, 12891, ""))
	if stale.apply || stale.notify || stale.decision != "reject_non_authoritative_plane" {
		t.Fatalf("stale file end resolution = %+v", stale)
	}
	if stale.after != "[turn-new]" {
		t.Fatalf("active turns after stale file end = %s, want [turn-new]", stale.after)
	}

	real := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 12905, "turn-new"))
	if !real.apply || !real.notify || real.active {
		t.Fatalf("real stream end resolution = %+v, want applied idle transition", real)
	}
}

func TestTurnLifecycleRejectsWrongTurnIdentity(t *testing.T) {
	var lifecycle turnLifecycle
	lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 1, "turn-current"))

	got := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 2, "turn-prior"))
	if got.apply || got.notify || got.decision != "reject_turn_id_mismatch" {
		t.Fatalf("mismatched end resolution = %+v", got)
	}
	if got.after != "[turn-current]" {
		t.Fatalf("active turns = %s, want current claim retained", got.after)
	}
}

func TestTurnLifecycleKeepsStateActiveUntilEveryQueuedTurnEnds(t *testing.T) {
	var lifecycle turnLifecycle
	lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 1, "turn-1"))
	second := lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 2, "turn-2"))
	if !second.apply || second.notify {
		t.Fatalf("second start resolution = %+v, want applied without a second active edge", second)
	}

	firstEnd := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 3, "turn-1"))
	if firstEnd.apply || firstEnd.notify || !firstEnd.active {
		t.Fatalf("first queued end resolution = %+v, want state held active", firstEnd)
	}

	lastEnd := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 4, "turn-2"))
	if !lastEnd.apply || !lastEnd.notify || lastEnd.active {
		t.Fatalf("last queued end resolution = %+v, want final idle edge", lastEnd)
	}
}

func TestTurnLifecycleAdmitsOrderedLegacyReplay(t *testing.T) {
	var lifecycle turnLifecycle
	start := turnStartEvent(corev1.Plane_PLANE_STREAM, 1, "")
	end := turnEndEvent(corev1.Plane_PLANE_STREAM, 2, "")
	if got := lifecycle.resolve(start); got.decision != "accept_legacy_stream_start" || !got.apply {
		t.Fatalf("legacy start resolution = %+v", got)
	}
	if got := lifecycle.resolve(end); got.decision != "accept_legacy_stream_end" || !got.apply {
		t.Fatalf("legacy end resolution = %+v", got)
	}
}

func TestTurnLifecycleHandshakeRestoresCorrelationForAnUnseenEnd(t *testing.T) {
	var lifecycle turnLifecycle
	hello, err := lifecycle.reconcileHandshake([]string{"turn-live"}, true)
	if err != nil {
		t.Fatalf("reconcile handshake: %v", err)
	}
	if !hello.notify || !hello.active {
		t.Fatalf("handshake resolution = %+v, want active edge", hello)
	}

	end := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 99, "turn-live"))
	if !end.apply || !end.notify || end.active {
		t.Fatalf("end after handshake = %+v, want correlated idle edge", end)
	}
}

func TestTurnLifecycleAcceptsAStreamEndAfterProcessLocalStateWasLost(t *testing.T) {
	var lifecycle turnLifecycle
	got := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 99, "turn-live"))
	if !got.apply || !got.notify || got.decision != "accept_recovered_stream_end" {
		t.Fatalf("recovered stream end resolution = %+v", got)
	}
}
