package sessioncontroller

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

func testTurnLifecycle() (turnLifecycle, *fakeApplier) {
	store := &fakeApplier{}
	return newTurnLifecycle(store, "ws", "s1"), store
}

func TestTurnLifecycleRejectsDelayedFileEndAcrossANewerTurn(t *testing.T) {
	lifecycle, store := testTurnLifecycle()
	start, err := lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 12885, "turn-new"), "")
	if err != nil {
		t.Fatalf("start: %v", err)
	}
	if !start.apply || !start.notify || !start.active {
		t.Fatalf("start resolution = %+v, want applied active transition", start)
	}

	stale, err := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_FILE, 12891, ""), "")
	if err == nil {
		t.Fatal("stale file end succeeded")
	}
	if stale.apply || stale.notify || stale.decision != "reject_non_authoritative_plane" {
		t.Fatalf("stale file end resolution = %+v", stale)
	}
	if len(store.turns) != 1 || store.turns[0] != "turn-new" {
		t.Fatalf("durable active turns after stale file end = %v, want [turn-new]", store.turns)
	}

	real, err := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 12905, "turn-new"), "")
	if err != nil {
		t.Fatalf("real stream end: %v", err)
	}
	if !real.apply || !real.notify || real.active {
		t.Fatalf("real stream end resolution = %+v, want applied idle transition", real)
	}
}

func TestTurnLifecycleRejectsWrongTurnIdentity(t *testing.T) {
	lifecycle, store := testTurnLifecycle()
	if _, err := lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 1, "turn-current"), ""); err != nil {
		t.Fatalf("start: %v", err)
	}

	got, err := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 2, "turn-prior"), "")
	if err == nil {
		t.Fatal("mismatched end succeeded")
	}
	if got.apply || got.notify || got.decision != "reject_durable_claim" {
		t.Fatalf("mismatched end resolution = %+v", got)
	}
	if len(store.turns) != 1 || store.turns[0] != "turn-current" {
		t.Fatalf("durable active turns = %v, want current claim retained", store.turns)
	}
}

func TestTurnLifecycleKeepsStateActiveUntilEveryQueuedTurnEnds(t *testing.T) {
	lifecycle, _ := testTurnLifecycle()
	if _, err := lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 1, "turn-1"), ""); err != nil {
		t.Fatalf("first start: %v", err)
	}
	second, err := lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 2, "turn-2"), "")
	if err != nil {
		t.Fatalf("second start: %v", err)
	}
	if !second.apply || second.notify {
		t.Fatalf("second start resolution = %+v, want applied without a second active edge", second)
	}

	firstEnd, err := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 3, "turn-1"), "")
	if err != nil {
		t.Fatalf("first end: %v", err)
	}
	if firstEnd.apply || firstEnd.notify || !firstEnd.active {
		t.Fatalf("first queued end resolution = %+v, want state held active", firstEnd)
	}

	lastEnd, err := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 4, "turn-2"), "")
	if err != nil {
		t.Fatalf("last end: %v", err)
	}
	if !lastEnd.apply || !lastEnd.notify || lastEnd.active {
		t.Fatalf("last queued end resolution = %+v, want final idle edge", lastEnd)
	}
}

func TestTurnLifecycleAdmitsOrderedLegacyReplay(t *testing.T) {
	lifecycle, _ := testTurnLifecycle()
	start := turnStartEvent(corev1.Plane_PLANE_STREAM, 1, "")
	end := turnEndEvent(corev1.Plane_PLANE_STREAM, 2, "")
	if got, err := lifecycle.resolve(start, ""); err != nil || got.decision != "accept_legacy_stream_start" || !got.apply {
		t.Fatalf("legacy start resolution = %+v", got)
	}
	if got, err := lifecycle.resolve(end, ""); err != nil || got.decision != "accept_legacy_stream_end" || !got.apply {
		t.Fatalf("legacy end resolution = %+v", got)
	}
}

func TestTurnLifecycleHandshakeRestoresCorrelationForAnUnseenEnd(t *testing.T) {
	store := &fakeApplier{}
	var snapshots []bool
	consumer := &consumer{
		workspace: "ws", sessionID: "s1", ssm: store,
		logf:   func(string, ...any) {},
		onTurn: func(active bool, _ int64) { snapshots = append(snapshots, active) },
	}
	active, closed, err := consumer.reconcileTurnHandshake(&corev1.ShimHello{
		ActiveTurnIds: []string{"turn-live"}, TurnInFlight: true,
	})
	if err != nil {
		t.Fatalf("reconcile handshake: %v", err)
	}
	if !active {
		t.Fatal("handshake resolved idle, want active")
	}
	if len(closed) != 0 {
		t.Fatalf("handshake closed turns %v, want none — the shim confirmed the very claim the ledger holds", closed)
	}
	if len(snapshots) != 0 {
		t.Fatalf("handshake emitted boundary callbacks = %v, want none", snapshots)
	}

	lifecycle := newTurnLifecycle(store, "ws", "s1")
	end, err := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 99, "turn-live"), "")
	if err != nil {
		t.Fatalf("end after handshake: %v", err)
	}
	if !end.apply || !end.notify || end.active {
		t.Fatalf("end after handshake = %+v, want correlated idle edge", end)
	}
}

func TestTurnLifecycleRejectsAStreamEndWithoutDurableClaim(t *testing.T) {
	lifecycle, _ := testTurnLifecycle()
	got, err := lifecycle.resolve(turnEndEvent(corev1.Plane_PLANE_STREAM, 99, "turn-live"), "")
	if err == nil {
		t.Fatal("unclaimed stream end succeeded")
	}
	if got.apply || got.notify || got.decision != "reject_durable_claim" {
		t.Fatalf("unclaimed stream end resolution = %+v", got)
	}
}

func TestTurnLifecycleReplaysARecordedFinalEndToInterruptedConsumers(t *testing.T) {
	lifecycle, _ := testTurnLifecycle()
	if _, err := lifecycle.resolve(turnStartEvent(corev1.Plane_PLANE_STREAM, 1, "turn-live"), ""); err != nil {
		t.Fatalf("start: %v", err)
	}
	end := turnEndEvent(corev1.Plane_PLANE_STREAM, 2, "turn-live")
	if _, err := lifecycle.resolve(end, ""); err != nil {
		t.Fatalf("first end: %v", err)
	}
	replayed, err := lifecycle.resolve(end, "")
	if err != nil {
		t.Fatalf("replayed end: %v", err)
	}
	if !replayed.replayed || !replayed.apply || !replayed.notify || replayed.active {
		t.Fatalf("replayed final end = %+v, want applied idle snapshot", replayed)
	}
}

func TestTurnLifecycleReplayDoesNotPaintIdleWhileQueuedTurnRemains(t *testing.T) {
	lifecycle, _ := testTurnLifecycle()
	for seq, id := range []string{"turn-1", "turn-2"} {
		if _, err := lifecycle.resolve(turnStartEvent(
			corev1.Plane_PLANE_STREAM, uint64(seq+1), id,
		), ""); err != nil {
			t.Fatalf("start %s: %v", id, err)
		}
	}
	end := turnEndEvent(corev1.Plane_PLANE_STREAM, 3, "turn-1")
	if _, err := lifecycle.resolve(end, ""); err != nil {
		t.Fatalf("first queued end: %v", err)
	}
	replayed, err := lifecycle.resolve(end, "")
	if err != nil {
		t.Fatalf("replayed queued end: %v", err)
	}
	if !replayed.replayed || replayed.apply || !replayed.notify || !replayed.active {
		t.Fatalf("replayed queued end = %+v, want active snapshot without SSM idle paint", replayed)
	}
}

func TestContradictoryTurnHandshakeAbortsBeforeHandshakeSideEffects(t *testing.T) {
	h := newQueueHarness(t, nil)
	if _, _, _, err := h.applier.ReconcileTurnHandshake(
		"ws", "s1", []string{"turn-current"}, true,
	); err != nil {
		t.Fatalf("seed durable turn: %v", err)
	}

	err := h.m.onHandshake("ws", "s1", &corev1.ShimHello{
		Pid:             4242,
		QueryInstanceId: "query-1",
		VendorSessionId: "uuid-other",
		TurnInFlight:    true,
		ActiveTurnIds:   []string{"turn-other"},
	})
	if err == nil {
		t.Fatal("contradictory handshake succeeded")
	}
	if got := h.m.shimPIDFor("s1"); got != 0 {
		t.Fatalf("rejected handshake recorded pid %d", got)
	}
	if got := h.reg.adoptionWrites(); len(got) != 0 {
		t.Fatalf("rejected handshake wrote vendor adoption %v", got)
	}
	if got := h.applier.degradedCalls(); len(got) != 0 {
		t.Fatalf("rejected stale handshake mutated connectivity through degradation calls = %+v", got)
	}
}
