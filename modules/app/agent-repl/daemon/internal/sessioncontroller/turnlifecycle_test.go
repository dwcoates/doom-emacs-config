package sessioncontroller

import (
	"errors"
	"fmt"
	"reflect"
	"strings"
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

// evidenceConsumer is a consumer wired to a fakeApplier and a stub durable
// probe, for the ONE judgment durablySettledClaims makes: which standing claims
// the store proves completed.
func evidenceConsumer(applier *fakeApplier, probe durableTurnEndProbe, logf func(string, ...any)) *consumer {
	cons := newConsumer("ws", "s1", &fakePusher{}, applier, nil, newFakeClearCompactStore(),
		emptyTurnAccountingStore{}, logf, nil, nil, nil, nil, nil)
	cons.durableTurnEnds = probe
	return cons
}

func TestDurablySettledClaimsNamesTheProvenCompletion(t *testing.T) {
	// Arrange — the shim is silent and the store holds the turn's terminal.
	applier := &fakeApplier{turns: []string{"turn-finished"}}
	cons := evidenceConsumer(applier, func([]string) ([]string, error) {
		return []string{"turn-finished"}, nil
	}, func(string, ...any) {})

	// Act.
	got := cons.durablySettledClaims(&corev1.ShimHello{})

	// Assert.
	if !reflect.DeepEqual(got, []string{"turn-finished"}) {
		t.Fatalf("durablySettledClaims = %v, want the durably-ended claim spared", got)
	}
}

func TestDurablySettledClaimsSkipsTheProbeWhenTheHelloNamesATurn(t *testing.T) {
	// Arrange — a hello that names its turns reaches a decision without the
	// store, so it must not pay for a replay.
	applier := &fakeApplier{turns: []string{"turn-live"}}
	probed := false
	cons := evidenceConsumer(applier, func([]string) ([]string, error) {
		probed = true
		return nil, nil
	}, func(string, ...any) {})

	// Act.
	got := cons.durablySettledClaims(&corev1.ShimHello{TurnInFlight: true, ActiveTurnIds: []string{"turn-live"}})

	// Assert.
	if got != nil || probed {
		t.Fatalf("durablySettledClaims = %v probed = %v, want no store read for a hello that names its turns", got, probed)
	}
}

func TestDurablySettledClaimsSkipsTheProbeWithNoStandingClaim(t *testing.T) {
	// Arrange — nothing is claimed, so there is nothing that could be cut.
	probed := false
	cons := evidenceConsumer(&fakeApplier{}, func([]string) ([]string, error) {
		probed = true
		return nil, nil
	}, func(string, ...any) {})

	// Act.
	got := cons.durablySettledClaims(&corev1.ShimHello{})

	// Assert.
	if got != nil || probed {
		t.Fatalf("durablySettledClaims = %v probed = %v, want no store read with no claim to judge", got, probed)
	}
}

func TestDurablySettledClaimsProvesNothingWhenTheProbeFails(t *testing.T) {
	// Arrange — an unreadable store is not evidence of completion, so the
	// pre-existing interrupted verdict must stand.
	applier := &fakeApplier{turns: []string{"turn-standing"}}
	var logged []string
	cons := evidenceConsumer(applier, func([]string) ([]string, error) {
		return nil, errors.New("store socket refused")
	}, func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) })

	// Act.
	got := cons.durablySettledClaims(&corev1.ShimHello{})

	// Assert.
	if got != nil {
		t.Fatalf("durablySettledClaims = %v, want nothing proved by a failed read", got)
	}
	if !strings.Contains(strings.Join(logged, "\n"), "durable turn-end evidence UNREADABLE") {
		t.Fatalf("missing the loud unreadable-evidence record; log:\n%s", strings.Join(logged, "\n"))
	}
}

func TestDurablySettledClaimsProvesNothingWithNoProbeBound(t *testing.T) {
	// Arrange — a consumer with no durable history source cannot tell a
	// completed turn from a cut one, and says so.
	applier := &fakeApplier{turns: []string{"turn-standing"}}
	var logged []string
	cons := evidenceConsumer(applier, nil, func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) })

	// Act.
	got := cons.durablySettledClaims(&corev1.ShimHello{})

	// Assert.
	if got != nil {
		t.Fatalf("durablySettledClaims = %v, want nothing proved without a probe", got)
	}
	if !strings.Contains(strings.Join(logged, "\n"), "durable turn-end evidence UNAVAILABLE") {
		t.Fatalf("missing the loud unavailable-evidence record; log:\n%s", strings.Join(logged, "\n"))
	}
}

func TestDurablySettledClaimsProvesNothingWhenTheClaimReadFails(t *testing.T) {
	// Arrange — the standing claims cannot be read, so there is no candidate
	// list to prove anything about.
	applier := &fakeApplier{activeTurnIDsErr: errors.New("ledger unreadable")}
	probed := false
	cons := evidenceConsumer(applier, func([]string) ([]string, error) {
		probed = true
		return nil, nil
	}, func(string, ...any) {})

	// Act.
	got := cons.durablySettledClaims(&corev1.ShimHello{})

	// Assert.
	if got != nil || probed {
		t.Fatalf("durablySettledClaims = %v probed = %v, want no evidence and no store read", got, probed)
	}
}

func TestReconcileTurnHandshakeKeepsTheDurablyEndedClaimOpen(t *testing.T) {
	// Arrange — the whole gap, end to end at the judgment site: a claim stands,
	// the returning shim contradicts it, and the store proves it completed.
	applier := &fakeApplier{turns: []string{"turn-finished"}}
	cons := evidenceConsumer(applier, func([]string) ([]string, error) {
		return []string{"turn-finished"}, nil
	}, func(string, ...any) {})

	// Act.
	active, closed, err := cons.reconcileTurnHandshake(&corev1.ShimHello{})

	// Assert.
	if err != nil {
		t.Fatalf("reconcileTurnHandshake: %v", err)
	}
	if len(closed) != 0 {
		t.Fatalf("closed = %v, want none — a completed turn is never cut as interrupted", closed)
	}
	if !active {
		t.Fatal("active = false, want the spared claim still standing for its own replayed TurnEnded")
	}
}

func TestReconcileTurnHandshakeCutsTheClaimTheStoreCannotProve(t *testing.T) {
	// Arrange — the same shape with NO durable terminal: this turn really was
	// cut when the process behind it went away.
	applier := &fakeApplier{turns: []string{"turn-cut"}}
	cons := evidenceConsumer(applier, func([]string) ([]string, error) { return nil, nil }, func(string, ...any) {})

	// Act.
	active, closed, err := cons.reconcileTurnHandshake(&corev1.ShimHello{})

	// Assert.
	if err != nil {
		t.Fatalf("reconcileTurnHandshake: %v", err)
	}
	if !reflect.DeepEqual(closed, []string{"turn-cut"}) {
		t.Fatalf("closed = %v, want the unprovable claim cut", closed)
	}
	if active {
		t.Fatal("active = true, want the workspace idle after the cut")
	}
}

func TestContradictoryTurnHandshakeAbortsBeforeHandshakeSideEffects(t *testing.T) {
	h := newQueueHarness(t, nil)
	if _, _, _, err := h.applier.ReconcileTurnHandshake(
		"ws", "s1", []string{"turn-current"}, true, nil,
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
