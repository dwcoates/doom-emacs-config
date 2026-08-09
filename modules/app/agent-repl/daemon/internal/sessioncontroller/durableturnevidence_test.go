package sessioncontroller

import (
	"errors"
	"reflect"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// THE STORE'S RECORD OUTRANKS ANY PROCESS'S MEMORY. These cover the read that
// makes that possible: which of a returning shim's contradicted claims already
// carry a terminal event in the durable store.

func TestDurableTurnEndsNamesTheStoredTerminal(t *testing.T) {
	// Arrange — the turn finished while the daemon was away, so its TurnEnded
	// sits in the store above the daemon's last-delivered seq.
	h := newDurableHarness(t, &durableHistorySpy{
		events: []*corev1.Event{turnEndEvent(corev1.Plane_PLANE_STREAM, 7, "turn-finished")},
	})

	// Act.
	settled, err := h.m.durableTurnEnds("ws", "s1", []string{"turn-finished"})

	// Assert.
	if err != nil {
		t.Fatalf("durableTurnEnds: %v", err)
	}
	if !reflect.DeepEqual(settled, []string{"turn-finished"}) {
		t.Fatalf("settled = %v, want the turn the store proves ended", settled)
	}
}

func TestDurableTurnEndsReportsNothingForAnUnendedTurn(t *testing.T) {
	// Arrange — the store holds the turn's START and no end: it was cut.
	h := newDurableHarness(t, &durableHistorySpy{
		events: []*corev1.Event{turnStartEvent(corev1.Plane_PLANE_STREAM, 7, "turn-cut")},
	})

	// Act.
	settled, err := h.m.durableTurnEnds("ws", "s1", []string{"turn-cut"})

	// Assert.
	if err != nil {
		t.Fatalf("durableTurnEnds: %v", err)
	}
	if len(settled) != 0 {
		t.Fatalf("settled = %v, want none — no terminal is recorded for this turn", settled)
	}
}

func TestDurableTurnEndsIgnoresATerminalForAnotherTurn(t *testing.T) {
	// Arrange — a stored end that names a DIFFERENT turn proves nothing about
	// the standing claim.
	h := newDurableHarness(t, &durableHistorySpy{
		events: []*corev1.Event{turnEndEvent(corev1.Plane_PLANE_STREAM, 7, "turn-other")},
	})

	// Act.
	settled, err := h.m.durableTurnEnds("ws", "s1", []string{"turn-standing"})

	// Assert.
	if len(settled) != 0 || err != nil {
		t.Fatalf("settled = %v err = %v, want no evidence for an unrelated terminal", settled, err)
	}
}

func TestDurableTurnEndsSkipsTheLegacyIdentitylessClaim(t *testing.T) {
	// Arrange — a legacy claim's end carries no turn id, so nothing in the
	// store can ever be attributed to it and the read is not worth making.
	spy := &durableHistorySpy{}
	h := newDurableHarness(t, spy)

	// Act.
	settled, err := h.m.durableTurnEnds("ws", "s1", []string{""})

	// Assert.
	if len(settled) != 0 || err != nil {
		t.Fatalf("settled = %v err = %v, want no evidence for an identity-less claim", settled, err)
	}
	if replays := spy.replays(); len(replays) != 0 {
		t.Fatalf("store replays = %v, want none — an identity-less claim is unprovable", replays)
	}
}

func TestDurableTurnEndsReadsFromTheDaemonsOwnLastSeenSeq(t *testing.T) {
	// Arrange — the scan must cover exactly the events the reattaching
	// subscription is about to replay, and nothing already delivered.
	spy := &durableHistorySpy{}
	h := newDurableHarness(t, spy)
	h.seq.seq["s1"] = 41

	// Act.
	if _, err := h.m.durableTurnEnds("ws", "s1", []string{"turn-standing"}); err != nil {
		t.Fatalf("durableTurnEnds: %v", err)
	}

	// Assert.
	if replays := spy.replays(); len(replays) != 1 || replays[0] != [2]uint64{41, 0} {
		t.Fatalf("store replays = %v, want one unbounded read from the daemon's last_seen_seq", replays)
	}
}

func TestDurableTurnEndsRefusesATruncatedScan(t *testing.T) {
	// Arrange — a capped read leaves ends UNSEEN, and unseen is not absent.
	h := newDurableHarness(t, &durableHistorySpy{
		events:    []*corev1.Event{turnStartEvent(corev1.Plane_PLANE_STREAM, 7, "turn-standing")},
		truncated: "max_events",
	})

	// Act.
	_, err := h.m.durableTurnEnds("ws", "s1", []string{"turn-standing"})

	// Assert.
	if !errors.Is(err, ErrRepullTruncated) {
		t.Fatalf("durableTurnEnds err = %v, want a truncation refusal rather than a negative answer", err)
	}
}

func TestDurableTurnEndsSurfacesAnUnreadableStore(t *testing.T) {
	// Arrange — the read failed, so nothing is known either way.
	h := newDurableHarness(t, &durableHistorySpy{err: errors.New("store socket refused")})

	// Act.
	_, err := h.m.durableTurnEnds("ws", "s1", []string{"turn-standing"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "store socket refused") {
		t.Fatalf("durableTurnEnds err = %v, want the store failure surfaced", err)
	}
}

func TestDurableTurnEndsWithNoCandidatesReadsNothing(t *testing.T) {
	// Arrange — no standing claim means no question to answer.
	spy := &durableHistorySpy{}
	h := newDurableHarness(t, spy)

	// Act.
	settled, err := h.m.durableTurnEnds("ws", "s1", nil)

	// Assert.
	if settled != nil || err != nil {
		t.Fatalf("settled = %v err = %v, want a no-op", settled, err)
	}
	if replays := spy.replays(); len(replays) != 0 {
		t.Fatalf("store replays = %v, want none", replays)
	}
}

func TestBindDurableTurnEndProbeLeavesTheHookNilWithoutAHistorySource(t *testing.T) {
	// Arrange — a Manager with no durable history cannot prove anything, and
	// the judgment site must be able to SEE that rather than be handed a probe
	// that silently answers "no evidence".
	h := newQueueHarness(t, nil)
	cons := newConsumer("ws", "s1", h.push, h.applier, nil, newFakeClearCompactStore(),
		emptyTurnAccountingStore{}, func(string, ...any) {}, nil, nil, nil, nil, nil)

	// Act.
	h.m.bindDurableTurnEndProbe(cons, "ws", "s1")

	// Assert.
	if cons.durableTurnEnds != nil {
		t.Fatal("durableTurnEnds bound with no DurableHistorySource, want nil so the handshake says so out loud")
	}
}
