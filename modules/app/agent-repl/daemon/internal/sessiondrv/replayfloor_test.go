package sessiondrv

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"
)

// THE REPLAY FLOOR.
//
// A frontend replay starts at max(clientLastSeq, newestClearOrCompactSeq),
// INCLUSIVE. History above a clear or a compaction is history the frontend
// would only discard, and the event itself is the bubble it draws and the rule
// it discards above — so a floor that excluded it would tell a frontend to
// throw everything away and hand it nothing to show for it.

// clearEvent is a first-class ContextCleared at seq.
func clearEvent(seq uint64, uuid string) *corev1.Event {
	return &corev1.Event{
		SessionId: "vendor-uuid", Seq: seq, DedupKey: "clear:" + uuid,
		Payload: &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}},
	}
}

// compactEvent is a first-class ContextCompacted at seq.
func compactEvent(seq uint64, uuid string) *corev1.Event {
	return &corev1.Event{
		SessionId: "vendor-uuid", Seq: seq, DedupKey: "compact:" + uuid,
		Payload: &corev1.Event_ContextCompacted{ContextCompacted: &corev1.ContextCompacted{
			PreTokens: 180000, PostTokens: 24000, Summary: "the story so far",
		}},
	}
}

// --- observing a clear or a compaction raises the floor ---------------------

func TestObservingAClearRecordsTheReplayFloor(t *testing.T) {
	// Arrange.
	h := newRepullHarness(t, &replayClient{})

	// Act.
	h.driver(t).consumer.Consume(clearEvent(30, "u-clear"))

	// Assert.
	if got := h.floors.NewestClearOrCompactSeq("s1"); got != 30 {
		t.Fatalf("replay floor = %d, want 30", got)
	}
}

func TestObservingACompactionRecordsTheReplayFloor(t *testing.T) {
	// Arrange — a compaction discards the history that preceded it just as a
	// clear does, so it floors a replay for exactly the same reason.
	h := newRepullHarness(t, &replayClient{})

	// Act.
	h.driver(t).consumer.Consume(compactEvent(44, "b-1"))

	// Assert.
	if got := h.floors.NewestClearOrCompactSeq("s1"); got != 44 {
		t.Fatalf("replay floor = %d, want 44", got)
	}
}

func TestTheNewerOfAClearAndACompactionWinsTheFloor(t *testing.T) {
	// Arrange — one seq holds the mark, so whichever came last must win. The
	// older one is already below the floor the newer one sets.
	h := newRepullHarness(t, &replayClient{})
	cons := h.driver(t).consumer

	// Act — the clear is newer.
	cons.Consume(compactEvent(44, "b-1"))
	cons.Consume(clearEvent(90, "u-clear"))

	// Assert.
	if got := h.floors.NewestClearOrCompactSeq("s1"); got != 90 {
		t.Fatalf("replay floor = %d, want 90", got)
	}
}

func TestAnOlderClearNeverLowersTheFloor(t *testing.T) {
	// Arrange — a re-delivery after a reattach can present one already
	// recorded; lowering the floor there would replay history the newer
	// compaction had already made irrelevant.
	h := newRepullHarness(t, &replayClient{})
	cons := h.driver(t).consumer

	// Act.
	cons.Consume(compactEvent(90, "b-1"))
	cons.Consume(clearEvent(30, "u-clear"))

	// Assert.
	if got := h.floors.NewestClearOrCompactSeq("s1"); got != 90 {
		t.Fatalf("replay floor = %d, want 90 — an older event must not lower the floor", got)
	}
}

func TestASeqlessClearDoesNotRaiseTheFloor(t *testing.T) {
	// Arrange — seq 0 means the event never reached the store, so it has no
	// position to floor at. A floor derived from it would be no floor at all.
	h := newRepullHarness(t, &replayClient{})

	// Act.
	h.driver(t).consumer.Consume(clearEvent(0, "u-clear"))

	// Assert.
	if got := h.floors.NewestClearOrCompactSeq("s1"); got != 0 {
		t.Fatalf("replay floor = %d, want 0", got)
	}
}

func TestASeqlessClearIsLoudRatherThanSilentlyForgotten(t *testing.T) {
	// Arrange — a clear the daemon saw but cannot position is a real anomaly,
	// never "nothing happened".
	var logged []string
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(),
		func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) }, nil, nil, nil, nil, nil)

	// Act.
	c.Consume(clearEvent(0, "u-clear"))

	// Assert.
	var found bool
	for _, line := range logged {
		if strings.Contains(line, "replay floor NOT raised") {
			found = true
		}
	}
	if !found {
		t.Fatalf("logged = %v, want a loud line about the unraisable replay floor", logged)
	}
}

// --- a clear and a compaction each reach the frontend -----------------------

func TestAClearIsPushedAsAConversationItem(t *testing.T) {
	// Arrange — the floor is only half the job: a frontend that discards its
	// history and receives no bubble has nothing to show for it.
	h := newRepullHarness(t, &replayClient{})

	// Act.
	h.driver(t).consumer.Consume(clearEvent(30, "u-clear"))

	// Assert.
	if got := clearItems(h.push); len(got) != 1 {
		t.Fatalf("pushed %d context_cleared items, want 1", len(got))
	}
}

func TestACompactionIsPushedAsAConversationItem(t *testing.T) {
	// Arrange.
	h := newRepullHarness(t, &replayClient{})

	// Act.
	h.driver(t).consumer.Consume(compactEvent(44, "b-1"))

	// Assert.
	if got := compactItems(h.push); len(got) != 1 {
		t.Fatalf("pushed %d context_compacted items, want 1", len(got))
	}
}

// --- Resync honors the floor ------------------------------------------------

func TestResyncWithNoClearOrCompactionReplaysFromTheClientMark(t *testing.T) {
	// Arrange — nothing to floor at, so the client's own mark stands.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.driver(t).consumer.Consume(assistantEvent(t, 10, "u10"))

	// Act — the frontend asks from below the ring, so the gap is re-pulled.
	if err := h.m.Resync("ws", 4); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the re-pull's EXCLUSIVE lower bound is one below the client mark.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{3, 10} {
		t.Fatalf("replay calls = %v, want one [3 10]", client.calls)
	}
}

func TestResyncFloorsAtAClearRatherThanTheClientMark(t *testing.T) {
	// Arrange — the client is far behind a clear. Everything it asks for below
	// the clear is history the clear discarded.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 300)
	h.driver(t).consumer.Consume(assistantEvent(t, 400, "u400"))

	// Act.
	if err := h.m.Resync("ws", 5); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the re-pull starts at the clear, not at 5.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{299, 400} {
		t.Fatalf("replay calls = %v, want one [299 400] — the floor must replace the client mark", client.calls)
	}
}

func TestResyncFloorsAtACompactionRatherThanTheClientMark(t *testing.T) {
	// Arrange — the same must hold for a compaction; there is no reason to
	// floor on one of the pair and not the other. The compaction is still in
	// the live ring here, so the whole floored request is served from it and no
	// store re-pull is needed at all.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	cons := h.driver(t).consumer
	cons.Consume(assistantEvent(t, 100, "u100"))
	cons.Consume(compactEvent(300, "b-1"))
	cons.Consume(assistantEvent(t, 400, "u400"))
	h.push.mu.Lock()
	h.push.convo = nil // drop the live pushes; only the replay is under test
	h.push.mu.Unlock()

	// Act — the client asks from far below the compaction.
	if err := h.m.Resync("ws", 5); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — nothing below the compaction comes back, and no gap is re-pulled
	// because the floor is already inside the ring.
	for _, seq := range replayedSeqs(h.push) {
		if seq < 300 {
			t.Fatalf("replayed seq %d, below the compaction at 300 — its discarded history must never be sent", seq)
		}
	}
	if client.callCount() != 0 {
		t.Fatalf("re-pulled %d time(s); the ring already covered everything from the floor", client.callCount())
	}
}

func TestResyncReplaysTheCompactionItselfFromTheRing(t *testing.T) {
	// Arrange — the compaction bubble carries the summary that stands in for
	// the history it discarded, so it is the one item a floored replay must
	// never omit.
	h := newRepullHarness(t, &replayClient{})
	cons := h.driver(t).consumer
	cons.Consume(assistantEvent(t, 100, "u100"))
	cons.Consume(compactEvent(300, "b-1"))
	h.push.mu.Lock()
	h.push.convo = nil
	h.push.mu.Unlock()

	// Act.
	if err := h.m.Resync("ws", 5); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := compactItems(h.push); len(got) != 1 {
		t.Fatalf("replayed %d context_compacted items, want 1", len(got))
	}
}

func TestTheRePullBoundIsExclusiveSoTheFloorEventIsItselfReplayed(t *testing.T) {
	// Arrange — THE off-by-one. A core.v1 ReplayRequest's from_seq is an
	// EXCLUSIVE lower bound, so a floor of 300 must be asked for as 299. Passed
	// straight through, the clear itself would be the one thing missing from
	// the replay — in exactly the case that matters most, a restarted daemon
	// whose ring is empty.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 300)
	h.seq.SetLastSeq("s1", 399) // empty ring: the floor comes from the durable mark

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0][0] != 299 {
		t.Fatalf("replay calls = %v, want a from_seq of 299 (exclusive of 299 = inclusive of the floor at 300)", client.calls)
	}
}

func TestTheRingReplayIncludesTheFloorEventItself(t *testing.T) {
	// Arrange — the same inclusivity on the OTHER replay path: the live ring.
	// The clear at the floor must come back, or the frontend discards its
	// history with no bubble to show for it.
	h := newRepullHarness(t, &replayClient{})
	cons := h.driver(t).consumer
	cons.Consume(assistantEvent(t, 10, "u10"))
	cons.Consume(clearEvent(20, "u-clear"))
	cons.Consume(assistantEvent(t, 30, "u30"))
	h.push.mu.Lock()
	h.push.convo = nil // drop the live pushes; only the replay is under test
	h.push.mu.Unlock()

	// Act — the client is far behind, so the floor at 20 governs.
	if err := h.m.Resync("ws", 1); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := clearItems(h.push); len(got) != 1 {
		t.Fatalf("replayed %d context_cleared items, want 1 — the floor event must itself be replayed", len(got))
	}
}

func TestTheRingReplayDropsHistoryBelowTheFloor(t *testing.T) {
	// Arrange — the other half of the same rule: history the clear discarded is
	// never sent, so replay is idempotent by construction.
	h := newRepullHarness(t, &replayClient{})
	cons := h.driver(t).consumer
	cons.Consume(assistantEvent(t, 10, "u10"))
	cons.Consume(clearEvent(20, "u-clear"))
	cons.Consume(assistantEvent(t, 30, "u30"))
	h.push.mu.Lock()
	h.push.convo = nil
	h.push.mu.Unlock()

	// Act.
	if err := h.m.Resync("ws", 1); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — u10 sat below the clear and must not come back.
	for _, seq := range replayedSeqs(h.push) {
		if seq < 20 {
			t.Fatalf("replayed seq %d, below the floor at 20 — the clear's discarded history must never be sent", seq)
		}
	}
}

func TestAClientAheadOfTheFloorKeepsItsOwnMark(t *testing.T) {
	// Arrange — a client already past the clear must not be dragged BACK to it,
	// or a reconnect would re-send everything it has drawn since.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 300)
	h.driver(t).consumer.Consume(assistantEvent(t, 400, "u400"))

	// Act — the client is one AHEAD of the floor.
	if err := h.m.Resync("ws", 301); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{300, 400} {
		t.Fatalf("replay calls = %v, want one [300 400] — the client's own mark must stand", client.calls)
	}
}

func TestAClientOneBehindTheFloorIsRaisedToIt(t *testing.T) {
	// Arrange — the tight edge on the other side.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 300)
	h.driver(t).consumer.Consume(assistantEvent(t, 400, "u400"))

	// Act.
	if err := h.m.Resync("ws", 299); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{299, 400} {
		t.Fatalf("replay calls = %v, want one [299 400] — a client one behind must be raised to the floor", client.calls)
	}
}

func TestAClientExactlyAtTheFloorStillReceivesTheFloorEvent(t *testing.T) {
	// Arrange — max(300, 300) is 300, and 300 is INCLUSIVE, so the clear comes
	// back rather than being treated as already delivered.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 300)
	h.driver(t).consumer.Consume(assistantEvent(t, 400, "u400"))

	// Act.
	if err := h.m.Resync("ws", 300); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{299, 400} {
		t.Fatalf("replay calls = %v, want one [299 400]", client.calls)
	}
}

func TestTheFloorSurvivesADaemonRestart(t *testing.T) {
	// Arrange — the whole reason the mark is a durable store rather than driver
	// state. The daemon re-Subscribes from last_seen_seq, so a clear observed
	// before the restart is never re-delivered; an in-memory floor would revert
	// to "nothing seen" and replay the entire discarded conversation.
	floors := newFakeClearCompactStore()
	first := newRestartHarness(t, floors, &replayClient{})
	first.driver(t).consumer.Consume(clearEvent(300, "u-clear"))
	first.m.Close()

	// Act — a fresh Manager over the SAME durable store, as a restart gives.
	client := &replayClient{}
	second := newRestartHarness(t, floors, client)
	second.seq.SetLastSeq("s1", 399) // empty ring after the restart
	if err := second.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0][0] != 299 {
		t.Fatalf("replay calls = %v, want a from_seq of 299 — the floor must outlive the daemon", client.calls)
	}
}

func TestAManagerWithoutAClearCompactStoreIsRejected(t *testing.T) {
	// Arrange — a driver that observed a clear and had nowhere to record it
	// would serve the next resync the very history that clear discarded.
	cfg := Config{
		Push: &fakePusher{}, SSM: &fakeApplier{}, Spawner: &fakeSpawner{},
		Locator: fakeLocator{m: map[string]string{}}, SeqStore: &fakeSeqStore{seq: map[string]uint64{}},
		Source: stubSource{}, ProtocolVersion: "1",
	}

	// Act.
	_, err := New(cfg)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "ClearCompactStore") {
		t.Fatalf("New err = %v, want a rejection naming ClearCompactStore", err)
	}
}

// --- helpers ----------------------------------------------------------------

// newRestartHarness builds a driver over a CALLER-OWNED floor store, so two
// harnesses can model the same conversation across a daemon restart.
func newRestartHarness(t *testing.T, floors *fakeClearCompactStore, client *replayClient) *repullHarness {
	t.Helper()
	h := &repullHarness{
		push: &fakePusher{}, applier: &fakeApplier{}, progress: &fakeProgress{},
		client: client, seq: &fakeSeqStore{seq: map[string]uint64{}}, floors: floors,
	}
	m, err := New(Config{
		Push: h.push, SSM: h.applier, Progress: h.progress, Spawner: &fakeSpawner{},
		Locator: fakeLocator{m: map[string]string{"ws": "s1"}}, SeqStore: h.seq,
		ClearCompactStore: floors, ProtocolVersion: "1", Source: stubSource{},
		FileDiagnostics: fakeFileDiagnosticPersister{},
		newClient:       func(shimclient.Config) sessionClient { return client },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	h.m = m
	return h
}

// clearItems collects every pushed context_cleared conversation item.
func clearItems(p *fakePusher) []*frontendv1.ConversationItem {
	p.mu.Lock()
	defer p.mu.Unlock()
	var out []*frontendv1.ConversationItem
	for _, cd := range p.convo {
		for _, item := range cd.GetItems() {
			if _, ok := item.GetItem().(*frontendv1.ConversationItem_ContextCleared); ok {
				out = append(out, item)
			}
		}
	}
	return out
}

// compactItems collects every pushed context_compacted conversation item.
func compactItems(p *fakePusher) []*frontendv1.ConversationItem {
	p.mu.Lock()
	defer p.mu.Unlock()
	var out []*frontendv1.ConversationItem
	for _, cd := range p.convo {
		for _, item := range cd.GetItems() {
			if _, ok := item.GetItem().(*frontendv1.ConversationItem_ContextCompacted); ok {
				out = append(out, item)
			}
		}
	}
	return out
}

// replayedSeqs lists the through_seq of every pushed conversation delta.
func replayedSeqs(p *fakePusher) []uint64 {
	p.mu.Lock()
	defer p.mu.Unlock()
	out := make([]uint64, 0, len(p.convo))
	for _, cd := range p.convo {
		out = append(out, cd.GetThroughSeq())
	}
	return out
}
