package sessiondrv

import (
	"fmt"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/shimclient"
)

// A REPLAY MARK FROM A RETIRED SEQ SPACE.
//
// A `/clear` rotates the vendor session uuid, which retires one store seq space
// and starts another at 1. A frontend that was connected across the rotation
// still holds a mark counted in the retired space — the production observation
// was a webview acking through_seq=1060 while the new space had reached 12 — and
// read as an ordinary client mark it means "already past everything", so the
// replay serves nothing at all: no clear line, no post-rotation history.
//
// Such a mark is impossible IN-SPACE (the daemon records last_seen_seq before
// forwarding an event, so a frontend can never hold a seq the conversation has
// not produced), which is what lets the daemon recognize it rather than guess.

// staleEpochHarness is newRepullHarness plus a log sink: the retired-space
// ruling is a decision the daemon makes silently in the wire path, so its line
// is part of the contract under test.
type staleEpochHarness struct {
	*repullHarness
	mu     sync.Mutex
	logged []string
}

func (h *staleEpochHarness) lines() []string {
	h.mu.Lock()
	defer h.mu.Unlock()
	out := make([]string, len(h.logged))
	copy(out, h.logged)
	return out
}

func newStaleEpochHarness(t *testing.T, client *replayClient) *staleEpochHarness {
	t.Helper()
	h := &staleEpochHarness{repullHarness: &repullHarness{
		push:     &fakePusher{},
		applier:  &fakeApplier{},
		progress: &fakeProgress{},
		client:   client,
		seq:      &fakeSeqStore{seq: map[string]uint64{}},
		floors:   newFakeClearCompactStore(),
	}}
	m, err := New(Config{
		Push:              h.push,
		SSM:               h.applier,
		Progress:          h.progress,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          h.seq,
		ClearCompactStore: h.floors,
		ProtocolVersion:   "1",
		Source:            stubSource{},
		Logf: func(format string, args ...any) {
			h.mu.Lock()
			h.logged = append(h.logged, fmt.Sprintf(format, args...))
			h.mu.Unlock()
		},
		newClient: func(shimclient.Config) sessionClient { return client },
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

func TestAMarkAboveEverySeqIsFlooredAtTheClearRatherThanTrusted(t *testing.T) {
	// Arrange — the rotated conversation's new space holds a clear at 12 and one
	// message above it. The client's mark belongs to the space that clear
	// retired.
	h := newStaleEpochHarness(t, &replayClient{})
	cons := h.driver(t).consumer
	cons.Consume(clearEvent(12, "u-clear"))
	cons.Consume(assistantEvent(t, 13, "u13"))
	h.push.mu.Lock()
	h.push.convo = nil // drop the live pushes; only the replay is under test
	h.push.mu.Unlock()

	// Act
	if err := h.m.Resync("ws", 1060); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the clear that caused the rotation is exactly what a mark trusted
	// as "past everything" would have withheld.
	if got := clearItems(h.push); len(got) != 1 {
		t.Fatalf("replayed %d context_cleared items, want 1 — a retired-space mark must not floor the clear away", len(got))
	}
}

func TestAMarkAboveEverySeqIsLoudAboutTheRetiredSpace(t *testing.T) {
	// Arrange — a mark the daemon refuses to believe is a real anomaly, and the
	// silent version of this ruling is unattributable in a production log.
	h := newStaleEpochHarness(t, &replayClient{})
	h.driver(t).consumer.Consume(clearEvent(12, "u-clear"))

	// Act
	if err := h.m.Resync("ws", 1060); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert
	var found bool
	for _, line := range h.lines() {
		if strings.Contains(line, "RETIRED seq space") {
			found = true
		}
	}
	if !found {
		t.Fatalf("logged = %v, want a loud line naming the retired seq space", h.lines())
	}
}

func TestAMarkAtTheNewestSeqKeepsItsOwnFloor(t *testing.T) {
	// Arrange — THE TIGHT EDGE on the honest side. A client caught up to the
	// conversation's newest seq is in-space by definition, so the normal
	// inclusive-floor semantics must still hold for it: its own mark stands and
	// nothing already drawn is re-sent.
	client := &replayClient{}
	h := newStaleEpochHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 300)
	h.driver(t).consumer.Consume(assistantEvent(t, 400, "u400"))

	// Act — 400 is the newest seq the conversation has produced.
	if err := h.m.Resync("ws", 400); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the ring covers everything from 400, so no gap is re-pulled and
	// the client is not dragged back to the clear at 300.
	if n := client.callCount(); n != 0 {
		t.Fatalf("re-pulled %d time(s); a caught-up in-space client owes no replay at all", n)
	}
}
