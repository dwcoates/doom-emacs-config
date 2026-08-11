package sessioncontroller

import (
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/errclass"
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
		TurnAccountings:   emptyTurnAccountingStore{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
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

func TestAMarkAboveEverySeqIsREFUSEDRatherThanReplayed(t *testing.T) {
	// Arrange — the rotated conversation's new space holds a clear at 12 and one
	// message above it. The client's mark belongs to the space that clear
	// retired, so there is no delta above it to serve.
	h := newStaleEpochHarness(t, &replayClient{})
	cons := h.controller(t).consumer
	cons.Consume(clearEvent(12, "u-clear"))
	cons.Consume(assistantEvent(t, 13, "u13"))
	h.push.mu.Lock()
	h.push.convo = nil // drop the live pushes; only the replay is under test
	h.push.mu.Unlock()

	// Act
	err := h.m.Resync("ws", 1060)

	// Assert — REFUSED, not answered. Flooring the mark and serving everything
	// above the floor is a replay of the WHOLE conversation, which is the
	// backfill paging exists to end; the client re-anchors from a tail page on
	// this refusal instead.
	if !errors.Is(err, errclass.ErrReplayMarkRetired) {
		t.Fatalf("Resync from a retired-space mark returned %v, want ErrReplayMarkRetired", err)
	}
}

func TestARefusedRetiredMarkServesNoConversationAtAll(t *testing.T) {
	// Arrange — the same rotated conversation, with the assertion turned on the
	// thing the user actually reported: bytes on the wire.
	h := newStaleEpochHarness(t, &replayClient{})
	cons := h.controller(t).consumer
	cons.Consume(clearEvent(12, "u-clear"))
	cons.Consume(assistantEvent(t, 13, "u13"))
	h.push.mu.Lock()
	h.push.convo = nil
	h.push.mu.Unlock()

	// Act
	_ = h.m.Resync("ws", 1060)

	// Assert — not one item. A refusal that still pushed history would be the
	// full replay with an error stapled to it.
	h.push.mu.Lock()
	pushed := len(h.push.convo)
	h.push.mu.Unlock()
	if pushed != 0 {
		t.Fatalf("a REFUSED resync pushed %d conversation delta(s), want 0", pushed)
	}
}

func TestARefusedRetiredMarkNamesTheCauseTheClientMatchesOn(t *testing.T) {
	// Arrange — the webapp decides to re-anchor by matching the cause token, so
	// the token is contract, not prose.
	h := newStaleEpochHarness(t, &replayClient{})
	h.controller(t).consumer.Consume(clearEvent(12, "u-clear"))

	// Act
	err := h.m.Resync("ws", 1060)

	// Assert
	if err == nil || !strings.Contains(err.Error(), `rejection_cause="retired_seq_space"`) {
		t.Fatalf("refusal = %v, want it to carry rejection_cause=\"retired_seq_space\"", err)
	}
}

func TestAMarkAboveEverySeqIsLoudAboutTheRetiredSpace(t *testing.T) {
	// Arrange — a mark the daemon refuses to believe is a real anomaly, and the
	// silent version of this ruling is unattributable in a production log.
	h := newStaleEpochHarness(t, &replayClient{})
	h.controller(t).consumer.Consume(clearEvent(12, "u-clear"))

	// Act
	_ = h.m.Resync("ws", 1060)

	// Assert
	var found bool
	for _, line := range h.lines() {
		if strings.Contains(line, "RETIRED store seq space") || strings.Contains(line, "rejection_cause=\"retired_seq_space\"") {
			found = true
		}
	}
	if !found {
		t.Fatalf("logged = %v, want a loud line naming the retired seq space refusal", h.lines())
	}
}

func TestAnInSpaceMarkStillGetsItsDelta(t *testing.T) {
	// Arrange — THE OTHER HALF OF THE RULE. A mark the live space can honor is
	// untouched by the refusal: it still takes the incremental delta path, which
	// is the whole reason the refusal is narrow.
	h := newStaleEpochHarness(t, &replayClient{})
	cons := h.controller(t).consumer
	cons.Consume(assistantEvent(t, 12, "u12"))
	cons.Consume(assistantEvent(t, 13, "u13"))
	h.push.mu.Lock()
	h.push.convo = nil
	h.push.mu.Unlock()

	// Act — 12 is in-space, and 13 is the delta above it.
	if err := h.m.Resync("ws", 12); err != nil {
		t.Fatalf("Resync from an in-space mark: %v", err)
	}

	// Assert
	h.push.mu.Lock()
	pushed := len(h.push.convo)
	h.push.mu.Unlock()
	if pushed == 0 {
		t.Fatal("an in-space mark was served nothing; the delta path must be untouched by the retired-mark refusal")
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
	h.controller(t).consumer.Consume(assistantEvent(t, 400, "u400"))

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

// TestTheRetiredMarkRefusalIsMeasuredAgainstWhatItReplaced is the SIZE of the
// change, asserted rather than asserted-about.
//
// The old rule and the new one are both evaluated here on one conversation, so
// the test cannot drift from the claim it makes: `replayFloorAt` is the floor
// the daemon USED to serve a retired mark from — zero, for a conversation the
// rotation restarted — and every retained event at or above it is what the
// frontend was sent. The refusal sends none of them.
func TestTheRetiredMarkRefusalIsMeasuredAgainstWhatItReplaced(t *testing.T) {
	// Arrange — a conversation of 250 items in the LIVE space, and a client
	// holding a mark from the space that rotated away.
	const items = 250
	h := newStaleEpochHarness(t, &replayClient{})
	cons := h.controller(t).consumer
	for seq := uint64(1); seq <= items; seq++ {
		cons.Consume(assistantEvent(t, seq, fmt.Sprintf("u%d", seq)))
	}
	h.push.mu.Lock()
	h.push.convo = nil // drop the live pushes; only the replay is under test
	h.push.mu.Unlock()

	// Act — the floor the OLD rule would have replayed from, then the refusal.
	oldFloor := h.m.replayFloorAt("ws", "s1", items, 1060)
	err := h.m.Resync("ws", 1060)

	// Assert — the old floor is zero, which is the whole conversation; the new
	// answer is a refusal that serves none of it.
	if oldFloor != 0 {
		t.Fatalf("the old floor for a retired mark = %d, want 0 — the premise of this measurement is that it replayed everything", oldFloor)
	}
	if !errors.Is(err, errclass.ErrReplayMarkRetired) {
		t.Fatalf("Resync = %v, want ErrReplayMarkRetired", err)
	}
	h.push.mu.Lock()
	pushed := len(h.push.convo)
	h.push.mu.Unlock()
	if pushed != 0 {
		t.Fatalf("replay served %d delta(s) of a %d-item conversation, want 0", pushed, items)
	}
	t.Logf("retired-mark reconnect: replay floor was %d over a %d-item conversation (the whole of it); served now = %d deltas", oldFloor, items, pushed)
}
