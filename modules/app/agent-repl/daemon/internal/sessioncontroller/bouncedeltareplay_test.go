package sessioncontroller

import (
	"testing"
)

// ---------------------------------------------------------------------------
// RECOVERY AFTER A BOUNCE IS THE DELTA, NEVER THE WHOLE CONVERSATION.
//
// A page that survives a bounce still holds every item it has applied. What it
// is owed is what happened while the socket was down — and nothing else.
// Serving it the whole segment from seq 0 costs hundreds of events per
// workspace per bounce to re-deliver history it never lost.
//
// replayfloor_test.go pins the FLOOR rule in general. These pin it against the
// two bounce shapes specifically, because "the seq space is continuous across a
// bounce" is the premise the delta rests on, and a premise nobody tested is a
// premise that quietly stops being true.
// ---------------------------------------------------------------------------

func TestAReconnectAfterABounceReplaysOnlyTheDelta(t *testing.T) {
	// Arrange — the page applied through seq 400 before the bounce, and the
	// conversation moved on to 460 while it was away.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.controller(t).consumer.Consume(assistantEvent(t, 460, "u460"))

	// Act — it comes back and asks from its own high-water mark.
	if err := h.m.Resync("ws", 400); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the RANGE is the assertion. An exclusive lower bound of 399
	// means seq 400 itself is re-sent (the page's own mark is inclusive) and
	// nothing below it is, so the 399 events the page already holds are not
	// re-delivered.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{399, 460} {
		t.Fatalf("replay calls = %v, want one [399 460] — only the delta above the page's mark", client.calls)
	}
}

func TestAShimRespawnLeavesTheSeqSpaceContinuous(t *testing.T) {
	// Arrange — THE PREMISE, stated as a test.
	//
	// A bounce that has to take the shim with it respawns it with --resume. The
	// new shim re-ingests the vendor's context, but the DAEMON-side store
	// sequence space for this conversation is a different thing entirely: it is
	// keyed by the session, it is durable, and nothing about spawning a new
	// shim process renumbers it.
	//
	// If it ever did, a client mark counted before the respawn would read as a
	// number from a retired space and the page would be served a full replay.
	// This asserts the continuity rather than the symptom, so the defect is
	// caught where it would be introduced.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	// Everything the conversation produced before the bounce.
	h.seq.SetLastSeq("s1", 400)

	// Act — the successor daemon's shim has respawned and the conversation has
	// moved on. The page asks from the mark it held before the bounce.
	h.controller(t).consumer.Consume(assistantEvent(t, 460, "u460"))
	if err := h.m.Resync("ws", 400); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the mark is honored, which is only possible if the space is
	// continuous. A renumbered space would have put the mark above every seq
	// the conversation had produced, and the floor would have refused to trust
	// it.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{399, 460} {
		t.Fatalf("replay calls = %v, want one [399 460] — a shim respawn must not renumber the seq space", client.calls)
	}
}

func TestAMarkAboveEverythingTheConversationProducedIsNotTrusted(t *testing.T) {
	// Arrange — the counterpart, and the reason the test above is meaningful. A
	// mark this conversation cannot have produced is evidence of a RETIRED seq
	// space (a vendor uuid rotation restarts numbering at 1), and believing it
	// would serve the page nothing at all — the clear that caused the rotation
	// included.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 3)
	h.controller(t).consumer.Consume(assistantEvent(t, 12, "u12"))

	// Act — 1060 is a mark from the space that rotated away.
	if err := h.m.Resync("ws", 1060); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — floored at the rotation's own clear, not believed.
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0][0] != 2 {
		t.Fatalf("replay calls = %v, want a from_seq of 2 — an impossible mark is floored, never trusted", client.calls)
	}
}
