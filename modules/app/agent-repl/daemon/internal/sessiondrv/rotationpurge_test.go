package sessiondrv

import (
	"errors"
	"fmt"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/shimclient"
)

// ---------------------------------------------------------------------------
// THE ROTATION'S PURGE, and the re-pull that survives the rotation's own
// shim-link bounce.
//
// A store seq means something only inside ONE vendor seq space. When the vendor
// retires a session uuid the daemon resets its durable cursors — and used to
// leave the RETAINED CONVERSATION RING standing, so every ceiling derived from
// it (the re-pull's stop_at, the honest-mark ceiling) went on reporting numbers
// from a conversation that no longer exists. The observed production failure
// was a resync answered with a re-pull bounded at `stop_at=1122` against a space
// that had reached 12, which delivered nothing and reported a truncation.
// ---------------------------------------------------------------------------

// rotatingRegistrar is the fakeRegistrar with the production adapter's
// LOAD-BEARING side effect: RegistryRegistrar.AdoptVendorSessionID zeroes the
// conversation's cursors in the same write that adopts a new uuid. A fake
// without that reset is more forgiving than production, and a rotation test
// against it would pass while the real daemon subscribed into a retired space.
type rotatingRegistrar struct {
	fakeRegistrar
	seq    *fakeSeqStore
	floors *fakeClearCompactStore
}

func (r *rotatingRegistrar) AdoptVendorSessionID(sessionID, csid string) (bool, string) {
	rotated, previous := r.fakeRegistrar.AdoptVendorSessionID(sessionID, csid)
	if rotated {
		r.seq.SetLastSeq(sessionID, 0)
		r.floors.resetForRotation(sessionID)
	}
	return rotated, previous
}

// resetForRotation zeroes a conversation's replay floor. It bypasses the
// MONOTONIC setter deliberately: monotonicity is a rule about ONE seq space,
// and this is the moment that space is retired.
func (f *fakeClearCompactStore) resetForRotation(sessionID string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.seq[sessionID] = 0
}

// rotate drives a vendor session rotation through the same handshake hook the
// shim's re-handshake drives, so the reconciliation is exercised on its real
// trigger rather than by poking fields.
func (h *repullHarness) rotate(previous, next string) {
	h.m.onHandshake("ws", "s1", &corev1.ShimHello{
		SessionId: "s1", Vendor: "claude", ShimVersion: "test", ProtocolVersion: "1",
		VendorSessionId: previous,
	})
	h.m.onHandshake("ws", "s1", &corev1.ShimHello{
		SessionId: "s1", Vendor: "claude", ShimVersion: "test", ProtocolVersion: "1",
		VendorSessionId: next,
	})
}

// linkLost is the error a replay abandoned by a shim-link bounce comes back
// with — the shape the shim's own deliberate re-handshake produces.
func linkLost() error {
	return fmt.Errorf("%w: request_id=replay-1 (session s1): shim connection closed: context canceled",
		shimclient.ErrReplayLinkLost)
}

// pendingResyncFrom reports the re-armed resync's client mark, and whether one
// is armed at all. The driver is resolved FIRST because existing() takes the
// same lock.
func (h *repullHarness) pendingResyncFrom(t *testing.T) (uint64, bool) {
	t.Helper()
	d := h.driver(t)
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	if d.pendingResync == nil {
		return 0, false
	}
	return d.pendingResync.fromSeq, true
}

// --- the purge --------------------------------------------------------------

func TestRotationEmptiesTheRetainedRing(t *testing.T) {
	// Arrange — a retained item from the space the rotation is about to retire.
	h := newRepullHarness(t, &replayClient{})
	h.driver(t).consumer.Consume(assistantEvent(t, 1122, "retired"))

	// Act
	h.rotate("uuid-old", "uuid-new")

	// Assert
	if got := len(h.driver(t).consumer.snapshotRing()); got != 0 {
		t.Fatalf("ring holds %d retired-space event(s) after the rotation, want 0", got)
	}
}

func TestRotationResetsTheRetainedSeqCeiling(t *testing.T) {
	// Arrange — the ring's newest seq is the CEILING an honest client mark is
	// checked against (Manager.lastSeenSeq). A retired-space ceiling makes a
	// new-space mark look ordinary.
	h := newRepullHarness(t, &replayClient{})
	h.driver(t).consumer.Consume(assistantEvent(t, 1122, "retired"))

	// Act
	h.rotate("uuid-old", "uuid-new")

	// Assert
	if got := h.driver(t).consumer.newestRetainedSeq(); got != 0 {
		t.Fatalf("retained seq ceiling = %d after the rotation, want 0", got)
	}
}

func TestRotationPurgeIsAnnouncedLoudly(t *testing.T) {
	// Arrange — a silent purge is indistinguishable from the defect it fixes,
	// and the log line is the only account of what was dropped.
	cl := &logCapture{}
	h := newRepullHarnessWithLog(t, &replayClient{}, cl.logf)
	h.driver(t).consumer.Consume(assistantEvent(t, 1122, "retired"))

	// Act
	h.rotate("uuid-old", "uuid-new")

	// Assert
	if !cl.contains("retained conversation ring PURGED") {
		t.Fatal("the ring purge passed SILENTLY")
	}
}

func TestPostRotationResyncReplaysNoRetiredSpaceItem(t *testing.T) {
	// Arrange — the retired conversation is exactly what the frontend just
	// discarded; replaying it back is the opposite of serving the rotation.
	h := newRepullHarness(t, &replayClient{})
	h.driver(t).consumer.Consume(assistantEvent(t, 1122, "retired"))
	h.rotate("uuid-old", "uuid-new")
	h.driver(t).consumer.Consume(assistantEvent(t, 2, "new-space"))
	h.push.mu.Lock()
	h.push.convo = nil
	h.push.mu.Unlock()

	// Act — the rebased client asks from the beginning of the new space.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	for _, cd := range h.push.convo {
		for _, item := range cd.GetItems() {
			if item.GetUuid() == "retired" {
				t.Fatal("the resync replayed an item from the RETIRED seq space")
			}
		}
	}
}

func TestPostRotationRePullIsBoundedInTheNewSeqSpace(t *testing.T) {
	// Arrange — this is THE production defect: `stop_at` was read off a ring
	// still holding the retired space, so the re-pull was ceilinged at 1122
	// against a space that had reached 2.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.driver(t).consumer.Consume(assistantEvent(t, 1122, "retired"))
	h.rotate("uuid-old", "uuid-new")
	h.driver(t).consumer.Consume(assistantEvent(t, 2, "new-space"))

	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{0, 2} {
		t.Fatalf("replay calls = %v, want one [0 2] — the ceiling must come from the NEW seq space", client.calls)
	}
}

func TestRePullFromARetiredSpaceIsNeverCoalescedOnto(t *testing.T) {
	// Arrange — a re-pull whose bounds were computed before the rotation covers
	// nothing in the space that replaced it, however the numbers compare.
	client := &replayClient{block: make(chan struct{})}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 100)
	started := make(chan struct{})
	go func() {
		close(started)
		_ = h.m.Resync("ws", 0)
	}()
	<-started
	waitFor(t, "the first replay to start", func() bool { return client.callCount() == 1 })
	h.rotate("uuid-old", "uuid-new")

	// Act
	err := h.m.Resync("ws", 5)
	close(client.block)

	// Assert
	if !errors.Is(err, ErrRepullInFlight) {
		t.Fatalf("err = %v, want ErrRepullInFlight for a re-pull bounded in a retired seq space", err)
	}
}

// --- the rotation-safe re-pull ----------------------------------------------

func TestRePullInterruptedByALinkBounceIsNotReportedAsAFailure(t *testing.T) {
	// Arrange — the shim bounces the daemon link DELIBERATELY on a rotation. A
	// failure card about that, in a feed the rotation just emptied, is the pair
	// of symptoms the user saw: nothing to read and an alarm about it.
	client := &replayClient{queuedErrs: []error{linkLost()}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)

	// Act
	err := h.m.Resync("ws", 0)

	// Assert
	if err != nil {
		t.Fatalf("Resync = %v, want nil: a re-pull the shim's own re-handshake interrupted is re-armed, not failed", err)
	}
}

func TestRePullInterruptedByALinkBounceIsReArmed(t *testing.T) {
	// Arrange — the client's question was never answered, so it is retained to
	// be re-asked rather than answered wrongly.
	client := &replayClient{queuedErrs: []error{linkLost()}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)

	// Act
	_ = h.m.Resync("ws", 7)

	// Assert — the CLIENT's mark is what is held, not the floored position.
	from, armed := h.pendingResyncFrom(t)
	if !armed || from != 7 {
		t.Fatalf("pending resync = (%d, %v), want the client mark 7 armed", from, armed)
	}
}

func TestAReArmedResyncIsServedWhenTheShimReattaches(t *testing.T) {
	// Arrange — the reattach IS the event the re-arm waits for: nothing sleeps
	// and nothing polls.
	client := &replayClient{queuedErrs: []error{linkLost()}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	_ = h.m.Resync("ws", 0)

	// Act
	h.m.onConnected("ws", "s1", &corev1.ShimHello{SessionId: "s1"})

	// Assert
	waitFor(t, "the re-armed resync to reach the shim", func() bool { return client.callCount() == 2 })
}

func TestASecondConsecutiveLostLinkIsSurfaced(t *testing.T) {
	// Arrange — a link that will not stay up long enough to serve history is a
	// real outage, not a rotation. Retrying it forever is a fallback.
	client := &replayClient{queuedErrs: []error{linkLost(), linkLost()}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("the FIRST lost link must be re-armed, got %v", err)
	}

	// Act
	err := h.m.Resync("ws", 0)

	// Assert
	if !errors.Is(err, shimclient.ErrReplayLinkLost) {
		t.Fatalf("err = %v, want the second consecutive lost link surfaced", err)
	}
}

func TestAGenuineTruncationIsNeverReArmed(t *testing.T) {
	// Arrange — a bound the shim actually tripped is its verdict on the range:
	// "this is all you are getting". Retrying it would ask the same question and
	// trip the same bound, and hiding it would present a partial answer as whole.
	client := &replayClient{result: shimclient.ReplayResult{Delivered: 12, Truncated: true, Reason: "hit the cap"}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)

	// Act
	err := h.m.Resync("ws", 0)

	// Assert
	if !errors.Is(err, ErrRepullTruncated) {
		t.Fatalf("err = %v, want ErrRepullTruncated", err)
	}
	if _, armed := h.pendingResyncFrom(t); armed {
		t.Fatal("a genuine truncation re-armed a retry")
	}
}

// compile-time proof the rotation-aware fake still satisfies the interface the
// driver actually depends on.
var _ SessionRegistrar = (*rotatingRegistrar)(nil)
