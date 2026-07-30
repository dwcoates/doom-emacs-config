package sessiondrv

import (
	"fmt"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// ---------------------------------------------------------------------------
// Manager.onHandshake — the daemon's reconciliation across a VENDOR SESSION
// UUID ROTATION.
//
// The shim announces the uuid it is currently filing under on every handshake
// (ShimHello.vendor_session_id), and bounces the connection deliberately when
// the vendor rotates it. This hook runs BEFORE the resubscribe reads its
// position, and moves three things together: the registry (uuid + cursors), the
// queue's turn observation, and the SSM's agent axis.
// ---------------------------------------------------------------------------

// hello builds a ShimHello announcing vendorSessionID for session "s1".
func rotationHello(vendorSessionID string) *corev1.ShimHello {
	return &corev1.ShimHello{
		SessionId: "s1", Vendor: "claude", ShimVersion: "test", ProtocolVersion: "1",
		VendorSessionId: vendorSessionID,
	}
}

func rotationHelloInFlight(vendorSessionID string) *corev1.ShimHello {
	hello := rotationHello(vendorSessionID)
	hello.TurnInFlight = true
	return hello
}

// turnActiveFlag reads the driver's observed turn boundary under the manager
// lock. The driver is resolved FIRST because existing() takes the same lock.
func (h *queueHarness) turnActiveFlag() bool {
	d := h.driver()
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	return d.turnActive
}

// logCapture is a concurrency-safe Logf sink.
type logCapture struct {
	mu    sync.Mutex
	lines []string
}

func (c *logCapture) logf(format string, args ...any) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.lines = append(c.lines, fmt.Sprintf(format, args...))
}

func (c *logCapture) contains(substr string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, l := range c.lines {
		if strings.Contains(l, substr) {
			return true
		}
	}
	return false
}

func TestHandshakeAdoptsAFirstAnnouncementImmediately(t *testing.T) {
	// Arrange — the announcement is what the registry compares against, and
	// what the resolver later binds the new key's events to a workspace by.
	// It used to be HELD until a turn ran; that check now happens at resume,
	// against the transcript on disk, so nothing waits here.
	h := newQueueHarness(t, nil)

	// Act.
	h.m.onHandshake("ws", "s1", rotationHello("uuid-first"))

	// Assert.
	if got := h.reg.adoptionWrites(); len(got) != 1 {
		t.Fatalf("adoptions = %v, want the first announcement adopted at once", got)
	}
}

func TestHandshakeIgnoresAnUnannouncedVendorSessionID(t *testing.T) {
	// Arrange — a fresh session's shim has not learned its uuid yet and
	// announces "". Adopting that would look like a rotation to nothing.
	h := newQueueHarness(t, nil)

	// Act.
	h.m.onHandshake("ws", "s1", rotationHello(""))

	// Assert.
	if got := h.reg.adoptionWrites(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want none for an unannounced uuid", got)
	}
}

func TestRotationClearsTheTurnInFlightObservation(t *testing.T) {
	// Arrange — a turn the daemon has OBSERVED starting. Its end will be
	// reported under the NEW identity, so the flag it set has no boundary
	// coming to clear it.
	h := newQueueHarness(t, nil)
	h.m.onHandshake("ws", "s1", rotationHelloInFlight("uuid-old"))
	h.turn(true)

	// Act.
	h.m.onHandshake("ws", "s1", rotationHello("uuid-new"))

	// Assert.
	active := h.turnActiveFlag()
	if active {
		t.Fatal("turn_active still set after the rotation: the running turn's end belongs to the retired identity")
	}
}

func TestRotationDropsAStandingInterruptMark(t *testing.T) {
	// Arrange — a user-commanded stop marked the running turn, then the uuid
	// rotated. Honoring the mark later would report a stop to a turn that
	// never received one.
	h := newQueueHarness(t, nil)
	h.m.onHandshake("ws", "s1", rotationHello("uuid-old"))
	h.turn(true)
	mark := h.driver()
	h.m.mu.Lock()
	mark.interruptedTurn = true
	h.m.mu.Unlock()

	// Act.
	h.m.onHandshake("ws", "s1", rotationHello("uuid-new"))

	// Assert.
	d := h.driver()
	h.m.mu.Lock()
	marked := d.interruptedTurn
	h.m.mu.Unlock()
	if marked {
		t.Fatal("the interrupt mark survived the rotation")
	}
}

func TestRotationReconcilesTheSSMAgentAxis(t *testing.T) {
	// Arrange — the SSM holds its own `thinking` row for the same turn, and
	// resolves the workspace off it.
	h := newQueueHarness(t, nil)
	h.m.onHandshake("ws", "s1", rotationHello("uuid-old"))

	// Act.
	h.m.onHandshake("ws", "s1", rotationHello("uuid-new"))

	// Assert.
	want := rotationCall{workspace: "ws", previous: "uuid-old", next: "uuid-new"}
	if got := h.applier.rotationsApplied(); len(got) != 1 || got[0] != want {
		t.Fatalf("rotations applied = %v, want [%+v]", got, want)
	}
}

func TestRotationIsAnnouncedLoudly(t *testing.T) {
	// Arrange — a silent rotation is indistinguishable from the defect it
	// fixes, and the log line is the only account of the cursor reset.
	cl := &logCapture{}
	h := newQueueHarnessWithPusher(t, nil, nil, cl.logf)
	h.m.onHandshake("ws", "s1", rotationHello("uuid-old"))

	// Act.
	h.m.onHandshake("ws", "s1", rotationHello("uuid-new"))

	// Assert.
	if !cl.contains("VENDOR SESSION ROTATION") {
		t.Fatal("the rotation passed SILENTLY")
	}
}

func TestSameUUIDRehandshakeReconcilesNothing(t *testing.T) {
	// Arrange — the ordinary reattach: the shim re-announces the uuid it
	// always had, over a live turn. Nothing about that turn changed.
	h := newQueueHarness(t, nil)
	h.m.onHandshake("ws", "s1", rotationHello("uuid-old"))
	h.turn(true)

	// Act.
	h.m.onHandshake("ws", "s1", rotationHelloInFlight("uuid-old"))

	// Assert.
	active := h.turnActiveFlag()
	if !active {
		t.Fatal("a same-uuid re-handshake cleared the turn-in-flight observation")
	}
	if got := h.applier.rotationsApplied(); len(got) != 0 {
		t.Fatalf("rotations applied = %v, want none for a same-uuid re-handshake", got)
	}
}

func TestFirstAdoptionReconcilesNothing(t *testing.T) {
	// Arrange — a fresh session learning its uuid for the first time, mid
	// turn. There is no retired seq space, so there is nothing to reconcile.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	h.m.onHandshake("ws", "s1", rotationHelloInFlight("uuid-first"))

	// Assert.
	active := h.turnActiveFlag()
	if !active {
		t.Fatal("a first adoption cleared the turn-in-flight observation")
	}
	if got := h.applier.rotationsApplied(); len(got) != 0 {
		t.Fatalf("rotations applied = %v, want none for a first adoption", got)
	}
}

// ---------------------------------------------------------------------------
// consumer.onVendorSessionID — keeping the record current off the LIVE stream.
//
// A rotation is detected by comparing a handshake's announcement against the
// persisted uuid, so an empty record leaves nothing to differ from. Every
// PERSISTENT store event names the conversation it belongs to on its envelope,
// which is the earliest and most reliable place to read it.
// ---------------------------------------------------------------------------

func TestPersistentEventReportsItsVendorSessionID(t *testing.T) {
	// Arrange — a seq-stamped store event: the store keys by the vendor uuid,
	// so that is what the envelope carries.
	var seen []string
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), nil, nil, nil, nil, nil, nil)
	c.onVendorSessionID = func(id string) { seen = append(seen, id) }

	// Act.
	c.Apply(&corev1.Event{
		SessionId: "uuid-old", Seq: 7, Plane: corev1.Plane_PLANE_STREAM,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}},
	})

	// Assert.
	if len(seen) != 1 || seen[0] != "uuid-old" {
		t.Fatalf("observed vendor ids = %v, want [uuid-old]", seen)
	}
}

func TestEphemeralEventReportsNoVendorSessionID(t *testing.T) {
	// Arrange — seq 0 is the direct shim→daemon path, whose envelope carries
	// the DAEMON's own s_ id. Adopting that as the vendor uuid would file the
	// conversation under an identity no store event will ever use.
	var seen []string
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(), nil, nil, nil, nil, nil, nil)
	c.onVendorSessionID = func(id string) { seen = append(seen, id) }

	// Act.
	c.Apply(&corev1.Event{
		SessionId: "s1", Seq: 0,
		Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{}},
	})

	// Assert.
	if len(seen) != 0 {
		t.Fatalf("observed vendor ids = %v, want none for an ephemeral event", seen)
	}
}
