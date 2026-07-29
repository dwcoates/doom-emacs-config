package server

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func persistentEvent(session string, seq uint64) *corev1.Event {
	return &corev1.Event{
		SessionId: session,
		Seq:       seq,
		Class:     corev1.EventClass_EVENT_CLASS_PERSISTENT,
		Payload:   &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{}},
	}
}

func ephemeralEvent(session string) *corev1.Event {
	return &corev1.Event{
		SessionId: session,
		Class:     corev1.EventClass_EVENT_CLASS_EPHEMERAL,
		Payload:   &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{Uuid: "u"}},
	}
}

func TestFanoutDeliversToSessionSubscriber(t *testing.T) {
	// Arrange
	f := newFanout(4)
	sub := f.subscribe("s1")
	// Act
	f.publish(persistentEvent("s1", 1))
	// Assert
	select {
	case got := <-sub.ch:
		if got.GetSeq() != 1 {
			t.Fatalf("delivered seq = %d, want 1", got.GetSeq())
		}
	case <-time.After(time.Second):
		t.Fatal("timed out waiting for delivery")
	}
}

func TestFanoutIsSessionScoped(t *testing.T) {
	// Arrange
	f := newFanout(4)
	sub := f.subscribe("s1")
	// Act: publish for a different session.
	f.publish(persistentEvent("other", 1))
	// Assert: nothing delivered to s1's subscriber.
	select {
	case got := <-sub.ch:
		t.Fatalf("unexpected delivery for wrong session: %+v", got)
	case <-time.After(50 * time.Millisecond):
	}
}

func TestFanoutEphemeralPassesThrough(t *testing.T) {
	// Arrange
	f := newFanout(4)
	sub := f.subscribe("s1")
	// Act: the fanout is class-agnostic; ephemeral events reach live subscribers.
	f.publish(ephemeralEvent("s1"))
	// Assert
	select {
	case got := <-sub.ch:
		if got.GetClass() != corev1.EventClass_EVENT_CLASS_EPHEMERAL {
			t.Fatalf("delivered class = %v, want EPHEMERAL", got.GetClass())
		}
	case <-time.After(time.Second):
		t.Fatal("ephemeral event was not fanned out")
	}
}

func TestFanoutSlowConsumerDisconnected(t *testing.T) {
	// Arrange: buffer of 2, a subscriber that never drains.
	f := newFanout(2)
	sub := f.subscribe("s1")
	// Act: overflow the bounded buffer.
	f.publish(persistentEvent("s1", 1))
	f.publish(persistentEvent("s1", 2))
	f.publish(persistentEvent("s1", 3)) // buffer full → disconnect
	// Assert: the subscriber is dropped and deregistered; the requester owns
	// its session-specific reconnect diagnostic.
	select {
	case <-sub.done:
	case <-time.After(time.Second):
		t.Fatal("slow consumer was not disconnected")
	}
	if f.subscriberCount("s1") != 0 {
		t.Fatalf("subscriberCount = %d, want 0 after disconnect", f.subscriberCount("s1"))
	}
}

func TestFanoutUnsubscribeStopsDelivery(t *testing.T) {
	// Arrange
	f := newFanout(4)
	sub := f.subscribe("s1")
	// Act
	f.unsubscribe(sub)
	f.publish(persistentEvent("s1", 1))
	// Assert: no delivery, done closed, count zero.
	if f.subscriberCount("s1") != 0 {
		t.Fatalf("subscriberCount = %d, want 0", f.subscriberCount("s1"))
	}
	select {
	case <-sub.done:
	case <-time.After(time.Second):
		t.Fatal("done not closed after unsubscribe")
	}
}
