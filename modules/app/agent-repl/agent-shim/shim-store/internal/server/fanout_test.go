package server

import (
	"bytes"
	"io"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/logging"
)

func testFanout(buffer int) *fanout {
	return newFanout(buffer, logging.New(io.Discard, io.Discard, false))
}

func ignoreSubscriberDrop(subscriberDropReason) {}
func prepareSubscriber(*subscriber)             {}

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
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
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
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
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
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
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
	f := testFanout(2)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
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
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
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

func TestFanoutSlowConsumerLogsCanonicalContext(t *testing.T) {
	var logs bytes.Buffer
	f := newFanout(1, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}))
	sub := f.subscribe("vendor-session", ignoreSubscriberDrop, prepareSubscriber)
	f.publish(persistentEvent("vendor-session", 1))
	f.publish(persistentEvent("vendor-session", 2))

	select {
	case <-sub.done:
	case <-time.After(time.Second):
		t.Fatal("slow subscriber was not disconnected")
	}

	record, found := findLoggedRecord(t, logs.Bytes(), "slow-consumer", "warn")
	if !found {
		t.Fatalf("slow-consumer record missing: %s", logs.String())
	}
	if record.Level != "warn" || record.Operation != "slow-consumer" || record.Session != "vendor-session" || record.Context["subscriber"] != "1" || record.Context["component"] != "server" || record.Context["socket"] != "store.sock" {
		t.Fatalf("slow-consumer record lacks canonical context: %#v", record)
	}
}
