package server

import (
	"bytes"
	"io"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

func testFanout(buffer int) *fanout {
	return newFanout(buffer, logging.New(io.Discard, io.Discard, false))
}

func ignoreSubscriberDrop(subscriberDropReason) {}
func prepareSubscriber(*subscriber)             {}

// line is the frame the tail carries on store.v1: one StoreLineAt, positioned
// by an opaque store-minted pointer rather than by a seq.
func line(pointer string) *storev1.WatchAgentSessionResponse {
	return &storev1.WatchAgentSessionResponse{Line: &storev1.StoreLineAt{
		At: &storev1.StoreItemPointer{Value: pointer},
	}}
}

func TestFanoutDeliversToSessionSubscriber(t *testing.T) {
	// Arrange
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
	// Act
	f.publish("s1", line("p-1"))
	// Assert
	select {
	case got := <-sub.ch:
		if got.GetLine().GetAt().GetValue() != "p-1" {
			t.Fatalf("delivered pointer = %q, want p-1", got.GetLine().GetAt().GetValue())
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
	f.publish("other", line("p-1"))
	// Assert: nothing delivered to s1's subscriber.
	select {
	case got := <-sub.ch:
		t.Fatalf("unexpected delivery for wrong session: %+v", got)
	case <-time.After(50 * time.Millisecond):
	}
}

func TestFanoutRoutesAnEmptyKeyToNobody(t *testing.T) {
	// Arrange: the routing key is now stated by the caller, so a caller with no
	// key must reach no subscriber rather than every subscriber registered
	// under the empty string.
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)

	// Act
	f.publish("", line("p-1"))

	// Assert
	select {
	case got := <-sub.ch:
		t.Fatalf("an unkeyed publish reached a subscriber: %+v", got)
	case <-time.After(50 * time.Millisecond):
	}
}

func TestFanoutSlowConsumerDisconnected(t *testing.T) {
	// Arrange: buffer of 2, a subscriber that never drains.
	f := testFanout(2)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
	// Act: overflow the bounded buffer.
	f.publish("s1", line("p-1"))
	f.publish("s1", line("p-2"))
	f.publish("s1", line("p-3")) // buffer full → disconnect
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
	f.publish("s1", line("p-1"))
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
	// Arrange
	var logs bytes.Buffer
	f := newFanout(1, logging.New(&logs, io.Discard, false).With(logging.Fields{Component: "server", Socket: "store.sock"}))
	sub := f.subscribe("vendor-session", ignoreSubscriberDrop, prepareSubscriber)

	// Act
	f.publish("vendor-session", line("p-1"))
	f.publish("vendor-session", line("p-2"))

	// Assert
	select {
	case <-sub.done:
	case <-time.After(time.Second):
		t.Fatal("slow subscriber was not disconnected")
	}
	record, found := findLoggedRecord(t, splitLines(logs.Bytes()), "slow-consumer", "warn")
	if !found {
		t.Fatalf("slow-consumer record missing: %s", logs.String())
	}
	if record.Level != "warn" || record.Operation != "slow-consumer" || record.Session != "vendor-session" || record.Context["subscriber"] != "1" || record.Context["component"] != "server" || record.Context["socket"] != "store.sock" {
		t.Fatalf("slow-consumer record lacks canonical context: %#v", record)
	}
}
