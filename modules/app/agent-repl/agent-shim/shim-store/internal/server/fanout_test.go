package server

import (
	"bytes"
	"io"
	"testing"
	"time"

	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/shim-store/internal/logging"
)

func testFanout(buffer int) *fanout {
	return newFanout(buffer, logging.New(io.Discard, io.Discard, false))
}

func ignoreSubscriberDrop(subscriberDropReason) {}
func prepareSubscriber(*subscriber)             {}

// liveDelivery is the OTHER delivery arm — a record handed straight to the
// daemon that the store never saw. The store never publishes one, but the
// fan-out must still route it, because the routing key is on the external half
// both arms carry.
func liveDelivery(session string) *protocolv1.EntryDelivery {
	return &protocolv1.EntryDelivery{
		Delivery: &protocolv1.EntryDelivery_Live{Live: &protocolv1.LiveEntryDelivery{
			Entry: turnBegan(session, "live").GetExternal(),
		}},
	}
}

func TestFanoutDeliversToSessionSubscriber(t *testing.T) {
	// Arrange
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
	// Act
	f.publish(storedDelivery("s1", 1))
	// Assert
	select {
	case got := <-sub.ch:
		if got.GetStored().GetSeq() != 1 {
			t.Fatalf("delivered seq = %d, want 1", got.GetStored().GetSeq())
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
	f.publish(storedDelivery("other", 1))
	// Assert: nothing delivered to s1's subscriber.
	select {
	case got := <-sub.ch:
		t.Fatalf("unexpected delivery for wrong session: %+v", got)
	case <-time.After(50 * time.Millisecond):
	}
}

func TestFanoutRoutesALiveDeliveryByItsExternalHalf(t *testing.T) {
	// Arrange: a live delivery has NO seq field, which is the contract that
	// stops a consumer resuming from a position the store never assigned. Its
	// routing key still has to resolve, or such a record would silently reach
	// nobody.
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
	// Act
	f.publish(liveDelivery("s1"))
	// Assert
	select {
	case got := <-sub.ch:
		if got.GetLive() == nil {
			t.Fatalf("delivered %+v, want the live arm", got)
		}
		if got.GetStored().GetSeq() != 0 {
			t.Fatal("a live delivery reported a position it cannot have")
		}
	case <-time.After(time.Second):
		t.Fatal("a live delivery was not fanned out")
	}
}

func TestFanoutRoutesADeliveryWithNoArmToNobody(t *testing.T) {
	// Arrange: an envelope naming neither arm carries no external half, so it
	// carries no session either. It must reach no subscriber rather than every
	// subscriber that happens to be registered under the empty string.
	f := testFanout(4)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)

	// Act
	f.publish(&protocolv1.EntryDelivery{})

	// Assert
	select {
	case got := <-sub.ch:
		t.Fatalf("an armless delivery reached a subscriber: %+v", got)
	case <-time.After(50 * time.Millisecond):
	}
}

func TestFanoutSlowConsumerDisconnected(t *testing.T) {
	// Arrange: buffer of 2, a subscriber that never drains.
	f := testFanout(2)
	sub := f.subscribe("s1", ignoreSubscriberDrop, prepareSubscriber)
	// Act: overflow the bounded buffer.
	f.publish(storedDelivery("s1", 1))
	f.publish(storedDelivery("s1", 2))
	f.publish(storedDelivery("s1", 3)) // buffer full → disconnect
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
	f.publish(storedDelivery("s1", 1))
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
	f.publish(storedDelivery("vendor-session", 1))
	f.publish(storedDelivery("vendor-session", 2))

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
