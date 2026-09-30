package publish_test

import (
	"context"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/publish"
)

func TestLatestBeforeAnyPublish(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]

	// Act.
	_, ok := topic.Latest()

	// Assert.
	if ok {
		t.Fatal("Latest() reported a value before any Publish")
	}
}

func TestLatestAfterPublish(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]

	// Act.
	topic.Publish(7)
	got, ok := topic.Latest()

	// Assert.
	if !ok || got != 7 {
		t.Fatalf("Latest() = (%d, %v), want (7, true)", got, ok)
	}
}

func TestSubscribeDeliversLatestFirst(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	topic.Publish(1)

	// Act.
	got := <-topic.Subscribe(ctx)

	// Assert.
	if got != 1 {
		t.Fatalf("first delivery = %d, want the latest value 1", got)
	}
}

func TestSubscribeToEmptyTopicDeliversNothingUntilPublish(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	ch := topic.Subscribe(ctx)

	// Act.
	topic.Publish(42)

	// Assert.
	if got := <-ch; got != 42 {
		t.Fatalf("first delivery = %d, want 42", got)
	}
}

func TestSubscribeDeliversEveryLaterValueInOrder(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	ch := topic.Subscribe(ctx)

	// Act: publish faster than the subscriber reads; nothing may be skipped.
	for i := 1; i <= 100; i++ {
		topic.Publish(i)
	}

	// Assert.
	for i := 1; i <= 100; i++ {
		if got := <-ch; got != i {
			t.Fatalf("delivery %d = %d, want %d", i, got, i)
		}
	}
}

func TestPublishNeverBlocksOnASlowSubscriber(t *testing.T) {
	// Arrange: a subscriber that never reads.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	_ = topic.Subscribe(ctx)

	// Act: completing this loop is the assertion — a bounded queue would
	// deadlock here.
	done := make(chan struct{})
	go func() {
		defer close(done)
		for i := 0; i < 10_000; i++ {
			topic.Publish(i)
		}
	}()

	// Assert.
	<-done
}

func TestPublishDeduplicatesComparableValues(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	ch := topic.Subscribe(ctx)

	// Act.
	topic.Publish(5)
	topic.Publish(5)
	topic.Publish(6)

	// Assert: the duplicate never reaches the subscriber.
	if got := <-ch; got != 5 {
		t.Fatalf("first delivery = %d, want 5", got)
	}
	if got := <-ch; got != 6 {
		t.Fatalf("second delivery = %d, want 6 (the duplicate 5 must be dropped)", got)
	}
}

func TestPublishDeduplicatesProtoMessagesByValue(t *testing.T) {
	// Arrange: two distinct pointers with equal contents.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[*workspacev1.WorkspaceRef]
	ch := topic.Subscribe(ctx)

	// Act.
	topic.Publish(&workspacev1.WorkspaceRef{Id: "ws-a"})
	topic.Publish(&workspacev1.WorkspaceRef{Id: "ws-a"})
	topic.Publish(&workspacev1.WorkspaceRef{Id: "ws-b"})

	// Assert.
	if got := <-ch; got.GetId() != "ws-a" {
		t.Fatalf("first delivery = %q, want %q", got.GetId(), "ws-a")
	}
	if got := <-ch; got.GetId() != "ws-b" {
		t.Fatalf("second delivery = %q, want %q (the proto.Equal duplicate must be dropped)", got.GetId(), "ws-b")
	}
}

func TestPublishDoesNotDeduplicateIncomparableValues(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[[]string]
	ch := topic.Subscribe(ctx)

	// Act.
	topic.Publish([]string{"a"})
	topic.Publish([]string{"a"})

	// Assert: an incomparable value can never be proven a duplicate.
	if got := <-ch; len(got) != 1 {
		t.Fatalf("first delivery = %v, want one element", got)
	}
	if got := <-ch; len(got) != 1 {
		t.Fatalf("second delivery = %v, want the un-deduplicated repeat", got)
	}
}

func TestSubscriptionClosesOnContextCancel(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	var topic publish.Topic[int]
	ch := topic.Subscribe(ctx)

	// Act.
	cancel()

	// Assert.
	if _, open := <-ch; open {
		t.Fatal("channel delivered a value after cancel, want it closed")
	}
}

func TestCancelUnregistersTheSubscriber(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	var topic publish.Topic[int]
	ch := topic.Subscribe(ctx)

	// Act.
	cancel()
	<-ch // drains to closed, which happens after the pump unregisters

	// Assert.
	if got := topic.Subscribers(); got != 0 {
		t.Fatalf("Subscribers() = %d, want 0", got)
	}
}

func TestEachSubscriberSeesEveryValue(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	first := topic.Subscribe(ctx)
	second := topic.Subscribe(ctx)

	// Act.
	topic.Publish(1)
	topic.Publish(2)

	// Assert.
	for i, ch := range []<-chan int{first, second} {
		if got := <-ch; got != 1 {
			t.Fatalf("subscriber %d delivery 1 = %d, want 1", i, got)
		}
		if got := <-ch; got != 2 {
			t.Fatalf("subscriber %d delivery 2 = %d, want 2", i, got)
		}
	}
}

// TestRepublishDeliversTheLatestValueAgain covers the one caller that MEANS
// the repetition: an adopted workspace's clients are repainted from what the
// daemon holds, and Publish deliberately drops an identical re-render.
func TestRepublishDeliversTheLatestValueAgain(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	topic.Publish(7)
	ch := topic.Subscribe(ctx)
	<-ch

	// Act.
	republished := topic.Republish()

	// Assert.
	if !republished {
		t.Fatal("Republish reported nothing to hand out, want the standing value")
	}
	if got := <-ch; got != 7 {
		t.Fatalf("republished value = %d, want 7", got)
	}
}

// TestRepublishReportsAnEmptyTopic covers the topic nothing has published: a
// repaint with no value to draw is reported rather than sent empty.
func TestRepublishReportsAnEmptyTopic(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]

	// Act.
	got := topic.Republish()

	// Assert.
	if got {
		t.Fatal("Republish reported a value on a topic that never had one")
	}
}

func TestLatestOnlySubscriberSkipsSupersededValues(t *testing.T) {
	// Arrange: a subscriber that reads nothing while a burst is published.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	topic := publish.NewLatestOnly[int]()
	ch := topic.Subscribe(ctx)

	// Act.
	for i := 1; i <= 100; i++ {
		topic.Publish(i)
	}

	// Assert: at most the one value the pump already held, then the latest.
	received := 0
	for got := 0; got != 100; {
		got = <-ch
		received++
	}
	if received > 2 {
		t.Fatalf("a latest-only subscriber took %d values to reach the latest, want at most 2", received)
	}
}

func TestLatestOnlySubscribeDeliversLatestFirst(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	topic := publish.NewLatestOnly[int]()
	topic.Publish(1)
	topic.Publish(2)

	// Act.
	got := <-topic.Subscribe(ctx)

	// Assert.
	if got != 2 {
		t.Fatalf("first delivery = %d, want the latest value 2", got)
	}
}

func TestLatestOnlySubscriberReceivesEveryValueItKeepsUpWith(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	topic := publish.NewLatestOnly[int]()
	ch := topic.Subscribe(ctx)

	// Act, Assert: a reader that takes each value before the next is
	// published misses none of them.
	for i := 1; i <= 10; i++ {
		topic.Publish(i)
		if got := <-ch; got != i {
			t.Fatalf("delivery = %d, want %d", got, i)
		}
	}
}
