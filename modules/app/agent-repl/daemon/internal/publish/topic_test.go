package publish_test

import (
	"agent-repld/internal/inflight"
	"context"
	"errors"
	"testing"
	"time"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/publish"
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

// TestClearIfStopsReplayingAStaleValue covers the retraction's first half: a
// value cleared as stale is not handed to a later subscriber.
func TestClearIfStopsReplayingAStaleValue(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	topic.Publish(7)

	// Act.
	cleared := topic.ClearIf(func(v int) bool { return v == 7 })
	ch := topic.Subscribe(ctx)
	topic.Publish(8)

	// Assert.
	if !cleared {
		t.Fatal("ClearIf reported nothing cleared, want the stale value cleared")
	}
	if got := <-ch; got != 8 {
		t.Fatalf("first delivery = %d, want the later value 8 and no replay of 7", got)
	}
}

// TestClearIfKeepsAValueThatIsNotStale covers the predicate refusing: the
// latest value stands and is still replayed.
func TestClearIfKeepsAValueThatIsNotStale(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]
	topic.Publish(7)

	// Act.
	cleared := topic.ClearIf(func(v int) bool { return v == 9 })

	// Assert.
	if cleared {
		t.Fatal("ClearIf cleared a value its predicate did not call stale")
	}
	if got, ok := topic.Latest(); !ok || got != 7 {
		t.Fatalf("Latest() = (%d, %v), want (7, true)", got, ok)
	}
}

// TestClearIfLetsAnIdenticalValuePublishAgain covers the retraction's second
// half: the next transfer's notice is identical to the retracted one, and it
// must reach the wire rather than be dropped as a repeat.
func TestClearIfLetsAnIdenticalValuePublishAgain(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var topic publish.Topic[int]
	topic.Publish(7)
	ch := topic.Subscribe(ctx)
	<-ch
	topic.ClearIf(func(v int) bool { return v == 7 })

	// Act.
	topic.Publish(7)

	// Assert.
	if got := <-ch; got != 7 {
		t.Fatalf("delivery = %d, want the identical value 7 published again", got)
	}
}

// TestClearIfReportsAnEmptyTopic covers the topic nothing has published: there
// is nothing to judge, and the predicate is never asked.
func TestClearIfReportsAnEmptyTopic(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]
	asked := false

	// Act.
	cleared := topic.ClearIf(func(int) bool { asked = true; return true })

	// Assert.
	if cleared || asked {
		t.Fatalf("ClearIf on an empty topic: cleared=%v asked=%v, want neither", cleared, asked)
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

func TestChangesReceivesOncePerValueAndClosesWithItsContext(t *testing.T) {
	// Arrange: a topic with a latest value already published.
	var topic publish.Topic[int]
	topic.Publish(1)
	ctx, cancel := context.WithCancel(context.Background())
	changes, ack := publish.Changes(ctx, &topic)

	// Act
	<-changes // the latest value
	ack()
	topic.Publish(2)
	<-changes // the later one
	ack()
	cancel()
	_, open := <-changes
	for open {
		_, open = <-changes
	}

	// Assert: reaching here means one receive per value, then the close.
	if open {
		t.Fatalf("Changes stayed open after its context ended")
	}
}

func TestAwaitSubscribersWakesOnEachSubscriptionAddedOrEnded(t *testing.T) {
	cases := []struct {
		name string
		want int
		// change makes the subscription change the wait is for.
		change func(topic *publish.Topic[int], end context.CancelFunc)
	}{
		{name: "a subscription added", want: 2, change: func(topic *publish.Topic[int], _ context.CancelFunc) {
			topic.Subscribe(context.Background())
		}},
		{name: "a subscription ended", want: 0, change: func(_ *publish.Topic[int], end context.CancelFunc) { end() }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: one subscription already served.
			var topic publish.Topic[int]
			subCtx, end := context.WithCancel(context.Background())
			defer end()
			topic.Subscribe(subCtx)
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			done := make(chan error, 1)

			// Act
			go func() { done <- topic.AwaitSubscribers(ctx, tc.want) }()
			tc.change(&topic, end)
			err := <-done

			// Assert
			if err != nil {
				t.Fatalf("AwaitSubscribers = %v, want %d subscriptions seen", err, tc.want)
			}
		})
	}
}

func TestAwaitSubscribersEndsWithItsContext(t *testing.T) {
	// Arrange
	var topic publish.Topic[int]
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := topic.AwaitSubscribers(ctx, 1)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitSubscribers = %v, want context.Canceled", err)
	}
}

func TestAChangeIsCountedUntilItsAck(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]
	tracker := &inflight.Tracker{}
	changes, ack := publish.ChangesOn(t.Context(), &topic, tracker)

	// Act.
	topic.Publish(1)
	<-changes
	held := tracker.InFlight()
	ack()

	// Assert.
	if held != 1 || tracker.InFlight() != 0 {
		t.Fatalf("in flight = (%d held, %d acked), want (1, 0)", held, tracker.InFlight())
	}
}

func TestAChangeNeverTakenEndsWithItsSubscription(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]
	tracker := &inflight.Tracker{}
	ctx, cancel := context.WithCancel(context.Background())
	changes, _ := publish.ChangesOn(ctx, &topic, tracker)
	topic.Publish(1)

	// Act.
	cancel()
	for range changes {
	}

	// Assert.
	if err := tracker.AwaitIdle(t.Context()); err != nil {
		t.Fatalf("the untaken change is still counted: %v", err)
	}
}

func TestAnAckAfterTheSubscriptionEndedEndsNothingAndDoesNotPanic(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]
	tracker := &inflight.Tracker{}
	ctx, cancel := context.WithCancel(context.Background())
	changes, ack := publish.ChangesOn(ctx, &topic, tracker)
	topic.Publish(1)
	<-changes

	// Act: the subscription ends while the consumer still holds the change,
	// and its end ends that change's count (the tracker goes idle only then),
	// and only after that does the consumer ack.
	cancel()
	if err := tracker.AwaitIdle(t.Context()); err != nil {
		t.Fatalf("the ended subscription left its change counted: %v", err)
	}
	ack()

	// Assert.
	if got := tracker.InFlight(); got != 0 {
		t.Fatalf("in flight = %d after the subscription ended, want 0", got)
	}
}

func TestAnAckWithNothingHandedOnALiveSubscriptionPanics(t *testing.T) {
	// Arrange.
	var topic publish.Topic[int]
	_, ack := publish.ChangesOn(t.Context(), &topic, &inflight.Tracker{})

	// Act.
	got := func() (r any) {
		defer func() { r = recover() }()
		ack()
		return nil
	}()

	// Assert.
	if got != "publish: a counted subscription acknowledged a value it was never handed" {
		t.Fatalf("recovered %v, want the never-handed panic", got)
	}
}
