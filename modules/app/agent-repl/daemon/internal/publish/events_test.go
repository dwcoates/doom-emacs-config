package publish_test

import (
	"context"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/publish"
)

func TestEventsDeliversAnEventIdenticalToThePrevious(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var events publish.Events[*workspacev1.WorkspaceRef]
	ch := events.Subscribe(ctx)

	// Act: the same click twice, as two equal-but-distinct messages.
	events.Emit(&workspacev1.WorkspaceRef{Id: "a"})
	events.Emit(&workspacev1.WorkspaceRef{Id: "a"})

	// Assert.
	for i := 1; i <= 2; i++ {
		if got := <-ch; got.GetId() != "a" {
			t.Fatalf("delivery %d = %v, want the repeated event", i, got)
		}
	}
}

func TestEventsDoesNotReplayToALateSubscriber(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var events publish.Events[int]
	events.Emit(1)
	ch := events.Subscribe(ctx)

	// Act.
	events.Emit(2)

	// Assert: the first delivery is the event emitted after subscribing.
	if got := <-ch; got != 2 {
		t.Fatalf("first delivery = %d, want 2 (the earlier event is never replayed)", got)
	}
}

func TestEventsDeliversEveryEventInOrder(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var events publish.Events[int]
	ch := events.Subscribe(ctx)

	// Act: emit faster than the subscriber reads; nothing may be skipped.
	for i := 1; i <= 100; i++ {
		events.Emit(i)
	}

	// Assert.
	for want := 1; want <= 100; want++ {
		if got := <-ch; got != want {
			t.Fatalf("delivery = %d, want %d", got, want)
		}
	}
}

func TestEventsEachSubscriberSeesEveryEvent(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var events publish.Events[int]
	first := events.Subscribe(ctx)
	second := events.Subscribe(ctx)

	// Act.
	events.Emit(1)

	// Assert.
	for i, ch := range []<-chan int{first, second} {
		if got := <-ch; got != 1 {
			t.Fatalf("subscriber %d delivery = %d, want 1", i, got)
		}
	}
}

func TestEventsEmitWithNoSubscriberIsDropped(t *testing.T) {
	// Arrange.
	var events publish.Events[int]

	// Act.
	events.Emit(1)

	// Assert.
	if got := events.Subscribers(); got != 0 {
		t.Fatalf("Subscribers() = %d, want 0", got)
	}
}

func TestEventsSubscriptionClosesOnContextCancel(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	var events publish.Events[int]
	ch := events.Subscribe(ctx)

	// Act.
	cancel()

	// Assert.
	if _, open := <-ch; open {
		t.Fatal("channel delivered a value after cancel, want it closed")
	}
}

func TestEventsCancelUnregistersTheSubscriber(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	var events publish.Events[int]
	ch := events.Subscribe(ctx)

	// Act.
	cancel()
	<-ch // drains to closed, which happens after the pump unregisters

	// Assert.
	if got := events.Subscribers(); got != 0 {
		t.Fatalf("Subscribers() = %d, want 0", got)
	}
}

func TestEventsEmitAnswersHowManySubscribersItReached(t *testing.T) {
	tests := []struct {
		name        string
		subscribers int
	}{
		{name: "none", subscribers: 0},
		{name: "one", subscribers: 1},
		{name: "two", subscribers: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			var events publish.Events[int]
			for range tt.subscribers {
				events.Subscribe(ctx)
			}

			// Act.
			got := events.Emit(1)

			// Assert.
			if got != tt.subscribers {
				t.Fatalf("Emit = %d, want %d", got, tt.subscribers)
			}
		})
	}
}
