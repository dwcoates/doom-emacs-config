package feed

import (
	"context"
	"errors"
	"slices"
	"sync"
	"testing"
)

func TestQueuedStoreWorkRunsOffTheLockOnceItIsReleased(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	var ranHeld, ran bool
	h.resolver.mu.Lock()
	h.resolver.afterUnlock(func(storeHold) {
		ran = true
		ranHeld = h.resolver.mu.Held()
	})

	// Act.
	notYet := ran
	h.resolver.mu.Unlock()

	// Assert.
	if notYet {
		t.Fatal("the queued step ran while the lock was still held")
	}
	if !ran || ranHeld {
		t.Fatalf("ran = %v under the lock = %v, want it run off the lock before Unlock returned", ran, ranHeld)
	}
}

func TestQueuedStoreWorkRunsInTheOrderTheHoldsReleased(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	var order []string

	// Act: two holds, each queuing two steps.
	for _, hold := range []string{"first", "second"} {
		h.resolver.mu.Lock()
		h.resolver.afterUnlock(func(storeHold) { order = append(order, hold+"/a") })
		h.resolver.afterUnlock(func(storeHold) { order = append(order, hold+"/b") })
		h.resolver.mu.Unlock()
	}

	// Assert.
	if want := []string{"first/a", "first/b", "second/a", "second/b"}; !slices.Equal(order, want) {
		t.Fatalf("order = %v, want %v", order, want)
	}
}

// TestAReleaseWaitsForItsBatchWhileAnotherGoroutineDrains pins that a caller
// returns only once its own work ran, even when the queue is drained by
// another goroutine: no write is left behind for a shutdown.
func TestAReleaseWaitsForItsBatchWhileAnotherGoroutineDrains(t *testing.T) {
	// Arrange: the first hold's step blocks until the test lets it go.
	h := newHarness(t)
	inFirst, letFirstGo := make(chan struct{}), make(chan struct{})
	var secondRan bool
	var mu sync.Mutex
	firstDone := make(chan struct{})
	go func() {
		defer close(firstDone)
		h.resolver.mu.Lock()
		h.resolver.afterUnlock(func(storeHold) {
			close(inFirst)
			<-letFirstGo
		})
		h.resolver.mu.Unlock()
	}()
	<-inFirst

	// Act: the second hold releases while the first goroutine drains.
	secondReturned := make(chan bool, 1)
	go func() {
		h.resolver.mu.Lock()
		h.resolver.afterUnlock(func(storeHold) {
			mu.Lock()
			secondRan = true
			mu.Unlock()
		})
		h.resolver.mu.Unlock()
		mu.Lock()
		defer mu.Unlock()
		secondReturned <- secondRan
	}()
	close(letFirstGo)

	// Assert.
	if ran := <-secondReturned; !ran {
		t.Fatal("the second release returned before its own step ran")
	}
	<-firstDone
}

func TestASettleRunsUnderTheLockAndItsWorkJoinsTheDrain(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	var order []string
	var settledHeld bool

	// Act.
	h.resolver.mu.Lock()
	h.resolver.afterUnlock(func(hold storeHold) {
		order = append(order, "step")
		hold.settle(func() {
			settledHeld = h.resolver.mu.Held()
			h.resolver.afterUnlock(func(storeHold) { order = append(order, "queued by the settle") })
		})
		order = append(order, "after the settle")
	})
	h.resolver.mu.Unlock()

	// Assert.
	if !settledHeld {
		t.Fatal("the settle ran off the lock")
	}
	if want := []string{"step", "after the settle", "queued by the settle"}; !slices.Equal(order, want) {
		t.Fatalf("order = %v, want %v", order, want)
	}
}

func TestStoreWorkQueuedOutsideAHoldPanics(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("queuing store work outside a hold did not panic")
		}
	}()

	// Act.
	h.resolver.afterUnlock(func(storeHold) {})
}

func TestAWriteOutcomeIsKnownOnceTheHoldIsReleased(t *testing.T) {
	cause := errors.New("disk full")
	tests := []struct {
		name         string
		err          error
		wantFailed   bool
		wantRecorded bool
	}{
		{name: "a write that succeeds is recorded", wantRecorded: true},
		{name: "a write that fails is told and answered", err: cause, wantFailed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			var failed, recorded bool
			h.resolver.mu.Lock()
			outcome := h.resolver.writeAfterUnlock(func(context.Context) error { return tt.err },
				func(err error) { failed = errors.Is(err, cause) }, func() { recorded = true })

			// Act.
			h.resolver.mu.Unlock()

			// Assert.
			if !errors.Is(outcome.err, tt.err) || failed != tt.wantFailed || recorded != tt.wantRecorded {
				t.Fatalf("outcome = %v, failed = %v, recorded = %v; want %v, %v, %v", outcome.err, failed, recorded, tt.err, tt.wantFailed, tt.wantRecorded)
			}
		})
	}
}
