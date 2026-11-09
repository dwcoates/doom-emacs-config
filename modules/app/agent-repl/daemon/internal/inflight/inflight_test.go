package inflight

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

// waitBound is one wait's failure bound in these tests.
const waitBound = 5 * time.Second

func TestAwaitIdle(t *testing.T) {
	cases := []struct {
		name string
		// begin counts the work under test, answering what finishes it.
		begin func(tr *Tracker) (finish func())
	}{
		{name: "nothing in flight answers at once", begin: func(*Tracker) func() { return func() {} }},
		{name: "begun work answers once it ends", begin: func(tr *Tracker) func() { return tr.Begin() }},
		{name: "a goroutine answers once it returns", begin: func(tr *Tracker) func() {
			release := make(chan struct{})
			tr.Go(func() { <-release })
			return func() { close(release) }
		}},
		{name: "work that starts more is idle only once the chain ends", begin: func(tr *Tracker) func() {
			release := make(chan struct{})
			tr.Go(func() {
				tr.Go(func() { <-release })
			})
			return func() { close(release) }
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			tr := &Tracker{}
			finish := tc.begin(tr)
			ctx, cancel := context.WithTimeout(context.Background(), waitBound)
			defer cancel()
			idle := make(chan error, 1)

			// Act
			go func() { idle <- tr.AwaitIdle(ctx) }()
			finish()
			err := <-idle

			// Assert
			if err != nil {
				t.Fatalf("AwaitIdle = %v, want idle", err)
			}
			if got := tr.InFlight(); got != 0 {
				t.Fatalf("InFlight = %d once idle, want 0", got)
			}
		})
	}
}

func TestAwaitIdleEndsWithItsContextWhileWorkIsInFlight(t *testing.T) {
	// Arrange
	tr := &Tracker{}
	end := tr.Begin()
	defer end()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := tr.AwaitIdle(ctx)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitIdle = %v, want context.Canceled", err)
	}
}

func TestBeginsEndIsIdempotent(t *testing.T) {
	// Arrange
	tr := &Tracker{}
	end := tr.Begin()
	other := tr.Begin()

	// Act
	end()
	end()

	// Assert: the second call did not uncount the other work.
	if got := tr.InFlight(); got != 1 {
		t.Fatalf("InFlight = %d, want the other work still counted", got)
	}
	other()
}

func TestTrackerIsReusableAfterIdle(t *testing.T) {
	// Arrange: one round of work, idle, then a second round.
	tr := &Tracker{}
	tr.Begin()()
	if err := tr.AwaitIdle(context.Background()); err != nil {
		t.Fatalf("first AwaitIdle: %v", err)
	}
	end := tr.Begin()
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()
	idle := make(chan error, 1)

	// Act
	go func() { idle <- tr.AwaitIdle(ctx) }()
	end()
	err := <-idle

	// Assert
	if err != nil {
		t.Fatalf("second AwaitIdle = %v, want idle", err)
	}
}

func TestQueue(t *testing.T) {
	// Arrange
	tr := &Tracker{}
	q := NewQueue[int](tr)
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()

	// Act
	q.Put(1)
	q.Put(2)
	inFlight := tr.InFlight()
	first, doneFirst, ok1 := q.Take(ctx)
	second, doneSecond, ok2 := q.Take(ctx)
	doneFirst()
	afterFirst := tr.InFlight()
	doneSecond()

	// Assert
	if !ok1 || !ok2 || first != 1 || second != 2 {
		t.Fatalf("took (%d, %v), (%d, %v), want 1 then 2", first, ok1, second, ok2)
	}
	if inFlight != 2 || afterFirst != 1 || tr.InFlight() != 0 {
		t.Fatalf("in flight = %d, %d, %d; want each item counted from Put until its done", inFlight, afterFirst, tr.InFlight())
	}
}

func TestQueueTakeWaitsForAPutAndEndsWithItsContext(t *testing.T) {
	// Arrange
	q := NewQueue[string](&Tracker{})
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()
	got := make(chan string, 1)
	go func() {
		v, done, ok := q.Take(ctx)
		if ok {
			done()
		}
		got <- v
	}()

	// Act
	q.Put("x")
	v := <-got
	ended, endNow := context.WithCancel(context.Background())
	endNow()
	_, _, ok := q.Take(ended)

	// Assert
	if v != "x" {
		t.Fatalf("Take = %q, want the item put while it waited", v)
	}
	if ok {
		t.Fatalf("Take on an ended context answered an item")
	}
}

func TestEndingMoreThanBeganPanics(t *testing.T) {
	// Arrange
	tr := &Tracker{}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatalf("an unbalanced end did not panic")
		}
	}()

	// Act
	tr.end("anywhere")
}

func TestInFlightSitesNameWhereWorkBegan(t *testing.T) {
	// Arrange
	tr := &Tracker{}
	end := tr.Begin()

	// Act
	sites := tr.InFlightSites()
	end()

	// Assert
	if len(sites) != 1 || !strings.HasPrefix(sites[0], "inflight/inflight_test.go:") || !strings.HasSuffix(sites[0], " x1") {
		t.Fatalf("InFlightSites = %v, want this test's line, once", sites)
	}
	if got := tr.InFlightSites(); len(got) != 0 {
		t.Fatalf("InFlightSites after the end = %v, want none", got)
	}
}

func TestAChildsWorkIsCountedOnItsParent(t *testing.T) {
	// Arrange.
	parent := &Tracker{}
	child := NewChild(parent)

	// Act.
	end := child.Begin()

	// Assert.
	if parent.InFlight() != 1 || child.InFlight() != 1 {
		t.Fatalf("in flight = (parent %d, child %d), want (1, 1)", parent.InFlight(), child.InFlight())
	}
	end()
}

func TestAChildsEndUncountsItOnItsParent(t *testing.T) {
	// Arrange.
	parent := &Tracker{}
	child := NewChild(parent)
	end := child.Begin()

	// Act.
	end()

	// Assert.
	if parent.InFlight() != 0 || child.InFlight() != 0 {
		t.Fatalf("in flight = (parent %d, child %d), want (0, 0)", parent.InFlight(), child.InFlight())
	}
}

func TestAParentsOwnWorkIsNotTheChilds(t *testing.T) {
	// Arrange.
	parent := &Tracker{}
	child := NewChild(parent)

	// Act.
	end := parent.Begin()
	defer end()

	// Assert.
	if err := child.AwaitIdle(t.Context()); err != nil {
		t.Fatalf("child AwaitIdle = %v, want idle: the parent's work is not the child's", err)
	}
}

func TestNewChildRefusesNoParent(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("NewChild(nil) did not panic")
		}
	}()

	// Act.
	NewChild(nil)
}
