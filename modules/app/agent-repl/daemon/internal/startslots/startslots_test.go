package startslots

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/ids"
)

// lineWatch answers a pool whose every lining is signalled on the channel.
func lineWatch(size int) (*Pool, chan struct{}) {
	p := New(size)
	lined := make(chan struct{}, 8)
	p.lined = func() { lined <- struct{}{} }
	return p, lined
}

func TestTryAcquire(t *testing.T) {
	tests := []struct {
		name     string
		size     int
		holders  []ids.WorkspaceID
		ask      ids.WorkspaceID
		wantOK   bool
		wantHeld int
	}{
		{name: "takes a free slot", size: 1, ask: "a", wantOK: true, wantHeld: 1},
		{name: "refuses when every slot is held", size: 1, holders: []ids.WorkspaceID{"a"}, ask: "b", wantOK: false, wantHeld: 1},
		{name: "answers a holder from the slot it holds", size: 1, holders: []ids.WorkspaceID{"a"}, ask: "a", wantOK: true, wantHeld: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			p := New(tt.size)
			for _, ws := range tt.holders {
				if _, ok := p.TryAcquire(ws); !ok {
					t.Fatalf("arrange: %s could not take a slot", ws)
				}
			}

			// Act.
			_, ok := p.TryAcquire(tt.ask)

			// Assert.
			if ok != tt.wantOK || p.held != tt.wantHeld {
				t.Fatalf("TryAcquire = %v with %d held, want %v with %d", ok, p.held, tt.wantOK, tt.wantHeld)
			}
		})
	}
}

func TestAHoldersAcquireIsAnsweredAtOnce(t *testing.T) {
	// Arrange.
	p := New(1)
	p.TryAcquire("a")

	// Act.
	_, err := p.Acquire(context.Background(), "a")

	// Assert.
	if err != nil || p.held != 1 {
		t.Fatalf("Acquire = %v with %d held, want the holder's own slot", err, p.held)
	}
}

func TestAnAcquireWaitsForARelease(t *testing.T) {
	// Arrange.
	p, lined := lineWatch(1)
	release, _ := p.TryAcquire("a")
	got := make(chan error, 1)
	go func() {
		_, err := p.Acquire(context.Background(), "b")
		got <- err
	}()
	<-lined

	// Act.
	release()

	// Assert.
	if err := <-got; err != nil || !p.Holds("b") {
		t.Fatalf("Acquire = %v, holds %v; want b to hold the released slot", err, p.Holds("b"))
	}
}

func TestAnAcquireWhoseContextEndsLeavesTheLine(t *testing.T) {
	// Arrange.
	p, lined := lineWatch(1)
	p.TryAcquire("a")
	ctx, cancel := context.WithCancelCause(context.Background())
	stop := errors.New("stop")
	got := make(chan error, 1)
	go func() {
		_, err := p.Acquire(ctx, "b")
		got <- err
	}()
	<-lined

	// Act.
	cancel(stop)

	// Assert.
	if err := <-got; !errors.Is(err, stop) || p.Waiting() != 0 {
		t.Fatalf("Acquire = %v with %d waiting, want the context's cause and an empty line", err, p.Waiting())
	}
}

func TestNotify(t *testing.T) {
	tests := []struct {
		name string
		full bool
	}{
		{name: "is handed a released slot", full: true},
		{name: "is handed a free slot at once", full: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			p := New(1)
			var release Release
			if tt.full {
				release, _ = p.TryAcquire("a")
			}
			handed := make(chan Release, 1)

			// Act.
			p.Notify("b", func(r Release) { handed <- r })
			if release != nil {
				release()
			}

			// Assert.
			<-handed
			if !p.Holds("b") {
				t.Fatal("the notified workspace does not hold the slot it was handed")
			}
		})
	}
}

func TestNotifyDoesNotLineAWorkspaceTwice(t *testing.T) {
	// Arrange.
	p := New(1)
	p.TryAcquire("a")
	p.Notify("b", func(Release) {})

	// Act.
	again := p.Notify("b", func(Release) {})

	// Assert.
	if again || p.Waiting() != 1 {
		t.Fatalf("Notify again = %v with %d waiting, want b lined once", again, p.Waiting())
	}
}

func TestWithdrawTakesANotifyOutOfLine(t *testing.T) {
	// Arrange.
	p := New(1)
	p.TryAcquire("a")
	p.Notify("b", func(Release) {})

	// Act.
	withdrew := p.Withdraw("b")

	// Assert.
	if !withdrew || p.Waiting() != 0 {
		t.Fatalf("Withdraw = %v with %d waiting, want b out of line", withdrew, p.Waiting())
	}
}

func TestTheLongestWaiterIsServedFirst(t *testing.T) {
	// Arrange: b lines first, c second.
	p := New(1)
	release, _ := p.TryAcquire("a")
	handed := make(chan ids.WorkspaceID, 2)
	p.Notify("b", func(Release) { handed <- "b" })
	p.Notify("c", func(Release) { handed <- "c" })

	// Act.
	release()

	// Assert.
	if got := <-handed; got != "b" {
		t.Fatalf("the slot went to %s, want b, which waited longest", got)
	}
}

func TestAReleaseTwiceGivesBackOneSlot(t *testing.T) {
	// Arrange.
	p := New(2)
	release, _ := p.TryAcquire("a")
	p.TryAcquire("b")

	// Act.
	release()
	release()

	// Assert.
	if p.held != 1 {
		t.Fatalf("%d held after a double release, want 1", p.held)
	}
}

func TestTryAcquireDoesNotJumpTheLine(t *testing.T) {
	// Arrange: the one slot is handed to b, who waited, as a releases it.
	p := New(1)
	release, _ := p.TryAcquire("a")
	handed := make(chan Release, 1)
	p.Notify("b", func(r Release) { handed <- r })
	release()
	<-handed

	// Act.
	_, ok := p.TryAcquire("c")

	// Assert.
	if ok {
		t.Fatal("c took a slot the waiter b was handed")
	}
}

func TestThereAreAlwaysAtLeastTwoSlotsByDefault(t *testing.T) {
	// Act.
	got := DefaultSize()

	// Assert.
	if got < 2 {
		t.Fatalf("DefaultSize() = %d, want at least 2", got)
	}
}

func TestAnAcquireWhoseContextAlreadyEndedAnswersItsCause(t *testing.T) {
	// Arrange.
	p := New(1)
	p.TryAcquire("a")
	ctx, cancel := context.WithCancelCause(context.Background())
	stop := errors.New("stop")
	cancel(stop)

	// Act.
	_, err := p.Acquire(ctx, "b")

	// Assert.
	if !errors.Is(err, stop) {
		t.Fatalf("Acquire = %v, want the context's cause", err)
	}
}
