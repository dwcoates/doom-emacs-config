package sessioncontroller

import (
	"sync"
	"testing"
)

// TestTurnLatchMarkAndTest covers the fact a latch records for a turn and the
// turns it must not record it for.
func TestTurnLatchMarkAndTest(t *testing.T) {
	cases := []struct {
		name   string
		mark   string
		test   string
		marked bool
	}{
		{name: "the marked turn holds the fact", mark: "t-1", test: "t-1", marked: true},
		{name: "another turn does not inherit it", mark: "t-1", test: "t-2", marked: false},
		{name: "an unmarked latch holds nothing", mark: "", test: "t-1", marked: false},
		{name: "an empty id is never a member", mark: "", test: "", marked: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			latch := newTurnLatch()

			// Act.
			latch.mark(tc.mark)

			// Assert.
			if got := latch.marked(tc.test); got != tc.marked {
				t.Fatalf("marked(%q) = %v, want %v", tc.test, got, tc.marked)
			}
		})
	}
}

// TestTurnLatchMarkIsIdempotent covers a fact restated: marking twice leaves
// the turn a member exactly as one mark did, and reports no error.
func TestTurnLatchMarkIsIdempotent(t *testing.T) {
	// Arrange.
	latch := newTurnLatch()

	// Act.
	latch.mark("t-1")
	latch.mark("t-1")

	// Assert.
	if !latch.marked("t-1") {
		t.Fatal("a twice-marked turn lost its fact")
	}
}

// TestTurnLatchClaimAdmitsOnlyTheFirst covers the test-and-set: the first
// claimant establishes the fact, the second is told to stand down.
func TestTurnLatchClaimAdmitsOnlyTheFirst(t *testing.T) {
	// Arrange.
	latch := newTurnLatch()

	// Act.
	first := latch.claim("t-1")
	second := latch.claim("t-1")

	// Assert.
	if !first {
		t.Fatal("the first claim was refused")
	}
	if second {
		t.Fatal("the second claim was admitted, so one fact had two authors")
	}
}

// TestTurnLatchClaimAfterMarkIsRefused covers the two operations meeting over
// one turn: a fact already marked cannot then be claimed by anyone.
func TestTurnLatchClaimAfterMarkIsRefused(t *testing.T) {
	// Arrange.
	latch := newTurnLatch()
	latch.mark("t-1")

	// Act.
	claimed := latch.claim("t-1")

	// Assert.
	if claimed {
		t.Fatal("a marked turn was claimed, so the mark did not establish the fact")
	}
}

// TestTurnLatchClaimSeparatesTurns covers the per-turn scoping: one turn's
// established fact never refuses a different turn's first claimant.
func TestTurnLatchClaimSeparatesTurns(t *testing.T) {
	// Arrange.
	latch := newTurnLatch()
	latch.claim("t-1")

	// Act.
	other := latch.claim("t-2")

	// Assert.
	if !other {
		t.Fatal("a second turn's first claim was refused by the first turn's fact")
	}
}

// TestTurnLatchClaimIsAtomicUnderConcurrency covers the reason the test-and-set
// lives inside the latch rather than at its call sites: with many goroutines
// racing over one turn, exactly one may come away believing it is the author.
func TestTurnLatchClaimIsAtomicUnderConcurrency(t *testing.T) {
	// Arrange.
	const claimants = 64
	latch := newTurnLatch()
	var wg sync.WaitGroup
	var mu sync.Mutex
	won := 0
	start := make(chan struct{})

	// Act.
	for i := 0; i < claimants; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			<-start
			if latch.claim("t-1") {
				mu.Lock()
				won++
				mu.Unlock()
			}
		}()
	}
	close(start)
	wg.Wait()

	// Assert.
	if won != 1 {
		t.Fatalf("claim admitted %d of %d concurrent claimants, want exactly 1", won, claimants)
	}
}

// TestConsumerLatchesAreDistinctTurnLatches covers the shape the call sites
// share: the consumer's once-per-turn facts are held by the extracted latch —
// the typed assignments below stop compiling if either site is hand-rolled back
// into a bare map — and each fact is its own set, so a stopped turn is not
// thereby an announced one.
func TestConsumerLatchesAreDistinctTurnLatches(t *testing.T) {
	// Arrange.
	var announced *turnLatch = newTurnLatch()
	var stopped *turnLatch = newTurnLatch()
	c := &consumer{announcedTurnEnds: announced, stoppedTurns: stopped}

	// Act.
	c.noteTurnStopInFlight("t-1")

	// Assert.
	if !c.turnStopInFlight("t-1") {
		t.Fatal("the stopped-turn fact did not reach the consumer's own reader")
	}
	if !c.claimTurnEndAnnouncement("t-1") {
		t.Fatal("a stopped turn was refused its end announcement, so the two facts share one set")
	}
}
