package merge

import (
	"context"
	"slices"
	"testing"
)

// behindWins runs the race the merge behind the head wins: the head's first
// ask for its repository's test turn is parked, so the merge behind it takes
// the turn, passes on the target's tip and overtakes the head before the head
// ever tests. However the scheduler orders the two, the head cannot pass
// first: it never holds the turn. It answers once the overtaken head has let
// go, then drains the queue.
func (c *chain) behindWins(t *testing.T) {
	t.Helper()
	release := c.parkTurn(theWorkspace)
	done := admitAsync(c.h, context.Background())
	if err := <-done; err != nil {
		t.Fatalf("the overtaken head's admission ended on: %v", err)
	}
	release()
	c.drain(t)
}

func TestTheMergeBehindTheHeadWinsTheRaceWhenItsGatePassesFirst(t *testing.T) {
	// Arrange: the head holds in its gate; the merge behind it passes.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")

	// Act: the head yields to the winner, the winner lands, and the head
	// starts over on the new tip and lands.
	c.behindWins(t)

	// Assert.
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"b1"}) || !slices.Equal(got[1], []string{"a1"}) {
		t.Fatalf("landings brought %v, want [b1] then [a1]", got)
	}
	if tree := c.git.tree("master"); tree != "a1+b1+base" {
		t.Fatalf("master holds %s, want a1+b1+base", tree)
	}
}

func TestTheHeadWinsTheRaceWhenItsGatePassesFirst(t *testing.T) {
	// Arrange: the head holds in its gate, so the merge behind it waits in
	// line for the test turn.
	c := newChain(t)
	_, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())
	c.awaitLined(t, "ws-b")

	// Act: the head lands; the merge behind it starts over on the new tip as
	// the head and lands.
	close(releaseHead)
	if err := <-done; err != nil {
		t.Fatalf("the head's admission ended on: %v", err)
	}
	c.promote(t)

	// Assert.
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"a1"}) || !slices.Equal(got[1], []string{"b1"}) {
		t.Fatalf("landings brought %v, want [a1] then [b1]", got)
	}
	if got := c.runner.overlapped(); got != 1 {
		t.Fatalf("%d gates of the repository ran at once, want one at a time", got)
	}
}
