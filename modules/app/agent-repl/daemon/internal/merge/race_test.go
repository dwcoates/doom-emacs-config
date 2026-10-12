package merge

import (
	"context"
	"slices"
	"testing"
)

func TestTheMergeBehindTheHeadWinsTheRaceWhenItsGatePassesFirst(t *testing.T) {
	// Arrange: the head holds in its gate; the merge behind it passes.
	c := newChain(t)
	c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")

	// Act: the head yields to the winner, the winner lands, and the head
	// starts over on the new tip and lands.
	if err := <-admitAsync(c.h, context.Background()); err != nil {
		t.Fatalf("the overtaken head's admission ended on: %v", err)
	}
	c.promote(t)
	c.promote(t)

	// Assert.
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"b1"}) || !slices.Equal(got[1], []string{"a1"}) {
		t.Fatalf("landings brought %v, want [b1] then [a1]", got)
	}
	if tree := c.git.tree("master"); tree != "a1+b1+base" {
		t.Fatalf("master holds %s, want a1+b1+base", tree)
	}
}

func TestTheHeadWinsTheRaceWhenItsGatePassesFirst(t *testing.T) {
	// Arrange: both gates hold; the head is let go first, while the merge
	// behind it is still inside its gate on the old tip.
	c := newChain(t)
	_, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	behindGate, _ := c.runner.hold(c.dirs["ws-b"])
	done := admitAsync(c.h, context.Background())
	<-behindGate

	// Act: the head lands; the merge behind it, its gate on the old tip cut,
	// starts over on the new tip as the head and lands.
	close(releaseHead)
	if err := <-done; err != nil {
		t.Fatalf("the head's admission ended on: %v", err)
	}
	c.promote(t)

	// Assert.
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"a1"}) || !slices.Equal(got[1], []string{"b1"}) {
		t.Fatalf("landings brought %v, want [a1] then [b1]", got)
	}
	if got := c.runner.ran(c.dirs["ws-b"]); got != 2 {
		t.Fatalf("the merge behind the head ran its gate %d times, want twice: its run on the old tip was cut", got)
	}
}
