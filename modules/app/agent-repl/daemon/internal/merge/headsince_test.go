package merge

import (
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestHeadSinceIsTheRunningHeadsOwnStamp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	r := &run{ws: theWorkspace, repo: h.repoKey()}
	r.headMS.Store(1_700_000_000_000)
	h.o.mu.Lock()
	h.o.running[h.repoKey()] = r
	h.o.mu.Unlock()

	// Act
	got := h.o.headSince(h.repoKey(), theWorkspace)

	// Assert
	if got != time.UnixMilli(1_700_000_000_000) {
		t.Fatalf("headSince = %v, want the run's own head stamp", got)
	}
}

func TestHeadSinceStampsAWaitingFrontOnceAndKeepsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first := h.o.headSince(h.repoKey(), "ws-waiting")
	h.o.deps.Now()

	// Act
	again := h.o.headSince(h.repoKey(), "ws-waiting")

	// Assert
	if again != first {
		t.Fatalf("the waiting front's head clock moved from %v to %v; want it kept", first, again)
	}
}

func TestHeadSinceStartsANewFrontFromZero(t *testing.T) {
	// Arrange
	h := newHarness(t)
	old := h.o.headSince(h.repoKey(), "ws-old-front")

	// Act
	got := h.o.headSince(h.repoKey(), ids.WorkspaceID("ws-new-front"))

	// Assert
	if !got.After(old) {
		t.Fatalf("the new front's head clock = %v, want a fresh stamp after the old front's %v", got, old)
	}
}

func TestAResumedHeadKeepsTheHeadStampItsRecordCarries(t *testing.T) {
	// Arrange
	h := newHarness(t)
	r := &run{o: h.o, ws: theWorkspace, repo: h.repoKey(), rounds: map[string]int{}, openRounds: map[string]tabRound{}}
	r.restoreFrom(progressDoc{HeadMS: 1_700_000_000_000})

	// Act
	h.o.register(r)

	// Assert
	if got := r.headMS.Load(); got != 1_700_000_000_000 {
		t.Fatalf("the resumed head's stamp = %d, want the recorded 1700000000000", got)
	}
}

func TestAFreshlyAdmittedHeadIsStampedAsItTakesTheSlot(t *testing.T) {
	// Arrange
	h := newHarness(t)
	r := &run{o: h.o, ws: theWorkspace, repo: h.repoKey()}
	before := h.o.nowMS()

	// Act
	h.o.register(r)

	// Assert
	if got := r.headMS.Load(); got <= before {
		t.Fatalf("the fresh head's stamp = %d, want it stamped as it took the slot (after %d)", got, before)
	}
}

func TestAMergePromotedFromBehindTheHeadIsStampedAsItTakesTheSlot(t *testing.T) {
	// Arrange: a speculative run first in its chain, its entry admitted
	// first in line, and no head running.
	h := newHarness(t)
	r := &run{o: h.o, ws: theWorkspace, repo: h.repoKey(), speculative: true}
	h.o.mu.Lock()
	h.o.chains[h.repoKey()] = []*run{r}
	h.o.mu.Unlock()
	before := h.o.nowMS()

	// Act
	promoted, ok := h.o.promote(h.repoKey(), []wsm.MergeQueueEntry{{Repo: h.repoKey(), Workspace: theWorkspace, State: wsm.MergeAdmitted}})

	// Assert
	if !ok || promoted != r {
		t.Fatalf("promote = (%v, %v), want the speculative run promoted", promoted, ok)
	}
	if got := r.headMS.Load(); got <= before {
		t.Fatalf("the promoted head's stamp = %d, want it stamped as it took the slot (after %d)", got, before)
	}
}
