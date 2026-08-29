package feed

import (
	"context"
	"errors"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
)

// THE PIN is the whole point of the token: a reader misses no row between the
// page it was served and the first row it streams, and sees none twice.

func TestTailReplaysRowsPublishedBetweenThePageAndTheWatch(t *testing.T) {
	// Arrange: a page is served, then two rows land before the tail opens.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "on the page")
	page, token := h.openPage(rootFeed(), "reader-1")
	if got := len(pageRows(t, page)); got != 1 {
		t.Fatalf("page rows = %d, want 1", got)
	}
	h.deliverPrompt("turn-2", "after the page")
	h.deliverPrompt("turn-3", "also after the page")

	// Act.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Assert: exactly the two rows published after the page, in order, and the
	// paged row is NOT replayed.
	first := <-rows
	second := <-rows
	if first.GetId().GetValue() == h.promptRowID("turn-1") {
		t.Fatal("the tail replayed a row the page already carried")
	}
	if first.GetId().GetValue() != h.promptRowID("turn-2") {
		t.Fatalf("first streamed row = %q, want turn-2's", first.GetId().GetValue())
	}
	if second.GetId().GetValue() != h.promptRowID("turn-3") {
		t.Fatalf("second streamed row = %q, want turn-3's", second.GetId().GetValue())
	}
}

func TestTailFollowsRowsPublishedAfterItOpened(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act.
	h.deliverPrompt("turn-1", "live")

	// Assert.
	row := <-rows
	if row.GetId().GetValue() != h.promptRowID("turn-1") {
		t.Fatalf("streamed row = %q, want turn-1's", row.GetId().GetValue())
	}
}

func TestTailDeliversEachRowExactlyOnce(t *testing.T) {
	// Arrange: one row before the tail and one after, so both paths are live.
	h := newHarness(t)
	_, token := h.openPage(rootFeed(), "reader-1")
	h.deliverPrompt("turn-1", "before the watch")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act.
	h.deliverPrompt("turn-2", "after the watch")

	// Assert: two deliveries, two distinct rows.
	seen := map[string]int{}
	seen[(<-rows).GetId().GetValue()]++
	seen[(<-rows).GetId().GetValue()]++
	for id, count := range seen {
		if count != 1 {
			t.Fatalf("row %q delivered %d times, want exactly once", id, count)
		}
	}
	if len(seen) != 2 {
		t.Fatalf("distinct rows = %d, want 2", len(seen))
	}
}

func TestTailClosesOnlyOnTheReadersCancellation(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act.
	cancel()

	// Assert: the channel closes — and a stream ending any other way would be
	// a transport failure, which is exactly the distinction this preserves.
	if _, open := <-rows; open {
		t.Fatal("the tail delivered a row after cancellation")
	}
}

func TestTailRefusesATokenThisDaemonNeverMinted(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.resolver.Tail(context.Background(), testWorkspace, rootFeed(),
		&agentreplv1.FeedWatchToken{Value: "forged"})

	// Assert.
	if !errors.Is(err, ErrUnknownToken) {
		t.Fatalf("Tail err = %v, want ErrUnknownToken", err)
	}
	if !h.hasRecord("warn", "daemon.feed.tail_unknown_token") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.tail_unknown_token", h.records())
	}
}

func TestTailRefusesATokenMintedForAnotherFeed(t *testing.T) {
	// Arrange: a token minted on the root feed.
	h := newHarness(t)
	_, token := h.openPage(rootFeed(), "reader-1")
	other := feedid.Feed{Agent: &conversationv1.AgentId{Value: "agent-2"}}

	// Act: the same token offered for a sub-feed.
	_, err := h.resolver.Tail(context.Background(), testWorkspace, other, token)

	// Assert.
	if !errors.Is(err, ErrUnknownToken) {
		t.Fatalf("Tail err = %v, want ErrUnknownToken for a foreign feed", err)
	}
}

func TestTailRefusesAPinThatFellOutOfTheRetainedLog(t *testing.T) {
	// Arrange: a retention of one, and two publications after the pin.
	h := newHarness(t)
	h.resolver.deps.TailRetention = 1
	_, token := h.openPage(rootFeed(), "reader-1")
	h.deliverPrompt("turn-1", "first")
	h.deliverPrompt("turn-2", "second")

	// Act.
	_, err := h.resolver.Tail(context.Background(), testWorkspace, rootFeed(), token)

	// Assert: refused rather than silently gapped.
	if !errors.Is(err, ErrTokenExpired) {
		t.Fatalf("Tail err = %v, want ErrTokenExpired", err)
	}
}

func TestEachOpenMintsItsOwnToken(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	_, first := h.openPage(rootFeed(), "reader-1")
	_, second := h.openPage(rootFeed(), "reader-2")

	// Assert.
	if first.GetValue() == second.GetValue() {
		t.Fatalf("both opens minted %q, want distinct tokens", first.GetValue())
	}
}

// promptRowID is the identity a user-prompt row for this turn carries.
func (h *harness) promptRowID(turn string) string {
	return testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: turn},
	}).GetValue()
}
