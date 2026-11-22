package merge

import (
	"context"
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// staleLedger is the ledger of a bubble an earlier daemon left live.
const staleLedger = ids.LeaseID("stale-ledger")

// headPushesOf answers every head push of LEDGER's bubble, in order.
func headPushesOf(h *harness, ledger ids.LeaseID) []synthesized {
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	var out []synthesized
	for _, row := range h.feed.rows {
		if got, isHead := mergeHeadLedger(row.Row.GetId().GetValue()); isHead && got == ledger {
			out = append(out, row)
		}
	}
	return out
}

// tabPushesOf answers every tab push on LEDGER's bubble, in order.
func tabPushesOf(h *harness, ledger ids.LeaseID) []synthesized {
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	var out []synthesized
	for _, row := range h.feed.rows {
		if row.Feed.Merge != nil && *row.Feed.Merge == ledger {
			out = append(out, row)
		}
	}
	return out
}

// requireOffQueueTerminal fails unless PUSH is LEDGER's head ended abandoned
// for having left its queue, settled rather than pinned.
func requireOffQueueTerminal(t *testing.T, push synthesized) {
	t.Helper()
	abandoned := push.Row.GetActivity().GetMerge().GetError().GetAbandoned()
	if abandoned == nil || abandoned.GetSummary() != offQueueSummary {
		t.Fatalf("the head ended as %v, want abandoned reading %q", push.Row.GetActivity().GetMerge().GetResult(), offQueueSummary)
	}
	if push.Pinned {
		t.Fatal("the ended head is still pinned to the feed's bottom")
	}
}

// liveQueueTabRow is a queue tab of LEDGER's bubble still running.
func liveQueueTabRow(ws ids.WorkspaceID, ledger ids.LeaseID) *frontendv1.FeedRow {
	return tabRow(ws, ledger, roundTabKey(TabQueue, 1), queueTab(&frontendv1.FeedMergeQueue{}, tabState{startedMS: 1000}))
}

// storeLiveBubble records in the store the live bubble an earlier daemon
// drew for LEDGER in WS's feed: its pinned head and its running queue tab.
func storeLiveBubble(h *harness, ws ids.WorkspaceID, ledger ids.LeaseID) {
	h.db.putDurable(ws, headRow(ws, ledger, branchLineOf("feature", "master"), "", 1000, headWaiting), true)
	h.db.putDurable(ws, liveQueueTabRow(ws, ledger), false)
}

// TestEndBubblesOffQueueEndsABubbleWhoseMergeLeftTheQueue covers the
// invariant: a merge that left its queue with no terminal of its own leaves
// no live bubble once a queue is published, and its running tab settles.
func TestEndBubblesOffQueueEndsABubbleWhoseMergeLeftTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	ledger, ok := h.o.leaseOf(theWorkspace)
	if !ok {
		t.Fatal("the queued merge has no ledger")
	}
	if err := h.db.RemoveMergeQueueEntry(context.Background(), h.repoKey(), theWorkspace, causeRestartFailed); err != nil {
		t.Fatalf("removing the entry: %v", err)
	}

	// Act
	err := h.o.republishQueue(context.Background(), h.repoKey())

	// Assert
	if err != nil {
		t.Fatalf("republishQueue: %v", err)
	}
	heads := headPushesOf(h, ledger)
	requireOffQueueTerminal(t, heads[len(heads)-1])
	tabs := tabPushesOf(h, ledger)
	settled := tabs[len(tabs)-1].Row.GetMergeTab().GetQueue().GetSettled()
	if settled.GetFailed().GetSummary() != offQueueSummary || settled.GetStartedAtMs() == 0 {
		t.Fatalf("the queue tab is %v, want it settled failed keeping when it began", tabs[len(tabs)-1].Row.GetMergeTab())
	}
	if _, found := h.recordFor("info", opLiveBubbles); !found {
		t.Fatalf("no INFO %s record says the bubble was ended: %+v", opLiveBubbles, h.logs.Records())
	}
}

// TestEndBubblesOffQueueRefusesALaterLiveHead covers the latch: once ended,
// a restate decided from an earlier read never draws the bubble live again.
func TestEndBubblesOffQueueRefusesALaterLiveHead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	ledger, _ := h.o.leaseOf(theWorkspace)
	if err := h.db.RemoveMergeQueueEntry(context.Background(), h.repoKey(), theWorkspace, causeRestartFailed); err != nil {
		t.Fatalf("removing the entry: %v", err)
	}
	if err := h.o.republishQueue(context.Background(), h.repoKey()); err != nil {
		t.Fatalf("republishQueue: %v", err)
	}
	before := len(headPushesOf(h, ledger))

	// Act
	h.o.restateHead(theWorkspace, headReveal)

	// Assert
	if after := len(headPushesOf(h, ledger)); after != before {
		t.Fatalf("the ended bubble was drawn again (%d pushes, was %d)", after, before)
	}
}

// TestEndBubblesOffQueueLeavesAQueuedBubbleLive covers the other half: a
// merge still in its queue keeps its live bubble.
func TestEndBubblesOffQueueLeavesAQueuedBubbleLive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	ledger, _ := h.o.leaseOf(theWorkspace)

	// Act
	err := h.o.republishQueue(context.Background(), h.repoKey())

	// Assert
	if err != nil {
		t.Fatalf("republishQueue: %v", err)
	}
	heads := headPushesOf(h, ledger)
	if last := heads[len(heads)-1]; !headRowIsLive(last.Row) || !last.Pinned {
		t.Fatalf("the queued merge's head is %v (pinned %v), want it live and pinned", last.Row.GetActivity().GetMerge().GetResult(), last.Pinned)
	}
}

// TestEndBubblesOffQueueLeavesALegacyRowsMintedLedgerLive covers a row an
// earlier build queued with no ledger: the ledger this daemon minted for it
// is the one its queue holds.
func TestEndBubblesOffQueueLeavesALegacyRowsMintedLedgerLive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	ledger, _ := h.o.leaseOf(theWorkspace)
	h.db.mu.Lock()
	for i := range h.db.queues[h.repoKey()] {
		h.db.queues[h.repoKey()][i].Ledger = ""
	}
	h.db.mu.Unlock()

	// Act
	err := h.o.endBubblesOffQueue(context.Background(), "test")

	// Assert
	if err != nil {
		t.Fatalf("endBubblesOffQueue: %v", err)
	}
	heads := headPushesOf(h, ledger)
	if !headRowIsLive(heads[len(heads)-1].Row) {
		t.Fatal("the legacy row's bubble was ended while its merge waits in line")
	}
}

// TestRecoverEndsABubbleAnEarlierDaemonLeftLive covers the restart: a live
// bubble in the store whose ledger no queue holds is ended at boot, its
// running tab settled, so none survives a restart.
func TestRecoverEndsABubbleAnEarlierDaemonLeftLive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	storeLiveBubble(h, theWorkspace, staleLedger)

	// Act
	err := h.o.Recover(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Recover: %v", err)
	}
	heads := headPushesOf(h, staleLedger)
	if len(heads) != 1 {
		t.Fatalf("the stale head was pushed %d times, want once", len(heads))
	}
	requireOffQueueTerminal(t, heads[0])
	tabs := tabPushesOf(h, staleLedger)
	if len(tabs) != 1 || tabs[0].Row.GetMergeTab().GetQueue().GetSettled().GetFailed() == nil {
		t.Fatalf("the stale queue tab pushes are %v, want one settled failed", tabs)
	}
}

// TestAdoptWorkspaceEndsABubbleTheOutgoingDaemonLeftLive covers the handover:
// the adopted workspace's stored live bubbles are known and ended like a
// boot's.
func TestAdoptWorkspaceEndsABubbleTheOutgoingDaemonLeftLive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	storeLiveBubble(h, theWorkspace, staleLedger)

	// Act
	err := h.o.AdoptWorkspace(context.Background(), theWorkspace)

	// Assert
	if err != nil {
		t.Fatalf("AdoptWorkspace: %v", err)
	}
	heads := headPushesOf(h, staleLedger)
	if len(heads) != 1 {
		t.Fatalf("the stale head was pushed %d times, want once", len(heads))
	}
	requireOffQueueTerminal(t, heads[0])
}

// TestEndBubblesOffQueueForgetsABubbleTheStoreHoldsSettled covers a bubble
// another daemon ended meanwhile: it is forgotten and not drawn again.
func TestEndBubblesOffQueueForgetsABubbleTheStoreHoldsSettled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	storeLiveBubble(h, theWorkspace, staleLedger)
	if err := h.o.seedLiveBubbles(context.Background(), nil); err != nil {
		t.Fatalf("seedLiveBubbles: %v", err)
	}
	h.db.putDurable(theWorkspace, headRow(theWorkspace, staleLedger, branchLineOf("feature", "master"), "", 1000,
		&frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: "failed"}}}), false)

	// Act
	err := h.o.endBubblesOffQueue(context.Background(), "test")

	// Assert
	if err != nil {
		t.Fatalf("endBubblesOffQueue: %v", err)
	}
	if pushes := headPushesOf(h, staleLedger); len(pushes) != 0 {
		t.Fatalf("a head the store holds settled was drawn again: %v", pushes)
	}
	if _, live := h.o.heads.live[staleLedger]; live {
		t.Fatal("the settled bubble is still known as live")
	}
	if _, found := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "debug", Operation: opLiveBubbles}); !found {
		t.Fatalf("no DEBUG %s record says the bubble was forgotten", opLiveBubbles)
	}
}

// TestEndBubblesOffQueueFailsLoudlyOnAnUnreadableQueue covers the failed
// queue read: it is answered and recorded at ERROR, and nothing is ended.
func TestEndBubblesOffQueueFailsLoudlyOnAnUnreadableQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	storeLiveBubble(h, theWorkspace, staleLedger)
	if err := h.o.seedLiveBubbles(context.Background(), nil); err != nil {
		t.Fatalf("seedLiveBubbles: %v", err)
	}
	h.db.allQueuesErr = errors.New("disk gone")

	// Act
	err := h.o.endBubblesOffQueue(context.Background(), "test")

	// Assert
	if err == nil {
		t.Fatal("an unreadable queue ended the bubbles anyway")
	}
	if pushes := headPushesOf(h, staleLedger); len(pushes) != 0 {
		t.Fatalf("a bubble was ended with no queue read: %v", pushes)
	}
	rec, found := h.recordFor("error", opLiveBubbles)
	if !found || rec.Context["why"] != "test" || rec.Context["cause"] == nil {
		t.Fatalf("no ERROR %s record names why and the cause: %+v", opLiveBubbles, rec)
	}
}

// TestEndBubblesOffQueueFailsLoudlyOnAnUnreadableBubble covers the failed
// read of the bubble itself: answered at ERROR naming the ledger, the
// bubble left known for the next publication.
func TestEndBubblesOffQueueFailsLoudlyOnAnUnreadableBubble(t *testing.T) {
	// Arrange
	h := newHarness(t)
	storeLiveBubble(h, theWorkspace, staleLedger)
	if err := h.o.seedLiveBubbles(context.Background(), nil); err != nil {
		t.Fatalf("seedLiveBubbles: %v", err)
	}
	h.db.matchErr = errors.New("disk gone")

	// Act
	err := h.o.endBubblesOffQueue(context.Background(), "test")

	// Assert
	if err == nil {
		t.Fatal("an unreadable bubble was ended anyway")
	}
	if _, live := h.o.heads.live[staleLedger]; !live {
		t.Fatal("the unread bubble was forgotten")
	}
	rec, found := h.recordFor("error", opLiveBubbles)
	if !found || rec.Context["ledger"] != string(staleLedger) {
		t.Fatalf("no ERROR %s record names the ledger: %+v", opLiveBubbles, rec)
	}
}

// TestRecoverFailsLoudlyWhenTheLiveHeadsCannotBeRead covers the seed's failed
// read: the boot fails, recorded at ERROR, drawing nothing.
func TestRecoverFailsLoudlyWhenTheLiveHeadsCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	storeLiveBubble(h, theWorkspace, staleLedger)
	h.db.matchErr = errors.New("disk gone")

	// Act
	err := h.o.Recover(context.Background())

	// Assert
	if err == nil {
		t.Fatal("the boot went on with the live bubbles unread")
	}
	if pushes := headPushesOf(h, staleLedger); len(pushes) != 0 {
		t.Fatalf("a bubble was drawn: %v", pushes)
	}
	if _, found := h.recordFor("error", opLiveBubbles); !found {
		t.Fatalf("no ERROR %s record says the read failed", opLiveBubbles)
	}
}

// TestSeedLiveBubblesKeepsAResolvedLedgerResolved covers a bubble this daemon
// already ended: a stale live record of it is not taken back as live.
func TestSeedLiveBubblesKeepsAResolvedLedgerResolved(t *testing.T) {
	// Arrange
	h := newHarness(t)
	storeLiveBubble(h, theWorkspace, staleLedger)
	h.o.heads.resolved[staleLedger] = true

	// Act
	err := h.o.seedLiveBubbles(context.Background(), nil)

	// Assert
	if err != nil {
		t.Fatalf("seedLiveBubbles: %v", err)
	}
	if _, live := h.o.heads.live[staleLedger]; live {
		t.Fatal("a resolved ledger was taken back as live")
	}
}

// TestSettleRunningStates covers the badge walk every ended bubble's tabs
// take: each running badge settles failed keeping when it began, and
// nothing else changes.
func TestSettleRunningStates(t *testing.T) {
	cases := []struct {
		name        string
		tab         *frontendv1.FeedMergeTab
		wantChanged bool
		wantStarted int64
	}{
		{name: "live", tab: queueTab(&frontendv1.FeedMergeQueue{}, tabState{startedMS: 7}), wantChanged: true, wantStarted: 7},
		{name: "waiting on the user", tab: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Rebasing{Rebasing: &frontendv1.FeedMergeTabRebasing{
			State: &frontendv1.FeedMergeTabRebasing_WaitingOnUser{WaitingOnUser: &frontendv1.FeedMergeTabWaitingOnUser{StartedAtMs: 8}}}}}, wantChanged: true, wantStarted: 8},
		{name: "on hold", tab: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Committing{Committing: &frontendv1.FeedMergeTabCommitting{
			State: &frontendv1.FeedMergeTabCommitting_OnHold{OnHold: &frontendv1.FeedMergeTabHeld{StartedAtMs: 9, HeldSinceMs: 10}}}}}, wantChanged: true, wantStarted: 9},
		{name: "already settled", tab: queueTab(&frontendv1.FeedMergeQueue{}, tabState{startedMS: 7, settled: true, endedMS: 8}), wantChanged: false, wantStarted: 7},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			before := proto.Clone(tc.tab)

			// Act
			changed := settleRunningStates(tc.tab.ProtoReflect(), 99, "gone")

			// Assert
			if changed != tc.wantChanged {
				t.Fatalf("changed = %v, want %v", changed, tc.wantChanged)
			}
			if !tc.wantChanged {
				if !proto.Equal(before, tc.tab) {
					t.Fatalf("a tab with nothing running changed: %v", tc.tab)
				}
				return
			}
			inner := tc.tab.ProtoReflect().Get(tc.tab.ProtoReflect().WhichOneof(tc.tab.ProtoReflect().Descriptor().Oneofs().ByName("kind"))).Message()
			settled, ok := inner.Get(inner.WhichOneof(inner.Descriptor().Oneofs().ByName("state"))).Message().Interface().(*frontendv1.FeedMergeTabSettled)
			if !ok || settled.GetEndedAtMs() != 99 || settled.GetStartedAtMs() != tc.wantStarted || settled.GetFailed().GetSummary() != "gone" {
				t.Fatalf("the badge is %v, want settled failed (gone) from %d to 99", inner.Interface(), tc.wantStarted)
			}
		})
	}
}

// TestBubbleRowOf covers which rows are one bubble's: its head on the root
// feed and its tabs on its own sub-feed, and no other.
func TestBubbleRowOf(t *testing.T) {
	other := ids.LeaseID("other-ledger")
	cases := []struct {
		name string
		id   string
		want bool
	}{
		{name: "its head", id: feedid.Encode(headRef(theWorkspace, staleLedger)).GetValue(), want: true},
		{name: "its tab", id: liveQueueTabRow(theWorkspace, staleLedger).GetId().GetValue(), want: true},
		{name: "another bubble's head", id: feedid.Encode(headRef(theWorkspace, other)).GetValue(), want: false},
		{name: "another bubble's tab", id: liveQueueTabRow(theWorkspace, other).GetId().GetValue(), want: false},
		{name: "an id that decodes to nothing", id: "not-an-id", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := bubbleRowOf(tc.id, staleLedger)

			// Assert
			if got != tc.want {
				t.Fatalf("bubbleRowOf(%s) = %v, want %v", tc.name, got, tc.want)
			}
		})
	}
}
