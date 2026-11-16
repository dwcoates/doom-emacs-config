package merge

import (
	"context"
	"math/rand/v2"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// stateArm is the state arm TAB's kind draws.
func stateArm(tab *frontendv1.FeedMergeTab) string {
	m := tab.ProtoReflect()
	kind := m.WhichOneof(m.Descriptor().Oneofs().ByName("kind"))
	inner := m.Get(kind).Message()
	return string(inner.WhichOneof(inner.Descriptor().Oneofs().ByName("state")).Name())
}

func TestRenderTabDrawsAHeldBubblesRunningTabOnHold(t *testing.T) {
	held := bubbleHold{held: true, heldMS: 50}
	tests := []struct {
		name      string
		tab       *frontendv1.FeedMergeTab
		hold      bubbleHold
		wantState string
	}{
		{name: "a live tab of a held bubble is on hold", tab: committingTab(tabState{startedMS: 10}, "s"), hold: held, wantState: "on_hold"},
		{name: "a tab waiting on the user of a held bubble is on hold", tab: pickTab(tabState{startedMS: 10, waiting: true}), hold: held, wantState: "on_hold"},
		{name: "a settled tab of a held bubble stays settled", tab: committingTab(tabState{startedMS: 10, settled: true, endedMS: 20}, "s"), hold: held, wantState: "settled"},
		{name: "a live tab of a running bubble stays live", tab: committingTab(tabState{startedMS: 10}, "s"), wantState: "live"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := renderTab(tt.tab, tt.hold)

			// Assert
			if arm := stateArm(got); arm != tt.wantState {
				t.Fatalf("drawn state %s, want %s", arm, tt.wantState)
			}
			if tt.wantState == "on_hold" {
				onHold := committingOrPick(got)
				if onHold.GetStartedAtMs() != 10 || onHold.GetHeldSinceMs() != 50 {
					t.Fatalf("on hold = %+v, want started 10 held since 50", onHold)
				}
				if stateArm(tt.tab) == "on_hold" {
					t.Fatal("renderTab changed the published tab it was handed")
				}
			}
		})
	}
}

// committingOrPick is the on-hold leaf a committing or pick tab carries.
func committingOrPick(tab *frontendv1.FeedMergeTab) *frontendv1.FeedMergeTabHeld {
	if c := tab.GetCommitting(); c != nil {
		return c.GetOnHold()
	}
	return tab.GetResolving().GetOnHold()
}

// EVERY TAB KIND CAN BE DRAWN ON HOLD: renderTab panics on a kind whose state
// has no on_hold arm, so a new kind without one fails here, not in a bubble.
func TestEveryTabKindsStateHasAnOnHoldArm(t *testing.T) {
	kinds := (&frontendv1.FeedMergeTab{}).ProtoReflect().Descriptor().Oneofs().ByName("kind").Fields()
	for i := range kinds.Len() {
		kind := kinds.Get(i)
		t.Run(string(kind.Name()), func(t *testing.T) {
			states := kind.Message().Oneofs().ByName("state")
			if states == nil {
				t.Fatal("the kind has no state oneof")
			}
			onHold := states.Fields().ByName("on_hold")
			if onHold == nil || onHold.Kind() != protoreflect.MessageKind || onHold.Message().FullName() != (&frontendv1.FeedMergeTabHeld{}).ProtoReflect().Descriptor().FullName() {
				t.Fatalf("the kind's state has no FeedMergeTabHeld on_hold arm")
			}
		})
	}
}

// tabRowsOf answers every merge tab row the feed was pushed for LEASE's
// bubble, in push order.
func tabRowsOf(h *harness, lease ids.LeaseID) []*frontendv1.FeedMergeTab {
	h.feed.mu.Lock()
	defer h.feed.mu.Unlock()
	var out []*frontendv1.FeedMergeTab
	for _, r := range h.feed.rows {
		if r.Feed.Merge != nil && *r.Feed.Merge == lease && r.Row.GetMergeTab() != nil {
			out = append(out, r.Row.GetMergeTab())
		}
	}
	return out
}

func TestAQueueThatHoldsItsMergesRedrawsTheirBubblesOnHold(t *testing.T) {
	// Arrange: a waiting merge's live tab, then its queue paused.
	h := newHarness(t)
	seedQueue(h, h.repoKey(), false)
	lease := ids.LeaseID("lease-bubble")
	h.o.mu.Lock()
	h.o.ledgerOf[theWorkspace] = lease
	h.o.mu.Unlock()
	h.o.publishTab(theWorkspace, lease, roundTabKey(TabCommitting, 1), committingTab(tabState{startedMS: 10}, "s"))
	h.db.mu.Lock()
	h.db.paused[h.repoKey()] = true
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	rows := tabRowsOf(h, lease)
	if len(rows) != 2 || stateArm(rows[0]) != "live" || stateArm(rows[1]) != "on_hold" {
		t.Fatalf("tab rows = %v, want the live tab then its redraw on hold", rows)
	}
}

// THE BUBBLE AND THE ROSTER ARE ONE VIEW: for every generated ledger and
// queue hold, the bubble draws its running tab on hold exactly when the
// roster's view holds a stage held.
func TestTheBubbleAndTheRosterAgreeOnEveryGeneratedLedger(t *testing.T) {
	rng := rand.New(rand.NewPCG(3, 4))
	lease := ids.LeaseID("lease-g")
	for i := range 2000 {
		// Arrange
		rounds := generatedLedger(rng)
		queueHeld := rng.IntN(2) == 0
		heldRounds := map[wsm.LeaseID]int64{}
		live := false
		for _, r := range rounds {
			if r.EndedAt == nil {
				live = true
				if r.HeldSince != nil {
					heldRounds[lease] = r.HeldSince.UnixMilli()
				}
			}
		}
		if !live {
			continue
		}

		// Act
		view, err := viewOf(rounds, queueHeld)
		if err != nil {
			t.Fatalf("case %d: viewOf = %v", i, err)
		}
		drawn := renderTab(committingTab(tabState{startedMS: 10}, "s"), holdOf(lease, queueHeld, heldRounds))

		// Assert
		viewHeld := false
		for _, st := range view.stages {
			viewHeld = viewHeld || st.state == stageHeld
		}
		if (stateArm(drawn) == "on_hold") != viewHeld {
			t.Fatalf("case %d: the bubble drew %s, the roster's view holds a stage held: %v", i, stateArm(drawn), viewHeld)
		}
	}
}

func TestAQueueThatHoldsItsMergesPublishesTheirStepsHeld(t *testing.T) {
	// Arrange: a merging workspace's facts, then its queue paused.
	h := newHarness(t)
	seedQueue(h, h.repoKey(), false)
	h.o.mu.Lock()
	h.o.ledgerOf[theWorkspace] = "lease-footer"
	h.o.mu.Unlock()
	h.o.publish(h.repoKey(), theWorkspace, MergeFacts{State: StateMerging, Step: footer.StepTesting, QueuedAt: h.clock()})
	h.db.mu.Lock()
	h.db.paused[h.repoKey()] = true
	h.db.mu.Unlock()

	// Act
	h.o.publishRosterQueue(context.Background(), h.repoKey())

	// Assert
	if facts := h.footer.last(); facts.State != StateMerging || !facts.Held {
		t.Fatalf("footer facts = %+v, want the testing step published held", facts)
	}
}
