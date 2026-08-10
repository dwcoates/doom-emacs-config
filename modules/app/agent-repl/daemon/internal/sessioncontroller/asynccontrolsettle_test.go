package sessioncontroller

import (
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// --- the shared control-plane settle helpers -------------------------------
//
// controlSettlePush is what EVERY control-ordered settlement shares: the gap
// classification, the fault channel, and the caller's own sentence for what is
// left over. These cover the invariants it now guarantees for all its callers,
// which is precisely what a per-route copy could no longer be trusted to hold.

func TestControlSettlePushCarriesTheUpdatesThrough(t *testing.T) {
	// Arrange
	c := &consumer{workspace: "/ws", sessionID: "s1", logf: func(string, ...any) {}}
	ups := []*frontendv1.AsyncBubbleUpdate{{BubbleId: "b1"}, {BubbleId: "b2"}}

	// Act
	push := c.controlSettlePush(ups, nil, func(error) {
		t.Fatal("a settlement with no error must not report a degradation")
	})

	// Assert
	if len(push.Updates) != 2 {
		t.Fatalf("updates = %d, want 2", len(push.Updates))
	}
}

func TestControlSettlePushRoutesAGapToTheFaultChannel(t *testing.T) {
	// Arrange — a classified async gap, which becomes a failure card rather
	// than a warn.
	c := &consumer{workspace: "/ws", sessionID: "s1", logf: func(string, ...any) {}}
	gap := &frontend.AsyncGapError{BubbleID: "b1", Detail: "the bubble is gone"}

	// Act
	push := c.controlSettlePush(nil, gap, func(error) {
		t.Fatal("a classified gap is a fault, never the leftover warn")
	})

	// Assert
	if len(push.Faults) != 1 {
		t.Fatalf("faults = %d, want 1: a classified gap is the user's failure card", len(push.Faults))
	}
}

func TestControlSettlePushGivesTheLeftoverToTheCallersSentence(t *testing.T) {
	// Arrange — an unclassified refusal, which each route describes in its own
	// words because what it leaves on screen differs by route.
	c := &consumer{workspace: "/ws", sessionID: "s1", logf: func(string, ...any) {}}
	var got error

	// Act
	push := c.controlSettlePush(nil, errors.New("settlement refused"), func(residual error) {
		got = residual
	})

	// Assert
	if got == nil {
		t.Fatal("the leftover refusal never reached the caller's sentence")
	}
	if len(push.Faults) != 0 {
		t.Fatalf("faults = %d, want 0: an unclassified refusal is a warn, not a card", len(push.Faults))
	}
}

func TestPublishControlSettlePushesNothingForAnEmptySettlement(t *testing.T) {
	// Arrange
	push := &fakePusher{}
	c := &consumer{workspace: "/ws", sessionID: "s1", push: push, logf: func(string, ...any) {}}

	// Act
	c.publishControlSettle(asyncPush{}, 7, "probe")

	// Assert: a settlement that settled nothing is not a frame.
	push.mu.Lock()
	defer push.mu.Unlock()
	if len(push.bubbles) != 0 {
		t.Fatalf("async pushes = %d, want 0", len(push.bubbles))
	}
}

func TestPublishControlSettleCarriesTheGivenThroughSeq(t *testing.T) {
	// Arrange
	pusher := &fakePusher{}
	c := &consumer{workspace: "/ws", sessionID: "s1", push: pusher, logf: func(string, ...any) {}}

	// Act
	c.publishControlSettle(asyncPush{Updates: []*frontendv1.AsyncBubbleUpdate{{BubbleId: "b1"}}}, 7, "probe")

	// Assert: through_seq is the client's replay cursor, so the publisher must
	// carry the caller's number rather than mint one.
	pusher.mu.Lock()
	defer pusher.mu.Unlock()
	if len(pusher.bubbles) != 1 {
		t.Fatalf("async pushes = %d, want 1", len(pusher.bubbles))
	}
	if got := pusher.bubbles[0].GetThroughSeq(); got != 7 {
		t.Fatalf("through_seq = %d, want 7", got)
	}
}
