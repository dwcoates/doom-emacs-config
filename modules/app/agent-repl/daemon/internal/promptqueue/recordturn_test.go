package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// recordturn_test.go covers recordTurn: every turn this queue opens is recorded
// with the address it draws at, chosen by its origin, and that same address is
// handed to the feed, which is what both the live draw and a replay place the
// turn by.

// mergeTabAddress is a merge tab's output address.
func mergeTabAddress() *sessionwatcher.OutputAddress {
	lease := wsm.NewLeaseID()
	parent := feedid.Ref{WS: theWorkspace, Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: string(lease)}}
	return &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}, Parent: &parent}
}

// recordWithOrigin records turn t1 of the given origin.
func recordWithOrigin(t *testing.T, h *harness, origin conversationv1.PromptOrigin) {
	t.Helper()
	turn := wsm.Turn{ID: "t1", Workspace: theWorkspace, Origin: origin.String(), StartedAt: h.q.deps.Now()}
	if err := h.q.recordTurn(context.Background(), turn, h.log.Global()); err != nil {
		t.Fatalf("recordTurn: %v", err)
	}
}

func TestRecordTurnRecordsAMergeTurnAtTheStandingAddress(t *testing.T) {
	cases := []conversationv1.PromptOrigin{
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION,
	}
	for _, origin := range cases {
		t.Run(origin.String(), func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			addr := mergeTabAddress()
			h.feed.SetOutputAddress(theWorkspace, addr)

			// Act
			recordWithOrigin(t, h, origin)

			// Assert
			got, ok := h.db.startedTurn("t1")
			if !ok || got.Address == nil || *got.Address.Feed.Merge != *addr.Feed.Merge {
				t.Fatalf("turn = (%+v, %v), want it recorded at the merge's standing address", got, ok)
			}
		})
	}
}

func TestRecordTurnRecordsATurnNotStartedByTheMergeOnTheRootFeed(t *testing.T) {
	cases := []conversationv1.PromptOrigin{
		conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART,
	}
	for _, origin := range cases {
		t.Run(origin.String(), func(t *testing.T) {
			// Arrange: a merge stands an address while the turn opens.
			h := newHarness(t)
			h.feed.SetOutputAddress(theWorkspace, mergeTabAddress())

			// Act
			recordWithOrigin(t, h, origin)

			// Assert
			got, ok := h.db.startedTurn("t1")
			if !ok || got.Address != nil {
				t.Fatalf("turn = (%+v, %v), want it recorded with no address (the root feed)", got, ok)
			}
		})
	}
}

func TestRecordTurnRecordsAMergeTurnOnTheRootFeedWhenNoAddressStands(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	recordWithOrigin(t, h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR)

	// Assert
	got, ok := h.db.startedTurn("t1")
	if !ok || got.Address != nil {
		t.Fatalf("turn = (%+v, %v), want it recorded with no address (the root feed)", got, ok)
	}
	if !logged(h.log.Records(), "info", opDeliver, "a merge's own turn opened with no merge address standing; it draws on the root feed") {
		t.Fatalf("records = %+v, want the addressless merge turn said at info", h.log.Records())
	}
}

func TestRecordTurnHandsTheRecordedAddressToTheFeed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	addr := mergeTabAddress()
	h.feed.SetOutputAddress(theWorkspace, addr)

	// Act
	recordWithOrigin(t, h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)

	// Assert
	got, ok := h.feed.turnAddress("t1")
	if !ok || got == nil || *got.Feed.Merge != *addr.Feed.Merge {
		t.Fatalf("feed's turn address = (%+v, %v), want the recorded merge address", got, ok)
	}
}

func TestRecordTurnSurfacesAFailedWrite(t *testing.T) {
	// Arrange
	h := newHarness(t)
	boom := errors.New("disk full")
	h.db.putTurnErr = boom

	// Act
	err := h.q.recordTurn(context.Background(), wsm.Turn{ID: "t1", Workspace: theWorkspace, StartedAt: h.q.deps.Now()}, h.log.Global())

	// Assert
	if !errors.Is(err, boom) {
		t.Fatalf("recordTurn = %v, want the write's own error", err)
	}
}

func TestRecordTurnHandsNothingToTheFeedOnAFailedWrite(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.putTurnErr = errors.New("disk full")

	// Act
	_ = h.q.recordTurn(context.Background(), wsm.Turn{ID: "t1", Workspace: theWorkspace, StartedAt: h.q.deps.Now()}, h.log.Global())

	// Assert
	if _, ok := h.feed.turnAddress("t1"); ok {
		t.Fatalf("the feed was handed an address for a turn that was never recorded")
	}
}

// Every site that opens a turn records it through recordTurn: each case opens a
// turn its own way, and the feed must have been handed that turn's address.
func TestEveryOpenedTurnIsHandedToTheFeed(t *testing.T) {
	cases := []struct {
		name string
		turn ids.TurnID
		open func(t *testing.T, h *harness)
	}{
		{name: "a delivered prompt", turn: "t1", open: func(t *testing.T, h *harness) {
			if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
		}},
		{name: "a context cut", turn: "cut-1", open: func(t *testing.T, h *harness) {
			if err := h.q.SubmitSessionAct(context.Background(), theWorkspace,
				Act{Kind: ActClear, Turn: "cut-1", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}); err != nil {
				t.Fatalf("SubmitSessionAct: %v", err)
			}
		}},
		{name: "an adopted vendor turn", turn: "vendor-turn", open: func(t *testing.T, h *harness) {
			h.q.OnTurnAdopted(theWorkspace, "vendor-turn")
		}},
		{name: "a joining prompt", turn: "t1", open: func(t *testing.T, h *harness) {
			sentToJoin(t, h)
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			tc.open(t, h)

			// Assert
			if _, ok := h.feed.turnAddress(tc.turn); !ok {
				t.Fatalf("turn %s was opened without its address handed to the feed", tc.turn)
			}
		})
	}
}
