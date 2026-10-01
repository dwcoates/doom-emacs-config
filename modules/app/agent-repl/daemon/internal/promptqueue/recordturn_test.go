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
// with the output address standing as it opens, which is what a feed replay
// draws the turn at.

// mirroredTabAddress is a merge repair tab's mirrored address.
func mirroredTabAddress() *sessionwatcher.OutputAddress {
	lease := wsm.NewLeaseID()
	parent := feedid.Ref{WS: theWorkspace, Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindMergeHead, ID: string(lease)}}
	return &sessionwatcher.OutputAddress{Feed: feedid.Feed{Merge: &lease}, Parent: &parent, Mirror: true}
}

func TestRecordTurnStampsTheStandingOutputAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	addr := mirroredTabAddress()
	h.feed.SetOutputAddress(theWorkspace, addr)

	// Act
	if err := h.q.recordTurn(context.Background(), wsm.Turn{ID: "t1", Workspace: theWorkspace, StartedAt: h.q.deps.Now()}); err != nil {
		t.Fatalf("recordTurn: %v", err)
	}

	// Assert
	got, ok := h.db.startedTurn("t1")
	if !ok || got.Address == nil || *got.Address.Feed.Merge != *addr.Feed.Merge || !got.Address.Mirror {
		t.Fatalf("turn = (%+v, %v), want it recorded at the standing mirrored address", got, ok)
	}
}

func TestRecordTurnRecordsNoAddressWhenNoneStands(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.q.recordTurn(context.Background(), wsm.Turn{ID: "t1", Workspace: theWorkspace, StartedAt: h.q.deps.Now()}); err != nil {
		t.Fatalf("recordTurn: %v", err)
	}

	// Assert
	got, ok := h.db.startedTurn("t1")
	if !ok || got.Address != nil {
		t.Fatalf("turn = (%+v, %v), want it recorded with no address (the root feed)", got, ok)
	}
}

func TestRecordTurnSurfacesAFailedWrite(t *testing.T) {
	// Arrange
	h := newHarness(t)
	boom := errors.New("disk full")
	h.db.putTurnErr = boom

	// Act
	err := h.q.recordTurn(context.Background(), wsm.Turn{ID: "t1", Workspace: theWorkspace, StartedAt: h.q.deps.Now()})

	// Assert
	if !errors.Is(err, boom) {
		t.Fatalf("recordTurn = %v, want the write's own error", err)
	}
}

// Every site that opens a turn records it through recordTurn: each case opens a
// turn its own way while a mirrored address stands, and the record must carry
// it.
func TestEveryOpenedTurnIsRecordedAtTheStandingAddress(t *testing.T) {
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
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.feed.SetOutputAddress(theWorkspace, mirroredTabAddress())

			// Act
			tc.open(t, h)

			// Assert
			got, ok := h.db.startedTurn(tc.turn)
			if !ok || got.Address == nil || !got.Address.Mirror {
				t.Fatalf("turn = (%+v, %v), want it recorded at the standing mirrored address", got, ok)
			}
		})
	}
}

func TestAJoiningPromptsTurnIsRecordedAtTheStandingAddress(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.feed.SetOutputAddress(theWorkspace, mirroredTabAddress())

	// Act
	sentToJoin(t, h)

	// Assert
	got, ok := h.db.startedTurn("t1")
	if !ok || got.Address == nil || !got.Address.Mirror {
		t.Fatalf("turn = (%+v, %v), want the joining prompt's turn recorded at the standing address", got, ok)
	}
}
