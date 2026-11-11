package feed

import (
	"context"
	"reflect"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/viewquiet"
)

// quietGate puts the harness's resolver behind a quiet gate, followed as the
// constructor follows Deps.Quiet.
func (h *harness) quietGate() *viewquiet.Gate {
	gate := viewquiet.New(nil)
	h.resolver.deps.Quiet = gate
	gate.Follow(QuietViewName, h.resolver.followQuiet)
	return gate
}

func TestAQuietFeedHoldsItsRowsAndReleasesEachOnceAtItsCurrentState(t *testing.T) {
	// Arrange
	h := newHarness(t)
	gate := h.quietGate()
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tailRows(ctx, tail)
	gate.Set(true)
	h.deliverPrompt("turn-1", "while the lid is closed")
	h.deliverPrompt("turn-2", "also while closed")

	// Act
	gate.Set(false)
	h.deliverPrompt("turn-3", "after the lid opened")

	// Assert: the held rows come once each, then the live one, nothing else.
	var got []string
	for range 3 {
		got = append(got, (<-rows).GetId().GetValue())
	}
	want := []string{h.promptRowID("turn-1"), h.promptRowID("turn-2"), h.promptRowID("turn-3")}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("tail rows = %q, want %q", got, want)
	}
}

func TestAQuietFeedStillFilesItsLog(t *testing.T) {
	// Arrange
	h := newHarness(t)
	gate := h.quietGate()
	_, token := h.openPage(rootFeed(), "reader-1")
	gate.Set(true)
	h.deliverPrompt("turn-1", "while the lid is closed")

	// Act: a tail opened from the earlier pin replays the logged row.
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	gate.Set(false)
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}

	// Assert
	if row := <-tailRows(ctx, tail); row.GetId().GetValue() != h.promptRowID("turn-1") {
		t.Fatalf("replayed row = %q, want turn-1's", row.GetId().GetValue())
	}
}

func TestHoldQuietKeepsEachRowOnceAtItsLatestAndEveryHeldChange(t *testing.T) {
	// Arrange
	f := &feedState{}
	row := func(id, text string) Publication {
		return Publication{Row: &frontendv1.FeedRow{Id: &frontendv1.FeedId{Value: id}, Order: &frontendv1.FeedRowOrder{Key: text}}}
	}
	held := Publication{Held: &agentreplv1.WatchFeedHeldChange{}}

	// Act
	holdQuiet(f, row("a", "first"))
	holdQuiet(f, row("b", "only"))
	holdQuiet(f, held)
	holdQuiet(f, row("a", "latest"))
	holdQuiet(f, held)

	// Assert
	var got []string
	for _, pub := range f.quietHeld {
		switch {
		case pub == nil:
		case pub.Row != nil:
			got = append(got, pub.Row.GetId().GetValue()+":"+pub.Row.GetOrder().GetKey())
		default:
			got = append(got, "held")
		}
	}
	want := []string{"b:only", "held", "a:latest", "held"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("held = %q, want %q", got, want)
	}
}
