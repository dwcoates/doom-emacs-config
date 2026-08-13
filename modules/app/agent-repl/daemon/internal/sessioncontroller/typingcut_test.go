package sessioncontroller

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
)

// ---------------------------------------------------------------------------
// THE CUT (typingcut.go).
//
// A preview is retired by the AUTHORITATIVE RECORD of the block it previews.
// When the query dies mid-block that record can never arrive, so nothing
// retires it and the work spins "streaming input…" for the life of the page
// with no body. These pin who is cut, when, and — just as importantly — when
// nothing is.
// ---------------------------------------------------------------------------

// cutConsumer is a consumer wired to a pusher the test can read the cuts off.
func cutConsumer() (*consumer, *fakePusher) {
	push := &fakePusher{}
	c := newConsumer("ws", "s", push, &fakeApplier{}, nil,
		newFakeClearCompactStore(), emptyTurnAccountingStore{},
		func(string, ...any) {}, nil, nil, nil, nil, nil)
	return c, push
}

func cutMessageIDs(push *fakePusher) []string {
	push.mu.Lock()
	defer push.mu.Unlock()
	out := make([]string, 0, len(push.typingCuts))
	for _, c := range push.typingCuts {
		out = append(out, c.GetParentMessageId())
	}
	return out
}

func TestCuttingRetiresTheTopLevelPreview(t *testing.T) {
	// Arrange: a preview opened on the top-level feed.
	c, push := cutConsumer()
	c.notePreviewOpened("")

	// Act.
	c.cutOpenPreviews("test")

	// Assert: addressed exactly as the delta that opened it.
	if got := cutMessageIDs(push); len(got) != 1 || got[0] != "" {
		t.Fatalf("cut work ids = %q, want one cut addressed to the top-level feed", got)
	}
}

func TestCuttingRetiresADetachedWorkScopedPreview(t *testing.T) {
	// Arrange: a preview folded into an detached work, which is the case that
	// could never be retired from the feed at all.
	c, push := cutConsumer()
	c.notePreviewOpened("work-1")

	// Act.
	c.cutOpenPreviews("test")

	// Assert.
	if got := cutMessageIDs(push); len(got) != 1 || got[0] != "work-1" {
		t.Fatalf("cut work ids = %q, want one cut addressed to work-1", got)
	}
}

func TestCuttingRetiresEverySurfaceAPreviewWasOpenedOn(t *testing.T) {
	// Arrange: a session previewing on the feed and inside two work.
	c, push := cutConsumer()
	c.notePreviewOpened("")
	c.notePreviewOpened("work-b")
	c.notePreviewOpened("work-a")

	// Act.
	c.cutOpenPreviews("test")

	// Assert: all three, feed first, then sorted — one teardown's records read
	// the same way twice.
	want := []string{"", "work-a", "work-b"}
	got := cutMessageIDs(push)
	if len(got) != len(want) {
		t.Fatalf("cut work ids = %q, want %q", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("cut work ids = %q, want %q", got, want)
		}
	}
}

func TestCuttingASessionThatNeverPreviewedEmitsNothing(t *testing.T) {
	// Arrange: a session that never relayed a typing delta.
	c, push := cutConsumer()

	// Act.
	c.cutOpenPreviews("test")

	// Assert: a teardown must not push a cut for a preview that never existed.
	if got := cutMessageIDs(push); len(got) != 0 {
		t.Fatalf("cut work ids = %q, want none", got)
	}
}

// THE DRAIN IS WHAT MAKES A SECOND TEARDOWN SILENT. A teardown path that runs
// twice must not emit a second round of cuts.
func TestASecondCutEmitsNothing(t *testing.T) {
	// Arrange.
	c, push := cutConsumer()
	c.notePreviewOpened("")
	c.cutOpenPreviews("first")

	// Act.
	c.cutOpenPreviews("second")

	// Assert: exactly the one round.
	if got := cutMessageIDs(push); len(got) != 1 {
		t.Fatalf("cut work ids = %q, want only the first teardown's single cut", got)
	}
}

// THE PRODUCER EDGE. A query torn down mid-block is the case the whole
// mechanism exists for, and it is where the cut is emitted from.
func TestAnUnexpectedQueryTerminationCutsTheOpenPreview(t *testing.T) {
	// Arrange: a preview standing when the query dies.
	c, push := cutConsumer()
	c.notePreviewOpened("work-1")
	item := &frontendv1.FailureCardView{
		Kind: &frontendv1.FailureKind{
			Kind: &frontendv1.FailureKind_QueryTermination{
				QueryTermination: &frontendv1.FailureQueryTermination{
					Detail: &frontendv1.QueryTerminationFailure{QueryInstanceId: "q1"},
				},
			},
		},
	}

	// Act: the LIVE arm.
	c.surfaceUnexpectedQueryTermination(&protocolv1.Event{}, item, false)

	// Assert: the preview is retired rather than left spinning beside the
	// failure card that explains the session.
	if got := cutMessageIDs(push); len(got) != 1 || got[0] != "work-1" {
		t.Fatalf("cut work ids = %q, want the open preview retired", got)
	}
}

// AND A REPLAYED ONE CUTS NOTHING. A durable termination row being replayed is
// history; cutting on it would retire previews belonging to the session running
// now, on the strength of a query that died long ago.
func TestAReplayedQueryTerminationCutsNothing(t *testing.T) {
	// Arrange.
	c, push := cutConsumer()
	c.notePreviewOpened("work-1")
	item := &frontendv1.FailureCardView{
		Kind: &frontendv1.FailureKind{
			Kind: &frontendv1.FailureKind_QueryTermination{
				QueryTermination: &frontendv1.FailureQueryTermination{
					Detail: &frontendv1.QueryTerminationFailure{QueryInstanceId: "q1"},
				},
			},
		},
	}

	// Act: the HISTORICAL arm.
	c.surfaceUnexpectedQueryTermination(&protocolv1.Event{}, item, true)

	// Assert.
	if got := cutMessageIDs(push); len(got) != 0 {
		t.Fatalf("cut work ids = %q, want none from a replayed termination", got)
	}
}
