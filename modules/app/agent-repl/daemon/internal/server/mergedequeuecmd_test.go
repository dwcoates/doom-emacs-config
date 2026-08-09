package server

import (
	"context"
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/merge"
)

// answeringHandler is the arrangement every case below shares: a workspace with
// a merge on the queue and one outstanding offer, raised the way an interrupt
// raises it.
func answeringHandler(t *testing.T) (*commandHandler, *fakeMerges, *fakeDequeueOffers) {
	t.Helper()
	merges := &fakeMerges{
		standing:       merge.Standing{Repo: "/repo/.git", RunID: "run-7", Position: 2, Depth: 3},
		standingQueued: true,
		dequeueCount:   1,
	}
	offers := &fakeDequeueOffers{}
	h, _, _, _ := newGatedHandlerWithOffers(t, true, fakeLiveTasks{}, merges, offers)
	if err := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{}); err != nil {
		t.Fatalf("arrange: Interrupt: %v", err)
	}
	if !offers.outstandingOK {
		t.Fatal("arrange: want an outstanding offer after the interrupt")
	}
	return h, merges, offers
}

func dequeueAnswer(offerID string) *frontendv1.AnswerMergeDequeueCmd {
	return &frontendv1.AnswerMergeDequeueCmd{
		OfferId: offerID,
		Answer:  &frontendv1.AnswerMergeDequeueCmd_Dequeue{Dequeue: &frontendv1.MergeDequeueConfirm{}},
	}
}

func keepAnswer(offerID string) *frontendv1.AnswerMergeDequeueCmd {
	return &frontendv1.AnswerMergeDequeueCmd{
		OfferId: offerID,
		Answer:  &frontendv1.AnswerMergeDequeueCmd_Keep{Keep: &frontendv1.MergeDequeueDecline{}},
	}
}

// A `dequeue` answer takes the card down AND takes the merge off the queue.
func TestADequeueAnswerClearsTheOfferAndDequeues(t *testing.T) {
	// Arrange.
	h, merges, offers := answeringHandler(t)

	// Act.
	err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r2", dequeueAnswer(offers.outstanding))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerMergeDequeue() error = %v, want nil", err)
	}
	if len(offers.cleared) != 1 || offers.cleared[0] != "answered:dequeue" {
		t.Fatalf("cleared = %v, want one answered:dequeue", offers.cleared)
	}
	if len(merges.dequeued) != 1 || merges.dequeued[0] != "/ws1" {
		t.Fatalf("dequeued = %v, want [/ws1]", merges.dequeued)
	}
}

// A `keep` answer is a REAL answer: the card comes down and the merge stays.
// Without the clear the question would stand forever, since there is no other
// dismissal channel.
func TestAKeepAnswerClearsTheOfferAndLeavesTheMerge(t *testing.T) {
	// Arrange.
	h, merges, offers := answeringHandler(t)

	// Act.
	err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r2", keepAnswer(offers.outstanding))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerMergeDequeue() error = %v, want nil", err)
	}
	if len(offers.cleared) != 1 || offers.cleared[0] != "answered:keep" {
		t.Fatalf("cleared = %v, want one answered:keep", offers.cleared)
	}
	if len(merges.dequeued) != 0 {
		t.Fatalf("dequeued = %v, want the merge left alone on a keep", merges.dequeued)
	}
}

// A STALE offer id is refused rather than resolved to whatever question is
// outstanding: a click on a superseded card must not dequeue the merge its
// replacement is asking about.
func TestAStaleOfferIDIsRefused(t *testing.T) {
	// Arrange.
	h, merges, offers := answeringHandler(t)

	// Act.
	err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r2", dequeueAnswer("offer-from-a-card-that-is-gone"))

	// Assert.
	if err == nil {
		t.Fatal("want a loud refusal for an offer id that is not the outstanding one")
	}
	if len(merges.dequeued) != 0 {
		t.Fatalf("dequeued = %v, want nothing dequeued on a stale answer", merges.dequeued)
	}
	if len(offers.cleared) != 0 {
		t.Fatalf("cleared = %v, want the outstanding question left standing", offers.cleared)
	}
	if !offers.outstandingOK {
		t.Fatal("want the outstanding offer still outstanding after a stale answer")
	}
}

// An answer arriving when NO question is outstanding is refused. It is the
// double-click case: the first answer cleared the offer, and the second must
// not dequeue a second time.
func TestAnAnswerWithNoOutstandingOfferIsRefused(t *testing.T) {
	// Arrange.
	h, merges, offers := answeringHandler(t)
	answered := offers.outstanding
	if err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r2", keepAnswer(answered)); err != nil {
		t.Fatalf("arrange: first answer: %v", err)
	}

	// Act.
	err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r3", dequeueAnswer(answered))

	// Assert.
	if err == nil {
		t.Fatal("want a loud refusal when no offer is outstanding")
	}
	if len(merges.dequeued) != 0 {
		t.Fatalf("dequeued = %v, want nothing dequeued by a second answer", merges.dequeued)
	}
}

// THE MALFORMED COMMANDS, one per row. Each is refused loudly and none of them
// dequeues: a command that decided nothing must never be resolved into either
// decision.
func TestAMalformedAnswerIsRefused(t *testing.T) {
	tests := []struct {
		name string
		cmd  func(outstanding string) *frontendv1.AnswerMergeDequeueCmd
	}{
		{
			name: "no offer id",
			cmd:  func(string) *frontendv1.AnswerMergeDequeueCmd { return dequeueAnswer("") },
		},
		{
			name: "no answer arm",
			cmd: func(outstanding string) *frontendv1.AnswerMergeDequeueCmd {
				return &frontendv1.AnswerMergeDequeueCmd{OfferId: outstanding}
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h, merges, offers := answeringHandler(t)

			// Act.
			err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r2", tc.cmd(offers.outstanding))

			// Assert.
			if err == nil {
				t.Fatal("want a loud refusal")
			}
			if len(merges.dequeued) != 0 {
				t.Fatalf("dequeued = %v, want nothing dequeued", merges.dequeued)
			}
			if len(offers.cleared) != 0 {
				t.Fatalf("cleared = %v, want the question left standing", offers.cleared)
			}
		})
	}
}

// An UNWIRED offer store refuses the answer rather than dequeuing on a question
// it cannot check: without the store there is no id to compare against, and
// dequeuing anyway would take a merge off the queue on an unverified click.
func TestAnAnswerWithNoOfferStoreIsRefused(t *testing.T) {
	// Arrange.
	merges := &fakeMerges{}
	h, _, _, _ := newGatedHandlerWithOffers(t, true, fakeLiveTasks{}, merges, nil)

	// Act.
	err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r2", dequeueAnswer("offer-1"))

	// Assert.
	if err == nil {
		t.Fatal("want a loud refusal when the dequeue offer store is unwired")
	}
	if len(merges.dequeued) != 0 {
		t.Fatalf("dequeued = %v, want nothing dequeued", merges.dequeued)
	}
}

// A CLEAR that failed stops the answer before anything is dequeued. The card is
// the record that the question is answered, and dequeuing behind a clear that
// did not land would take the merge off while the card still invites a click.
func TestAClearFailureRefusesBeforeDequeuing(t *testing.T) {
	// Arrange.
	h, merges, offers := answeringHandler(t)
	clearErr := errors.New("push current state: subscriber gone")
	offers.clearErr = clearErr

	// Act.
	err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r2", dequeueAnswer(offers.outstanding))

	// Assert.
	if err == nil || !errors.Is(err, clearErr) {
		t.Fatalf("AnswerMergeDequeue() error = %v, want the clear's own error", err)
	}
	if len(merges.dequeued) != 0 {
		t.Fatalf("dequeued = %v, want nothing dequeued behind a failed clear", merges.dequeued)
	}
}

// A DEQUEUE that failed surfaces on the ack. The card is already down — the
// question WAS answered — so the honest report is that the merge the user asked
// to remove is still there, not silence.
func TestADequeueFailureSurfacesOnTheAck(t *testing.T) {
	// Arrange.
	h, merges, offers := answeringHandler(t)
	dequeueErr := errors.New("abort run: stopped waiting for the run to unwind")
	merges.dequeueErr = dequeueErr

	// Act.
	err := h.AnswerMergeDequeue(context.Background(), "/ws1", "r2", dequeueAnswer(offers.outstanding))

	// Assert.
	if err == nil || !errors.Is(err, dequeueErr) {
		t.Fatalf("AnswerMergeDequeue() error = %v, want the dequeue's own error", err)
	}
	if len(offers.cleared) != 1 {
		t.Fatalf("cleared = %v, want the answered question still taken down", offers.cleared)
	}
}

// An answer with no workspace key is refused by the same check every other
// workspace-scoped command uses: an unkeyed answer names no queue.
func TestAnAnswerWithNoWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	h, _, offers := answeringHandler(t)

	// Act.
	err := h.AnswerMergeDequeue(context.Background(), "", "r2", dequeueAnswer(offers.outstanding))

	// Assert.
	if err == nil {
		t.Fatal("want a loud refusal for an answer with no workspace")
	}
}
