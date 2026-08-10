package sessioncontroller

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/frontend"
)

// THE INSTRUCTION IS INVISIBLE ON THE PATH IT ACTUALLY ARRIVES BY.
//
// A re-drive's user message reaches the daemon as a FILE-PLANE transcript line
// carrying no request id at all — the daemon's correlation of a line to a
// submit comes from its outstanding receipts, and a re-drive mints none. That
// is how the daemon's internal instruction came to be rendered, live and on
// every replay, and it is what the marker on the submitted text closes.
//
// These tests drive a real transcript line through the consumer, which is the
// same path a live push, a backfill, a resync and a store re-pull all take.

// markedTranscriptEvent is the durable transcript line of one re-drive, as the
// file plane delivers it: the marked text, and no request id anywhere.
func markedTranscriptEvent(t *testing.T, seq uint64, uuid, requestID string) *corev1.Event {
	t.Helper()
	return transcriptUserEvent(t, seq, uuid,
		frontend.MarkInternalResumeInstruction(requestID, resumptionInstruction))
}

func TestAReDrivesTranscriptLineIsNeverPushed(t *testing.T) {
	// Arrange.
	receipts := newFakeReceiptStore()
	cons := receiptConsumer(t, receipts)

	// Act.
	cons.Consume(markedTranscriptEvent(t, 12, "u-redrive", reDriveRequest))

	// Assert.
	if got := pushedUserBubbles(cons); got != 0 {
		t.Fatalf("pushed %d user bubble(s), want none for the daemon's own instruction", got)
	}
}

func TestAReDrivesTranscriptLineDischargesItsResumption(t *testing.T) {
	// Arrange — the line IS the confirmed delivery, so seeing it is what
	// retires the owed turn.
	receipts := newFakeReceiptStore()
	if err := receipts.RecordPendingResumption(owedFor(reDriveRequest)); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}
	if _, err := receipts.ClaimResumptionForDelivery(reDriveRequest, 3_000); err != nil {
		t.Fatalf("ClaimResumptionForDelivery: %v", err)
	}
	cons := receiptConsumer(t, receipts)

	// Act.
	cons.Consume(markedTranscriptEvent(t, 12, "u-redrive", reDriveRequest))

	// Assert.
	owed, err := receipts.UndischargedResumptions("ws")
	if err != nil {
		t.Fatalf("UndischargedResumptions: %v", err)
	}
	if len(owed) != 0 {
		t.Fatalf("undischarged = %+v, want the delivered instruction to clear the record", owed)
	}
}

func TestAUsersOwnTranscriptLineIsStillPushed(t *testing.T) {
	// Arrange — the suppression must reach the daemon's instruction and
	// nothing else, or a prompt the user typed would vanish.
	receipts := newFakeReceiptStore()
	cons := receiptConsumer(t, receipts)

	// Act.
	cons.Consume(transcriptUserEvent(t, 12, "u-user", "what is the status"))

	// Assert.
	if pushed := pushedUserBubbles(cons); pushed != 1 {
		t.Fatalf("pushed %d user bubble(s), want the user's own prompt delivered", pushed)
	}
}

// pushedUserBubbles counts the user bubbles a consumer's pusher has been handed.
func pushedUserBubbles(cons *consumer) int {
	p := cons.push.(*fakePusher)
	p.mu.Lock()
	defer p.mu.Unlock()
	n := 0
	for _, cd := range p.convo {
		for _, it := range cd.GetItems() {
			if it.GetUserMessage() != nil {
				n++
			}
		}
	}
	return n
}
