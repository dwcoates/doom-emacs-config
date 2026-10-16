package integration

import (
	"context"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// CRITIQUE 6 — SIGTERM with a batch in flight.
//
// The sidecar holds no retry buffer and spills nothing, so a signal that arrives
// mid-ingest can only leave the store in one of two states for the batch that
// was in flight: the records AND the cursor advance committed together (they ride
// one transaction), or NEITHER did. What must never appear is a partial — records
// the cursor does not cover, or a cursor past records that were never stored.
//
// The two directions are separate edge cases and get a subject each, over the
// same observation: where the store's cursor for the file ended up, and which of
// the transcript's units its book holds.

// ---------------------------------------------------------------------------
// THE ACK IS THE LINE. The two subjects above read the real store's final
// state; these two read the PRODUCER's, on either side of the one event that
// decides whether its cursor may move — the store's answer.
//
// They run against the fake store's write gate, which stops the sidecar INSIDE
// the write for exactly as long as the subject needs, so neither of them races
// a poll interval or a signal: the batch is provably in flight when the SIGTERM
// lands, or provably answered before it does.
// ---------------------------------------------------------------------------

// cursorPast matches the first batch that offers a cursor for a path standing
// PAST an offset — the batch carrying bytes the producer has not been told are
// durable yet.
func cursorPast(path string, offset int64) func(*storev1.WriteBatchRequest) bool {
	return func(req *storev1.WriteBatchRequest) bool {
		cs := req.GetBatch().GetCursorAdvance()
		return cs != nil && samePath(cs.GetPath(), path) && cs.GetOffset() > offset
	}
}

// awaitAckedThrough blocks until the fake has ANSWERED a batch carrying a
// cursor for path at or past offset.
//
// IT IS WHAT MAKES THE GATE DETERMINISTIC. The reader splits a growing file
// into however many batches its poll clock happens to cut, so "one batch has
// arrived" says nothing about how much of the file is behind it: a gate armed
// on "past whatever is acked right now" can be beaten by a second batch of the
// SAME nine lines, and the pickup that batch earns then looks like a commit the
// gated write was never answered for. Waiting for the whole prefix to be
// durable first removes the ambiguity — after it, no batch past that offset can
// exist until this subject writes more bytes.
func awaitAckedThrough(ctx context.Context, t *testing.T, fake *fakeStore, path string, offset int64) {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		for _, b := range batchesCarryingCursorFor(fake.AckedBatches(), path) {
			if b.GetBatch().GetCursorAdvance().GetOffset() >= offset {
				return
			}
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the store never answered a batch carrying a cursor for %s at or past %d within the deadline", path, offset)
		case <-tick.C:
		}
	}
}

// lastCursorOffered answers the furthest cursor these batches offered for a
// path, and fails the subject if none of them named it at all.
func lastCursorOffered(t *testing.T, batches []*storev1.WriteBatchRequest, path, what string) int64 {
	t.Helper()
	carrying := batchesCarryingCursorFor(batches, path)
	if len(carrying) == 0 {
		t.Fatalf("no %s carried a cursor for %s, so there is no position to compare against", what, path)
	}
	return carrying[len(carrying)-1].GetBatch().GetCursorAdvance().GetOffset()
}

// pickupsPast lists every cursor commit the sidecar STATED for a path past an
// offset. A tail-pickup record is written only after Commit, which runs only on
// a durable answer, so it is the reader's own statement that its cursor moved.
func pickupsPast(t *testing.T, logPath, path string, offset int64) []logRecord {
	t.Helper()
	var out []logRecord
	for _, r := range readLog(t, logPath) {
		if r.Operation != "tail-pickup" || !samePathAny(r.Context["path"], path) {
			continue
		}
		if at, ok := r.Context["offset"].(float64); ok && int64(at) > offset {
			out = append(out, r)
		}
	}
	return out
}

// TestSigtermWithAnUnackedBatchLeavesTheCursorAtTheLastAck is the edge case
// where the signal lands with a batch on the wire and no answer to it: the
// producer's cursor must stand exactly where its last ACK left it, whatever the
// store later does with the bytes it was sent.
func TestSigtermWithAnUnackedBatchLeavesTheCursorAtTheLastAck(t *testing.T) {
	t.Parallel()
	// Arrange: one batch answered, the next one withheld forever.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	proc := startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:9] {
		g.AppendLine(line)
	}
	acked := g.Offset()
	awaitAckedThrough(ctx, t, fake, g.Path(), acked)
	gate := fake.gateOnBatch(t, cursorPast(g.Path(), acked))
	for _, line := range captured.Lines[9:] {
		g.AppendLine(line)
	}
	gate.await(ctx, t, "the batch carrying the rest of the transcript")

	// Act: the signal lands with that batch still unanswered.
	signalAndTime(t, proc, wedgedShutdownBudget)

	// Assert: the reader never claimed the position it was never told was
	// durable, and never offered one past it either.
	if moved := pickupsPast(t, proc.LogPath, g.Path(), acked); len(moved) != 0 {
		t.Errorf("the reader stated %d cursor commit(s) past its last ack at %d, though the batch that would have earned them was never answered: %v",
			len(moved), acked, moved[0].Message)
	}
	// AND SOMETHING REALLY WAS IN FLIGHT, so the check above is not vacuous:
	// the withheld batch offered a position past the last ack, and the reader
	// still did not take it.
	if offered := lastCursorOffered(t, fake.Batches(), g.Path(), "offered batch"); offered <= acked {
		t.Fatalf("the withheld batch offered cursor %d, which is not past the last ack at %d, so nothing was in flight", offered, acked)
	}
}

// TestAnAckBeforeSigtermAdvancesTheCursor is the other side of the same line:
// the store answers, and only then does the signal land — so the advance the
// answer earned is committed rather than thrown away with the shutdown.
func TestAnAckBeforeSigtermAdvancesTheCursor(t *testing.T) {
	t.Parallel()
	// Arrange: one batch answered, the next one held until this subject lets it
	// through.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	proc := startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:9] {
		g.AppendLine(line)
	}
	acked := g.Offset()
	awaitAckedThrough(ctx, t, fake, g.Path(), acked)
	gate := fake.gateOnBatch(t, cursorPast(g.Path(), acked))
	for _, line := range captured.Lines[9:] {
		g.AppendLine(line)
	}
	gate.await(ctx, t, "the batch carrying the rest of the transcript")
	held := lastCursorOffered(t, fake.Batches(), g.Path(), "offered batch")

	// Act: the answer arrives, THEN the signal.
	gate.release()
	got := awaitLog(ctx, t, proc.LogPath, "the cursor commit the answered batch earned", func(r logRecord) bool {
		if r.Operation != "tail-pickup" || !samePathAny(r.Context["path"], g.Path()) {
			return false
		}
		at, ok := r.Context["offset"].(float64)
		return ok && int64(at) >= held
	})
	signalAndTime(t, proc, wedgedShutdownBudget)

	// Assert.
	if at, _ := got.Context["offset"].(float64); int64(at) < held {
		t.Fatalf("the reader committed its cursor at %d, short of the %d the answered batch made durable", int64(at), held)
	}
	if got.Level != "" && got.Level != "info" {
		t.Fatalf("the pickup record is level %q, want info: a durable batch is ordinary progress", got.Level)
	}
}
