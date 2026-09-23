package integration

import (
	"context"
	"os"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/testclose"
)

// SUBJECT — a file that SHRINKS under the reader.
//
// internal/tail's TestTailerTruncationResets states the rule on the tailer
// directly: a file whose size falls below the committed offset has been
// truncated, so the position is reset to zero and the new content is read from
// the top. That is the unit half. The BLACK-BOX half is what a store holding
// the reader's cursor sees, and it is the half that matters — a reader that
// reset in memory but never re-offered the lower cursor would look identical to
// the unit test and still leave the store believing it had read bytes that no
// longer exist, so a restart would resume past the whole of the new file.
//
// THE SIGNAL IS THE CURSOR ITSELF, never a wait: the durable position dropping
// to exactly the new file's length is the observable event, and it can only
// happen after the reader has seen the shorter file and re-read it whole.

// TestATruncatedSpoolIsReReadFromItsNewStart truncates a shell spool that has
// already been read, replaces its contents, and asserts the durable cursor
// resets to the new length rather than staying past the file's end.
func TestATruncatedSpoolIsReReadFromItsNewStart(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/spool-truncation-probe",
		"1d1d1d1d-1d1d-41d1-81d1-1d1d1d1d1d1d")
	before := "a long first life of this spool, several lines worth of it\nand a second line too\n"
	after := "a shorter second life\n"

	// Act: read the whole first life...
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, fx.Parent.Path(), fx.Parent.Offset())
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte(before))
	awaitCursorInBatches(ctx, t, fake, fx.SpoolPath, int64(len(before)))

	// ...then replace the file's contents IN PLACE with something shorter. The
	// inode does not change, so nothing but the size drop can explain a re-read:
	// this is truncation, not rotation.
	truncateInPlace(t, fx.SpoolPath, after)

	// Assert: the durable cursor comes back DOWN — the shrink was noticed...
	awaitCursorAtMost(ctx, t, fake, fx.SpoolPath, int64(len(after)))
	// ...and then SETTLES at exactly the new file's whole length.
	//
	// THE TWO WAITS ARE ONE STATEMENT AND NEITHER IS REDUNDANT. O_TRUNC and the
	// write that follows it are two syscalls, so a poll can legitimately land
	// between them and commit a cursor of 0 for a genuinely zero-byte file. That
	// is a correct reset, and stopping at the first cursor at-or-below the new
	// length would read it as the end state and assert against 0. The re-read is
	// its own event, and the exact offset is the signal for it.
	cs := awaitCursorSettledAt(ctx, t, fake, fx.SpoolPath, int64(len(after)))
	if got := cs.GetOffset(); got != int64(len(after)) {
		t.Errorf("the cursor settled at %d after truncation, wanted the new file's whole length %d", got, int64(len(after)))
	}
	// And the new content actually reached the store, so the reset was a
	// RE-READ rather than the reader simply forgetting the file.
	fake.awaitEntry(ctx, t, "a delta carrying the truncated spool's new content", func(e *storev1.StoreEntry) bool {
		return e.GetAgentUpdate().GetBash().GetFrame().GetUpdate().GetNewOutput() == after
	})
}

// truncateInPlace replaces a file's contents without changing its inode — the
// shape a harness reusing a spool produces, and the one the tailer distinguishes
// from a rotation by the size falling below its committed offset.
func truncateInPlace(t *testing.T, path, content string) {
	t.Helper()
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		t.Fatalf("truncate %s: %v", path, err)
	}
	defer testclose.OrFail(t, f)
	if _, err := f.WriteString(content); err != nil {
		t.Fatalf("write %s after truncation: %v", path, err)
	}
	if err := f.Sync(); err != nil {
		t.Fatalf("fsync %s after truncation: %v", path, err)
	}
}

// awaitCursorAtMost waits until the sidecar's DURABLE cursor for a path stands
// at or below an offset — the signal that a shrink was NOTICED, which no wait
// on growth can express.
func awaitCursorAtMost(ctx context.Context, t *testing.T, f *fakeStore, path string, offset int64) *storev1.CursorState {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		// The LAST cursor offered for the path, not the highest: after a
		// truncation the newest position is deliberately lower than the old one,
		// and latestCursorFor answers the maximum.
		if cs := lastCursorOfferedFor(f.AckedBatches(), path); cs != nil && cs.GetOffset() <= offset {
			return cs
		}
		select {
		case <-ctx.Done():
			cs := lastCursorOfferedFor(f.AckedBatches(), path)
			t.Fatalf("the cursor for %s never came back to %d or below (last: %v) within the deadline", path, offset, cs)
			return nil
		case <-tick.C:
		}
	}
}

// lastCursorOfferedFor answers the cursor a producer offered for a path MOST
// RECENTLY, in write order. It is the counterpart of latestCursorFor, which
// answers the highest — a distinction that only matters when a position is
// allowed to move backward, which truncation is the one case of.
func lastCursorOfferedFor(batches []*storev1.WriteBatchRequest, path string) *storev1.CursorState {
	var out *storev1.CursorState
	for _, b := range batches {
		if cs := b.GetBatch().GetCursorAdvance(); cs != nil && samePath(cs.GetPath(), path) {
			out = cs
		}
	}
	return out
}
