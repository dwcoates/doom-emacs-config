package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — a TRANSCRIPT that shrinks under the reader.
//
// The spool half of this is covered (TestATruncatedSpoolIsReReadFromItsNewStart)
// and the two files are read by DIFFERENT codecs: a spool is raw bytes and a
// transcript is JSONL, so a reset that works for one proves nothing about the
// other. A transcript that reset in memory but never re-offered the lower cursor
// would leave the store believing it had read records that no longer exist, and
// a restart would resume past the whole of the rewritten conversation.

// truncatedTranscriptRecord is the transcript's SECOND life: one small assistant
// text block, shorter than what it replaces, whose unit exists nowhere in the
// first life — so the unit landing can only be a re-read of the new bytes.
const truncatedTranscriptRecord = `{"type":"assistant","uuid":"t-1","isSidechain":false,` +
	`"timestamp":"2026-07-21T22:00:00.000Z","message":{"id":"msg_truncated_life","role":"assistant",` +
	`"content":[{"type":"text","text":"the second life"}]}}`

// TestATruncatedTranscriptIsReReadFromItsNewStart replaces an already-read
// transcript's contents in place with something shorter and asserts the durable
// cursor comes back to the new length, with the new record actually stored.
func TestATruncatedTranscriptIsReReadFromItsNewStart(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	session := "7c7c7c7c-7c7c-47c7-87c7-7c7c7c7c7c7c"
	slug := cwdSlug("/work/transcript-truncation-probe")
	path := tree.sessionPath(slug, session)
	after := truncatedTranscriptRecord + "\n"

	// Act: read a first life several records long...
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, path)
	for _, i := range []int{8, 9, 10} {
		g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[i]),
			session, "/work/transcript-truncation-probe")))
	}
	awaitCursorInBatches(ctx, t, fake, path, g.Offset())

	// ...then replace its contents IN PLACE with something shorter. The inode is
	// unchanged, so only the size drop can explain a re-read: truncation, not
	// rotation.
	if g.Offset() <= int64(len(after)) {
		t.Fatalf("the first life is %d bytes and the second %d; the second must be SHORTER or this is not truncation",
			g.Offset(), len(after))
	}
	truncateInPlace(t, path, after)

	// Assert: the durable cursor comes back DOWN — the shrink was noticed —
	// and then SETTLES at exactly the new file's whole length. Both waits are
	// needed: O_TRUNC and the write that follows it are two syscalls, so a poll
	// can land between them and commit a correct cursor of 0 for a genuinely
	// empty file, and stopping at the first position at-or-below the new length
	// would read that intermediate state as the end state.
	awaitCursorAtMost(ctx, t, fake, path, int64(len(after)))
	cs := awaitCursorSettledAt(ctx, t, fake, path, int64(len(after)))
	if got := cs.GetOffset(); got != int64(len(after)) {
		t.Errorf("the cursor settled at %d after truncation, wanted the new file's whole length %d", got, int64(len(after)))
	}
	// ...and the rewritten record reached the store, so the reset was a RE-READ
	// rather than the reader simply forgetting the file.
	fake.awaitEntry(ctx, t, "the truncated transcript's new unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == "activity:msg_truncated_life:0"
	})
}
