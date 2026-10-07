package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — TWO transcripts tailed at once.
//
// The sidecar holds one tailer per discovered file, each with its own cursor and
// its own converter state. Nothing about that is visible while only one file is
// growing, so the interleaving is the subject: two sessions written turn by turn
// must produce two books that share no row, and two cursors that name two
// different files by their OWN file_id.

// interleavedFile is one of the two transcripts the subject grows.
type interleavedFile struct {
	Session  string
	File     *growingFile
	Thinking string // the unit id of its first block
	Call     string // the unit id of its tool call
}

// seedInterleavedTranscript writes one session's opening response with unit ids
// that are unique to it, so a row landing in the wrong book is visible by name
// rather than by counting.
func seedInterleavedTranscript(t *testing.T, tree *vendorTree, cwd, session, tag string) (interleavedFile, []string) {
	t.Helper()
	captured := loadCapturedSession(t)
	messageID := "msg_interleave_" + tag
	callID := "toolu_interleave_" + tag

	thinking := retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)
	thinking = setMessageID(t, thinking, messageID)
	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	call = setMessageID(t, call, messageID)
	call = setToolUseID(t, call, callID)

	return interleavedFile{
		Session:  session,
		File:     newGrowingFile(t, tree.sessionPath(cwdSlug(cwd), session)),
		Thinking: messageID + ":0",
		Call:     callID,
	}, []string{encodeRecord(t, thinking), encodeRecord(t, call)}
}

// TestTwoTranscriptsTailedAtOnceKeepTheirRowsApart grows two sessions turn by
// turn and asserts each book holds only its own units.
func TestTwoTranscriptsTailedAtOnceKeepTheirRowsApart(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	a, aLines := seedInterleavedTranscript(t, tree, "/work/interleave-a", "2a2a2a2a-2a2a-42a2-82a2-2a2a2a2a2a2a", "a")
	b, bLines := seedInterleavedTranscript(t, tree, "/work/interleave-b", "2b2b2b2b-2b2b-42b2-82b2-2b2b2b2b2b2b", "b")

	// Act: strictly interleaved, so neither file is ever read to its end alone.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	for i := range aLines {
		a.File.AppendLine(aLines[i])
		b.File.AppendLine(bLines[i])
	}
	awaitCursorInBatches(ctx, t, fake, a.File.Path(), a.File.Offset())
	awaitCursorInBatches(ctx, t, fake, b.File.Path(), b.File.Offset())

	// Assert.
	entries := fake.Entries()
	for _, tc := range []struct {
		book  interleavedFile
		other interleavedFile
	}{{a, b}, {b, a}} {
		own := map[string]bool{tc.book.Thinking: true, tc.book.Call: true}
		foreign := map[string]bool{tc.other.Thinking: true, tc.other.Call: true}
		var seen int
		for _, line := range linesForBook(entries, tc.book.Session) {
			id := activityOf(line).GetActivityId().GetValue()
			if id == "" {
				continue
			}
			if foreign[id] {
				t.Errorf("book %q holds unit %q, which belongs to the other transcript", tc.book.Session, id)
			}
			if own[id] {
				seen++
			}
		}
		if seen == 0 {
			t.Errorf("book %q received none of its own units", tc.book.Session)
		}
	}
}

// TestTwoTranscriptsAdvanceTheirOwnCursorsByFileId asserts the position half:
// each file's cursor names that file, by its own dev:inode identity.
func TestTwoTranscriptsAdvanceTheirOwnCursorsByFileId(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	a, aLines := seedInterleavedTranscript(t, tree, "/work/interleave-cursor-a", "2c2c2c2c-2c2c-42c2-82c2-2c2c2c2c2c2c", "ca")
	b, bLines := seedInterleavedTranscript(t, tree, "/work/interleave-cursor-b", "2d2d2d2d-2d2d-42d2-82d2-2d2d2d2d2d2d", "cb")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	for i := range aLines {
		a.File.AppendLine(aLines[i])
		b.File.AppendLine(bLines[i])
	}
	awaitCursorInBatches(ctx, t, fake, a.File.Path(), a.File.Offset())
	awaitCursorInBatches(ctx, t, fake, b.File.Path(), b.File.Offset())

	// Assert.
	batches := fake.AckedBatches()
	cursors := map[string]*storev1.CursorState{}
	for _, f := range []*growingFile{a.File, b.File} {
		cs := latestCursorFor(batches, f.Path())
		if cs == nil {
			t.Fatalf("no cursor was committed for %s", f.Path())
		}
		if got, want := cs.GetFileId(), fileID(t, f.Path()); got != want {
			t.Errorf("the cursor for %s names file_id %q, wanted the file's own %q", f.Path(), got, want)
		}
		if cs.GetOffset() != f.Offset() {
			t.Errorf("the cursor for %s stands at %d, wanted %d", f.Path(), cs.GetOffset(), f.Offset())
		}
		cursors[cs.GetFileId()] = cs
	}
	if len(cursors) != 2 {
		t.Errorf("the two files committed %d distinct file_ids, wanted 2", len(cursors))
	}
}
