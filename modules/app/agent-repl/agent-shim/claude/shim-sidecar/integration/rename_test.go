package integration

import (
	"testing"
)

// CRITIQUE 7 (integration half) — a file the vendor RENAMES under the reader.
//
// THE CURSOR'S IDENTITY IS THE FILE, NOT ITS NAME. `cursor_advance.file_id` is
// the file's dev:inode, and the store keys the cursor row by it precisely so a
// rename cannot mint a second row or lose the first — the path column is
// descriptive and follows the file. A reader that keyed the cursor by path would
// pass every unit test in the tailer and still re-ingest a whole conversation the
// first time the vendor moved one of its directories.
//
// The two subjects are the two kinds of file this can happen to, and the
// assertion in each is the same invariant read off the REAL store: one row for
// that identity, naming the new path, covering every byte the vendor wrote.

// TestARenamedTranscriptKeepsItsFileIdCursor moves a session transcript to a
// different project directory mid-tail — the vendor's own relocation, which
// changes the lossy cwd-slug segment and nothing about the session's identity —
// and asserts the cursor row followed the file rather than being re-minted.
func TestARenamedTranscriptKeepsItsFileIdCursor(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)
	movedSlug := cwdSlug("/work/transcript-rename-probe")
	cut := 9

	// Act: read the head, move the file, and keep appending to it in its new home.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:cut] {
		g.AppendLine(line)
	}
	awaitCursorAtLeast(ctx, t, store.Client, g.Path(), 1)
	identity := fileID(t, g.Path())

	movedPath := tree.sessionPath(movedSlug, captured.Session)
	renameFile(t, g.Path(), movedPath)
	moved := newGrowingFile(t, movedPath)
	for _, line := range captured.Lines[cut:] {
		moved.AppendLine(line)
	}
	awaitCursorForFileID(ctx, t, store.Client, identity, moved.Offset())

	// Assert: ONE row for the identity, naming where the file is now.
	rows := cursorsForPath(ctx, t, store.Client, movedPath)
	if len(rows) != 1 {
		t.Fatalf("the store holds %d cursor rows for the renamed transcript: %v", len(rows), describeCursors(rows))
	}
	if got := rows[0].GetFileId(); got != identity {
		t.Errorf("the renamed transcript's cursor is keyed %q, wanted the file's own identity %q", got, identity)
	}
	if before := cursorsForPath(ctx, t, store.Client, g.Path()); len(before) != 0 {
		t.Errorf("a cursor row still names the transcript's OLD path: %v", describeCursors(before))
	}
}

// TestARenamedSpoolKeepsItsFileIdCursor does the same for a task spool, moved
// under the harness's runtime-session segment — the segment discovery
// deliberately never reads, so the file is the same run in a new location.
func TestARenamedSpoolKeepsItsFileIdCursor(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/work/spool-rename-probe",
		"d1d1d1d1-d1d1-4d1d-8d1d-d1d1d1d1d1d1")
	movedPath := tree.spoolPath(fx.Slug, "d2d2d2d2-d2d2-4d2d-8d2d-d2d2d2d2d2d2", fx.TaskID)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	awaitCursorAtLeast(ctx, t, store.Client, fx.Parent.Path(), 1)
	spool := newGrowingFile(t, fx.SpoolPath)
	spool.AppendRaw([]byte("output before the move\n"))
	awaitCursorAtLeast(ctx, t, store.Client, fx.SpoolPath, spool.Offset())
	identity := fileID(t, fx.SpoolPath)

	renameFile(t, fx.SpoolPath, movedPath)
	moved := newGrowingFile(t, movedPath)
	moved.AppendRaw([]byte("output after the move\n"))
	awaitCursorForFileID(ctx, t, store.Client, identity, moved.Offset())

	// Assert.
	rows := cursorsForPath(ctx, t, store.Client, movedPath)
	if len(rows) != 1 {
		t.Fatalf("the store holds %d cursor rows for the renamed spool: %v", len(rows), describeCursors(rows))
	}
	if got := rows[0].GetFileId(); got != identity {
		t.Errorf("the renamed spool's cursor is keyed %q, wanted the file's own identity %q", got, identity)
	}
	if got := rows[0].GetOffset(); got < moved.Offset() {
		t.Errorf("the renamed spool's cursor stands at %d, short of the %d bytes the vendor wrote", got, moved.Offset())
	}
}

// TestARenamedTranscriptIsNotReReadFromZeroWithinOneCycle is the IN-CYCLE half.
//
// Cursors are recovered once, when a cycle BEGINS, so a file the vendor renames
// while that cycle is live is simply absent from the snapshot — and treating
// "absent from a snapshot" as "the store holds no cursor" built the new path's
// tailer at zero and re-converted the whole conversation, absorbed only because
// the write ids are deterministic. A snapshot miss now ASKS the store for that
// one identity, so the file resumes where it actually stood.
//
// IT RUNS AGAINST THE REAL STORE because the question IS what the store holds:
// the in-process fake answers cursor reads from what a subject seeded, not from
// what the sidecar wrote, so every file would read as one the store holds
// nothing for and the subject would pass on the defect.
func TestARenamedTranscriptIsNotReReadFromZeroWithinOneCycle(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)
	movedSlug := cwdSlug("/work/transcript-rename-incycle-probe")
	cut := 9

	// Act: no restart — one cycle throughout.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:cut] {
		g.AppendLine(line)
	}
	committed := awaitCursorAtLeast(ctx, t, store.Client, g.Path(), 1).GetOffset()

	movedPath := tree.sessionPath(movedSlug, captured.Session)
	renameFile(t, g.Path(), movedPath)
	moved := newGrowingFile(t, movedPath)
	for _, line := range captured.Lines[cut:] {
		moved.AppendLine(line)
	}
	awaitCursorAtLeast(ctx, t, store.Client, movedPath, moved.Offset())

	// Assert: the new path's tailer was built from a restored position — the
	// boot rewind is applied to nothing else — and that position is below the
	// committed cursor only by the rewind's own bounded walk, never at zero.
	rec := awaitLog(ctx, t, opts.LogPath, "the renamed file's boot rewind", func(r logRecord) bool {
		return r.Operation == "boot-rewind" && samePathAny(r.Context["path"], movedPath)
	})
	offset, ok := rec.Context["offset"].(float64)
	if !ok {
		t.Fatalf("the boot-rewind record states no offset; its context was %v", rec.Context)
	}
	if offset <= 0 {
		t.Errorf("the renamed file was positioned at %d within one cycle; a snapshot miss must ask the store, never assume zero", int64(offset))
	}
	if int64(offset) > committed {
		t.Errorf("the renamed file was positioned at %d, past the %d already committed", int64(offset), committed)
	}
}
