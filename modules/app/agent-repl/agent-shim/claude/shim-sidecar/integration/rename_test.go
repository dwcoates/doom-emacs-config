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
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)
	movedSlug := cwdSlug("/Users/dodgecoates/transcript-rename-probe")
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
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	fx := seedDetachedShell(t, tree, "/Users/dodgecoates/spool-rename-probe",
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
