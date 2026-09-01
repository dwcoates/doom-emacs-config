package integration

import (
	"path/filepath"
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

// TestARenamedTranscriptResumesFromItsFileIdCursorOnTheNextCycle is the subject
// the recovered-cursor index exists for, and the one a PATH-keyed index broke.
//
// The store's row survives a rename either way — it is keyed by file_id and the
// next write simply overwrites it — so every assertion about the store's FINAL
// state passed while a renamed file was being re-read from zero and re-converted
// whole, absorbed only because the write ids are deterministic. What separates
// the two is whether the tailer built for the NEW path was RESTORED at all: a
// resumed one is REWOUND to its in-progress turn — a boot rewind happens only
// inside "the store handed us a cursor for this file" — and a cold one is not,
// because there was no restored position to walk back from.
//
// The next cycle is where a recovered cursor is consulted at all (cursors are
// recovered per cycle, and the tailer is rebuilt from what THAT cycle's store
// handed us), so the rename is followed by a restart — the ordinary case, since
// a rename the reader is running through is a rotation it will meet again on its
// next boot.
func TestARenamedTranscriptResumesFromItsFileIdCursorOnTheNextCycle(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, store.Socket, tree)
	movedSlug := cwdSlug("/Users/dodgecoates/transcript-rename-resume-probe")
	cut := 9

	// Act: read the head, rename the file, and start a fresh reader over it.
	first := startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:cut] {
		g.AppendLine(line)
	}
	committed := awaitCursorAtLeast(ctx, t, store.Client, g.Path(), 1).GetOffset()
	first.Stop()

	movedPath := tree.sessionPath(movedSlug, captured.Session)
	renameFile(t, g.Path(), movedPath)
	restarted := opts
	restarted.LogPath = filepath.Join(t.TempDir(), "sidecar-restarted.log")
	startSidecar(t, restarted)

	// Assert: the NEW path's tailer was built from a restored cursor, which the
	// boot rewind is only ever applied to.
	rec := awaitLog(ctx, t, restarted.LogPath, "the renamed file's boot rewind", func(r logRecord) bool {
		return r.Operation == "boot-rewind" && samePathAny(r.Context["path"], movedPath)
	})
	offset, ok := rec.Context["offset"].(float64)
	if !ok {
		t.Fatalf("the boot-rewind record states no offset; its context was %v", rec.Context)
	}
	if int64(offset) > committed {
		t.Errorf("the renamed file was positioned at %d, past the %d the store had committed for its identity",
			int64(offset), committed)
	}
}
