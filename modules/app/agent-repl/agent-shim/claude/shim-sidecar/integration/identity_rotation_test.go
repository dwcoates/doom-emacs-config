package integration

import (
	"os"
	"path/filepath"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — a `/clear` ROTATION, and which book the new transcript writes to.
//
// A clear mints a NEW vendor session id and a NEW transcript file, and — as
// clear_cut_test.go's own capture shows — the old transcript simply STOPS.
// NOTHING IN EITHER FILE LINKS THEM: the shim's survey of 1,107 real
// transcripts found no `forkedFrom`, `parentSessionId` or `resumedFrom` key
// anywhere, and the SDK's ForkSessionResult is `{ sessionId }` alone.
//
// SO THE SHIM WRITES THE LINK THE FILES LACK, at
// `<state>/shim/<workspace-key>/vendor-id/<new-id>.json`, naming the original
// its `agent-id.json` holds. These subjects run a REAL sidecar over a real
// state root and assert the consequence: the rotated transcript's records land
// in the ORIGINAL's book, so the store is never asked to move a row between
// books — the refusal that used to park the file and freeze its cursor.

const (
	// The conversation's original vendor session id — the shim-minted main
	// AgentId — and the id a `/clear` rotated it to.
	rotationOriginalID = "c1ea4c1e-a4c1-4ea4-8c1e-a4c1ea4c1ea4"
	rotationNewID      = "0af7e40a-f7e4-40a7-8e40-af7e40af7e40"
	// The workspace key the shim derives from its cwd (md5(cwd)[:8]). The
	// sidecar never derives it — it enumerates the directories — so any
	// eight-hex-digit name serves here, and that is exactly the point.
	rotationWorkspaceKey = "0a1b2c3d"
)

// stateRootWithIdentity builds a state root holding the shim's identity record
// for one conversation, in engine/identity.ts's own on-disk field names.
func stateRootWithIdentity(t *testing.T, originalID string) string {
	t.Helper()
	stateDir := filepath.Join(t.TempDir(), "state")
	dir := filepath.Join(stateDir, "shim", rotationWorkspaceKey)
	mustMkdirAll(t, dir)
	mustWriteFile(t, filepath.Join(dir, "agent-id.json"), `{
  "original_vendor_session_id": "`+originalID+`",
  "workspace_key": "`+rotationWorkspaceKey+`",
  "minted_at_ms": 1735689600000
}
`)
	return stateDir
}

// writeVendorLink writes the pointer file SessionIdentity.rotate leaves behind.
func writeVendorLink(t *testing.T, stateDir, vendorID, originalID string) {
	t.Helper()
	dir := filepath.Join(stateDir, "shim", rotationWorkspaceKey, "vendor-id")
	mustMkdirAll(t, dir)
	mustWriteFile(t, filepath.Join(dir, vendorID+".json"), `{
  "vendor_session_id": "`+vendorID+`",
  "original_vendor_session_id": "`+originalID+`",
  "linked_at_ms": 1735689700000
}
`)
}

func mustWriteFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("writing %s: %v", path, err)
	}
}

// TestARotatedTranscriptsRecordsLandInTheOriginalsBook is the whole fix, driven
// through a real sidecar process: the file is NAMED by the rotated id and every
// record it produces is booked under the original.
func TestARotatedTranscriptsRecordsLandInTheOriginalsBook(t *testing.T) {
	t.Parallel()
	// Arrange: the shim minted an identity for this conversation and then
	// rotated it, leaving the link the transcripts do not carry.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	stateDir := stateRootWithIdentity(t, rotationOriginalID)
	writeVendorLink(t, stateDir, rotationNewID, rotationOriginalID)
	captured := loadCapturedSession(t)

	options := defaultSidecarOptions(t, fake.Socket, tree)
	options.StateDir = stateDir

	// Act: the rotated transcript — a new file under the NEW id — is written.
	startSidecar(t, options)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, rotationNewID))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	fake.awaitEntry(ctx, t, "the rotated transcript's unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == "activity:"+capturedBashCall1
	})

	// Assert: the unit is a line in the ORIGINAL's book...
	entries := fake.Entries()
	unit := entryByUpsertKey(entries, "activity:"+capturedBashCall1)
	if got := unit.GetAgentUpdate().GetServeableFrame().GetPageAgentId().GetValue(); got != rotationOriginalID {
		t.Errorf("the rotated transcript's unit landed in book %q, wanted the conversation's original id %q", got, rotationOriginalID)
	}
	// ...and the rotated id opened no book of its own, which is the whole
	// refusal: a second book for one conversation is what the store rejects.
	if lines := linesForBook(entries, rotationNewID); len(lines) != 0 {
		t.Errorf("%d page line(s) landed in a book named by the ROTATED vendor session id %q; the AgentId is unaffected by a rotation (R9)",
			len(lines), rotationNewID)
	}
}

// TestARotationLinkThatAppearsMidTailMovesTheBook is the race the reader must
// survive: the transcript is discovered and read BEFORE its link file is
// visible, so the first records were booked under the rotated id.
func TestARotationLinkThatAppearsMidTailMovesTheBook(t *testing.T) {
	t.Parallel()
	// Arrange: an identity, and no link yet.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	stateDir := stateRootWithIdentity(t, rotationOriginalID)
	captured := loadCapturedSession(t)

	options := defaultSidecarOptions(t, fake.Socket, tree)
	options.StateDir = stateDir
	startSidecar(t, options)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, rotationNewID))
	// THE HEAD MUST CONVERT TO A TYPED ENTRY. The book is read off a STORED
	// update's `top_level`, and residue is never stored — so a head of the
	// capture's opening bookkeeping lines would leave the store empty and the
	// precondition unable to say which book the file was reading into. Lines 0-7
	// end on the first response's own assistant record, which is typed.
	for _, line := range captured.Lines[:8] {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	// The books are read off `top_level`, which every update carries, rather
	// than off page lines alone.
	if books := booksOf(fake.Entries()); !contains(books, rotationNewID) {
		t.Fatalf("precondition: with no link on disk the transcript must book under its own id %q; the books written were %v",
			rotationNewID, books)
	}

	// Act: the link appears, and the rest of the transcript is written.
	writeVendorLink(t, stateDir, rotationNewID, rotationOriginalID)
	for _, line := range captured.Lines[8:] {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	fake.awaitEntry(ctx, t, "the rotated transcript's unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == "activity:"+capturedBashCall1
	})

	// Assert: everything read after the link landed in the original's book.
	unit := entryByUpsertKey(fake.Entries(), "activity:"+capturedBashCall1)
	if got := unit.GetAgentUpdate().GetServeableFrame().GetPageAgentId().GetValue(); got != rotationOriginalID {
		t.Errorf("after the link appeared the unit landed in book %q, wanted %q", got, rotationOriginalID)
	}
}

// TestATranscriptWithNoIdentityRecordKeepsItsOwnBook: a tree no shim ever wrote
// a record for reads exactly as it did before the records existed.
func TestATranscriptWithNoIdentityRecordKeepsItsOwnBook(t *testing.T) {
	t.Parallel()
	// Arrange: a state root with no record for this conversation at all.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	stateDir := stateRootWithIdentity(t, rotationOriginalID)
	captured := loadCapturedSession(t)

	options := defaultSidecarOptions(t, fake.Socket, tree)
	options.StateDir = stateDir

	// Act.
	startSidecar(t, options)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	fake.awaitEntry(ctx, t, "the unrecorded transcript's unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == "activity:"+capturedBashCall1
	})

	// Assert.
	unit := entryByUpsertKey(fake.Entries(), "activity:"+capturedBashCall1)
	if got := unit.GetAgentUpdate().GetServeableFrame().GetPageAgentId().GetValue(); got != captured.Session {
		t.Errorf("an unrecorded transcript landed in book %q, wanted its own file's uuid %q", got, captured.Session)
	}
}

func contains(values []string, want string) bool {
	for _, value := range values {
		if value == want {
			return true
		}
	}
	return false
}

// booksOf names every book the store was written to, so a failure states what
// actually happened rather than only what did not.
func booksOf(entries []*storev1.StoreEntry) []string {
	seen := map[string]bool{}
	var out []string
	for _, e := range entries {
		book := e.GetAgentUpdate().GetTopLevel().GetValue()
		if book == "" || seen[book] {
			continue
		}
		seen[book] = true
		out = append(out, book)
	}
	return out
}
