package integration

import (
	"os"
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

// stateRootWithIdentity writes the shim's identity record for one conversation
// into the subject's state root, in engine/identity.ts's own on-disk field
// names, and holds its workspace's lock: the shim that minted it is live.
func stateRootWithIdentity(t *testing.T, tree *vendorTree, originalID string) string {
	t.Helper()
	tree.live.mint(t, rotationWorkspaceKey, originalID)
	return tree.live.state
}

// writeVendorLink writes the pointer file SessionIdentity.rotate leaves behind.
func writeVendorLink(t *testing.T, tree *vendorTree, vendorID, originalID string) {
	t.Helper()
	tree.live.link(t, rotationWorkspaceKey, vendorID, originalID)
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
	stateDir := stateRootWithIdentity(t, tree, rotationOriginalID)
	writeVendorLink(t, tree, rotationNewID, rotationOriginalID)
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

// TestARotationIsNotReadUntilItsLinkNamesItsBook is the race the reader must
// survive: the rotated transcript is written BEFORE its link file exists. No
// identity record names it yet, so no active workspace owns it and it is not
// read at all (active.go); when the link lands it is admitted and read from its
// start, so every record lands in the original's book and none is ever booked
// under the rotated id. (It used to be read at once under the rotated id and
// re-keyed mid-tail, which split the conversation's first records off.)
func TestARotationIsNotReadUntilItsLinkNamesItsBook(t *testing.T) {
	t.Parallel()
	// Arrange: an identity, and no link yet.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	stateDir := stateRootWithIdentity(t, tree, rotationOriginalID)
	captured := loadCapturedSession(t)

	options := defaultSidecarOptions(t, fake.Socket, tree)
	options.StateDir = stateDir
	// The gate's per-file decision is DEBUG, and it is the signal this subject
	// waits on.
	options.ExtraEnv = append(options.ExtraEnv, "AGENT_REPL_LOG_LEVEL=debug")
	startSidecar(t, options)
	g := newGrowingFile(t, tree.inactiveSessionPath(captured.Slug, rotationNewID))
	for _, line := range captured.Lines[:8] {
		g.AppendLine(line)
	}
	awaitLog(ctx, t, options.LogPath, "the rotated transcript gated out before its link", func(r logRecord) bool {
		return r.Operation == "watch-dormant" && r.Context["vendor_session_id"] == rotationNewID
	})

	// Act: the link appears, and the tail is appended.
	writeVendorLink(t, tree, rotationNewID, rotationOriginalID)
	for _, line := range captured.Lines[8:] {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	fake.awaitEntry(ctx, t, "the rotated transcript's unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == "activity:"+capturedBashCall1
	})

	// Assert: the whole file landed in the original's book, and none of it
	// under the rotated id.
	entries := fake.Entries()
	unit := entryByUpsertKey(entries, "activity:"+capturedBashCall1)
	if got := unit.GetAgentUpdate().GetServeableFrame().GetPageAgentId().GetValue(); got != rotationOriginalID {
		t.Errorf("after the link appeared the unit landed in book %q, wanted %q", got, rotationOriginalID)
	}
	if books := booksOf(entries); contains(books, rotationNewID) {
		t.Errorf("records were booked under the rotated id %q before its link named its book; the books written were %v", rotationNewID, books)
	}
}

// TestASessionRunOutsideAgentReplIsNotRead: a transcript no shim ever wrote a
// record for is a session run outside agent-repl, and no active workspace owns
// it, so it is never read (owner ruling, 2026-09-24). It used to be read and
// booked under its own id.
func TestASessionRunOutsideAgentReplIsNotRead(t *testing.T) {
	t.Parallel()
	// Arrange: a live agent-repl conversation, and an external session beside it.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	options := defaultSidecarOptions(t, fake.Socket, tree)
	options.ExtraEnv = append(options.ExtraEnv, "AGENT_REPL_LOG_LEVEL=debug")
	startSidecar(t, options)

	// Act.
	external := newGrowingFile(t, tree.inactiveSessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		external.AppendLine(line)
	}
	awaitLog(ctx, t, options.LogPath, "the external session gated out", func(r logRecord) bool {
		return r.Operation == "watch-dormant" && r.Context["vendor_session_id"] == captured.Session
	})
	cwd := "/Users/dodgecoates/live-beside-external-probe"
	live := newGrowingFile(t, tree.sessionPath(cwdSlug(cwd), rotationOriginalID))
	live.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), rotationOriginalID, cwd)))
	awaitCursorInBatches(ctx, t, fake, live.Path(), live.Offset())

	// Assert.
	if latestCursorFor(fake.Batches(), external.Path()) != nil {
		t.Errorf("the external session's transcript was read: %s", external.Path())
	}
	if lines := linesForBook(fake.Entries(), captured.Session); len(lines) != 0 {
		t.Errorf("the external session produced %d page line(s)", len(lines))
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
