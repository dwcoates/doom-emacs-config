package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — `/clear`, end to end, and the prose that merely mentions it.
//
// THE LITERAL "/clear" NEVER APPEARS ON DISK. The harness expands a slash
// command into an envelope — `<command-name>/clear</command-name>` plus a
// message and an args element — and the reader detects a clear by UNWRAPPING
// that envelope and finding nothing else left. So the two halves of the rule
// are one subject: the real envelope produces a cleared cut, and a prompt whose
// text merely quotes the command produces none.
//
// THE FIXTURE IS A REAL CAPTURE. testdata/corpus/transcript-lines/user-clear-command.jsonl
// is the verbatim line the vendor wrote for a genuine `/clear` (see the corpus
// MANIFEST for its provenance), so the whitespace and element order this
// detection walks are the vendor's own rather than a reconstruction.
//
// WHAT THE CAPTURE ALSO SHOWS, and what nothing here may assume: the OLD
// transcript simply STOPS. There is no closing record of any kind; the session
// rotates by a NEW transcript file appearing under a new id. The cut record
// itself lands in a transcript like any other line, which is why one growing
// file is the whole arrangement below.

// TestAClearEnvelopeLandsAsAClearedCutAndAQuotedClearDoesNot drives both halves
// of the detection through one sidecar.
func TestAClearEnvelopeLandsAsAClearedCutAndAQuotedClearDoesNot(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/clear-cut-probe"
	slug := cwdSlug(cwd)
	session := "9a9a9a9a-9a9a-49a9-89a9-9a9a9a9a9a9a"

	clear := retargetSession(t, corpusRecord(t, "transcript-lines/user-clear-command.jsonl", 0), session, cwd)
	clearUUID, _ := clear["uuid"].(string)
	if clearUUID == "" {
		t.Fatalf("the captured clear record carries no uuid, so the quoted twin below cannot be given one of its own: %v", clear)
	}

	// The same real record, with prose wrapped around the SAME envelope. It is
	// a prompt ABOUT the command rather than an invocation of it, and the
	// leftover text outside the envelope is exactly what says so.
	quoted := withFields(t, clear, map[string]any{
		"uuid":    "quoted-" + clearUUID,
		"message": quotedClearMessage(t, clear),
	})

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, clear))
	g.AppendLine(encodeRecord(t, quoted))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the real envelope produced ONE cut, on the cleared arm, keyed by
	// THE SESSION THE CLEAR ROTATED TO — this transcript's own session uuid,
	// which is the one identity the STREAM plane can also mint for the same cut
	// — in the main agent's book.
	entries := fake.Entries()
	wantKey := "session:context_cut:" + session
	e := entryByUpsertKey(entries, wantKey)
	if e == nil {
		t.Fatalf("the clear envelope produced no cut under %q; the keys written were %v", wantKey, upsertKeysOf(entries))
	}
	line := e.GetAgentUpdate().GetServeableFrame()
	if line == nil {
		t.Fatalf("a context cut is a page line of the main agent's book: %v", e.GetAgentUpdate())
	}
	if got := line.GetPageAgentId().GetValue(); got != session {
		t.Errorf("the cut names book %q, wanted the main agent %q", got, session)
	}
	cut := contextCutOf(line)
	if cut.GetCleared() == nil {
		t.Fatalf("a /clear produces the CLEARED arm — history discarded outright, no token delta — not %v", cut.GetCut())
	}

	// And the prose that merely quotes the command produced none. THE COUNT IS
	// WHAT SAYS SO, not a second key: every clear in one session now keys on
	// that session, so a wrongly-detected quote would land as a SECOND entry
	// under the same key rather than under one of its own.
	if n := countCuts(entries); n != 1 {
		t.Errorf("the file produced %d context cuts, wanted exactly the one real /clear: a prompt that merely QUOTES the envelope must not cut a conversation", n)
	}
}

// quotedClearMessage wraps the captured envelope in prose, so the record is the
// REAL command text embedded in a question about it.
func quotedClearMessage(t *testing.T, clear map[string]any) map[string]any {
	t.Helper()
	msg, ok := clear["message"].(map[string]any)
	if !ok {
		t.Fatalf("the captured clear record carries no message object: %v", clear)
	}
	text, ok := msg["content"].(string)
	if !ok {
		t.Fatalf("the captured clear record's content is not the plain text a command envelope is written as: %v", msg)
	}
	return withFields(t, msg, map[string]any{
		"content": "here is what the harness writes for a clear:\n" + text + "\nwhat does the args element do?",
	})
}

// countCuts answers how many context cuts a batch stream carried.
func countCuts(entries []*storev1.StoreEntry) int {
	n := 0
	for _, line := range pageLinesOf(entries) {
		if contextCutOf(line) != nil {
			n++
		}
	}
	return n
}
