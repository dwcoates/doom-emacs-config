package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — A PROMPT RE-SENT WHILE THE FIRST VERSION IS STILL PENDING, LIVE.
//
// The first version is read, converted and committed on its own poll before
// the re-send exists, so a feed watching the book has already drawn it. The
// re-send, read on a later poll, must be written onto THAT row — the one the
// feed holds — so the bubble is replaced in place: at no instant does the book
// hold two prompt rows for one question (convert/resend.go).

// resendFixtureLines is the captured re-send (testdata/corpus MANIFEST):
// [0] the shared parent, [1] a snapshot, [2] the first version, [3] its
// attachment, [4] a snapshot, [5] the re-sent version, [6] its attachment,
// [7] the answer's first line.
const resendFixtureLines = "transcript-lines/user-prompt-resend.jsonl"

// TestALiveReSendReplacesThePendingPromptRowInPlace asserts both polls wrote
// ONE prompt row, and that it holds the re-sent version's words at the end.
func TestALiveReSendReplacesThePendingPromptRowInPlace(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/prompt-resend-probe"
	slug := cwdSlug(cwd)
	session := "7c7c7c7c-7c7c-47c7-87c7-7c7c7c7c7c7c"
	var records []map[string]any
	for _, line := range corpusLines(t, resendFixtureLines) {
		records = append(records, retargetSession(t, decodeRecord(t, line), session, cwd))
	}
	// The captured versions are word-for-word identical; the re-send is given
	// an edit so the row's words say which version it holds.
	records[5] = withFields(t, records[5], map[string]any{
		"message": map[string]any{"role": "user", "content": "what would this look like on the gns page, edited"},
	})
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	appendRecords(t, g, records[:4]...)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	pending := promptKeysOf(fake.Entries())

	// Act: the re-send and its answer land on a later poll.
	appendRecords(t, g, records[4:]...)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	writes := promptWritesOf(fake.Entries())
	if len(pending) != 1 {
		t.Fatalf("the pending version wrote prompt rows %v, want exactly one", pending)
	}
	for _, w := range writes {
		if w.GetUpsertKey() != pending[0] {
			t.Fatalf("a prompt write landed on row %s beside the pending row %s; the feed would draw two bubbles",
				w.GetUpsertKey(), pending[0])
		}
	}
	last := writes[len(writes)-1].GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentPrompt()
	if got := last.GetSaid().GetContent().GetBlocks()[0].GetText().GetText(); got != "what would this look like on the gns page, edited" {
		t.Fatalf("the row's last write holds %q, want the re-sent version's words", got)
	}
}

// promptWritesOf is every prompt page-line write, in producer order.
func promptWritesOf(entries []*storev1.StoreEntry) []*storev1.StoreEntry {
	var out []*storev1.StoreEntry
	for _, e := range entries {
		if e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentPrompt() != nil {
			out = append(out, e)
		}
	}
	return out
}

// promptKeysOf is the distinct rows those writes landed on, in first-write order.
func promptKeysOf(entries []*storev1.StoreEntry) []string {
	var keys []string
	seen := map[string]bool{}
	for _, e := range promptWritesOf(entries) {
		if !seen[e.GetUpsertKey()] {
			seen[e.GetUpsertKey()] = true
			keys = append(keys, e.GetUpsertKey())
		}
	}
	return keys
}
