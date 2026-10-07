package integration

import (
	"os"
	"path/filepath"
	"testing"
)

// REGRESSION (realtest 1, ledger row 51) — a FORK sidechain that QUOTES a parent
// assistant message must not ask the real store to MOVE that message's row into
// the fork's book, and the legitimate work in the same batch must still commit.
//
// A fork copies the parent's whole conversation ahead of its own work, keeping
// each copied assistant record's `message.id`. Re-booking a copied block under
// the fork hands the store `activity:<message id>:<block>` — a key the producer
// already used under ANOTHER book — and an upsert may supersede a row's content
// but never move it between books, so the WHOLE batch is refused (atomic
// rollback) and every legitimate entry beside it is lost too. The sidecar now
// keeps a quoted record as book-NULL residue, so nothing moves and the fork's
// own work commits.

const (
	forkQuoteLocator     = "af0f0f0f0f0f0f0f0"        // the agent-<id> file name (a locator)
	forkQuoteBook        = "toolu_fork_quote_0001"    // the meta's toolUseId — the fork's book
	forkQuoteParentType  = "general-purpose"          // the type of the agent that PRODUCED the quoted message
	forkQuotedMessageID  = "msg_parent_quoted_row_51" // the parent message the fork quotes
	forkOwnMessageID     = "msg_fork_own_row_51"      // the fork's OWN work
	forkOwnUnitFirstBlk  = forkOwnMessageID + ":0"
	forkQuotedUnitFirstB = forkQuotedMessageID + ":0"
)

// writeForkMeta drops a fork's companion meta.json: agentType "fork" and the
// toolUseId that IS the fork's book.
func writeForkMeta(t *testing.T, tree *vendorTree, slug, session, locator, book string) {
	t.Helper()
	path := tree.subagentMetaPath(slug, session, locator)
	mustMkdirAll(t, filepath.Dir(path))
	meta := `{"agentType":"fork","isFork":true,"description":"a fork of its parent",` +
		`"toolUseId":"` + book + `","spawnDepth":1,"parentAgentId":"aparent","model":"inherit"}`
	if err := os.WriteFile(path, []byte(meta), 0o644); err != nil {
		t.Fatalf("write fork meta %s: %v", path, err)
	}
}

// forkSidechainLine builds one sidechain assistant record stamped with the
// vendor's `attributionAgent` (the TYPE of the agent that produced it). `cwd` is
// what the reader resolves the record's workspace from.
func forkSidechainLine(uuid, messageID, attributionAgent, locator, cwd, blocks string) string {
	return `{"type":"assistant","uuid":"` + uuid + `","isSidechain":true,"agentId":"` + locator +
		`","cwd":"` + cwd + `","attributionAgent":"` + attributionAgent + `","timestamp":"2026-07-22T19:58:36.000Z",` +
		`"message":{"id":"` + messageID + `","role":"assistant","content":[` + blocks + `]}}`
}

// assertNoProducerDefect fails if the sidecar ever stated a producer defect for
// the given file — the record a book-move refusal would produce.
func assertNoProducerDefect(t *testing.T, logPath, filePath string) {
	t.Helper()
	for _, r := range logsForOperation(readLog(t, logPath), "producer-defect") {
		if samePathAny(r.Context["path"], filePath) {
			t.Fatalf("the fork transcript produced a producer-defect (%v); a quoted message must not move a row", r.Context)
		}
	}
}

func TestAForkQuotingAParentMessageDoesNotMoveItsRowAndItsOwnWorkCommits(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	cwd := "/work/fork-quote-probe"
	slug := cwdSlug(cwd)
	session := "f0f0f0f0-f0f0-4f0f-8f0f-f0f0f0f0f0f0"
	opts := defaultSidecarOptions(t, store.Socket, tree)

	// The producer already booked the parent's message under its OWN book,
	// exactly as first ingest would. Seeding claims `activity:<message id>:0` so
	// a re-book under the fork's book is a genuine identity move the real store
	// refuses.
	seedDecoyRow(ctx, t, store.Client, "claude-shim:"+session,
		decoyEntry("activity:"+forkQuotedUnitFirstB, "producer-"+forkQuotedMessageID, "the-producing-agent"))

	// The fork sidechain: the parent's message it QUOTES, then its OWN work.
	writeForkMeta(t, tree, slug, session, forkQuoteLocator, forkQuoteBook)
	g := newGrowingFile(t, tree.subagentPath(slug, session, forkQuoteLocator))
	g.AppendLine(forkSidechainLine("quoted-uuid-1", forkQuotedMessageID, forkQuoteParentType, forkQuoteLocator, cwd,
		`{"type":"text","text":"a message copied from the parent"}`))
	g.AppendLine(forkSidechainLine("own-uuid-1", forkOwnMessageID, "fork", forkQuoteLocator, cwd,
		`{"type":"text","text":"the fork's own work"}`))

	// Act: ingest. The fork's own unit reaching its book is the signal the batch
	// committed — if the quoted entry had moved a row, the atomic batch would
	// have rolled back and this unit would never appear.
	first := startSidecar(t, opts)
	awaitBookUnits(ctx, t, store.Client, forkQuoteBook, forkOwnUnitFirstBlk)

	// Assert: no book-move refusal — the fork wrote residue, not the seeded key.
	assertNoProducerDefect(t, opts.LogPath, g.Path())

	// Re-ingest must be idempotent: a restart re-reads the in-progress turn from
	// the committed cursor and re-mints byte-identical write ids, which the store
	// absorbs — never a second refusal, never a duplicated unit.
	first.Stop()
	startSidecar(t, opts)
	awaitBookUnits(ctx, t, store.Client, forkQuoteBook, forkOwnUnitFirstBlk)
	assertNoProducerDefect(t, opts.LogPath, g.Path())

	// The fork's own book holds its own unit EXACTLY once after the replay.
	lines := awaitBookUnits(ctx, t, store.Client, forkQuoteBook, forkOwnUnitFirstBlk)
	seen := 0
	for _, at := range lines {
		if a := activityOf(at.GetLine()); a != nil && a.GetActivityId().GetValue() == forkOwnUnitFirstBlk {
			seen++
		}
	}
	if seen != 1 {
		t.Fatalf("the fork's own unit appears %d times after a re-ingest, want exactly 1", seen)
	}
	// The quoted message never became a page line in the fork's book.
	for _, at := range lines {
		if a := activityOf(at.GetLine()); a != nil && a.GetActivityId().GetValue() == forkQuotedUnitFirstB {
			t.Fatalf("the quoted parent message was booked under the fork %q; it must be residue", forkQuoteBook)
		}
	}
}
