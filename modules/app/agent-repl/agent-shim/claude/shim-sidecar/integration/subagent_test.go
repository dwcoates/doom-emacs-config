package integration

import (
	"os"
	"path/filepath"
	"testing"
)

// SUBJECT 2 — a subagent transcript and its meta.json.
//
// A sidechain file is NOT ingestible without its meta: the meta is the only
// source for the agent's type, description, spawn depth and model — and, under
// the Landing 3 rule, for the agent's very IDENTITY. A transcript whose meta has
// not appeared yet is HELD (not tailed) with a warning and never dropped. Once
// the meta lands, the sidechain's frames form the subagent's OWN book, keyed by
// the meta's `toolUseId` — the tool_use_id of the spawning call. The vendor's
// own `agentId`, which is also the `agent-<id>` file name, is a LOCATOR for the
// files on disk and never an identity.

// corpusSubagentID is the agent id of the checked-in sidechain fixture; its
// file name and its records' `agentId` field agree on it.
// corpusSubagentID is the `agent-<id>` of the checked-in sidechain fixture — a
// LOCATOR that names the file on disk, and deliberately NOT the agent's identity.
const corpusSubagentID = "aef975b7bc3422d4b"

// corpusSubagentAgentID is that agent's IDENTITY: the tool_use_id of the call
// that spawned it, as the fixture's own meta.json states it in `toolUseId`. The
// cross-plane minting rule binds both planes to this one id, so the book, the
// frames' agent_id and the spawn's created_agent_id all carry it.
const corpusSubagentAgentID = "toolu_019w534yMVsDAc3KqJYLGhP8"

// writeSubagentTranscript copies the sidechain fixture into a session's
// subagents/ directory, growing it one fsynced line at a time.
func writeSubagentTranscript(t *testing.T, tree *vendorTree, slug, session, agent string) *growingFile {
	t.Helper()
	g := newGrowingFile(t, tree.subagentPath(slug, session, agent))
	for _, line := range corpusLines(t, "sidechain/agent-"+corpusSubagentID+".jsonl") {
		g.AppendLine(line)
	}
	return g
}

// writeSubagentMeta drops the fixture's meta.json beside the transcript.
func writeSubagentMeta(t *testing.T, tree *vendorTree, slug, session, agent string) string {
	t.Helper()
	path := tree.subagentMetaPath(slug, session, agent)
	mustMkdirAll(t, filepath.Dir(path))
	if err := os.WriteFile(path, corpusBytes(t, "sidechain/agent-"+corpusSubagentID+".meta.json"), 0o644); err != nil {
		t.Fatalf("write subagent meta %s: %v", path, err)
	}
	return path
}

// TestASidechainWithoutItsMetaIsHeldRatherThanIngested asserts the transcript
// produces nothing while its meta is absent, and says so.
func TestASidechainWithoutItsMetaIsHeldRatherThanIngested(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-hold-probe"
	slug := cwdSlug(cwd)
	session := "33333333-3333-4333-8333-333333333333"
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act: the sidechain lands with no meta beside it.
	startSidecar(t, opts)
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	awaitLog(ctx, t, opts.LogPath, "the held-sidechain warning", func(r logRecord) bool {
		return r.Level == "warn" && samePathAny(r.Context["path"], g.Path())
	})

	// Assert: nothing from that file was written. BOTH candidate books are
	// checked — the file's `agent-<id>` LOCATOR and the identity the meta would
	// have named — because checking only the locator proves nothing: the reader
	// never keys a book by the locator anyway, so that assertion held even for a
	// sidechain that had been ingested wholesale into its real book.
	if latestCursorFor(fake.Batches(), g.Path()) != nil {
		t.Errorf("a sidechain with no meta advanced a cursor; it must not be tailed at all")
	}
	for _, book := range []string{corpusSubagentID, corpusSubagentAgentID} {
		if len(linesForBook(fake.Entries(), book)) != 0 {
			t.Errorf("a sidechain with no meta produced page lines in book %q", book)
		}
	}
}

// TestASidechainIsIngestedOnceItsMetaAppears asserts the held file is picked up
// — never dropped — as soon as the meta is written.
func TestASidechainIsIngestedOnceItsMetaAppears(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-meta-probe"
	slug := cwdSlug(cwd)
	session := "44444444-4444-4444-8444-444444444444"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	if len(fake.Entries()) == 0 {
		t.Fatalf("the sidechain produced nothing after its meta appeared")
	}
}

// TestASubagentsFramesFormItsOwnBook asserts the sidechain's page lines are
// keyed by the agent's OWN identity — the spawning call's tool_use_id from its
// meta.json — and not by the owning session or by the file's name.
func TestASubagentsFramesFormItsOwnBook(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-book-probe"
	slug := cwdSlug(cwd)
	session := "55555555-5555-4555-8555-555555555555"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	lines := awaitBookLines(ctx, t, store.Client, corpusSubagentAgentID, 1)

	// Assert.
	for _, at := range lines {
		if got := at.GetLine().GetPageAgentId().GetValue(); got != corpusSubagentAgentID {
			t.Errorf("a subagent's line names book %q, wanted its spawning call %q", got, corpusSubagentAgentID)
		}
	}
}

// TestASubagentsBookIsNotItsFileName asserts the other half of the minting rule:
// the `agent-<id>` locator must reach no book at all, or one agent would have two
// — one per plane — that no consumer could reconcile.
func TestASubagentsBookIsNotItsFileName(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-locator-probe"
	slug := cwdSlug(cwd)
	session := "5b5b5b5b-5b5b-45b5-85b5-5b5b5b5b5b5b"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	for _, line := range pageLinesOf(fake.Entries()) {
		if got := line.GetPageAgentId().GetValue(); got == corpusSubagentID {
			t.Errorf("a page line names book %q, which is the file's locator rather than the agent's identity", got)
		}
	}
	if len(linesForBook(fake.Entries(), corpusSubagentAgentID)) == 0 {
		t.Fatalf("no line reached the subagent's own book %q", corpusSubagentAgentID)
	}
}

// TestASubagentsFramesNameTheSessionsMainAgentAsTopLevel asserts top_level on a
// sidechain frame is the owning session's main agent — the nearest non-sync
// ancestor — because this spawn was not backgrounded.
func TestASubagentsFramesNameTheSessionsMainAgentAsTopLevel(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-toplevel-probe"
	slug := cwdSlug(cwd)
	session := "66666666-6666-4666-8666-666666666666"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	var checked int
	for _, e := range fake.Entries() {
		up := e.GetAgentUpdate()
		if up.GetServeableFrame().GetPageAgentId().GetValue() != corpusSubagentAgentID {
			continue
		}
		checked++
		if got := up.GetTopLevel().GetValue(); got != session {
			t.Errorf("subagent frame %q names top_level %q, wanted the session's main agent %q",
				e.GetUpsertKey(), got, session)
		}
	}
	if checked == 0 {
		t.Fatalf("no subagent page line was written, so top_level was never stated")
	}
}

// TestASubagentsFirstUserMessageIsWithheld asserts the sidechain's opening user
// message is a commission, not a served prompt (R15).
//
// RE-AIMED for the 2026-09-13 residue ruling: the commission was read and
// classified as before, but a `vendor_specific` classification is now withheld
// at the write path instead of stored, so it is checked in the reader's own
// account of what it withheld.
func TestASubagentsFirstUserMessageIsWithheld(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-prompt-probe"
	slug := cwdSlug(cwd)
	session := "77777777-7777-4777-8777-777777777777"
	// The withheld-record accounts are verbose, so the subject asks for them.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	for _, line := range pageLinesOf(fake.Entries()) {
		if line.GetAgentItem().GetAgentPrompt() != nil {
			t.Errorf("a subagent transcript minted an AgentPrompt page line: %v", line)
		}
	}
	awaitResidueWithheld(ctx, t, opts.LogPath, "vendor_specific/"+vendorSpecificUserPrompt)
	requireNoResidueStored(t, fake.Entries())
}
