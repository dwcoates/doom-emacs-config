package integration

import (
	"os"
	"path/filepath"
	"testing"
)

// SUBJECT 2 — a subagent transcript and its meta.json.
//
// A sidechain file is NOT ingestible without its meta: the meta is the only
// source for the agent's type, description, spawn depth and model. A transcript
// whose meta has not appeared yet is HELD (not tailed) with a warning and never
// dropped. Once the meta lands, the sidechain's frames form the subagent's OWN
// book, keyed by the vendor agentId — which is also the agent-<id> file name.

// corpusSubagentID is the agent id of the checked-in sidechain fixture; its
// file name and its records' `agentId` field agree on it.
const corpusSubagentID = "aef975b7bc3422d4b"

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
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/subagent-hold-probe"
	slug := cwdSlug(cwd)
	session := "33333333-3333-4333-8333-333333333333"
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act: the sidechain lands with no meta beside it.
	startSidecar(t, opts)
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	awaitLog(ctx, t, opts.LogPath, "the held-sidechain warning", func(r logRecord) bool {
		return r.Level == "warn" && samePathAny(r.Context["path"], g.Path())
	})

	// Assert: nothing from that file was written.
	for _, cs := range []string{g.Path()} {
		if latestCursorFor(fake.Batches(), cs) != nil {
			t.Errorf("a sidechain with no meta advanced a cursor; it must not be tailed at all")
		}
	}
	if len(linesForBook(fake.Entries(), corpusSubagentID)) != 0 {
		t.Errorf("a sidechain with no meta produced page lines")
	}
}

// TestASidechainIsIngestedOnceItsMetaAppears asserts the held file is picked up
// — never dropped — as soon as the meta is written.
func TestASidechainIsIngestedOnceItsMetaAppears(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/subagent-meta-probe"
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
// keyed by the vendor agentId, not by the owning session.
func TestASubagentsFramesFormItsOwnBook(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/subagent-book-probe"
	slug := cwdSlug(cwd)
	session := "55555555-5555-4555-8555-555555555555"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	lines := awaitBookLines(ctx, t, store.Client, corpusSubagentID, 1)

	// Assert.
	for _, at := range lines {
		if got := at.GetLine().GetPageAgentId().GetValue(); got != corpusSubagentID {
			t.Errorf("a subagent's line names book %q, wanted the vendor agentId %q", got, corpusSubagentID)
		}
	}
}

// TestASubagentsFramesNameTheSessionsMainAgentAsTopLevel asserts top_level on a
// sidechain frame is the owning session's main agent — the nearest non-sync
// ancestor — because this spawn was not backgrounded.
func TestASubagentsFramesNameTheSessionsMainAgentAsTopLevel(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/subagent-toplevel-probe"
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
		if up.GetServeableFrame().GetPageAgentId().GetValue() != corpusSubagentID {
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
func TestASubagentsFirstUserMessageIsWithheld(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/subagent-prompt-probe"
	slug := cwdSlug(cwd)
	session := "77777777-7777-4777-8777-777777777777"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	for _, line := range pageLinesOf(fake.Entries()) {
		if line.GetAgentItem().GetAgentPrompt() != nil {
			t.Errorf("a subagent transcript minted an AgentPrompt page line: %v", line)
		}
	}
	if !containsString(vendorSpecificKinds(fake.Entries()), vendorSpecificUserPrompt) {
		t.Errorf("the sidechain's first user message was not withheld as %q; kinds were %v",
			vendorSpecificUserPrompt, vendorSpecificKinds(fake.Entries()))
	}
}
