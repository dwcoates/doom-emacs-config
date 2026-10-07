package integration

import (
	"testing"
)

// CRITIQUE 24 — the spool task-id PREFIX is how a conversion is selected (R-S4),
// and each of the three recognized prefixes is a different thing.
//
// b* already has the whole detached-shell suite. The unclassifiable prefix
// already has its two subjects in spool_ownership_test.go (the loud ERROR, and
// the file never being read). These are the ones that were missing: an a* spool
// is a backgrounded SUBAGENT'S OWN TRANSCRIPT and converts as one, into the
// SPAWNING CALL's book; a w* spool nobody claimed is never read, because
// workflow is KICKED this wave and nothing renders it.

// corpusAsyncAgentTask is the a* task id the checked-in async-launch fixture
// names — the vendor's `agentId`, which is the SPOOL's name and never the
// created agent's identity.
const corpusAsyncAgentTask = "a15b5267244c1360e"

// seedBackgroundedAgent writes the parent transcript's spawn pair for a
// BACKGROUNDED subagent: the Agent call, and the real async-launch result that
// names the a* task and its output file, re-pointed at this test's spool.
func seedBackgroundedAgent(t *testing.T, tree *vendorTree, cwd, session string) (slug, spoolPath string, parent *growingFile) {
	t.Helper()
	captured := loadCapturedSession(t)
	slug = cwdSlug(cwd)
	spoolPath = tree.spoolPath(slug, session, corpusAsyncAgentTask)

	call := renameToolUse(t, retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd), "Agent")
	result := retargetSession(t, decodeRecord(t, corpusLine(t, "tool-results/agent_async_launch.jsonl", 0)), session, cwd)
	result = setToolUseID(t, result, capturedBashCall1)
	result = setNested(t, result, "toolUseResult", "outputFile", spoolPath)

	parent = newGrowingFile(t, tree.sessionPath(slug, session))
	parent.AppendLine(encodeRecord(t, call))
	parent.AppendLine(encodeRecord(t, result))
	return slug, spoolPath, parent
}

// TestAnAgentSpoolConvertsAsATranscriptIntoItsSpawningCallsBook asserts the a*
// routing: the spool's JSONL is read as the agent's own transcript, and its page
// lines are the spawning call's book — the cross-plane minting rule's identity.
func TestAnAgentSpoolConvertsAsATranscriptIntoItsSpawningCallsBook(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "a1a1a1a1-a1a1-4a1a-8a1a-a1a1a1a1a1a1"
	_, spoolPath, parent := seedBackgroundedAgent(t, tree, "/work/agent-spool-probe", session)

	// Act: the spawn is observed first, then the agent's transcript arrives
	// through its task spool.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	awaitCursorInBatches(ctx, t, fake, parent.Path(), parent.Offset())
	spool := newGrowingFile(t, spoolPath)
	for _, line := range corpusLines(t, "sidechain/agent-"+corpusSubagentID+".jsonl") {
		spool.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, spoolPath, spool.Offset())

	// Assert: the records reached the spawning call's book...
	if len(linesForBook(fake.Entries(), capturedBashCall1)) == 0 {
		t.Fatalf("an a* spool produced no page line in the spawning call's book %q", capturedBashCall1)
	}
	// ...and never a book named by the spool's own task id, which is a LOCATOR.
	if len(linesForBook(fake.Entries(), corpusAsyncAgentTask)) != 0 {
		t.Errorf("an a* spool's records reached a book named by the vendor task id %q", corpusAsyncAgentTask)
	}
}

// TestAnAgentSpoolIsNotIngestedAsRawResidue asserts the routing was a
// CONVERSION rather than a fallback: an a* spool read raw would classify its
// whole contents as unparsed residue and still advance its cursor, which no
// assertion about the book above would notice on its own.
//
// THE READER'S OWN STATEMENT IS THE EVIDENCE. Residue is never persisted, so an
// empty store proves nothing here — a raw-read spool would leave the store just
// as clean. What separates conversion from fallback is that the reader never
// said it classified a single line of this file as residue.
func TestAnAgentSpoolIsNotIngestedAsRawResidue(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "a2a2a2a2-a2a2-4a2a-8a2a-a2a2a2a2a2a2"
	_, spoolPath, parent := seedBackgroundedAgent(t, tree, "/work/agent-spool-residue-probe", session)
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	awaitCursorInBatches(ctx, t, fake, parent.Path(), parent.Offset())
	spool := newGrowingFile(t, spoolPath)
	for _, line := range corpusLines(t, "sidechain/agent-"+corpusSubagentID+".jsonl") {
		spool.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, spoolPath, spool.Offset())

	// Assert.
	// `unparsed` IS THE FALLBACK, and it is the only label that says this file
	// was read raw. A converted transcript legitimately classifies some of its
	// own lines as `vendor_specific` — that is the converter working, not the
	// routing failing — so the defect this subject hunts is the raw arm alone.
	id := fileID(t, spoolPath)
	for _, r := range readLog(t, opts.LogPath) {
		if r.Operation == "residue-drop" && r.Context["file_id"] == id && r.Context["reason"] == "unparsed" {
			t.Errorf("an a* spool's bytes were read raw and classified unparsed rather than converted: %v", r.Context)
		}
	}
	requireNoResidueStored(t, fake.Entries())
}

// TestAnUnclaimedWorkflowSpoolIsNeverRead asserts the w* routing while
// workflow is KICKED: no transcript ever names the spool, so it is held, its
// window lapses, and it is never read — nothing renders it.
func TestAnUnclaimedWorkflowSpoolIsNeverRead(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/workflow-spool-probe"
	slug := cwdSlug(cwd)
	session := "a3a3a3a3-a3a3-4a3a-8a3a-a3a3a3a3a3a3"
	spoolPath := tree.spoolPath(slug, session, "ww0dfgg1i")
	// The re-resolution this subject waits on is stated at DEBUG.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("{\"kind\":\"workflow-journal-line\"}\n"))
	awaitLog(ctx, t, opts.LogPath, "the hold expiring", func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	lapsedAt := logIndexOf(t, opts.LogPath, func(r logRecord) bool {
		return r.Operation == "hold-expired" && samePathAny(r.Context["path"], spoolPath)
	})
	awaitRestatedAfter(ctx, t, opts.LogPath, spoolPath, "hold-spool", lapsedAt)

	// Assert.
	requireNeverRead(t, fake, opts.LogPath, spoolPath)
	if len(pageLinesOf(fake.Entries())) != 0 {
		t.Errorf("a w* spool produced %d page line(s) while workflow is kicked", len(pageLinesOf(fake.Entries())))
	}
}
