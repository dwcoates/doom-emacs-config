package integration

import (
	"testing"
)

// CRITIQUE 24 — the spool task-id PREFIX is how a conversion is selected (R-S4),
// and each of the three recognized prefixes is a different thing.
//
// b* already has the whole detached-shell suite. The unclassifiable prefix
// already has its two subjects in spool_ownership_test.go (the loud ERROR, and
// the bytes landing whole as residue anyway). These are the two that were
// missing: an a* spool is a backgrounded SUBAGENT'S OWN TRANSCRIPT and converts
// as one, into the SPAWNING CALL's book; a w* spool is residue only, because
// workflow is KICKED this wave.

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
	_, spoolPath, parent := seedBackgroundedAgent(t, tree, "/Users/dodgecoates/agent-spool-probe", session)

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
	_, spoolPath, parent := seedBackgroundedAgent(t, tree, "/Users/dodgecoates/agent-spool-residue-probe", session)
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

// TestAWorkflowSpoolLandsAsResidueOnly asserts the w* routing while workflow is
// KICKED: the file is discovered and cursor-tailed like any other, its bytes are
// read whole and classified as residue, and nothing about it is converted as
// workflow.
//
// RESIDUE IS NEVER PERSISTED, so "landed" is asserted where the evidence now is:
// the reader's own record saying it classified this file's bytes and withheld
// them, and a cursor that reached the end of the file.
func TestAWorkflowSpoolLandsAsResidueOnly(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/workflow-spool-probe"
	slug := cwdSlug(cwd)
	session := "a3a3a3a3-a3a3-4a3a-8a3a-a3a3a3a3a3a3"
	spoolPath := tree.spoolPath(slug, session, "ww0dfgg1i")
	payload := "{\"kind\":\"workflow-journal-line\"}\n"
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte(payload))
	// The label is `unparsed`: no transcript ever names this spool, so it is
	// held and then ingested by the unowned path rather than by its w* prefix.
	awaitResidueWithheldNamingFile(ctx, t, opts.LogPath, spoolPath, "unparsed")

	// Assert: the whole file was read, nothing workflow-shaped was produced, and
	// no residue row was stored.
	awaitCursorInBatches(ctx, t, fake, spoolPath, spool.Offset())
	for _, e := range fake.Entries() {
		if e.GetAgentUpdate().GetWorkflow() != nil {
			t.Errorf("a w* spool produced a workflow entry while workflow is kicked: %v", e.GetUpsertKey())
		}
	}
	requireNoResidueStored(t, fake.Entries())
}

// TestAWorkflowSpoolReachesNoPage asserts the other half of "residue only": a
// kicked kind is structurally unservable, so none of it reaches any book.
func TestAWorkflowSpoolReachesNoPage(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/workflow-spool-page-probe"
	slug := cwdSlug(cwd)
	session := "a4a4a4a4-a4a4-4a4a-8a4a-a4a4a4a4a4a4"
	spoolPath := tree.spoolPath(slug, session, "ww0dfgg1i")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	spool := newGrowingFile(t, spoolPath)
	spool.AppendRaw([]byte("{\"kind\":\"workflow-journal-line\"}\n"))
	awaitAnyCursorFor(ctx, t, fake, spoolPath)

	// Assert.
	if len(pageLinesOf(fake.Entries())) != 0 {
		t.Errorf("a w* spool produced %d page line(s) while workflow is kicked", len(pageLinesOf(fake.Entries())))
	}
}
