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
