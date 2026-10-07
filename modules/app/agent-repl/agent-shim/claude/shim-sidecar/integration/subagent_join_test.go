package integration

import (
	"os"
	"path/filepath"
	"testing"
)

// SUBJECT — the SUBAGENT JOIN, and the meta's edge cases.
//
// One agent, two files, one identity. The PARENT transcript holds the Agent
// call that spawned it; the SIDECHAIN holds the agent's own records; and the
// `agent-<id>.meta.json` beside the sidechain is the only place the vendor
// states which call that was (`toolUseId`). The cross-plane minting rule binds
// all three: the spawn's created_agent_id, the sidechain's book, and the meta's
// toolUseId are ONE id — the tool_use_id of the spawning call. The vendor's own
// `agentId` is a LOCATOR for the files and never an identity.

// seedAgentSpawn writes the parent side of the join: a real Agent call carrying
// the corpus's Agent input, and the corpus's real async-launch result, both
// re-pointed at the sidechain fixture's own spawning call.
func seedAgentSpawn(t *testing.T, tree *vendorTree, cwd, session string) *growingFile {
	t.Helper()
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	call = renameToolUse(t, call, "Agent")
	call = setToolUseInput(t, call, corpusToolUseInput(t, "tool-inputs/agent.jsonl"))
	call = setToolUseID(t, call, corpusSubagentAgentID)

	launch := retargetSession(t, decodeRecord(t, corpusLine(t, "tool-results/agent_async_launch.jsonl", 0)), session, cwd)
	launch = setToolUseID(t, launch, corpusSubagentAgentID)

	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, call))
	g.AppendLine(encodeRecord(t, launch))
	return g
}

// TestAnAsyncSpawnNamesTheSameAgentTheSidechainsMetaDoes asserts the join: the
// spawn unit's created_agent_id is the id the sidechain's book is keyed by.
func TestAnAsyncSpawnNamesTheSameAgentTheSidechainsMetaDoes(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-join-probe"
	slug := cwdSlug(cwd)
	session := "3a3a3a3a-3a3a-43a3-83a3-3a3a3a3a3a3a"

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	parent := seedAgentSpawn(t, tree, cwd, session)
	sidechain := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	awaitCursorInBatches(ctx, t, fake, parent.Path(), parent.Offset())
	awaitCursorInBatches(ctx, t, fake, sidechain.Path(), sidechain.Offset())

	// Assert: the spawn announced the agent by the call's id…
	entries := fake.Entries()
	spawn := unitEntries(entries, corpusSubagentAgentID)
	if len(spawn) == 0 {
		t.Fatalf("the Agent call produced no unit under %q; keys were %v",
			"activity:"+corpusSubagentAgentID, upsertKeysOf(entries))
	}
	var announced bool
	for _, e := range spawn {
		start := activityOf(e.GetAgentUpdate().GetServeableFrame()).GetSubagent().GetStart()
		if start == nil {
			continue
		}
		announced = true
		if got := start.GetCreatedAgentId().GetValue(); got != corpusSubagentAgentID {
			t.Errorf("the spawn created agent %q, wanted the spawning call %q", got, corpusSubagentAgentID)
		}
	}
	if !announced {
		t.Fatalf("no async spawn was announced for %q", corpusSubagentAgentID)
	}

	// …and the sidechain's own records reached that same book.
	if len(linesForBook(entries, corpusSubagentAgentID)) == 0 {
		t.Errorf("the sidechain's records reached no book named %q", corpusSubagentAgentID)
	}
}

// TestAMetaThatParsesButNamesNoToolUseIdIsRefusedLoudly asserts the edge case
// the meta reader exists for: a WELL-FORMED meta with no `toolUseId` states no
// identity, so the transcript is held and the refusal is an ERROR.
//
// ERROR IS CORRECT, NOT A MISLEVEL. A meta that is merely ABSENT is a WARNING —
// it is expected to appear a moment later and the reader re-checks every rescan.
// A meta that is THERE and unusable will not fix itself, so it is the louder
// level; naming the agent by its filename instead would mint a SECOND book for
// one agent that no consumer could reconcile.
func TestAMetaThatParsesButNamesNoToolUseIdIsRefusedLoudly(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-meta-anonymous-probe"
	slug := cwdSlug(cwd)
	session := "3b3b3b3b-3b3b-43b3-83b3-3b3b3b3b3b3b"
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act: a meta the vendor could have written, minus the one field that IS
	// the agent's identity.
	g := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMetaWithout(t, tree, slug, session, corpusSubagentID, "toolUseId")
	// THE WHOLE FIXTURE IS ON DISK BEFORE THE READER IS: the subject is a
	// meta that is PRESENT and unusable, and starting the sidecar first
	// leaves a window in which it scans between the transcript's write
	// and the meta's — the ABSENT-meta case, which is a WARNING and is a
	// different subject. Writing both first closes the window with an
	// ordering rather than with a hope about scheduling.
	startSidecar(t, opts)
	rec := awaitLog(ctx, t, opts.LogPath, "the unusable-meta refusal", func(r logRecord) bool {
		return r.Operation == "discover-meta" && samePathAny(r.Context["path"], g.Path())
	})

	// Assert.
	if rec.Level != "error" {
		t.Errorf("a meta that states no toolUseId was reported at %q; it will not fix itself and must be an error", rec.Level)
	}
	entries := fake.Entries()
	if latestCursorFor(fake.Batches(), g.Path()) != nil {
		t.Errorf("a sidechain whose meta names no agent advanced a cursor; it must not be tailed at all")
	}
	for _, book := range []string{corpusSubagentAgentID, corpusSubagentID} {
		if len(linesForBook(entries, book)) != 0 {
			t.Errorf("an unidentifiable sidechain produced page lines in book %q", book)
		}
	}
}

// writeSubagentMetaWithout drops the fixture's meta.json beside the transcript
// with one field removed, so an edge case is a REAL meta minus one fact rather
// than an invented object.
func writeSubagentMetaWithout(t *testing.T, tree *vendorTree, slug, session, agent, field string) string {
	t.Helper()
	meta := decodeRecord(t, string(corpusBytes(t, "sidechain/agent-"+corpusSubagentID+".meta.json")))
	if _, ok := meta[field]; !ok {
		t.Fatalf("the meta fixture carries no %q to remove: %v", field, meta)
	}
	delete(meta, field)
	path := tree.subagentMetaPath(slug, session, agent)
	mustMkdirAll(t, filepath.Dir(path))
	if err := os.WriteFile(path, []byte(encodeRecord(t, meta)), 0o644); err != nil {
		t.Fatalf("write subagent meta %s: %v", path, err)
	}
	return path
}

// TestASubagentsBookNamesNoVendorLocator asserts the third leg of the minting
// rule end to end: the vendor's own `agentId` from the launch result reaches no
// book and no unit identity.
func TestASubagentsBookNamesNoVendorLocator(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/subagent-locator-join-probe"
	slug := cwdSlug(cwd)
	session := "3c3c3c3c-3c3c-43c3-83c3-3c3c3c3c3c3c"
	launch := corpusRecord(t, "tool-results/agent_async_launch.jsonl", 0)
	vendorLocator, _ := launch["toolUseResult"].(map[string]any)["agentId"].(string)
	if vendorLocator == "" {
		t.Fatalf("the async-launch fixture names no vendor agentId: %v", launch)
	}

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	parent := seedAgentSpawn(t, tree, cwd, session)
	sidechain := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	awaitCursorInBatches(ctx, t, fake, parent.Path(), parent.Offset())
	awaitCursorInBatches(ctx, t, fake, sidechain.Path(), sidechain.Offset())

	// Assert.
	entries := fake.Entries()
	for _, line := range pageLinesOf(entries) {
		if got := line.GetPageAgentId().GetValue(); got == vendorLocator {
			t.Errorf("a page line names book %q, which is the vendor's file locator rather than the agent's identity", got)
		}
	}
	for _, e := range entries {
		if e.GetUpsertKey() == "activity:"+vendorLocator {
			t.Errorf("a unit was keyed by the vendor locator %q", vendorLocator)
		}
	}
	if len(linesForBook(entries, corpusSubagentAgentID)) == 0 {
		t.Fatalf("no line reached the agent's own book %q, so the negative above is vacuous", corpusSubagentAgentID)
	}
}
