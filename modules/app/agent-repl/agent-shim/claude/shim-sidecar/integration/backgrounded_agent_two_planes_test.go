package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — ONE backgrounded subagent, present on disk TWICE.
//
// The vendor writes a backgrounded agent's transcript to its task spool
// (`tasks/a<id>.output`) AND to the session's sidechain directory
// (`subagents/agent-<id>.jsonl`). Both are the SAME agent, so both must produce
// the SAME book, the SAME upsert keys, and the SAME `top_level` — and top_level
// for a backgrounded spawn is the SUBAGENT ITSELF, because its stream outlives
// the turn that spawned it.
//
// THE TWO PATHS LEARNED IT DIFFERENTLY, which is why they disagreed. The spool's
// discovery target names a TASK and the backgrounded flag was looked up by task
// id; the sidechain's target names no task at all, so it answered false and its
// writes named the session's main agent instead. One agent, two planes, two
// answers no consumer could reconcile.

// TestABackgroundedAgentSeenAsSpoolAndSidechainIsOneBookWithOneTopLevel writes
// the same agent's transcript through both paths and asserts the two writes
// agree on key and top_level, and that the store holds one row per key.
func TestABackgroundedAgentSeenAsSpoolAndSidechainIsOneBookWithOneTopLevel(t *testing.T) {
	t.Parallel()
	// Arrange: a parent transcript whose Agent call is the SPAWNING CALL the
	// sidechain's meta.json already names, launched async.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	proxy := startProxyStore(t, store.Socket)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/backgrounded-two-planes-probe"
	slug := cwdSlug(cwd)
	session := "b7b7b7b7-b7b7-4b7b-8b7b-b7b7b7b7b7b7"
	spoolPath := tree.spoolPath(slug, session, corpusAsyncAgentTask)

	call := setToolUseID(t,
		renameToolUse(t, retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd), "Agent"),
		corpusSubagentAgentID)
	result := retargetSession(t, decodeRecord(t, corpusLine(t, "tool-results/agent_async_launch.jsonl", 0)), session, cwd)
	result = setToolUseID(t, result, corpusSubagentAgentID)
	result = setNested(t, result, "toolUseResult", "outputFile", spoolPath)

	// Act: the launch is observed, then the same transcript arrives on BOTH
	// paths — through the task spool and through the sidechain directory.
	startSidecar(t, defaultSidecarOptions(t, proxy.Socket, tree))
	parent := newGrowingFile(t, tree.sessionPath(slug, session))
	parent.AppendLine(encodeRecord(t, call))
	parent.AppendLine(encodeRecord(t, result))
	awaitCursorAtLeast(ctx, t, store.Client, parent.Path(), parent.Offset())

	spool := newGrowingFile(t, spoolPath)
	for _, line := range corpusLines(t, "sidechain/agent-"+corpusSubagentID+".jsonl") {
		spool.AppendLine(line)
	}
	writeSubagentMeta(t, tree, slug, session, corpusSubagentID)
	sidechain := writeSubagentTranscript(t, tree, slug, session, corpusSubagentID)
	awaitCursorAtLeast(ctx, t, store.Client, spoolPath, spool.Offset())
	awaitCursorAtLeast(ctx, t, store.Client, sidechain.Path(), sidechain.Offset())

	// Assert: both planes wrote at least one key twice, and every write of a key
	// carries the SAME top_level, which is the subagent itself.
	byKey := map[string][]*storev1.StoreEntry{}
	for _, e := range entriesOf(proxy.Batches()) {
		line := e.GetAgentUpdate().GetServeableFrame()
		if line == nil || line.GetPageAgentId().GetValue() != corpusSubagentAgentID {
			continue
		}
		byKey[e.GetUpsertKey()] = append(byKey[e.GetUpsertKey()], e)
	}
	if len(byKey) == 0 {
		t.Fatalf("neither plane produced a page line in the subagent's book %q", corpusSubagentAgentID)
	}
	shared := 0
	for key, written := range byKey {
		if len(written) > 1 {
			shared++
		}
		for _, e := range written {
			if got := e.GetAgentUpdate().GetTopLevel().GetValue(); got != corpusSubagentAgentID {
				t.Errorf("a write of %q names top_level %q; a BACKGROUNDED subagent's stream outlives its turn, so it is its own top_level (%q)",
					key, got, corpusSubagentAgentID)
			}
		}
	}
	if shared == 0 {
		t.Fatalf("no upsert key was written by both planes, so this subject compared nothing; keys seen: %v", sortedStrings(keysOfEntries(byKey)))
	}

	// And the store holds ONE row per key however many planes wrote it.
	seen := map[string]int{}
	for _, at := range bookLines(ctx, t, store.Client, corpusSubagentAgentID) {
		if a := activityOf(at.GetLine()); a != nil {
			seen[a.GetActivityId().GetValue()]++
		}
	}
	for id, n := range seen {
		if n != 1 {
			t.Errorf("unit %q holds %d rows; one agent seen on two planes is still one row per key", id, n)
		}
	}
}

// keysOfEntries lists the upsert keys of a grouping, for a failure message.
func keysOfEntries(byKey map[string][]*storev1.StoreEntry) []string {
	out := make([]string, 0, len(byKey))
	for k := range byKey {
		out = append(out, k)
	}
	return out
}
