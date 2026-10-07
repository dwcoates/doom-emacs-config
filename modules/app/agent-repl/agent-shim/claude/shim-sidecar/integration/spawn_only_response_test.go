package integration

import (
	"strings"
	"testing"
)

// SUBJECT — an assistant response whose ONLY content block is a subagent
// SPAWN, read from a real transcript.
//
// WHY IT IS A SUBJECT AT ALL. This shape produces NO store entries at the call:
// the spawn's announcement waits on the launch's answer, because the
// created_agent_id is not knowable until then. That is a decision this
// converter took, and it is fine. What was NOT fine is what the reader SAID
// about it — every such response was reported as "every content block is in
// the exempt set", a decision that was never taken for the Agent tool. On the
// owner's machine that sentence fired 23 times in a 46-second window, and a
// reader hunting a modelling gap would have gone looking in the exempt set,
// which has nothing to do with it.
//
// THE UNIT TEST CANNOT COVER THE WHOLE OF IT. internal/convert's
// TestASpawnOnlyResponseNamesTheDeferredAnnounce builds the block by hand; this
// one reads the REAL captured record out of the golden corpus, through a real
// tailer, into a real log — so a shape the converter would refuse, or a record
// the reader would never reach, fails here rather than passing on a fixture the
// vendor does not write.

// TestARealSpawnOnlyResponseConvertsAndNamesTheDeferredAnnounce reads the
// captured record and pins both halves: the file's cursor advances past it, and
// the record that reports its zero units names the real cause.
func TestARealSpawnOnlyResponseConvertsAndNamesTheDeferredAnnounce(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/spawn-only-probe"
	slug := cwdSlug(cwd)
	session := "9b9b9b9b-9b9b-49b9-89b9-9b9b9b9b9b9b"
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	// The zero-unit record is BENIGN and now debug (a spawn announces at its
	// result, not its call), so the reader must run at debug to persist it.
	opts.ExtraEnv = []string{"AGENT_REPL_LOG_LEVEL=debug"}

	// The captured record is a SIDECHAIN line; the shape under test is the
	// block list, so it is retargeted onto a main-agent transcript rather than
	// dragging a subagent's meta file into a subject that is not about
	// identity.
	spawn := retargetSession(t, corpusRecord(t, "content-blocks/tool_use_spawn_only.jsonl", 0), session, cwd)
	delete(spawn, "isSidechain")
	delete(spawn, "agentId")

	// Act.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, spawn))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the reader got past the record, and said the true thing about it.
	rec := awaitLog(ctx, t, opts.LogPath, "the zero-unit record for the spawn-only response", func(r logRecord) bool {
		return r.Operation == "assistant-line" && r.Level == "debug" && strings.Contains(r.Message, "produced no units")
	})
	if !strings.Contains(rec.Message, "announces at its result") {
		t.Errorf("the zero-unit record does not name the deferred announce: %q", rec.Message)
	}
	if strings.Contains(rec.Message, "exempt set") {
		t.Errorf("the zero-unit record blames the exempt set for a spawn's deferred announce: %q", rec.Message)
	}
}
