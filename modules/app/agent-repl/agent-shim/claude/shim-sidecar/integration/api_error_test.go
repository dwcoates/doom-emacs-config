package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 9 — a recorded API failure.
//
// A transcript `system/api_error` line is MID-TURN EVIDENCE, never a terminal:
// it lands as an AgentUpdate.api_error page line keyed session:api_error:<uuid>,
// carrying the vendor's own error taxonomy. The turn's END is the frame-level
// failure arm and nothing else.

// seedApiError writes a session whose second line is the corpus api_error
// record, and answers the record's uuid.
func seedApiError(t *testing.T, tree *vendorTree, cwd, session string) (*growingFile, string) {
	t.Helper()
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)
	rec := retargetSession(t, decodeRecord(t, corpusLine(t, "transcript-lines/system-api_error.jsonl", 0)), session, cwd)
	uuid, _ := rec["uuid"].(string)
	if uuid == "" {
		t.Fatalf("the api_error fixture carries no uuid")
	}
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, rec))
	return g, uuid
}

// TestAnApiErrorIsNeverATerminal asserts the frame is an UPDATE: the agent must
// remain live, so nothing on the success or failure arm may be produced for it.
func TestAnApiErrorIsNeverATerminal(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	session := "d0d0d0d0-d0d0-40d0-80d0-d0d0d0d0d0d0"
	g, uuid := seedApiError(t, tree, "/work/api-error-live-probe", session)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	wantKey := "session:api_error:" + uuid
	fake.awaitEntry(ctx, t, "the api_error record", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey
	})
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	e := entryByUpsertKey(fake.Entries(), wantKey)
	frame := frameOf(e.GetAgentUpdate().GetServeableFrame())
	if frame.GetUpdate() == nil {
		t.Fatalf("an api_error must be an AgentUpdate, not a terminal arm: %v", frame.GetResult())
	}
	for _, line := range linesForBook(fake.Entries(), session) {
		f := frameOf(line)
		if f.GetSuccess() != nil || f.GetFailure() != nil {
			t.Errorf("a recorded api_error must not conclude the agent; a terminal frame was written: %v", f.GetResult())
		}
	}
}
