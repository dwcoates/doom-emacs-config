package integration

import (
	"testing"
)

// SUBJECT — THE NEGATIVE: the file plane produces NO SessionUpdate row.
//
// StoreEntry has two arms, and only one of them is the sidecar's. Landing 4
// moved the LAST candidate — the context-budget warning — off SessionUpdate
// (and it has since been retired outright, owner ruling 2026-10-06). So the
// whole file plane owes zero session_update rows, and the only way to state
// that is to assert it over a real, wide ingest.

// TestNoFilePlaneRecordEverProducesASessionUpdate ingests the captured
// transcript together with the recorded api_error, and asserts every entry
// landed on agent_update.
func TestNoFilePlaneRecordEverProducesASessionUpdate(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/session-update-probe"
	slug := cwdSlug(cwd)
	session := "5a5a5a5a-5a5a-45a5-85a5-5a5a5a5a5a5a"
	apiError := retargetSession(t,
		decodeRecord(t, corpusLine(t, "transcript-lines/system-api_error.jsonl", 0)), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	for _, line := range captured.Lines {
		g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, line), session, cwd)))
	}
	g.AppendLine(encodeRecord(t, apiError))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the ingest was real…
	entries := fake.Entries()
	if len(entries) == 0 {
		t.Fatal("the ingest produced no entries at all, so the negative would hold vacuously")
	}

	// …and not one record of it reached the session_update arm.
	for _, e := range entries {
		if e.GetSessionUpdate() != nil {
			t.Errorf("entry %q landed on session_update; the file plane produces no session-scoped rows: %v",
				e.GetUpsertKey(), e.GetSessionUpdate())
		}
		if e.GetAgentUpdate() == nil {
			t.Errorf("entry %q carries neither arm", e.GetUpsertKey())
		}
	}
}
