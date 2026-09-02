package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — THE NEGATIVE: the file plane produces NO SessionUpdate row.
//
// StoreEntry has two arms, and only one of them is the sidecar's. Landing 4
// moved the LAST candidate — the context-budget warning, which the sidecar is
// the sole producer of — off SessionUpdate and onto the agent's own book,
// because SessionUpdate had no producer on the live session stream at all. So
// the whole file plane owes zero session_update rows, and the only way to state
// that is to assert it over a real, wide ingest.

// TestNoFilePlaneRecordEverProducesASessionUpdate ingests the captured
// transcript together with the sole-producer attachment and the recorded
// api_error, and asserts every entry landed on agent_update.
func TestNoFilePlaneRecordEverProducesASessionUpdate(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/session-update-probe"
	slug := cwdSlug(cwd)
	session := "5a5a5a5a-5a5a-45a5-85a5-5a5a5a5a5a5a"
	warning := retargetSession(t,
		decodeRecord(t, corpusLine(t, "attachments/context_budget_warning.jsonl", 0)), session, cwd)
	apiError := retargetSession(t,
		decodeRecord(t, corpusLine(t, "transcript-lines/system-api_error.jsonl", 0)), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	for _, line := range captured.Lines {
		g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, line), session, cwd)))
	}
	g.AppendLine(encodeRecord(t, warning))
	g.AppendLine(encodeRecord(t, apiError))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the ingest was real…
	entries := fake.Entries()
	if len(entries) == 0 {
		t.Fatal("the ingest produced no entries at all, so the negative would hold vacuously")
	}
	var budget int
	for _, e := range entries {
		if e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetContextBudgetWarning() != nil {
			budget++
		}
	}
	if budget == 0 {
		t.Fatal("the sole-producer context-budget warning never landed; the negative below would not be about a real ingest")
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

// TestTheContextBudgetWarningIsAnAgentUpdateRatherThanASessionUpdate pins the
// one record the old contract DID put on SessionUpdate: it is a page line of the
// agent's own book, and the arm is the whole point of landing 4.
func TestTheContextBudgetWarningIsAnAgentUpdateRatherThanASessionUpdate(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/budget-arm-probe"
	slug := cwdSlug(cwd)
	session := "5b5b5b5b-5b5b-45b5-85b5-5b5b5b5b5b5c"
	warning := retargetSession(t,
		decodeRecord(t, corpusLine(t, "attachments/context_budget_warning.jsonl", 0)), session, cwd)
	uuid, _ := warning["uuid"].(string)
	if uuid == "" {
		t.Fatalf("the context-budget fixture carries no uuid")
	}

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	for _, line := range captured.Lines[:8] {
		g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, line), session, cwd)))
	}
	g.AppendLine(encodeRecord(t, warning))
	wantKey := "session:context_budget_warning:" + uuid
	fake.awaitEntry(ctx, t, "the context-budget warning", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey
	})

	// Assert.
	e := entryByUpsertKey(fake.Entries(), wantKey)
	if e.GetSessionUpdate() != nil {
		t.Fatalf("the context-budget warning landed on session_update: %v", e.GetSessionUpdate())
	}
	line := e.GetAgentUpdate().GetServeableFrame()
	if line == nil {
		t.Fatalf("the warning is a page line of the agent's own book: %v", e.GetAgentUpdate())
	}
	if got := line.GetPageAgentId().GetValue(); got != session {
		t.Errorf("the warning names book %q, wanted the agent's own %q", got, session)
	}
}
