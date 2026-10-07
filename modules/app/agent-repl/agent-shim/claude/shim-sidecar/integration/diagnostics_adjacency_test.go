package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — the two SOLE-PRODUCER attachments that depend on state the reader
// carries across polls: the IDE diagnostics report, and injected skills.
//
// THE DIAGNOSTICS JOIN IS BY ADJACENCY AND NOTHING ELSE. The vendor's record
// carries no tool-call id, so the converter holds ONE remembered "last
// write/edit unit". That remembered value is per-file converter state, NOT
// per-batch — which is invisible while a fixture writes the change and the
// report in one append, and is exactly what breaks if the state is ever rebuilt
// per poll. So the report is written on its OWN poll here.

// seedWriteChange writes a real Write call and its real captured result, so the
// remembered change unit is set by the vendor's own records.
func seedWriteChange(t *testing.T, tree *vendorTree, cwd, session string) (*growingFile, string) {
	t.Helper()
	captured := loadCapturedSession(t)
	slug := cwdSlug(cwd)
	result := retargetSession(t, decodeRecord(t, corpusLine(t, "tool-results/write.jsonl", 0)), session, cwd)
	callID := toolUseIDOfResult(t, result)

	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)
	call = renameToolUse(t, call, "Write")
	call = setToolUseInput(t, call, corpusToolUseInput(t, "tool-inputs/file_write.jsonl"))
	call = setToolUseID(t, call, callID)

	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, call))
	g.AppendLine(encodeRecord(t, result))
	return g, callID
}

// TestDiagnosticsJoinTheChangeUnitAcrossAPollBoundary asserts the remembered
// change unit survives the poll that settled it.
func TestDiagnosticsJoinTheChangeUnitAcrossAPollBoundary(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/diagnostics-adjacency-probe"
	session := "7a7a7a7a-7a7a-47a7-87a7-7a7a7a7a7a7a"

	// Act: the change lands and is fully committed FIRST; the report follows on
	// a later poll.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g, callID := seedWriteChange(t, tree, cwd, session)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	report := retargetSession(t,
		decodeRecord(t, corpusLine(t, "attachments/diagnostics.jsonl", 0)), session, cwd)
	g.AppendLine(encodeRecord(t, report))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	wantKey := "activity:" + callID
	e := fake.awaitEntry(ctx, t, "the diagnostics frame of the change unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey &&
			activityOf(e.GetAgentUpdate().GetServeableFrame()).GetWrite().GetDiagnostics() != nil
	})
	files := activityOf(e.GetAgentUpdate().GetServeableFrame()).GetWrite().GetDiagnostics().GetFiles()
	if len(files) == 0 {
		t.Fatalf("the diagnostics report names no file")
	}
	if files[0].GetPath() == "" {
		t.Errorf("a diagnostics file names no path: %v", files[0])
	}
	if len(files[0].GetDiagnostics()) == 0 {
		t.Errorf("a diagnostics file carries no findings: %v", files[0])
	}
}

// TestDiagnosticsWithNoObservedChangeAreKeptWholeRatherThanGuessed asserts the
// refusal: with no remembered change unit the findings are carried whole as
// residue, never pinned onto a unit the reader guessed at.
//
// WHOLE MEANS CLASSIFIED WHOLE, not stored. Residue is never persisted, so the
// report's disposition is stated by the sidecar's own withholding record naming
// `attachment/diagnostics` — and the refusal to guess is exactly what that
// record proves, because a guessed join would have produced a page line instead.
func TestDiagnosticsWithNoObservedChangeAreKeptWholeRatherThanGuessed(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/diagnostics-orphan-probe"
	slug := cwdSlug(cwd)
	session := "7b7b7b7b-7b7b-47b7-87b7-7b7b7b7b7b7b"
	// The orphaned-diagnostics record is BENIGN on a re-scan and emitted at
	// debug (see diagnosticsAttachment): a report whose causing change scrolled
	// past this reader's cursor is carried whole as residue, which is correct,
	// so it must not flood the strict all-logs harvest at warn. Debug logging is
	// enabled here so that trace, and the withholding record, both reach the log.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))
	report := retargetSession(t,
		decodeRecord(t, corpusLine(t, "attachments/diagnostics.jsonl", 0)), session, cwd)

	// Act: no write or edit precedes the report.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)))
	g.AppendLine(encodeRecord(t, report))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	awaitResidueWithheld(ctx, t, opts.LogPath, "vendor_specific/attachment/diagnostics")
	requireNoResidueStored(t, fake.Entries())
	for _, line := range pageLinesOf(fake.Entries()) {
		a := activityOf(line)
		if a.GetWrite().GetDiagnostics() != nil || a.GetEdit().GetDiagnostics() != nil {
			t.Errorf("orphaned findings were pinned onto unit %q", a.GetActivityId().GetValue())
		}
	}
	awaitLog(ctx, t, opts.LogPath, "the orphaned-diagnostics debug trace", func(r logRecord) bool {
		return r.Level == "debug" && r.Verbosity == "verbose" && r.Operation == "diagnostics"
	})
}

// TestInjectedSkillsReachTheAgentsBook asserts the other sole-producer
// attachment: skills the vendor pulled in with NO tool call of their own land as
// an AgentContextInjected activity in the agent's own book.
func TestInjectedSkillsReachTheAgentsBook(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/injected-skills-probe"
	slug := cwdSlug(cwd)
	session := "7c7c7c7c-7c7c-47c7-87c7-7c7c7c7c7c7c"
	skills := retargetSession(t,
		decodeRecord(t, corpusLine(t, "attachments/invoked_skills.jsonl", 0)), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	for _, line := range captured.Lines[:8] {
		g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, line), session, cwd)))
	}
	g.AppendLine(encodeRecord(t, skills))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	e := fake.awaitEntry(ctx, t, "the injected-skills activity", func(e *storev1.StoreEntry) bool {
		return len(activityOf(e.GetAgentUpdate().GetServeableFrame()).GetContextInjected().GetSkills().GetSkills()) > 0
	})
	line := e.GetAgentUpdate().GetServeableFrame()
	if got := line.GetPageAgentId().GetValue(); got != session {
		t.Errorf("the injected skills landed in book %q, wanted the agent's own %q", got, session)
	}
	for _, skill := range activityOf(line).GetContextInjected().GetSkills().GetSkills() {
		if skill.GetName() == "" {
			t.Errorf("an injected skill carries no name: %v", skill)
		}
	}
}
