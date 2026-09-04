package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT — R9's identity rule, exercised against the divergence it exists for.
//
// A MAIN AGENT'S IDENTITY IS THE TRANSCRIPT FILE'S SESSION UUID, never the
// per-record `sessionId` field, which disagrees with the runtime's answer in
// roughly a fifth of real records. The rule is only load-bearing where the two
// DISAGREE: a suite whose fixtures always agree would go on passing after a
// converter started reading the record's field instead, and every diverging
// record would quietly open a second book nobody could reconcile.

// divergentSessionID is a uuid no file in the tree is named by, so a book named
// by it can only have come from reading the record's own field.
const divergentSessionID = "d1ffe4e4-d1ff-4d1f-8d1f-d1ffd1ffd1ff"

// TestARecordWhoseSessionIdDivergesStillLandsInTheFilesBook asserts both halves:
// the diverging record's unit is a line in the FILE's book, and no book named by
// the record's own value exists at all.
func TestARecordWhoseSessionIdDivergesStillLandsInTheFilesBook(t *testing.T) {
	t.Parallel()
	// Arrange: the captured transcript, with the assistant record carrying the
	// Bash call re-stamped with a sessionId that names no file here.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	lines := append([]string(nil), captured.Lines...)
	lines[8] = encodeRecord(t, withFields(t, decodeRecord(t, lines[8]),
		map[string]any{"sessionId": divergentSessionID}))

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	fake.awaitEntry(ctx, t, "the diverging record's unit", func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == "activity:"+capturedBashCall1
	})

	// Assert: the unit is a line in the FILE's book...
	unit := entryByUpsertKey(fake.Entries(), "activity:"+capturedBashCall1)
	if got := unit.GetAgentUpdate().GetServeableFrame().GetPageAgentId().GetValue(); got != captured.Session {
		t.Errorf("the diverging record's unit landed in book %q, wanted the transcript file's uuid %q", got, captured.Session)
	}
	// ...and the record's own sessionId opened no book of its own.
	if lines := linesForBook(fake.Entries(), divergentSessionID); len(lines) != 0 {
		t.Errorf("%d page line(s) landed in a book named by the record's sessionId %q; that divergence never rides the wire",
			len(lines), divergentSessionID)
	}
}
