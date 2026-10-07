package integration

import (
	"path/filepath"
	"strings"
	"testing"
)

// SUBJECT — a workflow JOURNAL, while workflow ingestion is kicked.
//
// spool_routing_test.go covers the w* SPOOL. The journal is the other workflow
// file, and it is a different shape entirely: it lives under the session's own
// tree at `.../subagents/workflows/wf_<id>/journal.jsonl`, and it is JSONL the
// vendor really does write records into rather than raw output.
//
// THE DISPOSITION IS THE SAME AND THAT IS THE POINT. A kicked kind is not an
// unread one: the file is discovered, cursor-tailed, and every record is
// CLASSIFIED as declared residue under `workflow_journal/<type>` — named by that
// kind the day workflow ingestion arrives — while reaching no page, because a
// kicked kind is structurally unservable.
//
// AND NONE OF IT IS PERSISTED. Residue is never written, so the evidence that
// the reader carried the journal is the sidecar's own withholding record naming
// the kind it classified, not a row.

// TestAWorkflowJournalIsClassifiedAsDeclaredResidueAndReachesNoPage drives the
// checked-in complete journal capture through the discovered journal path.
func TestAWorkflowJournalIsClassifiedAsDeclaredResidueAndReachesNoPage(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/workflow-journal-probe"
	slug := cwdSlug(cwd)
	session := "a6a6a6a6-a6a6-4a6a-8a6a-a6a6a6a6a6a6"
	journalPath := filepath.Join(tree.projectDir(slug), session, "subagents", "workflows", "wf_0001", "journal.jsonl")
	// The withholding record is the only evidence a kicked kind leaves, and it
	// is verbose.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	anchor := newGrowingFile(t, tree.sessionPath(slug, session))
	anchor.AppendLine(encodeRecord(t, map[string]any{"type": "queue-operation", "cwd": cwd, "sessionId": session}))
	startSidecar(t, opts)
	journal := newGrowingFile(t, journalPath)
	for _, line := range corpusLines(t, "journals/complete-journal.jsonl") {
		journal.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, journalPath, journal.Offset())

	// Assert: the capture's two record types were each classified under their
	// declared kind and withheld...
	for _, want := range []string{"vendor_specific/workflow_journal/started", "vendor_specific/workflow_journal/result"} {
		awaitResidueWithheld(ctx, t, opts.LogPath, want)
	}
	// ...nothing failed to parse or fell to `unknown`, which would mean the
	// records were being classified rather than deliberately held...
	for _, label := range residueWithheldLabels(t, opts.LogPath) {
		if label == "unparsed" || strings.HasPrefix(label, "unknown/") {
			t.Errorf("the journal produced residue %q; both of the capture's record types are declared shapes", label)
		}
	}
	// ...none of it was persisted, because residue never is...
	entries := fake.Entries()
	requireNoResidueStored(t, entries)
	// ...and none of it reached a page.
	if n := len(pageLinesOf(entries)); n != 0 {
		t.Errorf("a workflow journal produced %d page line(s) while workflow is kicked", n)
	}
}
