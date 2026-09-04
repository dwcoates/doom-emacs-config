package integration

import (
	"path/filepath"
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
// unread one: the file is discovered, cursor-tailed, and every record lands as
// DECLARED residue under `workflow_journal/<type>` — findable by that kind the
// day workflow ingestion arrives — while reaching no page, because a kicked
// kind is structurally unservable.

// TestAWorkflowJournalIsTailedToResidueAndReachesNoPage drives the checked-in
// complete journal capture through the discovered journal path.
func TestAWorkflowJournalIsTailedToResidueAndReachesNoPage(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/Users/dodgecoates/workflow-journal-probe"
	slug := cwdSlug(cwd)
	session := "a6a6a6a6-a6a6-4a6a-8a6a-a6a6a6a6a6a6"
	journalPath := filepath.Join(tree.projectDir(slug), session, "subagents", "workflows", "wf_0001", "journal.jsonl")

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	journal := newGrowingFile(t, journalPath)
	for _, line := range corpusLines(t, "journals/complete-journal.jsonl") {
		journal.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, journalPath, journal.Offset())

	// Assert: the capture's two record types each landed as declared residue...
	entries := fake.Entries()
	kinds := vendorSpecificKinds(entries)
	for _, want := range []string{"workflow_journal/started", "workflow_journal/result"} {
		if !containsString(kinds, want) {
			t.Errorf("the journal produced no vendor_specific %q; the kinds written were %v", want, kinds)
		}
	}
	// ...nothing failed to parse or fell to `unknown`, which would mean the
	// records were being classified rather than deliberately held...
	for _, u := range unparsedOf(entries) {
		if samePath(u.GetSource(), journalPath) {
			t.Errorf("a journal record failed to parse: %q", u.GetParseError())
		}
	}
	if n := len(unknownsOf(entries)); n != 0 {
		t.Errorf("the journal produced %d unknown-residue rows; both of the capture's record types are declared shapes", n)
	}
	// ...and none of it reached a page.
	if n := len(pageLinesOf(entries)); n != 0 {
		t.Errorf("a workflow journal produced %d page line(s) while workflow is kicked", n)
	}
}
