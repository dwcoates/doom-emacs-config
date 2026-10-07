package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// SUBJECT — a workflow's PER-AGENT transcript, while workflow is kicked.
//
// A run's directory holds two different files. `journal.jsonl` carries the run's
// two journal shapes; `agent-<id>.jsonl` carries ordinary TRANSCRIPT records.
// Both are discovered under one kind, so both reached the journal converter —
// and every transcript record fell through it to `unknown` residue.
//
// `unknown` MEANS "WE PARSED THIS AND DO NOT MODEL IT", which is the query built
// to find real modelling gaps. These shapes ARE modeled; the workflow FEATURE is
// what is kicked. Filing them as unknown both misstates the fact and buries the
// real gaps under a workflow run's whole transcript, so the classification is a
// DECLARED kind — named the day workflow ingestion lands.
//
// THE CLASSIFICATION IS THE WHOLE SUBJECT, because none of it is persisted:
// residue is never written, so what the reader called these records survives
// only in the sidecar's own withholding record.

// TestAWorkflowPerAgentTranscriptIsClassifiedAsDeclaredWorkflowResidue drives a
// workflow run's per-agent transcript and asserts the declared classification.
func TestAWorkflowPerAgentTranscriptIsClassifiedAsDeclaredWorkflowResidue(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	cwd := "/work/workflow-agent-transcript-probe"
	slug := cwdSlug(cwd)
	session := "a8a8a8a8-a8a8-4a8a-8a8a-a8a8a8a8a8a8"
	runDir := filepath.Join(tree.projectDir(slug), session, "subagents", "workflows", "wf_0002")
	transcriptPath := filepath.Join(runDir, "agent-"+corpusSubagentID+".jsonl")
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act: the meta lands first — a transcript without one is HELD, whatever
	// directory it sits in — and then the agent's own transcript records.
	anchor := newGrowingFile(t, tree.sessionPath(slug, session))
	anchor.AppendLine(encodeRecord(t, map[string]any{"type": "queue-operation", "cwd": cwd, "sessionId": session}))
	startSidecar(t, opts)
	mustMkdirAll(t, runDir)
	if err := os.WriteFile(filepath.Join(runDir, "agent-"+corpusSubagentID+".meta.json"),
		corpusBytes(t, "sidechain/agent-"+corpusSubagentID+".meta.json"), 0o644); err != nil {
		t.Fatalf("write workflow agent meta: %v", err)
	}
	g := newGrowingFile(t, transcriptPath)
	for _, line := range corpusLines(t, "sidechain/agent-"+corpusSubagentID+".jsonl") {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, transcriptPath, g.Offset())

	// Assert: the records were classified under the DECLARED workflow kind...
	awaitResidueWithheld(ctx, t, opts.LogPath, "vendor_specific/workflow/agent_transcript")
	// ...never as `unknown`, which would claim the shapes are unmodeled, and
	// never as a parse failure.
	for _, label := range residueWithheldLabels(t, opts.LogPath) {
		if strings.HasPrefix(label, "unknown/") {
			t.Errorf("a workflow per-agent transcript produced residue %q; its records are ordinary transcript shapes and the FEATURE is what is kicked", label)
		}
		if label == "unparsed" {
			t.Errorf("a workflow per-agent transcript record failed to parse")
		}
	}
	// ...none of it was persisted, because residue never is...
	entries := fake.Entries()
	requireNoResidueStored(t, entries)
	// ...and none of it reached a page, because a kicked kind is unservable.
	if n := len(pageLinesOf(entries)); n != 0 {
		t.Errorf("a workflow per-agent transcript produced %d page line(s) while workflow is kicked", n)
	}
}
