package handler

// journal_test.go — the workflow handler, and the two DIFFERENT files a run's
// directory holds.
//
// `wf_<id>/journal.jsonl` carries the run's two journal shapes.
// `wf_<id>/agent-<id>.jsonl` carries ordinary TRANSCRIPT records. They share one
// discovery kind, so a handler that ran both through the journal converter filed
// every transcript record as `unknown` residue — the query built to find real
// modelling gaps, polluted by records whose shapes are modeled perfectly well
// and whose FEATURE is what is kicked.

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/tail"
)

// workflowContext is the attribution a workflow file is read under.
func workflowContext(path, session, runID, taskID string) *Context {
	return &Context{
		Path: path, SessionID: session, MainAgentID: session, AgentID: session,
		RunID: runID, TaskID: taskID, FileID: testFileID(path), Kind: tail.KindWorkflowJournal,
	}
}

// workflowTranscriptLine is an ordinary assistant record, which is what a
// workflow's per-agent transcript is made of.
const workflowTranscriptLine = `{"type":"assistant","uuid":"wf-a-1","isSidechain":true,` +
	`"timestamp":"2026-07-21T20:00:00.000Z","message":{"id":"msg_wf","role":"assistant",` +
	`"content":[{"type":"text","text":"working"}]}}`

func TestAWorkflowPerAgentTranscriptRecordIsDeclaredResidueNotUnknown(t *testing.T) {
	// Arrange.
	h := NewWorkflowJournalHandler(testLogger(t))
	ctx := workflowContext("/p/s/subagents/workflows/wf_1/agent-w1.jsonl", "s", "wf_1", "w1")

	// Act.
	entries := h.Handle(framesFrom(t, workflowTranscriptLine), ctx)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	if u := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown(); u != nil {
		t.Fatalf("a workflow per-agent transcript record landed as unknown residue (discriminator %q); its shape IS modeled and the FEATURE is what is kicked",
			u.GetDiscriminator())
	}
	if got := vendorKinds(entries); len(got) != 1 || got[0] != "workflow/agent_transcript" {
		t.Fatalf("vendor_specific kinds = %v, want exactly the declared workflow transcript kind", got)
	}
}

func TestAWorkflowPerAgentTranscriptRecordReachesNoPage(t *testing.T) {
	// Arrange. Workflow is KICKED: residue-only is the whole outcome, so none of
	// it may be servable.
	h := NewWorkflowJournalHandler(testLogger(t))
	ctx := workflowContext("/p/s/subagents/workflows/wf_1/agent-w1.jsonl", "s", "wf_1", "w1")

	// Act.
	entries := h.Handle(framesFrom(t, workflowTranscriptLine), ctx)

	// Assert.
	if pageLine(entries[0]) != nil {
		t.Fatal("a workflow per-agent transcript record reached a page line while workflow is kicked")
	}
}

func TestAWorkflowJournalRecordStillConvertsAsAJournalRecord(t *testing.T) {
	// Arrange. The routing above must not have taken the journal with it: the
	// run's own journal keeps its own declared kind.
	h := NewWorkflowJournalHandler(testLogger(t))
	ctx := workflowContext("/p/s/subagents/workflows/wf_1/journal.jsonl", "s", "wf_1", "wf_1")

	// Act.
	entries := h.Handle(framesFrom(t, `{"type":"started","uuid":"j-1","key":"k","agentId":"w1"}`), ctx)

	// Assert.
	if got := vendorKinds(entries); len(got) != 1 || got[0] != "workflow_journal/started" {
		t.Fatalf("vendor_specific kinds = %v, want exactly the journal's own declared kind", got)
	}
}
